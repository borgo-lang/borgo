package borgo

import (
	"fmt"
	"hash/fnv"
	"log"
	"strconv"
	"strings"

	"borgo/immutable"
	"reflect"
)

var Unit struct{}

var make_Unit = Unit

// ------------
// Runtime
// ------------

type TypeKind int

const (
	TypeStruct TypeKind = iota
	TypeEnum
)

type TypeRep struct {
	kind    TypeKind
	name    string
	mangled string
	fields  []string
}

func (r TypeRep) IsStruct() bool {
	return r.kind == TypeStruct
}

func (r TypeRep) IsEnum() bool {
	return r.kind == TypeEnum
}

type B struct {
	effect_functions map[string]any     // "ref_set"  -> *ref_set
	global_functions map[string]any     // "List::push"  -> *List_push
	make_functions   map[string]any     // "Option::Some" -> *make_Option_Some
	type_reps        map[string]TypeRep // "Option_Some" -> TypeRep
	debug_output_fn  func(string)
}

var borgo = B{
	effect_functions: map[string]any{},
	global_functions: map[string]any{},
	make_functions:   map[string]any{},
	type_reps:        map[string]TypeRep{},
	debug_output_fn:  func(s string) { fmt.Println(s) },
}

var Ops = OperatorImpl{}

// ------------
// Core
// ------------

func RegisterTypeConstructor(original string, mangled string) {
	borgo.type_reps[mangled] = TypeRep{name: original, mangled: mangled, kind: TypeEnum, fields: []string{}}
}

func RegisterStruct(original string, mangled string, fields []string) {
	borgo.type_reps[mangled] = TypeRep{name: original, mangled: mangled, kind: TypeStruct, fields: fields}
}

func RegisterMakeFunction(name string, fn any) {
	borgo.make_functions[name] = fn
}

func RegisterGlobalFunction(name string, fn any) {
	borgo.global_functions[name] = fn
}

func RegisterEffectFunction(name string, fn any) {
	// TODO This is a bit messy
	// there should be a distinction between a native function and an effect
	RegisterGlobalFunction(name, fn)
	// borgo.effect_functions[name] = fn
}

func SetOutputFunction(fn func(string)) {
	borgo.debug_output_fn = fn
}

// TODO These functions know about BorgoValue, which isn't ideal.
// I should be able to swap implementation depending if codegen/eval.
// For now, it's all crammed in a single function.
func ValuesIsOfType(x any, typ string) bool {
	if v, ok := x.(BorgoValue); ok {
		return v.ValueIsOfType(typ)
	}

	name := reflect.TypeOf(x).Name()
	return borgo.type_reps[name].name == typ
}

func GetArg(x any, index int) any {
	if v, ok := x.(BorgoValue); ok {
		return v.GetArg(index)
	}

	return GetField(x, "Field"+strconv.Itoa(index))
}

func GetField(x any, field string) any {
	if v, ok := x.(BorgoValue); ok {
		return v.GetField(field)
	}

	ref := reflect.ValueOf(x)
	return ref.FieldByName(field).Interface()
}

func Create(typ string) any {
	return borgo.make_functions[typ]
}

func Create1(typ string, arg0 any) any {
	make_fn := borgo.make_functions[typ]
	return make_fn.(func(any) any)(arg0)
}

func Create2(typ string, arg0 any, arg1 any) any {
	make_fn := borgo.make_functions[typ]
	return make_fn.(func(any, any) any)(arg0, arg1)
}

func GetNative0(name string) func() any {
	return borgo.global_functions[name].(func() any)
}

func GetNative1(name string) func(any) any {
	return borgo.global_functions[name].(func(any) any)
}

func GetNative2(name string) func(any, any) any {
	return borgo.global_functions[name].(func(any, any) any)
}

func GetNative3(name string) func(any, any, any) any {
	return borgo.global_functions[name].(func(any, any, any) any)
}

func Call(fun any, args []any) any {
	x := reflect.ValueOf(fun)

	new_args := []reflect.Value{}
	for _, a := range args {
		// During evaluation, a `nil` value may end up here.
		// That doesn't play nicely with reflect.ValueOf()
		// Just set it to a random value so it works.
		// Probably worth digging into why a `nil` value gets here in the first place...
		if a == nil {
			a = 0
		}

		new_args = append(new_args, reflect.ValueOf(a))
	}

	val := x.Call(new_args)
	return val[0].Interface()
}

func OverloadImpl(trait_name string, values []any) any {
	return borgo.overloadImplementation(trait_name, values)
}

func (borgo *B) overloadImplementation(trait_name string, values []any) any {
	target := values[0]
	ref := reflect.TypeOf(target)

	type_name := refToTypeName(target)

	method := fmt.Sprintf("%s::%s", type_name, trait_name)

	// Check if there is a custom implementation for this trait
	if impl, ok := borgo.global_functions[method]; ok {
		return Call(impl, values)
	}

	// Skip equals implementation, can't have user provided impls for now
	if ref.Kind() != reflect.Struct && ref.Kind() != reflect.Ptr && trait_name != "equals" {
		log.Fatalf("missing overload impl for %s, kind %v", method, ref.Kind())
	}

	// If there's not, it must be a trait the compiler knows about
	if ret, ok := builtInTraitImpl(trait_name, ref, values); ok {
		return ret
	}

	log.Fatalf("unhandled trait impl %s", method)
	return nil
}

func builtInTraitImpl(trait_name string, ref reflect.Type, values []any) (any, bool) {
	switch ref.Kind() {
	case reflect.Array:
	case reflect.Map:
		panic("can't handle array/map")

	case reflect.Chan:
		panic("TODO do channels")

	}

	// TODO both to_hash and equals will get proper support when comptime lands.
	switch trait_name {
	case "to_string":
		return builtInToString(values[0]), true
	case "to_hash":
		return builtinHashImpl(values[0]), true
	case "equals":
		return reflect.DeepEqual(values[0], values[1]), true
	}

	return nil, false
}

func builtInToString(value any) any {
	ref := reflect.TypeOf(value)
	ref_value := reflect.ValueOf(value)

	name := ref.Name()

	// Accumulate all data in a map
	// This is to use the same logic across real structures and BorgoValues
	var data = []FieldData{}

	isBorgoValue := name == "BorgoValue"

	if isBorgoValue {
		name = value.(BorgoValue).typ
		for k, v := range value.(BorgoValue).data {
			data = append(data, FieldData{k, v})
		}
	}

	type_rep, ok := borgo.type_reps[name]
	if !ok {
		panic("type not found " + name)
	}

	// Populate `data` with all the fields in a struct
	if !isBorgoValue && type_rep.IsStruct() {
		for i := 0; i < ref.NumField(); i++ {
			f := ref.Field(i)

			field_name := revertStructFieldName(f.Name)
			field_value := ref_value.Field(i).Interface()
			data = append(data, FieldData{field_name, field_value})
		}
	}

	// Populate `data` with all the fields in an enum
	if !isBorgoValue && type_rep.IsEnum() {
		for i := 0; i < ref.NumField(); i++ {
			field_value := ref_value.Field(i).Interface()
			data = append(data, FieldData{strconv.Itoa(i), field_value})
		}
	}

	fields := []string{}

	if type_rep.IsStruct() {
		isTuple := strings.HasPrefix(type_rep.name, "Tuple")

		// Iterate over type rep fields, so that order is preserved
		for _, field_name := range type_rep.fields {

			// Lookup field
			var field FieldData
			for _, f := range data {
				if f.name == field_name {
					field = f
					break
				}
			}

			field_value := field.value
			s := genericToString(field_value)

			if !isTuple {
				s = field_name + ": " + s
			}

			fields = append(fields, s)
		}

		return type_rep.name + " { " + strings.Join(fields, ", ") + " }"
	}

	if type_rep.IsEnum() {
		if len(data) == 0 {
			return type_rep.name
		}

		for _, field := range data {
			s := genericToString(field.value)
			fields = append(fields, s)
		}

		return type_rep.name + "(" + strings.Join(fields, ", ") + ")"

	}

	log.Fatalf("builtin to_string broke %+v", value)
	return ""
}

func refToTypeName(value any) string {
	ref := reflect.TypeOf(value)
	ref_value := reflect.ValueOf(value)

	// Check if it's a borgo value
	if v, ok := value.(BorgoValue); ok {
		return constructorToType(v.typ)
	}

	switch ref.Kind() {
	case reflect.Int:
		return "Int"
	case reflect.Float64:
		return "Float"
	case reflect.Bool:
		return "Bool"
	case reflect.String:
		return "String"
	case reflect.Func:
		return "$Function"

	case reflect.Int32:
		// int32 -> char
		// int64 -> int
		// bit of a hack, not great
		return "Char"

	case reflect.Uint32:
		return "Hash"

	case reflect.Struct:
		name := ref.Name()

		if name == "" {
			return "Unit"
		}

		cons := borgo.type_reps[name].name
		return constructorToType(cons)

	case reflect.Ptr:
		// Follow the pointer
		inner := reflect.Indirect(ref_value).Interface()
		name := reflect.TypeOf(inner).Name()

		switch name {
		case "List":
			return "List"
		case "Map":
			return "Map"
		case "Internal_Ref":
			return "Ref"
		}

		panic("unhandled pointer type " + name)

	default:
		log.Fatalf("Unhandled kind kindToType %v", ref.Kind())
	}

	return ""
}

func constructorToType(cons string) string {
	parts := strings.Split(cons, "::")
	return parts[0]
}

// ------------
// Ints
// ------------

type OperatorImpl struct{}

func (OperatorImpl) IAdd(x any, y any) any {
	return x.(int) + y.(int)
}

func (OperatorImpl) ISub(x any, y any) any {
	return x.(int) - y.(int)
}

func (OperatorImpl) IMul(x any, y any) any {
	return x.(int) * y.(int)
}

func (OperatorImpl) IDiv(x any, y any) any {
	return x.(int) / y.(int)
}

func (OperatorImpl) IRem(x any, y any) any {
	return x.(int) % y.(int)
}

func (OperatorImpl) ILt(x any, y any) any {
	return x.(int) < y.(int)
}

func (OperatorImpl) ILe(x any, y any) any {
	return x.(int) <= y.(int)
}

func (OperatorImpl) IGt(x any, y any) any {
	return x.(int) > y.(int)
}

func (OperatorImpl) IGe(x any, y any) any {
	return x.(int) >= y.(int)
}

func (OperatorImpl) INeg(x any) any {
	return -x.(int)
}

// ------------
// Floats
// ------------

func (OperatorImpl) FAdd(x any, y any) any {
	return x.(float64) + y.(float64)
}

func (OperatorImpl) FSub(x any, y any) any {
	return x.(float64) - y.(float64)
}

func (OperatorImpl) FMul(x any, y any) any {
	return x.(float64) * y.(float64)
}

func (OperatorImpl) FDiv(x any, y any) any {
	return x.(float64) / y.(float64)
}

func (OperatorImpl) FLt(x any, y any) any {
	return x.(float64) < y.(float64)
}

func (OperatorImpl) FLe(x any, y any) any {
	return x.(float64) <= y.(float64)
}

func (OperatorImpl) FGt(x any, y any) any {
	return x.(float64) > y.(float64)
}

func (OperatorImpl) FGe(x any, y any) any {
	return x.(float64) >= y.(float64)
}

func (OperatorImpl) FNeg(x any) any {
	return -x.(float64)
}

// ------------
// Bools
// ------------

func (OperatorImpl) And(x any, y any) any {
	return x.(bool) && y.(bool)
}

func (OperatorImpl) Or(x any, y any) any {
	return x.(bool) || y.(bool)
}

func (OperatorImpl) Eq(x any, y any) any {
	return OverloadImpl("equals", []any{x, y})
}

func (OperatorImpl) Ne(x any, y any) any {
	return !Ops.Eq(x, y).(bool)
}

func (OperatorImpl) Not(x any) any {
	return !x.(bool)
}

// ------------
// Data structures
// ------------

func List(elems ...any) any {
	xs := immutable.NewListBuilder()

	for _, v := range elems {
		xs.Append(v)
	}

	return xs.List()
}

func listIter(iter *immutable.ListIterator) any {
	if iter.Done() {
		return Create("Seq::Nil")
	}

	_, value := iter.Next()
	return Create2("Seq::Cons", value, func() any {
		return listIter(iter)
	})
}

type ListToSeq struct {
	list  *immutable.List
	index int
}

func (sq *ListToSeq) Cons() any {
	if sq.list.Len() == sq.index {
		return Create("Seq::Nil")
	}

	value := sq.list.Get(sq.index)
	new_sq := ListToSeq{list: sq.list, index: sq.index + 1}

	return Create2("Seq::Cons", value, func() any {
		return new_sq.Cons()
	})
}

func list_to_seq(xs *immutable.List) any {
	seqable := ListToSeq{list: xs, index: 0}
	return seqable.Cons()
}

func mapIter(iter *immutable.MapIterator) any {
	if iter.Done() {
		return Create("Seq::Nil")
	}

	key, value := iter.Next()
	tuple := Create2("Tuple2", key, value)

	return Create2("Seq::Cons", tuple, func() any {
		return mapIter(iter)
	})
}

func sliceToSeq(s_ any) any {
	s := s_.([]any)
	if len(s) == 0 {
		return Create("Seq::Nil")
	}

	return Create2("Seq::Cons", s[0], func() any { return sliceToSeq(s[1:]) })
}

// Specialized to string
func strSliceToSeq(s_ any) any {
	s := s_.([]string)
	if len(s) == 0 {
		return Create("Seq::Nil")
	}

	return Create2("Seq::Cons", s[0], func() any { return strSliceToSeq(s[1:]) })
}

func Debug(v any) {
	// s := fmt.Sprintf("%s -> %v", reflect.TypeOf(v), v)
	s := fmt.Sprintf("%+v", v)
	borgo.debug_output_fn(s)
}

// Hashing related functions.
// These will be replaced by user-land trait implementations when available.
type hashTraitHasher struct{}

func (h *hashTraitHasher) Hash(key any) uint32 {
	return OverloadImpl("to_hash", []any{key}).(uint32)
}

func (h *hashTraitHasher) Equal(a, b any) bool {
	return h.Hash(a) == h.Hash(b)
}

// Reuse Display trait implementation to generate an hash.
// This is inefficient but ok for now.
func builtinHashImpl(key any) uint32 {
	algorithm := fnv.New32a()

	var text string
	if s, ok := key.(string); ok {
		text = s
	} else {
		text = genericToString(key)
	}

	algorithm.Write([]byte(text))
	return algorithm.Sum32()
}

// ------------
// Display trait implementation
// ------------

type FieldData struct {
	name  string
	value any
}

func genericToString(value any) string {
	return OverloadImpl("to_string", []any{value}).(string)
}

// The first char in field names is turned to uppercase during codegen.
// Restore it to lowercase when printing a struct
func revertStructFieldName(s string) string {
	return strings.ToLower(string(s[0])) + s[1:]
}

// ------------
// Initialize Core Native functions & Effects
// ------------

func InitCore() {

	RegisterStruct("Unit", "Unit", []string{})
	RegisterMakeFunction("Unit", make_Unit)

	RegisterGlobalFunction("Option::unwrap", func(o any) any {
		if ValuesIsOfType(o, "Option::Some") {
			return GetArg(o, 0)
		}

		borgo.debug_output_fn("UNWRAP NONE!")
		borgo.debug_output_fn("--------------")
		panic("unwrapped None value")
	})

	RegisterGlobalFunction("Result::unwrap", func(o any) any {
		if ValuesIsOfType(o, "Result::Ok") {
			return GetArg(o, 0)
		}

		borgo.debug_output_fn("UNWRAP ERR!")
		borgo.debug_output_fn("--------------")
		Debug(o)
		panic("unwrapped Err value")
	})

	RegisterGlobalFunction("List::len", func(xs_ any) any {
		xs := xs_.(*immutable.List)
		return xs.Len()
	})

	RegisterGlobalFunction("List::push", func(xs_ any, v any) any {
		xs := xs_.(*immutable.List)
		return xs.Append(v)
	})

	RegisterGlobalFunction("List::pop", func(xs_ any) any {
		xs := xs_.(*immutable.List)
		count := xs.Len()
		if count == 0 {
			return xs
		}

		return xs.Slice(0, count-1)
	})

	RegisterGlobalFunction("List::get", func(xs_ any, index_ any) any {
		xs := xs_.(*immutable.List)
		index := index_.(int)
		l := xs.Len()

		if index < 0 || index >= l {
			return Create("Option::None")
		}

		return Create1("Option::Some", xs.Get(index))
	})

	RegisterGlobalFunction("List::seq", func(xs_ any) any {
		xs := xs_.(*immutable.List)
		return list_to_seq(xs)
	})

	// ------------
	// Strings
	// ------------

	RegisterGlobalFunction("String::len", func(s any) any {
		return len(s.(string))
	})

	RegisterGlobalFunction("String::append", func(s any, other any) any {
		return s.(string) + other.(string)
	})

	RegisterGlobalFunction("String::split", func(s_ any, sep_ any) any {
		s := s_.(string)
		sep := sep_.(string)
		slice := strings.Split(s, sep)
		return strSliceToSeq(slice)
	})

	RegisterGlobalFunction("String::slice", func(s_ any, start_ any, end_ any) any {
		s := s_.(string)
		start := start_.(int)
		end := end_.(int)

		limit := len(s)

		if start < 0 {
			start = 0
		}

		if end < 0 {
			end = limit + end
		}

		if start > limit {
			return ""
		}

		if end > limit {
			end = limit
		}

		return s[start:end]
	})

	RegisterGlobalFunction("String::chars", func(s any) any {
		slice := []any{}

		for _, c := range s.(string) {
			slice = append(slice, c)
		}

		return sliceToSeq(slice)
	})

	RegisterGlobalFunction("String::contains", func(s any, needle any) any {
		return strings.Contains(s.(string), needle.(string))
	})

	RegisterGlobalFunction("String::starts_with", func(s any, other any) any {
		return strings.HasPrefix(s.(string), other.(string))
	})

	RegisterGlobalFunction("String::ends_with", func(s any, other any) any {
		return strings.HasSuffix(s.(string), other.(string))
	})

	RegisterGlobalFunction("String::index_of", func(s any, needle any) any {
		index := strings.Index(s.(string), needle.(string))

		if index == -1 {
			return Create("Option::None")
		}

		return Create1("Option::Some", index)
	})

	RegisterGlobalFunction("String::parse_int", func(s any) any {
		v, e := strconv.Atoi(s.(string))
		if e != nil {
			return Create("Option::None")
		}

		return Create1("Option::Some", v)
	})

	RegisterGlobalFunction("String::parse_float", func(s any) any {
		v, e := strconv.ParseFloat(s.(string), 64)
		if e != nil {
			return Create("Option::None")
		}

		return Create1("Option::Some", v)
	})

	RegisterGlobalFunction("Char::to_int", func(c any) any {
		return int(c.(rune))
	})

	RegisterGlobalFunction("Char::to_unquoted_string", func(c any) any {
		return string(c.(rune))
	})

	// ------------
	// Refs
	// ------------

	type Internal_Ref struct {
		inner any
	}

	RegisterEffectFunction("ref_new", func(inner any) any {
		return &Internal_Ref{inner}
	})

	RegisterEffectFunction("ref_get", func(ref any) any {
		return ref.(*Internal_Ref).inner
	})

	RegisterEffectFunction("ref_set", func(ref any, inner any) any {
		ref.(*Internal_Ref).inner = inner
		return nil
	})

	// ------------
	// Maps
	// ------------

	immutable.SetDefaultHasher(func(key any) immutable.Hasher {
		// TODO this should call TraitImpl or something
		return &hashTraitHasher{}

	})

	RegisterGlobalFunction("Map::new", func() any {
		return immutable.NewMap(nil)
	})

	RegisterGlobalFunction("Map::insert", func(m_ any, k any, v any) any {
		m := m_.(*immutable.Map)
		return m.Set(k, v)
	})

	RegisterGlobalFunction("Map::len", func(m_ any) any {
		m := m_.(*immutable.Map)
		return m.Len()
	})

	RegisterGlobalFunction("Map::get", func(m_ any, k any) any {
		m := m_.(*immutable.Map)
		value, ok := m.Get(k)

		if !ok {
			return Create("Option::None")
		}

		return Create1("Option::Some", value)
	})

	RegisterGlobalFunction("Map::delete", func(m_ any, k any) any {
		m := m_.(*immutable.Map)
		return m.Delete(k)
	})

	RegisterGlobalFunction("Map::seq", func(m_ any) any {
		m := m_.(*immutable.Map)
		iter := m.Iterator()
		return mapIter(iter)
	})

	// ----

	RegisterEffectFunction("channel_new", func() any {
		return make(chan any)
	})

	RegisterEffectFunction("channel_recv", func(ch any) any {
		value, more := <-ch.(chan any)
		if more {
			return Create1("Option::Some", value)
		} else {
			return Create("Option::None")
		}
	})

	RegisterEffectFunction("channel_send", func(ch any, value any) any {
		ch.(chan any) <- value
		return make_Unit
	})

	// ------------
	// Debug and Tests
	// ------------

	RegisterGlobalFunction("Internal::unsafe_coerce", func(v any) any {
		return v
	})

	RegisterGlobalFunction("Debug::inspect", func(v any) any {
		s := borgo.overloadImplementation("to_string", []any{v})
		borgo.debug_output_fn(s.(string))
		return v
	})

	RegisterGlobalFunction("Debug::unreachable", func() any {
		borgo.debug_output_fn("UNREACHABLE!")
		borgo.debug_output_fn("--------------")
		panic("unreachable!()")
	})

	RegisterGlobalFunction("Debug::todo", func() any {
		borgo.debug_output_fn("TODO!")
		borgo.debug_output_fn("--------------")
		panic("todo!()")
	})

	RegisterGlobalFunction("Debug::assert_eq", func(x any, y any) any {
		if !Ops.Eq(x, y).(bool) {
			borgo.debug_output_fn("ASSERT FAILED!")
			borgo.debug_output_fn("--------------")
			borgo.debug_output_fn(genericToString(x))
			borgo.debug_output_fn(genericToString(y))
			borgo.debug_output_fn("--------------")
			log.Fatalln("assert failed")
		}

		return make_Unit
	})

	// ------------
	// Native to_string implementations
	// ------------
	RegisterGlobalFunction("Int::to_string", func(v any) any {
		return fmt.Sprintf("%d", v.(int))
	})

	RegisterGlobalFunction("String::to_string", func(v any) any {
		return "\"" + v.(string) + "\""
	})

	RegisterGlobalFunction("Bool::to_string", func(v any) any {
		return fmt.Sprintf("%v", v)
	})

	RegisterGlobalFunction("Char::to_string", func(v any) any {
		return strconv.QuoteRune(v.(int32))
	})

	RegisterGlobalFunction("Float::to_string", func(v any) any {
		return fmt.Sprintf("%.2f", v)
	})

	RegisterGlobalFunction("$Function::to_string", func(v any) any {
		return "<function>"
	})

	RegisterGlobalFunction("Hash::to_string", func(v any) any {
		return fmt.Sprintf("%d", v)
	})

	// ------------
	// Native to_hash implementations
	// ------------
	RegisterGlobalFunction("Int::to_hash", func(v any) any {
		return uint32(v.(int))
	})

	RegisterGlobalFunction("String::to_hash", func(v any) any {
		return builtinHashImpl(v)
	})

	RegisterGlobalFunction("Bool::to_hash", func(v any) any {
		if v.(bool) {
			return 1
		}

		return 0
	})

	RegisterGlobalFunction("Char::to_hash", func(v any) any {
		return uint32(v.(int32))
	})

	RegisterGlobalFunction("Float::to_hash", func(v any) any {
		return builtinHashImpl(v)
	})
}

/// -------
// BorgoValue is used only in the interpreter
/// -------

type BorgoValue struct {
	typ  string
	data map[string]any
}

func (val BorgoValue) GetArg(index int) any {
	return val.GetField("Field" + strconv.Itoa(index))
}

func (val BorgoValue) GetField(field string) any {
	return val.data[field]
}

func (val BorgoValue) ValueIsOfType(typ string) bool {
	return val.typ == typ
}

func makeBorgoValue(typ string, fields ...any) any {
	data := map[string]any{}

	for i, value := range fields {
		data["Field"+strconv.Itoa(i)] = value
	}

	return BorgoValue{typ, data}
}

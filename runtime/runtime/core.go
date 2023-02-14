package borgo

import (
	"fmt"
	"hash/fnv"
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
	borgo.effect_functions[name] = fn
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

func TraitImpl(trait_name string, values []any) any {
	switch trait_name {
	case "Eq":
		return Ops.Eq(values[0], values[1])

	case "Hash":
		return genericHash(values[0])

	case "Display":
		return genericToString(values[0])

	default:
		panic("Trait not implemented: " + trait_name)
	}
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
	// TODO this isn't great
	return reflect.DeepEqual(x, y)
}

func (OperatorImpl) Ne(x any, y any) any {
	return !Ops.Eq(x, y).(bool)
}

func (OperatorImpl) Not(x any) any {
	return !x.(bool)
}

func compareLists(x any, y any) any {
	xs := x.(*immutable.List)
	ys := y.(*immutable.List)

	if xs.Len() != ys.Len() {
		return false
	}

	x_iter := xs.Iterator()
	y_iter := xs.Iterator()

	for !x_iter.Done() {
		_, a := x_iter.Next()
		_, b := y_iter.Next()

		if !Ops.Eq(a, b).(bool) {
			return false
		}
	}

	return true
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
	s := fmt.Sprintf("%v", v)
	borgo.debug_output_fn(s)
}

// Hashing related functions.
// These will be replaced by user-land trait implementations when available.
type hashTraitHasher struct{}

func (h *hashTraitHasher) Hash(key any) uint32 {
	return genericHash(key)
}

func (h *hashTraitHasher) Equal(a, b any) bool {
	return h.Hash(a) == h.Hash(b)
}

// Reuse Display trait implementation to generate an hash.
// This is inefficient but ok for now.
func genericHash(key any) uint32 {
	algorithm := fnv.New32a()
	text := genericToString(key)
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
	ref := reflect.TypeOf(value)
	ref_value := reflect.ValueOf(value)

	switch ref.Kind() {
	case reflect.Int:
		return fmt.Sprintf("%d", value)
	case reflect.Float64:
		return fmt.Sprintf("%.2f", value)
	case reflect.Bool:
		return fmt.Sprintf("%v", value)
	case reflect.String:
		return "\"" + value.(string) + "\""
	case reflect.Func:
		return "<function>"

	case reflect.Int32:
		// this is not great
		return strconv.QuoteRune(value.(int32))

	case reflect.Ptr:
		// Follow the pointer
		inner := reflect.Indirect(ref_value).Interface()
		name := reflect.TypeOf(inner).Name()

		if name == "List" {
			return listToString(value.(*immutable.List))
		}

		if name == "Map" {
			return mapToString(value.(*immutable.Map))
		}

		if name == "Internal_Ref" {
			return "<ref>"
		}

		panic("unhandled pointer type " + name)

	case reflect.Struct:
		name := ref.Name()

		// Unit type
		if name == "" {
			return "()"
		}

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

		// Special case Seq
		// Realize the first N elements and turn into a list
		if type_rep.name == "Seq::Cons" {
			elems_to_render := 30
			slice := Call(GetNative2("Seq::take"), []any{value, elems_to_render})
			list := Call(GetNative1("Seq::to_list"), []any{slice})
			return genericToString(list)
		}

		// Special case Set
		if type_rep.name == "Set" {
			// Get `m`, which is the inner Map in the Set
			m := GetField(value, "m")
			s := Call(GetNative1("Map::seq_keys"), []any{m})
			list := Call(GetNative1("Seq::to_list"), []any{s})
			return genericToString(list)
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

			if isTuple {
				return "(" + strings.Join(fields, ", ") + ")"
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

		panic("not a struct and not an enum?")
	}

	Debug(value)
	Debug(ref.Kind())
	panic("unimplemented to string")
}

// The first char in field names is turned to uppercase during codegen.
// Restore it to lowercase when printing a struct
func revertStructFieldName(s string) string {
	return strings.ToLower(string(s[0])) + s[1:]
}

func listToString(xs *immutable.List) string {
	iter := xs.Iterator()
	elems := []string{}

	for !iter.Done() {
		_, x := iter.Next()
		elems = append(elems, genericToString(x))
	}

	return "[" + strings.Join(elems, ", ") + "]"
}

func mapToString(m *immutable.Map) string {
	iter := m.Iterator()
	elems := []string{}

	for !iter.Done() {
		key, value := iter.Next()

		s := genericToString(key) + " => " + genericToString(value)
		elems = append(elems, s)
	}

	return "{ " + strings.Join(elems, ", ") + " }"
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

	RegisterGlobalFunction("Char::to_string", func(c any) any {
		return string(c.(rune))
	})

	// ------------
	// Refs
	// ------------

	type Internal_Ref struct {
		inner any
	}

	RegisterGlobalFunction("ref_new", func(inner any) any {
		return &Internal_Ref{inner}
	})

	RegisterGlobalFunction("ref_get", func(ref any) any {
		return ref.(*Internal_Ref).inner
	})

	RegisterGlobalFunction("ref_set", func(ref any, inner any) any {
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

	RegisterGlobalFunction("Loop::start", func(initial any, f_ any) any {
		current := initial

		for {
			next := Call(f_, []any{current})

			if ValuesIsOfType(next, "Loop::Done") {
				return GetArg(next, 0)
			}

			if ValuesIsOfType(next, "Loop::Recur") {
				current = GetArg(next, 0)
				continue
			}

			borgo.debug_output_fn("LOOP FAILED!")
			borgo.debug_output_fn("--------------")
			panic("broken")
		}

	})

	// ----

	RegisterGlobalFunction("channel_new", func() any {
		return make(chan any)
	})

	RegisterGlobalFunction("channel_recv", func(ch any) any {
		value, more := <-ch.(chan any)
		if more {
			return Create1("Option::Some", value)
		} else {
			return Create("Option::None")
		}
	})

	RegisterGlobalFunction("channel_send", func(ch any, value any) any {
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
		borgo.debug_output_fn(genericToString(v))
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
			panic("assert failed")
		}

		return make_Unit
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

package borgo

import (
	"borgo/immutable"
	"errors"

	"io"
	"os"
	"reflect"
)

func Assert(e any) {
	if e != nil {
		Debug(e)
		panic("Assert failed")
	}
}

type ReturnValue struct {
	inner any
}

func isReturn(x any) bool {
	_, ok := x.(ReturnValue)
	return ok
}

func unwrapReturn(x any) any {
	if ret, ok := x.(ReturnValue); ok {
		return ret.inner
	}

	return x
}

type Eval struct {
	globals *Globals
	env     *immutable.Map
}

// This type is necessary so we can share the pointer to this struct across all Eval instances
type Globals struct {
	values *immutable.Map
}

func CreateEvaluator() Eval {
	hasher := immutable.NewHasher("yo")
	globals := Globals{values: immutable.NewMap(hasher)}
	env := immutable.NewMap(hasher)
	return Eval{&globals, env}
}

// Keep a reference to the existing `globals` map, but clone `env`
func (eval *Eval) beginScope() *Eval {
	new_env := *eval.env
	return &Eval{globals: eval.globals, env: &new_env}
}

func (eval *Eval) setVariable(k string, v any) {
	eval.env = eval.env.Set(k, v)
}

func (eval *Eval) lookupVariable(k string) (any, bool) {
	value, ok := eval.env.Get(k)
	if ok {
		return value, ok
	}

	return eval.globals.values.Get(k)
}

func (eval *Eval) addGlobal(k string, v any) {
	eval.globals.values = eval.globals.values.Set(k, v)
}

func (eval *Eval) putPatternInScope(pat Pat, value any) {
	switch pat := pat.(type) {
	case *Pat__Type:
		eval.setVariable(pat.Ident, value)

	case *Pat__Struct:
		value := value.(BorgoValue)
		for _, field := range pat.Fields {
			eval.putPatternInScope(field.Value, value.GetField(field.Name))
		}

	case *Pat__Wild:
		// Nothing to do

	default:
		Debug(pat)
		panic("unhandled pat")
	}
}

func (eval *Eval) exitScope() {
	// Nothing to do here, might even remove it. Right?
}

func (eval *Eval) matchArm(subject any, arm Arm) (any, bool) {
	subs := map[string]any{}
	ret := eval.matchPattern(subject, arm.Pat, subs)

	if isMatching(ret) {
		scope := eval.beginScope()
		for k, v := range subs {
			scope.setVariable(k, v)
		}
		expr := scope.Run(arm.Expr)

		return expr, true
	}

	return nil, false
}

type MatchResult int

const (
	MatchNone MatchResult = iota
	MatchOk
)

func isMatching(m MatchResult) bool {
	return m == MatchOk
}

func (eval *Eval) matchPattern(subject any, pat Pat, subs map[string]any) MatchResult {
	switch pat := pat.(type) {
	case *Pat__Lit:
		expr := Expr__Literal{Lit: pat.Lit}
		p := eval.Run(&expr)

		if Ops.Eq(p, subject).(bool) {
			return MatchOk
		}

		return MatchNone

	case *Pat__Pat:
		subject := subject.(BorgoValue)
		if subject.typ == pat.Ident {
			for i, nested_pat := range pat.Elems {
				matching := eval.matchPattern(subject.GetArg(i), nested_pat, subs)
				if !isMatching(matching) {
					return MatchNone
				}
			}

			return MatchOk
		}

		return MatchNone

	case *Pat__Type:
		subs[pat.Ident] = subject
		return MatchOk

	case *Pat__Struct:
		subject := subject.(BorgoValue)
		for _, field := range pat.Fields {
			matching := eval.matchPattern(subject.GetField(field.Name), field.Value, subs)
			if !isMatching(matching) {
				return MatchNone
			}
		}

		return MatchOk

	case *Pat__Wild:
		return MatchOk

	case *Pat__Unit:
		if Ops.Eq(make_Unit, subject).(bool) {
			return MatchOk
		}

		return MatchNone

	}

	Debug(pat)
	panic("match pattern")
}

type ToLiteralValue interface {
	LiteralValue(eval *Eval) any
}

func (lit *Literal__List) LiteralValue(eval *Eval) any {
	xs := immutable.NewListBuilder()

	for _, e := range *lit {
		value := eval.Run(e)
		if isReturn(value) {
			return value
		}

		xs.Append(value)
	}

	return xs.List()
}

func (lit *Literal__Bool) LiteralValue(_ *Eval) any {
	return bool(*lit)
}

func (lit *Literal__Int) LiteralValue(_ *Eval) any {
	return int(*lit)
}

func (lit *Literal__Float) LiteralValue(_ *Eval) any {
	return float64(*lit)
}

func (lit *Literal__String) LiteralValue(_ *Eval) any {
	return string(*lit)
}

func (lit *Literal__Char) LiteralValue(_ *Eval) any {
	return rune(string(*lit)[0])
}

func (eval *Eval) Run(expr Expr) any {
	switch expr := expr.(type) {

	case *Expr__Closure:
		capturedEnv := eval.beginScope()

		closure := func(args ...any) any {
			// This seems silly, but ensures that the original `capturedEnv` is preserved across function calls
			scope := capturedEnv.beginScope()

			for i, binding := range expr.Fun.Args {
				scope.putPatternInScope(binding.Pat, args[i])
			}

			result := scope.Run(expr.Fun.Body)
			return unwrapReturn(result)
		}

		switch expr.Kind.(type) {
		case *FunctionKind__TopLevel:
			eval.addGlobal(expr.Fun.Name, closure)
			registerGlobalHackyArity(expr.Fun.Name, closure, len(expr.Fun.Args))
			return make_Unit

		case *FunctionKind__Inline:
			capturedEnv.setVariable(expr.Fun.Name, closure) // This allows recursive functions
			eval.setVariable(expr.Fun.Name, closure)
			return closure

		case *FunctionKind__Lambda:
			return closure

		default:
			panic("unhandled closure kind")
		}

	case *Expr__Block:
		var last any

		scope := eval.beginScope()

		for _, s := range expr.Stmts {
			last = scope.Run(s)
			if isReturn(last) {
				return last
			}
		}

		return last

	case *Expr__Match:
		subject := eval.Run(expr.Subject)
		if isReturn(subject) {
			return subject
		}

		for _, arm := range expr.Arms {
			if res, ok := eval.matchArm(subject, arm); ok {
				return res
			}
		}

		Debug(subject)
		panic("non exhaustive pattern match")

	case *Expr__Literal:
		if lit, ok := expr.Lit.(ToLiteralValue); ok {
			return lit.LiteralValue(eval)
		}

		Debug(expr.Lit)
		panic("ToLiteralValue not implemented")

	case *Expr__Call:
		fun := eval.Run(expr.Func)
		if isReturn(fun) {
			return fun
		}

		args := []any{}

		for _, e := range expr.Args {
			value := eval.Run(e)
			if isReturn(value) {
				return value
			}

			args = append(args, value)
		}

		return Call(fun, args)

	case *Expr__Var:
		value, found := eval.lookupVariable(expr.Value)
		if !found {
			Debug(expr.Span)
			panic("unknown var value: " + expr.Value)
		}

		return value

	case *Expr__Let:
		value := eval.Run(expr.Value)
		if isReturn(value) {
			return value
		}

		eval.putPatternInScope(expr.Binding.Pat, value)
		return make_Unit

	case *Expr__Debug:
		switch expr.Kind.(type) {

		case *DebugKind__Inspect:
			fun := Expr__Var{Value: "Debug::inspect"}
			args := []Expr{expr.Expr}
			new_expr := Expr__Call{Func: &fun, Args: args}
			return eval.Run(&new_expr)

		case *DebugKind__Unreachable:
			Debug(expr.Span)
			panic("reached unreachable! code")

		default:
			Debug(expr)
			panic("unhandled debug")
		}

	case *Expr__Binary:
		left := eval.Run(expr.Left)
		if isReturn(left) {
			return left
		}

		// bit of a hack here, clean this up when function overloading is implemented
		switch expr.Op.(type) {
		case *Operator__Or:
			if left.(bool) {
				return left
			}

			right := eval.Run(expr.Right)
			return right

		case *Operator__And:
			if !left.(bool) {
				return false
			}

			right := eval.Run(expr.Right)
			return right

		case *Operator__Eq:
			right := eval.Run(expr.Right)
			if isReturn(right) {
				return right
			}

			return Ops.Eq(left, right)

		case *Operator__Ne:
			right := eval.Run(expr.Right)
			if isReturn(right) {
				return right
			}

			eq := Ops.Eq(left, right).(bool)
			return !eq

		}

		// Not a boolean operator, can evaluate `right` eagerly
		// Match all remaining numeric operators
		right := eval.Run(expr.Right)
		if isReturn(right) {
			return right
		}

		var prefix string

		switch left.(type) {
		case int:
			prefix = "I"
		case float64:
			prefix = "F"
		default:
			panic("unknown prefix")
		}

		var method string

		switch expr.Op.(type) {
		case *Operator__Add:
			method = "Add"
		case *Operator__Sub:
			method = "Sub"
		case *Operator__Mul:
			method = "Mul"
		case *Operator__Div:
			method = "Div"
		case *Operator__Rem:
			method = "Rem"
		case *Operator__Lt:
			method = "Lt"
		case *Operator__Le:
			method = "Le"
		case *Operator__Gt:
			method = "Gt"
		case *Operator__Ge:
			method = "Ge"
		default:
			Debug(expr.Op)
			panic("unknown operator")
		}

		fun := reflect.ValueOf(Ops).MethodByName(prefix + method).Interface()
		return Call(fun, []any{left, right})

	case *Expr__If:
		cond := eval.Run(expr.Cond)
		if isReturn(cond) {
			return cond
		}

		if cond.(bool) {
			return eval.Run(expr.Then)
		}

		if expr.Els != nil {
			return eval.Run(expr.Els)
		}

		return make_Unit

	case *Expr__EnumDef:
		def := expr.Def

		for _, cons := range def.Cons {
			qualifiedName := def.Name + "::" + cons.Name

			var makeConstructorFn any

			// For each arity, create a specific function.
			// Not great, maybe there's a better way.

			switch len(cons.Fields) {
			case 0:
				makeConstructorFn = makeBorgoValue(qualifiedName)

			case 1:
				makeConstructorFn = func(f0 any) any {
					return makeBorgoValue(qualifiedName, f0)
				}

			case 2:
				makeConstructorFn = func(f0 any, f1 any) any {
					return makeBorgoValue(qualifiedName, f0, f1)
				}

			case 3:
				makeConstructorFn = func(f0 any, f1 any, f2 any) any {
					return makeBorgoValue(qualifiedName, f0, f1, f2)
				}

			default:
				panic("unhandled arity")
			}

			/*
				if len(cons.Fields) == 0 {
					makeConstructorFn = makeBorgoValue(qualifiedName, nil)
				} else {
					makeConstructorFn = func(fields ...any) any {
						return makeBorgoValue(qualifiedName, fields)
					}
				}
			*/

			// Like in infer, also alias the constructor name
			// So the constructor is aliased twice (ie. None and Option::None)
			eval.addGlobal(qualifiedName, makeConstructorFn)
			eval.addGlobal(cons.Name, makeConstructorFn)

			// Register this function as a make function so that CreateN() methods work
			RegisterMakeFunction(qualifiedName, makeConstructorFn)
			RegisterTypeConstructor(qualifiedName, qualifiedName)
			RegisterTypeConstructor(qualifiedName, cons.Name)
		}

		return make_Unit

	case *Expr__StructDef:
		var makeConstructorFn any

		// For each arity, create a specific function.
		// Not great, maybe there's a better way.
		qualifiedName := expr.Def.Name
		fields := expr.Def.Fields

		switch len(fields) {
		case 0:
			makeConstructorFn = makeBorgoValue(qualifiedName)

		case 1:
			makeConstructorFn = func(f0 any) any {
				data := map[string]any{}
				data[fields[0].Name] = f0
				return BorgoValue{qualifiedName, data}
			}

		case 2:
			makeConstructorFn = func(f0 any, f1 any) any {
				data := map[string]any{}
				data[fields[0].Name] = f0
				data[fields[1].Name] = f1
				return BorgoValue{qualifiedName, data}
			}

		case 3:
			makeConstructorFn = func(f0 any, f1 any, f2 any) any {
				data := map[string]any{}
				data[fields[0].Name] = f0
				data[fields[1].Name] = f1
				data[fields[2].Name] = f2
				return BorgoValue{qualifiedName, data}
			}

		default:
			panic("unhandled arity")
		}

		RegisterMakeFunction(qualifiedName, makeConstructorFn)
		RegisterStruct(qualifiedName, qualifiedName)

		// If it's a trait, register the trait function as well
		if expr.IsTrait {
			field := expr.Def.Fields[0]
			traitFn := qualifiedName + "::" + field.Name
			fun := func(values ...any) any {
				return TraitImpl(qualifiedName, values)
			}

			eval.addGlobal(traitFn, fun)
		}

		return make_Unit

	case *Expr__StructCall:
		data := map[string]any{}

		if expr.Rest != nil {
			rest := eval.Run(*expr.Rest)
			if isReturn(rest) {
				return rest
			}

			for k, v := range rest.(BorgoValue).data {
				data[k] = v
			}
		}

		for _, f := range expr.Fields {
			value := eval.Run(f.Value)
			if isReturn(value) {
				return value
			}

			data[f.Name] = value
		}

		return BorgoValue{typ: expr.Name, data: data}

	case *Expr__StructAccess:
		value := eval.Run(expr.Expr)
		if isReturn(value) {
			return value
		}

		return value.(BorgoValue).data[expr.Field]

	case *Expr__Paren:
		value := eval.Run(expr.Expr)
		return value

	case *Expr__Return:
		value := eval.Run(expr.Expr)
		return ReturnValue{inner: value}

	case *Expr__Unary:
		value := eval.Run(expr.Expr)
		if isReturn(value) {
			return value
		}

		var method string

		switch expr.Op.(type) {
		case *UnOp__Not:
			method = "Not"

		case *UnOp__Neg:
			switch value.(type) {
			case int:
				method = "INeg"
			case float64:
				method = "FNeg"
			default:
				panic("unknown prefix")
			}
		}

		fun := reflect.ValueOf(Ops).MethodByName(method).Interface()
		return Call(fun, []any{value})

	case *Expr__Try:
		value := eval.Run(expr.Expr)
		if isReturn(value) {
			return value
		}

		v := value.(BorgoValue)
		if v.ValueIsOfType("Result::Err") {
			return ReturnValue{inner: v}
		}

		if v.ValueIsOfType("Result::Ok") {
			return v.GetArg(0)
		}

		panic("broken Try")

	case *Expr__ExternDecl:
		var target map[string]any

		switch expr.Kind.(type) {
		case *ExternKind__Native:
			target = borgo.global_functions
		case *ExternKind__Effect:
			target = borgo.global_functions
			// TODO they're all in the same bucket for now
			// target = borgo.effect_functions
		}

		for _, decl := range expr.Items {
			closure := decl.(*Expr__Closure)
			name := closure.Fun.Name
			fun, ok := target[name]
			if !ok {
				panic("native function not found " + name)
			}

			eval.addGlobal(name, fun)
		}

		return make_Unit

	case *Expr__ImplBlock:
		for _, decl := range expr.Items {
			eval.Run(decl)
		}

		return make_Unit

	case *Expr__Unit:
		return make_Unit

	case *Expr__Const:
		value := eval.Run(expr.Expr)
		eval.addGlobal(expr.Ident, value)
		return make_Unit

	case *Expr__Spawn:
		// TODO don't handle concurrency for now
		os.Exit(0)
		return nil

	case *Expr__Noop:
		return make_Unit

	default:
		Debug(expr)
		panic("unhandled expr")
	}
}

func ParseProject(input []byte) Project {
	project, err := BincodeDeserializeProject(input)
	if err != io.EOF {
		Assert(err)
	}

	return project
}

func RunMainFunction(project Project) error {
	InitCore()

	eval := CreateEvaluator()

	packages := []string{"std", "user"}

	for _, pkg := range packages {
		for _, file := range project.Packages[pkg].Files {
			for _, expr := range file.Decls {
				eval.Run(expr)
			}
		}
	}

	if _, ok := eval.lookupVariable("borgo_main"); !ok {
		return errors.New("Function `borgo_main` not found")
	}

	call := Expr__Call{Func: &Expr__Var{Value: "borgo_main"}}
	eval.Run(&call)
	return nil
}

// TODO This is disgusting, there's probably a better way of going about this.
// This essentially turns a dynamic variadic function into a regular function with N params. It's necessary so that GetNativeN work as expected, but it's really dumb.
func registerGlobalHackyArity(name string, fn func(...any) any, args int) {

	t := reflect.TypeOf(fn)
	switch t.Kind() {
	case reflect.Func:
		if !t.IsVariadic() {
			panic("was expecting fn to have variadic args")
		}

		var ret any

		switch args {
		case 0:
			ret = func() any { return fn() }
		case 1:
			ret = func(x any) any { return fn(x) }
		case 2:
			ret = func(x any, y any) any { return fn(x, y) }
		case 3:
			ret = func(x any, y any, z any) any { return fn(x, y, z) }
		case 4:
			ret = func(x any, y any, z any, a any) any { return fn(x, y, z, a) }
		default:
			panic("unhandled arity")
		}

		RegisterGlobalFunction(name, ret)

	default:
		panic("not a function")
	}

}

package borgo

import (
	"fmt"
	"github.com/novifinancial/serde-reflection/serde-generate/runtime/golang/bincode"
	"github.com/novifinancial/serde-reflection/serde-generate/runtime/golang/serde"
)

type ArityError struct {
	Expected []Type
	Actual   []Type
	Span     Span
}

func (obj *ArityError) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	if err := serialize_vector_Type(obj.Expected, serializer); err != nil {
		return err
	}
	if err := serialize_vector_Type(obj.Actual, serializer); err != nil {
		return err
	}
	if err := obj.Span.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *ArityError) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func DeserializeArityError(deserializer serde.Deserializer) (ArityError, error) {
	var obj ArityError
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := deserialize_vector_Type(deserializer); err == nil {
		obj.Expected = val
	} else {
		return obj, err
	}
	if val, err := deserialize_vector_Type(deserializer); err == nil {
		obj.Actual = val
	} else {
		return obj, err
	}
	if val, err := DeserializeSpan(deserializer); err == nil {
		obj.Span = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

func BincodeDeserializeArityError(input []byte) (ArityError, error) {
	if input == nil {
		var obj ArityError
		return obj, fmt.Errorf("Cannot deserialize null array")
	}
	deserializer := bincode.NewDeserializer(input)
	obj, err := DeserializeArityError(deserializer)
	if err == nil && deserializer.GetBufferOffset() < uint64(len(input)) {
		return obj, fmt.Errorf("Some input bytes were not read")
	}
	return obj, err
}

type Arm struct {
	Pat  Pat
	Expr Expr
}

func (obj *Arm) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	if err := obj.Pat.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Expr.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Arm) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func DeserializeArm(deserializer serde.Deserializer) (Arm, error) {
	var obj Arm
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := DeserializePat(deserializer); err == nil {
		obj.Pat = val
	} else {
		return obj, err
	}
	if val, err := DeserializeExpr(deserializer); err == nil {
		obj.Expr = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

func BincodeDeserializeArm(input []byte) (Arm, error) {
	if input == nil {
		var obj Arm
		return obj, fmt.Errorf("Cannot deserialize null array")
	}
	deserializer := bincode.NewDeserializer(input)
	obj, err := DeserializeArm(deserializer)
	if err == nil && deserializer.GetBufferOffset() < uint64(len(input)) {
		return obj, fmt.Errorf("Some input bytes were not read")
	}
	return obj, err
}

type Binding struct {
	Pat Pat
	Ann TypeAst
	Ty  Type
}

func (obj *Binding) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	if err := obj.Pat.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Ann.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Ty.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Binding) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func DeserializeBinding(deserializer serde.Deserializer) (Binding, error) {
	var obj Binding
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := DeserializePat(deserializer); err == nil {
		obj.Pat = val
	} else {
		return obj, err
	}
	if val, err := DeserializeTypeAst(deserializer); err == nil {
		obj.Ann = val
	} else {
		return obj, err
	}
	if val, err := DeserializeType(deserializer); err == nil {
		obj.Ty = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

func BincodeDeserializeBinding(input []byte) (Binding, error) {
	if input == nil {
		var obj Binding
		return obj, fmt.Errorf("Cannot deserialize null array")
	}
	deserializer := bincode.NewDeserializer(input)
	obj, err := DeserializeBinding(deserializer)
	if err == nil && deserializer.GetBufferOffset() < uint64(len(input)) {
		return obj, fmt.Errorf("Some input bytes were not read")
	}
	return obj, err
}

type Constructor struct {
	Name   string
	Fields []EnumFieldDef
}

func (obj *Constructor) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	if err := serializer.SerializeStr(obj.Name); err != nil {
		return err
	}
	if err := serialize_vector_EnumFieldDef(obj.Fields, serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Constructor) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func DeserializeConstructor(deserializer serde.Deserializer) (Constructor, error) {
	var obj Constructor
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := deserializer.DeserializeStr(); err == nil {
		obj.Name = val
	} else {
		return obj, err
	}
	if val, err := deserialize_vector_EnumFieldDef(deserializer); err == nil {
		obj.Fields = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

func BincodeDeserializeConstructor(input []byte) (Constructor, error) {
	if input == nil {
		var obj Constructor
		return obj, fmt.Errorf("Cannot deserialize null array")
	}
	deserializer := bincode.NewDeserializer(input)
	obj, err := DeserializeConstructor(deserializer)
	if err == nil && deserializer.GetBufferOffset() < uint64(len(input)) {
		return obj, fmt.Errorf("Some input bytes were not read")
	}
	return obj, err
}

type DebugKind interface {
	isDebugKind()
	Serialize(serializer serde.Serializer) error
	BincodeSerialize() ([]byte, error)
}

func DeserializeDebugKind(deserializer serde.Deserializer) (DebugKind, error) {
	index, err := deserializer.DeserializeVariantIndex()
	if err != nil {
		return nil, err
	}

	switch index {
	case 0:
		if val, err := load_DebugKind__Todo(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 1:
		if val, err := load_DebugKind__Unreachable(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 2:
		if val, err := load_DebugKind__Inspect(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	default:
		return nil, fmt.Errorf("Unknown variant index for DebugKind: %d", index)
	}
}

func BincodeDeserializeDebugKind(input []byte) (DebugKind, error) {
	if input == nil {
		var obj DebugKind
		return obj, fmt.Errorf("Cannot deserialize null array")
	}
	deserializer := bincode.NewDeserializer(input)
	obj, err := DeserializeDebugKind(deserializer)
	if err == nil && deserializer.GetBufferOffset() < uint64(len(input)) {
		return obj, fmt.Errorf("Some input bytes were not read")
	}
	return obj, err
}

type DebugKind__Todo struct {
}

func (*DebugKind__Todo) isDebugKind() {}

func (obj *DebugKind__Todo) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(0)
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *DebugKind__Todo) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_DebugKind__Todo(deserializer serde.Deserializer) (DebugKind__Todo, error) {
	var obj DebugKind__Todo
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type DebugKind__Unreachable struct {
}

func (*DebugKind__Unreachable) isDebugKind() {}

func (obj *DebugKind__Unreachable) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(1)
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *DebugKind__Unreachable) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_DebugKind__Unreachable(deserializer serde.Deserializer) (DebugKind__Unreachable, error) {
	var obj DebugKind__Unreachable
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type DebugKind__Inspect struct {
}

func (*DebugKind__Inspect) isDebugKind() {}

func (obj *DebugKind__Inspect) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(2)
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *DebugKind__Inspect) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_DebugKind__Inspect(deserializer serde.Deserializer) (DebugKind__Inspect, error) {
	var obj DebugKind__Inspect
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Declaration struct {
	FileId FileId
	Span   Span
}

func (obj *Declaration) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	if err := obj.FileId.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Span.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Declaration) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func DeserializeDeclaration(deserializer serde.Deserializer) (Declaration, error) {
	var obj Declaration
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := DeserializeFileId(deserializer); err == nil {
		obj.FileId = val
	} else {
		return obj, err
	}
	if val, err := DeserializeSpan(deserializer); err == nil {
		obj.Span = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

func BincodeDeserializeDeclaration(input []byte) (Declaration, error) {
	if input == nil {
		var obj Declaration
		return obj, fmt.Errorf("Cannot deserialize null array")
	}
	deserializer := bincode.NewDeserializer(input)
	obj, err := DeserializeDeclaration(deserializer)
	if err == nil && deserializer.GetBufferOffset() < uint64(len(input)) {
		return obj, fmt.Errorf("Some input bytes were not read")
	}
	return obj, err
}

type EmittedFile struct {
	Name   string
	Source string
}

func (obj *EmittedFile) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	if err := serializer.SerializeStr(obj.Name); err != nil {
		return err
	}
	if err := serializer.SerializeStr(obj.Source); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *EmittedFile) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func DeserializeEmittedFile(deserializer serde.Deserializer) (EmittedFile, error) {
	var obj EmittedFile
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := deserializer.DeserializeStr(); err == nil {
		obj.Name = val
	} else {
		return obj, err
	}
	if val, err := deserializer.DeserializeStr(); err == nil {
		obj.Source = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

func BincodeDeserializeEmittedFile(input []byte) (EmittedFile, error) {
	if input == nil {
		var obj EmittedFile
		return obj, fmt.Errorf("Cannot deserialize null array")
	}
	deserializer := bincode.NewDeserializer(input)
	obj, err := DeserializeEmittedFile(deserializer)
	if err == nil && deserializer.GetBufferOffset() < uint64(len(input)) {
		return obj, fmt.Errorf("Some input bytes were not read")
	}
	return obj, err
}

type EnumDefinition struct {
	Name     string
	Generics []string
	Cons     []Constructor
}

func (obj *EnumDefinition) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	if err := serializer.SerializeStr(obj.Name); err != nil {
		return err
	}
	if err := serialize_vector_str(obj.Generics, serializer); err != nil {
		return err
	}
	if err := serialize_vector_Constructor(obj.Cons, serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *EnumDefinition) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func DeserializeEnumDefinition(deserializer serde.Deserializer) (EnumDefinition, error) {
	var obj EnumDefinition
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := deserializer.DeserializeStr(); err == nil {
		obj.Name = val
	} else {
		return obj, err
	}
	if val, err := deserialize_vector_str(deserializer); err == nil {
		obj.Generics = val
	} else {
		return obj, err
	}
	if val, err := deserialize_vector_Constructor(deserializer); err == nil {
		obj.Cons = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

func BincodeDeserializeEnumDefinition(input []byte) (EnumDefinition, error) {
	if input == nil {
		var obj EnumDefinition
		return obj, fmt.Errorf("Cannot deserialize null array")
	}
	deserializer := bincode.NewDeserializer(input)
	obj, err := DeserializeEnumDefinition(deserializer)
	if err == nil && deserializer.GetBufferOffset() < uint64(len(input)) {
		return obj, fmt.Errorf("Some input bytes were not read")
	}
	return obj, err
}

type EnumFieldDef struct {
	Name string
	Ann  TypeAst
	Ty   Type
}

func (obj *EnumFieldDef) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	if err := serializer.SerializeStr(obj.Name); err != nil {
		return err
	}
	if err := obj.Ann.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Ty.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *EnumFieldDef) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func DeserializeEnumFieldDef(deserializer serde.Deserializer) (EnumFieldDef, error) {
	var obj EnumFieldDef
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := deserializer.DeserializeStr(); err == nil {
		obj.Name = val
	} else {
		return obj, err
	}
	if val, err := DeserializeTypeAst(deserializer); err == nil {
		obj.Ann = val
	} else {
		return obj, err
	}
	if val, err := DeserializeType(deserializer); err == nil {
		obj.Ty = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

func BincodeDeserializeEnumFieldDef(input []byte) (EnumFieldDef, error) {
	if input == nil {
		var obj EnumFieldDef
		return obj, fmt.Errorf("Cannot deserialize null array")
	}
	deserializer := bincode.NewDeserializer(input)
	obj, err := DeserializeEnumFieldDef(deserializer)
	if err == nil && deserializer.GetBufferOffset() < uint64(len(input)) {
		return obj, fmt.Errorf("Some input bytes were not read")
	}
	return obj, err
}

type Error interface {
	isError()
	Serialize(serializer serde.Serializer) error
	BincodeSerialize() ([]byte, error)
}

func DeserializeError(deserializer serde.Deserializer) (Error, error) {
	index, err := deserializer.DeserializeVariantIndex()
	if err != nil {
		return nil, err
	}

	switch index {
	case 0:
		if val, err := load_Error__Unification(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 1:
		if val, err := load_Error__WrongArity(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 2:
		if val, err := load_Error__VarNotFound(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 3:
		if val, err := load_Error__MethodNotFound(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 4:
		if val, err := load_Error__NotExhaustivePatternMatch(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 5:
		if val, err := load_Error__Generic(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 6:
		if val, err := load_Error__Parse(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	default:
		return nil, fmt.Errorf("Unknown variant index for Error: %d", index)
	}
}

func BincodeDeserializeError(input []byte) (Error, error) {
	if input == nil {
		var obj Error
		return obj, fmt.Errorf("Cannot deserialize null array")
	}
	deserializer := bincode.NewDeserializer(input)
	obj, err := DeserializeError(deserializer)
	if err == nil && deserializer.GetBufferOffset() < uint64(len(input)) {
		return obj, fmt.Errorf("Some input bytes were not read")
	}
	return obj, err
}

type Error__Unification struct {
	Value UnificationError
}

func (*Error__Unification) isError() {}

func (obj *Error__Unification) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(0)
	if err := obj.Value.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Error__Unification) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Error__Unification(deserializer serde.Deserializer) (Error__Unification, error) {
	var obj Error__Unification
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := DeserializeUnificationError(deserializer); err == nil {
		obj.Value = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Error__WrongArity struct {
	Value ArityError
}

func (*Error__WrongArity) isError() {}

func (obj *Error__WrongArity) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(1)
	if err := obj.Value.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Error__WrongArity) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Error__WrongArity(deserializer serde.Deserializer) (Error__WrongArity, error) {
	var obj Error__WrongArity
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := DeserializeArityError(deserializer); err == nil {
		obj.Value = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Error__VarNotFound struct {
	Field0 string
	Field1 Span
}

func (*Error__VarNotFound) isError() {}

func (obj *Error__VarNotFound) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(2)
	if err := serializer.SerializeStr(obj.Field0); err != nil {
		return err
	}
	if err := obj.Field1.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Error__VarNotFound) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Error__VarNotFound(deserializer serde.Deserializer) (Error__VarNotFound, error) {
	var obj Error__VarNotFound
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := deserializer.DeserializeStr(); err == nil {
		obj.Field0 = val
	} else {
		return obj, err
	}
	if val, err := DeserializeSpan(deserializer); err == nil {
		obj.Field1 = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Error__MethodNotFound struct {
	Field0 string
	Field1 Span
}

func (*Error__MethodNotFound) isError() {}

func (obj *Error__MethodNotFound) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(3)
	if err := serializer.SerializeStr(obj.Field0); err != nil {
		return err
	}
	if err := obj.Field1.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Error__MethodNotFound) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Error__MethodNotFound(deserializer serde.Deserializer) (Error__MethodNotFound, error) {
	var obj Error__MethodNotFound
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := deserializer.DeserializeStr(); err == nil {
		obj.Field0 = val
	} else {
		return obj, err
	}
	if val, err := DeserializeSpan(deserializer); err == nil {
		obj.Field1 = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Error__NotExhaustivePatternMatch struct {
	Field0 string
	Field1 Span
}

func (*Error__NotExhaustivePatternMatch) isError() {}

func (obj *Error__NotExhaustivePatternMatch) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(4)
	if err := serializer.SerializeStr(obj.Field0); err != nil {
		return err
	}
	if err := obj.Field1.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Error__NotExhaustivePatternMatch) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Error__NotExhaustivePatternMatch(deserializer serde.Deserializer) (Error__NotExhaustivePatternMatch, error) {
	var obj Error__NotExhaustivePatternMatch
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := deserializer.DeserializeStr(); err == nil {
		obj.Field0 = val
	} else {
		return obj, err
	}
	if val, err := DeserializeSpan(deserializer); err == nil {
		obj.Field1 = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Error__Generic struct {
	Field0 string
	Field1 Span
}

func (*Error__Generic) isError() {}

func (obj *Error__Generic) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(5)
	if err := serializer.SerializeStr(obj.Field0); err != nil {
		return err
	}
	if err := obj.Field1.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Error__Generic) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Error__Generic(deserializer serde.Deserializer) (Error__Generic, error) {
	var obj Error__Generic
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := deserializer.DeserializeStr(); err == nil {
		obj.Field0 = val
	} else {
		return obj, err
	}
	if val, err := DeserializeSpan(deserializer); err == nil {
		obj.Field1 = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Error__Parse struct {
	Value ParseError
}

func (*Error__Parse) isError() {}

func (obj *Error__Parse) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(6)
	if err := obj.Value.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Error__Parse) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Error__Parse(deserializer serde.Deserializer) (Error__Parse, error) {
	var obj Error__Parse
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := DeserializeParseError(deserializer); err == nil {
		obj.Value = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Expr interface {
	isExpr()
	Serialize(serializer serde.Serializer) error
	BincodeSerialize() ([]byte, error)
}

func DeserializeExpr(deserializer serde.Deserializer) (Expr, error) {
	index, err := deserializer.DeserializeVariantIndex()
	if err != nil {
		return nil, err
	}

	switch index {
	case 0:
		if val, err := load_Expr__Literal(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 1:
		if val, err := load_Expr__Closure(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 2:
		if val, err := load_Expr__Block(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 3:
		if val, err := load_Expr__Let(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 4:
		if val, err := load_Expr__Var(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 5:
		if val, err := load_Expr__Call(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 6:
		if val, err := load_Expr__If(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 7:
		if val, err := load_Expr__CheckType(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 8:
		if val, err := load_Expr__Match(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 9:
		if val, err := load_Expr__Tuple(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 10:
		if val, err := load_Expr__EnumDef(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 11:
		if val, err := load_Expr__StructDef(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 12:
		if val, err := load_Expr__StructCall(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 13:
		if val, err := load_Expr__StructAccess(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 14:
		if val, err := load_Expr__VarUpdate(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 15:
		if val, err := load_Expr__MethodCall(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 16:
		if val, err := load_Expr__Return(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 17:
		if val, err := load_Expr__Try(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 18:
		if val, err := load_Expr__ExternDecl(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 19:
		if val, err := load_Expr__ImplBlock(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 20:
		if val, err := load_Expr__Binary(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 21:
		if val, err := load_Expr__Paren(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 22:
		if val, err := load_Expr__Unary(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 23:
		if val, err := load_Expr__Const(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 24:
		if val, err := load_Expr__Debug(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 25:
		if val, err := load_Expr__Spawn(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 26:
		if val, err := load_Expr__Select(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 27:
		if val, err := load_Expr__Loop(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 28:
		if val, err := load_Expr__Flow(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 29:
		if val, err := load_Expr__Unit(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 30:
		if val, err := load_Expr__Noop(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 31:
		if val, err := load_Expr__Todo(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	default:
		return nil, fmt.Errorf("Unknown variant index for Expr: %d", index)
	}
}

func BincodeDeserializeExpr(input []byte) (Expr, error) {
	if input == nil {
		var obj Expr
		return obj, fmt.Errorf("Cannot deserialize null array")
	}
	deserializer := bincode.NewDeserializer(input)
	obj, err := DeserializeExpr(deserializer)
	if err == nil && deserializer.GetBufferOffset() < uint64(len(input)) {
		return obj, fmt.Errorf("Some input bytes were not read")
	}
	return obj, err
}

type Expr__Literal struct {
	Lit  Literal
	Ty   Type
	Span Span
}

func (*Expr__Literal) isExpr() {}

func (obj *Expr__Literal) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(0)
	if err := obj.Lit.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Ty.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Span.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Expr__Literal) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Expr__Literal(deserializer serde.Deserializer) (Expr__Literal, error) {
	var obj Expr__Literal
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := DeserializeLiteral(deserializer); err == nil {
		obj.Lit = val
	} else {
		return obj, err
	}
	if val, err := DeserializeType(deserializer); err == nil {
		obj.Ty = val
	} else {
		return obj, err
	}
	if val, err := DeserializeSpan(deserializer); err == nil {
		obj.Span = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Expr__Closure struct {
	Fun  Function
	Kind FunctionKind
	Ty   Type
	Span Span
}

func (*Expr__Closure) isExpr() {}

func (obj *Expr__Closure) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(1)
	if err := obj.Fun.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Kind.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Ty.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Span.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Expr__Closure) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Expr__Closure(deserializer serde.Deserializer) (Expr__Closure, error) {
	var obj Expr__Closure
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := DeserializeFunction(deserializer); err == nil {
		obj.Fun = val
	} else {
		return obj, err
	}
	if val, err := DeserializeFunctionKind(deserializer); err == nil {
		obj.Kind = val
	} else {
		return obj, err
	}
	if val, err := DeserializeType(deserializer); err == nil {
		obj.Ty = val
	} else {
		return obj, err
	}
	if val, err := DeserializeSpan(deserializer); err == nil {
		obj.Span = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Expr__Block struct {
	Stmts []Expr
	Ty    Type
	Span  Span
}

func (*Expr__Block) isExpr() {}

func (obj *Expr__Block) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(2)
	if err := serialize_vector_Expr(obj.Stmts, serializer); err != nil {
		return err
	}
	if err := obj.Ty.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Span.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Expr__Block) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Expr__Block(deserializer serde.Deserializer) (Expr__Block, error) {
	var obj Expr__Block
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := deserialize_vector_Expr(deserializer); err == nil {
		obj.Stmts = val
	} else {
		return obj, err
	}
	if val, err := DeserializeType(deserializer); err == nil {
		obj.Ty = val
	} else {
		return obj, err
	}
	if val, err := DeserializeSpan(deserializer); err == nil {
		obj.Span = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Expr__Let struct {
	Binding Binding
	Value   Expr
	Ty      Type
	Span    Span
}

func (*Expr__Let) isExpr() {}

func (obj *Expr__Let) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(3)
	if err := obj.Binding.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Value.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Ty.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Span.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Expr__Let) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Expr__Let(deserializer serde.Deserializer) (Expr__Let, error) {
	var obj Expr__Let
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := DeserializeBinding(deserializer); err == nil {
		obj.Binding = val
	} else {
		return obj, err
	}
	if val, err := DeserializeExpr(deserializer); err == nil {
		obj.Value = val
	} else {
		return obj, err
	}
	if val, err := DeserializeType(deserializer); err == nil {
		obj.Ty = val
	} else {
		return obj, err
	}
	if val, err := DeserializeSpan(deserializer); err == nil {
		obj.Span = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Expr__Var struct {
	Value string
	Decl  Declaration
	Ty    Type
	Span  Span
}

func (*Expr__Var) isExpr() {}

func (obj *Expr__Var) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(4)
	if err := serializer.SerializeStr(obj.Value); err != nil {
		return err
	}
	if err := obj.Decl.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Ty.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Span.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Expr__Var) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Expr__Var(deserializer serde.Deserializer) (Expr__Var, error) {
	var obj Expr__Var
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := deserializer.DeserializeStr(); err == nil {
		obj.Value = val
	} else {
		return obj, err
	}
	if val, err := DeserializeDeclaration(deserializer); err == nil {
		obj.Decl = val
	} else {
		return obj, err
	}
	if val, err := DeserializeType(deserializer); err == nil {
		obj.Ty = val
	} else {
		return obj, err
	}
	if val, err := DeserializeSpan(deserializer); err == nil {
		obj.Span = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Expr__Call struct {
	Func Expr
	Args []Expr
	Ty   Type
	Span Span
}

func (*Expr__Call) isExpr() {}

func (obj *Expr__Call) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(5)
	if err := obj.Func.Serialize(serializer); err != nil {
		return err
	}
	if err := serialize_vector_Expr(obj.Args, serializer); err != nil {
		return err
	}
	if err := obj.Ty.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Span.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Expr__Call) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Expr__Call(deserializer serde.Deserializer) (Expr__Call, error) {
	var obj Expr__Call
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := DeserializeExpr(deserializer); err == nil {
		obj.Func = val
	} else {
		return obj, err
	}
	if val, err := deserialize_vector_Expr(deserializer); err == nil {
		obj.Args = val
	} else {
		return obj, err
	}
	if val, err := DeserializeType(deserializer); err == nil {
		obj.Ty = val
	} else {
		return obj, err
	}
	if val, err := DeserializeSpan(deserializer); err == nil {
		obj.Span = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Expr__If struct {
	Cond Expr
	Then Expr
	Els  Expr
	Ty   Type
	Span Span
}

func (*Expr__If) isExpr() {}

func (obj *Expr__If) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(6)
	if err := obj.Cond.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Then.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Els.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Ty.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Span.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Expr__If) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Expr__If(deserializer serde.Deserializer) (Expr__If, error) {
	var obj Expr__If
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := DeserializeExpr(deserializer); err == nil {
		obj.Cond = val
	} else {
		return obj, err
	}
	if val, err := DeserializeExpr(deserializer); err == nil {
		obj.Then = val
	} else {
		return obj, err
	}
	if val, err := DeserializeExpr(deserializer); err == nil {
		obj.Els = val
	} else {
		return obj, err
	}
	if val, err := DeserializeType(deserializer); err == nil {
		obj.Ty = val
	} else {
		return obj, err
	}
	if val, err := DeserializeSpan(deserializer); err == nil {
		obj.Span = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Expr__CheckType struct {
	Expr Expr
	Ann  TypeAst
	Ty   Type
	Span Span
}

func (*Expr__CheckType) isExpr() {}

func (obj *Expr__CheckType) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(7)
	if err := obj.Expr.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Ann.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Ty.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Span.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Expr__CheckType) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Expr__CheckType(deserializer serde.Deserializer) (Expr__CheckType, error) {
	var obj Expr__CheckType
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := DeserializeExpr(deserializer); err == nil {
		obj.Expr = val
	} else {
		return obj, err
	}
	if val, err := DeserializeTypeAst(deserializer); err == nil {
		obj.Ann = val
	} else {
		return obj, err
	}
	if val, err := DeserializeType(deserializer); err == nil {
		obj.Ty = val
	} else {
		return obj, err
	}
	if val, err := DeserializeSpan(deserializer); err == nil {
		obj.Span = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Expr__Match struct {
	Subject Expr
	Arms    []Arm
	Ty      Type
	Span    Span
}

func (*Expr__Match) isExpr() {}

func (obj *Expr__Match) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(8)
	if err := obj.Subject.Serialize(serializer); err != nil {
		return err
	}
	if err := serialize_vector_Arm(obj.Arms, serializer); err != nil {
		return err
	}
	if err := obj.Ty.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Span.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Expr__Match) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Expr__Match(deserializer serde.Deserializer) (Expr__Match, error) {
	var obj Expr__Match
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := DeserializeExpr(deserializer); err == nil {
		obj.Subject = val
	} else {
		return obj, err
	}
	if val, err := deserialize_vector_Arm(deserializer); err == nil {
		obj.Arms = val
	} else {
		return obj, err
	}
	if val, err := DeserializeType(deserializer); err == nil {
		obj.Ty = val
	} else {
		return obj, err
	}
	if val, err := DeserializeSpan(deserializer); err == nil {
		obj.Span = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Expr__Tuple struct {
	Elems []Expr
	Ty    Type
	Span  Span
}

func (*Expr__Tuple) isExpr() {}

func (obj *Expr__Tuple) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(9)
	if err := serialize_vector_Expr(obj.Elems, serializer); err != nil {
		return err
	}
	if err := obj.Ty.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Span.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Expr__Tuple) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Expr__Tuple(deserializer serde.Deserializer) (Expr__Tuple, error) {
	var obj Expr__Tuple
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := deserialize_vector_Expr(deserializer); err == nil {
		obj.Elems = val
	} else {
		return obj, err
	}
	if val, err := DeserializeType(deserializer); err == nil {
		obj.Ty = val
	} else {
		return obj, err
	}
	if val, err := DeserializeSpan(deserializer); err == nil {
		obj.Span = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Expr__EnumDef struct {
	Def  EnumDefinition
	Span Span
}

func (*Expr__EnumDef) isExpr() {}

func (obj *Expr__EnumDef) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(10)
	if err := obj.Def.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Span.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Expr__EnumDef) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Expr__EnumDef(deserializer serde.Deserializer) (Expr__EnumDef, error) {
	var obj Expr__EnumDef
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := DeserializeEnumDefinition(deserializer); err == nil {
		obj.Def = val
	} else {
		return obj, err
	}
	if val, err := DeserializeSpan(deserializer); err == nil {
		obj.Span = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Expr__StructDef struct {
	Def  StructDefinition
	Span Span
}

func (*Expr__StructDef) isExpr() {}

func (obj *Expr__StructDef) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(11)
	if err := obj.Def.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Span.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Expr__StructDef) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Expr__StructDef(deserializer serde.Deserializer) (Expr__StructDef, error) {
	var obj Expr__StructDef
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := DeserializeStructDefinition(deserializer); err == nil {
		obj.Def = val
	} else {
		return obj, err
	}
	if val, err := DeserializeSpan(deserializer); err == nil {
		obj.Span = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Expr__StructCall struct {
	Name   string
	Fields []StructField
	Rest   *Expr
	Ty     Type
	Span   Span
}

func (*Expr__StructCall) isExpr() {}

func (obj *Expr__StructCall) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(12)
	if err := serializer.SerializeStr(obj.Name); err != nil {
		return err
	}
	if err := serialize_vector_StructField(obj.Fields, serializer); err != nil {
		return err
	}
	if err := serialize_option_Expr(obj.Rest, serializer); err != nil {
		return err
	}
	if err := obj.Ty.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Span.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Expr__StructCall) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Expr__StructCall(deserializer serde.Deserializer) (Expr__StructCall, error) {
	var obj Expr__StructCall
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := deserializer.DeserializeStr(); err == nil {
		obj.Name = val
	} else {
		return obj, err
	}
	if val, err := deserialize_vector_StructField(deserializer); err == nil {
		obj.Fields = val
	} else {
		return obj, err
	}
	if val, err := deserialize_option_Expr(deserializer); err == nil {
		obj.Rest = val
	} else {
		return obj, err
	}
	if val, err := DeserializeType(deserializer); err == nil {
		obj.Ty = val
	} else {
		return obj, err
	}
	if val, err := DeserializeSpan(deserializer); err == nil {
		obj.Span = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Expr__StructAccess struct {
	Expr  Expr
	Field string
	Ty    Type
	Span  Span
}

func (*Expr__StructAccess) isExpr() {}

func (obj *Expr__StructAccess) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(13)
	if err := obj.Expr.Serialize(serializer); err != nil {
		return err
	}
	if err := serializer.SerializeStr(obj.Field); err != nil {
		return err
	}
	if err := obj.Ty.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Span.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Expr__StructAccess) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Expr__StructAccess(deserializer serde.Deserializer) (Expr__StructAccess, error) {
	var obj Expr__StructAccess
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := DeserializeExpr(deserializer); err == nil {
		obj.Expr = val
	} else {
		return obj, err
	}
	if val, err := deserializer.DeserializeStr(); err == nil {
		obj.Field = val
	} else {
		return obj, err
	}
	if val, err := DeserializeType(deserializer); err == nil {
		obj.Ty = val
	} else {
		return obj, err
	}
	if val, err := DeserializeSpan(deserializer); err == nil {
		obj.Span = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Expr__VarUpdate struct {
	Target Expr
	Value  Expr
	Span   Span
}

func (*Expr__VarUpdate) isExpr() {}

func (obj *Expr__VarUpdate) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(14)
	if err := obj.Target.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Value.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Span.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Expr__VarUpdate) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Expr__VarUpdate(deserializer serde.Deserializer) (Expr__VarUpdate, error) {
	var obj Expr__VarUpdate
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := DeserializeExpr(deserializer); err == nil {
		obj.Target = val
	} else {
		return obj, err
	}
	if val, err := DeserializeExpr(deserializer); err == nil {
		obj.Value = val
	} else {
		return obj, err
	}
	if val, err := DeserializeSpan(deserializer); err == nil {
		obj.Span = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Expr__MethodCall struct {
	Target Expr
	Method string
	Args   []Expr
	Ty     Type
	Span   Span
}

func (*Expr__MethodCall) isExpr() {}

func (obj *Expr__MethodCall) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(15)
	if err := obj.Target.Serialize(serializer); err != nil {
		return err
	}
	if err := serializer.SerializeStr(obj.Method); err != nil {
		return err
	}
	if err := serialize_vector_Expr(obj.Args, serializer); err != nil {
		return err
	}
	if err := obj.Ty.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Span.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Expr__MethodCall) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Expr__MethodCall(deserializer serde.Deserializer) (Expr__MethodCall, error) {
	var obj Expr__MethodCall
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := DeserializeExpr(deserializer); err == nil {
		obj.Target = val
	} else {
		return obj, err
	}
	if val, err := deserializer.DeserializeStr(); err == nil {
		obj.Method = val
	} else {
		return obj, err
	}
	if val, err := deserialize_vector_Expr(deserializer); err == nil {
		obj.Args = val
	} else {
		return obj, err
	}
	if val, err := DeserializeType(deserializer); err == nil {
		obj.Ty = val
	} else {
		return obj, err
	}
	if val, err := DeserializeSpan(deserializer); err == nil {
		obj.Span = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Expr__Return struct {
	Expr Expr
	Ty   Type
	Span Span
}

func (*Expr__Return) isExpr() {}

func (obj *Expr__Return) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(16)
	if err := obj.Expr.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Ty.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Span.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Expr__Return) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Expr__Return(deserializer serde.Deserializer) (Expr__Return, error) {
	var obj Expr__Return
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := DeserializeExpr(deserializer); err == nil {
		obj.Expr = val
	} else {
		return obj, err
	}
	if val, err := DeserializeType(deserializer); err == nil {
		obj.Ty = val
	} else {
		return obj, err
	}
	if val, err := DeserializeSpan(deserializer); err == nil {
		obj.Span = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Expr__Try struct {
	Expr Expr
	Ty   Type
	Span Span
}

func (*Expr__Try) isExpr() {}

func (obj *Expr__Try) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(17)
	if err := obj.Expr.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Ty.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Span.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Expr__Try) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Expr__Try(deserializer serde.Deserializer) (Expr__Try, error) {
	var obj Expr__Try
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := DeserializeExpr(deserializer); err == nil {
		obj.Expr = val
	} else {
		return obj, err
	}
	if val, err := DeserializeType(deserializer); err == nil {
		obj.Ty = val
	} else {
		return obj, err
	}
	if val, err := DeserializeSpan(deserializer); err == nil {
		obj.Span = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Expr__ExternDecl struct {
	Name  string
	Kind  ExternKind
	Items []Expr
	Span  Span
}

func (*Expr__ExternDecl) isExpr() {}

func (obj *Expr__ExternDecl) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(18)
	if err := serializer.SerializeStr(obj.Name); err != nil {
		return err
	}
	if err := obj.Kind.Serialize(serializer); err != nil {
		return err
	}
	if err := serialize_vector_Expr(obj.Items, serializer); err != nil {
		return err
	}
	if err := obj.Span.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Expr__ExternDecl) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Expr__ExternDecl(deserializer serde.Deserializer) (Expr__ExternDecl, error) {
	var obj Expr__ExternDecl
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := deserializer.DeserializeStr(); err == nil {
		obj.Name = val
	} else {
		return obj, err
	}
	if val, err := DeserializeExternKind(deserializer); err == nil {
		obj.Kind = val
	} else {
		return obj, err
	}
	if val, err := deserialize_vector_Expr(deserializer); err == nil {
		obj.Items = val
	} else {
		return obj, err
	}
	if val, err := DeserializeSpan(deserializer); err == nil {
		obj.Span = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Expr__ImplBlock struct {
	Ann      TypeAst
	Ty       Type
	Items    []Expr
	Generics []string
	Span     Span
}

func (*Expr__ImplBlock) isExpr() {}

func (obj *Expr__ImplBlock) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(19)
	if err := obj.Ann.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Ty.Serialize(serializer); err != nil {
		return err
	}
	if err := serialize_vector_Expr(obj.Items, serializer); err != nil {
		return err
	}
	if err := serialize_vector_str(obj.Generics, serializer); err != nil {
		return err
	}
	if err := obj.Span.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Expr__ImplBlock) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Expr__ImplBlock(deserializer serde.Deserializer) (Expr__ImplBlock, error) {
	var obj Expr__ImplBlock
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := DeserializeTypeAst(deserializer); err == nil {
		obj.Ann = val
	} else {
		return obj, err
	}
	if val, err := DeserializeType(deserializer); err == nil {
		obj.Ty = val
	} else {
		return obj, err
	}
	if val, err := deserialize_vector_Expr(deserializer); err == nil {
		obj.Items = val
	} else {
		return obj, err
	}
	if val, err := deserialize_vector_str(deserializer); err == nil {
		obj.Generics = val
	} else {
		return obj, err
	}
	if val, err := DeserializeSpan(deserializer); err == nil {
		obj.Span = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Expr__Binary struct {
	Op    Operator
	Left  Expr
	Right Expr
	Ty    Type
	Span  Span
}

func (*Expr__Binary) isExpr() {}

func (obj *Expr__Binary) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(20)
	if err := obj.Op.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Left.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Right.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Ty.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Span.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Expr__Binary) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Expr__Binary(deserializer serde.Deserializer) (Expr__Binary, error) {
	var obj Expr__Binary
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := DeserializeOperator(deserializer); err == nil {
		obj.Op = val
	} else {
		return obj, err
	}
	if val, err := DeserializeExpr(deserializer); err == nil {
		obj.Left = val
	} else {
		return obj, err
	}
	if val, err := DeserializeExpr(deserializer); err == nil {
		obj.Right = val
	} else {
		return obj, err
	}
	if val, err := DeserializeType(deserializer); err == nil {
		obj.Ty = val
	} else {
		return obj, err
	}
	if val, err := DeserializeSpan(deserializer); err == nil {
		obj.Span = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Expr__Paren struct {
	Expr Expr
	Ty   Type
	Span Span
}

func (*Expr__Paren) isExpr() {}

func (obj *Expr__Paren) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(21)
	if err := obj.Expr.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Ty.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Span.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Expr__Paren) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Expr__Paren(deserializer serde.Deserializer) (Expr__Paren, error) {
	var obj Expr__Paren
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := DeserializeExpr(deserializer); err == nil {
		obj.Expr = val
	} else {
		return obj, err
	}
	if val, err := DeserializeType(deserializer); err == nil {
		obj.Ty = val
	} else {
		return obj, err
	}
	if val, err := DeserializeSpan(deserializer); err == nil {
		obj.Span = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Expr__Unary struct {
	Op   UnOp
	Expr Expr
	Ty   Type
	Span Span
}

func (*Expr__Unary) isExpr() {}

func (obj *Expr__Unary) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(22)
	if err := obj.Op.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Expr.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Ty.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Span.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Expr__Unary) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Expr__Unary(deserializer serde.Deserializer) (Expr__Unary, error) {
	var obj Expr__Unary
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := DeserializeUnOp(deserializer); err == nil {
		obj.Op = val
	} else {
		return obj, err
	}
	if val, err := DeserializeExpr(deserializer); err == nil {
		obj.Expr = val
	} else {
		return obj, err
	}
	if val, err := DeserializeType(deserializer); err == nil {
		obj.Ty = val
	} else {
		return obj, err
	}
	if val, err := DeserializeSpan(deserializer); err == nil {
		obj.Span = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Expr__Const struct {
	Ident string
	Expr  Expr
	Ann   TypeAst
	Ty    Type
	Span  Span
}

func (*Expr__Const) isExpr() {}

func (obj *Expr__Const) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(23)
	if err := serializer.SerializeStr(obj.Ident); err != nil {
		return err
	}
	if err := obj.Expr.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Ann.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Ty.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Span.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Expr__Const) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Expr__Const(deserializer serde.Deserializer) (Expr__Const, error) {
	var obj Expr__Const
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := deserializer.DeserializeStr(); err == nil {
		obj.Ident = val
	} else {
		return obj, err
	}
	if val, err := DeserializeExpr(deserializer); err == nil {
		obj.Expr = val
	} else {
		return obj, err
	}
	if val, err := DeserializeTypeAst(deserializer); err == nil {
		obj.Ann = val
	} else {
		return obj, err
	}
	if val, err := DeserializeType(deserializer); err == nil {
		obj.Ty = val
	} else {
		return obj, err
	}
	if val, err := DeserializeSpan(deserializer); err == nil {
		obj.Span = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Expr__Debug struct {
	Kind DebugKind
	Expr Expr
	Ty   Type
	Span Span
}

func (*Expr__Debug) isExpr() {}

func (obj *Expr__Debug) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(24)
	if err := obj.Kind.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Expr.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Ty.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Span.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Expr__Debug) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Expr__Debug(deserializer serde.Deserializer) (Expr__Debug, error) {
	var obj Expr__Debug
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := DeserializeDebugKind(deserializer); err == nil {
		obj.Kind = val
	} else {
		return obj, err
	}
	if val, err := DeserializeExpr(deserializer); err == nil {
		obj.Expr = val
	} else {
		return obj, err
	}
	if val, err := DeserializeType(deserializer); err == nil {
		obj.Ty = val
	} else {
		return obj, err
	}
	if val, err := DeserializeSpan(deserializer); err == nil {
		obj.Span = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Expr__Spawn struct {
	Expr Expr
	Ty   Type
	Span Span
}

func (*Expr__Spawn) isExpr() {}

func (obj *Expr__Spawn) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(25)
	if err := obj.Expr.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Ty.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Span.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Expr__Spawn) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Expr__Spawn(deserializer serde.Deserializer) (Expr__Spawn, error) {
	var obj Expr__Spawn
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := DeserializeExpr(deserializer); err == nil {
		obj.Expr = val
	} else {
		return obj, err
	}
	if val, err := DeserializeType(deserializer); err == nil {
		obj.Ty = val
	} else {
		return obj, err
	}
	if val, err := DeserializeSpan(deserializer); err == nil {
		obj.Span = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Expr__Select struct {
	Arms []Arm
	Ty   Type
	Span Span
}

func (*Expr__Select) isExpr() {}

func (obj *Expr__Select) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(26)
	if err := serialize_vector_Arm(obj.Arms, serializer); err != nil {
		return err
	}
	if err := obj.Ty.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Span.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Expr__Select) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Expr__Select(deserializer serde.Deserializer) (Expr__Select, error) {
	var obj Expr__Select
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := deserialize_vector_Arm(deserializer); err == nil {
		obj.Arms = val
	} else {
		return obj, err
	}
	if val, err := DeserializeType(deserializer); err == nil {
		obj.Ty = val
	} else {
		return obj, err
	}
	if val, err := DeserializeSpan(deserializer); err == nil {
		obj.Span = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Expr__Loop struct {
	Kind Loop
	Body Expr
	Span Span
}

func (*Expr__Loop) isExpr() {}

func (obj *Expr__Loop) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(27)
	if err := obj.Kind.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Body.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Span.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Expr__Loop) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Expr__Loop(deserializer serde.Deserializer) (Expr__Loop, error) {
	var obj Expr__Loop
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := DeserializeLoop(deserializer); err == nil {
		obj.Kind = val
	} else {
		return obj, err
	}
	if val, err := DeserializeExpr(deserializer); err == nil {
		obj.Body = val
	} else {
		return obj, err
	}
	if val, err := DeserializeSpan(deserializer); err == nil {
		obj.Span = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Expr__Flow struct {
	Kind LoopFlow
	Span Span
}

func (*Expr__Flow) isExpr() {}

func (obj *Expr__Flow) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(28)
	if err := obj.Kind.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Span.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Expr__Flow) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Expr__Flow(deserializer serde.Deserializer) (Expr__Flow, error) {
	var obj Expr__Flow
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := DeserializeLoopFlow(deserializer); err == nil {
		obj.Kind = val
	} else {
		return obj, err
	}
	if val, err := DeserializeSpan(deserializer); err == nil {
		obj.Span = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Expr__Unit struct {
	Span Span
}

func (*Expr__Unit) isExpr() {}

func (obj *Expr__Unit) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(29)
	if err := obj.Span.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Expr__Unit) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Expr__Unit(deserializer serde.Deserializer) (Expr__Unit, error) {
	var obj Expr__Unit
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := DeserializeSpan(deserializer); err == nil {
		obj.Span = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Expr__Noop struct {
}

func (*Expr__Noop) isExpr() {}

func (obj *Expr__Noop) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(30)
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Expr__Noop) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Expr__Noop(deserializer serde.Deserializer) (Expr__Noop, error) {
	var obj Expr__Noop
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Expr__Todo struct {
}

func (*Expr__Todo) isExpr() {}

func (obj *Expr__Todo) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(31)
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Expr__Todo) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Expr__Todo(deserializer serde.Deserializer) (Expr__Todo, error) {
	var obj Expr__Todo
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type ExternKind interface {
	isExternKind()
	Serialize(serializer serde.Serializer) error
	BincodeSerialize() ([]byte, error)
}

func DeserializeExternKind(deserializer serde.Deserializer) (ExternKind, error) {
	index, err := deserializer.DeserializeVariantIndex()
	if err != nil {
		return nil, err
	}

	switch index {
	case 0:
		if val, err := load_ExternKind__Effect(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 1:
		if val, err := load_ExternKind__Native(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 2:
		if val, err := load_ExternKind__Overload(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	default:
		return nil, fmt.Errorf("Unknown variant index for ExternKind: %d", index)
	}
}

func BincodeDeserializeExternKind(input []byte) (ExternKind, error) {
	if input == nil {
		var obj ExternKind
		return obj, fmt.Errorf("Cannot deserialize null array")
	}
	deserializer := bincode.NewDeserializer(input)
	obj, err := DeserializeExternKind(deserializer)
	if err == nil && deserializer.GetBufferOffset() < uint64(len(input)) {
		return obj, fmt.Errorf("Some input bytes were not read")
	}
	return obj, err
}

type ExternKind__Effect struct {
}

func (*ExternKind__Effect) isExternKind() {}

func (obj *ExternKind__Effect) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(0)
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *ExternKind__Effect) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_ExternKind__Effect(deserializer serde.Deserializer) (ExternKind__Effect, error) {
	var obj ExternKind__Effect
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type ExternKind__Native struct {
}

func (*ExternKind__Native) isExternKind() {}

func (obj *ExternKind__Native) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(1)
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *ExternKind__Native) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_ExternKind__Native(deserializer serde.Deserializer) (ExternKind__Native, error) {
	var obj ExternKind__Native
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type ExternKind__Overload struct {
}

func (*ExternKind__Overload) isExternKind() {}

func (obj *ExternKind__Overload) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(2)
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *ExternKind__Overload) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_ExternKind__Overload(deserializer serde.Deserializer) (ExternKind__Overload, error) {
	var obj ExternKind__Overload
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type File struct {
	Name   string
	Decls  []Expr
	Source string
}

func (obj *File) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	if err := serializer.SerializeStr(obj.Name); err != nil {
		return err
	}
	if err := serialize_vector_Expr(obj.Decls, serializer); err != nil {
		return err
	}
	if err := serializer.SerializeStr(obj.Source); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *File) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func DeserializeFile(deserializer serde.Deserializer) (File, error) {
	var obj File
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := deserializer.DeserializeStr(); err == nil {
		obj.Name = val
	} else {
		return obj, err
	}
	if val, err := deserialize_vector_Expr(deserializer); err == nil {
		obj.Decls = val
	} else {
		return obj, err
	}
	if val, err := deserializer.DeserializeStr(); err == nil {
		obj.Source = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

func BincodeDeserializeFile(input []byte) (File, error) {
	if input == nil {
		var obj File
		return obj, fmt.Errorf("Cannot deserialize null array")
	}
	deserializer := bincode.NewDeserializer(input)
	obj, err := DeserializeFile(deserializer)
	if err == nil && deserializer.GetBufferOffset() < uint64(len(input)) {
		return obj, fmt.Errorf("Some input bytes were not read")
	}
	return obj, err
}

type FileId struct {
	Package  string
	Filename string
}

func (obj *FileId) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	if err := serializer.SerializeStr(obj.Package); err != nil {
		return err
	}
	if err := serializer.SerializeStr(obj.Filename); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *FileId) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func DeserializeFileId(deserializer serde.Deserializer) (FileId, error) {
	var obj FileId
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := deserializer.DeserializeStr(); err == nil {
		obj.Package = val
	} else {
		return obj, err
	}
	if val, err := deserializer.DeserializeStr(); err == nil {
		obj.Filename = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

func BincodeDeserializeFileId(input []byte) (FileId, error) {
	if input == nil {
		var obj FileId
		return obj, fmt.Errorf("Cannot deserialize null array")
	}
	deserializer := bincode.NewDeserializer(input)
	obj, err := DeserializeFileId(deserializer)
	if err == nil && deserializer.GetBufferOffset() < uint64(len(input)) {
		return obj, fmt.Errorf("Some input bytes were not read")
	}
	return obj, err
}

type Function struct {
	Name     string
	Generics []string
	Bounds   []TypeAst
	Args     []Binding
	Ret      Type
	Ann      TypeAst
	Body     Expr
}

func (obj *Function) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	if err := serializer.SerializeStr(obj.Name); err != nil {
		return err
	}
	if err := serialize_vector_str(obj.Generics, serializer); err != nil {
		return err
	}
	if err := serialize_vector_TypeAst(obj.Bounds, serializer); err != nil {
		return err
	}
	if err := serialize_vector_Binding(obj.Args, serializer); err != nil {
		return err
	}
	if err := obj.Ret.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Ann.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Body.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Function) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func DeserializeFunction(deserializer serde.Deserializer) (Function, error) {
	var obj Function
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := deserializer.DeserializeStr(); err == nil {
		obj.Name = val
	} else {
		return obj, err
	}
	if val, err := deserialize_vector_str(deserializer); err == nil {
		obj.Generics = val
	} else {
		return obj, err
	}
	if val, err := deserialize_vector_TypeAst(deserializer); err == nil {
		obj.Bounds = val
	} else {
		return obj, err
	}
	if val, err := deserialize_vector_Binding(deserializer); err == nil {
		obj.Args = val
	} else {
		return obj, err
	}
	if val, err := DeserializeType(deserializer); err == nil {
		obj.Ret = val
	} else {
		return obj, err
	}
	if val, err := DeserializeTypeAst(deserializer); err == nil {
		obj.Ann = val
	} else {
		return obj, err
	}
	if val, err := DeserializeExpr(deserializer); err == nil {
		obj.Body = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

func BincodeDeserializeFunction(input []byte) (Function, error) {
	if input == nil {
		var obj Function
		return obj, fmt.Errorf("Cannot deserialize null array")
	}
	deserializer := bincode.NewDeserializer(input)
	obj, err := DeserializeFunction(deserializer)
	if err == nil && deserializer.GetBufferOffset() < uint64(len(input)) {
		return obj, fmt.Errorf("Some input bytes were not read")
	}
	return obj, err
}

type FunctionKind interface {
	isFunctionKind()
	Serialize(serializer serde.Serializer) error
	BincodeSerialize() ([]byte, error)
}

func DeserializeFunctionKind(deserializer serde.Deserializer) (FunctionKind, error) {
	index, err := deserializer.DeserializeVariantIndex()
	if err != nil {
		return nil, err
	}

	switch index {
	case 0:
		if val, err := load_FunctionKind__TopLevel(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 1:
		if val, err := load_FunctionKind__Inline(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 2:
		if val, err := load_FunctionKind__Lambda(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	default:
		return nil, fmt.Errorf("Unknown variant index for FunctionKind: %d", index)
	}
}

func BincodeDeserializeFunctionKind(input []byte) (FunctionKind, error) {
	if input == nil {
		var obj FunctionKind
		return obj, fmt.Errorf("Cannot deserialize null array")
	}
	deserializer := bincode.NewDeserializer(input)
	obj, err := DeserializeFunctionKind(deserializer)
	if err == nil && deserializer.GetBufferOffset() < uint64(len(input)) {
		return obj, fmt.Errorf("Some input bytes were not read")
	}
	return obj, err
}

type FunctionKind__TopLevel struct {
}

func (*FunctionKind__TopLevel) isFunctionKind() {}

func (obj *FunctionKind__TopLevel) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(0)
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *FunctionKind__TopLevel) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_FunctionKind__TopLevel(deserializer serde.Deserializer) (FunctionKind__TopLevel, error) {
	var obj FunctionKind__TopLevel
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type FunctionKind__Inline struct {
}

func (*FunctionKind__Inline) isFunctionKind() {}

func (obj *FunctionKind__Inline) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(1)
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *FunctionKind__Inline) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_FunctionKind__Inline(deserializer serde.Deserializer) (FunctionKind__Inline, error) {
	var obj FunctionKind__Inline
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type FunctionKind__Lambda struct {
}

func (*FunctionKind__Lambda) isFunctionKind() {}

func (obj *FunctionKind__Lambda) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(2)
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *FunctionKind__Lambda) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_FunctionKind__Lambda(deserializer serde.Deserializer) (FunctionKind__Lambda, error) {
	var obj FunctionKind__Lambda
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type LineColumn struct {
	Line uint64
	Col  uint64
}

func (obj *LineColumn) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	if err := serializer.SerializeU64(obj.Line); err != nil {
		return err
	}
	if err := serializer.SerializeU64(obj.Col); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *LineColumn) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func DeserializeLineColumn(deserializer serde.Deserializer) (LineColumn, error) {
	var obj LineColumn
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := deserializer.DeserializeU64(); err == nil {
		obj.Line = val
	} else {
		return obj, err
	}
	if val, err := deserializer.DeserializeU64(); err == nil {
		obj.Col = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

func BincodeDeserializeLineColumn(input []byte) (LineColumn, error) {
	if input == nil {
		var obj LineColumn
		return obj, fmt.Errorf("Cannot deserialize null array")
	}
	deserializer := bincode.NewDeserializer(input)
	obj, err := DeserializeLineColumn(deserializer)
	if err == nil && deserializer.GetBufferOffset() < uint64(len(input)) {
		return obj, fmt.Errorf("Some input bytes were not read")
	}
	return obj, err
}

type Literal interface {
	isLiteral()
	Serialize(serializer serde.Serializer) error
	BincodeSerialize() ([]byte, error)
}

func DeserializeLiteral(deserializer serde.Deserializer) (Literal, error) {
	index, err := deserializer.DeserializeVariantIndex()
	if err != nil {
		return nil, err
	}

	switch index {
	case 0:
		if val, err := load_Literal__Int(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 1:
		if val, err := load_Literal__Float(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 2:
		if val, err := load_Literal__Bool(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 3:
		if val, err := load_Literal__String(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 4:
		if val, err := load_Literal__Char(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 5:
		if val, err := load_Literal__List(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	default:
		return nil, fmt.Errorf("Unknown variant index for Literal: %d", index)
	}
}

func BincodeDeserializeLiteral(input []byte) (Literal, error) {
	if input == nil {
		var obj Literal
		return obj, fmt.Errorf("Cannot deserialize null array")
	}
	deserializer := bincode.NewDeserializer(input)
	obj, err := DeserializeLiteral(deserializer)
	if err == nil && deserializer.GetBufferOffset() < uint64(len(input)) {
		return obj, fmt.Errorf("Some input bytes were not read")
	}
	return obj, err
}

type Literal__Int int64

func (*Literal__Int) isLiteral() {}

func (obj *Literal__Int) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(0)
	if err := serializer.SerializeI64(((int64)(*obj))); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Literal__Int) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Literal__Int(deserializer serde.Deserializer) (Literal__Int, error) {
	var obj int64
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return (Literal__Int)(obj), err
	}
	if val, err := deserializer.DeserializeI64(); err == nil {
		obj = val
	} else {
		return ((Literal__Int)(obj)), err
	}
	deserializer.DecreaseContainerDepth()
	return (Literal__Int)(obj), nil
}

type Literal__Float float64

func (*Literal__Float) isLiteral() {}

func (obj *Literal__Float) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(1)
	if err := serializer.SerializeF64(((float64)(*obj))); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Literal__Float) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Literal__Float(deserializer serde.Deserializer) (Literal__Float, error) {
	var obj float64
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return (Literal__Float)(obj), err
	}
	if val, err := deserializer.DeserializeF64(); err == nil {
		obj = val
	} else {
		return ((Literal__Float)(obj)), err
	}
	deserializer.DecreaseContainerDepth()
	return (Literal__Float)(obj), nil
}

type Literal__Bool bool

func (*Literal__Bool) isLiteral() {}

func (obj *Literal__Bool) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(2)
	if err := serializer.SerializeBool(((bool)(*obj))); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Literal__Bool) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Literal__Bool(deserializer serde.Deserializer) (Literal__Bool, error) {
	var obj bool
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return (Literal__Bool)(obj), err
	}
	if val, err := deserializer.DeserializeBool(); err == nil {
		obj = val
	} else {
		return ((Literal__Bool)(obj)), err
	}
	deserializer.DecreaseContainerDepth()
	return (Literal__Bool)(obj), nil
}

type Literal__String string

func (*Literal__String) isLiteral() {}

func (obj *Literal__String) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(3)
	if err := serializer.SerializeStr(((string)(*obj))); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Literal__String) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Literal__String(deserializer serde.Deserializer) (Literal__String, error) {
	var obj string
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return (Literal__String)(obj), err
	}
	if val, err := deserializer.DeserializeStr(); err == nil {
		obj = val
	} else {
		return ((Literal__String)(obj)), err
	}
	deserializer.DecreaseContainerDepth()
	return (Literal__String)(obj), nil
}

type Literal__Char string

func (*Literal__Char) isLiteral() {}

func (obj *Literal__Char) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(4)
	if err := serializer.SerializeStr(((string)(*obj))); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Literal__Char) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Literal__Char(deserializer serde.Deserializer) (Literal__Char, error) {
	var obj string
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return (Literal__Char)(obj), err
	}
	if val, err := deserializer.DeserializeStr(); err == nil {
		obj = val
	} else {
		return ((Literal__Char)(obj)), err
	}
	deserializer.DecreaseContainerDepth()
	return (Literal__Char)(obj), nil
}

type Literal__List []Expr

func (*Literal__List) isLiteral() {}

func (obj *Literal__List) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(5)
	if err := serialize_vector_Expr((([]Expr)(*obj)), serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Literal__List) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Literal__List(deserializer serde.Deserializer) (Literal__List, error) {
	var obj []Expr
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return (Literal__List)(obj), err
	}
	if val, err := deserialize_vector_Expr(deserializer); err == nil {
		obj = val
	} else {
		return ((Literal__List)(obj)), err
	}
	deserializer.DecreaseContainerDepth()
	return (Literal__List)(obj), nil
}

type Loop interface {
	isLoop()
	Serialize(serializer serde.Serializer) error
	BincodeSerialize() ([]byte, error)
}

func DeserializeLoop(deserializer serde.Deserializer) (Loop, error) {
	index, err := deserializer.DeserializeVariantIndex()
	if err != nil {
		return nil, err
	}

	switch index {
	case 0:
		if val, err := load_Loop__NoCondition(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 1:
		if val, err := load_Loop__WithCondition(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	default:
		return nil, fmt.Errorf("Unknown variant index for Loop: %d", index)
	}
}

func BincodeDeserializeLoop(input []byte) (Loop, error) {
	if input == nil {
		var obj Loop
		return obj, fmt.Errorf("Cannot deserialize null array")
	}
	deserializer := bincode.NewDeserializer(input)
	obj, err := DeserializeLoop(deserializer)
	if err == nil && deserializer.GetBufferOffset() < uint64(len(input)) {
		return obj, fmt.Errorf("Some input bytes were not read")
	}
	return obj, err
}

type Loop__NoCondition struct {
}

func (*Loop__NoCondition) isLoop() {}

func (obj *Loop__NoCondition) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(0)
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Loop__NoCondition) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Loop__NoCondition(deserializer serde.Deserializer) (Loop__NoCondition, error) {
	var obj Loop__NoCondition
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Loop__WithCondition struct {
	Binding Binding
	Expr    Expr
}

func (*Loop__WithCondition) isLoop() {}

func (obj *Loop__WithCondition) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(1)
	if err := obj.Binding.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Expr.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Loop__WithCondition) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Loop__WithCondition(deserializer serde.Deserializer) (Loop__WithCondition, error) {
	var obj Loop__WithCondition
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := DeserializeBinding(deserializer); err == nil {
		obj.Binding = val
	} else {
		return obj, err
	}
	if val, err := DeserializeExpr(deserializer); err == nil {
		obj.Expr = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type LoopFlow interface {
	isLoopFlow()
	Serialize(serializer serde.Serializer) error
	BincodeSerialize() ([]byte, error)
}

func DeserializeLoopFlow(deserializer serde.Deserializer) (LoopFlow, error) {
	index, err := deserializer.DeserializeVariantIndex()
	if err != nil {
		return nil, err
	}

	switch index {
	case 0:
		if val, err := load_LoopFlow__Break(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 1:
		if val, err := load_LoopFlow__Continue(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	default:
		return nil, fmt.Errorf("Unknown variant index for LoopFlow: %d", index)
	}
}

func BincodeDeserializeLoopFlow(input []byte) (LoopFlow, error) {
	if input == nil {
		var obj LoopFlow
		return obj, fmt.Errorf("Cannot deserialize null array")
	}
	deserializer := bincode.NewDeserializer(input)
	obj, err := DeserializeLoopFlow(deserializer)
	if err == nil && deserializer.GetBufferOffset() < uint64(len(input)) {
		return obj, fmt.Errorf("Some input bytes were not read")
	}
	return obj, err
}

type LoopFlow__Break struct {
}

func (*LoopFlow__Break) isLoopFlow() {}

func (obj *LoopFlow__Break) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(0)
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *LoopFlow__Break) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_LoopFlow__Break(deserializer serde.Deserializer) (LoopFlow__Break, error) {
	var obj LoopFlow__Break
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type LoopFlow__Continue struct {
}

func (*LoopFlow__Continue) isLoopFlow() {}

func (obj *LoopFlow__Continue) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(1)
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *LoopFlow__Continue) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_LoopFlow__Continue(deserializer serde.Deserializer) (LoopFlow__Continue, error) {
	var obj LoopFlow__Continue
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Operator interface {
	isOperator()
	Serialize(serializer serde.Serializer) error
	BincodeSerialize() ([]byte, error)
}

func DeserializeOperator(deserializer serde.Deserializer) (Operator, error) {
	index, err := deserializer.DeserializeVariantIndex()
	if err != nil {
		return nil, err
	}

	switch index {
	case 0:
		if val, err := load_Operator__Add(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 1:
		if val, err := load_Operator__Sub(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 2:
		if val, err := load_Operator__Mul(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 3:
		if val, err := load_Operator__Div(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 4:
		if val, err := load_Operator__Lt(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 5:
		if val, err := load_Operator__Le(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 6:
		if val, err := load_Operator__Gt(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 7:
		if val, err := load_Operator__Ge(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 8:
		if val, err := load_Operator__Rem(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 9:
		if val, err := load_Operator__Eq(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 10:
		if val, err := load_Operator__Ne(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 11:
		if val, err := load_Operator__And(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 12:
		if val, err := load_Operator__Or(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	default:
		return nil, fmt.Errorf("Unknown variant index for Operator: %d", index)
	}
}

func BincodeDeserializeOperator(input []byte) (Operator, error) {
	if input == nil {
		var obj Operator
		return obj, fmt.Errorf("Cannot deserialize null array")
	}
	deserializer := bincode.NewDeserializer(input)
	obj, err := DeserializeOperator(deserializer)
	if err == nil && deserializer.GetBufferOffset() < uint64(len(input)) {
		return obj, fmt.Errorf("Some input bytes were not read")
	}
	return obj, err
}

type Operator__Add struct {
}

func (*Operator__Add) isOperator() {}

func (obj *Operator__Add) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(0)
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Operator__Add) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Operator__Add(deserializer serde.Deserializer) (Operator__Add, error) {
	var obj Operator__Add
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Operator__Sub struct {
}

func (*Operator__Sub) isOperator() {}

func (obj *Operator__Sub) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(1)
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Operator__Sub) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Operator__Sub(deserializer serde.Deserializer) (Operator__Sub, error) {
	var obj Operator__Sub
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Operator__Mul struct {
}

func (*Operator__Mul) isOperator() {}

func (obj *Operator__Mul) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(2)
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Operator__Mul) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Operator__Mul(deserializer serde.Deserializer) (Operator__Mul, error) {
	var obj Operator__Mul
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Operator__Div struct {
}

func (*Operator__Div) isOperator() {}

func (obj *Operator__Div) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(3)
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Operator__Div) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Operator__Div(deserializer serde.Deserializer) (Operator__Div, error) {
	var obj Operator__Div
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Operator__Lt struct {
}

func (*Operator__Lt) isOperator() {}

func (obj *Operator__Lt) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(4)
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Operator__Lt) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Operator__Lt(deserializer serde.Deserializer) (Operator__Lt, error) {
	var obj Operator__Lt
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Operator__Le struct {
}

func (*Operator__Le) isOperator() {}

func (obj *Operator__Le) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(5)
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Operator__Le) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Operator__Le(deserializer serde.Deserializer) (Operator__Le, error) {
	var obj Operator__Le
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Operator__Gt struct {
}

func (*Operator__Gt) isOperator() {}

func (obj *Operator__Gt) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(6)
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Operator__Gt) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Operator__Gt(deserializer serde.Deserializer) (Operator__Gt, error) {
	var obj Operator__Gt
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Operator__Ge struct {
}

func (*Operator__Ge) isOperator() {}

func (obj *Operator__Ge) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(7)
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Operator__Ge) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Operator__Ge(deserializer serde.Deserializer) (Operator__Ge, error) {
	var obj Operator__Ge
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Operator__Rem struct {
}

func (*Operator__Rem) isOperator() {}

func (obj *Operator__Rem) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(8)
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Operator__Rem) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Operator__Rem(deserializer serde.Deserializer) (Operator__Rem, error) {
	var obj Operator__Rem
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Operator__Eq struct {
}

func (*Operator__Eq) isOperator() {}

func (obj *Operator__Eq) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(9)
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Operator__Eq) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Operator__Eq(deserializer serde.Deserializer) (Operator__Eq, error) {
	var obj Operator__Eq
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Operator__Ne struct {
}

func (*Operator__Ne) isOperator() {}

func (obj *Operator__Ne) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(10)
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Operator__Ne) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Operator__Ne(deserializer serde.Deserializer) (Operator__Ne, error) {
	var obj Operator__Ne
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Operator__And struct {
}

func (*Operator__And) isOperator() {}

func (obj *Operator__And) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(11)
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Operator__And) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Operator__And(deserializer serde.Deserializer) (Operator__And, error) {
	var obj Operator__And
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Operator__Or struct {
}

func (*Operator__Or) isOperator() {}

func (obj *Operator__Or) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(12)
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Operator__Or) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Operator__Or(deserializer serde.Deserializer) (Operator__Or, error) {
	var obj Operator__Or
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Package struct {
	Name   string
	Files  []File
	Errors map[string][]Error
}

func (obj *Package) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	if err := serializer.SerializeStr(obj.Name); err != nil {
		return err
	}
	if err := serialize_vector_File(obj.Files, serializer); err != nil {
		return err
	}
	if err := serialize_map_str_to_vector_Error(obj.Errors, serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Package) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func DeserializePackage(deserializer serde.Deserializer) (Package, error) {
	var obj Package
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := deserializer.DeserializeStr(); err == nil {
		obj.Name = val
	} else {
		return obj, err
	}
	if val, err := deserialize_vector_File(deserializer); err == nil {
		obj.Files = val
	} else {
		return obj, err
	}
	if val, err := deserialize_map_str_to_vector_Error(deserializer); err == nil {
		obj.Errors = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

func BincodeDeserializePackage(input []byte) (Package, error) {
	if input == nil {
		var obj Package
		return obj, fmt.Errorf("Cannot deserialize null array")
	}
	deserializer := bincode.NewDeserializer(input)
	obj, err := DeserializePackage(deserializer)
	if err == nil && deserializer.GetBufferOffset() < uint64(len(input)) {
		return obj, fmt.Errorf("Some input bytes were not read")
	}
	return obj, err
}

type ParseError struct {
	Msg  string
	Span Span
}

func (obj *ParseError) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	if err := serializer.SerializeStr(obj.Msg); err != nil {
		return err
	}
	if err := obj.Span.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *ParseError) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func DeserializeParseError(deserializer serde.Deserializer) (ParseError, error) {
	var obj ParseError
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := deserializer.DeserializeStr(); err == nil {
		obj.Msg = val
	} else {
		return obj, err
	}
	if val, err := DeserializeSpan(deserializer); err == nil {
		obj.Span = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

func BincodeDeserializeParseError(input []byte) (ParseError, error) {
	if input == nil {
		var obj ParseError
		return obj, fmt.Errorf("Cannot deserialize null array")
	}
	deserializer := bincode.NewDeserializer(input)
	obj, err := DeserializeParseError(deserializer)
	if err == nil && deserializer.GetBufferOffset() < uint64(len(input)) {
		return obj, fmt.Errorf("Some input bytes were not read")
	}
	return obj, err
}

type Pat interface {
	isPat()
	Serialize(serializer serde.Serializer) error
	BincodeSerialize() ([]byte, error)
}

func DeserializePat(deserializer serde.Deserializer) (Pat, error) {
	index, err := deserializer.DeserializeVariantIndex()
	if err != nil {
		return nil, err
	}

	switch index {
	case 0:
		if val, err := load_Pat__Type(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 1:
		if val, err := load_Pat__Lit(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 2:
		if val, err := load_Pat__Pat(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 3:
		if val, err := load_Pat__Struct(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 4:
		if val, err := load_Pat__Wild(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 5:
		if val, err := load_Pat__Unit(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	default:
		return nil, fmt.Errorf("Unknown variant index for Pat: %d", index)
	}
}

func BincodeDeserializePat(input []byte) (Pat, error) {
	if input == nil {
		var obj Pat
		return obj, fmt.Errorf("Cannot deserialize null array")
	}
	deserializer := bincode.NewDeserializer(input)
	obj, err := DeserializePat(deserializer)
	if err == nil && deserializer.GetBufferOffset() < uint64(len(input)) {
		return obj, fmt.Errorf("Some input bytes were not read")
	}
	return obj, err
}

type Pat__Type struct {
	Ident string
	IsMut bool
	Ann   TypeAst
	Span  Span
}

func (*Pat__Type) isPat() {}

func (obj *Pat__Type) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(0)
	if err := serializer.SerializeStr(obj.Ident); err != nil {
		return err
	}
	if err := serializer.SerializeBool(obj.IsMut); err != nil {
		return err
	}
	if err := obj.Ann.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Span.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Pat__Type) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Pat__Type(deserializer serde.Deserializer) (Pat__Type, error) {
	var obj Pat__Type
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := deserializer.DeserializeStr(); err == nil {
		obj.Ident = val
	} else {
		return obj, err
	}
	if val, err := deserializer.DeserializeBool(); err == nil {
		obj.IsMut = val
	} else {
		return obj, err
	}
	if val, err := DeserializeTypeAst(deserializer); err == nil {
		obj.Ann = val
	} else {
		return obj, err
	}
	if val, err := DeserializeSpan(deserializer); err == nil {
		obj.Span = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Pat__Lit struct {
	Lit  Literal
	Ty   Type
	Span Span
}

func (*Pat__Lit) isPat() {}

func (obj *Pat__Lit) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(1)
	if err := obj.Lit.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Ty.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Span.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Pat__Lit) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Pat__Lit(deserializer serde.Deserializer) (Pat__Lit, error) {
	var obj Pat__Lit
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := DeserializeLiteral(deserializer); err == nil {
		obj.Lit = val
	} else {
		return obj, err
	}
	if val, err := DeserializeType(deserializer); err == nil {
		obj.Ty = val
	} else {
		return obj, err
	}
	if val, err := DeserializeSpan(deserializer); err == nil {
		obj.Span = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Pat__Pat struct {
	Ident string
	Elems []Pat
	Ty    Type
	Span  Span
}

func (*Pat__Pat) isPat() {}

func (obj *Pat__Pat) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(2)
	if err := serializer.SerializeStr(obj.Ident); err != nil {
		return err
	}
	if err := serialize_vector_Pat(obj.Elems, serializer); err != nil {
		return err
	}
	if err := obj.Ty.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Span.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Pat__Pat) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Pat__Pat(deserializer serde.Deserializer) (Pat__Pat, error) {
	var obj Pat__Pat
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := deserializer.DeserializeStr(); err == nil {
		obj.Ident = val
	} else {
		return obj, err
	}
	if val, err := deserialize_vector_Pat(deserializer); err == nil {
		obj.Elems = val
	} else {
		return obj, err
	}
	if val, err := DeserializeType(deserializer); err == nil {
		obj.Ty = val
	} else {
		return obj, err
	}
	if val, err := DeserializeSpan(deserializer); err == nil {
		obj.Span = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Pat__Struct struct {
	Ident  string
	Fields []StructFieldPat
	Ty     Type
	Span   Span
}

func (*Pat__Struct) isPat() {}

func (obj *Pat__Struct) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(3)
	if err := serializer.SerializeStr(obj.Ident); err != nil {
		return err
	}
	if err := serialize_vector_StructFieldPat(obj.Fields, serializer); err != nil {
		return err
	}
	if err := obj.Ty.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Span.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Pat__Struct) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Pat__Struct(deserializer serde.Deserializer) (Pat__Struct, error) {
	var obj Pat__Struct
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := deserializer.DeserializeStr(); err == nil {
		obj.Ident = val
	} else {
		return obj, err
	}
	if val, err := deserialize_vector_StructFieldPat(deserializer); err == nil {
		obj.Fields = val
	} else {
		return obj, err
	}
	if val, err := DeserializeType(deserializer); err == nil {
		obj.Ty = val
	} else {
		return obj, err
	}
	if val, err := DeserializeSpan(deserializer); err == nil {
		obj.Span = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Pat__Wild struct {
	Span Span
}

func (*Pat__Wild) isPat() {}

func (obj *Pat__Wild) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(4)
	if err := obj.Span.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Pat__Wild) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Pat__Wild(deserializer serde.Deserializer) (Pat__Wild, error) {
	var obj Pat__Wild
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := DeserializeSpan(deserializer); err == nil {
		obj.Span = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Pat__Unit struct {
	Span Span
}

func (*Pat__Unit) isPat() {}

func (obj *Pat__Unit) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(5)
	if err := obj.Span.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Pat__Unit) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Pat__Unit(deserializer serde.Deserializer) (Pat__Unit, error) {
	var obj Pat__Unit
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := DeserializeSpan(deserializer); err == nil {
		obj.Span = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Project struct {
	Packages map[string]Package
	Output   map[string]EmittedFile
}

func (obj *Project) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	if err := serialize_map_str_to_Package(obj.Packages, serializer); err != nil {
		return err
	}
	if err := serialize_map_str_to_EmittedFile(obj.Output, serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Project) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func DeserializeProject(deserializer serde.Deserializer) (Project, error) {
	var obj Project
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := deserialize_map_str_to_Package(deserializer); err == nil {
		obj.Packages = val
	} else {
		return obj, err
	}
	if val, err := deserialize_map_str_to_EmittedFile(deserializer); err == nil {
		obj.Output = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

func BincodeDeserializeProject(input []byte) (Project, error) {
	if input == nil {
		var obj Project
		return obj, fmt.Errorf("Cannot deserialize null array")
	}
	deserializer := bincode.NewDeserializer(input)
	obj, err := DeserializeProject(deserializer)
	if err == nil && deserializer.GetBufferOffset() < uint64(len(input)) {
		return obj, fmt.Errorf("Some input bytes were not read")
	}
	return obj, err
}

type Span struct {
	Start LineColumn
	End   LineColumn
}

func (obj *Span) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	if err := obj.Start.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.End.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Span) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func DeserializeSpan(deserializer serde.Deserializer) (Span, error) {
	var obj Span
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := DeserializeLineColumn(deserializer); err == nil {
		obj.Start = val
	} else {
		return obj, err
	}
	if val, err := DeserializeLineColumn(deserializer); err == nil {
		obj.End = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

func BincodeDeserializeSpan(input []byte) (Span, error) {
	if input == nil {
		var obj Span
		return obj, fmt.Errorf("Cannot deserialize null array")
	}
	deserializer := bincode.NewDeserializer(input)
	obj, err := DeserializeSpan(deserializer)
	if err == nil && deserializer.GetBufferOffset() < uint64(len(input)) {
		return obj, fmt.Errorf("Some input bytes were not read")
	}
	return obj, err
}

type StructDefinition struct {
	Name     string
	Generics []string
	Fields   []StructFieldDef
}

func (obj *StructDefinition) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	if err := serializer.SerializeStr(obj.Name); err != nil {
		return err
	}
	if err := serialize_vector_str(obj.Generics, serializer); err != nil {
		return err
	}
	if err := serialize_vector_StructFieldDef(obj.Fields, serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *StructDefinition) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func DeserializeStructDefinition(deserializer serde.Deserializer) (StructDefinition, error) {
	var obj StructDefinition
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := deserializer.DeserializeStr(); err == nil {
		obj.Name = val
	} else {
		return obj, err
	}
	if val, err := deserialize_vector_str(deserializer); err == nil {
		obj.Generics = val
	} else {
		return obj, err
	}
	if val, err := deserialize_vector_StructFieldDef(deserializer); err == nil {
		obj.Fields = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

func BincodeDeserializeStructDefinition(input []byte) (StructDefinition, error) {
	if input == nil {
		var obj StructDefinition
		return obj, fmt.Errorf("Cannot deserialize null array")
	}
	deserializer := bincode.NewDeserializer(input)
	obj, err := DeserializeStructDefinition(deserializer)
	if err == nil && deserializer.GetBufferOffset() < uint64(len(input)) {
		return obj, fmt.Errorf("Some input bytes were not read")
	}
	return obj, err
}

type StructField struct {
	Name  string
	Value Expr
}

func (obj *StructField) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	if err := serializer.SerializeStr(obj.Name); err != nil {
		return err
	}
	if err := obj.Value.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *StructField) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func DeserializeStructField(deserializer serde.Deserializer) (StructField, error) {
	var obj StructField
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := deserializer.DeserializeStr(); err == nil {
		obj.Name = val
	} else {
		return obj, err
	}
	if val, err := DeserializeExpr(deserializer); err == nil {
		obj.Value = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

func BincodeDeserializeStructField(input []byte) (StructField, error) {
	if input == nil {
		var obj StructField
		return obj, fmt.Errorf("Cannot deserialize null array")
	}
	deserializer := bincode.NewDeserializer(input)
	obj, err := DeserializeStructField(deserializer)
	if err == nil && deserializer.GetBufferOffset() < uint64(len(input)) {
		return obj, fmt.Errorf("Some input bytes were not read")
	}
	return obj, err
}

type StructFieldDef struct {
	Name string
	Ann  TypeAst
	Ty   Type
}

func (obj *StructFieldDef) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	if err := serializer.SerializeStr(obj.Name); err != nil {
		return err
	}
	if err := obj.Ann.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Ty.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *StructFieldDef) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func DeserializeStructFieldDef(deserializer serde.Deserializer) (StructFieldDef, error) {
	var obj StructFieldDef
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := deserializer.DeserializeStr(); err == nil {
		obj.Name = val
	} else {
		return obj, err
	}
	if val, err := DeserializeTypeAst(deserializer); err == nil {
		obj.Ann = val
	} else {
		return obj, err
	}
	if val, err := DeserializeType(deserializer); err == nil {
		obj.Ty = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

func BincodeDeserializeStructFieldDef(input []byte) (StructFieldDef, error) {
	if input == nil {
		var obj StructFieldDef
		return obj, fmt.Errorf("Cannot deserialize null array")
	}
	deserializer := bincode.NewDeserializer(input)
	obj, err := DeserializeStructFieldDef(deserializer)
	if err == nil && deserializer.GetBufferOffset() < uint64(len(input)) {
		return obj, fmt.Errorf("Some input bytes were not read")
	}
	return obj, err
}

type StructFieldPat struct {
	Name  string
	Value Pat
}

func (obj *StructFieldPat) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	if err := serializer.SerializeStr(obj.Name); err != nil {
		return err
	}
	if err := obj.Value.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *StructFieldPat) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func DeserializeStructFieldPat(deserializer serde.Deserializer) (StructFieldPat, error) {
	var obj StructFieldPat
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := deserializer.DeserializeStr(); err == nil {
		obj.Name = val
	} else {
		return obj, err
	}
	if val, err := DeserializePat(deserializer); err == nil {
		obj.Value = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

func BincodeDeserializeStructFieldPat(input []byte) (StructFieldPat, error) {
	if input == nil {
		var obj StructFieldPat
		return obj, fmt.Errorf("Cannot deserialize null array")
	}
	deserializer := bincode.NewDeserializer(input)
	obj, err := DeserializeStructFieldPat(deserializer)
	if err == nil && deserializer.GetBufferOffset() < uint64(len(input)) {
		return obj, fmt.Errorf("Some input bytes were not read")
	}
	return obj, err
}

type Type interface {
	isType()
	Serialize(serializer serde.Serializer) error
	BincodeSerialize() ([]byte, error)
}

func DeserializeType(deserializer serde.Deserializer) (Type, error) {
	index, err := deserializer.DeserializeVariantIndex()
	if err != nil {
		return nil, err
	}

	switch index {
	case 0:
		if val, err := load_Type__Con(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 1:
		if val, err := load_Type__Fun(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 2:
		if val, err := load_Type__Var(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	default:
		return nil, fmt.Errorf("Unknown variant index for Type: %d", index)
	}
}

func BincodeDeserializeType(input []byte) (Type, error) {
	if input == nil {
		var obj Type
		return obj, fmt.Errorf("Cannot deserialize null array")
	}
	deserializer := bincode.NewDeserializer(input)
	obj, err := DeserializeType(deserializer)
	if err == nil && deserializer.GetBufferOffset() < uint64(len(input)) {
		return obj, fmt.Errorf("Some input bytes were not read")
	}
	return obj, err
}

type Type__Con struct {
	Name string
	Args []Type
}

func (*Type__Con) isType() {}

func (obj *Type__Con) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(0)
	if err := serializer.SerializeStr(obj.Name); err != nil {
		return err
	}
	if err := serialize_vector_Type(obj.Args, serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Type__Con) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Type__Con(deserializer serde.Deserializer) (Type__Con, error) {
	var obj Type__Con
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := deserializer.DeserializeStr(); err == nil {
		obj.Name = val
	} else {
		return obj, err
	}
	if val, err := deserialize_vector_Type(deserializer); err == nil {
		obj.Args = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Type__Fun struct {
	Args   []Type
	Bounds []Type
	Ret    Type
	Fx     []string
}

func (*Type__Fun) isType() {}

func (obj *Type__Fun) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(1)
	if err := serialize_vector_Type(obj.Args, serializer); err != nil {
		return err
	}
	if err := serialize_vector_Type(obj.Bounds, serializer); err != nil {
		return err
	}
	if err := obj.Ret.Serialize(serializer); err != nil {
		return err
	}
	if err := serialize_vector_str(obj.Fx, serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Type__Fun) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Type__Fun(deserializer serde.Deserializer) (Type__Fun, error) {
	var obj Type__Fun
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := deserialize_vector_Type(deserializer); err == nil {
		obj.Args = val
	} else {
		return obj, err
	}
	if val, err := deserialize_vector_Type(deserializer); err == nil {
		obj.Bounds = val
	} else {
		return obj, err
	}
	if val, err := DeserializeType(deserializer); err == nil {
		obj.Ret = val
	} else {
		return obj, err
	}
	if val, err := deserialize_vector_str(deserializer); err == nil {
		obj.Fx = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type Type__Var int32

func (*Type__Var) isType() {}

func (obj *Type__Var) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(2)
	if err := serializer.SerializeI32(((int32)(*obj))); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *Type__Var) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_Type__Var(deserializer serde.Deserializer) (Type__Var, error) {
	var obj int32
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return (Type__Var)(obj), err
	}
	if val, err := deserializer.DeserializeI32(); err == nil {
		obj = val
	} else {
		return ((Type__Var)(obj)), err
	}
	deserializer.DecreaseContainerDepth()
	return (Type__Var)(obj), nil
}

type TypeAst interface {
	isTypeAst()
	Serialize(serializer serde.Serializer) error
	BincodeSerialize() ([]byte, error)
}

func DeserializeTypeAst(deserializer serde.Deserializer) (TypeAst, error) {
	index, err := deserializer.DeserializeVariantIndex()
	if err != nil {
		return nil, err
	}

	switch index {
	case 0:
		if val, err := load_TypeAst__Con(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 1:
		if val, err := load_TypeAst__Fun(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 2:
		if val, err := load_TypeAst__Unknown(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	default:
		return nil, fmt.Errorf("Unknown variant index for TypeAst: %d", index)
	}
}

func BincodeDeserializeTypeAst(input []byte) (TypeAst, error) {
	if input == nil {
		var obj TypeAst
		return obj, fmt.Errorf("Cannot deserialize null array")
	}
	deserializer := bincode.NewDeserializer(input)
	obj, err := DeserializeTypeAst(deserializer)
	if err == nil && deserializer.GetBufferOffset() < uint64(len(input)) {
		return obj, fmt.Errorf("Some input bytes were not read")
	}
	return obj, err
}

type TypeAst__Con struct {
	Name string
	Args []TypeAst
}

func (*TypeAst__Con) isTypeAst() {}

func (obj *TypeAst__Con) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(0)
	if err := serializer.SerializeStr(obj.Name); err != nil {
		return err
	}
	if err := serialize_vector_TypeAst(obj.Args, serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *TypeAst__Con) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_TypeAst__Con(deserializer serde.Deserializer) (TypeAst__Con, error) {
	var obj TypeAst__Con
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := deserializer.DeserializeStr(); err == nil {
		obj.Name = val
	} else {
		return obj, err
	}
	if val, err := deserialize_vector_TypeAst(deserializer); err == nil {
		obj.Args = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type TypeAst__Fun struct {
	Args []TypeAst
	Ret  TypeAst
}

func (*TypeAst__Fun) isTypeAst() {}

func (obj *TypeAst__Fun) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(1)
	if err := serialize_vector_TypeAst(obj.Args, serializer); err != nil {
		return err
	}
	if err := obj.Ret.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *TypeAst__Fun) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_TypeAst__Fun(deserializer serde.Deserializer) (TypeAst__Fun, error) {
	var obj TypeAst__Fun
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := deserialize_vector_TypeAst(deserializer); err == nil {
		obj.Args = val
	} else {
		return obj, err
	}
	if val, err := DeserializeTypeAst(deserializer); err == nil {
		obj.Ret = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type TypeAst__Unknown struct {
}

func (*TypeAst__Unknown) isTypeAst() {}

func (obj *TypeAst__Unknown) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(2)
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *TypeAst__Unknown) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_TypeAst__Unknown(deserializer serde.Deserializer) (TypeAst__Unknown, error) {
	var obj TypeAst__Unknown
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type UnOp interface {
	isUnOp()
	Serialize(serializer serde.Serializer) error
	BincodeSerialize() ([]byte, error)
}

func DeserializeUnOp(deserializer serde.Deserializer) (UnOp, error) {
	index, err := deserializer.DeserializeVariantIndex()
	if err != nil {
		return nil, err
	}

	switch index {
	case 0:
		if val, err := load_UnOp__Neg(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	case 1:
		if val, err := load_UnOp__Not(deserializer); err == nil {
			return &val, nil
		} else {
			return nil, err
		}

	default:
		return nil, fmt.Errorf("Unknown variant index for UnOp: %d", index)
	}
}

func BincodeDeserializeUnOp(input []byte) (UnOp, error) {
	if input == nil {
		var obj UnOp
		return obj, fmt.Errorf("Cannot deserialize null array")
	}
	deserializer := bincode.NewDeserializer(input)
	obj, err := DeserializeUnOp(deserializer)
	if err == nil && deserializer.GetBufferOffset() < uint64(len(input)) {
		return obj, fmt.Errorf("Some input bytes were not read")
	}
	return obj, err
}

type UnOp__Neg struct {
}

func (*UnOp__Neg) isUnOp() {}

func (obj *UnOp__Neg) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(0)
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *UnOp__Neg) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_UnOp__Neg(deserializer serde.Deserializer) (UnOp__Neg, error) {
	var obj UnOp__Neg
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type UnOp__Not struct {
}

func (*UnOp__Not) isUnOp() {}

func (obj *UnOp__Not) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	serializer.SerializeVariantIndex(1)
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *UnOp__Not) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func load_UnOp__Not(deserializer serde.Deserializer) (UnOp__Not, error) {
	var obj UnOp__Not
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

type UnificationError struct {
	Msg      string
	Expected Type
	Actual   Type
	Span     Span
}

func (obj *UnificationError) Serialize(serializer serde.Serializer) error {
	if err := serializer.IncreaseContainerDepth(); err != nil {
		return err
	}
	if err := serializer.SerializeStr(obj.Msg); err != nil {
		return err
	}
	if err := obj.Expected.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Actual.Serialize(serializer); err != nil {
		return err
	}
	if err := obj.Span.Serialize(serializer); err != nil {
		return err
	}
	serializer.DecreaseContainerDepth()
	return nil
}

func (obj *UnificationError) BincodeSerialize() ([]byte, error) {
	if obj == nil {
		return nil, fmt.Errorf("Cannot serialize null object")
	}
	serializer := bincode.NewSerializer()
	if err := obj.Serialize(serializer); err != nil {
		return nil, err
	}
	return serializer.GetBytes(), nil
}

func DeserializeUnificationError(deserializer serde.Deserializer) (UnificationError, error) {
	var obj UnificationError
	if err := deserializer.IncreaseContainerDepth(); err != nil {
		return obj, err
	}
	if val, err := deserializer.DeserializeStr(); err == nil {
		obj.Msg = val
	} else {
		return obj, err
	}
	if val, err := DeserializeType(deserializer); err == nil {
		obj.Expected = val
	} else {
		return obj, err
	}
	if val, err := DeserializeType(deserializer); err == nil {
		obj.Actual = val
	} else {
		return obj, err
	}
	if val, err := DeserializeSpan(deserializer); err == nil {
		obj.Span = val
	} else {
		return obj, err
	}
	deserializer.DecreaseContainerDepth()
	return obj, nil
}

func BincodeDeserializeUnificationError(input []byte) (UnificationError, error) {
	if input == nil {
		var obj UnificationError
		return obj, fmt.Errorf("Cannot deserialize null array")
	}
	deserializer := bincode.NewDeserializer(input)
	obj, err := DeserializeUnificationError(deserializer)
	if err == nil && deserializer.GetBufferOffset() < uint64(len(input)) {
		return obj, fmt.Errorf("Some input bytes were not read")
	}
	return obj, err
}
func serialize_map_str_to_EmittedFile(value map[string]EmittedFile, serializer serde.Serializer) error {
	if err := serializer.SerializeLen(uint64(len(value))); err != nil {
		return err
	}
	offsets := make([]uint64, len(value))
	count := 0
	for k, v := range value {
		offsets[count] = serializer.GetBufferOffset()
		count += 1
		if err := serializer.SerializeStr(k); err != nil {
			return err
		}
		if err := v.Serialize(serializer); err != nil {
			return err
		}
	}
	serializer.SortMapEntries(offsets)
	return nil
}

func deserialize_map_str_to_EmittedFile(deserializer serde.Deserializer) (map[string]EmittedFile, error) {
	length, err := deserializer.DeserializeLen()
	if err != nil {
		return nil, err
	}
	obj := make(map[string]EmittedFile)
	previous_slice := serde.Slice{0, 0}
	for i := 0; i < int(length); i++ {
		var slice serde.Slice
		slice.Start = deserializer.GetBufferOffset()
		var key string
		if val, err := deserializer.DeserializeStr(); err == nil {
			key = val
		} else {
			return nil, err
		}
		slice.End = deserializer.GetBufferOffset()
		if i > 0 {
			err := deserializer.CheckThatKeySlicesAreIncreasing(previous_slice, slice)
			if err != nil {
				return nil, err
			}
		}
		previous_slice = slice
		if val, err := DeserializeEmittedFile(deserializer); err == nil {
			obj[key] = val
		} else {
			return nil, err
		}
	}
	return obj, nil
}

func serialize_map_str_to_Package(value map[string]Package, serializer serde.Serializer) error {
	if err := serializer.SerializeLen(uint64(len(value))); err != nil {
		return err
	}
	offsets := make([]uint64, len(value))
	count := 0
	for k, v := range value {
		offsets[count] = serializer.GetBufferOffset()
		count += 1
		if err := serializer.SerializeStr(k); err != nil {
			return err
		}
		if err := v.Serialize(serializer); err != nil {
			return err
		}
	}
	serializer.SortMapEntries(offsets)
	return nil
}

func deserialize_map_str_to_Package(deserializer serde.Deserializer) (map[string]Package, error) {
	length, err := deserializer.DeserializeLen()
	if err != nil {
		return nil, err
	}
	obj := make(map[string]Package)
	previous_slice := serde.Slice{0, 0}
	for i := 0; i < int(length); i++ {
		var slice serde.Slice
		slice.Start = deserializer.GetBufferOffset()
		var key string
		if val, err := deserializer.DeserializeStr(); err == nil {
			key = val
		} else {
			return nil, err
		}
		slice.End = deserializer.GetBufferOffset()
		if i > 0 {
			err := deserializer.CheckThatKeySlicesAreIncreasing(previous_slice, slice)
			if err != nil {
				return nil, err
			}
		}
		previous_slice = slice
		if val, err := DeserializePackage(deserializer); err == nil {
			obj[key] = val
		} else {
			return nil, err
		}
	}
	return obj, nil
}

func serialize_map_str_to_vector_Error(value map[string][]Error, serializer serde.Serializer) error {
	if err := serializer.SerializeLen(uint64(len(value))); err != nil {
		return err
	}
	offsets := make([]uint64, len(value))
	count := 0
	for k, v := range value {
		offsets[count] = serializer.GetBufferOffset()
		count += 1
		if err := serializer.SerializeStr(k); err != nil {
			return err
		}
		if err := serialize_vector_Error(v, serializer); err != nil {
			return err
		}
	}
	serializer.SortMapEntries(offsets)
	return nil
}

func deserialize_map_str_to_vector_Error(deserializer serde.Deserializer) (map[string][]Error, error) {
	length, err := deserializer.DeserializeLen()
	if err != nil {
		return nil, err
	}
	obj := make(map[string][]Error)
	previous_slice := serde.Slice{0, 0}
	for i := 0; i < int(length); i++ {
		var slice serde.Slice
		slice.Start = deserializer.GetBufferOffset()
		var key string
		if val, err := deserializer.DeserializeStr(); err == nil {
			key = val
		} else {
			return nil, err
		}
		slice.End = deserializer.GetBufferOffset()
		if i > 0 {
			err := deserializer.CheckThatKeySlicesAreIncreasing(previous_slice, slice)
			if err != nil {
				return nil, err
			}
		}
		previous_slice = slice
		if val, err := deserialize_vector_Error(deserializer); err == nil {
			obj[key] = val
		} else {
			return nil, err
		}
	}
	return obj, nil
}

func serialize_option_Expr(value *Expr, serializer serde.Serializer) error {
	if value != nil {
		if err := serializer.SerializeOptionTag(true); err != nil {
			return err
		}
		if err := (*value).Serialize(serializer); err != nil {
			return err
		}
	} else {
		if err := serializer.SerializeOptionTag(false); err != nil {
			return err
		}
	}
	return nil
}

func deserialize_option_Expr(deserializer serde.Deserializer) (*Expr, error) {
	tag, err := deserializer.DeserializeOptionTag()
	if err != nil {
		return nil, err
	}
	if tag {
		value := new(Expr)
		if val, err := DeserializeExpr(deserializer); err == nil {
			*value = val
		} else {
			return nil, err
		}
		return value, nil
	} else {
		return nil, nil
	}
}

func serialize_vector_Arm(value []Arm, serializer serde.Serializer) error {
	if err := serializer.SerializeLen(uint64(len(value))); err != nil {
		return err
	}
	for _, item := range value {
		if err := item.Serialize(serializer); err != nil {
			return err
		}
	}
	return nil
}

func deserialize_vector_Arm(deserializer serde.Deserializer) ([]Arm, error) {
	length, err := deserializer.DeserializeLen()
	if err != nil {
		return nil, err
	}
	obj := make([]Arm, length)
	for i := range obj {
		if val, err := DeserializeArm(deserializer); err == nil {
			obj[i] = val
		} else {
			return nil, err
		}
	}
	return obj, nil
}

func serialize_vector_Binding(value []Binding, serializer serde.Serializer) error {
	if err := serializer.SerializeLen(uint64(len(value))); err != nil {
		return err
	}
	for _, item := range value {
		if err := item.Serialize(serializer); err != nil {
			return err
		}
	}
	return nil
}

func deserialize_vector_Binding(deserializer serde.Deserializer) ([]Binding, error) {
	length, err := deserializer.DeserializeLen()
	if err != nil {
		return nil, err
	}
	obj := make([]Binding, length)
	for i := range obj {
		if val, err := DeserializeBinding(deserializer); err == nil {
			obj[i] = val
		} else {
			return nil, err
		}
	}
	return obj, nil
}

func serialize_vector_Constructor(value []Constructor, serializer serde.Serializer) error {
	if err := serializer.SerializeLen(uint64(len(value))); err != nil {
		return err
	}
	for _, item := range value {
		if err := item.Serialize(serializer); err != nil {
			return err
		}
	}
	return nil
}

func deserialize_vector_Constructor(deserializer serde.Deserializer) ([]Constructor, error) {
	length, err := deserializer.DeserializeLen()
	if err != nil {
		return nil, err
	}
	obj := make([]Constructor, length)
	for i := range obj {
		if val, err := DeserializeConstructor(deserializer); err == nil {
			obj[i] = val
		} else {
			return nil, err
		}
	}
	return obj, nil
}

func serialize_vector_EnumFieldDef(value []EnumFieldDef, serializer serde.Serializer) error {
	if err := serializer.SerializeLen(uint64(len(value))); err != nil {
		return err
	}
	for _, item := range value {
		if err := item.Serialize(serializer); err != nil {
			return err
		}
	}
	return nil
}

func deserialize_vector_EnumFieldDef(deserializer serde.Deserializer) ([]EnumFieldDef, error) {
	length, err := deserializer.DeserializeLen()
	if err != nil {
		return nil, err
	}
	obj := make([]EnumFieldDef, length)
	for i := range obj {
		if val, err := DeserializeEnumFieldDef(deserializer); err == nil {
			obj[i] = val
		} else {
			return nil, err
		}
	}
	return obj, nil
}

func serialize_vector_Error(value []Error, serializer serde.Serializer) error {
	if err := serializer.SerializeLen(uint64(len(value))); err != nil {
		return err
	}
	for _, item := range value {
		if err := item.Serialize(serializer); err != nil {
			return err
		}
	}
	return nil
}

func deserialize_vector_Error(deserializer serde.Deserializer) ([]Error, error) {
	length, err := deserializer.DeserializeLen()
	if err != nil {
		return nil, err
	}
	obj := make([]Error, length)
	for i := range obj {
		if val, err := DeserializeError(deserializer); err == nil {
			obj[i] = val
		} else {
			return nil, err
		}
	}
	return obj, nil
}

func serialize_vector_Expr(value []Expr, serializer serde.Serializer) error {
	if err := serializer.SerializeLen(uint64(len(value))); err != nil {
		return err
	}
	for _, item := range value {
		if err := item.Serialize(serializer); err != nil {
			return err
		}
	}
	return nil
}

func deserialize_vector_Expr(deserializer serde.Deserializer) ([]Expr, error) {
	length, err := deserializer.DeserializeLen()
	if err != nil {
		return nil, err
	}
	obj := make([]Expr, length)
	for i := range obj {
		if val, err := DeserializeExpr(deserializer); err == nil {
			obj[i] = val
		} else {
			return nil, err
		}
	}
	return obj, nil
}

func serialize_vector_File(value []File, serializer serde.Serializer) error {
	if err := serializer.SerializeLen(uint64(len(value))); err != nil {
		return err
	}
	for _, item := range value {
		if err := item.Serialize(serializer); err != nil {
			return err
		}
	}
	return nil
}

func deserialize_vector_File(deserializer serde.Deserializer) ([]File, error) {
	length, err := deserializer.DeserializeLen()
	if err != nil {
		return nil, err
	}
	obj := make([]File, length)
	for i := range obj {
		if val, err := DeserializeFile(deserializer); err == nil {
			obj[i] = val
		} else {
			return nil, err
		}
	}
	return obj, nil
}

func serialize_vector_Pat(value []Pat, serializer serde.Serializer) error {
	if err := serializer.SerializeLen(uint64(len(value))); err != nil {
		return err
	}
	for _, item := range value {
		if err := item.Serialize(serializer); err != nil {
			return err
		}
	}
	return nil
}

func deserialize_vector_Pat(deserializer serde.Deserializer) ([]Pat, error) {
	length, err := deserializer.DeserializeLen()
	if err != nil {
		return nil, err
	}
	obj := make([]Pat, length)
	for i := range obj {
		if val, err := DeserializePat(deserializer); err == nil {
			obj[i] = val
		} else {
			return nil, err
		}
	}
	return obj, nil
}

func serialize_vector_StructField(value []StructField, serializer serde.Serializer) error {
	if err := serializer.SerializeLen(uint64(len(value))); err != nil {
		return err
	}
	for _, item := range value {
		if err := item.Serialize(serializer); err != nil {
			return err
		}
	}
	return nil
}

func deserialize_vector_StructField(deserializer serde.Deserializer) ([]StructField, error) {
	length, err := deserializer.DeserializeLen()
	if err != nil {
		return nil, err
	}
	obj := make([]StructField, length)
	for i := range obj {
		if val, err := DeserializeStructField(deserializer); err == nil {
			obj[i] = val
		} else {
			return nil, err
		}
	}
	return obj, nil
}

func serialize_vector_StructFieldDef(value []StructFieldDef, serializer serde.Serializer) error {
	if err := serializer.SerializeLen(uint64(len(value))); err != nil {
		return err
	}
	for _, item := range value {
		if err := item.Serialize(serializer); err != nil {
			return err
		}
	}
	return nil
}

func deserialize_vector_StructFieldDef(deserializer serde.Deserializer) ([]StructFieldDef, error) {
	length, err := deserializer.DeserializeLen()
	if err != nil {
		return nil, err
	}
	obj := make([]StructFieldDef, length)
	for i := range obj {
		if val, err := DeserializeStructFieldDef(deserializer); err == nil {
			obj[i] = val
		} else {
			return nil, err
		}
	}
	return obj, nil
}

func serialize_vector_StructFieldPat(value []StructFieldPat, serializer serde.Serializer) error {
	if err := serializer.SerializeLen(uint64(len(value))); err != nil {
		return err
	}
	for _, item := range value {
		if err := item.Serialize(serializer); err != nil {
			return err
		}
	}
	return nil
}

func deserialize_vector_StructFieldPat(deserializer serde.Deserializer) ([]StructFieldPat, error) {
	length, err := deserializer.DeserializeLen()
	if err != nil {
		return nil, err
	}
	obj := make([]StructFieldPat, length)
	for i := range obj {
		if val, err := DeserializeStructFieldPat(deserializer); err == nil {
			obj[i] = val
		} else {
			return nil, err
		}
	}
	return obj, nil
}

func serialize_vector_Type(value []Type, serializer serde.Serializer) error {
	if err := serializer.SerializeLen(uint64(len(value))); err != nil {
		return err
	}
	for _, item := range value {
		if err := item.Serialize(serializer); err != nil {
			return err
		}
	}
	return nil
}

func deserialize_vector_Type(deserializer serde.Deserializer) ([]Type, error) {
	length, err := deserializer.DeserializeLen()
	if err != nil {
		return nil, err
	}
	obj := make([]Type, length)
	for i := range obj {
		if val, err := DeserializeType(deserializer); err == nil {
			obj[i] = val
		} else {
			return nil, err
		}
	}
	return obj, nil
}

func serialize_vector_TypeAst(value []TypeAst, serializer serde.Serializer) error {
	if err := serializer.SerializeLen(uint64(len(value))); err != nil {
		return err
	}
	for _, item := range value {
		if err := item.Serialize(serializer); err != nil {
			return err
		}
	}
	return nil
}

func deserialize_vector_TypeAst(deserializer serde.Deserializer) ([]TypeAst, error) {
	length, err := deserializer.DeserializeLen()
	if err != nil {
		return nil, err
	}
	obj := make([]TypeAst, length)
	for i := range obj {
		if val, err := DeserializeTypeAst(deserializer); err == nil {
			obj[i] = val
		} else {
			return nil, err
		}
	}
	return obj, nil
}

func serialize_vector_str(value []string, serializer serde.Serializer) error {
	if err := serializer.SerializeLen(uint64(len(value))); err != nil {
		return err
	}
	for _, item := range value {
		if err := serializer.SerializeStr(item); err != nil {
			return err
		}
	}
	return nil
}

func deserialize_vector_str(deserializer serde.Deserializer) ([]string, error) {
	length, err := deserializer.DeserializeLen()
	if err != nil {
		return nil, err
	}
	obj := make([]string, length)
	for i := range obj {
		if val, err := deserializer.DeserializeStr(); err == nil {
			obj[i] = val
		} else {
			return nil, err
		}
	}
	return obj, nil
}

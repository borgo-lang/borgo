package testpkg

import "io"

type HobbyKind int

type AliasForString = string

const (
	HobbyBueno HobbyKind = iota
	HobbyNoBueno
)

const SomeConst = 1

type Person struct {
	Name    string
	Hobbies []Hobby
}

type Hobby struct {
	Name string
	Kind HobbyKind
}

type Container[T any] struct {
	X T
}

type AnInt int

type NoEmbedded interface {
	Check() string
}

type Foo interface {
	io.ReadCloser
	Bar() string
	TakeFn(f func(rune) bool)
}

func Simple() {}

func Withdata(a int, b []string) Person {
	return Person{}
}

func (p Person) GetName() string {
	return p.Name
}

func WithFunc(f func(x int) string) {}

func WithMap(m map[int]bool) {}

func WithGenerics[T any, Y any](x T, y Y)          {}
func WithGenericsConstrained[T io.ReadCloser](x T) {}

func WithPointers(p *Person) {}

func WithMultipleReturn() (int, error) { return 1, nil }

func WithVariadic(v ...any) {}

func WithSelector(r io.Reader) {}

func WithChan(read <-chan int, write chan<- string) {}

func WithLiteralStruct(s struct{}) {}

func (c *Container[T]) UpdateContainer() {}
func (c Container[Y]) MethodValue()      {}

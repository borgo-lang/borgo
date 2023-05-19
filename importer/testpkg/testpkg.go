package testpkg

import "io"

type HobbyKind int

const (
	HobbyBueno HobbyKind = iota
	HobbyNoBueno
)

type Person struct {
	Name    string
	Hobbies []Hobby
}

type Hobby struct {
	Kind HobbyKind
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

func WithGenerics[T any, Y any](x T, y Y) {}

func WithPointers(p *Person) {}

func WithMultipleReturn() (int, error) { return 1, nil }

func WithVariadic(v ...any) {}

func WithSelector(r io.Reader) {}

func WithChan(read <-chan int, write chan<- string) {}

func WithLiteralStruct(s struct{}) {}
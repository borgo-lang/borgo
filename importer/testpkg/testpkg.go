package testpkg

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

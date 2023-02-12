package main

import (
	borgo "borgo/runtime"
	"syscall/js"
)

func main() {

	js.Global().Set("runProject", js.FuncOf(runProject))

	borgo.SetOutputFunction(func(s string) {
		js.Global().Get("Borgo").Call("log", s)
	})

	// Keep program running
	select {}
}

func runProject(this js.Value, args []js.Value) any {
	input := args[0]
	length := input.Get("length").Int()

	buf := make([]byte, length)
	_ = js.CopyBytesToGo(buf, input)

	project := borgo.ParseProject(buf)
	err := borgo.RunMainFunction(project)

	if err != nil {
		borgo.Debug(err.Error())
	}

	return 0
}

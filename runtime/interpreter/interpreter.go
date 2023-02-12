package main

import (
	"io/ioutil"

	borgo "borgo/runtime"
)

func main() {
	file, err := ioutil.ReadFile("project.bin")
	borgo.Assert(err)

	project := borgo.ParseProject(file)
	borgo.RunMainFunction(project)
}

package main

import (
	"fmt"
	"os"
	"strings"
)

type World struct {
	rotations []int
}

func parse() World {
	data, err := os.ReadFile("./input.txt")
	if err != nil {
		panic(err)
	}
	str := strings.TrimRight(string(data), "\n")
	lines := strings.Split(str, "\n")

	// solution starts

	return World{}
}

func main() {
	world := parse()
	// solution starts
	fmt.Println("Part 1:")

	fmt.Println("Part 2:")

}

// extra funcs

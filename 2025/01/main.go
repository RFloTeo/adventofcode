package main

import (
	"fmt"
	"os"
	"strconv"
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
	lines := strings.Split(string(data), "\n")

	// solution starts
	rotations := make([]int, len(lines)-1)
	for i, line := range lines {
		if len(line) == 0 {
			break
		}
		value, _ := strconv.Atoi(line[1:])
		if line[0] == 'L' {
			value *= -1
		}
		rotations[i] = value
	}
	return World{rotations}
}

func main() {
	world := parse()
	// solution starts
	fmt.Println("Part 1:")
	current, zeros := 50, 0
	for _, v := range world.rotations {
		current = (current + v) % 100
		if current == 0 {
			zeros += 1
		}
	}
	fmt.Println(zeros)

	fmt.Println("Part 2:")
	current, zeros = 50, 0
	for _, v := range world.rotations {
		current = current + v
		if current == 0 {
			zeros += 1
		} else if current < 0 {
			zeros += (current / 100) * -1
			if current-v != 0 {
				zeros += 1
			}
		} else if current >= 100 {
			zeros += current / 100
		}
		current %= 100
		if current < 0 {
			current += 100
		}
	}
	fmt.Println(zeros)
}

// extra funcs

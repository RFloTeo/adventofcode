package main

import (
	"fmt"
	"os"
	"strings"
)

type World struct {
	gameMap [][]rune
	rows    int
	cols    int
}

func parse() World {
	data, err := os.ReadFile("./input.txt")
	if err != nil {
		panic(err)
	}
	str := strings.TrimRight(string(data), "\n")
	lines := strings.Split(str, "\n")
	rows := make([][]rune, len(lines))
	rowCount := len(lines)
	colCount := len(lines[0])
	for i, line := range lines {
		rows[i] = []rune(line)
	}

	// solution starts

	return World{rows, rowCount, colCount}
}

func main() {
	world := parse()
	// solution starts
	fmt.Println("Part 1:")
	//accessible := iteration(&world)
	//fmt.Println(accessible)
	fmt.Println("Part 2:")
	accessible := 0
	for newAccessible := iteration(&world); newAccessible > 0; newAccessible = iteration(&world) {
		accessible += newAccessible
	}
	fmt.Println(accessible)

}

// extra funcs
func isAccessible(world *World, row, col int) bool {
	rolls := 0
	for i := row - 1; i <= row+1; i++ {
		for j := col - 1; j <= col+1; j++ {
			if i >= 0 && i < world.rows && j >= 0 && j < world.cols && world.gameMap[i][j] == '@' {
				rolls++
			}
		}
	}
	return rolls <= 4
}
func iteration(world *World) int {
	accessible := 0
	for i := 0; i < world.rows; i++ {
		for j := 0; j < world.cols; j++ {
			if world.gameMap[i][j] == '@' && isAccessible(world, i, j) {
				accessible++
				world.gameMap[i][j] = '.'
			}
		}
	}
	return accessible
}

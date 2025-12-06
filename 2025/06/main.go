package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

type World struct {
	nums [][]int
	ops  []rune
}

func parse() World {
	data, err := os.ReadFile("./input.txt")
	if err != nil {
		panic(err)
	}
	str := strings.TrimRight(string(data), "\n")
	lines := strings.Split(str, "\n")

	// solution starts
	nums := make([][]int, len(lines)-1)
	ops := make([]rune, 0)
	for i, line := range lines {
		if isDigit(line[0]) {
			row := make([]int, 0)
			strs := strings.Split(line, " ")
			for _, s := range strs {
				if s != "" {
					num, _ := strconv.Atoi(s)
					row = append(row, num)
				}
			}
			nums[i] = row
		} else {
			for _, c := range line {
				if c != ' ' {
					ops = append(ops, c)
				}
			}
		}
	}

	return World{nums, ops}
}

func main() {
	world := parse()
	// solution starts
	fmt.Println("Part 1:")
	sum := 0
	for col := 0; col < len(world.ops); col++ {
		op := world.ops[col]
		acc := 0
		if op == '*' {
			acc = 1
		}
		for row := 0; row < len(world.nums); row++ {
			if op == '+' {
				acc += world.nums[row][col]
			} else {
				acc *= world.nums[row][col]
			}
		}
		sum += acc
	}
	fmt.Println(sum)
	fmt.Println("Part 2:")
	sum = 0
	w2 := parse2()
	for _, multLine := range w2.mults {
		acc := 1
		for _, num := range multLine {
			acc *= num
		}
		sum += acc
	}
	for _, addLine := range w2.adds {
		for _, num := range addLine {
			sum += num
		}
	}
	fmt.Println(sum)
}

// extra funcs
func isDigit(r byte) bool {
	return r >= '0' && r <= '9'
}
func isOp(r byte) bool {
	return r == '+' || r == '*'
}

type W2 struct {
	mults [][]int
	adds  [][]int
}

func parse2() W2 {
	mults := make([][]int, 0)
	adds := make([][]int, 0)
	data, err := os.ReadFile("./input.txt")
	if err != nil {
		panic(err)
	}
	str := strings.TrimRight(string(data), "\n")
	lines := strings.Split(str, "\n")

	rows := len(lines)
	cols := len(lines[0])
	var op byte
	var allSpaces bool
	mathLine := make([]int, 0)
	for col := range cols {
		allSpaces = true
		num := 0
		for row := range rows {
			if len(lines[row]) <= col {
				continue
			}
			c := lines[row][col]
			if isDigit(c) {
				num = num*10 + int(c-'0')
				allSpaces = false
			} else if isOp(c) {
				op = c
			}
		}
		if allSpaces {
			if op == '*' {
				mults = append(mults, mathLine)
			} else {
				adds = append(adds, mathLine)
			}
			mathLine = make([]int, 0)
		} else {
			mathLine = append(mathLine, num)
		}
	}
	if op == '*' {
		mults = append(mults, mathLine)
	} else {
		adds = append(adds, mathLine)
	}

	return W2{mults, adds}
}

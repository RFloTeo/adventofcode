package main

import (
	"fmt"
	"os"
	"strings"
)

const P2_SIZE = 12

type World struct {
	lines []string
}

func parse() World {
	data, err := os.ReadFile("./input.txt")
	if err != nil {
		panic(err)
	}
	str := strings.TrimRight(string(data), "\n")
	lines := strings.Split(str, "\n")

	// solution starts

	return World{lines}
}

func main() {
	lines := parse().lines
	// solution starts
	fmt.Println("Part 1:")
	sum := 0
	for _, line := range lines {
		biggestNum := 0
		biggestTens := 0
		for _, c := range line {
			digit := ctoi(c)
			num := concat(biggestTens, digit)
			if num > biggestNum {
				biggestNum = num
			}
			if digit > biggestTens {
				biggestTens = digit
			}
		}
		sum += biggestNum
	}
	fmt.Println(sum)

	fmt.Println("Part 2:")
	sum = 0
	for _, line := range lines {
		recordedNum := [P2_SIZE]int{}
		for _, c := range line {
			digit := ctoi(c)
			replaced := false
			for i := 0; i <= P2_SIZE-2; i++ {
				if replaced || recordedNum[i] < recordedNum[i+1] {
					replaced = true
					recordedNum[i] = recordedNum[i+1]
				}
			}
			if replaced || digit > recordedNum[P2_SIZE-1] {
				recordedNum[P2_SIZE-1] = digit
			}
		}
		sum += concatArr(recordedNum)
	}
	fmt.Println(sum)
}

// extra funcs
func ctoi(c rune) int {
	return int(c - '0')
}

func concat(a, b int) int {
	return 10*a + b
}

func concatArr(arr [P2_SIZE]int) int {
	num := arr[0]
	for _, digit := range arr[1:] {
		num = num*10 + digit
	}
	return num
}

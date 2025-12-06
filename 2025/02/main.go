package main

import (
	"fmt"
	"math"
	"os"
	"strconv"
	"strings"
)

type World struct {
	intervals []Interval
}

type Interval struct {
	start int
	end   int
}

func parse() World {
	data, err := os.ReadFile("./input.txt")
	if err != nil {
		panic(err)
	}
	str := strings.TrimRight(string(data), "\n")
	lines := strings.Split(str, ",")

	// solution starts
	intervals := make([]Interval, len(lines))

	for i, line := range lines {
		parts := strings.Split(line, "-")
		start, _ := strconv.Atoi(parts[0])
		end, _ := strconv.Atoi(parts[1])
		intervals[i] = Interval{start, end}
	}
	return World{intervals}
}

func main() {
	intervals := parse().intervals
	// solution starts
	fmt.Println("Part 1:")
	sum := 0
	sum2 := 0
	for _, interval := range intervals {
		sum += intervalSum1(interval)
		sum2 += intervalSum2(interval)
	}
	fmt.Println(sum)
	fmt.Println("Part 2:")
	fmt.Println(sum2)

}

// extra funcs
func intervalSum1(i Interval) int {
	sum, startDigits, endDigits := 0, digitCount(i.start), digitCount(i.end)
	for halfDigits := (startDigits + 1) / 2; halfDigits*2 <= endDigits; halfDigits++ {
		shape := int(math.Pow10(halfDigits)) + 1 // 11, 101, 1001 etc
		valMin := int(math.Max(
			float64(i.start/shape),
			math.Pow10(halfDigits-1),
		)) // 1, 10, 100 etc
		valMax := int(math.Min(
			float64(i.end/shape),
			math.Pow10(halfDigits)-1,
		)) // 9, 99, 999 etc
		for val := valMin; val <= valMax; val++ {
			number := val * shape
			if isInside(number, i) {
				sum += number
			}
		}
	}
	return sum
}

func intervalSum2(i Interval) int {
	sum, startDigits, endDigits := 0, digitCount(i.start), digitCount(i.end)
	for digits := startDigits; digits <= endDigits; digits++ {
		var prevShapes []int
		for period := 1; period <= digits/2; period++ {
			if digits%period != 0 {
				continue
			}
			shape := makeShape(digits, period)
			valMin := int(math.Max(
				float64(i.start/shape),
				math.Pow10(period-1),
			))
			valMax := int(math.Min(
				float64(i.end/shape),
				math.Pow10(period)-1,
			))
			for val := valMin; val <= valMax; val++ {
				number := val * shape
				if isInside(number, i) && notMatches(number, prevShapes) {
					fmt.Println(number)
					sum += number
				}
			}
			prevShapes = append(prevShapes, shape)
		}
	}
	return sum
}

func digitCount(i int) int {
	digits := 0
	for i > 0 {
		digits += 1
		i /= 10
	}
	return digits
}

func isInside(n int, i Interval) bool {
	return n >= i.start && n <= i.end
}

func makeShape(totalDigits, period int) int {
	shape := 0
	for exp := 0; exp < totalDigits; exp += period {
		shape += int(math.Pow10(exp))
	}
	return shape
}

func notMatches(n int, divisors []int) bool {
	for _, d := range divisors {
		if n%d == 0 {
			return false
		}
	}
	return true
}

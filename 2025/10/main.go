package main

import (
	"fmt"
	"math"
	"os"
	"strconv"
	"strings"
)

// Target and buttons are expressed as binary
// Target has least significant bit first
// e.g. .#.##. -> 011010 -> 24
// buttons are sum of exponents
// e.g (0, 1, 3) -> 2**0 + 2**1 + 2**3 -> 11
type Machine struct {
	count    int
	target   int
	buttons  []int
	joltages []int
}

type World struct {
	machines []Machine
}

func parse() World {
	data, err := os.ReadFile("./input.txt")
	if err != nil {
		panic(err)
	}
	str := strings.TrimRight(string(data), "\n")
	lines := strings.Split(str, "\n")

	// solution starts
	machines := make([]Machine, len(lines))
	for i, line := range lines {
		machines[i] = parseMachine(line)
	}
	return World{machines}
}

func main() {
	machines := parse().machines
	// solution starts
	fmt.Println("Part 1:")
	presses := 0
	for _, m := range machines {
		if m.target == 0 {
			continue
		}
		presses += m.tryButtons(0, 0, 0)
	}
	fmt.Println(presses)

	fmt.Println("Part 2:")

}

// extra funcs
func parseMachine(line string) Machine {
	lightsEnd := strings.Index(line, "]")
	lights := line[1:lightsEnd]
	count := len(lights)
	target := 0
	exp := 1
	for _, c := range lights {
		if c == '#' {
			target += exp
		}
		exp *= 2
	}
	joltagesStart := strings.Index(line, "{")
	buttonSpecs := line[lightsEnd+2 : joltagesStart-1]
	buttons := parseButtons(buttonSpecs)
	joltagesEnd := strings.Index(line, "}")
	joltageStrings := strings.Split(line[joltagesStart+1:joltagesEnd], ",")
	joltages := make([]int, len(joltageStrings))
	for i, s := range joltageStrings {
		joltages[i], _ = strconv.Atoi(s)
	}
	return Machine{count: count, target: target, buttons: buttons, joltages: joltages}
}

func parseButtons(specs string) []int {
	buttonStrings := strings.Split(specs, " ")
	buttons := make([]int, len(buttonStrings))
	for i, s := range buttonStrings {
		numberStrings := strings.Split(s[1:len(s)-1], ",")
		total := 0
		for _, numString := range numberStrings {
			n, _ := strconv.Atoi(numString)
			total += int(math.Pow(2, float64(n)))
		}
		buttons[i] = total
	}
	return buttons
}

func (m Machine) tryButtons(nextButton, runningTotal, pressed int) int {
	if runningTotal == m.target {
		return pressed
	}
	if nextButton >= len(m.buttons) {
		return m.count + 1
	}
	withPress := runningTotal ^ m.buttons[nextButton]
	return min(m.tryButtons(nextButton+1, withPress, pressed+1), m.tryButtons(nextButton+1, runningTotal, pressed))
}

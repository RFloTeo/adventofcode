package main

import (
	"fmt"
	"os"
	"strings"
)

type set struct {
	m map[int]bool
}

func newSet() set {
	s := set{make(map[int]bool)}
	return s
}

func (s set) add(n int) bool {
	wasPresent := s.m[n]
	s.m[n] = true
	return !wasPresent
}

func (s set) remove(n int) bool {
	if s.m[n] {
		delete(s.m, n)
		return true
	}
	return false
}

func (s set) contains(n int) bool {
	return s.m[n]
}

func parse() []string {
	data, err := os.ReadFile("./input.txt")
	if err != nil {
		panic(err)
	}
	str := strings.TrimRight(string(data), "\n")
	lines := strings.Split(str, "\n")

	return lines
}

func main() {
	lines := parse()
	// solution starts
	fmt.Println("Part 1:")
	start := strings.Index(lines[0], "S")
	splitters := make([]set, 0)
	for _, line := range lines[1:] {
		splitterSet := newSet()
		for index, chr := range line {
			if chr == '^' {
				splitterSet.add(index)
			}
		}
		splitters = append(splitters, splitterSet)
	}

	beams := newSet()
	beams.add(start)
	edge := len(lines[0])
	splits := 0
	for _, splitterSet := range splitters {
		for beam := range beams.m {
			if splitterSet.contains(beam) {
				splits++
				beams.remove(beam)
				addBeam(&beams, beam-1, edge)
				addBeam(&beams, beam+1, edge)
			}
		}
	}
	fmt.Println(splits)

	fmt.Println("Part 2:")
	beamPaths := make(map[int]uint64)
	beamPaths[start] = 1
	for _, splitterSet := range splitters {
		newPaths := make(map[int]uint64)
		for path, timelines := range beamPaths {
			if splitterSet.contains(path) {
				addTimeline(newPaths, path+1, timelines, edge)
				addTimeline(newPaths, path-1, timelines, edge)
			} else {
				addTimeline(newPaths, path, timelines, edge)
			}
		}
		beamPaths = newPaths
	}

	var sum uint64 = 0
	for _, value := range beamPaths {
		sum += value
	}
	fmt.Println(sum)

}

// extra funcs
func addBeam(beams *set, beam, edge int) {
	if beam >= 0 && beam < edge {
		beams.add(beam)
	}
}

func addTimeline(paths map[int]uint64, path int, times uint64, edge int) {
	if path < 0 || path >= edge {
		return
	}
	existingTimes, ok := paths[path]
	if ok {
		paths[path] = existingTimes + times
	} else {
		paths[path] = times
	}
}

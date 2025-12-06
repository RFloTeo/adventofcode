package main

import (
	"fmt"
	"math"
	"os"
	"strconv"
	"strings"
)

type Range struct {
	start int
	end   int
}

func (r Range) contains(n int) bool {
	return n >= r.start && n <= r.end
}

type World struct {
	ranges      []Range
	ingredients []int
}

type Node struct {
	r         Range
	neighbors []*Node
	visited   bool
}

func (n *Node) connect(other *Node) {
	r1 := n.r
	r2 := other.r
	if r1.contains(r2.start) || r2.contains(r1.start) {
		n.neighbors = append(n.neighbors, other)
		other.neighbors = append(other.neighbors, n)
	}
}

func (n *Node) findLimits(min, max *int) {
	if n.visited {
		return
	}
	*min = minI(*min, n.r.start)
	*max = maxI(*max, n.r.end)
	n.visited = true
	for _, other := range n.neighbors {
		other.findLimits(min, max)
	}
}

func parse() World {
	data, err := os.ReadFile("./input.txt")
	if err != nil {
		panic(err)
	}
	str := strings.TrimRight(string(data), "\n")
	lines := strings.Split(str, "\n")

	// solution starts
	ranges := make([]Range, 0)
	var ingredientsStart int
	for i, line := range lines {
		if line == "" {
			ingredientsStart = i + 1
			break
		}
		parts := strings.Split(line, "-")
		start, _ := strconv.Atoi(parts[0])
		end, _ := strconv.Atoi(parts[1])
		ranges = append(ranges, Range{start, end})
	}

	ingredients := make([]int, 0)
	for _, line := range lines[ingredientsStart:] {
		id, _ := strconv.Atoi(line)
		ingredients = append(ingredients, id)
	}

	return World{ranges, ingredients}
}

func main() {
	world := parse()
	// solution starts
	fmt.Println("Part 1:")
	fresh := 0
	for _, id := range world.ingredients {
		for _, r := range world.ranges {
			if r.contains(id) {
				fresh++
				break
			}
		}
	}
	fmt.Println(fresh)
	fmt.Println("Part 2:")
	nodes := make([]*Node, 0)
	for _, r := range world.ranges {
		node := Node{r: r, neighbors: make([]*Node, 0), visited: false}
		for _, other := range nodes {
			node.connect(other)
		}
		nodes = append(nodes, &node)
	}
	total := 0
	for _, node := range nodes {
		if node.visited {
			continue
		}
		min := node.r.start
		max := node.r.end
		node.findLimits(&min, &max)
		total += max - min + 1
	}
	fmt.Println(total)
}

// extra funcs
func minI(a, b int) int {
	return int(math.Min(float64(a), float64(b)))
}

func maxI(a, b int) int {
	return int(math.Max(float64(a), float64(b)))
}

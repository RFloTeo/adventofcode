package main

import (
	utils "aoc/aocutils"
	"fmt"
	"math"
	"os"
	"slices"
	"strconv"
	"strings"
)

type World struct {
	points []Point
}

type Point struct {
	x int
	y int
	z int
}

type connection struct {
	a int
	b int
}

type connHeap struct {
	heap    *utils.MinHeap[float64]
	mapping map[float64][]connection
}

func heap() connHeap {
	return connHeap{utils.NewHeap[float64](), make(map[float64][]connection)}
}

func (c *connHeap) insert(a, b int, distance float64) {
	c.heap.Insert(distance)
	list, ok := c.mapping[distance]
	if ok {
		list = append(list, connection{a, b})
	} else {
		c.mapping[distance] = []connection{{a, b}}
	}
}

func (c *connHeap) pop() connection {
	if c.heap.IsEmpty() {
		return connection{-1, -1}
	}
	distance := c.heap.Pop()
	conns := c.mapping[distance]
	conn := conns[len(conns)-1]
	c.mapping[distance] = conns[:len(conns)-1]
	return conn
}

func parse() World {
	data, err := os.ReadFile("./input.txt")
	if err != nil {
		panic(err)
	}
	str := strings.TrimRight(string(data), "\n")
	lines := strings.Split(str, "\n")

	// solution starts
	points := make([]Point, len(lines))
	for i, line := range lines {
		coords := strings.Split(line, ",")
		points[i] = Point{getInt(coords, 0), getInt(coords, 1), getInt(coords, 2)}
	}

	return World{points}
}

func main() {
	points := parse().points
	// solution starts
	fmt.Println("Part 1:")

	// Make a heap of distances between points
	size := len(points)
	heap := heap()
	for i1, p1 := range points[:size-1] {
		for i2, p2 := range points[i1+1:] {
			dist := distance(p1, p2)
			heap.insert(i1, i1+i2+1, dist)
		}
	}

	// Make first 1000 connections
	graph := utils.NewGraph(size)
	for i := range size {
		node := graph.GetNode(i)
		node.Val = i
	}
	for range 1000 {
		conn := heap.pop()
		node1 := graph.GetNode(conn.a)
		node2 := graph.GetNode(conn.b)
		connectValues(node1, node2)
		graph.Connect(conn.a, conn.b)
	}

	// calculate island sizes
	islandSizes := make(map[int]int)
	for i := range size {
		val := graph.GetNode(i).Val
		_, ok := islandSizes[val]
		if ok {
			islandSizes[val]++
		} else {
			islandSizes[val] = 1
		}
	}

	// get top 3 island sizes
	top3 := []int{0, 0, 0}
	for _, islandSize := range islandSizes {
		top3 = append(top3, islandSize)
		slices.Sort(top3)
		top3 = top3[1:]
	}
	fmt.Println(top3[0] * top3[1] * top3[2])

	fmt.Println("Part 2:")
	islands := len(islandSizes)
	var point1, point2 int
	for islands > 1 && !heap.heap.IsEmpty() {
		conn := heap.pop()
		point1 = conn.a
		point2 = conn.b
		node1 := graph.GetNode(conn.a)
		node2 := graph.GetNode(conn.b)
		graph.Connect(point1, point2)
		islands -= connectValues2(node1, node2)
	}
	fmt.Println(points[point1].x * points[point2].x)

}

// extra funcs
func getInt(a []string, i int) int {
	val, _ := strconv.Atoi(a[i])
	return val
}

func distance(a, b Point) float64 {
	xsq := math.Pow(float64(a.x-b.x), 2)
	ysq := math.Pow(float64(a.y-b.y), 2)
	zsq := math.Pow(float64(a.z-b.z), 2)
	return math.Sqrt(xsq + ysq + zsq)
}

func connectValues(a, b *utils.Node) {
	if a.Val == b.Val {
		return
	}
	visited := make(map[*utils.Node]bool)
	if a.Val < b.Val {
		propagateValue(b, a.Val, visited)
	} else {
		propagateValue(a, b.Val, visited)
	}
}

// returns 1 if it merges 2 islands and 0 otherwise
func connectValues2(a, b *utils.Node) int {
	if a.Val == b.Val {
		return 0
	}
	connectValues(a, b)
	return 1
}

func propagateValue(node *utils.Node, value int, visited map[*utils.Node]bool) {
	if visited[node] {
		return
	}
	node.Val = value
	visited[node] = true
	for _, neighbor := range node.Neighbors {
		propagateValue(neighbor, value, visited)
	}
}

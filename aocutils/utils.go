package aocutils

import (
	"cmp"
	"fmt"
)

// Hash set implementation
type Set[T comparable] struct {
	m map[T]bool
}

func IntSet() Set[int] {
	s := Set[int]{make(map[int]bool)}
	return s
}

func (s Set[T]) Add(n T) bool {
	wasPresent := s.m[n]
	s.m[n] = true
	return !wasPresent
}

func (s Set[T]) Remove(n T) bool {
	if s.m[n] {
		delete(s.m, n)
		return true
	}
	return false
}

func (s Set[T]) Contains(n T) bool {
	return s.m[n]
}

// Graph node implementation that can store a value
type Node struct {
	Val       int
	Neighbors []*Node
}

type Graph struct {
	NodeCount int
	nodes     map[int]*Node
}

// Initializes and returns an instance of Graph
// Optionally, pass a number to initialize with that many nodes
// Parameters past the first will be ignored
func NewGraph(count ...int) *Graph {
	g := Graph{0, make(map[int]*Node)}
	if len(count) > 0 {
		g.AddNodes(count[0])
	}
	return &g
}

// Adds a node and returns its index and the node pointer
func (g *Graph) AddNode() (int, *Node) {
	index := g.NodeCount
	newNode := Node{0, make([]*Node, 0)}
	g.NodeCount++
	g.nodes[index] = &newNode
	return index, &newNode
}

// Adds multiple nodes as specified
func (g *Graph) AddNodes(count int) {
	if count <= 0 {
		return
	}
	for range count {
		g.AddNode()
	}
}

// Connects 2 nodes by index
func (g *Graph) Connect(a, b int) {
	if a >= g.NodeCount || b >= g.NodeCount {
		return
	}
	nodeA := g.nodes[a]
	nodeB := g.nodes[b]
	nodeA.Neighbors = append(nodeA.Neighbors, nodeB)
	nodeB.Neighbors = append(nodeB.Neighbors, nodeA)
}

// Get a node by index
func (g *Graph) GetNode(index int) *Node {
	if index >= g.NodeCount {
		fmt.Println("index too big: ", index, g.NodeCount)
		return nil
	}
	if g.nodes[index] == nil {
		fmt.Println("index has nil node:", index)
	}
	return g.nodes[index]
}

//TODO add methods

// Weighted graph implementation
type Number interface {
	int | float64
}
type WeightedGraph[T Number] struct {
	NodeCount int
	matrix    [][]T
}

// If an argument is passed, initialize with that many nodes
func IntWeightedGraph(count ...int) WeightedGraph[int] {
	size := 0
	if len(count) > 0 {
		size = count[0]
	}
	g := WeightedGraph[int]{0, make([][]int, size)}
	for row := range size {
		g.matrix[row] = make([]int, size)
		for col := range size {
			if row == col {
				g.matrix[row][col] = 0
			} else {
				g.matrix[row][col] = -1
			}
		}
	}
	return g
}

// If an argument is passed, initialize with that many nodes
func FloatWeightedGraph(count ...int) WeightedGraph[float64] {
	size := 0
	if len(count) > 0 {
		size = count[0]
	}
	g := WeightedGraph[float64]{0, make([][]float64, size)}
	for row := range size {
		g.matrix[row] = make([]float64, size)
		for col := range size {
			if row == col {
				g.matrix[row][col] = 0
			} else {
				g.matrix[row][col] = -1
			}
		}
	}
	return g
}

// Adds a node to the graph and returns its index
func (g WeightedGraph[T]) AddNode() int {
	index := g.NodeCount
	g.NodeCount++
	for row := range len(g.matrix) {
		g.matrix[row] = append(g.matrix[row], -1)
	}

	newRow := make([]T, g.NodeCount)
	for i := range newRow {
		if i != index {
			newRow[i] = -1
		}
	}
	g.matrix = append(g.matrix, newRow)

	return index
}

// Sets the weight of the connection between nodes a and b to the specified value
// Passing non-existent node indices will do nothing
func (g WeightedGraph[T]) Connect(a, b int, weight T) {
	if a >= g.NodeCount || b >= g.NodeCount {
		return
	}
	g.matrix[a][b] = weight
	g.matrix[b][a] = weight
}

// Returns the weight of the connection between nodes a and b
// Passing non-existent node indices or unconnected nodes returns -1
func (g WeightedGraph[T]) GetWeight(a, b int) T {
	if a >= g.NodeCount || b >= g.NodeCount {
		return -1
	}
	return g.matrix[a][b]
}

// MinHeap implementation
type MinHeap[T cmp.Ordered] struct {
	arr []T
}

func NewHeap[T cmp.Ordered]() *MinHeap[T] {
	return &MinHeap[T]{make([]T, 0)}
}

func MinHeapify[T cmp.Ordered](source []T) MinHeap[T] {
	return MinHeap[T]{make([]T, 0)} // TODO
}

func (h *MinHeap[T]) Insert(item T) {
	index := len(h.arr)
	h.arr = append(h.arr, item)
	for parent := getHeapParent(index); index > 0 && h.arr[index] < h.arr[parent]; index, parent = parent, getHeapParent(parent) {
		Swap(h.arr, index, parent)
	}
}

func (h *MinHeap[T]) Pop() T {
	if h.IsEmpty() {
		return T(0)
	}
	result := h.arr[0]
	lastIndex := len(h.arr) - 1
	h.arr[0] = h.arr[lastIndex]
	h.arr = h.arr[:lastIndex]

	index := 0
	for next := smallerChild(h.arr, index); next >= 0 && h.arr[index] > h.arr[next]; index, next = next, smallerChild(h.arr, next) {
		Swap(h.arr, index, next)
	}

	return result
}

func (h *MinHeap[T]) Peek() T {
	if h.IsEmpty() {
		return T(0)
	}
	return h.arr[0]
}

func (h *MinHeap[T]) IsEmpty() bool {
	return len(h.arr) == 0
}

func getHeapParent(index int) int {
	return (index - 1) / 2
}

func getHeapChild[T any](arr []T, index int, child int) int {
	childIndex := index*2 + child
	if childIndex >= len(arr) {
		return -1
	}
	return childIndex
}

func smallerChild[T cmp.Ordered](arr []T, index int) int {
	left := getHeapChild(arr, index, 1)
	right := getHeapChild(arr, index, 2)
	if left == -1 {
		return -1
	}
	if right == -1 {
		return left
	}
	if arr[left] < arr[right] {
		return left
	}
	return right
}

func Swap[T any](slice []T, a, b int) {
	if a >= len(slice) || b >= len(slice) {
		return
	}
	aux := slice[a]
	slice[a] = slice[b]
	slice[b] = aux
}

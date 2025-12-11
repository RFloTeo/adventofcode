package main

import (
	"fmt"
	"os"
	"strings"
)

type Node struct {
	key       string
	val       int
	neighbors []*Node
}

type DirectedGraph struct {
	nodes map[string]*Node
}

var cnt int = 0

func (g *DirectedGraph) addNode(key string) {
	node := &Node{key, 0, make([]*Node, 0)}
	g.nodes[key] = node
}

func (g *DirectedGraph) getNode(key string) *Node {
	return g.nodes[key]
}

func (g *DirectedGraph) addConnection(from, to string) {
	toNode := g.getNode(to)
	fromNode := g.getNode(from)
	fromNode.neighbors = append(fromNode.neighbors, toNode)
}

func (g *DirectedGraph) reset() {
	for _, node := range g.nodes {
		node.val = 0
	}
}

type World struct {
	graph DirectedGraph
}

func parse() World {
	data, err := os.ReadFile("./input.txt")
	if err != nil {
		panic(err)
	}
	str := strings.TrimRight(string(data), "\n")
	lines := strings.Split(str, "\n")

	// solution starts
	g := DirectedGraph{make(map[string]*Node)}
	g.addNode("out")
	for _, line := range lines {
		g.addNode(line[:3])
	}

	for _, line := range lines {
		from := line[:3]
		neighbors := strings.Split(line[5:], " ")
		for _, neighbor := range neighbors {
			g.addConnection(from, neighbor)
		}
	}

	return World{g}
}

func main() {
	g := parse().graph
	// solution starts
	fmt.Println("Part 1:")
	addPath(g.getNode("you"))
	fmt.Println(g.getNode("out").val)

	fmt.Println("Part 2:")
	g.reset()
	sortedNodes := topologicalSort(g)
	server := g.getNode("svr")
	dac := g.getNode("dac")
	fft := g.getNode("fft")
	out := g.getNode("out")

	server.val = 1
	calculatePaths(sortedNodes)
	sToD := dac.val
	sToF := fft.val

	g.reset()
	dac.val = 1
	calculatePaths(sortedNodes)
	dToF := fft.val
	dToO := out.val

	g.reset()
	fft.val = 1
	calculatePaths(sortedNodes)
	fToD := dac.val
	fToO := out.val

	if dToF == 0 {
		fmt.Println("nothing from dac to fft")
		fmt.Println(sToF * fToD * dToO)
	}
	if fToD == 0 {
		fmt.Println("nothing from fft to dac")
		fmt.Println(sToD * dToF * fToO)
	}

}

// extra funcs
func hasCycle(n *Node, visited map[string]bool, recStack map[string]bool) bool {
	k := n.key
	if recStack[k] {
		return true
	}
	if n.key == "out" || visited[k] {
		return false
	}

	visited[k] = true
	recStack[k] = true

	for _, neighbor := range n.neighbors {
		if hasCycle(neighbor, visited, recStack) {
			return true
		}
	}

	recStack[k] = false
	return false
}

func addPath(n *Node) {
	n.val += 1
	if n.key == "out" {
		return
	}
	for _, neighbor := range n.neighbors {
		addPath(neighbor)
	}
}

func calculatePaths(sortedNodes []*Node) {
	for _, node := range sortedNodes {
		for _, neighbor := range node.neighbors {
			neighbor.val += node.val
		}
	}

}

func topologicalSort(g DirectedGraph) []*Node {
	indegs := map[string]int{}
	output := make([]*Node, len(g.nodes))

	// calculate indegrees
	for key := range g.nodes {
		indegs[key] = 0
	}
	for _, node := range g.nodes {
		for _, neighbor := range node.neighbors {
			indegs[neighbor.key]++
		}
	}

	// enqueue entry nodes
	queue := []*Node{}
	for key, node := range g.nodes {
		if indegs[key] == 0 {
			queue = append(queue, node)
		}
	}

	// process nodes
	for i := 0; i < len(queue); i++ {
		node := queue[i]
		output[i] = queue[i]
		for _, neighbor := range node.neighbors {
			indegs[neighbor.key]--
			if indegs[neighbor.key] == 0 {
				queue = append(queue, neighbor)
			}
		}
	}

	return output
}

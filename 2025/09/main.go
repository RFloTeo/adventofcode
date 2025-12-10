package main

import (
	"fmt"
	"math"
	"os"
	"slices"
	"strconv"
	"strings"
)

type point struct {
	x int
	y int
}

// vertical edges
type edge struct {
	x    int
	ymin int
	ymax int
}

type hEdge struct {
	xmin int
	xmax int
	y    int
}

type World struct {
	points          []point
	verticalEdges   []edge
	horizontalEdges []hEdge
}

func parse() World {
	data, err := os.ReadFile("./input.txt")
	if err != nil {
		panic(err)
	}
	str := strings.TrimRight(string(data), "\n")
	lines := strings.Split(str, "\n")

	// solution starts
	points := make([]point, len(lines))
	for i, line := range lines {
		coords := strings.Split(line, ",")
		x, _ := strconv.Atoi(coords[0])
		y, _ := strconv.Atoi(coords[1])
		points[i] = point{x, y}
	}
	edges := make([]edge, len(lines)/2)
	// input has vertical edges between pts. 0-1, 2-3 etc.
	for i := range edges {
		p1 := points[2*i]
		p2 := points[2*i+1]
		ymin := min(p1.y, p2.y)
		ymax := max(p1.y, p2.y)
		edges[i] = edge{p1.x, ymin, ymax}
	}
	hEdges := make([]hEdge, len(lines)/2-1)
	for i := range hEdges {
		p1 := points[2*i+1]
		p2 := points[2*i+2]
		xmin := min(p1.x, p2.x)
		xmax := max(p1.x, p2.x)
		hEdges[i] = hEdge{xmin, xmax, p1.y}
	}
	l := len(points) - 1
	hEdges = append(hEdges, hEdge{min(points[0].x, points[l].x), max(points[0].x, points[l].x), points[0].y})

	return World{points, edges, hEdges}
}

func main() {
	world := parse()
	points := world.points
	// solution starts
	fmt.Println("Part 1:")
	maxArea := 0
	for _, p := range points {
		for _, q := range points {
			maxArea = max(int((math.Abs(float64(p.x-q.x))+1)*(math.Abs(float64(p.y-q.y))+1)), maxArea)
		}
	}
	fmt.Println(maxArea)

	fmt.Println("Part 2:")
	orderedPoints := make([]point, len(points))
	copy(orderedPoints, points)
	slices.SortFunc(orderedPoints, pointCmp)

	maxArea = 0
	for i := range orderedPoints {
		for j := range orderedPoints {
			if i >= j || pointsBetween(orderedPoints, i, j) {
				continue
			}
			pi := orderedPoints[i]
			pj := orderedPoints[j]
			corner1 := point{pi.x, pj.y}
			corner2 := point{pj.x, pi.y}
			signx := sign(pj.x - pi.x)
			signy := sign(pj.y - pi.y)
			diagi := point{pi.x + signx, pi.y + signy}
			diagj := point{pj.x - signx, pj.y - signy}
			if world.edgesIntersect(pi, pj) || world.isOutside(corner1) || world.isOutside(corner2) || world.isOutside(diagi) || world.isOutside(diagj) {
				continue
			}
			area := (pj.x - pi.x + 1) * int(math.Abs(float64(pj.y-pi.y))+1)
			maxArea = max(area, maxArea)
		}
	}
	fmt.Println(maxArea)
}

// extra funcs
func pointCmp(a, b point) int {
	if a.x == b.x {
		return a.y - b.y
	}
	return a.x - b.x
}

func pointsBetween(points []point, start, end int) bool {
	pStart := points[start]
	pEnd := points[end]
	ymax := max(pStart.y, pEnd.y)
	ymin := min(pStart.y, pEnd.y)
	xmax := max(pStart.x, pEnd.x)
	xmin := max(pStart.x, pEnd.x)
	for i := start + 1; i < end; i++ {
		p := points[i]
		if p.x > xmin && p.x < xmax && p.y > ymin && p.y < ymax {
			return true
		}
	}
	return false
}

func (w World) isOutside(p point) bool {
	left := 0
	right := 0
	for _, edge := range w.verticalEdges {
		if p.x == edge.x && p.y >= edge.ymin && p.y <= edge.ymax {
			return false
		}
		if p.y < edge.ymin || p.y > edge.ymax {
			continue
		}
		if p.x > edge.x {
			left++
		} else if p.x < edge.x {
			right++
		}
	}

	for _, hEdge := range w.horizontalEdges {
		if p.y != hEdge.y {
			continue
		}
		if p.x >= hEdge.xmin && p.x <= hEdge.xmax {
			return false
		}
		if p.x > hEdge.xmax {
			left++
		} else if p.x < hEdge.xmin {
			right++
		}
	}
	return (left%2 == 0) && (right%2 == 0)
}

func (w World) edgesIntersect(p1, p2 point) bool {
	for _, e := range w.horizontalEdges {
		if hInRect(p1, p2, e) {
			return true
		}
	}
	for _, e := range w.verticalEdges {
		if vInRect(p1, p2, e) {
			return true
		}
	}
	return false
}

func sign(n int) int {
	if n < 0 {
		return -1
	}
	if n > 0 {
		return 1
	}
	return 0
}

func hInRect(p1, p2 point, e hEdge) bool {
	xmin := min(p1.x, p2.x)
	ymin := min(p1.y, p2.y)
	xmax := max(p1.x, p2.x)
	ymax := max(p1.y, p2.y)
	if e.y <= ymin || e.y >= ymax {
		return false
	}
	return (e.xmin > xmin && e.xmin < xmax) || (xmin > e.xmin && xmin < e.xmax)
}

func vInRect(p1, p2 point, e edge) bool {
	xmin := min(p1.x, p2.x)
	ymin := min(p1.y, p2.y)
	xmax := max(p1.x, p2.x)
	ymax := max(p1.y, p2.y)
	if e.x <= xmin || e.x >= xmax {
		return false
	}
	return (e.ymin > ymin && e.ymin < ymax) || (ymin > e.ymin && ymin < e.ymax)
}

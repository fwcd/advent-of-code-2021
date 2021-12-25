package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

type MONADParams struct {
	q int
	a int
	b int
}

func check(err error) {
	if err != nil {
		panic(err)
	}
}

func step(w int, z int, params MONADParams) int {
	if z%26+params.a == w {
		return z / params.q
	} else {
		return (z/params.q)*26 + w + params.b
	}
}

func compute(ws []int, params []MONADParams) int {
	z := 0
	for i := range ws {
		z = step(ws[i], z, params[i])
	}
	return z
}

func parseOperand(line string) int {
	xs := strings.Split(line, " ")
	x, err := strconv.Atoi(xs[len(xs)-1])
	check(err)
	return x
}

func main() {
	raw, err := os.ReadFile("resources/input.txt")
	check(err)

	lines := strings.Split(string(raw), "\n")
	count := len(lines) / 18
	params := make([]MONADParams, 0)

	for i := 0; i < count*18; i += 18 {
		q := parseOperand(lines[i+4])
		a := parseOperand(lines[i+5])
		b := parseOperand(lines[i+15])
		params = append(params, MONADParams{q, a, b})
	}

	fmt.Printf("%d\n", compute([]int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13}, params))
}

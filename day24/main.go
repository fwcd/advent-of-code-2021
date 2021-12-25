package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

func check(err error) {
	if err != nil {
		panic(err)
	}
}

func step(w int, z int, q int, a int, b int) int {
	var x int

	if ((z%26)/q + a) != w {
		x = 1
	} else {
		x = 0
	}

	return z*(25*x+1) + (w+b)*x
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
	for i := 0; i < (len(lines)/18)*18; i += 18 {
		q := parseOperand(lines[i+4])
		a := parseOperand(lines[i+5])
		b := parseOperand(lines[i+15])
		fmt.Printf("q = %d, a = %d, b = %d\n", q, a, b)
	}
}

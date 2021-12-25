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
	// 'Peek' last base-26-digit
	peek := z % 26
	// If q == 26 'pop' last base-26-digit, otherwise do nothing
	z /= params.q
	// If last base-26-digit + some offset a matches do nothing, otherwise 'push' new stuff
	if peek+params.a == w {
		return z
	} else {
		return z*26 + w + params.b
	}

	// => If we pop, we need to ensure that the popped value + offset a == input w
	//    otherwise we land in the second branch, which pushes again (and we don't
	//    want that since we otherwise won't arrive at z = 0 with the limited number
	//    of pops we have)
	// => This lets us form constraints on the total input based on our q, a, b values
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
		fmt.Printf("q = %d, a = %d, b = %d\n", q, a, b)
	}

	// With our q, a, b values and the program's interpretation given above,
	// we get:
	//
	// Push. ws[0] + 15
	// Push. ws[1] + 12
	// Push. ws[2] + 15
	// Pop.  ws[3] == ws[2] + 15 - 9
	// Pop.  ws[4] == ws[1] + 12 - 7
	// Push. ws[5] + 2
	// Pop.  ws[6] == ws[5] + 2 - 1
	// Pop.  ws[7] == ws[0] + 15 - 16
	// Push. ws[8] + 10
	// Pop.  ws[9] == ws[8] + 10 - 15
	// Push. ws[10]
	// Push. ws[11]
	// Pop.  ws[12] == ws[11] - 4
	// Pop.  ws[13] == ws[10]
	//
	// Taking only the equations:
	//
	// ws[3] == ws[2] + 6
	// ws[4] == ws[1] + 5
	// ws[6] == ws[5] + 1
	// ws[7] == ws[0] - 1
	// ws[9] == ws[8] - 5
	// ws[12] == ws[11] - 4
	// ws[13] == ws[10]
	//
	// Now we simply take 99999999999999 and apply the constraints by hand, taking
	// care that we adjust numbers to be between 1 and 9 (inclusively):
	//
	// ws[0] arbitrary   => set ws[0] = 9
	// ws[1] arbitrary   => set ws[1] = 9
	// ws[2] arbitrary   => set ws[2] = 9
	// ws[3] constrained => set ws[3] = 9 and adjust ws[2] by -6
	// ...
	//
	// 94399898949959

	fmt.Printf("Part 1 Check: %d\n", compute([]int{9, 4, 3, 9, 9, 8, 9, 8, 9, 4, 9, 9, 5, 9}, params))

	// Same principle for part 2, just starting with 11111111111111:
	//
	// 21176121611511

	fmt.Printf("Part 2 Check: %d\n", compute([]int{2, 1, 1, 7, 6, 1, 2, 1, 6, 1, 1, 5, 1, 1}, params))
}

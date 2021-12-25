package main

import "fmt"

func step(w int, z int, q int, a int, b int) int {
	var x int

	if ((z%26)/q + a) != w {
		x = 1
	} else {
		x = 0
	}

	return z*(25*x+1) + (w+b)*x
}

func main() {
	fmt.Println("Hello world")
}

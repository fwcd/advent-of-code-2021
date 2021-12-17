package dev.fwcd.adventofcode2021

data class Vec2(val x: Int, val y: Int)

fun main() {
    val input = object {}.javaClass.getResource("/demo.txt").readText()
    val pattern = """target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)""".toRegex()
    val (x1, x2, y1, y2) = pattern.find(input)!!.groupValues.drop(1).map { it.toInt() }
    println("Got $x1, $x2, $y1, $y2")
}

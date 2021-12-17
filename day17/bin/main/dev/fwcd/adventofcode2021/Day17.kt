package dev.fwcd.adventofcode2021

import kotlin.math.sign

data class Vec(val x: Int, val y: Int) {
    operator fun plus(rhs: Vec) = Vec(x + rhs.x, y + rhs.y)
}

data class Rect(val min: Vec, val max: Vec) {
    operator fun contains(pos: Vec): Boolean =
           pos.x >= min.x && pos.x <= max.x
        && pos.y >= min.y && pos.y <= max.y
}

data class SimulationResults(val hitTarget: Boolean, val maxY: Int)

fun simulate(startVelocity: Vec, target: Rect): SimulationResults {
    var pos = Vec(0, 0)
    var velocity = startVelocity
    var maxY = 0

    while (pos.x <= target.max.x && pos.y >= target.min.y) {
        if (pos in target) {
            return SimulationResults(hitTarget = true, maxY)
        }
        pos += velocity
        maxY = Math.max(pos.y, maxY)
        velocity = velocity.copy(x = velocity.x - velocity.x.sign, y = velocity.y - 1)
    }

    return SimulationResults(hitTarget = false, maxY)
}

fun main() {
    val input = object {}.javaClass.getResource("/input.txt").readText()
    val pattern = """target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)""".toRegex()
    val (x1, x2, y1, y2) = pattern.find(input)!!.groupValues.drop(1).map { it.toInt() }
    val target = Rect(Vec(x1, y1), Vec(x2, y2))
    val radius = Math.max(
        Math.max(Math.abs(x1), Math.abs(x2)),
        Math.max(Math.abs(y1), Math.abs(y2))
    )
    var maxY = 0

    for (dy in -radius..radius) {
        for (dx in -radius..radius) {
            val results = simulate(Vec(dx, dy), target)
            if (results.hitTarget) {
                maxY = Math.max(maxY, results.maxY)
            }
        }
    }

    println("Part 1: $maxY")
}

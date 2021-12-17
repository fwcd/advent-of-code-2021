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
    val input = object {}.javaClass.getResource("/demo.txt").readText()
    val pattern = """target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)""".toRegex()
    val (x1, x2, y1, y2) = pattern.find(input)!!.groupValues.drop(1).map { it.toInt() }
    val target = Rect(Vec(x1, y1), Vec(x2, y2))
    
    println("In: ${simulate(Vec(6, 9), target)}")
}

package day3

import locations.Directory.currentDir
import inputs.Input.loadFileSync
import scala.util.boundary, boundary.break
import scala.annotation.tailrec

@main def part1: Unit = 
    println(s"Answer to part 1: ${part1(loadInput())}")

@main def part2: Unit = 
    println(s"Answer to part 2: ${part2(loadInput())}")

def loadInput(): String = loadFileSync(s"$currentDir/input.txt")

def toDigit(c: Char) = c.toInt - '0'.toInt

def part1(input: String) = {
    def solve(num: String) = {
        toDigit(num.init.max) * 10 + toDigit(num.slice(num.indexOf(num.init.max) + 1, num.length()).max)
    }
    input.linesIterator.map(solve).sum.toString
}

def part2(input: String) = {
    @tailrec
    def rec(num: String, total: Long, digits: Int): Long = {
        if digits == 0 then
            total
        else 
            rec(num.slice(num.indexOf(num.slice(0, num.length() - digits + 1).max) + 1, num.length()), total * 10L + toDigit(num.slice(0, num.length() - digits + 1).max), digits - 1)
    }
    def solve(num: String) = {
        rec(num, 0L, 12)
    }
    input.linesIterator.map(solve).sum.toString
}
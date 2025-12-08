package day5

import locations.Directory.currentDir
import inputs.Input.loadFileSync

@main def part1: Unit = 
    println(s"Answer to part 1: ${part1(loadInput())}")

@main def part2: Unit = 
    println(s"Answer to part 2: ${part2(loadInput())}")

def loadInput(): String = loadFileSync(s"$currentDir/input.txt")

def parseInput(input: String) =
    def parseRanges(s: String) = s match {
        case s"${a}-${b}" => (a.toLong, b.toLong) 
    }
    val Array(ranges, ids) = input.split("\r?\n\r?\n", 2)
    (ranges.linesIterator.map(parseRanges).toVector, ids.linesIterator.map(_.toLong).toVector)

def part1(input: String) =
    val (ranges, ids) = parseInput(input)
    ids.count(id => ranges.exists((l, r) => l <= id && id <= r)).toString()

def part2(input: String) =
    parseInput(input)._1.sorted.foldLeft(0L, 0L) { case ((ans, curLeft), (l, r)) =>
        if r < curLeft then (ans, curLeft)
        else if l > curLeft then (ans + r - l + 1, r + 1)
        else (ans + r - curLeft + 1, r + 1)
    }._1.toString()
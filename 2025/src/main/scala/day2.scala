package day2

import locations.Directory.currentDir
import inputs.Input.loadFileSync
import scala.util.boundary, boundary.break

@main def part1: Unit = 
    println(s"Answer to part 1: ${part1(loadInput())}")

@main def part2: Unit = 
    println(s"Answer to part 2: ${part2(loadInput())}")

def loadInput(): String = loadFileSync(s"$currentDir/input.txt")

def repeatTimes(x: Long, times: Int): Long = {
    return (x.toString() * times).toLong
}

def solve(ranges: Seq[(Long, Long)], repeat: Int): Long = {
    var i = 1L
    val mx = ranges.map(_._2).max
    val mxlen = mx.toString().length()
    var ansSet = Set[Long]()
    while repeatTimes(i, 2) <= mx do {
        boundary{
            for (rep <- 2 to repeat) {
                if i.toString().length() * rep > mxlen then break()
                val num = repeatTimes(i, rep)
                if num > mx then break()
                if ranges.exists { case (a, b) => a <= num && num <= b } then ansSet += num
            }
        }
        i = i + 1
    }
    return ansSet.sum
}

def part1(input: String): String = {
    var ranges = Seq[(Long, Long)]()

    def parseRanges(s: String): (Long, Long) = s match {
        case s"${a}-${b}" => (a.toLong, b.toLong) 
    }

    ranges = input.split(",").toSeq.map(parseRanges)
    return solve(ranges, 2).toString
}

def part2(input: String): String = {
    var ranges = Seq[(Long, Long)]()

    def parseRanges(s: String): (Long, Long) = s match {
        case s"${a}-${b}" => (a.toLong, b.toLong) 
    }

    ranges = input.split(",").toSeq.map(parseRanges)
    return solve(ranges, ranges.map(_._2).max.toString().length()).toString
}
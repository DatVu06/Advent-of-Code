package day12

import locations.Directory.currentDir
import inputs.Input.loadFileSync
import scala.collection.mutable

@main def part1: Unit = 
    println(s"Answer to part 1: ${part1(loadInput())}")

@main def part2: Unit = 
    println(s"Answer to part 2: ${part2(loadInput())}")

def loadInput(): String = loadFileSync(s"$currentDir/input.txt")

def part1(input: String) =
    def solve(n: Int, m: Int, rest: String, blockSizes: Array[Int]): Int =
        val cnts = rest.trim.split(" ").map(_.trim.toInt)
        if (n / 3) * (m / 3) >= cnts.sum then return 1
        if ((for i <- 0 until cnts.length yield cnts(i) * blockSizes(i)).sum > n * m)  then return 0
        println("LOL")
        1
        
    input.linesIterator.foldLeft(Array[Int](), 0, 0) { case ((blockSizes, answer, cnt), line) =>
        line.trim match
            case s"${x}:" => (blockSizes, answer, 0)
            case "" => (blockSizes :+ cnt, answer, 0)
            case s"${x}x${y}: $rest" => (blockSizes, answer + solve(x.toInt, y.toInt, rest, blockSizes), cnt)
            case s => (blockSizes, answer, cnt + s.count(_ == '#'))
    }._2.toString()

def part2(input: String) =
    part1(input)
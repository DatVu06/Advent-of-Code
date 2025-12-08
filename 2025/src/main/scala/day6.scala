package day6

import locations.Directory.currentDir
import inputs.Input.loadFileSync

@main def part1: Unit = 
    println(s"Answer to part 1: ${part1(loadInput())}")

@main def part2: Unit = 
    println(s"Answer to part 2: ${part2(loadInput())}")

def loadInput(): String = loadFileSync(s"$currentDir/input.txt")

def parseInput(input: String) =
    val lines = input.linesIterator.toVector
    (lines.init.map(line => line.split("\\s+").filter(_.nonEmpty).map(_.toLong)),
     lines.last.split("\\s+").filter(_.nonEmpty).toVector)

def part1(input: String) =
    val (numbers, ops) = parseInput(input)
    (0 until numbers(0).length).map(i =>
        ops(i) match {
            case "+" => numbers.map(_(i)).sum
            case "*" => numbers.map(_(i)).product
        }
    ).sum.toString()

def parseVerticalInput(input: String) =
    val lines = input.linesIterator.toSeq
    val mx = lines.map(_.length()).max
    lines.map(_.padTo(mx, ' '))
        .transpose
        .map(_.mkString)
        .foldLeft(Seq[Seq[String]](), Seq[String]()) { case ((acc, cur), line) =>
            if line.trim().isEmpty then (acc :+ cur, Seq())
            else (acc, cur :+ line)
        }.match { case (blocks, last) => if last.isEmpty then blocks else blocks :+ last }

def part2(input: String) =
    def solveBlock(block: Seq[String]) =
        val isAdd = block.exists(line => line.contains('+'))
        block.foldLeft(if isAdd then 0L else 1L) { (acc, line) =>
            val curNum = line.filterNot(c => c.isWhitespace || c == '+' || c == '*').toLong
            if isAdd then acc + curNum else acc * curNum
        }

    parseVerticalInput(input)
        .map(block => solveBlock(block))
        .sum
        .toString()
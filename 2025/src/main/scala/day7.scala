package day7

import locations.Directory.currentDir
import inputs.Input.loadFileSync

@main def part1: Unit = 
    println(s"Answer to part 1: ${part1(loadInput())}")

@main def part2: Unit = 
    println(s"Answer to part 2: ${part2(loadInput())}")

def loadInput(): String = loadFileSync(s"$currentDir/input.txt")

def part1(input: String) =
    input.linesIterator.drop(1).foldLeft(Set[Int](input.indexOf('S')), 0) { case ((visited, split), line) =>
        visited.foldLeft(Set[Int](), split) { case ((newVisited, ans), pos) =>
            line(pos) match {
                case '.' => (newVisited + pos, ans)
                case '^' => (newVisited + (pos - 1) + (pos + 1), ans + 1)
            }
        }
    }._2.toString()

def part2(input: String) =
    val width = input.linesIterator.toList(0).trim.length
    input.linesIterator.drop(1).foldRight(Array.fill(width)(1L)) { case (line, curState) =>
        val nxtState = Array.fill(width)(0L)
        (0 until width).foreach(i =>
            line(i) match {
                case '.' => nxtState(i) += curState(i)
                case '^' => 
                    if i > 0 then nxtState(i) += curState(i - 1)
                    if i < width - 1 then nxtState(i) += curState(i + 1)
            }
        )
        nxtState
    }(input.indexOf('S')).toString()
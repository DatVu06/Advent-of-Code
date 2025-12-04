package day4

import locations.Directory.currentDir
import inputs.Input.loadFileSync
import scala.annotation.tailrec

@main def part1: Unit = 
    println(s"Answer to part 1: ${part1(loadInput())}")

@main def part2: Unit = 
    println(s"Answer to part 2: ${part2(loadInput())}")

def loadInput(): String = loadFileSync(s"$currentDir/input.txt")

val directions = Seq((0,1), (1,1), (1,0), (1,-1), (0,-1), (-1,-1), (-1,0), (-1,1))

def countAccessible(grid: Vector[Vector[Char]]) =
   (for 
        (row, x) <- grid.zipWithIndex
        (cell, y) <- row.zipWithIndex
        if cell == '@'
        adj = directions.map((dx, dy) => (x + dx, y + dy))
        if adj.count((dx, dy) =>
            dx >= 0 && dx < grid.length && dy >= 0 && dy < grid(0).length && grid(dx)(dy) == '@'
        ) < 4
    yield (x, y))

def part1(input: String) =
    countAccessible(input.linesIterator.map(_.toVector).toVector).length.toString()

def countRemovable(grid: Vector[Vector[Char]]): Int =
    val accessible = countAccessible(grid)
    if accessible.isEmpty then 0
    else
        accessible.length + countRemovable(accessible.foldLeft(grid) { case (g, (x, y)) =>
            g.updated(x, g(x).updated(y, '.'))
        })

def part2(input: String) =
    countRemovable(input.linesIterator.map(_.toVector).toVector).toString()
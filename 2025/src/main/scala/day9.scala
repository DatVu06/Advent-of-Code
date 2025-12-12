package day9

import locations.Directory.currentDir
import inputs.Input.loadFileSync
import scala.collection.mutable
import scala.compiletime.ops.double

@main def part1: Unit = 
    println(s"Answer to part 1: ${part1(loadInput())}")

@main def part2: Unit = 
    println(s"Answer to part 2: ${part2(loadInput())}")

def loadInput(): String = loadFileSync(s"$currentDir/input.txt")

def parsePoints(input: String) =
    input.linesIterator.map(line => line match {
        case s"${a},${b}" => (a.toInt, b.toInt)
    }).toArray

def area(p1: (Int, Int), p2: (Int, Int)) =
    1L * (Math.abs(p1._1 - p2._1) + 1) * (Math.abs(p1._2 - p2._2) + 1)

def part1(input: String) =
    val points = parsePoints(input)
    (for
        i <- points.indices
        j <- i + 1 until points.length
    yield area(points(i), points(j)))
    .max.toString()


def part2(input: String) =
    val points = parsePoints(input)
    val coordinateX = (points.map(_._1) ++ points.map(_._1 - 1) :+ points.map(_._1).max + 1).distinct.sorted
    val coordinateY = (points.map(_._2) ++ points.map(_._2 - 1) :+ points.map(_._2).max + 1).distinct.sorted
    val newPoints = points.map(p => (coordinateX.indexOf(p._1), coordinateY.indexOf(p._2)))
    val grid = mutable.ArraySeq.fill(coordinateY.length, coordinateX.length)('.')
    val prefixSum = Array.fill(coordinateY.length, coordinateX.length)(0)

    for i <- 0 until newPoints.length do
        for x <- Math.min(newPoints(i)._1, newPoints((i + 1) % newPoints.length)._1) to Math.max(newPoints(i)._1, newPoints((i + 1) % newPoints.length)._1) do
            for y <- Math.min(newPoints(i)._2, newPoints((i + 1) % newPoints.length)._2) to Math.max(newPoints(i)._2, newPoints((i + 1) % newPoints.length)._2) do
                grid(y)(x) = '#'

    // Bfs to fill the area outside the polygon
    prefixSum(0)(0) = 1
    val directions = Array((1, 0), (-1, 0), (0, 1), (0, -1))
    val q = mutable.Queue[(Int, Int)]((0, 0))
    while q.nonEmpty do
        val (x, y) = q.dequeue()
        for (dx, dy) <- directions do
            val (nx, ny) = (x + dx, y + dy)
            if nx >= 0 && ny >= 0 && nx < grid(0).length && ny < grid.length && grid(ny)(nx) == '.' && prefixSum(ny)(nx) == 0 then
                prefixSum(ny)(nx) = 1
                q.enqueue((nx, ny))

    // Prepare prefix sum
    for x <- 0 until coordinateX.length - 1 do
        for y <- 0 until coordinateY.length - 1 do
            prefixSum(y + 1)(x + 1) += prefixSum(y + 1)(x) + prefixSum(y)(x + 1) - prefixSum(y)(x)
    
    // Check if the rectangle is inside the polygon
    def isInside(x1: Int, y1: Int, x2: Int, y2: Int) =
        val (maxX, maxY) = (Math.max(x1, x2), Math.max(y1, y2))
        val (minX, minY) = (Math.min(x1, x2), Math.min(y1, y2))
        if prefixSum(maxY)(maxX) - prefixSum(minY - 1)(maxX) - prefixSum(maxY)(minX - 1) + prefixSum(minY - 1)(minX - 1) > 0 then false
        else true

    // Calculate the answer
    (for
        i <- points.indices
        j <- i + 1 until points.length
        if isInside(newPoints(i)._1, newPoints(i)._2, newPoints(j)._1, newPoints(j)._2)
    yield area(points(i), points(j)))
    .max.toString()
package day8

import locations.Directory.currentDir
import inputs.Input.loadFileSync

@main def part1: Unit = 
    println(s"Answer to part 1: ${part1(loadInput())}")

@main def part2: Unit = 
    println(s"Answer to part 2: ${part2(loadInput())}")

def loadInput(): String = loadFileSync(s"$currentDir/input.txt")

trait Point:
    val x: Int
    val y: Int
    val z: Int

    def distance(that: Point) =
        Math.pow(this.x - that.x, 2L) + Math.pow(this.y - that.y, 2L) + Math.pow(this.z - that.z, 2L)

class DSU(val n: Int):
    val parent = Array.fill(n)(-1)

    def find(x: Int): Int =
        if parent(x) < 0 then x
        else 
            val root = find(parent(x))
            parent(x) = root
            root

    def union(x: Int, y: Int) =
        val rootX = find(x)
        val rootY = find(y)
        if rootX == rootY then false
        else if parent(rootX) < parent(rootY) then
            parent(rootX) += parent(rootY)
            parent(rootY) = rootX
            true
        else
            parent(rootY) += parent(rootX)
            parent(rootX) = rootY
            true

def getPoints(input: String) =
    input.linesIterator.map(line => line match {
        case s"${a},${b},${c}" => new Point {
            val x = a.toInt
            val y = b.toInt
            val z = c.toInt
        }
    }).toVector

def part1(input: String) =
    val points = getPoints(input)
    (for 
        i <- points.indices
        j <- i + 1 until points.length
    yield (points(i).distance(points(j)), (i, j)))
    .sortBy(_._1).take(1000).map(_._2)
    .foldLeft(new DSU(points.length)) { case (dsu, (i, j)) =>
        dsu.union(i, j)
        dsu
    }.parent.sorted.take(3).product.abs.toString()

def part2(input: String) =
    val points = getPoints(input)
    (for 
        i <- points.indices
        j <- i + 1 until points.length
    yield (points(i).distance(points(j)), (i, j)))
    .sortBy(_._1).map(_._2)
    .foldLeft(new DSU(points.length), 0L) { case ((dsu, ans), (i, j)) =>
        if dsu.union(i, j) == true 
        then (dsu, 1L * points(i).x  * points(j).x)
        else (dsu, ans)
    }._2.toString()
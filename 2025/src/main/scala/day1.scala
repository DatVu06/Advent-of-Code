package day1

import locations.Directory.currentDir
import inputs.Input.loadFileSync

@main def part1: Unit = 
    println(s"Answer to part 1: ${part1(loadInput())}")

@main def part2: Unit = 
    println(s"Answer to part 2: ${part2(loadInput())}")

def loadInput(): String = loadFileSync(s"$currentDir/input.txt")

def part1(input: String): String = {
    var rotations = Seq[Int]()

    def parseRotation(s: String): Int = s match {
        case s"R${n}" => n.toInt
        case s"L${n}" => -n.toInt
    }

    rotations = input.linesIterator.map(parseRotation).toSeq

    return rotations.foldLeft((0, 50)) { case ((ans, pos), rot) =>
        var newPos = (pos + rot + 100) % 100
        val newAns = ans + (if newPos == 0 then 1 else 0)
        (newAns, newPos)
    }._1.toString
}

def part2(input: String): String = {
    var rotations = Seq[Int]()

    def parseRotation(s: String): Int = s match {
        case s"R${n}" => n.toInt
        case s"L${n}" => -n.toInt
    }

    rotations = input.linesIterator.map(parseRotation).toSeq

    return rotations.foldLeft((0, 50)) { case ((ans, pos), rot) =>
        var newPos = pos + rot
        if pos == 0 && rot < 0 then newPos = 100 + rot
        var newAns = ans
        if newPos >= 100 then
            newAns = ans + (newPos / 100)
        else if newPos <= 0 then
            newAns = ans + (-newPos / 100) + 1
        newPos = (newPos % 100 + 100) % 100
        (newAns, newPos)
    }._1.toString
}
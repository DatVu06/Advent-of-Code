package day10

import locations.Directory.currentDir
import inputs.Input.loadFileSync
import scala.collection.mutable
import scala.compiletime.ops.double
import simplex.Simplex

@main def part1: Unit = 
    println(s"Answer to part 1: ${part1(loadInput())}")

@main def part2: Unit = 
    println(s"Answer to part 2: ${part2(loadInput())}")

def loadInput(): String = loadFileSync(s"$currentDir/input.txt")

def parseProblem(prob: String) =
    val parts = prob.split(" ")
    def removeStartAndEnd(s: String) = s.substring(1, s.length - 1)
    def parseIntArray(s: String) = s.split(",").map(_.toInt).toArray
    (
        removeStartAndEnd(parts.head), 
        parts.slice(1, parts.length - 1).map(s => parseIntArray(removeStartAndEnd(s))).toArray, 
        parseIntArray(removeStartAndEnd(parts.last))
    )

def solvePart1(prob: String) = 
    val (config, buttons, _) = parseProblem(prob)
    val buttonMasks = buttons.map(b => b.foldLeft(0)((mask, bit) => mask | (1 << bit)))
    val n = config.length
    buttonMasks.foldLeft(mutable.ArraySeq.fill(1 << n)(Int.MaxValue).updated(0, 0)) { (dp, buttonMask) =>
        val newDp = dp.clone()
        for mask <- 0 until (1 << n) do
            val newMask = mask ^ buttonMask
            if dp(mask) != Int.MaxValue then newDp(newMask) = Math.min(newDp(newMask), dp(mask) + 1)
        newDp
    } (config.foldRight(0)((ch, mask) => if ch == '.' then (mask << 1) else (mask << 1) | 1))


def part1(input: String) =
    input.linesIterator.map(solvePart1).sum.toString()

def branchAndBound(A: Array[Array[Double]], B: Array[Double], C: Array[Double]): (Int, Array[Int]) =
    val n = A(0).length
    var maxVal = Double.NegativeInfinity
    val bestSolution = Array.fill(n)(0)
    val eps = 1e-9

    def dfs(A: Array[Array[Double]], B: Array[Double]): Unit =
        val simplex = new Simplex(A, B, C)
        val result = simplex.solve()
        var branched = false
        // println(s"LP result: ${result}")
        if result._1 + eps <= maxVal then return
        for i <- result._2.indices do
            if !branched && Math.abs(result._2(i) - result._2(i).round) > eps then
                // Branch on floor
                dfs(A :+ Array.fill(n)(0.0).updated(i, 1.0), B :+ result._2(i).toInt)
                // Branch on ceil
                dfs(A :+ Array.fill(n)(0.0).updated(i, -1.0), B :+ ~result._2(i).toInt)
                branched = true
        if !branched && result._1 + eps > maxVal then
            maxVal = result._1
            // println(s"New best value: ${maxVal}, solution: ${result._2.mkString(", ")}")
            bestSolution.indices.foreach(i => bestSolution(i) = result._2(i).round.toInt)

    dfs(A, B)
    // println(s"Best solution value: ${maxVal}, solution: ${bestSolution.mkString(", ")}")
    (maxVal.round.toInt, bestSolution)
    

def solvePart2(prob: String) =
    val (_, buttons, jolts) = parseProblem(prob)
    val C = Array.fill(buttons.length)(-1.0) // get min
    val B = jolts.map(_.toDouble).toArray ++ jolts.map(a => -a.toDouble).toArray // To simulate equations of type <=
    val A = Array.ofDim[Double](jolts.length * 2, buttons.length)
    for i <- buttons.indices do
        for j <- buttons(i).indices do
            A(buttons(i)(j))(i) = 1.0
            A(buttons(i)(j) + jolts.length)(i) = -1.0
    // println(s"{A: {${A.map(_.mkString(", ")).mkString("},\n{")}}")
    // println(s"B: ${B.mkString(", ")}")
    // println(s"C: ${C.mkString(", ")}")
    val ans = branchAndBound(A, B, C)
    // Check answer validity
    val got_jolt = Array.fill(jolts.length)(0)
    for i <- ans._2.indices do
        if ans._2(i) >= 0 then
            for j <- buttons(i).indices do
                got_jolt(buttons(i)(j)) += ans._2(i).toInt
    for i <- jolts.indices do
        if got_jolt(i) != jolts(i) then
            throw Exception(s"Invalid solution: expected ${jolts.mkString(", ")}, got ${got_jolt.mkString(", ")}")
    -ans._1.round.toLong

def part2(input: String) =
    input.linesIterator.map(solvePart2).sum.toString()
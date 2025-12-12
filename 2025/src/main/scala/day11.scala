package day11

import locations.Directory.currentDir
import inputs.Input.loadFileSync
import scala.collection.mutable

@main def part1: Unit = 
    println(s"Answer to part 1: ${part1(loadInput())}")

@main def part2: Unit = 
    println(s"Answer to part 2: ${part2(loadInput())}")

def loadInput(): String = loadFileSync(s"$currentDir/input.txt")

def parseGraph(input: String) =
    val (nodes, edges) = input.linesIterator.foldLeft(Array[String](), mutable.Map[String, Array[String]]()) { (acc, line) =>
        val Array(key, rest) = line.split(":")
        (acc._1 :+ key.trim, acc._2.updated(key.trim, rest.trim.split(" ").map(_.trim)))
    }
    (edges.foldLeft(nodes) { case (allNodes, (key, values)) =>
        allNodes ++ values.filter(v => !allNodes.contains(v))
    }, edges.toMap)

def countPaths(nodes: Array[String], edges: Map[String, Array[String]], start: String, end: String) =
    val dp = mutable.Map[String, Long]().withDefaultValue(0L)
    val queue = mutable.Queue[String]()
    val degree = mutable.Map[String, Int]().withDefaultValue(0)
    dp(start) = 1L
    for (from, tos) <- edges do
        for to <- tos do
            degree(to) = degree(to) + 1
    nodes.foreach(node => if degree(node) == 0 then queue.enqueue(node))
    while queue.nonEmpty do
        val node = queue.dequeue()
        for nxt <- edges.getOrElse(node, Array.empty[String]) do
            dp(nxt) = dp(nxt) + dp(node)
            degree(nxt) = degree(nxt) - 1
            if degree(nxt) == 0 then queue.enqueue(nxt)
    dp(end)

def part1(input: String) =
    val (nodes, edges) = parseGraph(input)
    countPaths(nodes, edges, "you", "out").toString()

def countPathsWithIntermediates(nodes: Array[String], edges: Map[String, Array[String]], start: String, intermediates: Array[String], end: String) =
    val dp = mutable.Map[String, Array[Long]]()
    val queue = mutable.Queue[String]()
    val degree = mutable.Map[String, Int]().withDefaultValue(0)
    dp(start) = Array.fill(1 << intermediates.length)(0L)
    dp(start)(0) = 1L
    for (from, tos) <- edges do
        for to <- tos do
            degree(to) = degree(to) + 1
    nodes.foreach(node => if degree(node) == 0 then queue.enqueue(node))
    while queue.nonEmpty do
        val node = queue.dequeue()
        for nxt <- edges.getOrElse(node, Array.empty[String]) do
            val bit = intermediates.indexOf(nxt)
            if !dp.contains(nxt) then dp(nxt) = Array.fill(1 << intermediates.length)(0L)
            for mask <- 0 until (1 << intermediates.length) do
                val newMask = if bit != -1 then mask | (1 << bit) else mask
                dp(nxt)(newMask) = dp(nxt)(newMask) + dp(node)(mask)
            degree(nxt) = degree(nxt) - 1
            if degree(nxt) == 0 then queue.enqueue(nxt)
    dp(end)((1 << intermediates.length) - 1)

def part2(input: String) =
    val (nodes, edges) = parseGraph(input)
    countPathsWithIntermediates(nodes, edges, "svr", Array("dac", "fft"), "out").toString()
package simplex

import scala.collection.mutable

// Solve linear programming problems using the Simplex algorithm
// Maximize: c^T * x
// Conditions: A * x <= b, x >= 0
class Simplex(val A: Array[Array[Double]], val b: Array[Double], val c: Array[Double]):
    val m = b.length
    val n = c.length
    val N = mutable.ArraySeq.fill(n + 1)(0)
    val B = mutable.ArraySeq.fill(m)(0)
    val D = mutable.ArraySeq.fill(m + 2, n + 2)(0.0)
    val eps = 1e-9
    val INF = Double.PositiveInfinity

    for i <- 0 until m do
        for j <- 0 until n do
            D(i)(j) = A(i)(j)
    for i <- 0 until m do
        D(i)(n) = -1.0
        D(i)(n + 1) = b(i)
        B(i) = n + i
    for j <- 0 until n do
        D(m)(j) = -c(j)
        N(j) = j
    D(m + 1)(n) = 1.0
    N(n) = -1

    def pivot(r: Int, s: Int) =
        val inv = 1.0 / D(r)(s)
        for i <- 0 until m + 2 do
            if i != r && Math.abs(D(i)(s)) > eps then
                val factor = D(i)(s) * inv
                for j <- 0 until n + 2 do
                    D(i)(j) -= factor * D(r)(j)
                D(i)(s) = D(r)(s) * factor
        for i <- 0 until m + 2 do
            if i != r then D(i)(s) *= -inv
        for j <- 0 until n + 2 do
            if j != s then D(r)(j) *= inv
        D(r)(s) = inv

        val temp = B(r)
        B(r) = N(s)
        N(s) = temp
    
    def simplex(phase: Int): Boolean =
        val x = m + phase - 1
        while true do
            var s = -1
            for j <- 0 until n + 1 do
                if N(j) != -phase then
                    if s == -1 || D(x)(j) < D(x)(s) ||  (D(x)(j) == D(x)(s) && N(j) < N(s)) then
                        s = j
            if D(x)(s) >= -eps then return true

            var r = -1
            for i <- 0 until m do
                if D(i)(s) > eps then
                    if r == -1 || D(i)(n + 1) / D(i)(s) < D(r)(n + 1) / D(r)(s) || (D(i)(n + 1) / D(i)(s) == D(r)(n + 1) / D(r)(s) && B(i) < B(r)) then
                        r = i
            if r == -1 then return false
            pivot(r, s)
        true


    def solve(): (Double, Array[Double]) =
        var r = 0
        for i <- 1 until m do
            if D(i)(n + 1) < D(r)(n + 1) then
                r = i
        if D(r)(n + 1) < -eps then
            pivot(r, n)
            if !simplex(2) || D(m + 1)(n + 1) < -eps then
                return (Double.NegativeInfinity, Array.empty[Double])
            for i <- 0 until m do
                if B(i) == -1 then
                    var s = 0
                    for j <- 1 until n + 1 do
                        if s == -1 || D(i)(j) < D(i)(s) || (D(i)(j) == D(i)(s) && N(j) < N(s)) then
                            s = j
                    pivot(i, s)

        if !simplex(1) then
            return (Double.NegativeInfinity, Array.empty[Double])
        val x = Array.fill(n)(0.0)
        for i <- 0 until m do
            if B(i) < n then
                x(B(i)) = D(i)(n + 1)
        return (D(m)(n + 1), x)
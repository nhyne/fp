object Fibonacci {
    def main(args: Array[String]) : Unit = {
        val something = fibonacci(6)
        println(s"fibnumber is $something")
    }

    def fibonacci(n: Int) : Int = {
        @annotation.tailrec
        def go(n: Int, prev: Int, cur: Int): Int  = {
            if (n == 0) prev
            else go(n - 1, cur, cur + prev)
        }
        go(n, 0, 1)
    }
}

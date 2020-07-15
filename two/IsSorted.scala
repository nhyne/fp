package dev.nhyne.fp.two

object IsSorted {
    def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
        @annotation.tailrec
        def go(n: Int) : Boolean = {
            val next = n + 1
            if (n == as.length - 1) {
                true
            }
            else if (!ordered(as(n), as(next))) {
                false
            }
            else {
                go(n+1)
            }
        }
        go(0)
    }

    def order(a: Int, b: Int): Boolean = {
        a <= b
    }

    def main(args: Array[String]): Unit = {
        isSorted(Array(1, 2, 3), order)
    }
}


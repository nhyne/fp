package seven

object Parallel {
    case class Par[A](v: A)

    def unit[A](a: A): Par[A] = ???
    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def run[A](a: => Par[A]): A = {
        ???
    }

    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => Par[C]) : Par[C] = ???

    def fork[A](a: => Par[A]): Par[A] = ???

    def sums(ints: IndexedSeq[Int]): Int =
        if (ints.size <= 1) ints.headOption.getOrElse(0)
        else {
            val (l, r) = ints.splitAt(ints.length / 2)
            val sumL: Par[Int] = Parallel.unit(sums(l))
            val sumR: Par[Int] = Parallel.unit(sums(r))

            Parallel.run(sumL) + Parallel.run(sumR)
        }


    def main(args: Array[String]): Unit = {
        println("we're in parallel main!")
    }
}


package seven

object Parallel {
    case class Par[A](v: A)

    def unit[A](a: => A): Par[A] = ???

    def get[A](a: => Par[A]): A = ???

    def sums(ints: IndexedSeq[Int]): Int =
        if (ints.size <= 1) ints.headOption.getOrElse(0)
        else {
            val (l, r) = ints.splitAt(ints.length / 2)
            val sumL: Par[Int] = Parallel.unit(sums(l))
            val sumR: Par[Int] = Parallel.unit(sums(r))

            Parallel.get(sumL) + Parallel.get(sumR)
        }

}


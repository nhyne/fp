import java.util.concurrent.{ExecutorService, Future}

object Parallel {
    type Par[A] = ExecutorService => Future[A]

    def unit[A](a: A): Par[A] = ???
    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def run[A](es: ExecutorService)(a: => Par[A]): Future[A] = a(es)

    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => Par[C]) : Par[C] = ???

    def map[A, B](ps: Par[A])(f: A => B): Par[B] =
        map2(ps, unit(()))((a, _) => unit(f(a)))

    def fork[A](a: => Par[A]): Par[A] = ???

    def asyncF[A, B](f: A => B): A => Par[B] = ???

    def sequence[A](ps: List[Par[A]]): Par[List[A]] =
        ps.foldRight[Par[List[A]]](unit(List()))((h,t) => map2(h,t)((a, b) => unit(a :: b)))

    def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
        val fibs : List[Par[B]] = ps.map(asyncF(f))
        sequence(fibs)
    }

    def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
        es => {
            val choice = run(es)(pa).get()
            run(es)(choices(choice))
        }

    def parFilter[A](as: List[A])(f: A => Boolean) : Par[List[A]] = {
        as.foldRight[Par[List[A]]](unit(List())) { (a, b) =>
            if (f(a)) {
                map2(unit(a), b)((h, t) => unit(h :: t))
            } else b
        }
    }

    def parFilterSolution[A](as: List[A])(f: A => Boolean) : Par[List[A]] = {
        val pars = as.map(asyncF((a: A) => if(f(a)) List(a) else List()))
        map(sequence(pars))(_.flatten)
    }

    def sums(es: ExecutorService)(ints: IndexedSeq[Int]): Int = {
        if (ints.size <= 1) ints.headOption.getOrElse(0)
        else {
            val (l, r) = ints.splitAt(ints.length / 2)
            val sumL: Par[Int] = Parallel.unit(sums(es)(l))
            val sumR: Par[Int] = Parallel.unit(sums(es)(r))

            Parallel.run(es)(sumL).get() + Parallel.run(es)(sumR).get()
        }
    }


    def main(args: Array[String]): Unit = {
        println("we're in parallel main!")
    }
}

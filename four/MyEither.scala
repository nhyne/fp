sealed trait MyEither[+E, +A] {
    def map[B](f: A => B): MyEither[E, B] = {
        this match {
            case MyLeft(v) => MyLeft(v)
            case MyRight(v) => MyRight(f(v))
        }
    }

    def flatMap[EE >: E, B](f: A => MyEither[EE, B]): MyEither[EE, B] = {
        this match {
            case MyLeft(v) => MyLeft(v)
            case MyRight(v) => f(v)
        }
    }

    def orElse[EE >: E, B >: A](b: => MyEither[EE, B]): MyEither[EE, B] = {
        this match {
            case MyLeft(v) => b
            case MyRight(v) => MyRight(v)
        }
    }

    def map2[EE >: E, B, C](b: MyEither[EE, B])(f: (A, B) => C): MyEither[EE, C] = {
        for {
            aa <- this
            bb <- b
        } yield f(aa, bb)
    }

}

case class MyLeft[+E](value: E) extends MyEither[E, Nothing]

case class MyRight[+A](value: A) extends MyEither[Nothing, A]

object MyEither {
    def sequence[E, A](es: List[MyEither[E, A]]): MyEither[E, List[A]] = {
        es match {
            case Nil => MyRight(List.empty[A])
            case h :: t => for {
                a <- h
                tt <- sequence(t)
            } yield a :: tt
        }
    }

    def traverse[E, A, B](as: List[A])(f: A => MyEither[E, B]): MyEither[E, List[B]] = ???

    def main(args: Array[String]): Unit = {

        val eitherList = List(MyLeft("a"), MyRight(2), MyRight(3))
        println(s"${sequence(eitherList)}")

    }
}

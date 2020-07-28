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
        this match {
            case MyLeft(v) => MyLeft(v)
            case MyRight(v) => b match {
                case MyLeft(x) => MyLeft(x)
                case MyRight(x) => MyRight(f(v, x))
            }
        }
    }
}

case class MyLeft[+E](value: E) extends MyEither[E, Nothing]

case class MyRight[+A](value: A) extends MyEither[Nothing, A]

object MyEither {

    def main(args: Array[String]): Unit = {
        println("yay!")
    }
}

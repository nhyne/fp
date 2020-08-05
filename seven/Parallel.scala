package seven

trait Parallel {
    case class Par[A](v: A)

    def unit[A](a: => A): Par[A]

    def get[A](a: => Par[A]): A
}

object Parallel {


}


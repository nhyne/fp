package twelve
import eleven.Functor

trait Applicative[F[_]] extends Functor[F] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

object Main {
  val listFunctor = new Functor[List] {
    def map[A, B](as: List[A])(f: A => B): List[B] = as.map(f)
  }

  def main(args: Array[String]): Unit =
    println("something")
}

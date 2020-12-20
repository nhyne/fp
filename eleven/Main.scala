package eleven
import six.RNG
import six.RNG.Rand

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))
  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List[A]()))((a, acc) => map2(a, acc)(_ :: _))
  def sequenceLeft[A](lfa: List[F[A]]): F[List[A]] =
    lfa.foldLeft(unit(List[A]()))((acc, a) => map2(a, acc)(_ :: _))
  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    sequence(la.map(f))
  def theirTraverse[A, B](ls: List[A])(f: A => F[B]): F[List[B]] =
    ls.foldLeft(unit(List[B]()))((acc, a) => map2(f(a), acc)(_ :: _))

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = {
    def loop(n: Int, ma: F[A], acc: F[List[A]]): F[List[A]] =
      if (n == 0) acc
      else loop(n - 1, ma, map2(ma, acc)(_ :: _))

    loop(n, ma, unit(List[A]()))
  }

  def theirReplicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence(List.fill(n)(ma))

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = {
    val s = sequence(ms.map(f))
    map2(unit(ms), s) { (m, bools) =>
      val zipped = m.zip(bools)
      zipped.foldLeft(List[A]()) {
        case (acc, (a, boo)) =>
          if (boo) a :: acc
          else acc
      }
    }
  }

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)
}

object Monad {
  val genMonad = new Monad[Rand] {
    override def unit[A](a: => A): Rand[A] = RNG.unit(a)

    override def flatMap[A, B](fa: Rand[A])(f: A => Rand[B]): Rand[B] =
      RNG.flatMap(fa)(f)
  }

  val optionMonad = new Monad[Option] {
    def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
      fa.flatMap(f)

    def unit[A](a: => A): Option[A] = Some(a)
  }

  val listMonad = new Monad[List] {
    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] =
      fa.flatMap(f)

    override def unit[A](a: => A): List[A] = List(a)
  }
}

object Main {
  val listFunctor = new Functor[List] {
    def map[A, B](as: List[A])(f: A => B): List[B] = as.map(f)
  }

  def main(args: Array[String]): Unit =
    println("something")
}

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil // The book says this should probably throw an error which makes sense. Sticking with my original though.
      case Cons(_, t) => t
    }
  }

  def setHead[A](h: A, l: List[A]): List[A] = {
    l match {
      case Nil => sys.error("Attempting to set head of nil list.")
      case Cons(_, t) => Cons(h, t)
    }
  }

  @scala.annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) {
      l
    }
    else {
      l match {
        case Nil => sys.error("Attempting to remove elements from nil list.")
        case Cons(_, t) => {
          drop(t, n -1)
        }
      }
    }
  }

  @scala.annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(h, t) if (f(h)) => {
          dropWhile(t, f)
        }
      case _ => l
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(foldLeft(xs, z)(f), x)
    }
  }

  def sumLeft(as: List[Int]) : Int = {
    foldLeft(as, 0)((b, a) => b + a)
  }

  def productLeft(as: List[Int]): Int = {
    foldLeft(as, 1)((b, a) => b * a)
  }

  def lengthLeft[A](as: List[A]): Int = {
    foldLeft(as, 0)((b, _) => b + 1)
  }

  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, a) => a + 1)
  }

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc,h) => Cons(h,acc))

//  def reverse[A](as: List[A]): List[A] = {
//    foldLeft(as, List[A]())((b, a) => Cons(a, b))
//  }


      def main(args: Array[String]) : Unit = {
        val something = reverse(List.apply(1, 2, 3))
        println(s"reversed is $something")
    }

}

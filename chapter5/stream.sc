sealed trait MyStream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): MyStream[A] = {
    this match {
      case Cons(h, t) if n > 1 => Cons(h, () => t().take(n - 1))
      case Cons(h, _) if n == 1 => Cons(h, () => Empty)
      case _ => Empty
    }
  }

  def takeWhile(p: A => Boolean): MyStream[A] = {
    this match {
      case Cons(h, t) if p(h()) => MyStream.cons(h(), t().takeWhile(p))
      case _ => Empty
    }
  }

  def takeWhileFoldRight(p: A => Boolean): MyStream[A] = {
    foldRight(MyStream.empty[A])((a, b) => if (p(a)) MyStream.cons(a, b) else MyStream.empty)
  }

  def drop(n: Int): MyStream[A] = ???

  def forAll(p: A => Boolean): Boolean = {
    this match {
      case Empty => true
        case Cons(h, t) if p(h()) => t().forAll(p)
      case _ => false
    }
  }

  def headOption: Option[A] = {
    foldRight(None: Option[A])((a, _) => Some(a))
  }

  def toList: List[A] = {
    @scala.annotation.tailrec
    def go(s: MyStream[A], acc: List[A]): List[A] = {
      s match {
        case Empty => acc
        case Cons(h, t) => go(t(), h() :: acc)
      }
    }
    go(this, List.empty).reverse
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): MyStream[B] = {
    foldRight(MyStream.empty[B])((h,t) => MyStream.cons(f(h), t))
  }

  def filter(p: A => Boolean): MyStream[A] = {
    foldRight(MyStream.empty[A])((h, t) => if (p(h)) MyStream.cons(h, t) else t)
  }

  def append[B>:A](a: => MyStream[B]): MyStream[B] = {
    foldRight(a)((h, t) => MyStream.cons(h, t))
  }

  def flatMap[B](f: A => MyStream[B]) : MyStream[B] = {
    foldRight(MyStream.empty[B])((h, t) => f(h) append t)
  }

  def startsWith[B](s: MyStream[B]): Boolean = ???

}
case object Empty extends MyStream[Nothing]
case class Cons[+A](h: () => A, t: () => MyStream[A]) extends MyStream[A]

object MyStream {
  def cons[A](hd: => A, tl: => MyStream[A]): MyStream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: MyStream[A] = Empty

  def apply[A](as: A*): MyStream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: MyStream[Int] = Cons(() => 1, () => ones)

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): MyStream[A] = ???

  def constant[A](a: A): MyStream[A] = {
    lazy val myStream : MyStream[A] = Cons(() => a, () => myStream)
    myStream
  }

  def from(n: Int): MyStream[Int] = {
    MyStream.cons(n, from(n + 1))
  }

  def fibs() : MyStream[Int] = {
    def go(a: Int, b: Int): MyStream[Int] = {
      cons(a, go(b, a + b))
    }
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]) : MyStream[A] = {
    f(z) match {
      case Some((h, t)) => cons(h, unfold(t)(f))
      case None => empty
    }
  }
//  def main(args: Array[String]): Unit = {
//    println("In main")
//    val stream = cons(1, cons(2, Empty))
//    val something = stream.forAll((a) => a >= 1)
//    println(s"$something")
//  }
}

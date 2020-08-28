sealed trait MyOption[+A] {
  def map[B](f: A => B): MyOption[B] = {
    this match {
      case MySome(x) => MySome(f(x))
      case MyNone    => MyNone
    }
  }

  def flatMap[B](f: A => MyOption[B]): MyOption[B] = {
    map(f) getOrElse MyNone
  }

  def getOrElse[B >: A](default: => B): B = {
    this match {
      case MySome(x) => x
      case MyNone    => default
    }
  }

  def orElse[B >: A](ob: => MyOption[B]): MyOption[B] = {
    this map (MySome(_)) getOrElse ob
  }

  def filter(f: A => Boolean): MyOption[A] = {
    this match {
      case MySome(x) if f(x) => MySome(x)
      case MyNone            => MyNone
    }
  }

  def map2[A, B, C](a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] = {
    a.flatMap(aa => b.map(bb => f(aa, bb)))
  }

  def forMap2[A, B, C](a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] = {
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)
  }

  def map3[A, B, C, D](a: MyOption[A], b: MyOption[B], c: MyOption[C])(
    f: (A, B, C) => D
  ): MyOption[D] = {
    a.flatMap(aa => b.flatMap(bb => c.map(cc => f(aa, bb, cc))))
  }

  //    def something(a: Int, b: Int) = a * b + 3
  //
  //    def lift2[A, B, C](b: MyOption[B])(f: (A, B) => C): (MyOption[A], MyOption[B]) => MyOption[C] = {
  //        _ flatMap(aa => b.map( bb => f(aa, bb)))
  //    }

  def sequence[A](a: List[MyOption[A]]): MyOption[List[A]] = {
    a match {
      case Nil    => MyNone
      case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }
  }

  def sequenceT[A](a: List[MyOption[A]]): MyOption[List[A]] = {
    traverse(a)(x => x)
  }

  def traverse[A, B](a: List[A])(f: A => MyOption[B]): MyOption[List[B]] = {
    a match {
      case Nil => MySome(Nil)
      //            case h :: t => f(h).flatMap(hh => traverse(t)(f) map (hh :: _))
      case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
    }
  }

}

case class MySome[+A](get: A) extends MyOption[A]

object MyNone extends MyOption[Nothing] {
  def main(args: Array[String]): Unit = {

    val someList = List("1", "2", "3", "a")

    def toIntMyOption(s: String) = {
      try {
        MySome(s.toInt)
      } catch {
        case e: Exception => MyNone
      }
    }

    println(s"traversed list: ${traverse(someList)(toIntMyOption)}")

    val otherList = List("1", "2", "3")
    println(s"other traversed list: ${traverse(otherList)(toIntMyOption)}")

    println(toIntMyOption("1"))

  }
}

sealed trait MyOption[+A] {
  def map[B](f: A => B): MyOption[B] = {
    this match {
      case MySome(x) => MySome(f(x))
      case MyNone => MyNone
    }
  }


  def flatMap[B](f: A => MyOption[B]): MyOption[B] = {
      map(f) getOrElse MyNone
  }
  def getOrElse[B >: A](default: => B): B = {
    this match {
      case MySome(x) => x
      case MyNone => default
    }
  }

  def orElse[B >:A] (ob: => MyOption[B]): MyOption[B] = {
      this map (MySome(_)) getOrElse ob
  }

  def filter(f: A => Boolean): MyOption[A] = {
    this match {
      case MySome(x) if f(x) => MySome(x)
      case MyNone => MyNone
    }
  }

  def map2[A, B, C](a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] = {
    a.flatMap(aa => b.map(bb => f(aa, bb)))
  }

    def map3[A, B, C, D](a: MyOption[A], b: MyOption[B], c: MyOption[C])(f: (A, B, C) => D): MyOption[D] = {
        a.flatMap(aa => b.flatMap(bb => c.map(cc => f(aa, bb, cc))))
    }

//    def something(a: Int, b: Int) = a * b + 3
//
//    def lift2[A, B, C](b: MyOption[B])(f: (A, B) => C): (MyOption[A], MyOption[B]) => MyOption[C] = {
//        _ flatMap(aa => b.map( bb => f(aa, bb)))
//    }

    def sequence[A](a: List[MyOption[A]]): MyOption[List[A]] = {
        a match {
            case Nil => MyNone
            case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
        }
    }
}

case class MySome[+A](get: A) extends MyOption[A]
object MyNone extends MyOption[Nothing] {
    def main(args: Array[String]): Unit = {

        def someFunction(a: Int, b: Int) = a * b

//        val liftedFunction = map2(MySome(1), MySome(2))(someFunction)
//
//        println(s"First lift: ${liftedFunction()}")





    }
}

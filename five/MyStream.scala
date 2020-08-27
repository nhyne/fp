sealed trait MyStream[+A] {
    def toList: List[A] = {
        this match {
            case Empty => Nil
            case Cons(h, t) => (h() :: t().toList)
        }
    }

    def take(n: Int): MyStream[A] = {
        this match {
            case Empty => Empty
            case Cons(h, t) => {
                if (n <= 1) Cons(h, () => Empty)
                else Cons(h, () => t().take(n - 1))
            }
        }
    }

    def drop(n: Int): MyStream[A] = {
        this match {
            case Empty => Empty
            case Cons(h, t) => {
                if (n <= 0) Cons(h, t)
                else t().drop(n - 1)
            }
        }
    }

    def takeWhile(p: A => Boolean): MyStream[A] = {
        this match {
            case Empty => Empty
            case Cons(h, t) => {
                if (p(h())) Cons(h, () => t().takeWhile(p))
                else Empty
            }
        }
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B =
        this match {
            case Cons(h, t) => f(h(), t().foldRight(z)(f))
            case _ => z
        }

    def forAll(p: A => Boolean): Boolean = {
        this match {
            case Cons(h, t) => p(h()) && t().forAll(p)
            case _ => true
        }
    }

    def takeWhileFold(p: A => Boolean): MyStream[A] = {
        foldRight(MyStream.empty[A])((a, b) => {
            if (p(a)) {
                MyStream.cons(a, b)
            } else b
        })
    }

    def headOption(): Option[A] = {
        foldRight(None: Option[A])((h, _) => {
            Some(h)
        })
    }

    def map[B](f: A => B): MyStream[B] = {
        foldRight(MyStream.empty[B])((h, t) => {
            MyStream.cons(f(h), t)
        })
    }

    def filter(p: A => Boolean): MyStream[A] = {
        foldRight(MyStream.empty[A])((h, t) => {
            if (p(h)) MyStream.cons(h, t) else t
        })
    }

    def append[B >: A](a: => MyStream[B]): MyStream[B] = {
        foldRight(a)((h, t) => {
            MyStream.cons(h, t)
        })
    }

    def flatMap[B](f: A => MyStream[B]): MyStream[B] = {
        foldRight(MyStream.empty[B])((h, t) => {
            t.append(f(h))
        })
    }

    def find(p: A => Boolean): Option[A] = filter(p).headOption()

    def mapUnfold[B](f: A => B): MyStream[B] = {
        MyStream.unfold(this){
            case Cons(h, t) => Some(f(h()), t())
            case _ => None
        }
    }

    def takeUnfold(i: Int) : MyStream[A] = {
        MyStream.unfold((this, i)) {
            case (Cons(h, t), x) if (x > 0) => {
                Some((h(), (t(), x - 1)))
            }
            case _ => None
        }
    }

    def takeWhileUnfold(p: A => Boolean) : MyStream[A] = {
        MyStream.unfold(this) {
            case Cons(h, t) if p(h()) => Some((h(), t()))
            case _ => None
        }
    }

    def zipAllUnfold[B](s2: MyStream[B]): MyStream[(Option[A], Option[B])] = {
        MyStream.unfold(this, s2) {
            case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
            case (Empty, Cons(h2, t2)) => Some(((None, Some(h2())), (Empty, t2())))
            case (Cons(h1, t1), Empty) => Some(((Some(h1()), None), (t1(), Empty)))
            case _ => None
        }
    }

     def unfold[A, S](z: S)(f: S => Option[(A, S)]): MyStream[A] =
    f(z) match {
      case Some((h,s)) => Cons(() => h, () => unfold(s)(f))
      case None => Empty
    }


      def zipWith[B,C](s2: MyStream[B])(f: (A,B) => C): MyStream[C] =
    unfold((this, s2)) {
      case (Cons(h1,t1), Cons(h2,t2)) =>
        Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  // special case of `zipWith`
  def zip[B](s2: MyStream[B]): MyStream[(A,B)] =
    zipWith(s2)((_,_))


    def startsWith[A](s: MyStream[A]): Boolean = {
        MyStream.zipWithUnfold(this, s)((a, b) => {
            a == b
        }).find(!_).getOrElse(true)
    }

    def tails: MyStream[MyStream[A]] = {
        MyStream.unfold(this) {
            case Empty => None
            case s => Some((s, s.drop(1)))
        }.append(MyStream.empty)
    }

}

case object Empty extends MyStream[Nothing]

case class Cons[+A](h: () => A, t: () => MyStream[A]) extends MyStream[A]

object MyStream {
    def cons[A](hd: => A, t1: => MyStream[A]): MyStream[A] = {
        lazy val head = hd
        lazy val tail = t1

        Cons(() => head, () => tail)
    }

    def empty[A]: MyStream[A] = Empty

    def apply[A](as: A*): MyStream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

    def constant[A](a: A): MyStream[A] = {
        lazy val s: MyStream[A] = MyStream.cons(a, s)
        s
    }

    def from(n: Int): MyStream[Int] = {
        lazy val s: MyStream[Int] = MyStream.cons(n, s.map(_ + 1))
        s
    }

    def fibs(): MyStream[Int] = {
        def fib(a: Int, b: Int): MyStream[Int] = MyStream.cons(a, fib(b, a + b))

        fib(0, 1)
    }

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): MyStream[A] = {
        val newState = f(z)
        newState match {
            case Some((a, s)) => MyStream.cons(a, unfold(s)(f))
            case _ => Empty
        }
    }

    def fibsUnfold(): MyStream[Int] = {
        unfold((0, 1)){
            case (a, b) => Some((a, (b, a + b)))
        }
    }

    def constantUnfold[A](a: A) : MyStream[A] = {
        unfold(a)(x => Some((x, x)))
    }

    def fromUnfold(i: Int) : MyStream[Int] = {
        unfold(i)(x => Some((x, x + 1)))
    }

    def onesUnfold() : MyStream[Int] = {
        unfold(1)(_ => Some((1, 1)))
    }

    def zipWithUnfold[A, B](s0: MyStream[A], s1: MyStream[A])(f: (A, A) => B): MyStream[B] = {
        MyStream.unfold((s0, s1)) {
            case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
            case _ => None
        }
    }

    def main(args: Array[String]): Unit = {
        println("cool")

        val testStream = MyStream("a", "b", "c")
        val testList = testStream.toList
        println(s"list: $testList")

        println(s"taken: ${testStream.take(2).toList}")
        println(s"dropped: ${testStream.drop(2).toList}")

        def isAOrB(s: String) = s == "a" || s == "b"

        println(s"takeWhile: ${testStream.takeWhile(isAOrB).toList}")

        println(s"takeWhileFold: ${testStream.takeWhileFold(isAOrB).toList}")
        println(s"headOption: ${testStream.headOption()}")
        println(s"headOption of empty: ${MyStream.empty.headOption()}")

        val mappingStream = MyStream(1, 2, 3, 4)
        println(s"mapping: ${mappingStream.map(_ * 2).toList}")
        println(s"filtering: ${mappingStream.filter(_ % 2 == 0).toList}")
        println(s"appending: ${mappingStream.append(MyStream(5, 6, 7)).toList}")

        println(s"from 5: ${MyStream.from(5).take(5).toList}")
        println(s"fibs: ${MyStream.fibs().take(20).toList}")

        println(s"unfold test: ${MyStream.unfold(1)(x => Some((x + 1, x+ 1))).take(20).toList}")
        println(s"fibs via unfold: ${MyStream.fibsUnfold().take(20).toList}")
        println(s"constant via unfold: ${MyStream.constantUnfold(1).take(5).toList}")
        println(s"from via unfold: ${MyStream.fromUnfold(5).take(5).toList}")
        println(s"ones via unfold: ${MyStream.onesUnfold().take(10).toList}")

        println(s"map via unfold: ${mappingStream.mapUnfold(_ + 1).toList}")
        println(s"take unfold: ${mappingStream.takeUnfold(2).toList}")
        println(s"take while unfold: ${mappingStream.takeWhileUnfold(_ < 3).toList}")
        println(s"zip with unfold: ${MyStream.zipWithUnfold(mappingStream, mappingStream)(_ + _).toList}")

        println(s"zip all via unfold: ${testStream.zipAllUnfold(mappingStream).toList}")

        println(s"starts with (true): ${mappingStream.startsWith(MyStream(1, 2, 3))}")
        println(s"starts with (false): ${mappingStream.startsWith(MyStream(2, 3))}")

        println(s"tails: ${mappingStream.tails.map(_.toList).toList}")
    }
}

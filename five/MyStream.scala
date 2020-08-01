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
    }
}

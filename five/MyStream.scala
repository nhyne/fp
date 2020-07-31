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
        foldRight(MyStream.empty[A])( (a, b) => {
            if (p(a)) {
                MyStream.cons(a, b)
            } else b
        })
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
    }
}

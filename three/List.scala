sealed trait MyList[+A]
case object Nil extends MyList[Nothing]
case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {
  def apply[A](as: A*): MyList[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: MyList[A]): MyList[A] = {
    l match {
      case Nil => Nil // The book says this should probably throw an error which makes sense. Sticking with my original though.
      case Cons(_, t) => t
    }
  }

  def setHead[A](h: A, l: MyList[A]): MyList[A] = {
    l match {
      case Nil => sys.error("Attempting to set head of nil list.")
      case Cons(_, t) => Cons(h, t)
    }
  }

  @scala.annotation.tailrec
  def drop[A](l: MyList[A], n: Int): MyList[A] = {
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
  def dropWhile[A](l: MyList[A], f: A => Boolean): MyList[A] = {
    l match {
      case Cons(h, t) if (f(h)) => {
          dropWhile(t, f)
        }
      case _ => l
    }
  }

  def init[A](l: MyList[A]): MyList[A] = {
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  def foldRight[A, B](as: MyList[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  def foldLeft[A, B](as: MyList[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  def sumLeft(as: MyList[Int]) : Int = {
    foldLeft(as, 0)((b, a) => b + a)
  }

  def productLeft(as: MyList[Int]): Int = {
    foldLeft(as, 1)((b, a) => b * a)
  }

  def lengthLeft[A](as: MyList[A]): Int = {
    foldLeft(as, 0)((b, _) => b + 1)
  }

  def length[A](as: MyList[A]): Int = {
    foldRight(as, 0)((_, a) => a + 1)
  }

  def reverse[A](l: MyList[A]): MyList[A] = foldLeft(l, Nil:MyList[A])((acc, h) => Cons(h,acc))

//  def reverse[A](as: List[A]): List[A] = {
//    foldLeft(as, List[A]())((b, a) => Cons(a, b))
//  }

  def append[A](as: MyList[A], append: MyList[A]): MyList[A] = {
    foldRight(as, append)(Cons(_, _))
  }

  def addOne(as: MyList[Int]) : MyList[Int] = {
    foldRight(as, Nil:MyList[Int])((h, t) => Cons(h+1, t))
  }

  def map[A, B](as: MyList[A])(f: A => B): MyList[B] = {
    as match {
      case Nil => Nil
      case Cons(h, t) => Cons(f(h), map(t)(f))
    }
  }

  def filter[A](as: MyList[A])(f: A => Boolean): MyList[A] = {
    foldRight(as, Nil:MyList[A])(
      (h,t) => {
        if (f(h)) {
          Cons(h, t)
        } else {
          t
        }
      }
    )
  }

  def flatMap[A,B](as: MyList[A])(f: A => MyList[B]): MyList[B] = {
    as match {
      case Nil => Nil
      case Cons(h, t) => {
        append(f(h), flatMap(t)(f))
      }
    }
  }

  def addLists(l: MyList[Int], ls: MyList[Int]) : MyList[Int] = {
    (l, ls) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addLists(t1, t2))
    }
  }

  @scala.annotation.tailrec
  def hasSubsequence[A](sup: MyList[A], sub: MyList[A]): Boolean = {
    (sup, sub) match {
      case (Nil, _) => false
      case (_, Nil) => true
        case(Cons(h1, t1), Cons(h2, t2)) => {
          if (h1 != h2) {
            hasSubsequence(t1, sub)
          } else {
            hasSubsequence(t1, t2)
          }
        }
    }
  }

      def main(args: Array[String]) : Unit = {
        val cool = apply(1,2,3,4)
        val cool2 = apply(3)
        val something = hasSubsequence(cool, cool2)
        println(s"reversed is $something")
    }

}
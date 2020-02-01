sealed trait MyOption[+A] {
  def map[B](f: A => B): MyOption[B] = {
    this match {
      case MySome(x) => MySome(f(x))
      case MyNone => MyNone[B]
    }
  }
  def flatMap[B](f: A => MyOption[B]): MyOption[B] = {
    this match {
      case MySome(x) => f(x)
      case MyNone => MyNone[B]
    }
  }
  def getOrElse[B >: A](default: => B): B = {
    this match {
      case MySome(_) => _
      case MyNone => default
    }
  }
  def orElse[B >:A] (ob: => MyOption[B]): MyOption[B] = {
    this match {
      case MySome(x) => MySome(x)
      case MyNone => ob
    }

  }
  def filter(f: A => Boolean): MyOption[A] = {
    this match {
      case MySome(x) => if (f(x)) MySome(x) else MyNone
      case MyNone => MyNone
    }
  }
}

case class MySome[+A](get: A) extends MyOption[A]
case object MyNone extends MyOption[Nothing]

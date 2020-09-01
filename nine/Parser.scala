trait Parsers[Parser[+ _]] { self => // so inner classes may call methods of trait

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def char(c: Char): Parser[Char] = {
    string(c.toString).map(_.charAt(0))
  }
  def succeed[A](a: A): Parser[A] = string("").map(_ => a)
  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
    ParserOps(f(a))
  def many[A](p: Parser[A]): Parser[List[A]]
  def slice[A](p: Parser[A]): Parser[String]
  def map[A, B](a: Parser[A])(f: A => B): Parser[B]
  val numA: Parser[Int] = char('a').many.map(_.size)
  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)]
  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = {
    product(p, p2).map(f.tupled)
  }
  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B]      = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B]  = self.or(p, p2)
    def map[B](f: A => B): Parser[B]             = self.map(p)(f)
    def many: Parser[List[A]]                    = self.many(p)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
  }

  object Laws {}
}

trait JSON

object JSON {
  case object JNull                          extends JSON
  case class JNumber(get: Double)            extends JSON
  case class JString(get: String)            extends JSON
  case class JBool(get: Boolean)             extends JSON
  case class JArray(get: IndexedSeq[JSON])   extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col  = input.slice(0, offset + 1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset + n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.lines.skip(line - 1).findFirst().orElse("")
    else ""
}

case class ParseError(stack: List[(Location, String)] = List(),
                      otherFailures: List[ParseError] = List()) {}

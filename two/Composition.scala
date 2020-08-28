package two

object Composition {
  def compose[A, B, C](f: B => C, g: A => B): A => C = { (a: A) =>
    f(g(a))
  }

  def main(args: Array[String]): Unit = {
    def toInt(i: String): Int  = i.toInt
    def toFloat(s: Int): Float = s.toFloat

    val x = compose(toFloat, toInt)

    println(x("123"))
  }
}

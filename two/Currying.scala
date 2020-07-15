object Currying {
  def curry[A, B, C](f: (A, B) => C) : A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

    def compose[A, B, C](f: B => C, g: A => B): A => C = {
        (a: A) => f(g(a))
    }

    def main(args: Array[String]): Unit = {
        def add2(a: Int, b: Int) = a + b
        val addTwo = curry(add2)
        val addThree = addTwo(3)
        println(addThree(4))
    }
}

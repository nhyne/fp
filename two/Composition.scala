object Composition {
  def compose[A, B, C](f: B => C, g: A => B) : A => C = {
    (a: A) => f(g(a))
  }


    def main(args: Array[String]) : Unit = {
        println("nothing to run here")
    }
}

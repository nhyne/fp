object Currying {
  def curry[A, B, C](f: B => C, g: A => B) : A => C = {
    (a: A) => f(g(a))
  }
}

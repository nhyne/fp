package six

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  type Rand[+A] = RNG => (A, RNG)
//    type State[S, +A] = S => (A, S)
  type RandS[A] = State[RNG, A]

  case class State[S, +A](run: S => (A, S)) {
    def unit[A](a: A): State[S, A] = State(s => (a, s))

    def map[B](f: A => B): State[S, B] = {
      flatMap(a => unit(f(a)))
    }
    def map2[B, C](r: State[S, B])(f: (A, B) => C): State[S, C] = {
      flatMap(a => r.map(b => f(a, b)))
    }
    def flatMap[B](f: A => State[S, B]): State[S, B] = {
      State(s => {
        val (a, s1) = run(s)
        f(a).run(s1)
      })
    }
  }
  object State {
    def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
      def go(s: S, actions: List[State[S, A]], acc: List[A]): (List[A], S) =
        actions match {
          case Nil    => (acc.reverse, s)
          case h :: t => h.run(s) match { case (a, s2) => go(s2, t, a :: acc) }
        }
      State((s: S) => go(s, sas, List()))
    }
  }

  case class Random(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL
      val nextRNG = Random(newSeed)
      val n       = (newSeed >>> 16).toInt
      (n, nextRNG)
    }

  }

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = { rng =>
    {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }
  }

  def notNegative(rng: RNG): (Int, RNG) = {
    val (num, newRNG) = rng.nextInt
    if (num < 0) {
      if (num == Int.MinValue) (Int.MaxValue, newRNG)
      else (num * -1, newRNG)
    } else (num, newRNG)

  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
    {
      val (a, rng2) = f(rng)
      val gg        = g(a)
      gg(rng2)
    }
  }

  def mapFlat[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s) { x =>
      unit(f(x))
    }
  }

  def map2Flat[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra) { a =>
      mapFlat(rb)(b => f(a, b))
    }
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(notNegative) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List.empty[A]))((f, acc) => map2(f, acc)(_ :: _))
  }

  def double(rng: RNG): (Double, RNG) = {
    val (num, newRNG) = notNegative(rng)
    (num.toDouble / Int.MaxValue, newRNG)
  }

  def doubleMap(rng: RNG): Rand[Double] = {
    map(notNegative)(i => i.toDouble / Int.MaxValue)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (doub, rng2) = double(rng)
    val (i, rng3)    = rng2.nextInt
    ((i, doub), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (doub, rng2) = double(rng)
    val (i, rng3)    = rng2.nextInt
    ((doub, i), rng3)
  }

  def tripDouble(rng: RNG): ((Double, Double, Double), RNG) = {
    val (doub1, rng2) = double(rng)
    val (doub2, rng3) = double(rng2)
    val (doub3, rng4) = double(rng3)
    ((doub1, doub2, doub3), rng4)

  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count <= 0) (List.empty[Int], rng)
    else {
      val (i, rng2)        = rng.nextInt
      val (nextCall, rng3) = ints(count - 1)(rng2)
      (i :: nextCall, rng3)
    }
  }

  def main(args: Array[String]): Unit = {

    val rng = Random(12345)

    val (nonNeg1, rng1) = RNG.notNegative(rng)
    val (nonNeg2, rng2) = RNG.notNegative(rng1)
    val (nonNeg3, rng3) = RNG.notNegative(rng2)
    val (nonNeg4, rng4) = RNG.notNegative(rng3)

    println(s"non negatives: $nonNeg1, $nonNeg2, $nonNeg3, $nonNeg4")

    val (doub1, rng5) = RNG.double(rng4)
    val (doub2, rng6) = RNG.double(rng5)
    val (doub3, rng7) = RNG.double(rng6)
    val (doub4, rng8) = RNG.double(rng7)

    println(s"doubles: $doub1, $doub2, $doub3, $doub4")

    println(s"ints(4): ${RNG.ints(4)(rng)}")
    println(s"ints(10): ${RNG.ints(10)(rng8)}")
  }
}

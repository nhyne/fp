import Testing.Prop.{Falsified, Passed, Result}
import six.RNG

object Testing {
  type FailedCase   = String
  type SuccessCount = Int
  type TestCases    = Int

  case class SGen[+A](forSize: Int => Gen[A])

  object SGen {
    def listOf[A](g: Gen[A]): SGen[List[A]] =
      SGen(i => g.listOfN(Gen.unit(i)))
  }

  case class Prop(run: (TestCases, RNG) => Result) {
    def &&(p: Prop): Prop =
      Prop { (n, rng) =>
        {
          run(n, rng) match {
            case Passed => p.run(n, rng)
            case x      => x
          }
        }
      }

    def ||(p: Prop): Prop =
      Prop { (n, rng) =>
        {
          run(n, rng) match {
            case Falsified(f, s) => p.run(n, rng)
            case x               => x
          }
        }
      }
  }

  object Prop {

    sealed trait Result {
      def isFalsified: Boolean
    }

    case object Passed extends Result {
      override def isFalsified: Boolean = false
    }

    case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
      override def isFalsified: Boolean = true
    }

  }

  case class Gen[A](sample: RNG.State[RNG, A]) {
    def unsized: SGen[A] = SGen { _ =>
      this
    }

    def choose(start: Int, stopExclusive: Int): Gen[Int] = {
      Gen(RNG.State(RNG.notNegative).map(n => start + n % (stopExclusive - start)))
    }

    def flatMap[B](f: A => Gen[B]): Gen[B] = {
      Gen(sample.flatMap(a => f(a).sample))
    }

    def listOfN(size: Gen[Int]): Gen[List[A]] = {
      size.flatMap(n => Gen(RNG.State.sequence(List.fill(n)(sample))))
    }
  }

  object Gen {
    def unit[A](a: => A): Gen[A] = {
      Gen(RNG.State(RNG.unit(a)))
    }

    def boolean: Gen[Boolean] = {
      Gen(RNG.State(RNG.notNegative).map(_ % 2 == 0))
    }

    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
      boolean.flatMap(b => if (b) g1 else g2)
    }
  }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    Gen(RNG.State.sequence(List.fill(n)(g.sample)))
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop =
    Prop { (n, rng) =>
      randomStream(as)(rng)
        .zip(MyStream.from(0))
        .take(n)
        .map {
          case (a, i) =>
            try {
              if (f(a)) Passed else Falsified(a.toString, i)
            } catch {
              case e: Exception => Falsified(buildMsg(a, e), i)
            }
        }
        .find(_.isFalsified)
        .getOrElse(Passed)
    }

  def randomStream[A](g: Gen[A])(rng: RNG): MyStream[A] =
    MyStream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"""test case: $s
           |generated an exception: ${e.getMessage}
           |stack trace:
           |${e.getStackTrace.mkString("\n")}
           |""".stripMargin

  def main(args: Array[String]): Unit = {
    println("we're in the main method for 8")
  }
}

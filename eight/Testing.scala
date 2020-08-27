import Testing.Gen
import six.RNG

object Testing {
    type FailedCase = String
    type SuccessCount = Int

    trait Prop {
        def check: Either[(FailedCase, SuccessCount), SuccessCount]
    }

    case class Gen[A](sample: RNG.State[RNG, A]) {
        def choose(start: Int, stopExclusive: Int): Gen[Int] = {
            Gen(RNG.State(RNG.notNegative).map(n => start + n % (stopExclusive-start)))
        }

        def unit[A](a: => A): Gen[A] = {
            Gen(RNG.State(RNG.unit(a)))
        }

        def boolean: Gen[Boolean] = {
            Gen(RNG.State(RNG.notNegative).map(_ % 2 == 0))
        }

        def flatMap[B](f: A => Gen[B]): Gen[B] = {
            Gen(sample.flatMap(a => f(a).sample))
        }
        def listOfN(size: Gen[Int]): Gen[List[A]] = {
            size.flatMap(n =>
                Gen(RNG.State.sequence(List.fill(n)(sample)))
            )
        }
    }

    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
        Gen(RNG.State.sequence(List.fill(n)(g.sample)))
    }

    def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???


    def main(args: Array[String]): Unit = {
        println("we're in the main method for 8")
    }
}

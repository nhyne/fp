import six.RNG

object Testing {
    type FailedCase = String
    type SuccessCount = Int

    trait Prop {
        def check: Either[(FailedCase, SuccessCount), SuccessCount]
    }

    case class Gen[A](sample: RNG.State[RNG, A])

    def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] = ???
    def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???


    def main(args: Array[String]): Unit = {
        println("we're in the main method for 8")
    }
}

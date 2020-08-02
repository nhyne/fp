package six

trait RNG {
    def nextInt: (Int, RNG)
}


object RNG {
    type Rand[+A] = RNG => (A, RNG)

    case class Random(seed: Long) extends RNG {
        def nextInt: (Int, RNG) = {
            val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
            val nextRNG = Random(newSeed)
            val n = (newSeed >>> 16).toInt
            (n, nextRNG)
        }

    }

    def notNegative(rng: RNG): (Int, RNG) = {
        val (num, newRNG) = rng.nextInt
        if (num < 0) {
            if (num == Int.MinValue) (Int.MaxValue, newRNG)
            else (num * -1, newRNG)
        } else (num, newRNG)

    }

    def double(rng: RNG): (Double, RNG) = {
        val (num, newRNG) = notNegative(rng)
        (num.toDouble / Int.MaxValue, newRNG)
    }

    def intDouble(rng: RNG): ((Int, Double), RNG) = {
        val (doub, rng2) = double(rng)
        val (i, rng3) = rng2.nextInt
        ((i, doub), rng3)
    }

    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
        val (doub, rng2) = double(rng)
        val (i, rng3) = rng2.nextInt
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
            val (i, rng2) = rng.nextInt
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

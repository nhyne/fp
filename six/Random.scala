package six

trait RNG {
    def nextInt: (Int, RNG)
    def notNegative(rng: RNG): (Int, RNG)
    def double(rng: RNG): (Double, RNG)
}

case class Random(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
        val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
        val nextRNG = Random(newSeed)
        val n = (newSeed >>> 16).toInt
        (n, nextRNG)
    }

    override def notNegative(rng: RNG): (Int, RNG) = {
        val (num, newRNG) = rng.nextInt
        if (num < 0) {
            if (num == Int.MinValue) (Int.MaxValue, newRNG)
            else (num * -1, newRNG)
        } else (num, newRNG)

    }

    override def double(rng: RNG): (Double, RNG) = {
        val (num, newRNG) = rng.notNegative(rng)
        (num.toDouble / Int.MaxValue, newRNG)
    }
}

object Random {
    def main(args: Array[String]): Unit = {

        val rng = Random(12345)

        val (nonNeg1, rng1) = rng.notNegative(rng)
        val (nonNeg2, rng2) = rng.notNegative(rng1)
        val (nonNeg3, rng3) = rng.notNegative(rng2)
        val (nonNeg4, rng4) = rng.notNegative(rng3)

        println(s"non negatives: $nonNeg1, $nonNeg2, $nonNeg3, $nonNeg4")

        val (doub1, rng5) = rng.double(rng4)
        val (doub2, rng6) = rng.double(rng5)
        val (doub3, rng7) = rng.double(rng6)
        val (doub4, rng8) = rng.double(rng7)

        println(s"doubles: $doub1, $doub2, $doub3, $doub4")


    }
}

trait RNG {
    def nextInt: (Int, RNG)
}
case class Random(seed:Long) extends RNG {
    def nextInt: (Int, RNG) = {
        val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
        val nextRNG = Random(newSeed)
        val n = (newSeed >>> 16).toInt
        (n, nextRNG)
    }
}

object Random {
    def main(args: Array[String]): Unit = {
        println("we]re in main!")
    }
}

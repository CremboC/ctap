package problem2

object Main {
  import helpers.Helpers._

  val sboxValues = Seq(0x4, 0x0, 0xC, 0x3, 0x8, 0xB, 0xA, 0x9, 0xD, 0xE, 0x2, 0x7, 0x6, 0x5, 0xF, 0x1)
  val permboxValues = Seq(1, 5, 9, 13, 2, 6, 10, 14, 3, 7, 11, 15, 4, 8, 12, 16)

  val subkey1 = 4132
  val subkey2 = 8165
  val subkey3 = 14287
  val subkey4 = 54321
  val subkey5 = 53124
  val keys = Seq(subkey1, subkey2, subkey3, subkey4, subkey5)

  def main(args: Array[String]): Unit = {
    // i. tests
    Tests.substitution()
    Tests.permutation()

    Tests.fourRoundTestVector()
    Tests.fourRoundChallengeVector()

    // ii. cryptanalysis
    val size = 16
    val sbox = new SBox(sboxValues)
    val permBox = new PermutationBox(permboxValues)
    val heys = new Heys(size, sbox, permBox)

    // P1 xor P2 = 0001 (1)
    // O1 xor O2 = 1000 (8)

    val p1 = Integer.parseInt("00000001000000", 2)
    val out1 = heys.iteration(p1)
    val out2 = heys.iteration(out1)
    val out3 = heys.iteration(out2)
    val out4 = heys.iteration(out3)

    println(out4.toBinarySeq(size).mkString(""))



//    val outputs = (1 to Math.pow(2, size).toInt).map { heys.run(_, keys) }
//
//    val xoredPairs = for (
//      o1 <- outputs;
//      o2 <- outputs
//      if o1 != o2
//    ) yield {
//      val binaryO1 = o1.toBinarySeq(size)
//      val binaryO2 = o2.toBinarySeq(size)
//
//      binaryO1.zip(binaryO2).map(x => x._1 ^ x._2)
//    }
  }


}
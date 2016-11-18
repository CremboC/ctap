import scala.io.Source

object Problem1 extends App {
  /**
    * START GLOBAL VARIABLES
    */
  case class Register(size: Int, taps: Seq[Int])
  // 0th bit is the rightmost
  val R1 = Register(7, Seq(0, 1))
  val R2 = Register(11, Seq(0, 2))
  val R3 = Register(13, Seq(5, 2, 1, 0))

  /**
    * END GLOBAL VARIABLES
    * START MAIN BODY
    */

  test1()
  test2()
  test3()
  test4()
  challenge()

  // read in the test stream in a nice format
  val testStream: Seq[Int] = Source.fromFile("TestStream.txt").getLines().next().split(" ").map(_.toInt).toSeq
  val iterations = 2000

  /**
    * Next block of code gets the LSFR1 key
    */
  val similarities: Seq[(Double, Int)] = (1 to 2 ** R1.size).map { comb =>
    val lsfr = new LSFR(comb, R1.taps, R1.size)
    val output = for (_ <- 1 to iterations) yield lsfr.shift()

    (output.zip(testStream).count(x => x._1 == x._2) / iterations.toDouble, comb)
  }
  val r1result: Int = similarities.find(s => s._1 < 0.30 || s._1 > 0.70).map(_._2).get

  println(s"R1 key: $r1result (${r1result.paddedInt(R1.size)})")

  /**
    * Next block of code gets the LSFR2 key
    */
  // this will be used to get the LSFR2 key using the x1 XOR x2 method
  val r1stream: Seq[Int] = {
    val lsfr = new LSFR(r1result, R1.taps, R1.size)
    for (_ <- 1 to iterations) yield lsfr.shift()
  }

  val r1r2similarity: Seq[(Double, Int)] = (1 to 2 ** R2.size).map { comb =>
    val lsfr = new LSFR(comb, R2.taps, R2.size)
    val output = for (_ <- 1 to iterations) yield lsfr.shift()
    val xorStream = for (i <- r1stream.indices) yield r1stream(i) ^ output(i)
    (xorStream.zip(testStream).count(x => x._1 == x._2) / iterations.toDouble, comb)
  }

  val r2result: Int = r1r2similarity.find(x => x._1 > 0.6 || x._1 < 0.4).map(_._2).get
  println(s"R2 key: $r2result (${r2result.paddedInt(R2.size)})")

  /**
    * Next block of code gets the LSFR3 key
    */
  val r3result: Option[Int] = (1 to 2 ** R3.size).find { comb =>
    // for every combination we will create the three LSFRs in their corresponding state
    val lsfr1 = new LSFR(r1result, R1.taps, R1.size)
    val lsfr2 = new LSFR(r2result, R2.taps, R2.size)
    val lsfr3 = new LSFR(comb, R3.taps, R3.size)

    // and then shift for n amount of iterations, storing everything in a list
    val outs = for (_ <- 1 to iterations) yield {
      combineRegisterBits(lsfr1.shift(), lsfr2.shift(), lsfr3.shift())
    }

    // finally we simply do a element-wise comparison
    outs == testStream
  }

  r3result match {
    case Some(key) => println(s"R3 key: $key (${key.paddedInt(R3.size)})")
    case None => println("R3 key not found.")
  }

  /**
    * END MAIN BODY
    * START FUNCTIONS
    */

  // simplified combining function
  @inline
  def combineRegisterBits(a: Int, b: Int, c: Int): Int = 1 ^ a ^ b ^ (b & c)

  def test1(): Unit = {
    val lsfr = new LSFR(44, R1.taps, R1.size)
    val output = for (_ <- 0 until 25) yield lsfr.shift()
    require(output == "0011010010111011100110010".split("").map(_.toInt).toList, {
      s"""
         |Failed test 1
         |Expected 0011010010111011100110010
         |Got      ${output.mkString("")}
       """.stripMargin
    })
  }

  def test2(): Unit = {
    val lsfr = new LSFR(555, R2.taps, R2.size)
    val output = for (_ <- 0 until 25) yield lsfr.shift()
    require(output == "1101010001010000101000100".split("").map(_.toInt).toList, {
      s"""
         |Failed test 2
         |Expected 1101010001010000101000100
         |Got      ${output.mkString("")}
       """.stripMargin
    })
  }

  def test3(): Unit = {
    val lsfr = new LSFR(616, R3.taps, R3.size)
    val outs = for (_ <- 0 until 25) yield lsfr.shift()
    require(outs == "0001011001000101010110111".split("").map(_.toInt).toList, {
      s"""
         |Failed test 3
         |Expected 0001011001000101010110111
         |Got      ${outs.mkString("")}
       """.stripMargin
    })
  }

  def test4(): Unit = {
    val r0 = new LSFR(44, R1.taps, R1.size)
    val r1 = new LSFR(555, R2.taps, R2.size)
    val r2 = new LSFR(616, R3.taps, R3.size)

    val outs = for (_ <- 0 until 25) yield {
      combineRegisterBits(r0.shift(), r1.shift(), r2.shift())
    }

    require(outs == "0000101101010100110001101".split("").map(_.toInt).toList, {
      s"""
         |Failed overall algorithm
         |Expected 0000101101010100110001101
         |Got      ${outs.mkString("")}
       """.stripMargin
    })
  }

  def challenge(): Unit = {
    val r0 = new LSFR(97, R1.taps, R1.size)
    val r1 = new LSFR(975, R2.taps, R2.size)
    val r2 = new LSFR(6420, R3.taps, R3.size)

    val outs = for (_ <- 0 until 25) yield {
      combineRegisterBits(r0.shift(), r1.shift(), r2.shift())
    }

    println(s"Challenge Vector output: ${outs.mkString("")}")
  }

  /**
    * Source of bitwise operators is [1]
    *
    * @param state initial state of the LSFR
    * @param taps  which bits to tap
    * @param size  number of bits in this LSFR
    */
  class LSFR(var state: Int, val taps: Seq[Int], val size: Int) {
    def shift(): Int = {
      val lastBit = state & 1

      val bits = for (x <- taps) yield (state >> x) & 1
      val leftMostBit = bits.reduce(_ ^ _)

      // shift and set new left-most bit
      state = (state >> 1) + (leftMostBit << (size - 1))

      lastBit
    }
  }

  implicit class ExtendedInt(i: Int) {
    def **(pow: Int): Int = Math.pow(i, pow).toInt

    def paddedInt(size: Int): String = {
      String.format("%5s", i.toBinaryString).replace(' ', '0')
    }
  }
}
/**
  * [1] Ruten, J. (2016). How do you set, clear and toggle a single bit in C/C++?. [online] Stackoverflow.com. Available at: http://stackoverflow.com/questions/47981/how-do-you-set-clear-and-toggle-a-single-bit-in-c-c [Accessed 17 Nov. 2016].
  */
import scala.io.Source

/**
  * @author paulius
  */
object LSFRCracker {

  implicit class ExtendedInt(i: Int) {
    def toBinarySeq(length: Int): Seq[Int] = {
      i.toBinaryString.reverse.padTo(length, '0').split("").map(_.toInt).reverse
    }

    def toBoolean: Boolean = i match {
      case 0 => false
      case 1 => true
      case _ => throw new IllegalStateException()
    }
  }

  implicit class ExtendBool(b: Boolean) {
    def toInt: Int = b match {
      case true => 1
      case false => 0
    }
  }

  case class Register(size: Int, taps: Seq[Int])

  // since arrays are zero indexed, the taps are the given one minus 1.
  val R1 = Register(7, Seq(5, 6))
  val R2 = Register(11, Seq(8, 10))
  val R3 = Register(13, Seq(7, 10, 11, 12))

  def main(args: Array[String]): Unit = {
    // i.
    // runs tests first to make sure shifting is correct
    test0() // register 0 (LSFR1)
    test1() // register 1 (LSFR2)
    test2() // register 2 (LSFR3)
    test3() // test all together

    // Challenge Vector
    challengeVector()

    println()

    // ii.
    val testStream = Source.fromFile("TestStream.txt").getLines().next().split(" ").map(_.toInt)
    val iterations = 2000

    // get LSFR1 key
    val r1cracker = new LSFRCracker(lsfrSize = R1.size, taps = R1.taps, stream = testStream, iterations = iterations)
    val r1result = r1cracker.crack().get
    println(s"R1 key: ${r1result._2.toBinarySeq(R1.size).mkString("")} (${r1result._2})")
    val r1stream = r1cracker.simulateLsfr(r1result._2)

    // get LSFR2 key
    val r2cracker = new LSFRCracker(lsfrSize = R2.size, taps = R2.taps, iterations = iterations)
    val r2max = Math.pow(2, R2.size).toInt
    val r1r2similarity = (1 to r2max).par.map { comb =>
      val outs = r2cracker.simulateLsfr(comb)
      val xorStream = r1stream.zip(outs).map(i => i._1 ^ i._2)
      (xorStream.zip(testStream).count(x => x._1 == x._2) / 2000.0, comb)
    }

    // if the case is so there is a highly correlated combination
    val r2result = r1r2similarity.find(x => x._1 > 0.6 || x._1 < 0.4).map(_._2).get
    println(s"R2 key: ${r2result.toBinarySeq(R2.size).mkString("")} (${r2result})")

    // get LSFR3 key
    val r3max = Math.pow(2, R3.size).toInt

    val function = (a: Boolean, b: Boolean, c: Boolean) => {
      !((b && c) ^ a ^ b)
    }

    val systemSimilarities = (1 to r3max).par.map { comb =>
      val lsfr1 = new LSFR(r1result._2.toBinarySeq(R1.size), R1.taps)
      val lsfr2 = new LSFR(r2result.toBinarySeq(R2.size), R2.taps)
      val lsfr3 = new LSFR(comb.toBinarySeq(R3.size), R3.taps)

      val outs = for (_ <- 1 to 2000) yield {
        val r1shift = lsfr1.shift()
        val r2shift = lsfr2.shift()
        val r3shift = lsfr3.shift()

        val out = function(r1shift.out.toBoolean, r2shift.out.toBoolean, r3shift.out.toBoolean).toInt
        out
      }

      (outs.zip(testStream).count(x => x._1 == x._2) / 2000.0, comb)
    }

    systemSimilarities.maxBy(_._1) match {
      case max => println(s"R3 key: ${max._2.toBinarySeq(R3.size).mkString("")} (${max._2})")
    }
  }

  def test0(): Unit = {
    val lsfr = new LSFR(initialState = 44.toBinarySeq(R1.size), taps = R1.taps)
    val outs = for (_ <- 0 until 25) yield lsfr.shift().out
    require(outs == "0011010010111011100110010".split("").map(_.toInt).toList, {
      s"""
         |Failed test 0
         |Expected 0011010010111011100110010
         |Got ${outs.mkString("")}
       """.stripMargin
    })
  }

  def test1(): Unit = {
    val lsfr = new LSFR(initialState = 555.toBinarySeq(R2.size), taps = R2.taps)
    val outs = for (_ <- 0 until 25) yield lsfr.shift().out
    require(outs == "1101010001010000101000100".split("").map(_.toInt).toList, {
      s"""
         |Failed test 1
         |Expected 1101010001010000101000100
         |Got ${outs.mkString("")}
       """.stripMargin
    })
  }

  def test2(): Unit = {
    val lsfr = new LSFR(initialState = 616.toBinarySeq(R3.size), taps = R3.taps)
    val outs = for (_ <- 0 until 25) yield lsfr.shift().out
    require(outs == "0001011001000101010110111".split("").map(_.toInt).toList, {
      s"""
         |Failed test 2
         |Expected 0001011001000101010110111
         |Got ${outs.mkString("")}
       """.stripMargin
    })
  }

  def test3(): Unit = {
    val r0 = new LSFR(initialState = 44.toBinarySeq(R1.size), taps = R1.taps)
    val r1 = new LSFR(initialState = 555.toBinarySeq(R2.size), taps = R2.taps)
    val r2 = new LSFR(initialState = 616.toBinarySeq(R3.size), taps = R3.taps)

    val function = (a: Boolean, b: Boolean, c: Boolean) => {
      !((b && c) ^ a ^ b)
    }

    val outs = for (_ <- 0 until 25) yield {
      val r0shift = r0.shift()
      val r1shift = r1.shift()
      val r2shift = r2.shift()

      val out = function(r0shift.out.toBoolean, r1shift.out.toBoolean, r2shift.out.toBoolean).toInt
      out
    }

    require(outs == "0000101101010100110001101".split("").map(_.toInt).toList, {
      s"""
         |Failed overall algorithm
         |Expected 0000101101010100110001101
         |Got ${outs.mkString("")}
       """.stripMargin
    })
  }

  def challengeVector(): Unit = {
    val r0 = new LSFR(initialState = 97.toBinarySeq(R1.size), taps = R1.taps)
    val r1 = new LSFR(initialState = 975.toBinarySeq(R2.size), taps = R2.taps)
    val r2 = new LSFR(initialState = 6420.toBinarySeq(R3.size), taps = R3.taps)

    val function = (a: Boolean, b: Boolean, c: Boolean) => {
      !((b && c) ^ a ^ b)
    }

    val outs = for (_ <- 0 until 25) yield {
      val r0shift = r0.shift()
      val r1shift = r1.shift()
      val r2shift = r2.shift()

      val out = function(r0shift.out.toBoolean, r1shift.out.toBoolean, r2shift.out.toBoolean).toInt
      out
    }

    println(s"Challenge Vector output: ${outs.mkString("")}")
  }
}

/**
  * Provides helper methods to crack an LSFR
  * @param lsfrSize size of the LSFR
  * @param iterations how many iterations (shifts) should be simulated
  * @param taps the indices of which bits should be XORed
  * @param stream stream to compare to if cracked via easy method
  */
class LSFRCracker(val lsfrSize: Int,
                  val iterations: Int,
                  val taps: Seq[Int],
                  val stream: Seq[Int] = Seq()) {

  import LSFRCracker.ExtendedInt

  /**
    * Simulate an LSFR for the pre-specified number of shifts start the given state
    * @param state the state the LSFR will start at
    * @return stream of outputs bits
    */
  def simulateLsfr(state: Int): Seq[Int] = {
    val lsfr = new LSFR(initialState = state.toBinarySeq(lsfrSize), taps = taps)
    val outs = for (_ <- 1 to iterations) yield lsfr.shift().out
    outs
  }

  /**
    * Use the easy way of cracking the LSFR by checking its output stream correlation to the given stream
    * @return maybe a tuple of (correlation, combination), if found, else None
    */
  def crack(): Option[(Double, Int)] = {
    val max = Math.pow(2, lsfrSize).toInt
    val similarities = (1 to max).map { comb =>
      val outs = simulateLsfr(comb)
      (outs.zip(stream).count(x => x._1 == x._2) / iterations.toDouble, comb)
    }

    similarities.find(s => s._1 < 0.30 || s._1 > 0.70)
  }
}

/**
  * Represents an LSFR
  * @param initialState starting state
  * @param taps indices of bits that will be XOR'd to produce the next left-most bit
  */
class LSFR(private val initialState: Seq[Int],
           private val taps: Seq[Int]) {

  case class State(out: Int, nextState: Seq[Int])

  private var state: Seq[Int] = initialState

  /**
    * Shift the LSFR once
    * @return the produced left-most bit and the next state
    */
  def shift(): State = {
    val newEnd = state.head +: state.tail.init
    val lastBitOut = state.last
    val leftMost = state.zipWithIndex.filter(s => taps.contains(s._2)).map(_._1).sum % 2

    state = leftMost +: newEnd
    State(lastBitOut, state)
  }
}

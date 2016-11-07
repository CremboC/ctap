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

  def main(args: Array[String]): Unit = {
    // i.
    // runs tests first to make sure shifting is correct
    test0() // register 0
    test1() // register 1
    test2() // register 2

    test4() // test all together

    // Challenge Vector
    challengeVector()

    // ii.
    val testStream = Source.fromFile("TestStream.txt").getLines().next().split(" ").map(_.toInt)

    val r0cracker = new LSFRCracker(lsfrSize = 7, taps = Seq(5, 6), stream = testStream, iterations = 2000)
    val r0result = r0cracker.crack(s => s < 0.30 || s > 0.70)
    println(r0result)

    val r1cracker = new LSFRCracker(lsfrSize = 11, taps = Seq(8, 10), stream = testStream, iterations = 2000)
    val r1result = r1cracker.crack(s => s < 0.30 || s > 0.70)
    println(r1result)

    val r2cracker = new LSFRCracker(lsfrSize = 13, taps = Seq(7, 10, 11, 12), stream = testStream, iterations = 2000)
    val r2result = r2cracker.crack(s => s < 0.30 || s > 0.70)
    println(r2result)
  }

  def test0(): Unit = {
    val lsfr = new LSFR(initialState = 44.toBinarySeq(7), taps = Seq(5, 6))
    val outs = for (_ <- 0 until 25) yield {
      val shifting = lsfr.shift()
      shifting.out
    }
    require(outs == "0011010010111011100110010".split("").map(_.toInt).toList, {
      s"""
         |Failed test 0
         |Expected 0011010010111011100110010
         |Got ${outs.mkString("")}
       """.stripMargin
    })
  }

  def test1(): Unit = {
    val lsfr = new LSFR(initialState = 555.toBinarySeq(11), taps = Seq(8, 10))
    val outs = for (_ <- 0 until 25) yield {
      val shifting = lsfr.shift()
      shifting.out
    }
    require(outs == "1101010001010000101000100".split("").map(_.toInt).toList, {
      s"""
         |Failed test 1
         |Expected 1101010001010000101000100
         |Got ${outs.mkString("")}
       """.stripMargin
    })
  }

  def test2(): Unit = {
    val lsfr = new LSFR(initialState = 616.toBinarySeq(13), taps = Seq(7, 10, 11, 12))
    val outs = for (_ <- 0 until 25) yield {
      val shifting = lsfr.shift()
      shifting.out
    }
    require(outs == "0001011001000101010110111".split("").map(_.toInt).toList, {
      s"""
         |Failed test 2
         |Expected 0001011001000101010110111
         |Got ${outs.mkString("")}
       """.stripMargin
    })
  }

  def test4(): Unit = {
    val r0 = new LSFR(initialState = 44.toBinarySeq(7), taps = Seq(5, 6))
    val r1 = new LSFR(initialState = 555.toBinarySeq(11), taps = Seq(8, 10))
    val r2 = new LSFR(initialState = 616.toBinarySeq(13), taps = Seq(7, 10, 11, 12))

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
    val r0 = new LSFR(initialState = 97.toBinarySeq(7), taps = Seq(5, 6))
    val r1 = new LSFR(initialState = 975.toBinarySeq(11), taps = Seq(8, 10))
    val r2 = new LSFR(initialState = 6420.toBinarySeq(13), taps = Seq(7, 10, 11, 12))

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

class LSFRCracker(val lsfrSize: Int,
                  val taps: Seq[Int],
                  val stream: Seq[Int],
                  val iterations: Int) {
  import LSFRCracker.ExtendedInt

  def crack(similaritiesChecker: (Double) => Boolean): Option[(Double, Int)] = {
    val max = Math.pow(2, lsfrSize).toInt
    val combinations = 1 until max

    val similarities = combinations.map { comb =>
      val lsfr = new LSFR(initialState = comb.toBinarySeq(lsfrSize), taps = taps)
      val outs = for (_ <- 1 to iterations) yield {
        val shifting = lsfr.shift()
        shifting.out
      }
      outs.zip(stream).count(x => x._1 == x._2) / iterations.toDouble
    }

    similarities.zipWithIndex.find(x => similaritiesChecker(x._1))
  }
}

class LSFR(val initialState: Seq[Int],
           private val taps: Seq[Int]) {

  case class State(out: Int, nextState: Seq[Int])

  private var state: Seq[Int] = initialState
  private var lastBitOut: Int = 0

  val length: Int = initialState.size

  def lastOut: Int = lastBitOut
  def currentState: Seq[Int] = state

  def shift(): State = {
    val newEnd = state.head +: state.tail.init
    lastBitOut = state.last
    val leftMost = state.zipWithIndex.filter(s => taps.contains(s._2)).map(_._1).sum % 2

    state = leftMost +: newEnd
    State(lastBitOut, state)
  }
}

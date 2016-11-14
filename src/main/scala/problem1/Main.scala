package problem1

import scala.annotation.tailrec
import scala.io.Source

object Main {

  import helpers.Helpers.ExtendedBool
  import helpers.Helpers.ExtendedInt

  case class Register(size: Int, taps: Seq[Int])

  // since arrays are zero indexed, the taps are the given one minus 1.
  val R1 = Register(7, Seq(5, 6))
  val R2 = Register(11, Seq(8, 10))
  val R3 = Register(13, Seq(7, 10, 11, 12))

  // simplified combining function
  @inline
  def combineRegisterBits(a: Int, b: Int, c: Int): Int = 1 ^ a ^ b ^ (b & c)

  /**
    * Main starting point
    */
  def main(args: Array[String]): Unit = {
    // i.
    // runs tests first to make sure shifting is correct
    Tests.test0() // register 1 (LSFR1)
    Tests.test1() // register 2 (LSFR2)
    Tests.test2() // register 3 (LSFR3)
    Tests.test3() // test all together

    println("Tests completed.\n")

    // Challenge Vector
    Tests.challengeVector()

    // ii.
    val testStream = Source.fromFile("TestStream.txt").getLines().next().split(" ").map(_.toInt).toSeq
    val iterations = 2000

    /**
      * Next block of code gets the LSFR1 key
      */
    val r1breaker = new LSFRBreaker(lsfrSize = R1.size, taps = R1.taps, stream = testStream, iterations = iterations)
    val r1result = r1breaker.break().get
    println(s"R1 key: ${r1result._2.toBinarySeq(R1.size).mkString("")} (${r1result._2})")

    /**
      * Next block of code gets the LSFR2 key
      */
    // this will be used to get the LSFR2 key using the x1 XOR x2 method
    val r1stream = r1breaker.simulateLsfr(r1result._2)

    val r2cracker = new LSFRBreaker(lsfrSize = R2.size, taps = R2.taps, iterations = iterations)
    val r1r2similarity = (1 to 2 ** R2.size).par.map { comb =>
      val outs = r2cracker.simulateLsfr(comb)
      val xorStream = for (i <- r1stream.indices) yield r1stream(i) ^ outs(i)
      (xorStream.zip(testStream).count(x => x._1 == x._2) / iterations.toDouble, comb)
    }

    // if the case is so there is a highly correlated combination
    val r2result = r1r2similarity.find(x => x._1 > 0.6 || x._1 < 0.4).map(_._2).get
    println(s"R2 key: ${r2result.toBinarySeq(R2.size).mkString("")} (${r2result})")

    /**
      * Next block of code gets the LSFR3 key
      */
    // cache the initial state of LSFR1 and LSFR2 in order to avoid Int => Seq[Int] conversion
    // every time we try an LSFR3 combination
    val lsfr1InitialState = r1result._2.toBinarySeq(R1.size)
    val lsfr2InitialState = r2result.toBinarySeq(R2.size)

    // find the first LSFR3 combination that matches the provided output stream
    val r3result = (1 to 2 ** R3.size).par.find { comb =>
      // for every combination we will create the three LSFRs in their corresponding state
      val lsfr1 = new LSFR(lsfr1InitialState, R1.taps)
      val lsfr2 = new LSFR(lsfr2InitialState, R2.taps)
      val lsfr3 = new LSFR(comb.toBinarySeq(R3.size), R3.taps)

      // and then shift for n amount of iterations, storing everything in a list
      val outs = for (_ <- 1 to iterations) yield {
        combineRegisterBits(lsfr1.shift(), lsfr2.shift(), lsfr3.shift())
      }

      // finally we simply do a element-wise comparison
      outs == testStream
    }

    r3result match {
      case Some(key) => println(s"R3 key: ${key.toBinarySeq(R3.size).mkString("")} ($key)")
      case None => println("R3 key not found.")
    }
  }
}

package problem2

import sun.net.www.content.text.plain

import scala.collection.mutable
import scala.io.Source

object Main {

  import helpers.Helpers.{ExtendedInt, ExtendedSeq, StringToNumber}

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
    val regexp =
      """([0-9]+)  ([0-9]+)""".r
    val givenCiphers = Source.fromFile("block.txt").getLines().toSeq.map { s =>
      val regexp(plain, cipher) = s
      plain.toInt -> cipher.toInt
    }.toMap

    val size = 16
    val sbox = new SBox(sboxValues)
    val permBox = new PermutationBox(permboxValues)
    val heys = new Heys(size, sbox, permBox)

    @inline
    def partialDecrypt(cipher: Int): Seq[Int] = {
      val o1bin = cipher.toBinarySeq(size)
      val parts = for (i <- 0 until size by 4) yield {
        o1bin.view(i, i + 4).toInt
      }

      val substituted = for (i <- 0 until 4) yield {
        sbox.reverseSubstitute(parts(i))
      }

      substituted
    }

    def compareToExpected(result: Seq[Int], expected: Seq[Int]): Boolean = {
      result.head == expected.head && result(2) == expected(2) && result(3) == expected(3)
    }

    val dU = "0110000010000110".bin.toBinarySeq(size).grouped(4).map(_.toInt).toSeq
    val dP = "110000000000".bin

    val acc = mutable.Map[Int, Int]().par
    val keys = for (
      left <- 0 until 2 ** 4;
      part34 <- 0 until 2 ** 8
    ) yield (left.toBinarySeq(4) ++ Seq(0, 0, 0, 0) ++ part34.toBinarySeq(8)).toInt

    require(keys.size == 2 ** 12, {
      s"""
         |Keys should be ${2 ** 12}, got ${keys.size}
       """.stripMargin
    })

    var plaintextTests = 0

    (0 until 2 ** 16).par.foreach { p1 =>
      val p2 = p1 ^ dP
      if (p2 < p1) {
        for (key <- keys) {
          val o1 = givenCiphers(p1)
          val pdo1 = partialDecrypt(heys.subkeyMix(o1, key))

          val o2 = givenCiphers(p2)
          val pdo2 = partialDecrypt(heys.subkeyMix(o2, key))

          val diff = pdo1.binaryDifferenceTo(pdo2) // 4. XOR
          if (compareToExpected(diff, dU)) {
            acc.get(key) match {
              case None => acc.put(key, 1)
              case Some(v) =>
                acc.put(key, v + 1)
            }
          }
        }

        plaintextTests += 1
        if (plaintextTests % 100 == 0) {
          println(plaintextTests / ((2 ** 16).toDouble / 2))
        }
      }
    }

//    println(acc)


//    for () yield

//    val pp = for (
//      i <- 0 until 2 ** 4;
//      j <- 0 until 2 ** 4
//    ) yield {
//      s"${i.toBinarySeq(4).mkString("")}0000${i.toBinarySeq(4).mkString("")}0000".bin
//    }


//    val ddU = "0000100000000000".grouped(4).map(_.bin).toSeq
////      .bin.toBinarySeq(size).grouped(4).map(_.toInt).toSeq
//
//    val ddP = "0000110100001101".bin
//
////    (0 until 2 ** 16).map(_ ^ ddP)
////    val pp = (0 until 2 ** 16).collect { case x if (x ^ ddP) < x => x ^ ddP}
//
////    println(pp)
//
//    val keys2 = for (
//      bits <- 0 until 2 ** 4
//    ) yield ("0000" + bits.toBinarySeq(4).mkString("") + "00000000").bin
//
//    def compareToExpected2(result: Seq[Int], expected: Seq[Int]): Boolean = {
//      result(1) == expected(1)
//    }
//
//    (0 until 2 ** 16).par.foreach { p1 =>
//      val p2 = p1 ^ dP
//
//      for (key <- keys2) {
//        val o1 = givenCiphers(p1)
//        val pdo1 = partialDecrypt(heys.subkeyMix(o1, key))
//
//        val o2 = givenCiphers(p2)
//        val pdo2 = partialDecrypt(heys.subkeyMix(o2, key))
//
//        val diff = pdo1.binaryDifferenceTo(pdo2) // 4. XOR
//        if (compareToExpected2(diff, ddU)) {
//          acc.get(key) match {
//            case None => acc.put(key, 1)
//            case Some(v) =>
//              acc.put(key, v + 1)
//          }
//        }
//      }
//
//      plaintextTests += 1
//      if (plaintextTests % 100 == 0) {
//        println(plaintextTests.toDouble / (2 ** 16).toDouble)
//      }
//    }
//
//    println(acc)

    acc.map { case (key, value) => (key, value.toDouble) }.toSeq.seq.sortBy(_._2).foreach { case (key, value) => println(s"$key -> " + "%f".format(value)) }

//    val p = for (i <- 0 until 2 ** 12) yield s"0000${i.toBinarySeq(12).mkString("")}".bin
  }


}
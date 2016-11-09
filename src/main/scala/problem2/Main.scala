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

    //    val period = 0 until Math.pow(2, 4).toInt
    //    val matchingInputs = for (
    //      o1 <- period;
    //      o2 <- period
    //      if o1 < o2
    //      if o1.toBinarySeq(4).binaryDifferenceTo(o2.toBinarySeq(4)).toInt == "1100".bin
    //    ) yield (o1, o2)

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

    val dU = "0110000010000110".bin.toBinarySeq(size).grouped(4).map(_.toInt).toSeq

    def compareToExpected(result: Seq[Int]): Boolean = {
      result.head == dU.head && result(2) == dU(2) && result(3) == dU(3)
    }

    //    println(matchingInputs.size)

    println(java.lang.Long.parseLong("1100000000000000", 2))
    println(Integer.parseInt("1100000000000000", 2))

    val r = for (i <- 0 until 2 ** 12) yield s"1100${i.toBinarySeq(12).mkString("")}".bin
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
    val plaintextSize = r.size

    r.par.foreach { p1 =>
      val p2 = p1 ^ dP

      for (key <- keys) {
        val o1 = givenCiphers(p1)
        val pdo1 = partialDecrypt(heys.subkeyMix(o1, key))

        val o2 = givenCiphers(p2)
        val pdo2 = partialDecrypt(heys.subkeyMix(o2, key))

        val diff = pdo1.binaryDifferenceTo(pdo2) // 4. XOR
        if (compareToExpected(diff)) {
          acc.get(key) match {
            case None => acc.put(key, 1)
            case Some(v) =>
              acc.put(key, v + 1)
//              acc(key) = v + 1
          }
        }
      }

      plaintextTests += 1
      if (plaintextTests % 100 == 0) {
        println(plaintextTests / plaintextSize.toDouble)
      }
    }

    println(acc)


    //    println(heys.substitute("1100000000000000".bin))
    //    val validPairs2 = validPairs.take(validPairs.size / 2).filter {
    //      case (o1, o2) =>
    //        val o1bin = heys.permute(o1)
    //        val o2bin = heys.permute(o2)
    //        val p1 = s"00000000${o1bin}0000".bin
    //        val p2 = s"00000000${o2bin}0000".bin
    //
    //        val perm1 = heys.substitute(o1bin)
    //        val perm2 = heys.substitute(o2bin)
    //
    //        perm1.toBinarySeq(16).binaryDifferenceTo(perm2.toBinarySeq(16)).mkString("") == "0000000001100000"
    //    }
    //
    //    println(validPairs2)


    //    val p1 = "0000001000000000".bin
    //    val p2 = "0000100100000000".bin
    //
    //    val o1 = heys.permute(heys.substitute(p1))
    //    val o2 = heys.permute(heys.substitute(p2))
    //
    //
    //
    //    println(o1.toBinarySeq(16).mkString(""))
    //    println(o2.toBinarySeq(16).mkString(""))
    //    println(s"Diff: ${o1.toBinarySeq(16).binaryDifferenceTo(o2.toBinarySeq(16)).mkString("")}")
    //    println(s"Expc: 0000001000000000")
    //    val binarySeq = p1.toBinarySeq(size)
    //    val result1 = sbox.substitute(binarySeq.view(0, 4).toInt)
    //    val result2 = sbox.substitute(binarySeq.view(4, 8).toInt)
    //    val result3 = sbox.substitute(binarySeq.view(8, 12).toInt)
    //    val result4 = sbox.substitute(binarySeq.view(12, 16).toInt)
    //
    //    val output = result1.toBinarySeq(4) ++ result2.toBinarySeq(4) ++ result3.toBinarySeq(4) ++ result4.toBinarySeq(4)
    //
    //    println(output.mkString(""))


    //    val out1 = heys.iteration(p1)
    //    println(out1.toBinarySeq(size).mkString(""))

    //    val out2 = heys.iteration(out1)
    //    val out3 = heys.iteration(out2)
    //    val out4 = heys.iteration(out3)
    //    val out4permuted = permBox.permute(out4)
    //    println(out4.toBinarySeq(size).mkString(""))


  }


}
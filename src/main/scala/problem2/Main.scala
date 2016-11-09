package problem2

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

  def crack(sdP: String, sdU: String, setNumber: Int) = {

    require(sdP.length == size || sdP.length == size + 3, {
      s"deltaP must be of size $size or ${size + 3}"
    })

    require(sdU.length == size || sdU.length == size + 3, {
      s"deltaU must be of size $size or ${size + 3}"
    })

    def compareToExpected(result: Seq[Int], expected: Seq[Int]): Boolean = {
      result(setNumber) == expected(setNumber)
    }

    val acc = mutable.Map[Int, Int]().par

    val dP = sdP.replaceAll(" ", "").bin
    val dU = sdU.replaceAll(" ", "").grouped(4).map(_.bin).toSeq

    val keys = for (bits <- 0 until 2 ** 4) yield {
      val asString = bits.toBinarySeq(4).mkString("")
      val key = setNumber match {
        case 0 => asString + ("0" * 12)
        case 1 => ("0" * 4) + asString + ("0" * 8)
        case 2 => ("0" * 8) + asString + ("0" * 4)
        case 3 => ("0" * 12) + asString
        case _ => throw new IllegalStateException()
      }
      key.bin
    }

//    keys.map(_.toBinarySeq(16).grouped(4).map(_.mkString("")).mkString(" "))

    (0 until 2 ** 16).par
      .map(p1 => (p1, p1 ^ dP))
//      .filter(p => p._1 < p._2)
      .foreach { case (p1, p2) =>
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
      }

    acc
      .map { case (key, value) => (key, value.toDouble / (2 ** 16).toDouble) }
      .toSeq.seq
      .sortBy(_._2)
      .foreach { case (key, value) => println(s"$key -> " + "%f".format(value)) }

    acc.maxBy(_._1)._1
  }

  def main(args: Array[String]): Unit = {
    // i. tests
    Tests.substitution()
    Tests.permutation()

    Tests.fourRoundTestVector()
    Tests.fourRoundChallengeVector()

    // ii. cryptanalysis

    println("\nCracking 0")
    val subkey0 = crack("0000 0101 0000 0000", "0110 0000 0000 0000", 0) // crack 1st -- 1101
    println("\nCracking 1")
    val subkey1attempt1 = crack("0000 0000 1101 0000", "0000 1001 0000 0000", 1) // crack 2nd -- 1101
//    val subkey1attempt2 = crack("0000 0000 1101 0000", "0000 0010 0000 0000", 1) // crack 2nd -- 1101
//    val subkey1attempt3 = crack("0000 0000 0110 0110", "0000 0001 0000 0000", 1) // crack 2nd -- 1101
//    val subkey1attempt4 = crack("0000 0000 0000 0110", "0000 0100 0000 0000", 1) // crack 2nd -- 1101
    println("\nCracking 2")
    val subkey2 = crack("0010 0000 0000 0000", "0000 0000 0100 0000", 2) // crack 3rd -- 1101
    println("\nCracking 3")
    val subkey3 = crack("0001 0000 0000 0000", "0000 0000 0000 1110", 3) // crack 4th -- 0101

    println("\nFinal key:")
    val finalSubkey = subkey0.toBinarySeq(16).view(0, 4) ++
      subkey1attempt1.toBinarySeq(16).view(4, 8) ++
      subkey2.toBinarySeq(16).view(8, 12) ++
      subkey3.toBinarySeq(16).view(12, 16)

    println(finalSubkey.grouped(4).map(_.mkString("")).mkString(" "))


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


    //    val p = for (i <- 0 until 2 ** 12) yield s"0000${i.toBinarySeq(12).mkString("")}".bin
  }
}
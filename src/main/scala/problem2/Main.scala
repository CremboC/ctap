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
    println(s"\nCracking $setNumber with dP = $sdP; dU = $sdU")

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

    acc.maxBy(_._2)._1
  }

  def main(args: Array[String]): Unit = {
    // i. tests
    Tests.substitution()
    Tests.permutation()

    Tests.fourRoundTestVector()
    Tests.fourRoundChallengeVector()

    // ii. cryptanalysis

    println("\nCracking 0")
    val subkey0Guesses = Seq(
      crack("0000 1100 0000 0000", "0110 0000 0000 0000", 0), // crack 1st (1101 at 0.088, 1010 at 0.097); 0.005 overall
      crack("0000 0000 1100 0000", "1000 0000 0000 0000", 0), // crack 1st (1101 at 0.117); 0.0029 overall
      crack("0000 0000 0101 0000", "0010 0000 0000 0000", 0) // crack 1st (1101 at 0.17); 0.015 overall
    )
    println("\nCracking 1")
    val subkey1Guesses = Seq(
      crack("0000 0000 1101 0000", "0000 1001 0000 0000", 1), // crack 2nd (1101 at 0.082);
      crack("0000 0000 1101 0000", "0000 0010 0000 0000", 1), // crack 2nd (1101 at 0.125);
      crack("0000 0000 0110 0110", "0000 0001 0000 0000", 1), // crack 2nd (1101 at 0.102);
      crack("0000 0000 0000 0110", "0000 0100 0000 0000", 1) // crack 2nd (1101 at 0.092);
    )
    println("\nCracking 2")
    val subkey2Guesses = Seq(
      crack("0010 0000 0000 0000", "0000 0000 0100 0000", 2), // crack 3rd (1101 at 0.139); overall ??
      crack("0000 0000 0010 0000", "0000 0000 0010 0000", 2), // crack 3rd (1101 at 0.151); 0.015 overall
      crack("0000 0000 0010 0010", "0000 0000 0001 0000", 2), // crack 3rd (1101 at 0.100); 0.0039 overall
      crack("0000 0010 0000 0010", "0000 0000 1000 0000", 2) // crack 3rd (1101 at 0.094); 0.0078 overall
    )
    println("\nCracking 3")
    val subkey3Guesses = Seq(
      crack("0000 0000 0001 0000", "0000 0000 0000 0010", 3), // crack 4th (0101 at 0.123); 0.0078 overall
      crack("0000 0001 0000 0001", "0000 0000 0000 1000", 3) // crack 4th (0101 at 0.107); 0.0078 overall
    )

    val subkey0BestGuess = subkey0Guesses.max
    val subkey1BestGuess = subkey1Guesses.max
    val subkey2BestGuess = subkey2Guesses.max
    val subkey3BestGuess = subkey3Guesses.max

    println(subkey0BestGuess, subkey1BestGuess, subkey2BestGuess, subkey3BestGuess)

    println("\nFinal key:")
    val finalSubkey = subkey0BestGuess.toBinarySeq(16).view(0, 4) ++
      subkey1BestGuess.toBinarySeq(16).view(4, 8) ++
      subkey2BestGuess.toBinarySeq(16).view(8, 12) ++
      subkey3BestGuess.toBinarySeq(16).view(12, 16)

    println(finalSubkey.grouped(4).map(_.mkString("")).mkString(" "))

  }
}
import scala.collection.mutable
import scala.io.Source
import scala.util.matching.Regex

/**
  * Source of
  */
object Problem2 extends App {

  /**
    * START GLOBAL VARIABLES
    */

  val sboxValues = Seq(0x4, 0x0, 0xC, 0x3, 0x8, 0xB, 0xA, 0x9, 0xD, 0xE, 0x2, 0x7, 0x6, 0x5, 0xF, 0x1)
  val reverseSboxValues: Seq[Int] = sboxValues.zipWithIndex.sortBy(_._1).map {
    case (_, index) => index
  }
  val permboxValues = Seq(1, 5, 9, 13, 2, 6, 10, 14, 3, 7, 11, 15, 4, 8, 12, 16)

  val subkey1 = 4132
  val subkey2 = 8165
  val subkey3 = 14287
  val subkey4 = 54321
  val subkey5 = 53124
  val subkeys = Seq(subkey1, subkey2, subkey3, subkey4, subkey5)

  val regexp: Regex = """([0-9]+)  ([0-9]+)""".r
  val givenCiphers: Map[Int, Int] = Source.fromFile("block.txt").getLines().toSeq.map { s =>
    val regexp(plain, cipher) = s
    plain.toInt -> cipher.toInt
  }.toMap

  /**
    * END GLOBAL VARIABLES
    * START MAIN BODY
    */
  permuteTest(61440, 34952)
  permuteTest(3840, 17476)
  permuteTest(240, 8738)
  permuteTest(15, 4369)
  permuteTest(23130, 23130)

  substituteWholeTest(291, 16579)
  substituteWholeTest(17767, 35753)
  substituteWholeTest(33623, 54201)
  substituteWholeTest(30001, 39728)
  substituteWholeTest(23130, 45746)

  heysTest(12033, 20025)
  heysTest(62153, 7495)

  val challengeSubkeys = Seq(4132, 8165, 14287, 54321, 53124)
  println(s"Challenge output: ${heys(13571, challengeSubkeys)}")

  println("Cracking 0")
  val subkey0Guesses = Seq(
    break("0000 1100 0000 0000", "0110 0000 0000 0000", 0), // break 1st (1101 at 0.088, 1010 at 0.097); 0.0058 overall
    break("0000 0000 1100 0000", "1000 0000 0000 0000", 0), // break 1st (1101 at 0.117); 0.0029 overall
    break("0000 0000 0010 0000", "0010 0000 0000 0000", 0) // break 1st (1101 at 0.149); 0.0078 overall
  )
  println("Cracking 1")
  val subkey1Guesses = Seq(
    break("0000 0000 1101 0000", "0000 1001 0000 0000", 1), // break 2nd (1101 at 0.082); 0.0014 overall
    break("0000 0000 1101 0000", "0000 0010 0000 0000", 1), // break 2nd (1101 at 0.125); 0.00585 overall
    break("0000 0000 0110 0110", "0000 0001 0000 0000", 1), // break 2nd (1101 at 0.102); 0.0019 overall
    break("0000 0000 0000 0110", "0000 0100 0000 0000", 1) // break 2nd (1101 at 0.092); 0.0009 overall
  )
  println("Cracking 2")
  val subkey2Guesses = Seq(
    break("0010 0000 0000 0000", "0000 0000 0100 0000", 2), // break 3rd (1101 at 0.139); 0.0078 overall
    break("0000 0000 0010 0000", "0000 0000 0010 0000", 2), // break 3rd (1101 at 0.151); 0.0156 overall
    break("0000 0000 0010 0010", "0000 0000 0001 0000", 2), // break 3rd (1101 at 0.100); 0.0039 overall
    break("0000 0010 0000 0010", "0000 0000 1000 0000", 2) // break 3rd (1101 at 0.094); 0.00781 overall
  )
  println("Cracking 3")
  val subkey3Guesses = Seq(
    break("0000 0000 0001 0000", "0000 0000 0000 0010", 3), // break 4th (0101 at 0.123); 0.00781 overall
    break("0000 0001 0000 0001", "0000 0000 0000 1000", 3) // break 4th (0101 at 0.107); 0.015625 overall
  )

  val subkey0BestGuess: Int = subkey0Guesses.max
  println(s"Partial subkey 0: $subkey0BestGuess")
  val subkey1BestGuess: Int = subkey1Guesses.max
  println(s"Partial subkey 1: $subkey1BestGuess")
  val subkey2BestGuess: Int = subkey2Guesses.max
  println(s"Partial subkey 2: $subkey2BestGuess")
  val subkey3BestGuess: Int = subkey3Guesses.max
  println(s"Partial subkey 3: $subkey3BestGuess")

  val fullKey: Int = subkey0BestGuess + subkey1BestGuess + subkey2BestGuess + subkey3BestGuess
  println(s"Final subkey K5: ${fullKey.paddedInt(16)}")

  /**
    * END MAIN BODY
    * START FUNCTIONS
    */

  def bin(string: String): Int = Integer.parseInt(string.replace(" ", ""), 2)

  /**
    * Attempt to break some partial subkey
    * @param sdP delta P
    * @param sdU delta U
    * @param setNumber which partial subkey (0 -- left-most bits)
    * @return best-guess partial subkey
    */
  def break(sdP: String, sdU: String, setNumber: Int): Int = {
    val dP = bin(sdP)
    val dU = bin(sdU)

    // depending which partial subkey we attack, the shifting of the bits
    // changes
    val shifting = setNumber match {
      case 0 => 12
      case 1 => 8
      case 2 => 4
      case 3 => 0
    }

    val acc = mutable.Map[Int, Int]()
    val keys = for (bits <- 0 until 2 ** 4) yield bits << shifting

    def compareToExpected(result: Int, expected: Int): Boolean = {
      ((result >> shifting) & 0xFF) == ((expected >> shifting) & 0xFF)
    }

    // for every plaintext we find its corresponding pair
    // then test every possible subkey (as generated earlier)
    // by running the last round of encryption in reverse
    // and then comparing it to the expected deltaU
    (0 until 2 ** 16)
      .map(p1 => (p1, p1 ^ dP))
      .foreach { case (p1, p2) =>
        for (key <- keys) {
          val o1 = givenCiphers(p1)
          val pdo1 = substituteWhole(o1 ^ key, reverseSboxValues)

          val o2 = givenCiphers(p2)
          val pdo2 = substituteWhole(o2 ^ key, reverseSboxValues)

          val diff = pdo1 ^ pdo2 // 4. XOR
          if (compareToExpected(diff, dU)) {
            acc.get(key) match {
              case None => acc.put(key, 1)
              case Some(v) => acc.put(key, v + 1)
            }
          }
        }
      }

    // finally choose the best match
    acc.maxBy(_._2)._1
  }

  /**
    * Run Hey's cipher
    * @param input plaintext
    * @param subkeys subkeys used in different rounds. Determines number of rounds
    * @return encrypted text
    */
  def heys(input: Int, subkeys: Seq[Int]): Int = {
    // perform the rounds;
    // number of rounds is the number of keys minus one since
    // the last key is used in a separate round
    val output = (0 until subkeys.size - 1).foldLeft(input) {
      case (carry, round) =>
        val mixed = carry ^ subkeys(round)
        val substituted = substituteWhole(mixed, sboxValues)
        val permuted = permute(substituted, permboxValues)
        permuted
    }

    // and the last stage: final encryption
    permute(output, permboxValues) ^ subkeys.last
  }

  /**
    * Helper to test Hey's ciper
    */
  def heysTest(plaintext: Int, expected: Int): Unit = {
    val encrypted = heys(plaintext, subkeys)
    require(encrypted == expected, {
      s"$encrypted == $expected"
    })
  }

  /**
    * Permutation stage of Heys' cipher
    */
  def permute(number: Int, boxValues: Seq[Int]): Int = {
    val numbers = for (i <- boxValues.indices) yield {
      // get the ith bit; 15 - i because the 0th bit is the right-most
      val bit = (number >> 15 - i) & 1
      // and set it's new location; - 1 since the values are 1-indexed
      (bit, boxValues(i) - 1)
    }

    val permuted = numbers.sortBy(_._2).map(_._1).mkString("")
    Integer.parseInt(permuted, 2)
  }

  /**
    * Helper to test the permutation stage of the cipher
    */
  def permuteTest(initial: Int, expected: Int): Unit = {
    val result = permute(initial, permboxValues)
    require(result == expected, {
      s"$result == $expected"
    })
  }

  /**
    * Substitute the whole 16 bits at once
    */
  def substituteWhole(number: Int, sboxValues: Seq[Int]): Int = {
    val S1 = number >> 12
    val S2 = (number >> 8) & 0xF
    val S3 = (number >> 4) & 0xF
    val S4 = number & 0xF

    (sboxValues(S1) << 12) +
      (sboxValues(S2) << 8) +
      (sboxValues(S3) << 4) +
      sboxValues(S4)
  }

  /**
    * Helper to test the subtitution function
    */
  def substituteWholeTest(initial: Int, expected: Int): Unit = {
    val result = substituteWhole(initial, sboxValues)
    require(result == expected, {
      s"$result == $expected"
    })
  }

  implicit class ExtendedInt(i: Int) {
    def **(pow: Int): Int = Math.pow(i, pow).toInt
    def paddedInt(size: Int): String = {
      String.format(s"%${size}s", i.toBinaryString).replace(' ', '0')
    }
  }
}
/**
  * [1] Ruten, J. (2016). How do you set, clear and toggle a single bit in C/C++?. [online] Stackoverflow.com. Available at: http://stackoverflow.com/questions/47981/how-do-you-set-clear-and-toggle-a-single-bit-in-c-c [Accessed 17 Nov. 2016].
  */
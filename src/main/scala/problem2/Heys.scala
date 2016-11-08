package problem2

import scala.annotation.tailrec

class Heys(val size: Int, val sBox: SBox, val permBox: PermutationBox) {
  import helpers.Helpers.ExtendedInt
  import helpers.Helpers.ExtendedSeq

  /**
    * Subkey mixing
    * @return mixed subkey
    */
  def subkeyMix(input: Int, subkey: Int): Int = {
    val binaryKey = subkey.toBinarySeq(size)
    val binaryInput = input.toBinarySeq(size)

    binaryKey.zip(binaryInput).map(x => x._1 ^ x._2).toInt
  }

  def iteration(input: Int): Int = {
    val binarySeq = input.toBinarySeq(size)
    val result1 = sBox.substitute(binarySeq.view(0, 4).toInt)
    val result2 = sBox.substitute(binarySeq.view(4, 8).toInt)
    val result3 = sBox.substitute(binarySeq.view(8, 12).toInt)
    val result4 = sBox.substitute(binarySeq.view(12, 16).toInt)

    val output = result1.toBinarySeq(4) ++ result2.toBinarySeq(4) ++ result3.toBinarySeq(4) ++ result4.toBinarySeq(4)

    permBox.permute(output.toInt)
  }

  /**
    * A single iteration of the Heys cipher
    * @return mixed, substituted and permuted intermediate value
    */
  def iteration(input: Int, subkey: Int): Int = {
    val postMix = subkeyMix(input, subkey)
    iteration(postMix)
  }

  def postRoundEncrypt(input: Int, key: Int): Int = subkeyMix(permBox.permute(input), key)

  /**
    * Run the Heys cipher
    * @param input plaintext
    * @param keys all subkeys
    * @return encrypted output
    */
  def run(input: Int, keys: Seq[Int]): Int = {

    @tailrec
    def iter(in: Int, round: Int, rounds: Int): Int = {
      if (round == rounds) {
        in
      } else {
        val output = iteration(in, keys(round))
        iter(output, round + 1, rounds)
      }
    }

    val rounds = keys.size - 1
    val postRounds = iter(input, 0, rounds)

    postRoundEncrypt(postRounds, keys.last)
  }

}

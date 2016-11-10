package problem2

import scala.annotation.tailrec

class Heys(val size: Int, val sbox: SBox, val permBox: PermutationBox) {
  import helpers.Helpers.{ExtendedInt, ExtendedSeq}

  /**
    * Subkey mixing (XOR)
    * @return mixed subkey
    */
  def subkeyMix(input: Int, subkey: Int): Int = input ^ subkey

  /**
    * Substitution stage
    * @param input 16 bit input to substitute using the sbox's
    */
  def substitute(input: Int): Int = {
    val binarySeq = input.toBinarySeq(size)
    val result1 = sbox.substitute(binarySeq.view(0, 4).toInt)
    val result2 = sbox.substitute(binarySeq.view(4, 8).toInt)
    val result3 = sbox.substitute(binarySeq.view(8, 12).toInt)
    val result4 = sbox.substitute(binarySeq.view(12, 16).toInt)

    val output = result1.toBinarySeq(4) ++ result2.toBinarySeq(4) ++ result3.toBinarySeq(4) ++ result4.toBinarySeq(4)
    output.toInt
  }

  /**
    * Input permute stage
    */
  def permute(input: Int): Int = permBox.permute(input)

  /**
    * Final stage after all rounds are done
    * @return permuted and subkey-mixed input
    */
  def postRoundEncrypt(input: Int, key: Int): Int = subkeyMix(permBox.permute(input), key)

  /**
    * A single iteration of the Heys cipher
    * @return mixed, substituted and permuted intermediate value
    */
  def iteration(input: Int, subkey: Int): Int = {
    val mixed = subkeyMix(input, subkey)
    val substituted = substitute(mixed)
    val permuted = permute(substituted)
    permuted
  }

  /**
    * Run the Heys cipher
    * @param input plaintext
    * @param keys all subkeys
    * @return encrypted output
    */
  def run(input: Int, keys: Seq[Int]): Int = {

    /**
      * tail recursive implementation of the Hey's algorithm
      * @param in input for the round
      * @param round current round
      * @param rounds iterate to this round
      * @return
      */
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

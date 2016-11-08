package problem1

/**
  * Provides helper methods to crack an problem1.LSFR
  * @param lsfrSize size of the problem1.LSFR
  * @param iterations how many iterations (shifts) should be simulated
  * @param taps the indices of which bits should be XORed
  * @param stream stream to compare to if cracked via easy method
  */
class LSFRCracker(val lsfrSize: Int,
                  val iterations: Int,
                  val taps: Seq[Int],
                  val stream: Seq[Int] = Seq()) {

  import helpers.Helpers.ExtendedInt

  /**
    * Simulate an problem1.LSFR for the pre-specified number of shifts start the given state
    * @param state the state the problem1.LSFR will start at
    * @return stream of outputs bits
    */
  def simulateLsfr(state: Int): Seq[Int] = {
    val lsfr = new LSFR(initialState = state.toBinarySeq(lsfrSize), taps = taps)
    val outs = for (_ <- 1 to iterations) yield lsfr.shift().out
    outs
  }

  /**
    * Use the easy way of cracking the problem1.LSFR by checking its output stream correlation to the given stream
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

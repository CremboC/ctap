package problem1

/**
  * Provides helper methods to break an LSFR
  * @param lsfrSize size of LSFR
  * @param iterations how many iterations (shifts) should be simulated
  * @param taps the indices of which bits should be XORed
  * @param stream stream to compare to if cracked via easy method
  */
class LSFRBreaker(val lsfrSize: Int,
                  val iterations: Int,
                  val taps: Seq[Int],
                  val stream: Seq[Int] = Seq()) {

  import helpers.Helpers.ExtendedInt

  /**
    * Simulate an LSFR for the pre-specified number of shifts start the given state
    * @param state the state the LSFR will start at
    * @return stream of outputs bits
    */
  def simulateLsfr(state: Int): Seq[Int] = {
    val lsfr = new LSFR(initialState = state.toBinarySeq(lsfrSize), taps = taps)
    val outs = for (_ <- 1 to iterations) yield lsfr.shift()
    outs
  }

  /**
    * Use the easy way of breaking the LSFR by checking its output stream correlation to the given stream
    * @return maybe a tuple of (correlation, combination), if found, else None
    */
  def break(): Option[(Double, Int)] = {
    val similarities = (1 to 2 ** lsfrSize).par.map { comb =>
      val outs = simulateLsfr(comb)
      (outs.zip(stream).count(x => x._1 == x._2) / iterations.toDouble, comb)
    }

    similarities.find(s => s._1 < 0.30 || s._1 > 0.70)
  }
}

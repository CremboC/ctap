package problem1

/**
  * Represents an problem1.LSFR
  * @param initialState starting state
  * @param taps indices of bits that will be XOR'd to produce the next left-most bit
  */
class LSFR(private val initialState: Seq[Int],
           private val taps: Seq[Int]) {

  case class State(out: Int, nextState: Seq[Int])

  private var state = initialState
  private val stateSize = state.size

  /**
    * Shift the problem1.LSFR once
    * @return the produced left-most bit and the next state
    */
  def shift(): Int = {
    // the new end is the start of the previous state + the middle of it
    val newEnd = state.head +: state.tail.init
    val lastBitOut = state.last

    val tappedBits = for (i <- 0 until stateSize; if taps.contains(i)) yield state(i)
    val leftMost = tappedBits.sum % 2

    state = leftMost +: newEnd
    lastBitOut
  }
}

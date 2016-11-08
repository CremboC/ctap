/**
  * Represents an LSFR
  * @param initialState starting state
  * @param taps indices of bits that will be XOR'd to produce the next left-most bit
  */
class LSFR(private val initialState: Seq[Int],
           private val taps: Seq[Int]) {

  case class State(out: Int, nextState: Seq[Int])

  private var state: Seq[Int] = initialState

  /**
    * Shift the LSFR once
    * @return the produced left-most bit and the next state
    */
  def shift(): State = {
    // the new end is the start of the previous state + the middle of it
    val newEnd = state.head +: state.tail.init
    val lastBitOut = state.last
    val leftMost = state.zipWithIndex.filter(s => taps.contains(s._2)).map(_._1).sum % 2

    state = leftMost +: newEnd
    State(lastBitOut, state)
  }
}

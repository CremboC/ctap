package problem2

/**
  * @param values index -> value mapping
  */
class SBox(val values: Seq[Int]) {
  lazy val inverseValues: Seq[Int] = values.zipWithIndex.sortBy(_._1).map {
    case (_, index) => index
  }

  def substitute(input: Int): Int = values(input)
  def reverseSubstitute(input: Int): Int = inverseValues(input)
}

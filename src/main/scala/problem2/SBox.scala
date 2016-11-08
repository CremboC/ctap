package problem2

class SBox(val values: Seq[Int]) {
  def substitute(input: Int): Int = values(input)
}

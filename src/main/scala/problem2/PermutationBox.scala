package problem2

import scala.collection.mutable.ListBuffer

class PermutationBox(val values: Seq[Int]) {
  import helpers.Helpers.ExtendedInt
  import helpers.Helpers.ExtendedSeq

  def permute(number: Int): Int = {
    val output = ListBuffer.fill(values.size)(0)
    val binaryInput = number.toBinarySeq(values.size)


    binaryInput.zipWithIndex.foreach {
      case (bit, index) => output(values(index) - 1) = bit
    }

    output.toInt
  }
}

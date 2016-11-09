package helpers

/**
  * @author paulius
  */
object Helpers {
  /**
    * Helper classes
    */

  implicit class ExtendedInt(i: Int) {
    /**
      * Convert an integer into its binary representation using the given length
      */
    def toBinarySeq(length: Int): Seq[Int] = {
      i.toBinaryString.reverse.padTo(length, '0').split("").map(_.toInt).reverse
    }

    def toBoolean: Boolean = i match {
      case 0 => false
      case 1 => true
      case _ => throw new IllegalStateException(s"Tried to convert a non-binary number to a boolean (integer must be 0 or 1, got $i)")
    }

    def **(pow: Int): Int = Math.pow(i, pow).toInt
  }

  implicit class StringToNumber(s: String) {
    def bin: Int = Integer.parseInt(s, 2)
  }

  implicit class ExtendedSeq(s: Seq[Int]) {
    def toInt: Int = Integer.parseInt(s.mkString(""), 2)

    def binaryDifferenceTo(b: Seq[Int]): Seq[Int] = {
      s.zip(b).map(x => x._1 ^ x._2)
    }
  }

  implicit class ExtendedBool(b: Boolean) {
    def toInt: Int = b match {
      case true => 1
      case false => 0
    }
  }
}

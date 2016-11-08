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
  }

  implicit class ExtendedBool(b: Boolean) {
    def toInt: Int = b match {
      case true => 1
      case false => 0
    }
  }
}

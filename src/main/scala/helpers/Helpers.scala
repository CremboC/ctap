package helpers

/**
  * Helper classes
  */
object Helpers {

  implicit class ExtendedInt(i: Int) {
    /**
      * Convert an integer into its binary representation using the given length
      */
    def toBinarySeq(length: Int): Seq[Int] = {
      i.toBinaryString.reverse.padTo(length, '0').split("").map(_.toInt).reverse
    }

    /**
      * Convert 0 -> false, 1 -> true
      */
    def toBoolean: Boolean = i match {
      case 0 => false
      case 1 => true
      case _ => throw new IllegalStateException(s"Tried to convert a non-binary number to a boolean (integer must be 0 or 1, got $i)")
    }

    /**
      * Integer power of, shorthand
      */
    def **(pow: Int): Int = Math.pow(i, pow).toInt
  }

  implicit class StringToNumber(s: String) {
    /**
      * Parse string as a binary number
      */
    def bin: Int = Integer.parseInt(s, 2)
  }

  implicit class ExtendedSeq(s: Seq[Int]) {
    /**
      * Parse this binary sequence as a single number
      */
    def toInt: Int = Integer.parseInt(s.mkString(""), 2)

    /**
      * XOR difference between this and the given sequence
      */
    def binaryDifferenceTo(b: Seq[Int]): Seq[Int] = s.zip(b).map(x => x._1 ^ x._2)
  }

  implicit class ExtendedBool(b: Boolean) {
    /**
      * Convert true -> 1, false -> 0
      */
    def toInt: Int = if (b) 1 else 0
  }
}

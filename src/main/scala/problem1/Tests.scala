package problem1

object Tests {
  import helpers.Helpers.{ExtendedBool, ExtendedInt}
  import Main.{R1, R2, R3}

  def test0(): Unit = {
    val lsfr = new LSFR(initialState = 44.toBinarySeq(R1.size), taps = R1.taps)
    val outs = for (_ <- 0 until 25) yield lsfr.shift()
    require(outs == "0011010010111011100110010".split("").map(_.toInt).toList, {
      s"""
         |Failed test 0
         |Expected 0011010010111011100110010
         |Got ${outs.mkString("")}
       """.stripMargin
    })
  }

  def test1(): Unit = {
    val lsfr = new LSFR(initialState = 555.toBinarySeq(R2.size), taps = R2.taps)
    val outs = for (_ <- 0 until 25) yield lsfr.shift()
    require(outs == "1101010001010000101000100".split("").map(_.toInt).toList, {
      s"""
         |Failed test 1
         |Expected 1101010001010000101000100
         |Got ${outs.mkString("")}
       """.stripMargin
    })
  }

  def test2(): Unit = {
    val lsfr = new LSFR(initialState = 616.toBinarySeq(R3.size), taps = R3.taps)
    val outs = for (_ <- 0 until 25) yield lsfr.shift()
    require(outs == "0001011001000101010110111".split("").map(_.toInt).toList, {
      s"""
         |Failed test 2
         |Expected 0001011001000101010110111
         |Got ${outs.mkString("")}
       """.stripMargin
    })
  }

  def test3(): Unit = {
    val r0 = new LSFR(initialState = 44.toBinarySeq(R1.size), taps = R1.taps)
    val r1 = new LSFR(initialState = 555.toBinarySeq(R2.size), taps = R2.taps)
    val r2 = new LSFR(initialState = 616.toBinarySeq(R3.size), taps = R3.taps)

    val outs = for (_ <- 0 until 25) yield {
      Main.combineRegisterBits(r0.shift(), r1.shift(), r2.shift())
    }

    require(outs == "0000101101010100110001101".split("").map(_.toInt).toList, {
      s"""
         |Failed overall algorithm
         |Expected 0000101101010100110001101
         |Got ${outs.mkString("")}
       """.stripMargin
    })
  }

  def challengeVector(): Unit = {
    val r0 = new LSFR(initialState = 97.toBinarySeq(R1.size), taps = R1.taps)
    val r1 = new LSFR(initialState = 975.toBinarySeq(R2.size), taps = R2.taps)
    val r2 = new LSFR(initialState = 6420.toBinarySeq(R3.size), taps = R3.taps)

    val outs = for (_ <- 0 until 25) yield {
      Main.combineRegisterBits(r0.shift(), r1.shift(), r2.shift())
    }

    println(s"Challenge Vector output: ${outs.mkString("")}")
  }
}

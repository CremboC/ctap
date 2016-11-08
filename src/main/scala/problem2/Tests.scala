package problem2

import problem2.Main.permboxValues

/**
  * @author paulius
  */
object Tests {

  import helpers.Helpers.ExtendedInt
  import helpers.Helpers.ExtendedSeq
  import Main.sboxValues

  /**
    * SUBSTITUTION TESTS
    */
  def substitution(): Unit = {
    println("Substitution tests")

    val sbox = new SBox(sboxValues)

    def runTest1(): Unit = {
      /**
        * Before substitution: 291  0000000100100011
        * After substitution: 16579  0100000011000011
        */
      val test1 = 291.toBinarySeq(16)
      val result1 = sbox.substitute(test1.view(0, 4).toInt)
      val result2 = sbox.substitute(test1.view(4, 8).toInt)
      val result3 = sbox.substitute(test1.view(8, 12).toInt)
      val result4 = sbox.substitute(test1.view(12, 16).toInt)

      val output = result1.toBinarySeq(4) ++ result2.toBinarySeq(4) ++ result3.toBinarySeq(4) ++ result4.toBinarySeq(4)

      require(output.mkString("") == "0100000011000011", {
        s"""
           |Test 1 failed, expected 0100000011000011 (16579) got ${output.mkString("")} (${output.toInt})
       """.stripMargin
      })
      println("Test 1 successful")
    }

    runTest1()


    def runTest2(): Unit = {
      /**
        * Before substitution: 17767  0100010101100111
        * After substitution: 35753  1000101110101001
        */
      val test1 = 17767.toBinarySeq(16)
      val result1 = sbox.substitute(test1.view(0, 4).toInt)
      val result2 = sbox.substitute(test1.view(4, 8).toInt)
      val result3 = sbox.substitute(test1.view(8, 12).toInt)
      val result4 = sbox.substitute(test1.view(12, 16).toInt)

      val output = result1.toBinarySeq(4) ++ result2.toBinarySeq(4) ++ result3.toBinarySeq(4) ++ result4.toBinarySeq(4)

      require(output.mkString("") == "1000101110101001", {
        s"""
           |Test 2 failed, expected 1000101110101001 (35753) got ${output.mkString("")} (${output.toInt})
       """.stripMargin
      })
      println("Test 2 successful")
    }

    runTest2()


    def runTest3(): Unit = {
      /**
        * Before substitution: 33623  1000001101010111
        * After substitution: 54201  1101001110111001
        */
      val test1 = 33623.toBinarySeq(16)
      val result1 = sbox.substitute(test1.view(0, 4).toInt)
      val result2 = sbox.substitute(test1.view(4, 8).toInt)
      val result3 = sbox.substitute(test1.view(8, 12).toInt)
      val result4 = sbox.substitute(test1.view(12, 16).toInt)

      val output = result1.toBinarySeq(4) ++ result2.toBinarySeq(4) ++ result3.toBinarySeq(4) ++ result4.toBinarySeq(4)

      require(output.mkString("") == "1101001110111001", {
        s"""
           |Test 3 failed, expected 1101001110111001 (54201) got ${output.mkString("")} (${output.toInt})
       """.stripMargin
      })
      println("Test 3 successful")
    }

    runTest3()

    def runTest4(): Unit = {
      /**
        * Before substitution: 30001  0111010100110001
        * After substitution: 39728  1001101100110000
        */
      val test1 = 30001.toBinarySeq(16)
      val result1 = sbox.substitute(test1.view(0, 4).toInt)
      val result2 = sbox.substitute(test1.view(4, 8).toInt)
      val result3 = sbox.substitute(test1.view(8, 12).toInt)
      val result4 = sbox.substitute(test1.view(12, 16).toInt)

      val output = result1.toBinarySeq(4) ++ result2.toBinarySeq(4) ++ result3.toBinarySeq(4) ++ result4.toBinarySeq(4)

      require(output.mkString("") == "1001101100110000", {
        s"""
           |Test 4 failed, expected 1001101100110000 (39728) got ${output.mkString("")} (${output.toInt})
       """.stripMargin
      })
      println("Test 4 successful")
    }

    runTest4()

    def runTest5(): Unit = {
      /**
        * Before substitution: 23130  0101101001011010
        * After substitution: 45746  1011001010110010
        */
      val test1 = 23130.toBinarySeq(16)
      val result1 = sbox.substitute(test1.view(0, 4).toInt)
      val result2 = sbox.substitute(test1.view(4, 8).toInt)
      val result3 = sbox.substitute(test1.view(8, 12).toInt)
      val result4 = sbox.substitute(test1.view(12, 16).toInt)

      val output = result1.toBinarySeq(4) ++ result2.toBinarySeq(4) ++ result3.toBinarySeq(4) ++ result4.toBinarySeq(4)

      require(output.mkString("") == "1011001010110010", {
        s"""
           |Test 5 failed, expected 1011001010110010 (45746) got ${output.mkString("")} (${output.toInt})
       """.stripMargin
      })
      println("Test 5 successful")
    }

    runTest5()

  }

  def permutation(): Unit = {

    println("\nPermutation tests")

    val permutationBox = new PermutationBox(permboxValues)

    def runTest1(): Unit = {
      /**
        * Before permutation: 61440  1111000000000000
        * After permutation:  34952  1000100010001000
        */
      val result = permutationBox.permute(61440)

      require(result == 34952, {
        s"""
           |Test 1 failed, expected 34952, got $result
         """.stripMargin
      })
      println("Test 1 successful")
    }

    runTest1()

    def runTest2(): Unit = {
      /**
        * Before permutation: 3840  0000111100000000
        * After permutation:  17476  0100010001000100
        */
      val result = permutationBox.permute(3840)

      require(result == 17476, {
        s"""
           |Test 2 failed, expected 17476, got $result
         """.stripMargin
      })
      println("Test 2 successful")
    }

    runTest2()

    def runTest3(): Unit = {
      /**
        * Before permutation: 240  0000000011110000
        * After permutation:  8738  0010001000100010
        */
      val result = permutationBox.permute(240)

      require(result == 8738, {
        s"""
           |Test 3 failed, expected 8738, got $result
         """.stripMargin
      })
      println("Test 3 successful")
    }

    runTest3()

    def runTest4(): Unit = {
      /**
        * Before permutation: 15  0000000000001111
        * After permutation:  4369  0001000100010001
        */
      val result = permutationBox.permute(15)

      require(result == 4369, {
        s"""
           |Test 4 failed, expected 4369, got $result
         """.stripMargin
      })
      println("Test 4 successful")
    }

    runTest4()

    def runTest5(): Unit = {
      /**
        * Before permutation: 23130  0101101001011010
        * After permutation:  23130  0101101001011010
        */
      val result = permutationBox.permute(23130)

      require(result == 23130, {
        s"""
           |Test 5 failed, expected 23130, got $result
         """.stripMargin
      })
      println("Test 5 successful")
    }

    runTest5()

  }

  def fourRoundTestVector(): Unit = {
    println("\nFour Round Test Vector")
    /**
      * With the following  subkeys for the four round cipher:
      * 4132,  8165,  14287,  54321, and  53124
      *
      * plaintext 12033  produces ciphertext 20025
      * plaintext 62153  produces ciphertext 7495
      */

    val size = 16
    val sbox = new SBox(sboxValues)
    val permBox = new PermutationBox(permboxValues)

    val subkey1 = 4132
    val subkey2 = 8165
    val subkey3 = 14287
    val subkey4 = 54321
    val subkey5 = 53124
    val keys = Seq(subkey1, subkey2, subkey3, subkey4, subkey5)

    val plaintext1 = 12033
    val plaintext2 = 62153

    val heys = new Heys(size, sbox, permBox)
    val output = heys.run(plaintext1, keys)
    require(output == 20025, {
      s"""
         |Found Round Test Vector 1 failed. Found $output, required 20025
       """.stripMargin
    })
    println("Test 1 successful")

    val output2 = heys.run(plaintext2, keys)
    require(output2 == 7495, {
      s"""
         |Found Round Test Vector 2 failed. Found $output, required 7495
       """.stripMargin
    })
    println("Test 2 successful")
  }

  def fourRoundChallengeVector(): Unit = {
    /**
      * With the same five subkeys given above (i.e. 4132,  8165,  14287,  54321,  53124) for
      * the four round cipher encrypt the following plaintext 13571  and record the resulting ciphertext
      * in your assessment, clearly identified.
      */
    val size = 16
    val sbox = new SBox(sboxValues)
    val permBox = new PermutationBox(permboxValues)

    val subkey1 = 4132
    val subkey2 = 8165
    val subkey3 = 14287
    val subkey4 = 54321
    val subkey5 = 53124
    val keys = Seq(subkey1, subkey2, subkey3, subkey4, subkey5)

    val plaintext = 13571
    val heys = new Heys(size, sbox, permBox)
    val output = heys.run(plaintext, keys)

    println(s"\nFour Round Challenge Vector output: $output")
  }
}

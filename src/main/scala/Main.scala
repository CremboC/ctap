import scala.collection
import scala.collection.mutable
import scala.io.Source

/**
  * @author paulius
  */
object Main extends App {


  val sym = Seq(0, 1)
  println(217381273.toBinaryString)

  val z = Source.fromFile("StreamFile.txt").getLines().next().split(" ").map(_.toInt)

  def combinations(lst: Seq[Int], size: Int): List[List[Int]] = {
    if (size == 0)
      List(List())
    else {
      for {
        x <- lst.toList
        xs <- combinations(lst, size - 1)
      } yield x :: xs
    }
  }

  def lsfr(r: Seq[Int], taps: Seq[Int]): (Int, Seq[Int]) = {
    val ntail = r.head +: r.tail.init
    val out = r.last
    val nhead = r.zipWithIndex.filter(r => taps.contains(r._2)).map(_._1).sum % 2

    (out, nhead +: ntail)
  }

  def crack(length: Int, taps: Seq[Int]): Unit = {
    val combs = combinations(Seq(0, 1), length)

    val similarities = combs.map { comb =>
      var state: Seq[Int] = comb

      val outs = for (i <- 1 to 2000) yield {
        val r = lsfr(state, taps)
        state = r._2
        r._1
      }

      outs.zip(z).count(x => x._1 == x._2) / 2000.0
    }

    similarities.zipWithIndex.find(x => x._1 > 0.7) match {
      case Some(x) =>
        println(combs(x._2))
      case None =>
    }
  }

  def lsfr3(): Unit = {
    val taps = Seq(0, 2, 3, 12)
    crack(13, taps)
  }

  //  lsfr3()

  def lsfr2(): Unit = {
    val taps = Seq(1, 10)
    crack(11, taps)
  }

  //  lsfr2()

  val b = List(0, 0, 1, 0, 1, 1, 0, 1, 1, 0, 1)
  val c = List(1, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1)

  val func = (a: Boolean, b: Boolean, c: Boolean) => {
    c ^ (a & b) ^ (a & c)
  }

  def intToBool(a: Int): Boolean = if (a == 1) true else false

  def boolToInt(a: Boolean): Int = if (a) 1 else 0

  def lsfr1(): Unit = {
    val tapsA = Seq(0, 6)
    val tapsB = Seq(1, 10)
    val tapsC = Seq(0, 2, 3, 12)

    val combs = combinations(Seq(0, 1), 7)

    val similarities = combs.par.map { comb =>
      var stateA: Seq[Int] = comb
      var stateB: Seq[Int] = b
      var stateC: Seq[Int] = c

      val outs = for (_ <- 1 to 2000) yield {
        val rA = lsfr(stateA, tapsA)
        val rB = lsfr(stateB, tapsB)
        val rC = lsfr(stateC, tapsC)

        stateA = rA._2
        stateB = rB._2
        stateC = rC._2

        boolToInt(func(intToBool(rA._1), intToBool(rB._1), intToBool(rC._1)))
      }

      outs.zip(z).count(x => x._1 == x._2) / 2000.0
    }

    similarities.zipWithIndex.maxBy(_._1) match {
      case x => println(combs(x._2))
    }
  }

  lsfr1()
}

package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    @tailrec
    def inner(chars: Array[Char], offset: Int): Boolean =
      if (chars.isEmpty) offset == 0
      else if (offset < 0) false
      else chars.head match {
        case '(' => inner(chars.tail, offset + 1)
        case ')' => inner(chars.tail, offset - 1)
        case _ => inner(chars.tail, offset)
      }
    inner(chars, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    *
    *
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg0: Int, arg1: Int): (Int, Int) = {
      if (idx < until) {
        chars(idx) match {
          case '(' => traverse(idx + 1, until, arg0 + 1, arg1)
          case ')' =>
            if (arg0 > 0) traverse(idx + 1, until, arg0 - 1, arg1)
            else traverse(idx + 1, until, arg0, arg1 + 1)
          case _ => traverse(idx + 1, until, arg0, arg1)
        }
      } else (arg0, arg1)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) {
        traverse(from, until, 0, 0)
      } else {
        val mid = from + (until - from) / 2
        val (a, b) = parallel(
          reduce(from, mid),
          reduce(mid, until)
        )
        // ( )   (( ))
        if (a._1 > b._2) (a._1 - b._2 + b._1, a._2)
        // ()  ()
        else (b._1, b._2 - a._1 + a._2)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}

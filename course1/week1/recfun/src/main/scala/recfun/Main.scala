package recfun

import scala.util.Try

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(col: Int, row: Int): Int = {
    val divisor = factorial(col) * Try(factorial(row - col)).getOrElse(1)
    val dividend = factorial(row)
    Try(dividend / divisor).getOrElse(1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    if (chars.isEmpty) true
    else {
      def check(chars: List[Char], level: Int): Int = {
        if (chars.isEmpty || level < 0) level // abort recursion if level is negative
        else if (chars.head == '(') check(chars.tail, level + 1)
        else if (chars.head == ')') check(chars.tail, level - 1)
        else check(chars.tail, level)
      }
      val levelReached = check(chars, 0)
      levelReached == 0
    }
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = ???

  private def factorial(n: Int): Int = {
    if (n < 0) throw new ArithmeticException(s"There is no factorial for negative number: $n")
    else if (n == 0) 1
    else n * factorial(n - 1)
  }
}


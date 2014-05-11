package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  def pascal(c: Int, r: Int): Int =
    if (c == 0 || c == r) 1
    else pascal(c-1, r-1) + pascal(c, r-1)

  def balance(chars: List[Char]): Boolean = {
    def rec(chars: List[Char], open: Int): Boolean =
      if (chars.isEmpty) open == 0
      else if (chars.head == '(') rec(chars.tail, open + 1)
      else if (chars.head == ')') open > 0 && rec(chars.tail, open - 1)
      else rec(chars.tail, open)
    rec(chars, 0)
  }

  def countChange(money: Int, coins: List[Int]): Int =
    if (money == 0) 1
    else if (coins.isEmpty) 0
    else if (coins.head <= money)
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    else countChange(money, coins.tail)
}

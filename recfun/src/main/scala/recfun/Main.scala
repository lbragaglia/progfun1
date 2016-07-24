package recfun

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
    def pascal(c: Int, r: Int): Int = {
      if (c == 0 || c == r) 1
      else pascal(c-1, r-1) + pascal(c, r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def _unmatched(count: Int, chars: List[Char]): Int = {
        if (chars.isEmpty) count
        else if (count < 0) count
        else if (chars.head == '(') _unmatched(count + 1, chars.tail)
        else if (chars.head == ')') _unmatched(count - 1, chars.tail)
        else _unmatched(count, chars.tail)
      }
      _unmatched(0, chars) == 0
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def _tryCoins(sum: Int, coins: List[Int]): Int = {
        if (money == 0) 0
        else if (sum == money) 1
        else if (coins.isEmpty) 0
        else if (sum > money) 0
        else _tryCoins(sum + coins.head, coins) + _tryCoins(sum, coins.tail)
      }
      _tryCoins(0, coins)
    }
  }

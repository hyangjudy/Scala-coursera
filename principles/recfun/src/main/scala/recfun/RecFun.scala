package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c==0 || c==r)
      1
    else
      pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def balanceInner(chars: List[Char], sum: Int): Int = {
      if (chars.isEmpty || sum < 0)
        sum
      else {
        if (chars.head == '(')
          balanceInner(chars.tail, sum+1)
        else if (chars.head == ')')
          balanceInner(chars.tail, sum-1)
        else
          balanceInner(chars.tail, sum)
        }
      }

    balanceInner(chars, 0) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countChangeInner(money: Int, coins: List[Int]): Int = {
      if (money == 0)
        1
      else if (coins.isEmpty || money < 0)
        0
      else
        countChangeInner(money - coins.head, coins) + countChangeInner(money, coins.tail)
    }

    if (money == 0 || coins.isEmpty)
      0
    else
      countChangeInner(money, coins)
  }
}

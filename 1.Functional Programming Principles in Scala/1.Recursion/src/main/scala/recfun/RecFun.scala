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
  def pascal(c: Int, r: Int): Int = if (c == r || c == 0) 1 else pascal(c, r - 1) + pascal(c - 1, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def countparenthesis(chars: List[Char], count: Int): Boolean = {
      if (chars.isEmpty)
        count == 0
      else
        if (chars.head == '(')
          countparenthesis(chars.tail, count + 1)
        else if (chars.head == ')')
          countparenthesis(chars.tail, count - 1)
        else
          countparenthesis(chars.tail, count)
    }
    countparenthesis(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    0
  }
}

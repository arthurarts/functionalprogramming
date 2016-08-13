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
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def countOpenBrackets(substring: List[Char], openBracketCount: Int): Int = {
      if (substring.isEmpty)
        openBracketCount

      else if (substring.head == '(')
        countOpenBrackets(substring.tail, openBracketCount + 1)

      else if (substring.head == ')' && openBracketCount > 0)
        countOpenBrackets(substring.tail, openBracketCount - 1)

      else
        countOpenBrackets(substring.tail, openBracketCount)
    }

    if (chars.count(_.equals('(')) == chars.count(_.equals(')')))
      countOpenBrackets(chars, 0) == 0;
    else false

  }

  /**
    * Exercise 3
  */
  def countChange(money: Int, coins: List[Int]): Int = {

    if (coins.isEmpty || money < 0 ) 0

    else if (money == 0) 1

    else countChange(money - coins.head,  coins) + countChange(money, coins.tail)
}










}

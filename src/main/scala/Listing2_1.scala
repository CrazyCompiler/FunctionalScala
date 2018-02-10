object Listing2_1 {
  def factorial(n: Int): Int = {
    @annotation.tailrec
    def fact(result: Int, n: Int): Int = {
      if(n < 1) result
      else fact(result * n,n-1)
    }
    fact(1, n)
  }

  def abs(n: Int): Int = {
    if (n < 0) -n
    else n
  }

  def formatAbs(x: Int): String = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(factorial(4))
  }
}

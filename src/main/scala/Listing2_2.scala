object Listing2_2 {
  def factorial(n: Int): Int = {
    @annotation.tailrec
    def fact(result: Int, n: Int): Int = {
      if (n < 1) result
      else fact(result * n, n - 1)
    }

    fact(1, n)
  }

  def abs(n: Int): Int = {
    if (n < 0) -n
    else n
  }

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d."
    msg.format(x, abs(x))
  }

  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d."
    msg.format(n, factorial(n))
  }

  def formatResult(name :String, n:Int, f:Int => Int):String = {
    val msg = "The %s of %d is %d."
    msg.format(name,n,f(n))
  }

  def main(args: Array[String]): Unit = {
    println(formatResult("absolute",-42,abs))
    println(formatResult("factorial",7,factorial))
  }

}

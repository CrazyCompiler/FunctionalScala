object Exercise2_1 {
  def fiboncci(position: Int):Int = {

    def fib(n: Int):Int = {
      if (n-2 < 0) n
      else fib(n-1) + fib(n-2)
    }
    return fib(position)
  }


  def main(args: Array[String]): Unit = {
    println(fiboncci(10))
  }
}

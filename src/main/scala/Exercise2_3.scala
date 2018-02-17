object Exercise2_3 {

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (x:A) => (y: B) => f(x,y)
  }

  val add = (a:Int, b:Int) => a+b

  def main(args: Array[String]): Unit = {

      val c = curry(add)
      val add2 = c(2)
      val add3 = add2(8)
      println(add3)
  }
}

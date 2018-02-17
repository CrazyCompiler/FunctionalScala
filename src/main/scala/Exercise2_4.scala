object Exercise2_4 {

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (x: A) => (y: B) => f(x, y)
  }


  val add = (a: Int, b: Int) => a + b

  def main(args: Array[String]): Unit = {

    val c = uncurry(curry(add))
    val add3 = c(2, 3)
    println(add3)
  }
}

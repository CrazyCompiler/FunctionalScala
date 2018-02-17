object Exercise2_5 {

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => {
      f(g(a))
    }
  }

  val add = (a: Int) => a + a
  val multiply = (a: Int) => a * a

  def main(args: Array[String]): Unit = {
    val c = compose(add,multiply)
    val add2 = c(2)
    println(add2)
  }
}

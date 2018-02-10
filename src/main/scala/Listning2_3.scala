object Listning2_3 {
  def findFirst[A](ss: Array[A], key: A) = {
    def loop(n: Int):Int = {
      if(n > ss.length -1)  -1
      else if (ss(n) == key) n
      else loop(n+1)
    }
    loop(0)
  }

  def main(args: Array[String]): Unit = {
    val ss:Array[String] = Array("hello","man","la")
    val ii:Array[Int] = Array(2,3,4,5)
    println(findFirst(ss,"la"))
    println(findFirst(ss,"asdas"))
    println(findFirst(ii,5))
  }
}

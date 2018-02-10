object Exercise2_2 {
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(index: Int):Boolean = {
      if (index+1 >= as.length) true
      else if (ordered(as(index),as(index+1))) loop(index+1)
      else false
    }
    loop(0)
  }

  val isIntAsc: (Int, Int) => Boolean = (firstValue: Int, secondValue: Int) => firstValue < secondValue

  val isIntDesc: (Int, Int) => Boolean = (firstValue: Int, secondValue: Int) => firstValue > secondValue

  def main(args: Array[String]): Unit = {
    val intAscSortedArray:Array[Int] = Array(2,3,4,5)
    val intDescSortedArray:Array[Int] = Array(5,4,3,2)
    val intNotSortedArray:Array[Int] = Array(2,3,4,2)

    println(isSorted(intAscSortedArray,isIntAsc))
    println(isSorted(intDescSortedArray,isIntDesc))
    println(isSorted(intNotSortedArray,isIntAsc))
    println(isSorted(intNotSortedArray,isIntDesc))
  }
}

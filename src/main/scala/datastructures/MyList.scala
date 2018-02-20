/*Exercises :
  3.2
  3.16


TODO:// Make copy, append, dropWhile functions
 */

package datastructures

sealed trait MyList[+A]

case object Nil extends MyList[Nothing]

case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {

  def filterWithFoldLeft[A](ds: MyList[A])(f: (A) => Boolean): MyList[A] = MyList.foldLeft(ds, MyList[A]())(
    (a: A, b) => {
      var c = b
      if(f(a)){
        c = MyList.append(b: MyList[A], a)
      }
      c
    }
  )

  def filter[A](ds: MyList[A])(f: (A) => Boolean): MyList[A] = {
    def iterator(list: MyList[A],newList: MyList[A]):MyList[A] = list match {
      case Nil => newList
      case Cons(x, xs) if f(x) => Cons(x,iterator(xs,newList))
      case Cons(x, xs) => iterator(xs,newList)
    }
    iterator(ds,MyList())
  }

  def mapWithFoldLeft[A, B](ds: MyList[A])(f: (A) => B): MyList[B] = MyList.foldLeft(ds, MyList[B]())(
    (a: A, b) => {
      MyList.append(b: MyList[B], f(a))
    }
  )

  def map[A, B](ds: MyList[A])(f: (A) => B): MyList[B] = {
    def iterator(list: MyList[A],newList: MyList[B]):MyList[B] = list match {
      case Nil => newList
      case Cons(x, xs) => Cons(f(x),iterator(xs,newList))
    }
    iterator(ds,MyList())
  }

  def transform[Int](ds: MyList[Int]): MyList[Int] = MyList.foldLeft(ds, MyList[Int]())(
    (a: Int, b: MyList[Int]) => {
      MyList.append(b, (a + "4").asInstanceOf[Int])
    }
  )

  def normalise[A](ds: MyList[MyList[A]]): MyList[A] = MyList.foldLeft(ds, MyList[A]())(MyList.appendList)

  def append[A](ds: MyList[A], i: A): MyList[A] = MyList.foldRight(ds, Cons(i, Nil))(Cons.apply)

  def appendList[A](src: MyList[A], dest: MyList[A]): MyList[A] = MyList.foldRight(src, dest)(Cons.apply)

  //  def dropWhile[A](list: MyList[A])(fn: (A) => Boolean): MyList[A] = list match {
  //    case Cons(h,t) if fn(h) => {
  //      dropWhile(t)(fn)
  //    }
  //    case _ => {
  //      list
  //    }
  //  }

  def drop[A](ds: MyList[A], noOfElementsToDrop: Int): MyList[A] = ds match {
    case Nil => Nil
    case Cons(x, xs) if (noOfElementsToDrop != 0) => drop(xs, noOfElementsToDrop - 1)
    case Cons(x, xs) if (noOfElementsToDrop == 0) => ds
  }

  def sum(ints: MyList[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: MyList[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def tail[A](ds: MyList[A]): MyList[A] = {
    ds match {
      case Nil => Nil
      case Cons(x, xs) => xs
    }
  }

  def setHead[A](ds: MyList[A], head: A): MyList[A] = {
    ds match {
      case Nil => Nil
      case Cons(_, xs) => Cons(head, xs)
    }
  }

  def toString[A](ds: MyList[A]): String = {
    def generateRepresentation(list: MyList[A], representation: String): String = {
      list match {
        case Nil => representation
        case Cons(x, Nil) => generateRepresentation(Nil, representation + x + "]")
        case Cons(x, xs) => generateRepresentation(xs, representation + x + ", ")
      }
    }

    generateRepresentation(ds, "[")
  }

  def foldRight[A, B](as: MyList[A], accumulatedValue: B)(f: (A, B) => B): B = as match {
    case Nil => accumulatedValue
    case Cons(x, xs) => {
      f(x, foldRight(xs, accumulatedValue)(f))
    }
  }

  def foldLeft[A, B](as: MyList[A], accumulatedValue: B)(f: (A, B) => B): B = as match {
    case Nil => accumulatedValue
    case Cons(x, xs) => {
      foldLeft(xs, f(x, accumulatedValue))(f)
    }
  }

  def reverse[A](as: MyList[A]): MyList[A] = foldLeft(as, Nil: MyList[A])(Cons.apply)

  def length[A](as: MyList[A]): Int = foldLeft(as, 0)((x: A, y: Int) => y + 1)

  def apply[A](as: A*): MyList[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

}

object MyListTest {
  def main(args: Array[String]): Unit = {
    val a = MyList(1, 2, 3, 4, 5)
    println("List" + MyList.toString(a))
    println("Add: " + MyList.sum(a))

    val b = MyList(1.0, 2.0, 4.0)
    println("product: " + MyList.product(b))

    println("Tail : " + MyList.toString(MyList.tail(a)))
    println("string: " + MyList.toString(a))

    println("List for setHead" + MyList.toString(a))
    println("setHead: " + MyList.toString(MyList.setHead(a, 9)))

    println("List for fold right" + MyList.toString(a))
    println("fold right for sum: " + MyList.foldRight(a, 0)(_ + _))
    println("fold right for product: " + MyList.foldRight(a, 1)(_ * _))
    println("fold right for product: " + MyList.foldRight(a, Nil: MyList[Int])(Cons.apply))

    println("Print the lenth of : " + MyList.length(a))

    println("List for fold left" + MyList.toString(a))
    println("fold left for sum: " + MyList.foldLeft(a, 0)(_ + _))
    println("fold left for product: " + MyList.foldLeft(a, 1)(_ * _))

    println("List for Drop" + MyList.toString(a))
    println("Drop 3 items from the list: " + MyList.toString(MyList.drop(a, 3)))

    println("List for append using fold left" + MyList.toString(a))
    println("append using fold left: " + MyList.toString(MyList.append(a, 9)))

    val c = MyList(a, a, a)
    println("List for normalise list of list" + MyList.toString(c))
    println("normalise using fold left: " + MyList.toString(MyList.normalise(c)))

    println("List for creating transformation function" + MyList.toString(a))
    println("transformation all values in a list: " + MyList.toString(MyList.transform(a)))

    println("List for creating mapWithFoldLeft" + MyList.toString(a))
    println("map all values in a list and add 1 mapWithFoldLeft: " + MyList.toString(MyList.mapWithFoldLeft(a)(_ + 1)))
    println("map all values in a list and multiply 1 mapWithFoldLeft: " + MyList.toString(MyList.mapWithFoldLeft(a)(_ * 5)))

    println("List for creating map" + MyList.toString(a))
    println("map all values in a list and add 1: " + MyList.toString(MyList.map(a)(_ + 1)))
    println("map all values in a list and add 1: " + MyList.toString(MyList.map(a)(_ * 5)))

    println("List for creating filterWithFoldLeft" + MyList.toString(a))
    println("filter all values in a list which are smaller than 3 filterWithFoldLeft: " + MyList.toString(MyList.filterWithFoldLeft(a)(_ < 3)))
    println("filter all values in a list which are even: " + MyList.toString(MyList.filterWithFoldLeft(a)(_ % 2 == 0)))

    println("List for creating filter" + MyList.toString(a))
    println("filter all values in a list which are smaller than 3 filter: " + MyList.toString(MyList.filter(a)(_ < 3)))
    println("filter all values in a list which are even: " + MyList.toString(MyList.filter(a)(_ % 2 == 0)))


  }
}



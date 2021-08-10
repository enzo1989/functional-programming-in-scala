package exercises.week3

import java.util.NoSuchElementException

object Polymorphism extends App {
  val singletonIntList = singleton[Int](1)
  val singletonBooleanList = singleton[Boolean](true)
  val list = Cons(1, Cons(2, Cons(3,Nil())))
  val res = nth(list,2)
  println(res)
  println(singletonIntList)
  println(singletonBooleanList)
  def singleton[T](elem: T) = Cons[T](elem, Nil[T])

  def nth[T](xs: List[T], n: Int): T = {
    if(xs.isEmpty)
      throw new IndexOutOfBoundsException()
    if(n == 0) xs.head
    else nth(xs.tail, n-1)
  }
}



trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}


case class Cons[T](val head:T, val tail: List[T]) extends List[T] {
  def isEmpty = false


}

case class Nil[T]() extends List[T] {
  def isEmpty = true
  def head = throw new NoSuchElementException("Nil.Head")
  def tail = throw new NoSuchElementException("Nil.tail")

}
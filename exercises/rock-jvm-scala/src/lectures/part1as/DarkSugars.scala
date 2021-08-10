package lectures.part1as

import scala.util.Try

object DarkSugars extends App{
  // syntax sugar 1 methods with single param
  def singleArgMethod(arg: Int): String = s"$arg little ducks"

  val description = singleArgMethod {
    42
  }
  // list all combinations of numbers x and y where x is drawn from
  // 1 to M and y is drawn from 1 to N
  for (x <- 1 to 10; y <- 1 to 5)
    yield print(x, y)

  val aTryInstantce = Try {
    throw new RuntimeException
  }

  List(1,2,3).map {
    x => x + 1
  }

  // syntax sugar 2: single abstract method
  trait Action {
    def act(x : Int): Int
  }

  val anInstance: Action = new Action {
    override def act(x: Int): Int = x + 1
  }

  val aFunkyInstance: Action = (x:Int) => x + 1 // magic

  // example: Runnables
  val aThread = new Thread(new Runnable {
    override def run(): Unit = println("hello Scala")
  })

  val aSweeterThread = new Thread(() => println("hello Scala 2"))

  abstract class AnAbstractType {
    def implemented: Int = 23
    def f(a: Int): Unit
  }

  val anAbstractTypeInstance: AnAbstractType = (a: Int) => println("sweet")
  anAbstractTypeInstance.implemented

  // syntax sugar 3 the:: and #
  val prependedList = 2 :: List(3,4)
  // 2.::(List(3,4))

  // scala spec: last char decides associativity of method
  1 :: 2 :: 3 :: List(4,5)
  List(4,5).::(3).::(2).::(1)






}

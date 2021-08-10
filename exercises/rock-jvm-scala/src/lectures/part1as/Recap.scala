package lectures.part1as

import scala.annotation.tailrec

object Recap extends App {
  val aCondition: Boolean = false
  val aConditionedVal = if(aCondition) 42 else 65

  val aCodeBlock = {
    if(aCondition) 54
    56
  }

  // Unit: not return anything,
  val theUnit = println("Hello, Scala")

  // functions:
  def afunction(x: Int): Int = x + 1

  // recursion: stack and tail
  @tailrec def factorial(n: Int, accumulator: Int): Int =
    if(n <= 0) accumulator
    else factorial(n - 1, accumulator)

  // object - orientation
  class Animal
  class Dog extends Animal

  val aDog: Animal = new Dog
  trait  Carnivore {
    def eat(a: Animal): Unit
  }
  class Crocodile extends  Animal with Carnivore {
    override def eat(a: Animal): Unit = println("crunch!")
  }

  // method notations
  val aCroc = new Crocodile
  aCroc.eat(aDog)
  aCroc eat aDog

  // anonymous classes
  val aCarnivore = new Carnivore {
    override def eat(a: Animal): Unit = println("roar!//")
  }

  // generics
  abstract class MyList[+A]

  // singletons and companions
  object MyList

  // case classes
  case class Person(name: String, age: Int)

  // exceptions
  //val throwsException = throw new RuntimeException // Nothing

   val aPotentialFailure = try {
     throw new RuntimeException
   } catch {
     case e: Exception => "I caught u"
   } finally {
     println("some logs")
   }

  // packaging and imports

  // functional programming
  val incrementer = new Function1[Int, Int] {
    override def apply(v1: Int): Int = v1 + 1
  }

  incrementer(1)

  val anonymousIncrementer = (x:Int) => x+1

  List(1,2,3).map(anonymousIncrementer)

  // map flatMap, filter

  //for comprehension
  val pairs = for {
    num <- List(1,2,3)
    char <- List('a','b','c')
  } yield num + "-"  + char

  // Scala collections Seqs, Arrays Lists Vectors Maps Tuples
  val aMap = Map(
    "zhang" -> 11,
    "zui" -> 22
  )

  // "collections" options try
  val anOption = Some(2)

  // pattern matching
  val x = 2
  val order = x match {
    case 1 => "first"
    case 2 => "second"
    case 3 => "third"
    case _ => x + "th"
  }

  val bob = Person("bob", 22)
  val greeting = bob match {
    case Person(n,_) => s"Hi, this is $n"
    case Person(_,_) => "No"
  }

  // all the patterns


}

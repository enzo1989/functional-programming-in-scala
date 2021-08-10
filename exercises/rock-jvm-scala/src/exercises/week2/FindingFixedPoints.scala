package exercises.week2

import scala.math.abs

object FindingFixedPoints extends App {

  println(sqrt(4))
  //println(sqrtV1(4))

  val tolerance = 0.0001

  def fixedPoint(f: Double => Double)(firstGuess: Double): Double = {
    def iterate(guess: Double): Double = {
      val next = f(guess)
      println(next)
      if(isCloseEnough(guess, next)) next
      else iterate(next)
    }
    iterate(firstGuess)
  }

  def isCloseEnough(x: Double, y: Double): Boolean = {
    abs((x - y )/y) < tolerance
  }

  def averageDamp(f: Double => Double)(x : Double): Double = {
    (x + f(x)) / 2
  }

  def sqrt(x: Double) = fixedPoint(averageDamp(y => x/y))(1.0)

  def sqrtV0(x:Double) = fixedPoint(y => x/y )(1.0)
  def sqrtV1(x:Double) = fixedPoint(y => (x+y)/y/2 )(1.0)
}

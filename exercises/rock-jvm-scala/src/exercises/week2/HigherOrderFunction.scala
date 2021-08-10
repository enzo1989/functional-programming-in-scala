package exercises.week2

object HigherOrderFunction extends App {

  println(sumInts(1, 5))
  println(sumCubes(1, 5))
  println(sumFactorials(1, 5))
  println(sumFactorialsV1(1, 5))

  def sum(f: Int => Int, a:Int, b:Int): Int = {
    if(a > b) 0
    else f(a) + sum(f, a+1, b)
  }

  def sumV1(f: Int => Int, a:Int, b:Int): Int = {
    def loop(a: Int, acc:Int): Int = {
      if(a > b) acc
      else loop(a + 1, acc + f(a))
    }
    loop(a,0)
  }

  def sumInts(a: Int, b:Int) = sum(id, a, b)
  def sumIntsV1(a: Int, b:Int) = sum(x => x, a, b)
  def sumCubes(a: Int, b:Int) = sum(cube, a, b)
  def sumCubesV1(a: Int, b:Int) = sum(x => x * x * x, a, b)
  def sumFactorials(a: Int, b:Int) = sum(fact, a, b)
  def sumFactorialsV1(a: Int, b:Int) = sumV1(fact, a, b)

  def id(x : Int): Int = x
  def cube(x : Int): Int = x * x * x
  def fact(x : Int): Int = if(x == 0) 1 else x * fact(x - 1)

}

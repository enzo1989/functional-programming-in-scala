package exercises.week2

object Currying extends App {

  println(product(x => x * x)(1, 5))
  println(fact(5))

  println(sum(fact)(1, 5))
  println(productV2(identity)(1, 5))

  def product(f: Int => Int)(a:Int, b:Int):Int =
    if(a>b) 1
    else f(a) * product(f)(a + 1,b)

  def fact(n: Int) = product(x => x)(1,n)

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b:Int): Int = {
    def recur(a:Int): Int = {
      if(a > b) zero
      else combine(f(a), recur(a + 1))
    }
    recur(a)
  }

  def sum(f: Int => Int)(a: Int, b:Int):Int = mapReduce(f, (x,y) => x + y, 0)(a,b)
  def productV2(f: Int => Int)(a: Int, b:Int) = mapReduce(f, (x,y) => x * y, 1)(a, b)
}

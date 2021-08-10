package exercises.week2

object FunctionsData extends App {

  val rational_1 = new Rational(1,2)
  val rational_2 = new Rational(2,3)
  val rational_3 = new Rational(7,5)
  println(rational_1)
  println(rational_1.add(rational_2).sub(rational_3).neg)
  println(rational_1.numer)
  println(rational_1.demon)
  println(makeString(addRetional(rational_1, rational_2)))

  class Rational(x:Int, y:Int) {
    val numer = x
    val demon = y

    def add(r: Rational) = new Rational(r.numer * demon + numer * r.demon, r.demon * demon)
    def sub(r: Rational) = add(r.neg)
    def mul(r: Rational) = new Rational(r.numer * numer, r.demon * demon)
    def neg = new Rational(-x, y)
    override def toString: String = s"$numer/$demon"
  }

  def addRetional(r: Rational, s:Rational): Rational= {
    new Rational(r.numer * s.demon + s.numer * r.demon, r.demon * s.demon)
  }

  def makeString(r: Rational): String = {
    s"${r.numer}/${r.demon}"
  }
}

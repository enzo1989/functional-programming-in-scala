package exercises.week2

object FunWithRationals extends App{
  val rational_1  = new Rational(2)
  val rational_2  = new Rational(2,3)
  println(rational_1 + rational_2)
  println(rational_1.min(rational_2))
  println(rational_1)
  class Rational(x:Int, y:Int) {

    require( y > 0, "denominator must be positive")
    assert(y > 0)

    private def gcd(a: Int,b:Int):Int = {
      if(b == 0) a else gcd(b, a%b)
    }

    private val g = gcd(x, y)

    val numer = x
    val demon = y

    def this(x:Int) = this(x,1)
    def add(r: Rational) = new Rational(r.numer * demon + numer * r.demon, r.demon * demon)
    def sub(r: Rational) = add(r.neg)
    def mul(r: Rational) = new Rational(r.numer * numer, r.demon * demon)
    def neg = new Rational(-x, y)

    def + (r: Rational) = this.add(r)
    def - (r: Rational) = this.sub(r)
    def * (r: Rational) = this.mul(r)
    def / (r: Rational) = this.mul(r* new Rational(-1))

    def min(that: Rational) = less(that)

    def compare(that: Rational): Boolean = numer* that.demon < that.numer * demon
    def max(that: Rational): Rational = if(compare(that)) that else this
    def less(that: Rational): Rational = if(compare(that)) this else that
    override def toString: String = s"${numer/ gcd(x.abs, y)}/${demon/ gcd(x.abs, y)}"
  }
}

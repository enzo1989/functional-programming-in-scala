package exercises.week3

object PureOOL extends App {

  println(trueX < trueX)
  println(trueX < falseX)
  println(falseX < trueX)
  println(falseX < falseX)


  val two = Succ(Succ(Zero))
  val one = Succ(Zero)
  val three = two + one
  println(three)
}

trait BooleanX {
  def ifThenElse[T] (t: => T, e: => T) : T
  def && (x: => BooleanX): BooleanX = ifThenElse(x, falseX)
  def || (x: => BooleanX): BooleanX = ifThenElse(trueX, x)
  def unary_! : BooleanX = ifThenElse(falseX, trueX)
  def == (x: BooleanX): BooleanX = ifThenElse(x, x.unary_!)
  def != (x: BooleanX): BooleanX = ifThenElse(x.unary_!, x)

  def ==> (x: BooleanX) = ifThenElse(x, trueX)
  def < (x: BooleanX) = ifThenElse(falseX, x)
}

object trueX extends BooleanX {
  def ifThenElse[T](t: => T, e: => T) = t
}

object falseX extends BooleanX {
  def ifThenElse[T](t: => T, e: => T) = e
}

abstract class Nat {
  def isZero: Boolean = predecessor == null
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {

  def predecessor: Nat = null

  def +(that: Nat): Nat = that

  def -(that: Nat): Nat = throw new NoSuchElementException()

  override def toString: String = "0"
}

case class Succ(n: Nat) extends Nat {
  def predecessor: Nat = n

  def +(that: Nat): Nat = new Succ(n + that)

  def -(that: Nat): Nat = if(that.isZero) this else (predecessor - that.predecessor)

  override def toString: String = {
    def helper(amount:Nat, acc:Int) : String = {
      if (amount.isZero) acc.toString
      else helper(amount.predecessor, acc+1)
    }
    helper(this, 0)
  }
}
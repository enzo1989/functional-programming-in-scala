package exercises.week3


object ClassHierarchies extends App {

  val insetEmpty = Inset.singleton(1)
  val intSet1 = Inset(1)
  val intSet3 = Inset(3,4)
  println(insetEmpty)
  println(intSet1)
  println(intSet3)
}

trait IntSet {
  def name = "IntSet"
  def contains(x: Int): Boolean
  def insert(x: Int): IntSet
  def union(x: IntSet): IntSet
  def unionV1(x: IntSet): IntSet
}

object Inset {
  def apply(): IntSet = Empty
  def apply(x: Int): IntSet = Empty.insert(x)
  def apply(x: Int, y: Int): IntSet = Empty.insert(x).insert(y)
  def singleton(x: Int) = NonEmpty(x, Empty, Empty)
}

case class Empty() extends IntSet {
  override def name: String = "Empty"

  def contains(x: Int): Boolean = false

  def insert(x: Int): IntSet = NonEmpty(x, Empty(), Empty())

  def union(x: IntSet): IntSet = x
  def unionV1(x: IntSet): IntSet = x
}

case class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  override def name: String = "NonEmpty"

  def contains(x: Int): Boolean = {
    if(elem == x) true
    else if(elem > x){
      left.contains(x)
    } else {
      right.contains(x)
    }
  }

  def insert(x: Int): IntSet = {
    if(x == elem) this
    else if(elem > x ) NonEmpty(elem, left.insert(x), right)
    else NonEmpty(elem, left, right.insert(x))
  }

  def union(x: IntSet): IntSet = {
    left.union(right).union(x).insert(elem)
  }

  def unionV1(other: IntSet): IntSet = {
    val incl1 = other.insert(elem)
    val incl2 = left.unionV1(incl1)
    right.unionV1(incl2)
  }
}

object Empty extends IntSet {
  def contains(x: Int): Boolean = false

  def insert(x: Int): IntSet = NonEmpty(x, Empty, Empty)

  def union(x: IntSet): IntSet = x
  def unionV1(x: IntSet): IntSet = x
}
package fpinscala.monads

object Curry {
  def add(x: Int): Int=>Int = {
    y: Int => x + y
  }

  def addFour = add(4)

  def addCurry(x: Int)(y: Int): Int = {
    x + y
  }

  def addFive = addCurry(5)(_)

  def main(args: Array[String]): Unit = {
    println(addFour(2)) //4
    println(addFive(3)) //8
  }
}
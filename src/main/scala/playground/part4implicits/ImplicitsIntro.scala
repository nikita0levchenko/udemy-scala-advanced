package playground.part4implicits

object ImplicitsIntro extends App {
  val pair = "Nikita" -> "Ilya"
  // some implicit magic

  class Person(name: String) {
    implicit def greet: String = s"Hello, my name is $name"
  }

  private implicit def fromStringToPerson(str: String): Person = new Person(str)

  println(
    "Ilya".greet
  ) // println(fromStringToPerson("Ilya).greet) implicitly aply fromStringToPerson method

  // This will make a collision between two greet() methods
//  class A {
//    def greet: Int = 2
//  }
//
//  private implicit def fromStringToA(str: String): A = new A

// implicits parameters
  def increse(x: Int)(implicit amount: Int): Int = x + amount
  implicit val defaultAmount: Int = 10
  println(increse(1))

}

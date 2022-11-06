package playground.part4implicits

object PimpMyLib extends App {

  // Type inrichment == pimping the lib
  implicit class RichInt(value: Int) {
    def isEven: Boolean = value % 2 == 0
    def sqrt: Double = Math.sqrt(value)
    def times(function: () => Unit): Unit = (1 to value).foreach(x => function.apply())
    def *[T](list: List[T]) = (1 to value).flatMap(x => list).toList
  }

  println(new RichInt(42).isEven)
  println(42.isEven) // Cool!

  /**
    * Exercises
    * 1) Enrich the String class
    *   - asInt
    *   - encrypt(key: Int)
    *   - decrypt(key: Int)
    *
    * 2) Enrich the Int class
    *   - times(function)
    *     3.times(() => ...)
    *   - *
    *     3 * List(1,2) = List(1,2,1,2,1,2)
    */

  // Exercise 1
  implicit class RichString(val string: String) extends AnyVal {
    def asInt = Integer.valueOf(string)
    def encrypt(key: Int): String = string.map(str => (str.toInt + key).toChar)
    def decrypt(key: Int): String = string.map(str => (str.toInt - key).toChar)
  }

  println("3".asInt + 4)
  val encrypted = "John".encrypt(2)
  val decrypted = "SUS".decrypt(3)
  println(encrypted)
  println(decrypted)
  println(encrypted.decrypt(3))

  // Exercise 2
  3.times(() => println("Hi SUS"))
  println( 3 * List(1,2))

  // Trick like in JS: "6" / 2
  implicit def stringToInt(value: String): Int = Integer.valueOf(value)
  println("6" / 2) // stringToInt("6") / 2


}

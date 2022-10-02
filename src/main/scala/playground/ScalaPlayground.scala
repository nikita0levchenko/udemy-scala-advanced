package playground

import scala.annotation.tailrec

object ScalaPlayground extends App {

  /** Partial functions exercises
    * 1) Construct a partial function instance by yourself
    * 2) Dumb chatbot as partial function
    */

  // Exercise 1
  val aSimplePF: PartialFunction[Int, String] = {
    case 0 => "Zero"
    case 1 => "One"
  }

  // Exercise 2
  val chatBot: PartialFunction[String, String] = {
    case "hello" => "Hi, my name is Hall 9000"
    case "buy"   => "You can't stop this. Ha ha."
    case _       => "idk what are you talking about"
  }

  scala.io.Source.stdin.getLines().map(lline => chatBot(lline.toLowerCase)).foreach(println)

}

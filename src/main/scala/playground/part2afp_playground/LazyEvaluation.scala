package playground.part2afp_playground

import scala.collection.WithFilter

object LazyEvaluationPlayGround extends App {

  // lazy DELAYS evaluation of values
  lazy val x: Int = {
    println("hello")
    42
  }

  println(x)

  // examples of implications
  def sideEffectCondition: Boolean = {
    println("Smims")
    true
  }

  def simpleCondition: Boolean = false

  lazy val lazyCondition  = sideEffectCondition

  println(if(simpleCondition && lazyCondition) "yes" else "no")

  //in conjunction with call by name

  def evaluate(x: => Int): Int = {
    // call by nedd technique
    lazy val xByNeed = x // lzy vals evaluate only one time
    xByNeed + xByNeed + xByNeed + 1
  }

  def longProcessing: Int = {
    println("wait")
    Thread.sleep(1000)
    42
  }
  List(1,2).withFilter(_ < 2)

  println(evaluate(longProcessing))

  val lessThan20: Int => Boolean = x => {
  println(s"$x less thna 20?")
    x < 20
  }

  val moreThan10: Int => Boolean = x => {
    println(s"$x more than 10?")
    x > 10
  }

  println(List(22, 15, 9, 11).filter(moreThan10).filter(lessThan20))

  lazy val  list: WithFilter[Int, List] = List(22, 15, 9, 11).withFilter(moreThan10).withFilter(lessThan20)

  println(list) // withFilter use the lazy vals. this will print WithFilter object
  list.foreach(println)

  // for-comprehension use withFilter inside

  val testList = List(2,6,1,4,5,12)

  val res1 = for {
    l <- testList if lessThan20(l)
  } yield l + 1

  // the code above == code below
  val res2 = testList.withFilter(lessThan20).map(_ + 1)




}

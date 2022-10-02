package playground.part2afp_playground

object CurriesPAFPlayground extends App {

  // Curried function
  val superAdder: Int => Int => Int =
    x => y => x + y // the same as def(x: Int, y: Int): Int = x + y

  // supper adder take one arg and return a function for one arg
  val add3: Int => Int = superAdder(3) // Int => Int = y => 3 + y

  // This is a METHOD
  def curriedAdder(x: Int)(y: Int): Int = x + y // Curried method

  // But when you write expression below - you will create a functional value adder3
  // When you want to create a function value from method - this is a lifting process or ETA-EXPANSION
  // Kepp that in mind function != method
  val adder3: Int => Int = curriedAdder(3)

  println(adder3(5) == curriedAdder(3)(5))

  def inc(x: Int): Int = x + 1
  List(1, 2, 3).map(
    inc
  ) // when you will do something like this - compiller will do ETA-EXPANSION for you with inc method
  // In other words - the compliller will create a lambda from Lost(1,2,3).map(inc) into List(1,2,3).map(x => inc(x))
  List(1, 2, 3).map(x => inc(x)) // ETA-EXPANSION

  // Partial functions for ETA-EXPANSION
  val add5 =
    curriedAdder(
      5
    ) _ // When you put the _ into another function's parameters places - you tell to compiller
  // do the ETA-EXPANSION with this function and return a function value as a result

  //EXERCISE
  /** We have a three functions doing the same thing, but defined in different ways:
    */
  val simpleAddFunction = (x: Int, y: Int) => x + y
  def simpleAddMethod(x: Int, y: Int): Int = x + y
  def curriedAddFunction(x: Int)(y: Int): Int = x + y

  /** You shuld to implement add7 method in many different ways using this three types of functions. Be creative!
    */
  val add7v1: Int => Int = simpleAddFunction(7, _)
  val add7v2: Int => Int = simpleAddMethod(7, _)
  val add7v3: Int => Int = curriedAddFunction(7)
  val add7v4: Int => Int =
    curriedAddFunction(
      7
    ) _ // PAF, therefore compiller will do the ETA-EXPANSION
  val add7v4_1: Int => Int =
    curriedAddFunction(7)(_) // PAF = alternative syntax
  val add7v5 = (x: Int) => simpleAddFunction(x, 7)
  val add7v6 = (x: Int) => simpleAddMethod(x, 7)
  val add7v7 = (x: Int) => curriedAddFunction(x)(7)
  val add7v8 = simpleAddMethod.curried(7)
  val add7v9 = simpleAddFunction.curried(7)
  val add7v10 = simpleAddMethod(
    7,
    _: Int
  ) // Alternative syntax fro turn methods into a functional values
  // it will create a lambda for us: y => simpleAddMethod(7, y)

  // Underscores it's powerfull
  def concatenator(a: String, b: String, c: String): String = a + b + c
  val insertTheName: String => String =
    concatenator("Hello, my name is ", _: String, ", and I'm glad to see you")
  // the code above do the same ETA-EXPRESION and create the lambda: x: String => concatenator("str1", x, "str3")

  println(insertTheName("Nikita"))

  val fillInBlanks = concatenator(
    "Hello, ",
    _: String,
    _: String
  ) // ETA-EXPANSION: (x: String, y: String) => concatenator("hello", x, y)
  println(fillInBlanks("Nikita", " Scala is awesome"))

  /*
  1) Process the list of numbers and return their string representation in differend formats: %4.2f, %8.6f, %14.12f with a curriet formater function
   */

  def formater(format: String)(x: Double): String = format.format(x)

  val format4_2f = formater("%4.2f") _ // lift for ETA
  val format8_6f = formater("%8.6f") _
  val format14_12f = formater("%14.12f") _

  println(format4_2f(scala.math.Pi))
  println(format8_6f(scala.math.Pi))
  println(format14_12f(scala.math.Pi))

  val testList = List(scala.math.Pi, scala.math.E, 1, 89.463581)
  println("LISTS CHECK")
  testList.map(format4_2f).foreach(println)
  testList.map(format8_6f).foreach(println)
  testList.map(format14_12f).foreach(println)


  /*
  2) Difference between:
      - functions vs methods
      - parameters by name vs 0-lambda
   */

  def byName(n: => Int): Int =
    n + 1 // a method which receive a parameter by name
  def byZeroLambda(f: () => Int): Int =
    f() + 1 // a method which receive a parameter ny 0-lambda

  def method: Int = 42
  def parenMethod(): Int = 42
  val x: () => Int = parenMethod _

  /*
  Explore calling by name and by functions with the following expressions
    - int
    -method(103 line)
    -parenMethod
    -lambda
    -PAF
   */


  // byName checks
  println("BYNMAE CHECKS")
  println(byName(3))                     // Int
  println(byName(method))                // method
  println(byName(parenMethod()))         // parenMethod
  //println(byName(() => 42))            // lambda - wrong type (() => Int) != (=> Int)
  byName((() => 42)())                   // okay because you have a lambda () => 42, and when you put () after her - you actually calling her (() => 42)()
  //println(byName(parenMethod _))       // PAF - will not wor, because Int != (() => Int)

  // 0-lambda checks
  println("0-LAMBDA CHECKS")
  //println(byZeroLambda(3))             Int - you can't call this function on primitive object Int. You need a function
  //println(byZeroLambda(method))        method - you can't call thi sfunction on method of type Int. You should pass only the () => Int type
  //byZeroLambda(parenMethod())          You can't because paretnmethod() - will return the int
  println(byZeroLambda(parenMethod))    // without parentheses compiler does ETA, and result type will be () => Int
  println(byZeroLambda(() => 42))        //lambda
  //byZeroLambda((() => 42)())         Int != () => Int
  println(byZeroLambda(parenMethod _)) // PAF - it's okay

}

package playground.part2afp_playground

import scala.runtime.Nothing$

object MonadsPlayground extends App {

  trait Attempt[+A] {
    def flatMap[B](f: A => Attempt[B]): Attempt[B]
    def map[B](f: A => B): Attempt[B]
  }

  case class Yes[+A](elem: A) extends Attempt[A] {
    override def flatMap[B](f: A => Attempt[B]): Attempt[B] = try { f(elem) }
    catch {
      case e: Throwable => No(e)
    }

    override def map[B](f: A => B): Attempt[B] = try { Attempt(f(elem)) }
    catch {
      case e: Throwable => No(e)
    }
  }

  case class No(elem: Throwable) extends Attempt[Nothing] {
    override def flatMap[B](f: Nothing => Attempt[B]): Attempt[B] = this

    override def map[B](f: Nothing => B): Attempt[B] = this
  }

  object Attempt {
    def apply[A](elem: => A): Attempt[A] = try { Yes(elem) }
    catch {
      case e: Throwable => No(e)
    }

    def flatten[A](monad: Attempt[Attempt[A]]): Attempt[A] =
      monad.flatMap(x => x)
  }

  val yes = Attempt(4 / 2)
  val no = Attempt(4 / 0)
  val flatMapedYes = yes.flatMap(e => Attempt(e * 2))
  val flatMapedNo = no.flatMap(e => Attempt(e * 3))
  println((yes, no))
  println((flatMapedYes, flatMapedNo))

  // Prove the monad laws for Attempt monad

  /** left-identity law:
    * unit(a).flatMap(f) == f(a)
    * Attempt(a).flatMap(f) == f(a)
    */
  val tesFunc: Int => Attempt[Int] = x => Attempt(x * 2)
  val law1result = Attempt(2).flatMap(tesFunc) == tesFunc(2)
  println("1 monadic law")
  println(law1result)

  /** right-identity law:
    * unit(a).flatMap(unit) == unit(a)
    * Attempt(a).flatmap(Attempt(_)) == Attempt(a)
    */
  val law2result = Attempt(2).flatMap(Attempt(_)) == Attempt(2)
  println("2 monadic law")
  println(law2result)

  /** associativity law:
    * unit(a).flatMap(f).flatMap(g) == f(a).flatMap(g)
    * Attempt(a).flatMap(f).flatMap(g) == f(a).flatMap(g)
    * unit(a).flatMap(f).flatMap(g) = unit(a).flatMap(x => f(x).flatMap(g))
    */
  val testFunc2: Int => Attempt[Int] = x => Attempt(x + 3)
  val law3result =
    Attempt(2).flatMap(tesFunc).flatMap(testFunc2) == tesFunc(2).flatMap(
      testFunc2
    )
  val law3AlternativeResult =
    Attempt(2).flatMap(tesFunc).flatMap(testFunc2) == Attempt(2).flatMap(x =>
      tesFunc(x).flatMap(testFunc2)
    )
  println("3 monadic law")
  println(law3result)
  println("3 monadic law alternative")
  println(law3AlternativeResult)

  /** Exercise 1: Implement a Lazy[T] monad which might have a value or not
    */

  trait Lazy[+T] {
    def flatMap[S](f: (=>T) => Lazy[S]): Lazy[S]
    def use: T
  }

  object Lazy {
    def apply[T](elem: => T): Lazy[T] = new SomeLazy(elem)
  }

  class SomeLazy[+T](value: => T) extends Lazy[T] {
    // call by need
    private lazy val internalValue = value
    def use: T = internalValue
    override def flatMap[S](f: (=> T) => Lazy[S]): Lazy[S] = f(internalValue)
  }

  val lazyVal = Lazy{
    println("today I will lazy")
    42
  }

  /** Left-identity law:
    * NoneLazy.flatMap(f) => NoneLazy
    * SomeLazy(a).flatMap(f) => f(a)
    */

  /** right-identity law:
    * NoneLazy.flatMap(Lazy(a)) => NoneLazy
    * SomeLazy(a).flatMap(f) => f(a)
    */

  /** associativity law:
    * NoneLazy.flatMap(f).flatMap(g) => NoneLazy.flatMap(g) => NoneLazy
    * SomeLazy(a).flatMap(f).flatMap(g) => f(a).flatMap(g)
    * SomeLazy(a).flatmap(x => f(x).flatMap(g)) => f(a).flatMap(g)
    */

  /** Exercise 2:
    * implement map, and flatten in terms of flatMap
    * def map[B](f: A => B): Monad[B] = flatMap(x => unit(f(x)))
    * def flatten(monad: Monad[Monad[A]]): Monad[A] = monad.flatMap(x => x)
    */

 val flatMapedInstance = lazyVal.flatMap(e => Lazy(e * 10))
 val flatMapedInstance2 = lazyVal.flatMap(e => Lazy(e * 10))

  println("------/------")
 println(flatMapedInstance.use)
 println(flatMapedInstance2.use)

}

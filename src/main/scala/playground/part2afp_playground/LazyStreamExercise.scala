package playground.part2afp_playground

import playground.part2afp_playground.LazyStreamExercise.{ConsStream, MyStream}

import scala.annotation.tailrec

object LazyStreamExercise extends App {

  /*
    Exercise: implement a lazily evaluated, singly linked STREAM of elements.

    naturals = MyStream.from(1)(x => x + 1) = stream of natural numbers (potentially infinite!)
    naturals.take(100).foreach(println) // lazily evaluated stream of the first 100 naturals (finite stream)
    naturals.foreach(println) // will crash - infinite!
    naturals.map(_ * 2) // stream of all even numbers (potentially infinite)
   */
  abstract class MyStream[+A] {
    def isEmpty: Boolean
    def head: A
    def tail: MyStream[A]

    def #::[B >: A](element: B): MyStream[B] // prepend operator
    def ++[B >: A](
        anotherStream: => MyStream[B]
    ): MyStream[B] // concatenate two streams

    def foreach(f: A => Unit): Unit
    def map[B](f: A => B): MyStream[B]
    def flatMap[B](f: A => MyStream[B]): MyStream[B]
    def filter(predicate: A => Boolean): MyStream[A]

    def take(
        n: Int
    ): MyStream[A] // takes the first n elements out of this stream

    def takeAsList(n: Int): List[A] = {
      @tailrec
      def loop(
          count: Int,
          acc: List[A] = List(),
          stream: MyStream[A] = this.take(n)
      ): List[A] = count match {
        case 0 => acc
        case n => loop(count - 1, acc :+ stream.head, stream.tail)
      }
      loop(n)
    }

    def toList(): List[A] = if (tail.isEmpty) Nil else head +: tail.toList()
  }

  object EmptyStream extends MyStream[Nothing] {
    override def isEmpty: Boolean = true

    override def head: Nothing = throw new NoSuchElementException

    override def tail: MyStream[Nothing] = throw new NoSuchElementException

    override def #::[B >: Nothing](element: B): MyStream[B] =
      new ConsStream(element, this)

    override def ++[B >: Nothing](anotherStream: => MyStream[B]): MyStream[B] =
      anotherStream

    override def foreach(f: Nothing => Unit): Unit = ()

    override def map[B](f: Nothing => B): MyStream[B] = this

    override def flatMap[B](f: Nothing => MyStream[B]): MyStream[B] = this

    override def filter(predicate: Nothing => Boolean): MyStream[Nothing] = this

    override def take(n: Int): MyStream[Nothing] = this
  }

  class ConsStream[A](h: A, t: => MyStream[A]) extends MyStream[A] {
    override def isEmpty: Boolean = false

    override val head: A = h

    override lazy val tail: MyStream[A] = t // call by need

    override def #::[B >: A](element: B): MyStream[B] =
      new ConsStream[B](element, this)

    override def ++[B >: A](anotherStream: => MyStream[B]): MyStream[B] =
      new ConsStream[B](head, tail ++ anotherStream)

    override def foreach(f: A => Unit): Unit = { f(head); tail.foreach(f) }

    override def map[B](f: A => B): MyStream[B] =
      new ConsStream[B](f(head), tail.map(f))

    override def flatMap[B](f: A => MyStream[B]): MyStream[B] =
      f(head) ++ tail.flatMap(f)

    override def filter(predicate: A => Boolean): MyStream[A] = if (
      predicate(head)
    ) new ConsStream(head, tail.filter(predicate))
    else tail.filter(predicate) // preserves lazy eval!

    override def take(n: Int): MyStream[A] = n match {
      case 1           => new ConsStream(head, EmptyStream)
      case n if n <= 0 => EmptyStream
      case _           => new ConsStream(head, tail.take(n - 1))
    }
  }

  object MyStream {

    def apply[T](values: T*): MyStream[T] = {
      @tailrec
      def loop(valSeq: Seq[T], acc: MyStream[T] = EmptyStream): MyStream[T] =
        if (valSeq.isEmpty) acc else loop(valSeq.tail, valSeq.head #:: acc)

      loop(values.reverse)
    }

    def from[A](start: A)(generator: A => A): MyStream[A] =
      new ConsStream[A](start, MyStream.from(generator(start))(generator))
  }

}

object LazyStreamPlayground extends App {

  val testStream: MyStream[Int] = MyStream(1, 2, 3)
  println(testStream.takeAsList(3))

  val naturals = MyStream.from(1)(_ + 1)
  naturals.take(10000).foreach(println)

  println(naturals.map(_ * 2).takeAsList(100))

  println(naturals.flatMap(e => MyStream(e, e + 1)).takeAsList(10))

  val startFrom0 = 0 #:: naturals

  println(startFrom0.filter(_ < 10).take(10).toList())

  /*
  Exercises on stream :
  1) Infinite Fibonacci stream
  2)Stream of prime digits, which filtered by Eratosthene's sieve
  [2,3,4,5,6,7,8,9,10]
  divide by 2, exclude themself
  [2,3,5,7,9,11]
  divide by 3, exclude themself
  [2,3,5,7,11,13,17]
  and so on
   */

  def fibonacciStream(first: Int, second: Int): MyStream[Int] =
    new ConsStream(first, fibonacciStream(second, first + second))
  println("Fiboncci")
  println(fibonacciStream(1, 1).take(10).toList())

  def isPrime(elem: Int): Boolean = {
    @tailrec
    def loop(counter: Int = 2): Boolean = elem match {
      case 1 => false
      case e =>
        counter match {
          case c if c == e => true
          case c           => if (e % c == 0) false else loop(c + 1)
        }
    }
    loop()
  }

  def eraosthenes(stream: MyStream[Int]): MyStream[Int] = if (stream.isEmpty)
    stream
  else
    new ConsStream[Int](stream.head, eraosthenes(stream.tail.filter(_ % stream.head != 0)))

  println(isPrime(4))

  println(naturals.filter(isPrime).take(10).toList())
  println(eraosthenes(MyStream.from(2)(_ + 1)).take(10).toList())
}

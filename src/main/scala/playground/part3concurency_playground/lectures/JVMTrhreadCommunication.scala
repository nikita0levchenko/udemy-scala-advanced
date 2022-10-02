package playground.part3concurency_playground.lectures

import scala.collection.mutable
import scala.util.Random

object JVMTrhreadCommunication extends App {
  /**
    * Producer-consumer problem
    * Producer -> [ x ] -> Consumer
    * Somehow consumer have to wait producer will finish work, because producer and consu,er working in parallel
    */

  class SimpleContainer {
    private var value: Int = 0
    def isEmpty: Boolean = value ==  0
    def getValue: Int = {
      val result = value
      value = 0
      result
    }
    def setValue(elem: Int): Unit = value = elem
  }

  def naiveProcCons(): Unit = {
    val container = new SimpleContainer

    val consumerThread = new Thread(() => {
      println("[consumer] waiting...")
      while(container.isEmpty) {
        println("[consumer] actively waiting...")
      }
      println(s"[consumer] after waiting I consumed: ${container.getValue}")
    })

    val producerThread = new Thread(() => {
      println("[producer] computing...")
      Thread.sleep(500)
      val value = 42
      println(s"[producer] after long calculation I produced value $value")
      container.setValue(value)
    })

    consumerThread.start()
    producerThread.start()
  }

  //naiveProcCons()

  // wait and notify
  def smarterProcAndCons(): Unit = {
    val container = new SimpleContainer

    val consumerThread = new Thread(() => {
      println("[consumer] waiting")
      container.synchronized{
        container.wait()
      }

      // at this point we have a value
      println(s"[consumer] I got: ${container.getValue}")
    })

    val producerThread = new Thread(() => {
      println("[producer] is producing")
      Thread.sleep(2000)
      println("[producer] value os produced")
      val value = 42
      container.setValue(value)
      container.synchronized{
        container.notify()
      }
    })
    consumerThread.start()
    producerThread.start()
  }

  //smarterProcAndCons()

  /**
    * producer -> [x, x, x] -> consumer
    * consumer will consume last value in a buffer
    */

  def complicatedProcCons(): Unit = {
    val buffer: mutable.Queue[Int] = new mutable.Queue[Int]()
    val capacity = 3

    def consumerThread = new Thread(() => {
      val random: Random = new Random()
      while(true){
        buffer.synchronized{
          if(buffer.isEmpty){
            println("[consumer] buffer is empty, waiting for new values")
            // producer, buffer is empty, are you lazy?
            buffer.wait()
          }

          val value = buffer.dequeue()
          println(s"[consumer] consumed the last element in buffer $value")
          buffer.notify()
        }
        Thread.sleep(random.nextInt(250))
      }
    })

    def producerThread = new Thread(() => {
      val random: Random = new Random()
      while(true){
        buffer.synchronized{
          if(buffer.size == capacity){
            println("[producer] full buffer, waiting for empty space")
            buffer.wait()
          }

          // consumer, new food for you
          val value = random.nextInt()
          buffer.enqueue(value)
          println(s"[producer] produced a new value $value")
          buffer.notify()
        }
        Thread.sleep(random.nextInt(500))
      }
    })

    consumerThread.start()
    producerThread.start()
  }

  //complicatedProcCons()

  /**
    * Prod-cons level 3
    *
    * producer1 ---> [? ? ?] <--- consumer1
    * producer2 ------^   ^------ consumer2
    *
    */

  class Consumer(id: Int, buffer: mutable.Queue[Int]) extends Thread {
    override def run(): Unit = {
      val random = new Random()
      while(true){
        buffer.synchronized {
          while (buffer.isEmpty) {
            println(s"[consumer$id] buffer is empty, waiting for new values")
            buffer.wait()
          }

          // there must be at least ONE value in the buffer
          val x = buffer.dequeue()
          println(s"[consumer$id] consumed $x")

          buffer.notify()

        }
        Thread.sleep(random.nextInt(250))
      }
    }
  }

  class Producer(id: Int, buffer: mutable.Queue[Int], capacity: Int) extends Thread {
    override def run(): Unit = {
      val random = new Random()
      var i = 0

      while(true){
        buffer.synchronized{
          while(buffer.size == capacity){
            println(s"[producer$id] buffer is full, waiting...")
            buffer.wait()
          }

          // there must be at least one empty space in buffer
          println(s"[producer$id] producing $i")
          buffer.enqueue(i)

          buffer.notify()
          i += 1
        }

        Thread.sleep(random.nextInt(500))
      }
    }
  }

  def multiProdAndCons(prodsCount: Int, consCount: Int): Unit = {
    val buffer  = new mutable.Queue[Int]()
    val capacity = 3
    (1 to consCount).foreach(i => new Consumer(i, buffer).start())
    (1 to prodsCount).foreach(i => new Producer(i, buffer, capacity).start())
  }

  multiProdAndCons(3, 3)

}

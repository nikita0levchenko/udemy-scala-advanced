package playground.part3concurency_playground.lectures

import java.util.concurrent.Executors

object JVMConcurencyIntro extends App {

  // JVM threads
  /**
    * interface Runable {
    * public void run()
    * }
    */

    val runnable = new Runnable {
      override def run(): Unit = println("Hello concurency")
    }
  val aThread = new Thread(runnable)

  aThread.start() // Gives the signal to the JVM start a new JVM thread
  // Create a new thread => new OS thread

  runnable.run() // will not to do something in parallel

  aThread.join(1000) // blocks untill aThread finishes running

  val helloThread = new Thread(() => 1.to(5).foreach(_ => println("hello")))
  val goodBuyThread = new Thread(() => 1.to(5).foreach(_ => println("good buy")))
  // different runs produce different outputs
  helloThread.start()
  goodBuyThread.start()

  // Executors
  val threadPool = Executors.newFixedThreadPool(10)
  threadPool.execute(() => println("something in thread pool"))

  threadPool.execute(() => {
    Thread.sleep(1000)
    println("done after 1 second")
  })

  threadPool.execute(() => {
    Thread.sleep(1000)
    println("almost done")
    Thread.sleep(1000)
    println("done in 2 seconds")
  })

  threadPool.shutdown() // this will shotdown all threads in thread pool, which mean what no more actions you can execute on this thread pool
  //threadPool.execute(() => println("It's will not apear")) // It will throw an exception in the calling thread
  //threadPool.shutdownNow() // this comand will interupt the all threads, even sleaping threads. It will throw java.lang.InterruptedException: sleep interrupted

  println(threadPool.isShutdown) // will tell you the pool is shotdown or not. It can appearance before pool threads action.
  // Because shotdowned pool - will not execute a new actions more


}

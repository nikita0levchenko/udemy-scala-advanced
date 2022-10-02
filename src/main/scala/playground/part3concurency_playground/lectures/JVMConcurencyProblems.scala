package playground.part3concurency_playground.lectures

object JVMConcurencyProblems {

  case class BankAccount(var amount: Int)

  def buy(bankAccount: BankAccount, thing: String, price: Int): Unit = {
    bankAccount.amount -= price // this is critical section
    // operation "-=" is not atomar, she consists from severals steps:
    //  - read value
    //  - change value
    //  - write value
    // another thread can do the same things between our thread steps
  }

  // to protect the critical section from race condition we should use syncronised
  def safeBuy(bankAccount: BankAccount, thing: String, price: Int): Unit = {
    bankAccount.synchronized { // does not allow multiple threads to run the critical section in the same time
      bankAccount.amount -= price
    }
  }

  def demoBankingproblem(): Unit = {
    (1 to 10000).foreach { _ =>
      val account = BankAccount(50000)
      val thread1 = new Thread(() => safeBuy(account, "shoes", 3000))
      val thread2 = new Thread(() => safeBuy(account, "iPhone", 4000))
      thread1.start()
      thread2.start()
      thread1.join()
      thread2.join()
      if(account.amount != 43000) println(s"Bank hacked ${account.amount}")
    }
  }

  /**
    * Exercises
    * 1) "inception threads
    * Thread 1
    *   -> Thread 2
    *     -> Thread 3
    *       ...
    *         Thread N
    *each thread should print "Hello from thread $n"
    * print all messages in reverce order
    *
    * 2) Find the min and max values of x after minMaxX function
    *
    * 3) Find which phrase will print first after demoSleepFancy method
    *
    */

  // Exercise 1
  def inceptionThreads(maxThreads: Int, i: Int = 1): Thread = {
    new Thread(() => {
      if(i < maxThreads){
        val childThread = inceptionThreads(maxThreads, i + 1)
        childThread.start()
        childThread.join()
      }
      println(s"Hello from $i thread")
    })
  }

  // Exercise 2

  /**
    *
    * max value - 9999
    * min value - 1
    *   all threads read value 0 from x
    *   incremented him
    *   write back => each thread write 1
    *   1
    */
  def minMaxX(): Int = {
    var x = 0
    val threads = (1 to 10000).map(i => new Thread(() => x += 1))
    threads.foreach(_.start())
    x
  }

  // Exercise 3
  def demoSleepFancy(): Unit = {
    var message = ""
    val thread = new Thread(() => {
      message = "Scala is awesome"
      Thread.sleep(1000)
    })

    message = "Scala is sucks"
    thread.start()
    Thread.sleep(1001)
    println(message)
  }


  def main(args: Array[String]): Unit = {
    inceptionThreads(10).start()
  }
}

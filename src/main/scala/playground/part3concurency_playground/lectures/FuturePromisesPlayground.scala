package playground.part3concurency_playground.lectures

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.duration.*
import scala.util.{Failure, Random, Success, Try}

object FuturePromisesPlayground extends App {

  def calculateMeaningOfLife = {
    Thread.sleep(2000)
    42
  }

  val aFuture = Future { calculateMeaningOfLife }

  println(aFuture.value) // Return None, because Future[Try[Int]]

  println("Waiting future")
  aFuture.onComplete {
    case Success(value)     => println(s"Meaning of life is $value")
    case Failure(exception) => println(s"I have failed with $exception")
  } // this call-back will be call by SOME thread, we don't know which one exacly

  Thread.sleep(3000)

  // Mini social-network

  case class Profile(id: String, name: String) {
    def poke(anotherProfile: Profile): Unit = println(
      s"${this.name} poking ${anotherProfile.name}"
    )
  }

  object SocialNetwork {
    // database
    val names: Map[String, String] = Map(
      "fb.id.0-dummy" -> "dummy_user",
      "fb.id.1.nik" -> "Nikita",
      "fb.id.2.andrew" -> "Andrew"
    )

    val bestFriend: Map[String, String] = Map(
      "fb.id.1.nik" -> "fb.id.2.andrew",
      "fb.id.2.andrew" -> "fb.id.1.nik"
    )

    val friends: Map[String, List[String]] = Map(
      "fb.id.1.nik" -> List("fb.id.2.andrew", "fb.id.0-dummy"),
      "fb.id.2.andrew" -> List("fb.id.1.nik", "fb.id.0-dummy")
    )

    private val random = new Random()

    // API
    def fetchProfileById(id: String): Future[Profile] = Future {
      Thread.sleep(random.nextInt(300))
      new Profile(id, names(id))
    }

    def fetchBestFriends(profile: Profile): Future[List[String]] = Future {
      Thread.sleep(random.nextInt(400))
      friends(profile.id)
    }

    def fetchBestFriend(profile: Profile): Future[Profile] = Future {
      Thread.sleep(300)
      val bestFriendId = bestFriend(profile.id)
      new Profile(bestFriend(bestFriendId), names(bestFriendId))
    }
  }

  //client: mark to poke andrew

  // dirt way
  val nikita = SocialNetwork.fetchProfileById("fb.id.1.nik")
//  nikita.onComplete {
//    case Success(nikitaProfile) => {
//      val andrew = SocialNetwork.fetchBestFriend(nikitaProfile)
//      andrew.onComplete {
//        case Success(andrewProfile)   => nikitaProfile.poke(andrewProfile)
//        case Failure(andrewException) => andrewException.printStackTrace()
//      }
//    }
//    case Failure(nikitaException) => nikitaException.printStackTrace()
//  }
//
//  Thread.sleep(1000)

  // Functional composition of futures
  // map, flatMap, filter

  val nameOnTheWall: Future[String] = nikita.map(profile => profile.name)
  val nikitasBestFriend: Future[Profile] =
    nikita.flatMap(profile => SocialNetwork.fetchBestFriend(profile))
  val nikitasBestFriendRestricted: Future[Profile] =
    nikita.filter(profile => profile.name.startsWith("N"))

  // clean way
  for {
    nikita <- SocialNetwork.fetchProfileById("fb.id.1.nik")
    andrew <- SocialNetwork.fetchBestFriend(nikita)
  } nikita.poke(andrew)

  Thread.sleep(3000)

  // fallbacks or recover
  val aProfileNoMatterWhat: Future[Profile] =
    SocialNetwork.fetchProfileById("unknown id").recover { case e: Throwable =>
      Profile("fb.id.0-dummy", "Forever alone")
    }

  aProfileNoMatterWhat.onComplete {
    case Success(value) => println(value.toString)
    case Failure(e)     => e.printStackTrace()
  }
  Thread.sleep(3000)

  val aFetchedProfileNoMaterWhat: Future[Profile] =
    SocialNetwork.fetchProfileById("<error>").recoverWith { case e: Throwable =>
      SocialNetwork.fetchProfileById("fb.id.0-dummy")
    }

  aFetchedProfileNoMaterWhat.onComplete {
    case Success(value)     => println(value.toString)
    case Failure(exception) => exception.printStackTrace()
  }
  Thread.sleep(3000)

  val aFallbackProfileNoMaterWhat: Future[Profile] = SocialNetwork
    .fetchProfileById("-/-")
    .fallbackTo(SocialNetwork.fetchProfileById("fb.id.0-dummy"))

  aFallbackProfileNoMaterWhat.onComplete {
    case Success(value)     => println(value.toString)
    case Failure(exception) => println(exception.printStackTrace())
  }
  Thread.sleep(3000)

  //Banking application
  case class User(name: String)
  case class Transaction(
      sender: String,
      receiver: String,
      amount: Double,
      status: String
  )

  object BankingApp {
    val applicationName: String = "Rock the JVM banking"

    def fetchUser(name: String): Future[User] = Future {
      // Simulating fetching from DB
      Thread.sleep(500)
      User(name)
    }

    def createTransaction(
        sender: User,
        receiver: String,
        amount: Double
    ): Future[Transaction] = Future {
      // Simulating transaction processing
      Thread.sleep(1000)
      Transaction(sender.name, receiver, amount, "_SUCCESS")
    }

    def purchase(
        client: String,
        merchant: String,
        item: String,
        amount: Double
    ): String = {
      // fetch user from DB
      // create a transaction
      // wait until purchase finish
      val transactionStatusFuture = for {
        user <- fetchUser(client)
        transaction <- createTransaction(user, merchant, amount)
      } yield transaction.status
      //Thing below will block thread, untill all futures will complete
      Await.result(
        transactionStatusFuture,
        2.seconds
      ) // implicits conversions -> pimp my library
    }
  }

  println(BankingApp.purchase("Nikita", "Rock the JVM store", "Iphone12", 3000))

  // How you can manipulate futures with Promises
  val promise = Promise[Int]()

  // Thread 1 - consumer
  val future = promise.future

  future.onComplete { case Success(e) =>
    println(s"[consumer] received element $e")
  }

  // Thread 2 - producer
  val producer = new Thread(() => {
    println("[producer] producing...")
    Thread.sleep(500)
    //"fulfilling" the promise
    promise.success(42)
    println("[producer] is done")
  })

  producer.start()
  Thread.sleep(1000)

  /*
    1) fulfill a future IMMEDIATELY with a value
    2) inSequence(fa, fb)
    3) first(fa, fb) => new future with the first value of the two futures
    4) last(fa, fb) => new future with the last value
    5) retryUntil[T](action: () => Future[T], condition: T => Boolean): Future[T]
   */

  // 1 - fulfill immediately
  def fulfillImmediately[T](value: T): Future[T] = Future(value)
  // 2 - insequence
  def inSequence[A, B](first: Future[A], second: Future[B]): Future[B] =
    first.flatMap(_ => second)

  // 3 - first out of two futures
  def first[A](fa: Future[A], fb: Future[A]): Future[A] = {
    val promise = Promise[A]
    fa.onComplete(promise.tryComplete)
    fb.onComplete(promise.tryComplete)

    promise.future
  }

  // 4 - last out of the two futures
  def last[A](fa: Future[A], fb: Future[A]): Future[A] = {
    // 1 promise which both futures will try to complete
    // 2 promise which the LAST future will complete
    val bothPromise = Promise[A]
    val lastPromise = Promise[A]
    val checkAndComplete = (result: Try[A]) =>
      if (!bothPromise.tryComplete(result))
        lastPromise.complete(result)

    fa.onComplete(checkAndComplete)
    fb.onComplete(checkAndComplete)

    lastPromise.future
  }

  val fast = Future {
    Thread.sleep(100)
    42
  }

  val slow = Future {
    Thread.sleep(200)
    45
  }
  first(fast, slow).foreach(f => println("FIRST: " + f))
  last(fast, slow).foreach(l => println("LAST: " + l))

  Thread.sleep(1000)

  // retry until
  def retryUntil[A](
      action: () => Future[A],
      condition: A => Boolean
  ): Future[A] =
    action()
      .filter(condition)
      .recoverWith { case _ =>
        retryUntil(action, condition)
      }

  val random = new Random()
  val action = () =>
    Future {
      Thread.sleep(100)
      val nextValue = random.nextInt(100)
      println("generated " + nextValue)
      nextValue
    }

  retryUntil(action, (x: Int) => x < 10).foreach(result =>
    println("settled at " + result)
  )
  Thread.sleep(10000)
}

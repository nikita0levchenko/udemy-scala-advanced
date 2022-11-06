package playground.myPlayground

import lectures.part4implicits.TypeClasses.User
import playground.part4implicits.TypeClasses.HTMLSerializer

object EqualityPlayground extends App {

  trait Equal[T] {
    def equal(v1: T, v2: T): Boolean
  }

  object Equal {
    def apply[T](v1: T, v2: T)(implicit equalizer: Equal[T]): Boolean =
      equalizer.equal(v1, v2)

    implicit object equalUsersByName extends Equal[User] {
      override def equal(v1: User, v2: User): Boolean = v1.name == v2.name
    }

    implicit object equalUsersByAge extends Equal[User] {
      override def equal(v1: User, v2: User): Boolean = v1.age == v2.age
    }

    implicit object equalUsersByEmail extends Equal[User] {
      override def equal(v1: User, v2: User): Boolean = v1.email == v2.email
    }

    implicit object fullEqualityUsers extends Equal[User] {
      override def equal(v1: User, v2: User): Boolean =
        v1.name == v2.name && v1.age == v2.age && v1.email == v2.email
    }
  }

  val nikita = User("Nikita", 25, "nikita0levchenko@gmail.com")
  val ilya = User("Ilya", 25, "bababsasa@gmail.com")

  import Equal.fullEqualityUsers
  println(Equal(nikita, ilya))
  println(Equal(nikita, ilya))
  println(Equal(nikita, ilya))

  // Exercise implement the Type class pattern for the Equality pattern

  trait EqualutyTypeClass[T] {
    def equal(v1: T, v2: T): Boolean
    def apply(implicit
        equalutyTypeClass: EqualutyTypeClass[T]
    ): EqualutyTypeClass[T] = equalutyTypeClass
  }

  object EqualutyTypeClass {
    def apply[T](v1: T, v2: T)(implicit
        equalityTypeClass: EqualutyTypeClass[T]
    ): Boolean = equalityTypeClass.equal(v1, v2)

    implicit object UserEqualityByNameTypeClass
        extends EqualutyTypeClass[User] {
      override def equal(v1: User, v2: User): Boolean = v1.name == v2.name
    }

    implicit object UserEqualityByAgeTypeClass extends EqualutyTypeClass[User] {
      override def equal(v1: User, v2: User): Boolean = v1.age == v2.age
    }

    implicit object UserEqualityByEmailTypeClass
        extends EqualutyTypeClass[User] {
      override def equal(v1: User, v2: User): Boolean = v1.email == v2.email
    }

    implicit object UserFullEqualityTypeClass extends EqualutyTypeClass[User] {
      override def equal(v1: User, v2: User): Boolean =
        v1.name == v2.name && v1.age == v2.age && v1.email == v2.email
    }
  }

  implicit class EqualityEnricher[T](value: T) {
    def ===(anotherValue: T)(implicit
        equalizator: EqualutyTypeClass[T]
    ): Boolean = equalizator.equal(value, anotherValue)

    def !==(anotherValue: T)(implicit
        equalizator: EqualutyTypeClass[T]
    ): Boolean = !equalizator.equal(value, anotherValue)
  }

  import EqualutyTypeClass.UserEqualityByNameTypeClass
  println(EqualutyTypeClass(nikita, ilya))

  println(nikita === ilya)
  println(nikita !== ilya)
}

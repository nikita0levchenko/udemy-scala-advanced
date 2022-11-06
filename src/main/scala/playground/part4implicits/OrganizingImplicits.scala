package playground.part4implicits

object OrganizingImplicits extends App {

  implicit val reverseIntOrdering: Ordering[Int] =
    Ordering.fromLessThan(_ > _) // after that will return (List(5,4,3,2,1)
//  implicit val noramlIntOrdering: Ordering[Int] = Ordering.fromLessThan(
//    _ < _
//  ) will make compiller error because of two implicits val in the same scope
  println(
    List(3, 2, 1, 4, 5).sorted
  ) // without implicits in that scope will return List(1,2,3,4,5)

  /*
    Implicits(used as implicit parameters):
      - val/var
      - object
      - accessor methods = defs with no parentheses
   */

  // Implicits can be defined only in class/object/trait. Not on toplevel

  // Exercise
  case class Person(name: String, age: Int)
  object Person {
    implicit val personAlphabeticOrdering: Ordering[Person] =
      Ordering.fromLessThan((a, b) => a.name.compareTo(b.name) < 0)
  }
  implicit val personAgeOrdering: Ordering[Person] =
    Ordering.fromLessThan(_.age < _.age)

  val personsList =
    List(Person("Alina", 24), Person("Ilya", 25), Person("Nikita", 16))
  println(personsList.sorted)

  /*
  Implicit scope:
    - local level, in the same file
    - import level, in imported objects
    - companions of all types, involved in method signature
      Example:
      def sorted[B >: A](implicit ord: Ordering[B]): List[B]
      Here compiller will search implicits in List,Ordering, A and A superclasses
   */

  case class Purchase(nUnits: Int, unitPrice: Double)
  object Purchase {
    implicit val totalPriceOrdering: Ordering[Purchase] =
      Ordering.fromLessThan((a, b) =>
        a.nUnits * a.unitPrice < b.nUnits * b.unitPrice
      )
  }
  object PurchaseUnitCountOrderigng {
    implicit val purchaseUnitCountOrderign: Ordering[Purchase] = Ordering.fromLessThan(_.nUnits < _.nUnits)
  }
  object PurchaseUnitPriceOrdering {
    implicit val purchaseUnitPriceordering: Ordering[Purchase] = Ordering.fromLessThan(_.unitPrice < _.unitPrice)
  }
  /* Exercise
    - total price = most used (50%)
    - by unit count = 25%
    - by unit price = 25%
   */

  val purchases: List[Purchase] = List(Purchase(2, 22.5), Purchase(1, 100.11), Purchase(5, 11.00))
  import PurchaseUnitPriceOrdering.purchaseUnitPriceordering
  println(purchases.sorted)
}

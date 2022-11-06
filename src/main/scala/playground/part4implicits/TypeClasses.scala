package playground.part4implicits
import java.util.Date

object TypeClasses extends App {

  trait HTMLSerialazible {
    def toHtml: String
  }

  case class User(name: String, age: Int, email: String)
      extends HTMLSerialazible {
    override def toHtml: String =
      s"<div>$name ($age yo) <a href=$email/> </div>"
  }

  User("Nikita", 25, "nikita0levchenko@gmail.com")

  /** Problems
    *  1 - work for the types WE write
    * 2 - ONE implementation out of quite a number
    */

  // option 2 - pattern matching
  object HTMLSerialerPM {
    def serializeToPM(obj: Any): Unit = obj match {
      case User(name, age, email) =>
        println(s"<div>$name, $age y.o., <a href=$email></div>")
      case date: java.util.Date => println(s"<div>$date</div>")
      case _                    => println("something")
    }
  }

  /** Problems
    * 1 - Lost typesafety
    * 2 - need to modify code every time
    * 3 - still ONE implementation
    */

  // option 3 - type classes

  trait HTMLSerializer[T] {
    def serializeToHtml(value: T): String
  }

  // Part 2
  implicit object UserHTMLSerializer extends HTMLSerializer[User] {
    override def serializeToHtml(value: User): String =
      s"<div>${value.name}, ${value.age} y.o., <a href=${value.email}></div>"
  }

  // We can apply this type class HTMLSerializer[T] to many types
  object DateHTMLSerializer extends HTMLSerializer[java.util.Date] {
    override def serializeToHtml(value: Date): String =
      s"<div>${value.toString}</div>"
  }

  // We can implement many new implementations for the same type
  object PartialUserHTMLSerializer extends HTMLSerializer[User] {
    override def serializeToHtml(value: User): String =
      s"<div>${value.name}</div>"
  }

  implicit object IntHTMLSerializer extends HTMLSerializer[Int] {
    override def serializeToHtml(value: Int): String =
      s"<div style color=blue>$value</div>"
  }

  implicit object StringHTMLSerializer extends HTMLSerializer[String] {
    override def serializeToHtml(value: String): String = s"<p>$value</p>"
  }

  object HTMLSerializer {
    def serialize[T](value: T)(implicit serializer: HTMLSerializer[T]): String =
      serializer.serializeToHtml(value)
    def apply[T](implicit serializer: HTMLSerializer[T]): HTMLSerializer[T] =
      serializer
  }

  val nikita = User("Nikita", 25, "nikita0levchenko@gmail.com")
  val ilya = User("Ilya", 25, "bababsasa@gmail.com")

  println(HTMLSerializer.serialize(nikita))
  println(HTMLSerializer.apply.serializeToHtml(nikita))

  // part 3

  implicit class HTMLEnrichment[T](value: T) {
    def toHTML(implicit serializer: HTMLSerializer[T]): String =
      serializer.serializeToHtml(value)
  }

  /** - extend to new types
    * - choose implementation
    * - supre expressive
    */

  println(nikita.toHTML)
  println(2.toHTML)
  println(nikita.toHTML(PartialUserHTMLSerializer))

  // Type class pattern
  /** - type class itself / HTMLSerializer[T] { ... }
    * - type class instancess / UserSerializer or IntSeralizer in out case
    * - conversion of implicit classes / HTMLEnrichment
    */

  // context bounds
  def htmlBoilerplate[T](
      contetnt: T
  )(implicit serializer: HTMLSerializer[T]): String =
    s"<html><body>${serializer.serializeToHtml(contetnt)}</body><html>"

  def sugarHTMLBoilerPlate[T: HTMLSerializer](content: T): String =
    s"<html><body>${content.toHTML}</body></html>"

  println(htmlBoilerplate("some"))
  println(sugarHTMLBoilerPlate("SUS"))

  // implicitly
  case class Permission(mask: String)
  implicit val defaultPermissions: Permission = Permission("0477x8")

  // You cn extract implicit parameter in some part of code with implicitly method
  val standartPermissions = implicitly[Permission]
  println(standartPermissions)

  // with that trick you can extract implicit parameter for future operations with him

  def sugarHTMLBoilerplateWithTrick[T: HTMLSerializer](content: T): String = {
    val serializer = implicitly[HTMLSerializer[T]]
    s"<html><body>${content.toHTML(serializer)}</body></html>"
  }

}

package playground.myPlayground

// TYPE CLASSES
  trait MyTypeClassTemplate[T] {
    def action(value: T): String
    def apply[T](implicit typeClass: MyTypeClassTemplate[T]): MyTypeClassTemplate[T] =
      typeClass
  }
  
  object MyTypeClassTemplate {
    def apply[T](value: T)(implicit typeClass: MyTypeClassTemplate[T]): String = typeClass.action(value)
  }
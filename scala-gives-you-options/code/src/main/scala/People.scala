object People {
  
  case class Person(
      firstName: String,
      lastName: String,
      age: Int) {

    val fullName = s"${firstName} ${lastName}"
  }

  def celebrateBirthdayOf(p: Person): Person = p.copy(age = p.age + 1)
  def fromSameFamily(p1: Person, p2: Person) = p1.lastName == p2.lastName

}

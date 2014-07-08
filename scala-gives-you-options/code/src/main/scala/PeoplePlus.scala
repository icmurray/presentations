object PeopleAgain {

  case class Person(
      firstName: String,
      lastName: String,
      age: Int) {

    val fullName = s"${firstName} ${lastName}"
  }

  def celebrateBirthdayOf(p: Person): Person = p.copy(age = p.age + 1)
  def fromSameFamily(p1: Person, p2: Person) = p1.lastName == p2.lastName

  implicit class PersonOps(p: Person) {
    def celebrateBirthday() = celebrateBirthdayOf(p)
    def sameFamilyAs(other: Person) = fromSameFamily(p, other)
  }

}

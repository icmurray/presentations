case class Person(
    firstName: String,
    lastName: String,
    age: Int) {

  val fullName = s"${firstName} ${lastName}"

  def celebrateBirthday(): Person = copy(age=age+1)
  def sameFamilyAs(other: Person) = lastName == other.lastName
}


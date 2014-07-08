case class PersonData(
    firstName: String,
    lastName: String,
    age: Int) {

  val fullName = s"${firstName} ${lastName}"
}

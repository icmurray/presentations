object OptionExample {

  private val people = Map(
    0 -> Person("John", "Doe", 43),
    1 -> Person("Jane", "Doe", 44),
    2 -> Person("John", "Smith", 54)
  )

  def findPerson(id: Int): Option[Person] = people.get(id)
  def findRelated(person: Person): Option[Person] = {
    people.values.filter(_.lastName == person.lastName)
                 .filter(_ != person)
                 .headOption
  }
}

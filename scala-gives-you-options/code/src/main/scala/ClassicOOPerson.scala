class ClassicOOPerson(

    /** Constructor Args **/
    private var _firstName: String,
    private var _lastName: String,
    private var _age: Int) {

  /** Getters **/
  def firstName = _firstName
  def lastName  = _lastName
  def age       = _age

  /** Setters **/
  def firstName_=(s: String) = { _firstName = s }
  def lastName_=(s: String)  = { _lastName = s }

  /** Public methods **/
  def fullName = s"${firstName} ${lastName}"
  def celebrateBirthday(): Unit = { _age += 1 }
}

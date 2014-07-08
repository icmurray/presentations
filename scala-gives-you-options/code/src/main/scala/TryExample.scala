import scala.util.{Try, Success, Failure}

object TryExample {

  val service = OptionExample

  def findPerson(id: Int): Try[Person] = {
    Try {
      service.findPerson(id).getOrElse {
        throw new RuntimeException(s"Couldn't find: ${id}")
      }
    }
  }

  def findRelated(person: Person): Try[Person] = {
    Try {
      service.findRelated(person).getOrElse {
        throw new RuntimeException(s"Couldn't find Person related to ${person}")
      }
    }
  }
}

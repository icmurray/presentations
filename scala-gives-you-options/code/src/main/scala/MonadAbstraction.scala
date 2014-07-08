import scala.language.higherKinds

import scala.util.Try

import scalaz._
import scalaz.syntax.monad._
import scalaz.std.option._
import scalaz.contrib.std.utilTry._

abstract class PeopleService[M[+_]: Monad] {
  def findPerson(id: Int): M[Person]
  def findRelated(person: Person): M[Person]

  def findRelatedTo(id: Int): M[Person] = {
    for {
      p <- findPerson(id)
      related <- findRelated(p)
    } yield related
  }
}

object OptionPeopleService extends PeopleService[Option] {
  lazy val service = OptionExample
  def findPerson(id: Int) = service.findPerson(id)
  def findRelated(person: Person) = service.findRelated(person)
}

object TryPeopleService extends PeopleService[Try] {
  lazy val service = TryExample
  def findPerson(id: Int) = service.findPerson(id)
  def findRelated(person: Person) = service.findRelated(person)
}

package uk.co.sprily.scalaTalk

import scala.util.{Try, Success, Failure}

trait UserDAOModule {

  val userDAO: UserDAO

  trait UserDAO {
    def insert(username: String,
               password: HashedPassword): Try[User]

    def byUsername(username: String): Option[User]
  }
}

trait PostgresUserDAOModule extends UserDAOModule {

  val databaseURL: String
  val userDAO = new Impl()

  class Impl extends UserDAO {

    def insert(username: String,
               password: HashedPassword) = {
      // Some concrete implementation...
      ???
    }

    def byUsername(username: String) = {
      // Some concrete implementation...
      ???
    }
  }
}


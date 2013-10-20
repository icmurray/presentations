package uk.co.sprily.scalaTalk

import java.sql.SQLException

import scala.util.{Try, Success, Failure}

import org.mindrot.jbcrypt.BCrypt

case class UserId(value: Long) extends AnyVal
case class PlaintextPassword(value: String) extends AnyVal
case class HashedPassword(value: String) extends AnyVal
case class User(id: UserId, name: String, password: HashedPassword)

trait UserServiceModule {

  val userService: UserService

  trait UserService {
    def create(username: String,
               password: PlaintextPassword): Try[User]

    def authenticate(username: String,
                     password: PlaintextPassword): Option[User]

    def find(username: String): Option[User]

    protected def hashPassword(plaintext: PlaintextPassword): HashedPassword = {
      HashedPassword(BCrypt.hashpw(plaintext.value, BCrypt.gensalt()))
    }

    protected def passwordsMatch(plaintext: PlaintextPassword,
                               hashed: HashedPassword): Boolean = {
      BCrypt.checkpw(plaintext.value, hashed.value)
    }
  }

}

trait SlickUserServiceModule extends UserServiceModule {
  this: SlickUserTableModule with DatabaseModule =>

  val userService = new Impl()

  class Impl extends UserService {

    // Required for Query construction
    import slickProfile.simple._

    def create(username: String, plaintextPassword: PlaintextPassword) = {
      try {
        database.withSession { implicit s: Session =>
          val hashedPassword = hashPassword(plaintextPassword)
          val userId =
            Users.forInsert returning Users.id insert (username, hashedPassword)
          Success(User(userId, username, hashedPassword))
        }
      } catch {
        // Should really check the SQL error code
        case e: SQLException => Failure(UniqueConstraintException)
        case e: Exception    => Failure(e)
      }

    }

    def find(username: String) = {
      database.withSession { implicit s: Session =>
        Users.byUsername(username).firstOption
      }
    }

    def authenticate(username: String, plaintextPassword: PlaintextPassword) = {
      database.withSession { implicit s: Session =>
        for {
          user <- Users.byUsername(username).firstOption
          matches = passwordsMatch(plaintextPassword, user.password)
          if matches
        } yield user
      }
    }
  }
}

/**
 * UserService implementation that relies on a UserDAO
 */
trait PersistedUserServiceModule extends UserServiceModule {
  this: UserDAOModule =>

  val userService = new Impl()

  class Impl extends UserService {

    def create(username: String,
               plaintext: PlaintextPassword) = {
      val hashed = hashPassword(plaintext)
      userDAO.insert(username, hashed)
    }

    def find(username: String): Option[User] = {
      userDAO.byUsername(username)
    }

    def authenticate(username: String, plaintext: PlaintextPassword) = {
      for {
        user <- userDAO.byUsername(username)
        if passwordsMatch(plaintext, user.password)
      } yield user
    }

  }

}

trait SlickUserTableModule extends DatabaseModule {

  // The Slick database driver is required to define the UsersTable
  import slickProfile.simple._

  val Users = new UsersTable()

  trait UserTypeMappers {

    implicit def userIdTypeMapper = MappedTypeMapper.base[UserId, Long] (
      userId => userId.value,
      long   => UserId(long)
    )

    implicit def hashedPasswordTypeMapper = MappedTypeMapper.base[HashedPassword, String] (
      _.value,
      HashedPassword.apply
    )

  }

  class UsersTable extends Table[User]("users")
                      with UserTypeMappers {

    def id       = column[UserId]("id", O.PrimaryKey, O.AutoInc)
    def username = column[String]("username")
    def password = column[HashedPassword]("password")

    def * = id ~ username ~ password <> (User, User.unapply _)
    def forInsert = username ~ password

    def usernameIdx = index("idx__users__username", username, unique = true)

    def byUsername(name: Column[String]) = {
      for {
        user <- Query(this)
        if user.username === name
      } yield user
    }
  }

  abstract override def ddls = {
    Users.ddl :: super.ddls
  }
}

//trait SlickUserTableModule extends DatabaseModule {
//
//  // The Slick database driver is required to define the UsersTable
//  import slickProfile.simple._
//
//  val Users = new UsersTable()
//
//  trait UserTypeMappers {
//
//    implicit def userIdTypeMapper = MappedTypeMapper.base[UserId, Long] (
//      userId => userId.value,
//      long   => UserId(long)
//    )
//
//    implicit def hashedPasswordTypeMapper = MappedTypeMapper.base[HashedPassword, String] (
//      _.value,
//      HashedPassword.apply
//    )
//
//  }
//
//  class UsersTable extends Table[User]("users")
//                      with UserTypeMappers {
//
//    def id       = column[UserId]("id", O.PrimaryKey, O.AutoInc)
//    def username = column[String]("username")
//    def password = column[HashedPassword]("password")
//
//    def * = id ~ username ~ password <> (User, User.unapply _)
//    def forInsert = username ~ password
//
//    def usernameIdx = index("idx__users__username", username, unique = true)
//  }
//
//  abstract override def ddls = {
//    Users.ddl :: super.ddls
//  }
//    
//}

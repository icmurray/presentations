package uk.co.sprily.scalaTalk

import spray.json._
import DefaultJsonProtocol._
import spray.httpx.SprayJsonSupport
import spray.routing._

case class UserCreateData(username: String, password: String)

trait JSONFormats {

  implicit val userIdFormat = jsonFormat(UserId, "id")

  implicit val userWriter = new RootJsonWriter[User] {
    def write(user: User) = JsObject(
      "username" -> user.name.toJson,
      "id"       -> user.id.toJson)
  }

  implicit val userCreateDataReader = new RootJsonReader[UserCreateData] {
    def read(js: JsValue) = {
      try {
        val obj = js.asJsObject
        UserCreateData(
          obj.fields("username").toString,
          obj.fields("password").toString)
      } catch {
        case e: Exception => throw new DeserializationException("User Data Expected")
      }
    }
  }

}

trait UserRoutes extends HttpService
                    with JSONFormats
                    with SprayJsonSupport {
  this: UserServiceModule =>

  val userRoutes = pathPrefix("users") {
    path("") {
      post {
        entity(as[UserCreateData]) { data =>
          complete {
            userService.create(data.username,
                               PlaintextPassword(data.password))
                       .toOption
          }
        }
      }
    } ~
    path(PathElement) { username =>
      get {
        rejectEmptyResponse {
          complete(userService.find(username))
        }
      }
    }
  }

}

trait PrivateRoutes extends HttpService {
  this: UserServiceModule =>

  /** Required for the UserServiceAuthenticator **/
  import scala.concurrent.{Future, ExecutionContext}
  implicit def executionContext: ExecutionContext = actorRefFactory.dispatcher 

  import spray.routing.authentication._
  val privateRoutes = path("protected") {
    authenticate(BasicAuth(userAuthenticator, "realm name")) { user =>
      complete("Secret!")
    }
  }

  private class UserServiceAuthenticator extends UserPassAuthenticator[User] {
    def apply(userPassO: Option[UserPass]) = {
      userPassO match {
        case Some(UserPass(username, password)) =>
          Future.successful(userService.authenticate(username, PlaintextPassword(password)))
        case _ => Future.successful(None)
      }
    }
  }

  private val userAuthenticator = new UserServiceAuthenticator()

}

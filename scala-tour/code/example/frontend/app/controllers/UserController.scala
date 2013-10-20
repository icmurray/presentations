package uk.co.sprily.scalaTalk.frontend.controllers

import play.api.data._
import play.api.data.Forms._
import play.api.mvc._

import uk.co.sprily.scalaTalk._

case class UserCreation(username: String, password: String)

object UserController extends Controller {

  private val userService = ModuleRegistry.userService

  def detail(name: String) = Action {
    userService.find(name).map { user =>
      Ok(views.html.userDetails(user))
    }.getOrElse { NotFound("User not found") }
  }

  def create = Action {
    val emptyForm = userCreationForm    // can optionally pre-fill it with data
    Ok(views.html.userCreation(emptyForm))
  }

  def submit = Action { implicit request =>
    userCreationForm.bindFromRequest.fold(
      formErrors => BadRequest(views.html.userCreation(formErrors)),
      userData   => {
        val (username, password) = (userData.username, PlaintextPassword(userData.password))
        userService.create(username, password)
                   .map(creationRedirect)
                   .recover {
                     case UniqueConstraintException => duplicateUserError(userData)
                     case e                         =>
                       InternalServerError("Something went wrong...")
                   }
                   .get
      }
    )
  }

  private def duplicateUserError(userData: UserCreation) = {
    val form = userCreationForm.fill(userData)
    val uniqueError = FormError("name", s"User ${userData.username} already exists.")
    BadRequest(views.html.userCreation(form.copy(errors = uniqueError +: form.errors)))
  }

  private def creationRedirect(user: User) = {
    Redirect(routes.UserController.detail(user.name)).flashing {
      "success" -> s"Created new user: ${user.name}"
    }
  }

  private val userCreationForm = Form(
    mapping(
      "name" -> nonEmptyText,
      "password" -> nonEmptyText
    )(UserCreation.apply)(UserCreation.unapply))
}

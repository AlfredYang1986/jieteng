package controllers

import play.api._
import play.api.mvc._

object Application extends Controller {

  def index = Action {
    Ok(views.html.index("Change page"))
  }
  
  def dazhuPage = Action {
    Ok(views.html.dazhuPage("page 4"))
  }
}
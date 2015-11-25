package controllers

import play.api._
import play.api.mvc._

object Application extends Controller {

  def index = Action {
    Ok(views.html.index("Change page"))
  }
  
  def consultcatePage = Action {
   Ok(views.html.consultcatePage("page 3"))
  }
  
  def dazhuPage = Action {
    Ok(views.html.dazhuPage("page 4"))
  }
  
  def endPage = Action {
    Ok(views.html.endPage("page 5"))
  }

}
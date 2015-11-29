package controllers

import play.api._
import play.api.mvc._
import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue
import scala.io.Source

object JieTengService extends Controller {

	def consultation = Action {
		val data = Json.parse(Source.fromFile("public/data/abc-min.json").bufferedReader.readLine)
		var lst = (data \ "data").asOpt[List[JsValue]].get

		val i = lst.length / 6 + 1
		var rel : List[List[JsValue]] = Nil
		for (index <- 0 to i) {
		  val (a, b) = lst.splitAt(6)
		  rel = rel :+ a
		  lst = b
		}
		Ok(views.html.consultation("Jie Teng She")(rel))
	}
  
  def serviceProtocol = Action {
   Ok(views.html.serviceProtocol("page 2"))
  }
  
  def consultingPage = Action {
    Ok(views.html.consultingPage("page 6"))
  }

  def progress = Action {
   Ok(views.html.progress("page 7"))
  }
}
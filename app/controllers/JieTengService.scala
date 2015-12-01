package controllers

import play.api._
import play.api.mvc._
import play.api.libs.json.Json
import play.api.libs.json.Json.{toJson}
import play.api.libs.json.JsValue
import scala.io.Source
import controllers.common.requestArgsQuery._
import com.mongodb.casbah.Imports._
import java.util.Date
import util.dao._

/**
 * for con = 0: just upload
 * 			 1: read but not reply
 *     		 2: replied
 */
sealed abstract class status(val con : Int, val dis : String)
object queryStatus{
  case object justUpload extends status(0, "Just Uploaded")
  case object readed extends status(1, "readed")
  case object replied extends status(2, "replied")
}


object JieTengService extends Controller {

	def consultation = Action {
		val data = Json.parse(Source.fromFile("public/data/abc-min.json").bufferedReader.readLine)
		var lst = (data \ "data").asOpt[List[JsValue]].get
		var indexing = (data \ "indexing").asOpt[List[JsValue]].get

		val i = lst.length / 6
		var rel : List[List[JsValue]] = Nil
		for (index <- 0 to i) {
		  val (a, b) = lst.splitAt(6)
		  rel = rel :+ a
		  lst = b
		}
		Ok(views.html.consultation("Jie Teng She")(rel)(indexing))
	}
  
	def serviceProtocol = Action {
		Ok(views.html.serviceProtocol("page 2"))
	}
  
	def consultingPage(work_type: String, name: String) = Action {
		Ok(views.html.consultingPage("Jie Teng She")(work_type)(name))
	}

	def progress = Action {
		Ok(views.html.progress("page 7"))
	}

	def pushQueryContent = Action (request => requestArgs(request)(this.pushQueryContentImpl))
	def pushQueryContentImpl(data : JsValue) : JsValue = {

	  println(123)
		val nickName = (data \ "name").asOpt[String].get
		val email = (data \ "email").asOpt[String].get
		val content = (data \ "content").asOpt[String].get
	  println(456)
		
		val builder = MongoDBObject.newBuilder
		
		builder += "name" -> nickName
		builder += "email" -> email
		builder += "content" -> content
		
		builder += "date" -> (new Date).getTime
		builder += "status" -> queryStatus.justUpload.con
		
		_data_connection.getCollection("queries") += builder.result
		
		Json.toJson(Map("status" -> toJson("ok"), "message" -> toJson("发布成功，答主会在三天之内给你答复")))
	}
	
	def queryProgress = Action (request => requestArgs(request)(this.queryProgressImpl))
	def queryProgressImpl(data : JsValue) : JsValue = {
		null
	}
}
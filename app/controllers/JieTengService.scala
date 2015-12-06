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
import module.common.http._
import java.security.MessageDigest
import java.net.URLEncoder

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

	val app_id = "wxb46efccede9f5a76"
	val app_secret = "06ff8eb4765422c073f555284e227a9f"
	val weixin_http = "https://api.weixin.qq.com/cgi-bin/token?grant_type=client_credential&appid=" + app_id + "&secret=" + app_secret
	val weixin_jsapi = "https://api.weixin.qq.com/cgi-bin/ticket/getticket?type=jsapi&access_token="
	val sign = "jsapi_ticket=#(ticket)&noncestr=Wm3WZYTPz0wzccnW&timestamp=#(time)&url=http://localhost:9000/consultingPage"
  
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
		val wechat_token = ((HTTP(weixin_http)).get(null) \ "access_token").asOpt[String].get
		val wechat_jsapi = ((HTTP(weixin_jsapi + wechat_token)).get(null) \ "ticket").asOpt[String].get
		val timespan = (new Date().getTime / 1000).toString
		val str = "jsapi_ticket=" + wechat_jsapi + "&noncestr=Wm3WZYTPz0wzccnW&timestamp=" + timespan + "&url=http://localhost:9000/consultingPage"
		val messageDigest = MessageDigest.getInstance("SHA1");
		messageDigest.update(str.getBytes());
		val signiture = getFormattedText(messageDigest.digest());
		//println(signiture)
	  
		Ok(views.html.consultingPage("Jie Teng She")(work_type)(name)(app_id)(signiture)(timespan))
	}
	
	private def getFormattedText(bytes : Array[Byte]) : String = {
		val sb = new StringBuffer(bytes.length)
		var sTemp : String = ""
		for (i <- 0 to bytes.length - 1) {
		     sTemp = Integer.toHexString(0xFF & bytes.apply(i))
		     if (sTemp.length() < 2) sb.append(0)

		     sb.append(sTemp.toLowerCase)
		}   
		return sb.toString
	}

	def progress = Action {
		Ok(views.html.progress("page 7"))
	}

	def pushQueryContent = Action (request => requestArgs(request)(this.pushQueryContentImpl))
	def pushQueryContentImpl(data : JsValue) : JsValue = {

		val nickName = (data \ "name").asOpt[String].get
		val email = (data \ "email").asOpt[String].get
		val content = (data \ "content").asOpt[String].get
		
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
	
	/**
	 * wechat oauth
	 */
	def queryWechatAuthCode = Action {
		println("auth code")
		val redirect_uri = "http://192.168.1.101:9000/queryWechatOpenID"
		val authCodeUrl = "https://open.weixin.qq.com/connect/oauth2/authorize?appid=" + URLEncoder.encode(app_id) + "&redirect_uri=" + URLEncoder.encode(redirect_uri) + "&response_type=code&scope=snsapi_base"
		println(authCodeUrl)
		
//		(HTTP(authCodeUrl)).get(null)
		Redirect(authCodeUrl)
	 }
	
	def queryWechatOpenID(code: String, status: String) =	Action {
		println("get open id")
		val redirect_uri = "http://localhost:9000/queryWechatOpenID"
		Ok("get wechat open id")
	}
}
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
import scala.xml.XML
import module.sercurity
import scala.io.Source
import java.io._

/**
 * for con = 0: just upload
 * 			 1: read but not reply
 *     		 2: replied
 */
sealed abstract class status(val con : Int, val dis : String)
object queryStatus{
  case object justUpload extends status(0, "已送达")
  case object readed extends status(1, "答主正在答复")
  case object replied extends status(2, "答主已回复")
}


object JieTengService extends Controller {

	/**
	 * wechat app id
	 */
	val app_id = "wxb46efccede9f5a76"
	val app_secret = "06ff8eb4765422c073f555284e227a9f"
	
	/**
	 * wechat business id
	 */
	val mch_id = "1270524501"
//	val mch_key = "RataVageTigreConejoDragon8888888"
	val mch_key = "jietengculturejietengcultureabcd"
	val pay_noncestr = "b927722419c52622651a871d1d9ed8b2"
	val pay_body = "答主咨询费"
	val pay_notify = "http://wxpay.weixin.qq.com/pub_v2/pay/notify.php"
	  
	val weixin_http = "https://api.weixin.qq.com/cgi-bin/token?grant_type=client_credential&appid=" + app_id + "&secret=" + app_secret
	val weixin_jsapi = "https://api.weixin.qq.com/cgi-bin/ticket/getticket?type=jsapi&access_token="
  
	def consultation(openid: String) = Action {
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
		Ok(views.html.consultation("结藤社")(rel)(indexing)(openid))
	}
  
	def serviceProtocol = Action {
		val data = Source.fromFile("public/data/protocol.txt").getLines.toList.map(x => x + "\n").flatMap(x => x).mkString
		Ok(views.html.serviceProtocol("用户协议")(data))
	}
  
	def consultingPage(work_type: String, name: String, openid: String) = Action {
		val wechat_token = ((HTTP(weixin_http)).get(null) \ "access_token").asOpt[String].get
		val wechat_jsapi = ((HTTP(weixin_jsapi + wechat_token)).get(null) \ "ticket").asOpt[String].get
		val timespan = java.lang.Long.toString(System.currentTimeMillis() / 1000)// (new Date().getTime / 1000).toString
		val str_js = "jsapi_ticket=" + wechat_jsapi + "&noncestr=Wm3WZYTPz0wzccnW&timestamp=" + timespan + "&url=http://www.jietengculture.com/wxpay/consultingPage?work_type=" + URLEncoder.encode(work_type) + "&name=" + URLEncoder.encode(name) + "&openid=" + openid;			
		val crypt = MessageDigest.getInstance("SHA-1");
        crypt.reset();
        crypt.update(str_js.getBytes("UTF-8"));
		val signiture = getFormattedText(crypt.digest());
	  
		Ok(views.html.consultingPage("结藤社")(work_type)(name)(app_id)(signiture)(timespan)(null)(openid))
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

	def progress(openid: String) = Action {
		val status = ((from db() in "queries" where ("openid" -> openid)).selectTop(1)("date")(x => x.getAs[Int]("status").map(y => y).getOrElse(-1))).head
		Ok(views.html.progress("咨询进度查询")(status))
	}

	def createPrepayID = Action (request => requestArgs(request)(this.createPrepayIDImpl))
	def createPrepayIDImpl(data : JsValue) : JsValue = {
		val openid = (data \ "openid").asOpt[String].get
		var fee: Int = 0
		(data \ "fee").asOpt[Int].map(x => fee = x).getOrElse(fee = 8000)
		val timespan = java.lang.Long.toString(System.currentTimeMillis() / 1000)// (new Date().getTime / 1000).toString
		/**
		 * get uni order, prepay_id
		 */
		val trade_no = module.sercurity.Sercurity.md5Hash(openid + timespan)
		
		val str_pay = "appid=" + app_id + "&body=" + pay_body + "&mch_id=" + mch_id + "&nonce_str=" + pay_noncestr + "&notify_url="+ pay_notify + "&openid=" + openid + "&out_trade_no=" + trade_no + "&spbill_create_ip=127.0.0.1&total_fee=" + fee + "&trade_type=JSAPI&key=" + mch_key
		val str_md5 = module.sercurity.Sercurity.md5Hash(str_pay).toUpperCase
		val valxml = """<xml><appid>%s</appid><body><![CDATA[%s]]></body><mch_id>%s</mch_id><nonce_str>%s</nonce_str><notify_url>%s</notify_url><openid>%s</openid><out_trade_no>%s</out_trade_no><spbill_create_ip>127.0.0.1</spbill_create_ip><total_fee>%d</total_fee><trade_type>JSAPI</trade_type><sign><![CDATA[%s]]></sign></xml>"""
		  			.format(app_id, pay_body, mch_id, pay_noncestr, pay_notify, openid, trade_no, fee, str_md5)
		
		val order_url = "https://api.mch.weixin.qq.com/pay/unifiedorder"
		val result = ((HTTP(order_url)).post(valxml.toString))
		
		val tag = "prepay_id"
		var prepay_id = result.substring(result.indexOf(tag) + tag.length + 1, result.indexOf("</" + tag)) 
		if (prepay_id.startsWith("<![CDATA[") && prepay_id.endsWith("]]>")) 
			prepay_id = prepay_id.substring(9, prepay_id.length - 3)
		
		val pay_str = "appId=" + app_id + "&nonceStr=" + pay_noncestr + "&package=prepay_id=" + prepay_id + "&signType=MD5&timeStamp=" + timespan + "&key=" + mch_key
		val pay_str_md5 = module.sercurity.Sercurity.md5Hash(pay_str).toUpperCase 
			
		Json.toJson(Map("status" -> toJson("ok"), "package" -> toJson("prepay_id=" + prepay_id), 
		    "out_trade_no" -> toJson(trade_no), "pay_sign" -> toJson(pay_str_md5),
		    "timespan" -> toJson(timespan)))
	}
	
	def pushQueryContent = Action (request => requestArgs(request)(this.pushQueryContentImpl))
	def pushQueryContentImpl(data : JsValue) : JsValue = {

		val open_id = (data \ "openid").asOpt[String].get
		val trade_no = (data \ "trade_no").asOpt[String].get
		val nickName = (data \ "name").asOpt[String].get
		val email = (data \ "email").asOpt[String].get
		val content = (data \ "content").asOpt[String].get
		
		val builder = MongoDBObject.newBuilder

		builder += "openid" -> open_id
		builder += "trade_no" -> trade_no
		
		builder += "name" -> nickName
		builder += "email" -> email
		builder += "content" -> content
		
		builder += "date" -> (new Date).getTime
		builder += "status" -> queryStatus.justUpload.con
		
		_data_connection.getCollection("queries") += builder.result
		
		Json.toJson(Map("status" -> toJson("ok"), "message" -> toJson("发布成功，答主会在三天之内给你答复")))
	}
	

	/**
	 * Dazhu answer page
	 */
	def queryPostedQueries = Action {
		Ok(views.html.queryPostedQueries("回答咨询")(
		   (from db() in "queries" where ("status" -> 0) select { x => 
			x += "status" -> 2.asInstanceOf[AnyRef]
			val td = x.getAs[String]("trade_no").get
			_data_connection.getCollection("queries").update(DBObject("trade_no" -> td), x)
			
			toJson(Map("name" -> toJson(x.getAs[String]("name").get), 
			    "email" -> toJson(x.getAs[String]("email").get), 
			    "content" -> toJson(x.getAs[String]("content").get)))
		   })))
	}
	
	/**
	 * wechat oauth
	 */
	def queryWechatAuthCode = Action {
		val redirect_uri = "http://www.jietengculture.com/queryWechatOpenID"
		val authCodeUrl = "https://open.weixin.qq.com/connect/oauth2/authorize?appid=" + URLEncoder.encode(app_id) + "&redirect_uri=" + URLEncoder.encode(redirect_uri) + "&response_type=code&scope=snsapi_base"
		
		Redirect(authCodeUrl)
	}
	
	def queryWechatOpenID(code: String, status: String) = Action {
		val url = "https://api.weixin.qq.com/sns/oauth2/access_token?appid=" + app_id + "&secret=" + app_secret + "&code=" + code + "&grant_type=authorization_code"
		val openid = ((HTTP(url)).get(null) \ "openid").asOpt[String].get
		
		Redirect("http://www.jietengculture.com/consultation/" + openid)
	}
	
	/**
	 * wechat oauth with query progress
	 */
	def queryProgress = Action {
		val redirect_uri = "http://www.jietengculture.com/queryProgressWithWechatOpenID"
		val authCodeUrl = "https://open.weixin.qq.com/connect/oauth2/authorize?appid=" + URLEncoder.encode(app_id) + "&redirect_uri=" + URLEncoder.encode(redirect_uri) + "&response_type=code&scope=snsapi_base"
		
		Redirect(authCodeUrl)
	}
	
	def queryProgressWithWechatOpenID(code: String, status: String) = Action {
		val url = "https://api.weixin.qq.com/sns/oauth2/access_token?appid=" + app_id + "&secret=" + app_secret + "&code=" + code + "&grant_type=authorization_code"
		val openid = ((HTTP(url)).get(null) \ "openid").asOpt[String].get
		
		Redirect("http://www.jietengculture.com/progress/" + openid)
	}
}
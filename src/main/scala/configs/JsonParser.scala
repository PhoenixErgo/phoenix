package configs

import play.api.libs.json.{JsValue, Json}

object JsonParser {

  def getJsonFromErrorMsg(errorMsg: String): Option[JsValue] = {
    val jsonStart = errorMsg.indexOf("{")
    val jsonEnd = errorMsg.lastIndexOf("}") + 1
    if (jsonStart >= 0 && jsonEnd >= 0 && jsonEnd > jsonStart) {
      val jsonString = errorMsg.substring(jsonStart, jsonEnd)
      Some(Json.parse(jsonString))
    } else {
      None
    }
  }

  def isDoubleSpendingError(json: JsValue): Boolean = {
    val detail: String = (json \ "detail").asOpt[String].getOrElse("")
    detail.contains("Double spending attempt")
  }

  def isTxInMempoolError(json: JsValue): Boolean = {
    val detail: String = (json \ "detail").asOpt[String].getOrElse("")
    detail.contains("it is already in the mempool")
  }

}

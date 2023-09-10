package configs

import com.google.gson.{Gson, GsonBuilder}

import scala.io.Source

object SignedTransactionJsonParser {
  private val gson = new GsonBuilder()
    .setPrettyPrinting()
    .create()

  def read(filePath: String): SignedTransactionJson = {
    val jsonString: String = Source.fromFile(filePath).mkString
    gson.fromJson(jsonString, classOf[SignedTransactionJson])
  }

  def readJsonString(jsonString: String): SignedTransactionJson = {
    gson
      .fromJson(jsonString, classOf[SignedTransactionJson])
  }

  def toJsonString(
      json: SignedTransactionJson,
      prettyPrint: Boolean = true
  ): String = {
    if (prettyPrint) {
      val gson = new Gson()
      gson.toJson(json)
    }
    this.gson.toJson(json)
  }

  def toJsonStringFromArray(
      json: Array[SignedTransactionJson],
      prettyPrint: Boolean = true
  ): String = {
    if (prettyPrint) {
      val gson = new Gson()
      gson.toJson(json)
    }
    this.gson.toJson(json)
  }

}

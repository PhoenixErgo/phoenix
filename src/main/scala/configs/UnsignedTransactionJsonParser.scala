package configs

import com.google.gson.{Gson, GsonBuilder}

import scala.io.Source

object UnsignedTransactionJsonParser {
  private val gson = new GsonBuilder()
    .setPrettyPrinting()
    .create()

  def read(filePath: String): UnsignedTransactionJson = {
    val jsonString: String = Source.fromFile(filePath).mkString
    gson.fromJson(jsonString, classOf[UnsignedTransactionJson])
  }

  def readJsonString(jsonString: String): UnsignedTransactionJson = {
    gson
      .fromJson(jsonString, classOf[UnsignedTransactionJson])
  }

  def toJsonString(
      json: UnsignedTransactionJson,
      prettyPrint: Boolean = true
  ): String = {
    if (prettyPrint) {
      val gson = new Gson()
      gson.toJson(json)
    }
    this.gson.toJson(json)
  }

}

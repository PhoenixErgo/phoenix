package configs

import com.google.gson.{Gson, GsonBuilder, JsonElement}

import java.io.{FileWriter, Writer}
import scala.io.Source

case class TokenContractsConfig(
    Contracts: TokenConfig
)

case class TokenBankContract(
    contract: String,
    bankSingleton: String,
    hodlCoin: String,
    baseToken: String
)

case class TokenConfig(
    bankContract: TokenBankContract,
    proxyContract: ProxyContract,
    feeContract: FeeContract
)

class HodlTokenConf(
    bankContract: String,
    bankSingleton: String,
    hodlCoin: String,
    baseToken: String,
    feeContract: String,
    proxyContract: String
) {
  val bankContractInstance: TokenBankContract =
    TokenBankContract(bankContract, bankSingleton, hodlCoin, baseToken)
  val feeContractInstance: FeeContract =
    FeeContract(feeContract)
  val proxyContractInstance: ProxyContract = ProxyContract(proxyContract)

  val conf = TokenConfig(
    bankContractInstance,
    proxyContractInstance,
    feeContractInstance
  )
  val newConfig: TokenContractsConfig = TokenContractsConfig(conf)
  private val gson = new GsonBuilder().setPrettyPrinting().create()

  def write(filePath: String): Unit = {
    val writer: Writer = new FileWriter(filePath)
    writer.write(this.gson.toJson(this.newConfig))
    writer.close()
  }

  def read(filePath: String): ContractsConfig = {
    val jsonString: String = Source.fromFile(filePath).mkString
    gson.fromJson(jsonString, classOf[ContractsConfig])
  }

}

object tokenConf {
  private val gson = new GsonBuilder().setPrettyPrinting().create()

  def read(filePath: String): TokenContractsConfig = {
    val jsonString: String = Source.fromFile(filePath).mkString
    gson.fromJson(jsonString, classOf[TokenContractsConfig])
  }

  def write(filePath: String, newConfig: TokenContractsConfig): Unit = {
    val writer: Writer = new FileWriter(filePath)
    writer.write(this.gson.toJson(newConfig))
    writer.close()
  }

}

package utils

import configs.serviceOwnerConf
import explorer.Explorer
import org.ergoplatform.ErgoBox
import org.ergoplatform.appkit.InputBox
import org.ergoplatform.appkit.impl.{InputBoxImpl, ScalaBridge}
import org.ergoplatform.explorer.client.model.{
  InputInfo,
  OutputInfo,
  TransactionInfo
}
import org.ergoplatform.explorer.client.{DefaultApi, ExplorerApiClient}
import org.ergoplatform.restapi.client._

import java.util
import scala.collection.JavaConversions._

class explorerApi(
    apiUrl: String = serviceOwnerConf.read("serviceOwner.json").apiUrl,
    nodeUrl: String = serviceOwnerConf.read("serviceOwner.json").nodeUrl
) extends Explorer(
      nodeInfo = execute.DefaultNodeInfo(
        nodeUrl,
        apiUrl,
        new network(
          serviceOwnerConf.read("serviceOwner.json").nodeUrl
        ).getNetworkType
      )
    ) {

  def getExplorerApi(apiUrl: String): DefaultApi = {
    new ExplorerApiClient(apiUrl).createService(classOf[DefaultApi])
  }

  def buildNodeService(nodeUrl: String): ApiClient = {
    new ApiClient(nodeUrl)
  }

  def getUnspentBoxFromTokenID(tokenId: String): Option[OutputInfo] = {
    val api = this.getExplorerApi(this.apiUrl)
    val res =
      api.getApiV1BoxesUnspentBytokenidP1(tokenId, 0, 1).execute().body()
    try {
      Some(res.getItems.get(0))
    } catch {
      case e: Exception =>
        println("error getting unspent box from tokenId")
        None
    }
  }

  def getBoxesFromTokenID(tokenId: String): Option[OutputInfo] = {
    val api = this.getExplorerApi(this.apiUrl)
    try {
      val res = api.getApiV1BoxesBytokenidP1(tokenId, 0, 1).execute().body()
      val offset = res.getTotal - 1
      Some(
        api
          .getApiV1BoxesBytokenidP1(tokenId, offset, 1)
          .execute()
          .body()
          .getItems
          .get(0)
      )
    } catch {
      case e: Exception =>
        println("error getting boxes from token id")
        None
    }
  }

  def getBoxesfromTransaction(txId: String): Option[TransactionInfo] = {
    val api = this.getExplorerApi(this.apiUrl)
    try {
      Some(api.getApiV1TransactionsP1(txId).execute().body())
    } catch {
      case e: Exception =>
        println("error getting boxes from transaction")
        None
    }
  }

  def getAddressInfo(address: String): Option[util.List[OutputInfo]] = {
    val api = this.getExplorerApi(this.apiUrl)
    try {
      Some(
        api.getApiV1BoxesByaddressP1(address, 0, 100).execute().body().getItems
      )
    } catch {
      case e: Exception =>
        println("error getting boxes from transaction")
        None
    }
  }

  def getUnspentBoxesByAddress(
      address: String
  ): Option[util.List[OutputInfo]] = {
    val api = this.getExplorerApi(this.apiUrl)
    try {
      Some(
        api.getApiV1BoxesByaddressP1(address, 0, 100).execute().body().getItems
      )
    } catch {
      case e: Exception =>
        println("error getting unspent boxes by address")
        None
    }
  }

  def getBoxesfromUnconfirmedTransaction(
      txId: String
  ): Either[ErgoTransaction, Option[TransactionInfo]] = {
    try {
      val nodeService = this
        .buildNodeService(this.nodeUrl)
        .createService(classOf[TransactionsApi])
      val res = nodeService.getUnconfirmedTransactionById(txId).execute()

      if (res.code() == 404) {
        return Right(this.getBoxesfromTransaction(txId))
      }

      if (res.body() != null) {
        Left(res.body())
      } else {
        Right(None)
      }
    } catch {
      case e: Exception =>
        e.printStackTrace()
        Right(None)
    }
  }

  def getUnspentBoxFromMempool(boxId: String): Option[InputBox] = {
    try {
      val nodeService =
        this.buildNodeService(this.nodeUrl).createService(classOf[UtxoApi])
      val response = nodeService.getBoxWithPoolById(boxId).execute().body()
      if (response == null) {
        this.getErgoBoxfromID(boxId) match {
          case Right(ergoBox) =>
            Some(new InputBoxImpl(ergoBox).asInstanceOf[InputBox])
          case Left(error) =>
            println(s"Error: $error")
            None
        }
      } else {
        Some(new InputBoxImpl(response).asInstanceOf[InputBox])
      }
    } catch {
      case e: Exception =>
        e.printStackTrace()
        None
    }
  }

  def getMem(boxId: String): Boolean = {
    val nodeService =
      this.buildNodeService(this.nodeUrl).createService(classOf[UtxoApi])
    val response = nodeService.getBoxWithPoolById(boxId).execute().body()
    if (response == null) {
      return false
    }
    true
  }

  def getBoxbyIDfromExplorer(boxID: String): Option[OutputInfo] = {
    try {
      val api = this.getExplorerApi(this.apiUrl)
      Some(api.getApiV1BoxesP1(boxID).execute().body())
    } catch {
      case e: Exception =>
        println("error getting box id from explorer")
        None
    }
  }

  def getErgoBoxfromID(boxID: String): Either[String, ErgoBox] = {
    try {
      val nodeService =
        this.buildNodeService(this.nodeUrl).createService(classOf[UtxoApi])
      val response: ErgoTransactionOutput =
        nodeService.getBoxWithPoolById(boxID).execute().body()

      if (response == null) {
        val box = this
          .getBoxbyIDfromExplorer(boxID)
          .getOrElse(throw new Exception("No Box found from explorer"))
        val tokens = new util.ArrayList[Asset](box.getAssets.size)
        for (asset <- box.getAssets) {
          tokens.add(
            new Asset().tokenId(asset.getTokenId).amount(asset.getAmount)
          )
        }
        val registers = new Registers
        for (registerEntry <- box.getAdditionalRegisters.entrySet) {
          registers.put(
            registerEntry.getKey,
            registerEntry.getValue.serializedValue
          )
        }
        val boxConversion: ErgoTransactionOutput = new ErgoTransactionOutput()
          .ergoTree(box.getErgoTree)
          .boxId(box.getBoxId)
          .index(box.getIndex)
          .value(box.getValue)
          .transactionId(box.getTransactionId)
          .creationHeight(box.getCreationHeight)
          .assets(tokens)
          .additionalRegisters(registers)
        return Right(ScalaBridge.isoErgoTransactionOutput.to(boxConversion))
      }
      val tokens = new util.ArrayList[Asset](response.getAssets.size)
      for (asset <- response.getAssets) {
        tokens.add(
          new Asset().tokenId(asset.getTokenId).amount(asset.getAmount)
        )
      }
      val registers = new Registers
      for (registerEntry <- response.getAdditionalRegisters.entrySet()) {
        registers.put(registerEntry.getKey, registerEntry.getValue)
      }
      val boxConversion: ErgoTransactionOutput = new ErgoTransactionOutput()
        .ergoTree(response.getErgoTree)
        .boxId(response.getBoxId)
        .index(response.getIndex)
        .value(response.getValue)
        .transactionId(response.getTransactionId)
        .creationHeight(response.getCreationHeight)
        .assets(tokens)
        .additionalRegisters(registers)
      Right(ScalaBridge.isoErgoTransactionOutput.to(boxConversion))
    } catch {
      case e: Exception =>
        e.printStackTrace()
        Left(e.getMessage)
    }
  }

  def getErgoBoxfromIDNoApi(box: InputInfo): Either[String, ErgoBox] = {
    try {
      val tokens = new util.ArrayList[Asset](box.getAssets.size)
      for (asset <- box.getAssets) {
        tokens.add(
          new Asset().tokenId(asset.getTokenId).amount(asset.getAmount)
        )
      }
      val registers = new Registers
      for (registerEntry <- box.getAdditionalRegisters.entrySet) {
        registers.put(
          registerEntry.getKey,
          registerEntry.getValue.serializedValue
        )
      }
      val boxConversion: ErgoTransactionOutput = new ErgoTransactionOutput()
        .ergoTree(box.getErgoTree)
        .boxId(box.getBoxId)
        .index(box.getIndex)
        .value(box.getValue)
        .transactionId(null)
        .creationHeight(null)
        .assets(tokens)
        .additionalRegisters(registers)
      Right(ScalaBridge.isoErgoTransactionOutput.to(boxConversion))
    } catch {
      case e: Exception =>
        e.printStackTrace()
        Left(e.getMessage)
    }
  }

}

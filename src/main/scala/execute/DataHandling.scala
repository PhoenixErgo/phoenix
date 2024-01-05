package execute

import org.ergoplatform.appkit.{Address, InputBox, NetworkType}
import org.ergoplatform.sdk.ErgoToken
import special.collection.Coll
import utils.BoxJson

import scala.collection.JavaConverters._

case class ExtractionResult(
    recipientAddress: Option[Address],
    hodlSingleton: Option[ErgoToken],
    hodlTokenId: Option[Array[Byte]],
    totalTokenSupply: Option[Long],
    precisionFactor: Option[Long],
    minBankValue: Option[Long],
    devFee: Option[Long],
    bankFee: Option[Long],
    minBoxValue: Option[Long],
    minerFee: Option[Long],
    txOperatorFee: Option[Long]
)

object DataHandling {
  def extractInputData(
      proxyInput: InputBox,
      currentBankInput: InputBox,
      networkType: NetworkType
  ): Either[String, ExtractionResult] = {

    import scala.util.control.Exception._

    try {

      val recipientAddress = allCatch
        .opt(
          proxyInput.getRegisters
            .get(0)
            .getValue
            .asInstanceOf[special.sigma.SigmaProp]
        )
        .map(
          new org.ergoplatform.appkit.SigmaProp(_).toAddress(networkType)
        )

      val hodlSingleton =
        allCatch
          .opt(proxyInput.getRegisters.get(1).getValue.asInstanceOf[Coll[Byte]])
          .map(bytes => new ErgoToken(bytes.toArray, 1L))

      val hodlTokenId =
        allCatch
          .opt(proxyInput.getRegisters.get(2).getValue.asInstanceOf[Coll[Byte]])
          .map(_.toArray)

      val totalTokenSupply = allCatch.opt(
        currentBankInput.getRegisters.get(0).getValue.asInstanceOf[Long]
      )
      val precisionFactor = allCatch.opt(
        currentBankInput.getRegisters.get(1).getValue.asInstanceOf[Long]
      )
      val minBankValue = allCatch.opt(
        currentBankInput.getRegisters.get(2).getValue.asInstanceOf[Long]
      )
      val devFee = allCatch.opt(
        currentBankInput.getRegisters.get(3).getValue.asInstanceOf[Long]
      )
      val bankFee = allCatch.opt(
        currentBankInput.getRegisters.get(4).getValue.asInstanceOf[Long]
      )

      val minBoxValue = allCatch.opt(
        proxyInput.getRegisters.get(3).getValue.asInstanceOf[Long]
      )
      val minerFee = allCatch.opt(
        proxyInput.getRegisters.get(4).getValue.asInstanceOf[Long]
      )
      val txOperatorFee = allCatch.opt(
        proxyInput.getRegisters.get(5).getValue.asInstanceOf[Long]
      )

      Right(
        ExtractionResult(
          recipientAddress,
          hodlSingleton,
          hodlTokenId,
          totalTokenSupply,
          precisionFactor,
          minBankValue,
          devFee,
          bankFee,
          minBoxValue,
          minerFee,
          txOperatorFee
        )
      )
    } catch {
      case e: Exception =>
        Left(s"Failed to extract input data due an exception: ${e.getMessage}")
    }
  }

  def sortProxyInputs(
      inputs: Seq[InputBox],
      singleton: ErgoToken
  ): (Seq[InputBox], Seq[InputBox]) = {
    val (burnInputs, mintInputs) =
      inputs.partition(box => {
        // if hodl tokens exists it goes to the first variable
        val dummyHodlToken = new ErgoToken(
          box.getRegisters.get(2).getValue.asInstanceOf[Coll[Byte]].toArray,
          1L
        )
        box.getTokens.asScala
          .exists(t => t.getId.toString() == dummyHodlToken.getId.toString())
      })

    def sortBySingleton(box: InputBox): Boolean = {
      new ErgoToken(
        box.getRegisters.get(1).getValue.asInstanceOf[Coll[Byte]].toArray,
        1L
      ).getId.toString() == singleton.getId.toString()
    }

    (mintInputs.filter(sortBySingleton), burnInputs.filter(sortBySingleton))
  }

  def validateBox(
      box: BoxJson,
      minBoxValue: Long,
      minerFee: Long,
      minTxOperatorFee: Long
  ): Boolean = {
    box.additionalRegisters.R4 != null &&
    box.additionalRegisters.R5.serializedValue != null &&
    box.additionalRegisters.R6.serializedValue != null &&
    box.additionalRegisters.R7.renderedValue.toLong >= minBoxValue &&
    box.additionalRegisters.R8.renderedValue.toLong >= minerFee &&
    box.additionalRegisters.R9.renderedValue.toLong >= minTxOperatorFee
  }

  def validateTokenBox(
      box: BoxJson,
      minBoxValue: Long,
      minerFee: Long,
      minTxOperatorFee: Long,
      tokens: Array[ErgoToken]
  ): Boolean = {
    box.additionalRegisters.R4 != null &&
    box.additionalRegisters.R5.serializedValue != null &&
    box.additionalRegisters.R6.serializedValue != null &&
    box.additionalRegisters.R7.renderedValue.toLong >= minBoxValue &&
    box.additionalRegisters.R8.renderedValue.toLong >= minerFee &&
    box.additionalRegisters.R9.renderedValue.toLong >= minTxOperatorFee &&
    tokens.exists(token =>
      box.assets.map(_.tokenId).contains(token.getId.toString())
    )
  }

}

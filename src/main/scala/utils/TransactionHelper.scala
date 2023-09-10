package utils

import configs.JsonParser.{
  getJsonFromErrorMsg,
  isDoubleSpendingError,
  isTxInMempoolError
}
import configs.{SignedTransactionJsonParser, UnsignedTransactionJsonParser}
import org.ergoplatform.appkit.{
  Address,
  BlockchainContext,
  InputBox,
  Mnemonic,
  OutBox,
  SignedTransaction,
  UnsignedTransaction
}
import org.ergoplatform.sdk.{ErgoToken, SecretString}
import play.api.libs.json.{JsValue, Json}

import scala.collection.JavaConversions.`deprecated asScalaBuffer`

class DoubleSpendingError(message: String) extends Exception(message)
class TransactionInMempool(message: String) extends Exception(message)

class TransactionHelper(
    ctx: BlockchainContext,
    walletMnemonic: String,
    mnemonicPassword: String = ""
) {
  private val mnemonic = Mnemonic.create(
    SecretString.create(walletMnemonic),
    SecretString.create(mnemonicPassword)
  )
  private val txBuilder = this.ctx.newTxBuilder()

  val senderAddress: Address = Address.createEip3Address(
    0,
    ctx.getNetworkType,
    SecretString.create(walletMnemonic),
    SecretString.create(mnemonicPassword),
    false
  )

  private val minAmount = 1000000L

  def buildUnsignedTransaction(
      inputs: Seq[InputBox],
      outputs: Seq[OutBox],
      dataInputs: Seq[InputBox] = Seq.empty,
      tokensToBurn: Seq[ErgoToken] = Seq.empty,
      fee: Long = minAmount
  ): UnsignedTransaction = {
    val builder = this.ctx
      .newTxBuilder()
      .addInputs(inputs: _*)
      .addOutputs(outputs: _*)
      .fee(fee)
      .sendChangeTo(this.senderAddress)

    if (dataInputs.nonEmpty) builder.addDataInputs(dataInputs: _*)
    if (tokensToBurn.nonEmpty) builder.tokensToBurn(tokensToBurn: _*)

    builder.build()
  }

  def signTransaction(
      unsignedTransaction: UnsignedTransaction,
      proverIndex: Int = 0
  ): SignedTransaction = {
    val prover = this.ctx
      .newProverBuilder()
      .withMnemonic(mnemonic, false)
      .withEip3Secret(proverIndex)
      .build()
    prover.sign(unsignedTransaction)
  }
  def getUnsignedJson(
      unsignedTransaction: UnsignedTransaction,
      prettyPrint: Boolean = true
  ): String = {
    val unsignedJson = UnsignedTransactionJsonParser.readJsonString(
      unsignedTransaction.toJson(prettyPrint)
    )
    unsignedTransaction.getOutputs.zipWithIndex.foreach { case (out, index) =>
      unsignedJson.outputs(index).ergoTree = out.getErgoTree.bytesHex
    }
    UnsignedTransactionJsonParser.toJsonString(unsignedJson, prettyPrint)
  }

  def getSignedJson(
      signedTransaction: SignedTransaction,
      prettyPrint: Boolean = true
  ): String = {
    val signedJson = SignedTransactionJsonParser.readJsonString(
      signedTransaction.toJson(prettyPrint)
    )
    signedTransaction.getOutputs.zipWithIndex.foreach { case (out, index) =>
      signedJson.outputs(index).ergoTree = out.getErgoTree.bytesHex
    }
    SignedTransactionJsonParser.toJsonString(signedJson, prettyPrint)
  }

  def sendTx(signedTransaction: SignedTransaction): String = {
    try {
      this.ctx.sendTransaction(signedTransaction)
    } catch {
      case e: Exception =>
        val errorMsg = Option(e.getMessage).getOrElse("")
        getJsonFromErrorMsg(errorMsg) match {
          case Some(json) if isDoubleSpendingError(json) =>
            throw new DoubleSpendingError("double-spending-error")
          case Some(json) if isTxInMempoolError(json) =>
            throw new TransactionInMempool("tx-in-mempool-error")
          case _ => throw e
        }
    }
  }

  def createToken(
      receiver: Address,
      amountList: Seq[Long],
      inputBox: Option[Seq[InputBox]] = None,
      sender: Address = this.senderAddress,
      isCollection: Boolean = false,
      name: String,
      description: String,
      tokenAmount: Long,
      tokenDecimals: Int
  ): SignedTransaction = {
    val inBox: Seq[InputBox] = inputBox.getOrElse(
      new InputBoxes(ctx).getInputs(amountList, sender)
    )
    val outBoxObj = new OutBoxes(this.ctx)

    val token = if (isCollection) {
      outBoxObj.collectionTokenHelper(
        inBox.head,
        name,
        description,
        tokenAmount,
        tokenDecimals
      )
    } else {
      outBoxObj.tokenHelper(
        inBox.head,
        name,
        description,
        tokenAmount,
        tokenDecimals
      )
    }

    val outBox =
      outBoxObj.tokenMintOutBox(token, receiver, amount = amountList.head)
    val unsignedTransaction =
      this.buildUnsignedTransaction(inBox, Seq(outBox))

    this.signTransaction(unsignedTransaction)
  }

}

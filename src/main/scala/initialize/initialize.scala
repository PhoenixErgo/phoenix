package initialize

import configs.{HodlTokenConf, conf, serviceOwnerConf}
import contracts.PhoenixContracts
import execute.Client
import org.ergoplatform.appkit.{Address, ErgoContract, Parameters}
import org.ergoplatform.sdk.ErgoToken
import utils.{ContractCompile, InputBoxes, OutBoxes, TransactionHelper}

object initialize extends App {
  private val client: Client = new Client()
  client.setClient
  val ctx = client.getContext
  val serviceFilePath = "serviceOwner.json"
  val contractConfFilePath = "contracts.json"
  val serviceConf = serviceOwnerConf.read(serviceFilePath)

  val walletMnemonic = serviceConf.txOperatorMnemonic
  val walletMnemonicPw = serviceConf.txOperatorMnemonicPw
  val txHelper =
    new TransactionHelper(this.ctx, walletMnemonic, walletMnemonicPw)

  val inputBoxesObj = new InputBoxes(ctx)
  val outBoxObj = new OutBoxes(ctx)
  val compiler = new ContractCompile(ctx)

  private val feeScript
      : String = // make sure to change depending on testnet or mainnet
    PhoenixContracts.phoenix_v1_hodlcoin_feeTest.contractScript

  private val phoenixScript: String =
    PhoenixContracts.phoenix_v1_hodlcoin_bank.contractScript

  private val feeContract: ErgoContract = compiler.compileFeeContract(
    feeScript,
    serviceConf.minMinerFee
  )

  private val phoenixContract: ErgoContract =
    compiler.compileBankContract(phoenixScript, feeContract)

  private val proxyAddress = compiler
    .compileProxyContract(
      PhoenixContracts.phoenix_v1_hodlcoin_proxy.contractScript,
      serviceConf.minTxOperatorFee
    )
    .toAddress

  val hodlDecimals = 9
  val totalTokenSupply = (97739924 * math.pow(10, hodlDecimals)).toLong
  val precisionFactor = 1000000000L
  val minBankValue = 1000000L
  val bankFeeNum = 30L
  val devFeeNum = 3L

  val hodlDecimal = 9

  val genesisInput =
    inputBoxesObj.getInputs(
      Array(2 * Parameters.OneErg),
      txHelper.senderAddress
    )

  val singleton = outBoxObj.tokenHelper(
    genesisInput.head,
    "Phoenix hodlERG3 Bank Singleton",
    "Phoenix hodlERG3 bank identification token",
    1L,
    0
  )

  val singletonMintOutput =
    outBoxObj.tokenMintOutBox(singleton, txHelper.senderAddress)

  val unsignedSingletonMintTx = txHelper.buildUnsignedTransaction(
    inputs = genesisInput,
    outputs = Array(singletonMintOutput)
  )

  val singletonMintSignedTransaction =
    txHelper.signTransaction(unsignedSingletonMintTx)

  val singletonMintTxHash = txHelper.sendTx(singletonMintSignedTransaction)

  println("Singleton Mint Transaction: " + singletonMintTxHash)

  val hodlTokenMintInput =
    singletonMintSignedTransaction.getOutputsToSpend.get(2)

  val hodlTokens = outBoxObj.tokenHelper(
    hodlTokenMintInput,
    "hodlERG3",
    "The Phoenix Finance implementation of the hodlCoin protocol: hodlERG 3%",
    totalTokenSupply,
    hodlDecimal
  )

  val hodlTokenMintOutput =
    outBoxObj.tokenMintOutBox(hodlTokens, txHelper.senderAddress)

  val unsignedHodlMintTx = txHelper.buildUnsignedTransaction(
    inputs = Array(hodlTokenMintInput),
    outputs = Array(hodlTokenMintOutput)
  )

  val hodlMintSignedTx = txHelper.signTransaction(unsignedHodlMintTx)

  val hodlMintTxHash = txHelper.sendTx(hodlMintSignedTx)

  println("Hodl Mint Transaction: " + hodlMintTxHash)

  val hodlBoxInput = Array(
    singletonMintSignedTransaction.getOutputsToSpend.get(0),
    hodlMintSignedTx.getOutputsToSpend.get(0),
    hodlMintSignedTx.getOutputsToSpend.get(2)
  )

  val hodlBoxOutput = outBoxObj.hodlBankBox(
    phoenixContract,
    singleton,
    new ErgoToken(
      hodlMintSignedTx.getOutputsToSpend.get(0).getTokens.get(0).getId.toString,
      totalTokenSupply - (1L * math.pow(10, hodlDecimals)).toLong
    ),
    totalTokenSupply,
    precisionFactor,
    minBankValue,
    bankFeeNum,
    devFeeNum,
    Parameters.OneErg
  )

  val unsignedHodlBoxTx = txHelper.buildUnsignedTransaction(
    inputs = hodlBoxInput,
    outputs = Array(hodlBoxOutput),
    tokensToBurn = Array(
      new ErgoToken(
        hodlMintSignedTx.getOutputsToSpend
          .get(0)
          .getTokens
          .get(0)
          .getId
          .toString,
        (1L * math.pow(10, hodlDecimals)).toLong
      )
    )
  )

  val signedHodlBoxTx = txHelper.signTransaction(unsignedHodlBoxTx)

  val hodlBoxTxHash = txHelper.sendTx(signedHodlBoxTx)

  println("Hodl Box Transaction: " + hodlBoxTxHash)

  new conf(
    phoenixContract.toAddress.toString,
    singletonMintSignedTransaction.getOutputsToSpend
      .get(0)
      .getTokens
      .get(0)
      .getId
      .toString,
    hodlMintSignedTx.getOutputsToSpend.get(0).getTokens.get(0).getId.toString,
    feeContract.toAddress.toString,
    proxyAddress.toString
  ).write(contractConfFilePath)
}

object mintToken extends App {
  private val client: Client = new Client()
  client.setClient
  val ctx = client.getContext
  val serviceFilePath = "serviceOwner.json"
  val contractConfFilePath = "contracts.json"
  val serviceConf = serviceOwnerConf.read(serviceFilePath)

  val walletMnemonic = serviceConf.txOperatorMnemonic
  val walletMnemonicPw = serviceConf.txOperatorMnemonicPw
  val txHelper =
    new TransactionHelper(this.ctx, walletMnemonic, walletMnemonicPw)

  val inputBoxesObj = new InputBoxes(ctx)
  val outBoxObj = new OutBoxes(ctx)

  val genesisInput =
    inputBoxesObj
      .getBoxesById(
        ""
      )

  val totalTokenSupply = 21000000000L
  val decimal = 0

  val hodlTokens = outBoxObj.tokenHelper(
    genesisInput.head,
    "Comet",
    "Comet",
    totalTokenSupply,
    decimal
  )

  val tokenMintOutput =
    outBoxObj.tokenMintOutBox(hodlTokens, txHelper.senderAddress)

  val unsignedMintTx = txHelper.buildUnsignedTransaction(
    inputs = genesisInput,
    outputs = Array(tokenMintOutput),
    fee = 100000000L
  )

  val mintTx = txHelper.sendTx(txHelper.signTransaction(unsignedMintTx))

  println("Mint Transaction: " + mintTx)

}

object initializeToken extends App {
  private val client: Client = new Client()
  client.setClient
  val ctx = client.getContext
  val serviceFilePath = "serviceOwner.json"
  val contractConfFilePath = "contracts.json"
  val serviceConf = serviceOwnerConf.read(serviceFilePath)

  val walletMnemonic = serviceConf.txOperatorMnemonic
  val walletMnemonicPw = serviceConf.txOperatorMnemonicPw
  val txHelper =
    new TransactionHelper(this.ctx, walletMnemonic, walletMnemonicPw)

  val inputBoxesObj = new InputBoxes(ctx)
  val outBoxObj = new OutBoxes(ctx)
  val compiler = new ContractCompile(ctx)

  private val feeScript
      : String = // make sure to change depending on testnet or mainnet
    PhoenixContracts.phoenix_v1_hodltoken_fee.contractScript

  private val phoenixScript: String =
    PhoenixContracts.phoenix_v1_hodltoken_bank.contractScript

  val minBankValue =
    1L // this can change; when deploying you need one unit respective to decimal count
  // if decimals are 2 then 100L
  // if decimals are 0 then 1L and so on

  val comet = new ErgoToken(
    "0cd8c9f416e5b1ca9f986a7f10a84191dfb85941619e49e53c0dc30ebf83324b",
    minBankValue
  )

  val cometCreatorAddress =
    Address.create("9h6Ao31CVSsYisf4pWTM43jv6k3BaXV3jovGfaRj9PrqfYms6Rf")

  private val feeContract: ErgoContract = compiler.compileFeeTokenContract(
    feeScript,
    serviceConf.minMinerFee,
    comet,
    25L,
    60L,
    15L,
    66L,
    cometCreatorAddress
  )

  private val phoenixContract: ErgoContract =
    compiler.compileTokenBankContract(phoenixScript, feeContract)

  private val proxyAddress = compiler
    .compileProxyContract(
      PhoenixContracts.phoenix_v1_hodltoken_proxy.contractScript,
      serviceConf.minTxOperatorFee
    )
    .toAddress

  val totalTokenSupply = 21000000000L
  val precisionFactor = 1L
  val bankFeeNum = 100L
  val devFeeNum = 30L

  private val hodlDecimal = 0

//  val genesisInput =
//    inputBoxesObj.getInputs(
//      Array(2 * Parameters.OneErg),
//      txHelper.senderAddress
//    )
  val genesisInput =
    inputBoxesObj
      .getBoxesById(
        ""
      )

  val singleton = outBoxObj.tokenHelper(
    genesisInput.head,
    "Phoenix hodlComet10 Bank Singleton",
    "Phoenix hodlComet10 bank identification token",
    1L,
    0
  )

  val singletonMintOutput =
    outBoxObj.tokenMintOutBox(singleton, txHelper.senderAddress)

  val unsignedSingletonMintTx = txHelper.buildUnsignedTransaction(
    inputs = genesisInput,
    outputs = Array(singletonMintOutput)
  )

  val singletonMintSignedTransaction =
    txHelper.signTransaction(unsignedSingletonMintTx)

  val singletonMintTxHash = txHelper.sendTx(singletonMintSignedTransaction)

  println("Singleton Mint Transaction: " + singletonMintTxHash)

  val hodlTokenMintInput =
    singletonMintSignedTransaction.getOutputsToSpend.get(2)

  val hodlTokens = outBoxObj.tokenHelper(
    hodlTokenMintInput,
    "hodlCOMET10",
    "The Phoenix Finance implementation of the hodlToken protocol: hodlCOMET 10%",
    totalTokenSupply,
    hodlDecimal
  )

  val hodlTokenMintOutput =
    outBoxObj.tokenMintOutBox(hodlTokens, txHelper.senderAddress)

  val unsignedHodlMintTx = txHelper.buildUnsignedTransaction(
    inputs = Array(hodlTokenMintInput),
    outputs = Array(hodlTokenMintOutput)
  )

  val hodlMintSignedTx = txHelper.signTransaction(unsignedHodlMintTx)

  val hodlMintTxHash = txHelper.sendTx(hodlMintSignedTx)

  println("Hodl COMET Transaction: " + hodlMintTxHash)

  val hodlBoxInput = Array(
    inputBoxesObj
      .getBoxesById(
        ""
      )
      .head,
    singletonMintSignedTransaction.getOutputsToSpend.get(0),
    hodlMintSignedTx.getOutputsToSpend.get(0),
    hodlMintSignedTx.getOutputsToSpend.get(2)
  )

  val hodlBoxOutput = outBoxObj.hodlBankBox(
    phoenixContract,
    singleton,
    new ErgoToken(
      hodlMintSignedTx.getOutputsToSpend.get(0).getTokens.get(0).getId.toString,
      totalTokenSupply - (1L * math.pow(10, hodlDecimal)).toLong
    ),
    totalTokenSupply,
    precisionFactor,
    minBankValue,
    bankFeeNum,
    devFeeNum,
    Parameters.OneErg,
    Some(comet)
  )

  val unsignedHodlBoxTx = txHelper.buildUnsignedTransaction(
    inputs = hodlBoxInput,
    outputs = Array(hodlBoxOutput),
    tokensToBurn = Array(
      new ErgoToken(
        hodlMintSignedTx.getOutputsToSpend
          .get(0)
          .getTokens
          .get(0)
          .getId
          .toString,
        (1L * math.pow(10, hodlDecimal)).toLong
      )
    )
  )

  val signedHodlBoxTx = txHelper.signTransaction(unsignedHodlBoxTx)

  val hodlBoxTxHash = txHelper.sendTx(signedHodlBoxTx)

  println("Hodl Box Transaction: " + hodlBoxTxHash)

  new HodlTokenConf(
    phoenixContract.toAddress.toString,
    singletonMintSignedTransaction.getOutputsToSpend
      .get(0)
      .getTokens
      .get(0)
      .getId
      .toString,
    hodlMintSignedTx.getOutputsToSpend.get(0).getTokens.get(0).getId.toString,
    comet.getId.toString(),
    feeContract.toAddress.toString,
    proxyAddress.toString
  ).write(contractConfFilePath)
}

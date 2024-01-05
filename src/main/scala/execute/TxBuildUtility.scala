package execute

import contracts.PhoenixContracts
import execute.DataHandling.extractInputData
import execute.HodlCalulations.{
  burnAmount,
  burnTokenAmount,
  hodlMintAmountFromERG,
  hodlTokenMintAmount
}
import org.ergoplatform.appkit.{
  Address,
  BlockchainContext,
  ErgoContract,
  InputBox,
  NetworkType
}
import org.ergoplatform.sdk.ErgoToken
import utils.{
  ContractCompile,
  DoubleSpendingError,
  OutBoxes,
  TransactionHelper,
  TransactionInMempool,
  explorerApi
}

import scala.collection.JavaConverters._

class TxBuildUtility(
    val ctx: BlockchainContext,
    txOperatorMnemonic: String,
    txOperatorMnemonicPw: String,
    minMinerFee: Long
) {

  val outBoxObj = new OutBoxes(ctx)
  val txHelper = new TransactionHelper(
    ctx = ctx,
    walletMnemonic = txOperatorMnemonic,
    mnemonicPassword = txOperatorMnemonicPw
  )
  val compiler = new ContractCompile(ctx)
  val explorer = new explorerApi()

  def process(
      conversionFunc: (InputBox, InputBox) => Either[String, Option[InputBox]]
  )(
      proxyInputs: Seq[InputBox],
      bankInput: InputBox
  ): (List[String], Option[InputBox]) = {
    proxyInputs.foldLeft[(List[String], Option[InputBox])](
      List.empty[String],
      Some(bankInput)
    ) {
      case (acc @ (errors, Some(currentBankInput)), proxyInput) =>
        try {
          conversionFunc(proxyInput, currentBankInput) match {
            case Right(somebox) => (errors, somebox)
            case Left(errString) =>
              (errString :: errors, Some(currentBankInput))
          }
        } catch {
          case e: Exception => (e.getMessage :: errors, Some(currentBankInput))
        }
      case (result, _) => result
    }
  }

  def processHodlERGMint(
      proxyInput: InputBox,
      currentBankInput: InputBox
  ): Either[String, Option[InputBox]] = {
    extractInputData(proxyInput, currentBankInput, ctx.getNetworkType) match {
      case Right(res: ExtractionResult) =>
        val result = for {
          recipientAddress <- res.recipientAddress.toRight(
            "[hodlErg] Recipient address not found"
          )
          hodlSingleton <- res.hodlSingleton.toRight("[hodlErg] singleton not found")
          hodlTokenId <- res.hodlTokenId.toRight("[hodlErg] hodl token not found")
          totalTokenSupply <- res.totalTokenSupply.toRight(
            "[hodlErg] total token supply not found"
          )
          precisionFactor <- res.precisionFactor.toRight("[hodlErg] precision not found")
          minBankValue <- res.minBankValue.toRight("[hodlErg] min bank value not found")
          devFee <- res.devFee.toRight("[hodlErg] dev fee not found")
          bankFee <- res.bankFee.toRight("[hodlErg] bank fee not found")
          minBoxValue <- res.minBoxValue.toRight("[hodlErg] min box value not found")
          minerFee <- res.minerFee.toRight("[hodlErg] miner fee not found")
          txOperatorFee <- res.txOperatorFee.toRight(
            "[hodlErg] tx operator fee not found"
          )
        } yield {

          val ergMintAmount =
            try {
              proxyInput.getValue - minBoxValue - minerFee - txOperatorFee
            } catch {
              case e: Exception =>
                return Left("[hodlErg] ergMintAmount could not be calculated")
            }

          val hodlMintAmount =
            try {
              hodlMintAmountFromERG(
                currentBankInput,
                ergMintAmount
              )
            } catch {
              case e: Exception =>
                return Left("[hodlErg] hodlMintAmount could not be calculated")
            }

          val hodlOutBoxHodlTokenAmount =
            try {
              currentBankInput.getTokens //
                .get(1)
                .getValue - hodlMintAmount
            } catch {
              case e: Exception =>
                return Left("[hodlErg] holdOutBoxHodlTokenAmount could not be calculated")
            }

          val phoenixContract =
            try {
              Address
                .fromPropositionBytes(
                  ctx.getNetworkType,
                  currentBankInput.toErgoValue.getValue.propositionBytes.toArray
                )
                .toErgoContract
            } catch {
              case e: Exception =>
                return Left("[hodlErg] error getting phoenix contract")
            }

          val hodlOutBox =
            try {
              outBoxObj.hodlBankBox(
                phoenixContract,
                hodlSingleton,
                new ErgoToken(hodlTokenId, hodlOutBoxHodlTokenAmount.toLong),
                totalTokenSupply,
                precisionFactor,
                minBankValue,
                bankFee,
                devFee,
                currentBankInput.getValue + ergMintAmount
              )
            } catch {
              case e: Exception =>
                return Left("[hodlErg] error building hodlOutBox mint")
            }

          val recipientBox =
            try {
              outBoxObj.hodlMintBox(
                recipientAddress,
                new ErgoToken(hodlTokenId, hodlMintAmount.toLong)
              )
            } catch {
              case e: Exception =>
                return Left("[hodlErg] error building recipient box for mint tx")
            }

          val unsignedTransaction =
            try {
              txHelper.buildUnsignedTransaction(
                inputs = Array(currentBankInput, proxyInput),
                outputs = Array(hodlOutBox, recipientBox),
                fee = minerFee
              )
            } catch {
              case e: Exception =>
                return Left("[hodlErg] error building mint tx")
            }

          val signedTx =
            try {
              txHelper.signTransaction(
                unsignedTransaction
              )
            } catch {
              case e: Exception =>
                return Left("[hodlErg] error signing mint tx")
            }

          val txHash =
            try {
              txHelper.sendTx(signedTx)
            } catch {
              case e: DoubleSpendingError  => return Left(e.getMessage)
              case e: TransactionInMempool => return Left(e.getMessage)
              case e: Exception =>
                return Left("[hodlErg] error submitting hodlMint tx: " + e.getMessage)
            }

          println("[hodlErg] Mint Transaction Submitted: " + txHash)
          Thread.sleep(500)

          signedTx.getOutputsToSpend.get(0)
        }
        result.fold[Either[String, Option[InputBox]]](
          e => Left(e),
          s => Right(Some(s))
        )

      case Left(errorMessage) => Left(errorMessage)
    }
  }

  def processHodlERGBurn(
      proxyInput: InputBox,
      currentBankInput: InputBox
  ): Either[String, Option[InputBox]] = {
    extractInputData(proxyInput, currentBankInput, ctx.getNetworkType) match {
      case Right(res: ExtractionResult) =>
        for {
          recipientAddress <- res.recipientAddress.toRight(
            "[hodlErg] recipient address not found"
          )
          hodlSingleton <- res.hodlSingleton.toRight("[hodlErg] hodl singleton not found")
          hodlTokenId <- res.hodlTokenId.toRight("[hodlErg] hodl token id not found")
          totalTokenSupply <- res.totalTokenSupply.toRight(
            "[hodlErg] total token supply not found"
          )
          precisionFactor <- res.precisionFactor.toRight(
            "[hodlErg] precision factor not found"
          )
          minBankValue <- res.minBankValue.toRight(
            "[hodlErg] minimum bank value not found"
          )
          devFee <- res.devFee.toRight("[hodlErg] developer fee not found")
          bankFee <- res.bankFee.toRight("[hodlErg] bank fee not found")
          minBoxValue <- res.minBoxValue.toRight("[hodlErg] minimum box value not found")
          minerFee <- res.minerFee.toRight("[hodlErg] miner fee not found")
          txOperatorFee <- res.txOperatorFee.toRight(
            "[hodlErg] transaction operator fee not found"
          )
        } yield {

          val hodlDummyToken =
            try {
              new ErgoToken(hodlTokenId, 1L)
            } catch {
              case e: Exception =>
                return Left("[hodlErg] burn error building dummy token")
            }

          val hodlBurnAmount =
            try {
              proxyInput.getTokens.asScala
                .filter(t =>
                  t.getId.toString == hodlDummyToken.getId.toString()
                )
                .map(_.getValue)
                .sum
            } catch {
              case e: Exception =>
                return Left("[hodlErg] burn error finding dummy token")
            }

          val (userBoxAmount, devFeeAmount, bankFeeAmount) =
            try {
              burnAmount(currentBankInput, hodlBurnAmount)
            } catch {
              case e: Exception =>
                return Left("[hodlErg] burn error calculating burn amount")
            }

          val bankBoxOutAmount =
            try {
              currentBankInput.getValue - userBoxAmount - devFeeAmount
            } catch {
              case e: Exception =>
                return Left("[hodlErg] burn error getting bank input value")
            }

          val hodlOutBoxHodlTokenAmount =
            try {
              currentBankInput.getTokens.get(1).getValue + hodlBurnAmount
            } catch {
              case e: Exception =>
                return Left("[hodlErg] burn error getting bank tokens")
            }

          val phoenixContract =
            try {
              Address
                .fromPropositionBytes(
                  ctx.getNetworkType,
                  currentBankInput.toErgoValue.getValue.propositionBytes.toArray
                )
                .toErgoContract
            } catch {
              case e: Exception =>
                return Left("[hodlErg] burn error getting phoenix contract")
            }

          val hodlOutBox =
            try {
              outBoxObj.hodlBankBox(
                phoenixContract,
                hodlSingleton,
                new ErgoToken(hodlTokenId, hodlOutBoxHodlTokenAmount),
                totalTokenSupply,
                precisionFactor,
                minBankValue,
                bankFee,
                devFee,
                bankBoxOutAmount
              )
            } catch {
              case e: Exception =>
                return Left("[hodlErg] burn error building hodlOutBox for burn")
            }

          val recipientBox =
            try {
              outBoxObj.simpleOutBox(recipientAddress, userBoxAmount)
            } catch {
              case e: Exception =>
                return Left(
                  "[hodlErg] burn error building recipient box for hodlERG burn"
                )
            }

          val feeScript: String = {
            if (ctx.getNetworkType == NetworkType.MAINNET) {
              PhoenixContracts.phoenix_v1_hodlcoin_fee.contractScript
            } else {
              PhoenixContracts.phoenix_v1_hodlcoin_feeTest.contractScript
            }
          }

          val feeContract: ErgoContract = compiler.compileFeeContract(
            feeScript,
            minMinerFee
          )

          val devFeeBox =
            try {
              outBoxObj.simpleOutBox(feeContract.toAddress, devFeeAmount)
            } catch {
              case e: Exception =>
                return Left("[hodlErg] hodlErg burn cannot build dev fee box")
            }

          val unsignedTransaction =
            try {
              txHelper.buildUnsignedTransaction(
                inputs = Array(currentBankInput, proxyInput),
                outputs = Array(hodlOutBox, recipientBox, devFeeBox),
                fee = minerFee
              )
            } catch {
              case e: Exception =>
                return Left("[hodlErg] cannot build hodlERG burn tx: " + e.getMessage)
            }

//          println(txHelper.getUnsignedJson(unsignedTransaction)) // for debugging

          val signedTx =
            try {
              txHelper.signTransaction(
                unsignedTransaction
              )
            } catch {
              case e: Exception =>
                return Left("[hodlErg] error signing hodlERG burn tx: " + e.getMessage)
            }

          val txHash =
            try {
              txHelper.sendTx(signedTx)
            } catch {
              case e: DoubleSpendingError  => return Left(e.getMessage)
              case e: TransactionInMempool => return Left(e.getMessage)
              case e: Exception =>
                return Left("[hodlErg] error submitting burn tx: " + e.getMessage)
            }

          println("[hodlErg] Burn Transaction Submitted: " + txHash)
          Thread.sleep(500)
          Option(signedTx.getOutputsToSpend.get(0))
        }
      case Left(errorMessage) => Left(errorMessage)
    }
  }

  def processHodlTokenMint(
      proxyInput: InputBox,
      currentBankInput: InputBox
  ): Either[String, Option[InputBox]] = {
    extractInputData(proxyInput, currentBankInput, ctx.getNetworkType) match {
      case Right(res: ExtractionResult) =>
        val result = for {
          recipientAddress <- res.recipientAddress.toRight(
            "[hodlToken] Recipient address not found"
          )
          hodlSingleton <- res.hodlSingleton.toRight("[hodlToken] singleton not found")
          hodlTokenId <- res.hodlTokenId.toRight("[hodlToken] hodl token not found")
          totalTokenSupply <- res.totalTokenSupply.toRight(
            "[hodlToken] total token supply not found"
          )
          precisionFactor <- res.precisionFactor.toRight("[hodlToken] precision not found")
          minBankValue <- res.minBankValue.toRight("[hodlToken] min bank value not found")
          devFee <- res.devFee.toRight("[hodlToken] dev fee not found")
          bankFee <- res.bankFee.toRight("[hodlToken] bank fee not found")
          minBoxValue <- res.minBoxValue.toRight("[hodlToken] min box value not found")
          minerFee <- res.minerFee.toRight("[hodlToken] miner fee not found")
          txOperatorFee <- res.txOperatorFee.toRight(
            "[hodlToken] tx operator fee not found"
          )
        } yield {

          val baseTokenId =
            try {
              currentBankInput.getTokens.get(2).getId
            } catch {
              case e: Exception =>
                return Left("[hodlToken] base token id could not be extracted")
            }

          val incomingBaseTokenAmount =
            try {
              val baseToken = proxyInput.getTokens.get(0)
              if (baseToken.getId.toString() != baseTokenId.toString()) {
                throw new Exception(
                  "[hodlToken] base token does not match bank's base token"
                )
              }
              baseToken.getValue
            } catch {
              case e: Exception =>
                return Left("[hodlToken] base token could not be extracted from proxy")
            }

          val tokenMintAmount =
            try {
              hodlTokenMintAmount(
                currentBankInput,
                incomingBaseTokenAmount
              )
            } catch {
              case e: Exception =>
                return Left("[hodlToken] hodlMintAmount could not be calculated")
            }

          val phoenixContract =
            try {
              Address
                .fromPropositionBytes(
                  ctx.getNetworkType,
                  currentBankInput.toErgoValue.getValue.propositionBytes.toArray
                )
                .toErgoContract
            } catch {
              case e: Exception =>
                return Left("[hodlToken] error getting phoenix contract")
            }

          val hodlOutBox =
            try {
              outBoxObj.hodlBankBox(
                phoenixContract,
                hodlSingleton,
                new ErgoToken(
                  hodlTokenId,
                  currentBankInput.getTokens
                    .get(1)
                    .getValue - (tokenMintAmount.toLong)
                ),
                totalTokenSupply,
                precisionFactor,
                minBankValue,
                bankFee,
                devFee,
                currentBankInput.getValue,
                Some(
                  ErgoToken(
                    baseTokenId,
                    currentBankInput.getTokens
                      .get(2)
                      .getValue + incomingBaseTokenAmount
                  )
                )
              )
            } catch {
              case e: Exception =>
                return Left("[hodlToken] error building hodlOutBox for mint")
            }

          val recipientBox =
            try {
              outBoxObj.hodlMintBox(
                recipientAddress,
                new ErgoToken(hodlTokenId, tokenMintAmount.toLong)
              )
            } catch {
              case e: Exception =>
                return Left("[hodlToken] error building recipient box for mint")
            }

          val unsignedTransaction =
            try {
              txHelper.buildUnsignedTransaction(
                inputs = Array(currentBankInput, proxyInput),
                outputs = Array(hodlOutBox, recipientBox),
                fee = minerFee
              )
            } catch {
              case e: Exception =>
                return Left("[hodlToken] error building mint tx")
            }
//          println(txHelper.getUnsignedJson(unsignedTransaction))

          val signedTx =
            try {
              txHelper.signTransaction(
                unsignedTransaction
              )
            } catch {
              case e: Exception =>
                return Left("[hodlToken] error signing mint tx " + e)
            }

          val txHash =
            try {
              txHelper.sendTx(signedTx)
            } catch {
              case e: DoubleSpendingError  => return Left(e.getMessage)
              case e: TransactionInMempool => return Left(e.getMessage)
              case e: Exception =>
                return Left(
                  "[hodlToken] error submitting hodlToken mint tx: " + e.getMessage
                )
            }

          println("[hodlToken] mint Transaction Submitted: " + txHash)
          Thread.sleep(500)

          signedTx.getOutputsToSpend.get(0)
        }
        result.fold[Either[String, Option[InputBox]]](
          e => Left(e),
          s => Right(Some(s))
        )

      case Left(errorMessage) => Left(errorMessage)
    }
  }

  def processHodlTokenBurn(
      proxyInput: InputBox,
      currentBankInput: InputBox
  ): Either[String, Option[InputBox]] = {
    extractInputData(proxyInput, currentBankInput, ctx.getNetworkType) match {
      case Right(res: ExtractionResult) =>
        for {
          recipientAddress <- res.recipientAddress.toRight(
            "[hodlToken] recipient address not found"
          )
          hodlSingleton <- res.hodlSingleton.toRight("[hodlToken] hodl singleton not found")
          hodlTokenId <- res.hodlTokenId.toRight("[hodlToken] hodl token id not found")
          totalTokenSupply <- res.totalTokenSupply.toRight(
            "[hodlToken] total token supply not found"
          )
          precisionFactor <- res.precisionFactor.toRight(
            "[hodlToken] precision factor not found"
          )
          minBankValue <- res.minBankValue.toRight(
            "[hodlToken] minimum bank value not found"
          )
          devFee <- res.devFee.toRight("[hodlToken] developer fee not found")
          bankFee <- res.bankFee.toRight("[hodlToken] bank fee not found")
          minBoxValue <- res.minBoxValue.toRight("[hodlToken] minimum box value not found")
          minerFee <- res.minerFee.toRight("[hodlToken] miner fee not found")
          txOperatorFee <- res.txOperatorFee.toRight(
            "[hodlToken] transaction operator fee not found"
          )
        } yield {

          val baseTokenId =
            try {
              currentBankInput.getTokens.get(2).getId
            } catch {
              case e: Exception =>
                return Left("[hodlToken] base token id could not be extracted")
            }

          val hodlDummyToken =
            try {
              new ErgoToken(hodlTokenId, 1L)
            } catch {
              case e: Exception =>
                return Left("[hodlToken] error building dummy token")
            }

          val hodlBurnAmount =
            try {
              proxyInput.getTokens.asScala
                .filter(t =>
                  t.getId.toString == hodlDummyToken.getId.toString()
                )
                .map(_.getValue)
                .sum
            } catch {
              case e: Exception =>
                return Left("[hodlToken] error finding dummy token")
            }

          val (userBoxAmount, devFeeAmount, bankFeeAmount) =
            try {
              burnTokenAmount(currentBankInput, hodlBurnAmount)
            } catch {
              case e: Exception =>
                return Left("[hodlToken] error calculating burn amount")
            }

          val hodlOutBoxHodlTokenAmount =
            try {
              currentBankInput.getTokens.get(1).getValue + hodlBurnAmount
            } catch {
              case e: Exception =>
                return Left("[hodlToken] error getting bank tokens")
            }

          val phoenixContract =
            try {
              Address
                .fromPropositionBytes(
                  ctx.getNetworkType,
                  currentBankInput.toErgoValue.getValue.propositionBytes.toArray
                )
                .toErgoContract
            } catch {
              case e: Exception =>
                return Left("[hodlToken] error getting phoenix contract")
            }

          val hodlOutBox =
            try {
              outBoxObj.hodlBankBox(
                phoenixContract,
                hodlSingleton,
                new ErgoToken(hodlTokenId, hodlOutBoxHodlTokenAmount),
                totalTokenSupply,
                precisionFactor,
                minBankValue,
                bankFee,
                devFee,
                currentBankInput.getValue,
                Some(
                  ErgoToken(
                    baseTokenId,
                    currentBankInput.getTokens
                      .get(2)
                      .getValue - userBoxAmount - devFeeAmount
                  )
                )
              )
            } catch {
              case e: Exception =>
                return Left("[hodlToken] error building hodlOutBox for burn")
            }

          val recipientBox =
            try {
              outBoxObj.hodlBurnBox(
                recipientAddress,
                ErgoToken(baseTokenId, userBoxAmount)
              )
            } catch {
              case e: Exception =>
                return Left("[hodlToken] error building recipient box for burn")
            }

          val feeScript: String = {
            if (ctx.getNetworkType == NetworkType.MAINNET) {
              PhoenixContracts.phoenix_v1_hodltoken_fee.contractScript
            } else {
              PhoenixContracts.phoenix_v1_hodltoken_feeTest_testnet.contractScript
            }
          }

          val feeContract: ErgoContract = compiler.compileFeeTokenContract(
            feeScript,
            minMinerFee,
            currentBankInput.getTokens.get(2),
            25L,
            25L,
            25L
          )

          val devFeeBox =
            try {
              outBoxObj.tokenOutBox(
                Array(
                  ErgoToken(
                    currentBankInput.getTokens.get(2).getId,
                    devFeeAmount
                  )
                ),
                feeContract.toAddress
              )
            } catch {
              case e: Exception =>
                return Left("[hodlToken] cannot build dev fee box")
            }

          val unsignedTransaction =
            try {
              txHelper.buildUnsignedTransaction(
                inputs = Array(currentBankInput, proxyInput),
                outputs =
                  if (userBoxAmount > 0 && devFeeAmount > 0)
                    Array(hodlOutBox, recipientBox, devFeeBox)
                  else if (userBoxAmount > 0 && devFeeAmount == 0)
                    Array(hodlOutBox, recipientBox)
                  else Array(hodlOutBox),
                fee = minerFee
              )
            } catch {
              case e: Exception =>
                return Left("[hodlToken] cannot build hodlToken burn tx: " + e.getMessage)
            }

//          println(
//            txHelper.getUnsignedJson(unsignedTransaction)
//          ) // for debugging

          val signedTx =
            try {
              txHelper.signTransaction(
                unsignedTransaction
              )
            } catch {
              case e: Exception =>
                return Left("[hodlToken] error signing burn tx: " + e)
            }

          val txHash =
            try {
              txHelper.sendTx(signedTx)
            } catch {
              case e: DoubleSpendingError  => return Left(e.getMessage)
              case e: TransactionInMempool => return Left(e.getMessage)
              case e: Exception =>
                return Left(
                  "[hodlToken] error submitting burn tx: " + e.getMessage
                )
            }

          println("[hodlToken] Burn Transaction Submitted: " + txHash)
          Thread.sleep(500)
          Option(signedTx.getOutputsToSpend.get(0))
        }
      case Left(errorMessage) => Left(errorMessage)
    }
  }

}

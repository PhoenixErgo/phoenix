package execute

import contracts.PhoenixContracts
import execute.DataHandling.{burnAmount, extractInputData}
import execute.HodlCalulations.hodlMintAmountFromERG
import org.ergoplatform.appkit.{
  Address,
  BlockchainContext,
  ErgoContract,
  InputBox,
  NetworkType
}
import org.ergoplatform.sdk.ErgoToken
import utils.{
  BoxAPI,
  BoxJson,
  ContractCompile,
  DoubleSpendingError,
  OutBoxes,
  TransactionHelper,
  TransactionInMempool,
  explorerApi
}

import scala.collection.JavaConverters._
import scala.reflect.runtime.universe.Try
import scala.util.{Failure, Success}

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

  private val feeScript: String = {
    if (ctx.getNetworkType == NetworkType.MAINNET) {
      PhoenixContracts.phoenix_v1_hodlcoin_fee.contractScript
    } else {
      PhoenixContracts.phoenix_v1_hodlcoin_feeTest.contractScript
    }
  }

  private val feeContract: ErgoContract = compiler.compileFeeContract(
    feeScript,
    minMinerFee
  )

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
            "Recipient address not found"
          )
          hodlSingleton <- res.hodlSingleton.toRight("singleton not found")
          hodlTokenId <- res.hodlTokenId.toRight("hodl token not found")
          totalTokenSupply <- res.totalTokenSupply.toRight(
            "total token supply not found"
          )
          precisionFactor <- res.precisionFactor.toRight("precision not found")
          minBankValue <- res.minBankValue.toRight("min bank value not found")
          devFee <- res.devFee.toRight("dev fee not found")
          bankFee <- res.bankFee.toRight("bank fee not found")
          minBoxValue <- res.minBoxValue.toRight("min box value not found")
          minerFee <- res.minerFee.toRight("miner fee not found")
          txOperatorFee <- res.txOperatorFee.toRight(
            "tx operator fee not found"
          )
        } yield {

          val ergMintAmount =
            try {
              proxyInput.getValue - minBoxValue - minerFee - txOperatorFee
            } catch {
              case e: Exception =>
                return Left("ergMintAmount could not be calculated")
            }

          val hodlMintAmount =
            try {
              hodlMintAmountFromERG(
                currentBankInput,
                ergMintAmount
              )
            } catch {
              case e: Exception =>
                return Left("hodlMintAmount could not be calculated")
            }

          val hodlOutBoxHodlTokenAmount =
            try {
              currentBankInput.getTokens //
                .get(1)
                .getValue - hodlMintAmount
            } catch {
              case e: Exception =>
                return Left("holdOutBoxHodlTokenAmount could not be calculated")
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
                return Left("error getting phoenix contract")
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
                return Left("error building hodlOutBox for mint")
            }

          val recipientBox =
            try {
              outBoxObj.hodlMintBox(
                recipientAddress,
                new ErgoToken(hodlTokenId, hodlMintAmount.toLong)
              )
            } catch {
              case e: Exception =>
                return Left("error building recipient box for hodlMint")
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
                return Left("error building hodlMint tx")
            }

          val signedTx =
            try {
              txHelper.signTransaction(
                unsignedTransaction
              )
            } catch {
              case e: Exception =>
                return Left("error signing hodlMint tx")
            }

          val txHash =
            try {
              txHelper.sendTx(signedTx)
            } catch {
              case e: DoubleSpendingError  => return Left(e.getMessage)
              case e: TransactionInMempool => return Left(e.getMessage)
              case e: Exception =>
                return Left("error submitting hodlMint tx: " + e.getMessage)
            }

          println("Mint Transaction Submitted: " + txHash)
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
            "recipient address not found"
          )
          hodlSingleton <- res.hodlSingleton.toRight("hodl singleton not found")
          hodlTokenId <- res.hodlTokenId.toRight("hodl token id not found")
          totalTokenSupply <- res.totalTokenSupply.toRight(
            "total token supply not found"
          )
          precisionFactor <- res.precisionFactor.toRight(
            "precision factor not found"
          )
          minBankValue <- res.minBankValue.toRight(
            "minimum bank value not found"
          )
          devFee <- res.devFee.toRight("developer fee not found")
          bankFee <- res.bankFee.toRight("bank fee not found")
          minBoxValue <- res.minBoxValue.toRight("minimum box value not found")
          minerFee <- res.minerFee.toRight("miner fee not found")
          txOperatorFee <- res.txOperatorFee.toRight(
            "transaction operator fee not found"
          )
        } yield {

          val hodlDummyToken =
            try {
              new ErgoToken(hodlTokenId, 1L)
            } catch {
              case e: Exception =>
                return Left("error building dummy token")
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
                return Left("error finding dummy token")
            }

          val (userBoxAmount, devFeeAmount, bankFeeAmount) =
            try {
              burnAmount(currentBankInput, hodlBurnAmount)
            } catch {
              case e: Exception =>
                return Left("error calculating burn amount")
            }

          val bankBoxOutAmount =
            try {
              currentBankInput.getValue - userBoxAmount - devFeeAmount
            } catch {
              case e: Exception =>
                return Left("error getting bank input value")
            }

          val hodlOutBoxHodlTokenAmount =
            try {
              currentBankInput.getTokens.get(1).getValue + hodlBurnAmount
            } catch {
              case e: Exception =>
                return Left("error getting bank tokens")
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
                return Left("error getting phoenix contract")
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
                return Left("error building hodlOutBox for burn")
            }

          val recipientBox =
            try {
              outBoxObj.simpleOutBox(recipientAddress, userBoxAmount)
            } catch {
              case e: Exception =>
                return Left("error building recipient box for hodlERG burn")
            }

          val devFeeBox =
            try {
              outBoxObj.simpleOutBox(feeContract.toAddress, devFeeAmount)
            } catch {
              case e: Exception =>
                return Left("cannot build hodlERG dev fee outbox")
            }

          val unsignedTransaction =
            try {
              // for debugging for box with custom ergotree
//              val boxAPIObj = new BoxAPI("https://tn-ergo-explorer.anetabtc.io", "http://168.138.185.215:9052")
//              val bank = "3rJkun8FwRnCZdsyYuDTgw5fioFjtCFS59LEK5iTWLmGvqMYdyvEQz2PY7WXA5hSFCcacD36B5t333XAp8FyXNcvSq3UVVujAR5Xbi7Gc9rYZd1TBFEjeWk3Qet719HSVkp3PGGJozzwNhXjBYgTcy5PK2ZAFx4xpsd5GuatacbMKg2R5TkZWiZAxrpuZPkQn1y3UFZr5KeidjFgSa6jWUC82NKUGfZpmPHmE1q4w4Wdj8eoQYwNPQ1DYbykqmeL5znfoQTTtXX8vAdamRU6uNZBMMYzH3NZJWkmPAk8D69RATj5Gh8TUzV4NLBc8QoQwX1QWRtXXYEAXWaKk4PqAu9zJn2cVuATBT72RhvzDUGHw3MAnSbgjA277iZFJkyiroAaXYBWyi84d9nH2LgG8WBgSvzMDom3DwoRgLSEeKyXeS5ewdRrms1rL7rTGYoQ15nRUW1eHB2AWEgEREmw5rSEfCY9CoYAAAVmsDxB9CbkJEbR6dwCL3GoWzuuUEWuLaRbrpUSBCAx5LAcFD7DDgrg56TjRYuQE"
//              val boxes = boxAPIObj
//                .getUnspentBoxesFromApi(bank, selectAll = true)
//                .items
//
//              val myBank = boxes.find(_.boxId == "a72b7c4b159cd6079e6f231cea887b3fabf6855042bb8b8c2f42b35a00abfdce").getOrElse(boxes.head)
//
////              myBank.ergoTree = "100b0402040004020101040004000502050005d00f04040e2084606649e202b0be47cddf744ce5c24cbd087e287f7fc4acae77ad2e92440017d812d601db6308a7d602b27201730000d6038c720202d604b2a5730100d605db63087204d606b27205730200d6078c720602d6089972037207d609c17204d60a7ec1a706d60be4c6a70505d60c7e720b06d60de4c6a70405d60e9d9c720a720c7e99720d720306d60fe4c6a70605d610e4c6a70705d611e4c6a70805d61296830401927209720f73039683030193b27205730400b27201730500938c7206018c72020192720773069683050193e4c672040405720d93e4c672040505720b93e4c672040605720f93e4c672040705721093e4c6720408057211959172087307d1968302017212927e7209069a720a9d9c7e720806720e720cd803d6139d9c7e997207720306720e720cd6147308d615b2a5730900d1968303017212937e7209069a99720a72139d9c72137e7211067e72140696830201937ec17215069d9c72137e7210067e72140693cbc27215730a"

//              val box = boxAPIObj.convertJsonBoxToInputBox(myBank)

              txHelper.buildUnsignedTransaction(
                inputs = Array(currentBankInput, proxyInput),
                outputs = Array(hodlOutBox, recipientBox, devFeeBox),
                fee = minerFee
              )
            } catch {
              case e: Exception =>
                return Left("cannot build hodlERG burn tx: " + e.getMessage)
            }

//          println(txHelper.getUnsignedJson(unsignedTransaction)) // for debugging

          val signedTx =
            try {
              txHelper.signTransaction(
                unsignedTransaction
              )
            } catch {
              case e: Exception =>
                return Left("error signing hodlERG burn tx: " + e.getMessage)
            }

          val txHash =
            try {
              txHelper.sendTx(signedTx)
            } catch {
              case e: DoubleSpendingError  => return Left(e.getMessage)
              case e: TransactionInMempool => return Left(e.getMessage)
              case e: Exception =>
                return Left("error submitting hodlMint tx: " + e.getMessage)
            }

          println("Burn Transaction Submitted: " + txHash)
          Thread.sleep(500)
          Option(signedTx.getOutputsToSpend.get(0))
        }
      case Left(errorMessage) => Left(errorMessage)
    }
  }

}

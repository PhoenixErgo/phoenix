package execute

import configs.{conf, serviceOwnerConf}
import contracts.PhoenixContracts
import execute.DataHandling.{sortProxyInputs, validateBox}
import org.ergoplatform.appkit.InputBox
import org.ergoplatform.sdk.ErgoToken
import special.collection.Coll
import utils.{BoxAPI, BoxJson}

class ErgoScriptConstantDecodeError(message: String) extends Exception(message)

class akkaFunctions {

  private val client: Client = new Client()
  client.setClient
  private val ctx = client.getContext
  private val serviceFilePath = "serviceOwner.json"
  private val contractConfFilePath = "contracts.json"
  private lazy val serviceConf = serviceOwnerConf.read(serviceFilePath)
  private lazy val contractsConf = conf.read(contractConfFilePath)

  private val walletMnemonic = serviceConf.txOperatorMnemonic
  private val walletMnemonicPw = serviceConf.txOperatorMnemonicPw

  private val boxAPIObj = new BoxAPI(serviceConf.apiUrl, serviceConf.nodeUrl)

  private val txBuilder = new TxBuildUtility(
    ctx = ctx,
    txOperatorMnemonic = walletMnemonic,
    txOperatorMnemonicPw = walletMnemonicPw,
    minMinerFee = serviceConf.minMinerFee
  )

  private val hodlERGProxyAddress = txBuilder.compiler
    .compileProxyContract(
      PhoenixContracts.phoenix_v1_hodlcoin_proxy.contractScript,
      serviceConf.minTxOperatorFee
    )
    .toAddress

  private val hodlTokenProxyAddress = txBuilder.compiler
    .compileProxyContract(
      PhoenixContracts.phoenix_v1_hodltoken_proxy.contractScript,
      serviceConf.minTxOperatorFee
    )
    .toAddress

  println("Service Runner Address: " + txBuilder.txHelper.senderAddress)

  def main(): Unit = {

    val hodlERGboxes =
      boxAPIObj
        .getUnspentBoxesFromApi(hodlERGProxyAddress.toString, selectAll = true)
        .items

    mintHodlERGWithRetry(hodlERGboxes)

    val hodlTokenBoxes =
      boxAPIObj
        .getUnspentBoxesFromApi(
          hodlTokenProxyAddress.toString,
          selectAll = true
        )
        .items

  }

  def mintHodlERGWithRetry(boxes: Array[BoxJson]): Unit = {
    try {
      mint(boxes) // Call the mint function
    } catch {
      case _: Throwable => // Catch any error thrown
        if (boxes.nonEmpty) {
          mintHodlERGWithRetry(
            boxes.tail
          ) // Call mintWithRetry with the first element deleted from the boxes array
        }
    }
  }

  def mint(boxes: Array[BoxJson]): Unit = {
    val validatedBoxInputs: Array[InputBox] = boxes
      .filter(box => {
//        box.ergoTree = "10010101d17300" // for debugging with sigmaProp(true) ergotree
        validateBox(
          box,
          serviceConf.minBoxValue,
          serviceConf.minMinerFee,
          serviceConf.minTxOperatorFee
        )
      })
      .map(boxAPIObj.convertJsonBoxToInputBox)

    if (validatedBoxInputs.isEmpty) {
      println("No Valid Boxes Found")
      return
    }

    val bankSingleton = new ErgoToken(
      validatedBoxInputs.head.getRegisters
        .get(1)
        .getValue
        .asInstanceOf[Coll[Byte]]
        .toArray,
      1L
    )

    val bankBoxFromApi: Option[InputBox] = for {
      boxID <- txBuilder.explorer
        .getUnspentBoxFromTokenID(bankSingleton.getId.toString())
      box <- txBuilder.explorer.getUnspentBoxFromMempool(boxID.getBoxId)
    } yield box

    val (mintInputs, burnInputs) =
      sortProxyInputs(validatedBoxInputs, bankSingleton)

    val (errors, bankBoxAfterMints) =
      bankBoxFromApi
        .map { box =>
          txBuilder.process(txBuilder.processHodlERGMint)(mintInputs, box)
        }
        .getOrElse {
          println("No box provided for minting.")
          (List.empty[String], None)
          return
        }

    val filteredErrors = errors.filterNot(e =>
      e == "double-spending-error" || e == "tx-in-mempool-error"
    )

    if (filteredErrors.nonEmpty) {
      println(
        s"Errors while processing txns (${filteredErrors.size}): ${filteredErrors.mkString(", ")}"
      )
    }

    val (burnErrors, bankBoxAfterBurn) =
      bankBoxAfterMints
        .map { box =>
          txBuilder.process(txBuilder.processHodlERGBurn)(burnInputs, box)
        }
        .getOrElse {
          txBuilder.process(txBuilder.processHodlERGBurn)(
            burnInputs,
            bankBoxAfterMints.get
          )
        }

    val filteredBurnErrors = burnErrors.filterNot(e =>
      e == "double-spending-error" || e == "tx-in-mempool-error"
    )

    if (filteredBurnErrors.nonEmpty) {
      println(
        s"Errors while processing burns (${filteredBurnErrors.size}): ${filteredBurnErrors.mkString(", ")}"
      )
    }

  }

}

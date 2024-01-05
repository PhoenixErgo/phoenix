package reward

import configs.{ServiceOwnerConfig, serviceOwnerConf}
import contracts.PhoenixContracts
import execute.Client
import org.ergoplatform.appkit.{
  Address,
  BlockchainContext,
  ErgoContract,
  OutBox
}
import utils.{BoxAPI, ContractCompile, InputBoxes, OutBoxes, TransactionHelper}
import scala.collection.JavaConverters._

object DevReward extends App {

  private val client: Client = new Client()
  client.setClient
  val ctx: BlockchainContext = client.getContext
  val serviceFilePath = "serviceOwner.json"
  val serviceConf: ServiceOwnerConfig = serviceOwnerConf.read(serviceFilePath)

  val walletMnemonic: String = serviceConf.txOperatorMnemonic
  val walletMnemonicPw: String = serviceConf.txOperatorMnemonicPw
  val txHelper =
    new TransactionHelper(this.ctx, walletMnemonic, walletMnemonicPw)

  val inputBoxesObj = new InputBoxes(ctx)
  val outBoxObj = new OutBoxes(ctx)
  val compiler = new ContractCompile(ctx)

  private val boxAPIObj = new BoxAPI(serviceConf.apiUrl, serviceConf.nodeUrl)
  val minMinerFeeNanoErg = 1000000L

  val feeScript: String =
    PhoenixContracts.phoenix_v1_hodlcoin_fee.contractScript

  val feeContract: ErgoContract = compiler.compileFeeContract(
    feeScript,
    minMinerFeeNanoErg
  )

  val brunoAddress: Address = Address.create(
    "9gnBtmSRBMaNTkLQUABoAqmU2wzn27hgqVvezAC9SU1VqFKZCp8" // revert back to original address
  )
  val pulsarzAddress: Address =
    Address.create("9hHondX3uZMY2wQsXuCGjbgZUqunQyZCNNuwGu6rL7AJC8dhRGa")
  val kushtiAddress: Address =
    Address.create("9iE2MadGSrn1ivHmRZJWRxzHffuAk6bPmEv6uJmPHuadBY8td5u")
  val krasAddress: Address =
    Address.create("9g8QaPWTnx71tDdqgzdqZVdNg1cKgEHhAhjCvFqV7ggxRVwycCg")
  val phoenixAddress: Address =
    Address.create("9iPs1ujGj2eKXVg82aGyAtUtQZQWxFaki48KFixoaNmUAoTY6wV")

  val feeDenom: Long = 100L
  val brunoNum: Long = 25L
  val pulsarzNum: Long = 25L
  val phoenixNum: Long = 25L
  val kushtiNum: Long = 15L
  val krasNum: Long = 10L

  def getDevBoxes(totalAmountForDevs: Long): Array[OutBox] = {
    val truncatedTotal = (totalAmountForDevs / 1000000L) * 1000000L
    Array(
      outBoxObj
        .simpleOutBox(brunoAddress, ((BigDecimal(brunoNum) * BigDecimal(truncatedTotal)) / BigDecimal(feeDenom)).setScale(0, BigDecimal.RoundingMode.FLOOR).toLong),
      outBoxObj.simpleOutBox(
        pulsarzAddress,
        ((BigDecimal(pulsarzNum) * BigDecimal(truncatedTotal)) / BigDecimal(feeDenom)).setScale(0, BigDecimal.RoundingMode.FLOOR).toLong
      ),
      outBoxObj.simpleOutBox(
        phoenixAddress,
        ((BigDecimal(phoenixNum) * BigDecimal(truncatedTotal)) / BigDecimal(feeDenom)).setScale(0, BigDecimal.RoundingMode.FLOOR).toLong
      ),
      outBoxObj.simpleOutBox(
        kushtiAddress,
        ((BigDecimal(kushtiNum) * BigDecimal(truncatedTotal)) / BigDecimal(feeDenom)).setScale(0, BigDecimal.RoundingMode.FLOOR).toLong
      ),
      outBoxObj.simpleOutBox(
        krasAddress,
        ((BigDecimal(krasNum) * BigDecimal(truncatedTotal)) / BigDecimal(feeDenom)).setScale(0, BigDecimal.RoundingMode.FLOOR).toLong
      )
    )
  }
//  val feeAddy =
//    "mCbJgVdTUoKiLn8a8Xucgvj45icZtV2uC85276wHVJQGtRhWzVAFiRDjJohyFDV81nShLn5Afc33mnV9mZUTkVty1zRPPRcXWRjhouRpxor7kMycsv8WSrbbP4p9oxsrsdoc6GQaoddfGexyzyPoQmgxNV1B9WQm4Ec5DTvEceHeV69mvqHGxB7cgps7eCvp2wLfhLm4DDuoteC6igiiHTtVhcG6SesqycQfnH7HACfjZLDsqAHtxCG3XzoGBpz6TjHTCXEmYEN3FiTd4AVskFaV31z5Re69ArUTxMoWPjP6dXZb3LBXtti3RUBeXLSAyTCbJktxU6irjjobTgz4jrjFr9QPnBnFENUesLpF4RYyXsR4t3awPZgXLbHFvCT8t8bcpnrP92Nue1wVCthaWRmCaJyRZddZDZLwAzSVutgaANNzr795EzVhv1kRTPgrae25VFZfdnVwvAiL1g67pJFc74etaEFGUQ26aotehPH6Y9hMreHiUDFRepGZiWRgsdpgoGnRdv4yR6GvePRWEymWCfsy1cNRhmqz4XbJPAaNuEoPkGbBTfvCFdjhfh9tczf4tkuRdhNLBePSbq9vntwFrek2Sy62D7MUfwK4GqwDkFDyYUNsgVKjC7z3nLQAdAU72ChhAXMqKLrxhvp2VaUmZj8jYdbXLUaH3Q7uJC65EHcMau"

  val boxes =
    boxAPIObj
      .getUnspentBoxesFromApi(
        feeContract.toAddress.toString,
        amountToSelect = 200
      )
      .items
//      .map(box => {
//        box.ergoTree = feeContract.getErgoTree.bytesHex
//        box
//      })

  val devAmount = (boxes.map(_.value).sum) - (minMinerFeeNanoErg)

  val devBoxes = getDevBoxes(devAmount)

  val diff = devAmount - devBoxes.map(_.getValue).sum

  val minerFee = {
    if (diff > 0) {
      minMinerFeeNanoErg + diff
    } else {
      minMinerFeeNanoErg
    }
  }

  val unsignedTransaction = txHelper.buildUnsignedTransaction(
    inputs = boxes.map(boxAPIObj.convertJsonBoxToInputBox),
    outputs = devBoxes,
    fee = minerFee
  )

  val signedTx = txHelper.signTransaction(
    unsignedTransaction
  )

  val txHash = txHelper.sendTx(signedTx)

  println(txHash)

}

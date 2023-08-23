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
  val minMinerFeeNanoErg = 1029843L

  val feeScript: String =
    PhoenixContracts.phoenix_v1_hodlcoin_fee.contractScript

  val feeContract: ErgoContract = compiler.compileFeeContract(
    feeScript,
    minMinerFeeNanoErg
  )

  val brunoAddress: Address = Address.create(
    "9exfustUCPDKXsfDrGNrmtkyLDwAie2rKKdUsPVa26RuBFaYeCL" // revert back to original address
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
    Array(
      outBoxObj
        .simpleOutBox(brunoAddress, (brunoNum * totalAmountForDevs) / feeDenom),
      outBoxObj.simpleOutBox(
        pulsarzAddress,
        (pulsarzNum * totalAmountForDevs) / feeDenom
      ),
      outBoxObj.simpleOutBox(
        phoenixAddress,
        (phoenixNum * totalAmountForDevs) / feeDenom
      ),
      outBoxObj.simpleOutBox(
        kushtiAddress,
        (kushtiNum * totalAmountForDevs) / feeDenom
      ),
      outBoxObj.simpleOutBox(
        krasAddress,
        (krasNum * totalAmountForDevs) / feeDenom
      )
    )
  }

  val feeAddress =
    "mCbJgVdTUoKiLn8a8Xucgvj45icZtV2uC85276wHVJQGtRhWzVAFiRDjJohyFDV81nShLn5Afc33mnV9mZUTkVty1zRPPRcXWRjhouRpxor7kMycsv8WSrbbP4p9oxsrsdoc6GQaoddfGexyzyPoQmgxNV1B9WQm4Ec5DTvEceHeV69mvqHGxB7cgps7eCvp2wLfhLm4DDuoteC6igiiHTtVhcG6SesqycQfnH7HACfjZLDsqAHtxCG3XzoGBpz6TjHTCXEmYEN3FiTd4AVskFaV31z5Re69ArUTxMoWPjP6dXZb3LBXtti3RUBeXLSAyTCbJktxU6irjjobTgz4jrjFr9QPnBnFENUesLpF4RYyXsR4t3awPZgXLbHFvCT8t8bcpnrP92Nue1wVCthaWRmCaJyRZddZDZLwAzSVutgaANNzr795EzVhv1kRTPgrae25VFZfdnVwvAiL1g67pJFc74etaEFGUQ26aotehPH6Y9hMreHiUDFRepGZiWRgsdpgoGnRdv4yR6GvePRWEymWCfsy1cNRhmqz4XbJPAaNuEoPkGbBTfvCFdjhfh9tczf4tkuRdhNLBePSbq9vntwFrek2Sy62D7MUfwK4GqwDkFDyYUNsgVKjC7z3nLQAdAU72ChhAXMqKLrxhvp2VaUmZj8jYdbXLUaH3Q7uJC65EHcMau"

  val boxes =
    boxAPIObj
      .getUnspentBoxesFromApi(
        feeAddress,
        amountToSelect = 225
      )
      .items
      .map(box => {
        box.ergoTree = feeContract.getErgoTree.bytesHex
        box
      })

  val devAmount = (boxes.map(_.value).sum) - (minMinerFeeNanoErg)

  println(boxes.map(_.value).sum)
  println(devAmount)

  val devBoxes = getDevBoxes(devAmount)

  println(devBoxes.map(_.getValue).sum)

  val unsignedTransaction = txHelper.buildUnsignedTransaction(
    inputs = boxes.map(boxAPIObj.convertJsonBoxToInputBox),
    outputs = devBoxes,
    fee = minMinerFeeNanoErg
  )

  println("miner box value: " + unsignedTransaction.getOutputs.get(5).getValue)


  println(unsignedTransaction.getOutputs.size())

  txHelper.signTransaction(
    unsignedTransaction
  )

}

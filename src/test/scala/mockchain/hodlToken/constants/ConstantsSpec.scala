package mockchain.hodlToken.constants

import configs.ServiceOwnerConfig
import contracts.PhoenixContracts
import execute.Client
import org.ergoplatform.appkit.{Address, ErgoContract}
import org.ergoplatform.sdk.ErgoToken
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scorex.crypto.hash.Blake2b256
import special.collection.Coll
import utils.ContractCompile

class ConstantsSpec extends AnyFlatSpec with should.Matchers {

  val txOperatorMnemonic: String = ""
  val txOperatorMnemonicPw: String = ""
  val minTxOperatorFeeNanoErg: Long = 1000000
  val minerFeeNanoErg: Long = 1000000
  val minBoxValueNanoErg = 1000000
  val nodeUrl: String = "http://213.239.193.208:9053"
  val apiUrl: String = "https://ergo-explorer.anetabtc.io"

  val phoenixFeeContractBytesConstant = 16

  private val client: Client = new Client()
  client.setClient
  private val ctx = client.getContext

  private val serviceConf = ServiceOwnerConfig(
    txOperatorMnemonic,
    txOperatorMnemonicPw,
    nodeUrl,
    apiUrl,
    minTxOperatorFeeNanoErg,
    minBoxValueNanoErg,
    minerFeeNanoErg
  )

  val compiler = new ContractCompile(ctx)

  val developerFeeTokenContractScript: String =
    PhoenixContracts.phoenix_v1_hodltoken_fee.contractScript

  val developerFeeTokenContract: ErgoContract = compiler.compileFeeTokenContract(
    developerFeeTokenContractScript,
    serviceConf.minMinerFee,
    new ErgoToken(
      "9a06d9e545a41fd51eeffc5e20d818073bf820c635e2a9d922269913e0de369d",
      1L
    ),
    25L,
    25L,
    25L,
    50,
    Address.create("9eYUQP7tZFSa34NzXYP6VzDC6kHWBZRwEw4pXEHC2MWGs75vCAS")
  )

  val stateContract: ErgoContract = compiler.compileTokenBankContract(
    PhoenixContracts.phoenix_v1_hodltoken_bank.contractScript,
    developerFeeTokenContract
  )

  "Developer Fee Constant" should "match" in {

    val developerFeeContractBytes: Array[Byte] = stateContract.getErgoTree
      .constants(phoenixFeeContractBytesConstant)
      .value
      .asInstanceOf[Coll[Byte]]
      .toArray

    developerFeeTokenContract.toAddress.asP2S().scriptBytes should be(developerFeeContractBytes)
  }

}

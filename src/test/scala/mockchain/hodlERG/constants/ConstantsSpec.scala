package mockchain.hodlERG.constants

import configs.ServiceOwnerConfig
import contracts.PhoenixContracts
import execute.Client
import org.ergoplatform.appkit.ErgoContract
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scorex.crypto.hash.Blake2b256
import sigma.Coll
import utils.ContractCompile

class ConstantsSpec extends AnyFlatSpec with should.Matchers {

  val txOperatorMnemonic: String = ""
  val txOperatorMnemonicPw: String = ""
  val minTxOperatorFeeNanoErg: Long = 1000000
  val minerFeeNanoErg: Long = 1000000
  val minBoxValueNanoErg = 1000000
  val nodeUrl: String = "http://213.239.193.208:9053"
  val apiUrl: String = "https://ergo-explorer.anetabtc.io"

  val phoenixFeeContractBytesHashConstant = 9

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

  val developerFeeContractScript: String =
    PhoenixContracts.phoenix_v1_hodlcoin_fee.contractScript

  val developerFeeContract: ErgoContract = compiler.compileFeeContract(
    developerFeeContractScript,
    serviceConf.minMinerFee
  )

  val stateContract: ErgoContract = compiler.compileBankContract(
    PhoenixContracts.phoenix_v1_hodlcoin_bank.contractScript,
    developerFeeContract
  )

  "Developer Fee Constant" should "match" in {

    val developerFeeContractBytes: Array[Byte] = stateContract.getErgoTree
      .constants(phoenixFeeContractBytesHashConstant)
      .value
      .asInstanceOf[Coll[Byte]]
      .toArray

    Blake2b256(developerFeeContract.toAddress.asP2S().scriptBytes) should be(developerFeeContractBytes)
  }

}

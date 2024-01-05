package main

import configs.serviceOwnerConf
import contracts.PhoenixContracts
import execute.Client
import execute.HodlCalulations.{hodlMintAmountFromERG, hodlPrice}
import initialize.initializeToken.serviceFilePath
import org.ergoplatform.appkit.{ErgoContract, InputBox}
import org.ergoplatform.appkit.impl.InputBoxImpl
import org.ergoplatform.sdk.ErgoToken
import utils.{ContractCompile, explorerApi}

object phoenixTest extends App {
  private val client: Client = new Client()
  client.setClient
  private val ctx = client.getContext
  val contractString =
    contracts.PhoenixContracts.phoenix_v1_hodlcoin_bank.contractScript
  val compilerObj = new ContractCompile(ctx)

  val dummyContractString = "sigmaProp(true)"
  val dummyContract = compilerObj.compileDummyContract(dummyContractString)

  println(dummyContract.getErgoTree.bytesHex)

  val contract = compilerObj.compileProxyContract(
    contracts.PhoenixContracts.phoenix_v1_hodlcoin_proxy.contractScript,
    1000000L
  )

  println(contract.getErgoTree.bytes.length)

}

object bank extends App {
  private val client: Client = new Client()
  client.setClient
  private val ctx = client.getContext
  val contractString =
    PhoenixContracts.phoenix_v1_hodltoken_bank.contractScript

  val feeContractScript =
    PhoenixContracts.phoenix_v1_hodltoken_feeTest_testnet.contractScript
  val compilerObj = new ContractCompile(ctx)

  val dummyContractString = "sigmaProp(true)"
  val dummyContract = compilerObj.compileDummyContract(dummyContractString)
  val serviceFilePath = "serviceOwner.json"
  val serviceConf = serviceOwnerConf.read(serviceFilePath)

  val comet = new ErgoToken(
    "0014dfe03c2a58d604e8d8e1fc73b68fac7512f0672d8e086a1af4d0c401b6e1",
    1L
  )

  private val feeContract: ErgoContract = compilerObj.compileFeeTokenContract(
    feeContractScript,
    serviceConf.minMinerFee,
    comet,
    25L,
    25L,
    25L
  )

  val contract = compilerObj.compileBankContract(
    contractString,
    feeContract
  )

  println(contract.getErgoTree.bytesHex)

}

object proxyContract extends App {
  private val client: Client = new Client()
  client.setClient
  private val ctx = client.getContext
  val compilerObj = new ContractCompile(ctx)

  val contractString = PhoenixContracts.phoenix_v1_hodlcoin_proxy.contractScript //PhoenixContracts.phoenix_v1_hodltoken_proxy.contractScript

  val contract = compilerObj.compileProxyContract(
    contractString,
    1000000L
  )

  println(contract.getErgoTree.bytesHex)

}
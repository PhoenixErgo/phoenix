package main

import contracts.PhoenixContracts
import execute.Client
import execute.HodlCalulations.{hodlMintAmountFromERG, hodlPrice}
import org.ergoplatform.appkit.{ErgoContract, InputBox}
import org.ergoplatform.appkit.impl.InputBoxImpl
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
    contracts.PhoenixContracts.phoenix_v1_hodlcoin_bank.contractScript

  val feeContractScript =
    PhoenixContracts.phoenix_v1_hodlcoin_feeTest.contractScript
  val compilerObj = new ContractCompile(ctx)

  val dummyContractString = "sigmaProp(true)"
  val dummyContract = compilerObj.compileDummyContract(dummyContractString)

  val feeContract: ErgoContract = compilerObj.compileFeeContract(
    feeContractScript,
    1000000L
  )

  val contract = compilerObj.compileBankContract(
    contractString,
    feeContract
  )

  println(contract.getErgoTree.bytesHex)

}

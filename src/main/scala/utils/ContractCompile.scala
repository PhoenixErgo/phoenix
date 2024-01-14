package utils

import org.ergoplatform.appkit._
import org.ergoplatform.sdk.ErgoToken
import scorex.crypto.hash.Blake2b256

class ContractCompile(ctx: BlockchainContext) {

  def compileDummyContract(
      contract: String = "sigmaProp(true)"
  ): ErgoContract = {
    this.ctx.compileContract(
      ConstantsBuilder.empty(),
      contract
    )
  }
  def compileProxyContract(
      contract: String,
      minTxOperatorFee: Long
  ): ErgoContract = {
    this.ctx.compileContract(
      ConstantsBuilder
        .create()
        .item(
          "$minTxOperatorFee",
          minTxOperatorFee
        )
        .build(),
      contract
    )
  }

  def compileBankContract(
      contract: String,
      developerFeeContract: ErgoContract
  ): ErgoContract = {
    this.ctx.compileContract(
      ConstantsBuilder
        .create()
        .item(
          "$phoenixFeeContractBytesHash",
          Blake2b256(developerFeeContract.toAddress.asP2S().scriptBytes)
        )
        .build(),
      contract
    )
  }

  def compileFeeContract(
      contract: String,
      minMinerFeeNanoErg: Long
  ): ErgoContract = {
    this.ctx.compileContract(
      ConstantsBuilder
        .create()
        .item(
          "$minerFee",
          minMinerFeeNanoErg
        )
        .build(),
      contract
    )
  }

  def compileFeeTokenContract(
      contract: String,
      minMinerFeeNanoErg: Long,
      baseToken: ErgoToken,
      brunoNum: Long,
      phoenixNum: Long,
      kushtiNum: Long,
      creatorNum: Long = 0L,
      creatorAddress: Address =
        Address.create("9iPs1ujGj2eKXVg82aGyAtUtQZQWxFaki48KFixoaNmUAoTY6wV")
  ): ErgoContract = {
    this.ctx.compileContract(
      ConstantsBuilder
        .create()
        .item(
          "$minerFee",
          minMinerFeeNanoErg
        )
        .item(
          "$baseTokenId",
          baseToken.getId.getBytes
        )
        .item(
          "$brunoNum",
          brunoNum
        )
        .item(
          "$phoenixNum",
          phoenixNum
        )
        .item(
          "$kushtiNum",
          kushtiNum
        )
        .item(
          "$creatorNum",
          creatorNum
        )
        .item(
          "$creatorAddress",
          creatorAddress.getPublicKey
        )
        .build(),
      contract
    )
  }

}

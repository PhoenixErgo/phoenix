package mockchain

import contracts.PhoenixContracts
import mockClient.HttpClientTesting
import mockUtils.FileMockedErgoClient
import org.ergoplatform.appkit.{
  Address,
  BlockchainContext,
  ErgoContract,
  InputBox
}
import org.ergoplatform.sdk.ErgoToken
import utils.{ContractCompile, OutBoxes, TransactionHelper}

trait PhoenixCommon extends HttpClientTesting {

  val ergoClient: FileMockedErgoClient = createMockedErgoClient(
    MockData(Nil, Nil)
  )

  val ctx: BlockchainContext = ergoClient.execute(ctx => ctx)

  val compiler = new ContractCompile(ctx)
  val txHelper = new TransactionHelper(ctx, "", "")
  val outBoxObj = new OutBoxes(ctx)

  val feeDenom: Long = 100L
  val brunoNum: Long = 25L
  val pulsarzNum: Long = 25L
  val phoenixNum: Long = 25L
  val kushtiNum: Long = 15L
  val krasNum: Long = 10L

  val minMinerFeeNanoErg = 1600000L

  val fundingBoxValue: Long = 50000000 * 1000000000L
  val tokenFundingBoxValue: Long = 2000000L
  val minBoxValue = 1000000L
  val minTxOperatorFee = 1000000L

  val hodlTokenId =
    "2cbabc2be7292e2e857a1f2c34a8b0c090de2f30fa44c68ab71454e5586bd45e"
  val hodlBankNft =
    "dc62f27e6b4bc1432bf78545622cd517b81dea53ee360dde495b308ac0ef1b88"

  // for hodlToken (e.g. hodlComet) contracts
  val tokenId =
    "2ababc2be7292e2e857a1f2c34a8b0c090de2f30fa44c68ab71454e5586bd45e"

  val userAddress: Address =
    Address.create("9eiuh5bJtw9oWDVcfJnwTm1EHfK5949MEm5DStc2sD1TLwDSrpx")

  val feeScript: String =
    PhoenixContracts.phoenix_v1_hodlcoin_feeTest_mainnet.contractScript

  val feeTokenScript: String =
    PhoenixContracts.phoenix_v1_hodltoken_feeTest_mainnet.contractScript

  val phoenixScript: String =
    PhoenixContracts.phoenix_v1_hodlcoin_bank.contractScript

  val phoenixTokenScript: String =
    PhoenixContracts.phoenix_v1_hodltoken_bank.contractScript

  val feeContract: ErgoContract = compiler.compileFeeContract(
    feeScript,
    minMinerFeeNanoErg
  )
  val defaultFeeTokenContract: ErgoContract = compiler.compileFeeTokenContract(
    feeTokenScript,
    minMinerFeeNanoErg,
    new ErgoToken(
      "9a06d9e545a41fd51eeffc5e20d818073bf820c635e2a9d922269913e0de369d",
      1L
    ),
    25L,
    25L,
    25L
  )

  val phoenixContract: ErgoContract =
    compiler.compileBankContract(phoenixScript, feeContract)

  val phoenixTokenContract: ErgoContract =
    compiler.compileBankContract(phoenixTokenScript, defaultFeeTokenContract)

  val proxyScript: String =
    PhoenixContracts.phoenix_v1_hodlcoin_proxy.contractScript
  val proxyTokenScript: String =
    PhoenixContracts.phoenix_v1_hodltoken_proxy.contractScript
  val proxyContract: ErgoContract =
    compiler.compileProxyContract(proxyScript, minTxOperatorFee)
  val proxyTokenContract: ErgoContract =
    compiler.compileProxyContract(proxyTokenScript, minTxOperatorFee)

  // R4: Long             TotalTokenSupply
  // R5: Long             PrecisionFactor
  // R6: Long             MinBankValue
  // R7: Long             BankFee
  // R8: Long             DevFee

  val totalSupply: Long = 50000000 * 1000000000L
  val precisionFactor = 1000000L
  val minBankValue = 1000000L
  val bankFee = 30L
  val devFee = 3L

  def extractPrecisionFactor(hodlBoxIn: InputBox): Long = {
    hodlBoxIn.getRegisters.get(1).getValue.asInstanceOf[Long] // R5
  }

  def extractBaseTokenSupply(hodlBoxIn: InputBox): Long = {
    hodlBoxIn.getRegisters.get(0).getValue.asInstanceOf[Long] // R4
  }

  def hodlPrice(hodlBoxIn: InputBox): Long = {
    // preserving terminology from the contract
    val reserveIn = hodlBoxIn.getValue
    val totalTokenSupply =
      hodlBoxIn.getRegisters.get(0).getValue.asInstanceOf[Long] // R4
    val hodlCoinsIn: Long = hodlBoxIn.getTokens.get(1).getValue
    val hodlCoinsCircIn: Long = totalTokenSupply - hodlCoinsIn
    val precisionFactor = extractPrecisionFactor(hodlBoxIn)
    ((BigInt(reserveIn) * BigInt(precisionFactor)) / BigInt(
      hodlCoinsCircIn
    )).toLong
  }

  // amount of (nano) ERGs needed to mint given amount of hodlcoins against given hodl bank
  def mintAmount(hodlBoxIn: InputBox, hodlMintAmt: Long): Long = {
    val price = hodlPrice(hodlBoxIn)
    val precisionFactor = extractPrecisionFactor(hodlBoxIn)
    hodlMintAmt * price / precisionFactor
  }

  /** @return amount of hodl tokens to mint give amount of ERGs paid
    */
  def hodlMintAmount(hodlBoxIn: InputBox, ergMintAmt: Long): Long = {
    val price = hodlPrice(hodlBoxIn)
    val precisionFactor = extractPrecisionFactor(hodlBoxIn)
    ergMintAmt * precisionFactor / price
  }

  def hodlTokenMintAmount(hodlBoxIn: InputBox, ergMintAmt: Long): Long = {
    val price = hodlTokenPrice(hodlBoxIn)
    val precisionFactor = extractPrecisionFactor(hodlBoxIn)
    ergMintAmt * precisionFactor / price
  }

  // amount of (nano) ERGs which can be released to when given amount of hodlcoins burnt

  /** @return amount of (nano) ERGs which can be released to when given amount of hodlcoins burnt to user,
    *         and also dev fee
    */
  def burnAmount(hodlBoxIn: InputBox, hodlBurnAmt: Long): (Long, Long, Long) = {
    val feeDenom = 1000L

    val devFee =
      hodlBoxIn.getRegisters.get(3).getValue.asInstanceOf[Long] // R7
    val bankFee =
      hodlBoxIn.getRegisters.get(4).getValue.asInstanceOf[Long] // R8

    val price = hodlPrice(hodlBoxIn)
    val precisionFactor = extractPrecisionFactor(hodlBoxIn)
    val beforeFees = hodlBurnAmt * price / precisionFactor
    val bankFeeAmount: Long = (beforeFees * bankFee) / feeDenom
    val devFeeAmount: Long = (beforeFees * devFee) / feeDenom
    val expectedAmountWithdrawn: Long =
      beforeFees - bankFeeAmount - devFeeAmount
    (expectedAmountWithdrawn, devFeeAmount, bankFeeAmount)
  }

  def hodlMintAmountFromERG(hodlBoxIn: InputBox, ergMintAmt: Long): Long = {
    val price = hodlPrice(hodlBoxIn)
    val precisionFactor = extractPrecisionFactor(hodlBoxIn)
    ergMintAmt * precisionFactor / price
  }

  def divUp(operands: (BigInt, BigInt)): BigInt = {

    val a: BigInt = operands._1 // Dividend
    val b: BigInt = operands._2 // Divisor

    if (b == 0) {
      return -1
    } else {
      return (a + (b - 1)) / b
    }

  }

  /** @return amount of (nano) ERGs which can be released to when given amount of hodlcoins burnt to user,
    *          and also dev fee
    */
  def burnTokenAmount(
      hodlBoxIn: InputBox,
      hodlBurnAmt: Long
  ): (Long, Long, Long) = {
    val feeDenom = 1000L

    val bankFeeNum =
      hodlBoxIn.getRegisters.get(4).getValue.asInstanceOf[Long] // R8
    val devFeeNum =
      hodlBoxIn.getRegisters.get(3).getValue.asInstanceOf[Long] // R7

    val price = hodlTokenPrice(hodlBoxIn)
    val precisionFactor = extractPrecisionFactor(hodlBoxIn)

    val expectedAmountBeforeFees = hodlBurnAmt * price / precisionFactor

    val dividend_1: BigInt = (BigInt(expectedAmountBeforeFees) * (BigInt(
      bankFeeNum
    ) + BigInt(devFeeNum)))
    val divisor_1: BigInt = BigInt(feeDenom) // This is never zero.

    val bankFeeAndDevFeeAmount: BigInt = divUp((dividend_1, divisor_1)) // Y

    val dividend_2: BigInt = (bankFeeAndDevFeeAmount * BigInt(devFeeNum))
    val divisor_2: BigInt =
      (BigInt(bankFeeNum) + BigInt(
        devFeeNum
      )) // This is never zero, devFeeNum can be zero but bankFeeNum cannot.

    val devFeeAmount: BigInt = divUp((dividend_2, divisor_2)) // Z
    val bankFeeAmount: BigInt = bankFeeAndDevFeeAmount - devFeeAmount // Y - Z

    val devFeeAmountAdjusted: BigInt =
      if (bankFeeAmount == BigInt(0)) BigInt(0) else devFeeAmount
    val bankFeeAmountAdjusted: BigInt =
      if (bankFeeAmount == BigInt(0)) devFeeAmount else bankFeeAmount

    val expectedAmountWithdrawn: BigInt =
      expectedAmountBeforeFees - bankFeeAmountAdjusted - devFeeAmountAdjusted
    (
      expectedAmountWithdrawn.toLong,
      devFeeAmountAdjusted.toLong,
      bankFeeAmountAdjusted.toLong
    )
  }

  def hodlTokenPrice(hodlBoxIn: InputBox): Long = {
    // preserving terminology from the contract
    val reserveIn = hodlBoxIn.getTokens.get(2).getValue
    val totalTokenSupply =
      hodlBoxIn.getRegisters.get(0).getValue.asInstanceOf[Long] // R4
    val hodlCoinsIn: Long = hodlBoxIn.getTokens.get(1).getValue
    val hodlCoinsCircIn: Long = totalTokenSupply - hodlCoinsIn
    val precisionFactor = extractPrecisionFactor(hodlBoxIn)
    val num = (BigInt(reserveIn) * BigInt(precisionFactor))
    assert(
      num >= hodlCoinsCircIn,
      "baseToken supply must be greater than or equal to hodlTokens in circulation"
    )
    (num / BigInt(
      hodlCoinsCircIn
    )).toLong
  }

  // amount of (nano) ERGs needed to mint given amount of hodlcoins against given hodl bank
  def mintAmountToken(hodlBoxIn: InputBox, hodlMintAmt: Long): Long = {
    val price = hodlTokenPrice(hodlBoxIn)
    val precisionFactor = extractPrecisionFactor(hodlBoxIn)
    hodlMintAmt * price / precisionFactor
  }

}

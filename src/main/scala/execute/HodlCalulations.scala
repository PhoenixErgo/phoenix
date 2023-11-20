package execute

import org.ergoplatform.appkit.InputBox

object HodlCalulations {

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

  def extractPrecisionFactor(hodlBoxIn: InputBox): Long = {
    hodlBoxIn.getRegisters.get(1).getValue.asInstanceOf[Long]
  }

  def hodlMintAmountFromERG(hodlBoxIn: InputBox, ergMintAmt: Long): BigInt = {
    val price = hodlPrice(hodlBoxIn)
    val precisionFactor = extractPrecisionFactor(hodlBoxIn)
    ergMintAmt * precisionFactor / price
  }

  def hodlTokenPrice(hodlBoxIn: InputBox): Long = {
    // preserving terminology from the contract
    val reserveIn = hodlBoxIn.getTokens.get(2).getValue
    val totalTokenSupply =
      hodlBoxIn.getRegisters.get(0).getValue.asInstanceOf[Long] // R4
    val hodlCoinsIn: Long = hodlBoxIn.getTokens.get(1).getValue
    val hodlCoinsCircIn: Long = totalTokenSupply - hodlCoinsIn
    val precisionFactor = extractPrecisionFactor(hodlBoxIn)
    ((BigInt(reserveIn) * BigInt(precisionFactor)) / BigInt(
      hodlCoinsCircIn
    )).toLong
  }

  def hodlTokenMintAmount(
      hodlBoxIn: InputBox,
      hodlTokenMintAmount: Long
  ): BigInt = {
    val price = hodlTokenPrice(hodlBoxIn)
    val precisionFactor = extractPrecisionFactor(hodlBoxIn)
    hodlTokenMintAmount * price / precisionFactor
  }

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

  def burnTokenAmount(
      hodlBoxIn: InputBox,
      hodlBurnAmt: Long
  ): (Long, Long, Long) = {
    val feeDenom = 1000L

    val bankFee =
      hodlBoxIn.getRegisters.get(4).getValue.asInstanceOf[Long] // R8
    val devFee = hodlBoxIn.getRegisters.get(3).getValue.asInstanceOf[Long] // R7

    val price = hodlTokenPrice(hodlBoxIn)
    val precisionFactor = extractPrecisionFactor(hodlBoxIn)
    val beforeFees = hodlBurnAmt * price / precisionFactor
    val bankFeeAmount: Long = (beforeFees * bankFee) / feeDenom
    println(beforeFees)
    println(devFee)
    println(feeDenom)
    val devFeeAmount: Long = (beforeFees * devFee) / feeDenom
    val expectedAmountWithdrawn: Long =
      beforeFees - bankFeeAmount - devFeeAmount
    (expectedAmountWithdrawn, devFeeAmount, bankFeeAmount)
  }

}

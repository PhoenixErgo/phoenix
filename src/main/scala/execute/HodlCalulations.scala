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
    BigInt(ergMintAmt) * BigInt(precisionFactor) / BigInt(price)
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
    BigInt(hodlTokenMintAmount) * BigInt(precisionFactor) / BigInt(price)
  }

  def burnAmount(hodlBoxIn: InputBox, hodlBurnAmt: Long): (Long, Long, Long) = {
    val feeDenom = 1000L

    val devFee =
      hodlBoxIn.getRegisters.get(3).getValue.asInstanceOf[Long] // R7
    val bankFee =
      hodlBoxIn.getRegisters.get(4).getValue.asInstanceOf[Long] // R8

    val price = hodlPrice(hodlBoxIn)
    val precisionFactor = extractPrecisionFactor(hodlBoxIn)
    val beforeFees = BigInt(hodlBurnAmt) * BigInt(price) / BigInt(precisionFactor)
    val bankFeeAmount = (beforeFees * BigInt(bankFee)) / BigInt(feeDenom)
    val devFeeAmount = (beforeFees * BigInt(devFee)) / BigInt(feeDenom)
    val expectedAmountWithdrawn =
      beforeFees - bankFeeAmount - devFeeAmount
    (expectedAmountWithdrawn.toLong, devFeeAmount.toLong, bankFeeAmount.toLong)
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

}
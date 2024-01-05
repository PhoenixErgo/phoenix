package mockchain.hodlToken

import mockClient.{Common, HttpClientTesting}
import mockchain.PhoenixCommon
import org.ergoplatform.appkit.{Address, ErgoContract, InputBox, OutBox}
import org.ergoplatform.sdk.ErgoToken
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.TransactionHelper

import scala.collection.mutable.ListBuffer

class FeeTokenSpec
    extends AnyFlatSpec
    with Matchers
    with HttpClientTesting
    with Common
    with PhoenixCommon {

  // mnemonic that belongs to one in contract
  // address: 9exfustUCPDKXsfDrGNrmtkyLDwAie2rKKdUsPVa26RuBFaYeCL
  override val txHelper = new TransactionHelper(
    ctx,
    "domain pencil motor legend high nurse grief degree anger pitch invite elite virus swift pottery",
    ""
  )

  val brunoAddress: Address = Address.create(
    "9exfustUCPDKXsfDrGNrmtkyLDwAie2rKKdUsPVa26RuBFaYeCL" // revert back to original address
  )
  val kushtiAddress: Address =
    Address.create("9iE2MadGSrn1ivHmRZJWRxzHffuAk6bPmEv6uJmPHuadBY8td5u")
  val phoenixAddress: Address =
    Address.create("9iPs1ujGj2eKXVg82aGyAtUtQZQWxFaki48KFixoaNmUAoTY6wV")

  val feeContractAmount: Long = (100 * math.pow(10, 9)).toLong

  val baseTokenId: String =
    "9a06d9e545a41fd51eeffc5e20d818073bf820c635e2a9d922269913e0de369d"

  override val brunoNum = 25L
  override val kushtiNum = 25L
  override val phoenixNum = 50L

  val feeTokenContract: ErgoContract = compiler.compileFeeTokenContract(
    feeTokenScript,
    minMinerFeeNanoErg,
    new ErgoToken(baseTokenId, 1L),
    brunoNum,
    phoenixNum,
    kushtiNum
  )

  def getDevBoxes(totalAmountForDevs: Long): Array[OutBox] = {
    Array(
      outBoxObj
        .hodlMintBox(
          brunoAddress,
          new ErgoToken(baseTokenId, (brunoNum * totalAmountForDevs) / feeDenom)
        ),
      outBoxObj.hodlMintBox(
        phoenixAddress,
        new ErgoToken(baseTokenId, (phoenixNum * totalAmountForDevs) / feeDenom)
      ),
      outBoxObj.hodlMintBox(
        kushtiAddress,
        new ErgoToken(baseTokenId, (kushtiNum * totalAmountForDevs) / feeDenom)
      )
    )
  }

  "FeeContractWithdrawal" should "work correctly when all conditions are satisfied" in {

    val feeContractInput =
      outBoxObj
        .hodlMintBox(
          feeTokenContract.toAddress,
          new ErgoToken(baseTokenId, feeContractAmount),
          minBoxValue + minBoxValue + minBoxValue + minMinerFeeNanoErg
        )
        .convertToInputWith(fakeTxId1, fakeIndex)

    val devBoxes = getDevBoxes(feeContractAmount)

    val unsignedTransaction = txHelper.buildUnsignedTransaction(
      inputs = Array(feeContractInput),
      outputs = devBoxes,
      fee = minMinerFeeNanoErg
    )

    noException shouldBe thrownBy {
      txHelper.signTransaction(
        unsignedTransaction
      )
    }

  }

  "FeeContractWithdrawal" must "operate correctly when all conditions are met, even with a high miner fee" in {

    val highMinerFee = minMinerFeeNanoErg * 10L // <-- this is changed

    val feeContractInput =
      outBoxObj
        .hodlMintBox(
          feeTokenContract.toAddress,
          new ErgoToken(baseTokenId, feeContractAmount),
          minBoxValue + minBoxValue + minBoxValue + highMinerFee // <-- this is changed
        )
        .convertToInputWith(fakeTxId1, fakeIndex)

    val devBoxes = getDevBoxes(feeContractAmount)

    val unsignedTransaction = txHelper.buildUnsignedTransaction(
      inputs = Array(feeContractInput),
      outputs = devBoxes,
      fee = highMinerFee // <-- this is changed
    )

    noException shouldBe thrownBy {
      txHelper.signTransaction(
        unsignedTransaction
      )
    }

  }

  "FeeContractWithdrawal" should "fail with incorrect allocation" in {

    def getDevBoxes(totalAmountForDevs: Long): Array[OutBox] = {
      Array(
        outBoxObj
          .hodlMintBox(
            brunoAddress,
            new ErgoToken(
              baseTokenId,
              (50L * totalAmountForDevs) / feeDenom // <-- this is changed
            )
          ),
        outBoxObj.hodlMintBox(
          phoenixAddress,
          new ErgoToken(
            baseTokenId,
            (40L * totalAmountForDevs) / feeDenom // <-- this is changed
          )
        ),
        outBoxObj.hodlMintBox(
          kushtiAddress,
          new ErgoToken(
            baseTokenId,
            (10L * totalAmountForDevs) / feeDenom // <-- this is changed
          )
        )
      )
    }

    val feeContractInput =
      outBoxObj
        .hodlMintBox(
          feeTokenContract.toAddress,
          new ErgoToken(baseTokenId, feeContractAmount),
          minBoxValue + minBoxValue + minBoxValue + minMinerFeeNanoErg
        )
        .convertToInputWith(fakeTxId1, fakeIndex)

    val devBoxes = getDevBoxes(feeContractAmount)

    val unsignedTransaction = txHelper.buildUnsignedTransaction(
      inputs = Array(feeContractInput),
      outputs = devBoxes,
      fee = minMinerFeeNanoErg
    )

    the[Exception] thrownBy {
      txHelper.signTransaction(unsignedTransaction)
    } should have message "Script reduced to false"

  }

  "FeeContractWithdrawal" must "fail when the wrong address is used for receiving" in {

    def getDevBoxes(totalAmountForDevs: Long): Array[OutBox] = {
      Array(
        outBoxObj
          .hodlMintBox(
            phoenixAddress, // <-- this is changed
            new ErgoToken(
              baseTokenId,
              (brunoNum * totalAmountForDevs) / feeDenom
            )
          ),
        outBoxObj.hodlMintBox(
          phoenixAddress,
          new ErgoToken(
            baseTokenId,
            (phoenixNum * totalAmountForDevs) / feeDenom
          )
        ),
        outBoxObj.hodlMintBox(
          kushtiAddress,
          new ErgoToken(
            baseTokenId,
            (kushtiNum * totalAmountForDevs) / feeDenom
          )
        )
      )
    }

    val feeContractInput =
      outBoxObj
        .hodlMintBox(
          feeTokenContract.toAddress,
          new ErgoToken(baseTokenId, feeContractAmount),
          minBoxValue + minBoxValue + minBoxValue + minMinerFeeNanoErg
        )
        .convertToInputWith(fakeTxId1, fakeIndex)

    val devBoxes = getDevBoxes(feeContractAmount)

    val unsignedTransaction = txHelper.buildUnsignedTransaction(
      inputs = Array(feeContractInput),
      outputs = devBoxes,
      fee = minMinerFeeNanoErg
    )

    the[Exception] thrownBy {
      txHelper.signTransaction(unsignedTransaction)
    } should have message "Script reduced to false"

  }

  "FeeContractWithdrawal" should "fail with incorrect signer" in {

    // random signer
    val txHelper = new TransactionHelper(
      ctx,
      "expire soon coral camp wing cross raccoon brick mango about sadness wine resist snake wire",
      ""
    )

    val feeContractInput =
      outBoxObj
        .hodlMintBox(
          feeTokenContract.toAddress,
          new ErgoToken(baseTokenId, feeContractAmount),
          minBoxValue + minBoxValue + minBoxValue + minMinerFeeNanoErg
        )
        .convertToInputWith(fakeTxId1, fakeIndex)

    val devBoxes = getDevBoxes(feeContractAmount)

    val unsignedTransaction = txHelper.buildUnsignedTransaction(
      inputs = Array(feeContractInput),
      outputs = devBoxes,
      fee = minMinerFeeNanoErg
    )

    the[AssertionError] thrownBy {
      txHelper.signTransaction(unsignedTransaction)
    }

  }

  "FeeContractWithdrawalWithMultipleInputs" should "work correctly when all conditions are satisfied" in {

    val feeContractInput =
      outBoxObj
        .hodlMintBox(
          feeTokenContract.toAddress,
          new ErgoToken(baseTokenId, feeContractAmount),
          minBoxValue + minBoxValue + minBoxValue + minMinerFeeNanoErg
        )
        .convertToInputWith(fakeTxId1, fakeIndex)

    val feeContractInput2 =
      outBoxObj
        .hodlMintBox(
          feeTokenContract.toAddress,
          new ErgoToken(baseTokenId, 5436363436L),
          minBoxValue + minBoxValue + minBoxValue + minMinerFeeNanoErg
        )
        .convertToInputWith(fakeTxId1, fakeIndex)

    val feeContractInput3 =
      outBoxObj
        .hodlMintBox(
          feeTokenContract.toAddress,
          new ErgoToken(baseTokenId, 415154265L),
          minBoxValue + minBoxValue + minBoxValue + minMinerFeeNanoErg
        )
        .convertToInputWith(fakeTxId1, fakeIndex)

    val inputs = Array(feeContractInput, feeContractInput2, feeContractInput3)

    val totalBaseTokenReward = inputs.map(_.getTokens.get(0).getValue).sum

    val devBoxes = getDevBoxes(totalBaseTokenReward)

    val totalBaseTokenDistributedReward =
      devBoxes.map(_.getTokens.get(0).getValue).sum

    val rewardTokenDifference =
      totalBaseTokenReward - totalBaseTokenDistributedReward // <-- required if tokens can't be split evenly between devs

    val outputs = if (rewardTokenDifference > 0) {
      devBoxes.init :+
        outBoxObj.hodlMintBox(
          kushtiAddress,
          new ErgoToken(
            baseTokenId,
            devBoxes.last.getTokens.get(0).getValue + rewardTokenDifference
          ), // <-- tx processor (or anyone specified) get extra tokens
          inputs
            .map(_.getValue)
            .sum - minBoxValue - minBoxValue - minMinerFeeNanoErg // <-- tx processor (or anyone specified) get extra ergs
        )
    } else {
      devBoxes
    }

    val unsignedTransaction = txHelper.buildUnsignedTransaction(
      inputs = inputs,
      outputs = outputs,
      fee = minMinerFeeNanoErg
    )

    noException shouldBe thrownBy {
      txHelper.signTransaction(
        unsignedTransaction
      )
    }

  }

}

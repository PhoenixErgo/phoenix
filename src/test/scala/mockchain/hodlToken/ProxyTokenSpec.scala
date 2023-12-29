package mockchain.hodlToken

import mockClient.{Common, HttpClientTesting}
import mockchain.PhoenixCommon
import org.ergoplatform.appkit.{Address, OutBox, Parameters}
import org.ergoplatform.sdk.ErgoToken
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utils.TransactionHelper

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class ProxyTokenSpec
    extends AnyFlatSpec
    with Matchers
    with HttpClientTesting
    with Common
    with PhoenixCommon {

  override val txHelper = new TransactionHelper(
    ctx,
    "domain pencil motor legend high nurse grief degree anger pitch invite elite virus swift pottery",
    ""
  )

  val minerFee = 1600000L
  val recommendedMinerFee: Long = 1000000L
  val bankERGAmount: Long = Parameters.OneErg

  val hodlBankSingleton = new ErgoToken(hodlBankNft, 1L)
  val dummyHodlTokens = new ErgoToken(hodlTokenId, 100L)

  val baseTokenTotalSupply = 10000000000000L // 1b, no decimals
  val baseTokenId: String =
    "9a06d9e545a41fd51eeffc5e20d818073bf820c635e2a9d922269913e0de369d"

  val startingTVLAmount = 1000000000L

  "PhoenixTokenMintOperationWithProxy" should "work correctly when all conditions are satisfied" in {

    val tokenMintAmount = 20

    val hodlTokens = new ErgoToken(hodlTokenId, baseTokenTotalSupply - 1L)
    val baseTokens = new ErgoToken(baseTokenId, startingTVLAmount)

    val hodlBox = outBoxObj
      .hodlBankBox(
        phoenixTokenContract,
        hodlBankSingleton,
        hodlTokens,
        baseTokenTotalSupply,
        precisionFactor,
        minBankValue,
        bankFee,
        devFee,
        bankERGAmount,
        Some(baseTokens)
      )
      .convertToInputWith(fakeTxId1, fakeIndex)

    val amountBaseTokenRequired = mintAmountToken(
      hodlBox,
      tokenMintAmount
    ) // given that you want 20 hodlTokens how much base tokens you need to deposit

    val proxyInput = outBoxObj
      .proxyMintInputBox(
        proxyTokenContract,
        userAddress,
        hodlBankSingleton,
        dummyHodlTokens,
        minBoxValue,
        minerFee,
        minTxOperatorFee,
        minBoxValue + minerFee + minTxOperatorFee,
        Some(new ErgoToken(baseTokenId, amountBaseTokenRequired))
      )
      .convertToInputWith(fakeTxId1, fakeIndex)

    val hodlOutBox = outBoxObj.hodlBankBox(
      phoenixTokenContract,
      hodlBankSingleton,
      new ErgoToken(
        hodlTokenId,
        hodlBox.getTokens.get(1).getValue - tokenMintAmount
      ),
      baseTokenTotalSupply,
      precisionFactor,
      minBankValue,
      bankFee,
      devFee,
      bankERGAmount,
      Some(
        ErgoToken(
          hodlBox.getTokens.get(2).getId,
          hodlBox.getTokens.get(2).getValue + amountBaseTokenRequired
        )
      )
    )

    val recipientBox = outBoxObj.hodlMintBox(
      userAddress,
      new ErgoToken(hodlTokenId, tokenMintAmount)
    )

    val unsignedTransaction = txHelper.buildUnsignedTransaction(
      inputs = Array(hodlBox, proxyInput),
      outputs = Array(hodlOutBox, recipientBox),
      fee = minerFee
    )

    noException shouldBe thrownBy {
      txHelper.signTransaction(
        unsignedTransaction
      )
    }

  }

  "PhoenixTokenMintOperationWithProxy" should "work correctly when all conditions are satisfied with a generous tx operator fee" in {

    val hodlTokenMintAmount = 20

    val hodlTokens = new ErgoToken(hodlTokenId, baseTokenTotalSupply - 1L)
    val baseTokens = new ErgoToken(baseTokenId, startingTVLAmount)

    val generousTxOperatorFee = 1000000000L

    val hodlBox = outBoxObj
      .hodlBankBox(
        phoenixTokenContract,
        hodlBankSingleton,
        hodlTokens,
        baseTokenTotalSupply,
        precisionFactor,
        minBankValue,
        bankFee,
        devFee,
        bankERGAmount,
        Some(baseTokens)
      )
      .convertToInputWith(fakeTxId1, fakeIndex)

    val amountBaseTokenRequired = mintAmountToken(hodlBox, hodlTokenMintAmount)

    val proxyInput = outBoxObj
      .proxyMintInputBox(
        proxyTokenContract,
        userAddress,
        hodlBankSingleton,
        dummyHodlTokens,
        minBoxValue,
        minerFee,
        generousTxOperatorFee, // <-- this is changed
        minBoxValue + minerFee + generousTxOperatorFee, // <-- this is changed
        Some(new ErgoToken(baseTokenId, amountBaseTokenRequired))
      )
      .convertToInputWith(fakeTxId1, fakeIndex)

    val hodlOutBox = outBoxObj.hodlBankBox(
      phoenixTokenContract,
      hodlBankSingleton,
      new ErgoToken(
        hodlTokenId,
        hodlBox.getTokens.get(1).getValue - hodlTokenMintAmount
      ),
      baseTokenTotalSupply,
      precisionFactor,
      minBankValue,
      bankFee,
      devFee,
      bankERGAmount,
      Some(
        ErgoToken(
          hodlBox.getTokens.get(2).getId,
          hodlBox.getTokens.get(2).getValue + amountBaseTokenRequired
        )
      )
    )

    val recipientBox = outBoxObj.hodlMintBox(
      userAddress,
      new ErgoToken(hodlTokenId, hodlTokenMintAmount)
    )

    val unsignedTransaction = txHelper.buildUnsignedTransaction(
      inputs = Array(hodlBox, proxyInput),
      outputs = Array(hodlOutBox, recipientBox),
      fee = minerFee
    )

    require(
      unsignedTransaction.getOutputs
        .get(3)
        .getValue == generousTxOperatorFee // <-- this is changed
      ,
      "Does not receive generous tx operator fee"
    )

    noException shouldBe thrownBy {
      txHelper.signTransaction(
        unsignedTransaction
      )
    }

  }

  "PhoenixTokenMintOperationWithProxy" should "fail when offchain code does not use the specified miner fee" in {

    val hodlTokenMintAmount = 20

    val hodlTokens = new ErgoToken(hodlTokenId, baseTokenTotalSupply - 1L)
    val baseTokens = new ErgoToken(baseTokenId, startingTVLAmount)

    val generousMinerFee = 1000000000L // <-- this is changed

    val hodlBox = outBoxObj
      .hodlBankBox(
        phoenixTokenContract,
        hodlBankSingleton,
        hodlTokens,
        baseTokenTotalSupply,
        precisionFactor,
        minBankValue,
        bankFee,
        devFee,
        bankERGAmount,
        Some(baseTokens)
      )
      .convertToInputWith(fakeTxId1, fakeIndex)

    val amountBaseTokenRequired = mintAmountToken(hodlBox, hodlTokenMintAmount)

    val proxyInput = outBoxObj
      .proxyMintInputBox(
        proxyTokenContract,
        userAddress,
        hodlBankSingleton,
        dummyHodlTokens,
        minBoxValue,
        generousMinerFee,
        minTxOperatorFee, // <-- this is changed
        minBoxValue + generousMinerFee + minTxOperatorFee, // <-- this is changed
        Some(new ErgoToken(baseTokenId, amountBaseTokenRequired))
      )
      .convertToInputWith(fakeTxId1, fakeIndex)

    val hodlOutBox = outBoxObj.hodlBankBox(
      phoenixTokenContract,
      hodlBankSingleton,
      new ErgoToken(
        hodlTokenId,
        hodlBox.getTokens.get(1).getValue - hodlTokenMintAmount
      ),
      baseTokenTotalSupply,
      precisionFactor,
      minBankValue,
      bankFee,
      devFee,
      bankERGAmount,
      Some(
        ErgoToken(
          hodlBox.getTokens.get(2).getId,
          hodlBox.getTokens.get(2).getValue + amountBaseTokenRequired
        )
      )
    )

    val recipientBox = outBoxObj.hodlMintBox(
      userAddress,
      new ErgoToken(hodlTokenId, hodlTokenMintAmount)
    )

    val unsignedTransaction = txHelper.buildUnsignedTransaction(
      inputs = Array(hodlBox, proxyInput),
      outputs = Array(hodlOutBox, recipientBox),
      fee =
        minerFee // offchain uses the usual miner fee (this is not good and will fail)
    )

    the[Exception] thrownBy {
      txHelper.signTransaction(unsignedTransaction)
    } should have message "Script reduced to false"

  }

  "PhoenixTokenMintOperationWithProxy" should "fail when offchain code sends to an address which does not belong to buyer" in {

    val hodlTokenMintAmount = 20

    val hodlTokens = new ErgoToken(hodlTokenId, baseTokenTotalSupply - 1L)
    val baseTokens = new ErgoToken(baseTokenId, startingTVLAmount)

    val hodlBox = outBoxObj
      .hodlBankBox(
        phoenixTokenContract,
        hodlBankSingleton,
        hodlTokens,
        baseTokenTotalSupply,
        precisionFactor,
        minBankValue,
        bankFee,
        devFee,
        bankERGAmount,
        Some(baseTokens)
      )
      .convertToInputWith(fakeTxId1, fakeIndex)

    val amountBaseTokenRequired = mintAmountToken(hodlBox, hodlTokenMintAmount)

    val proxyInput = outBoxObj
      .proxyMintInputBox(
        proxyTokenContract,
        userAddress,
        hodlBankSingleton,
        dummyHodlTokens,
        minBoxValue,
        minerFee,
        minTxOperatorFee,
        minBoxValue + minerFee + minTxOperatorFee,
        Some(new ErgoToken(baseTokenId, amountBaseTokenRequired))
      )
      .convertToInputWith(fakeTxId1, fakeIndex)

    val hodlOutBox = outBoxObj.hodlBankBox(
      phoenixTokenContract,
      hodlBankSingleton,
      new ErgoToken(
        hodlTokenId,
        hodlBox.getTokens.get(1).getValue - hodlTokenMintAmount
      ),
      baseTokenTotalSupply,
      precisionFactor,
      minBankValue,
      bankFee,
      devFee,
      bankERGAmount,
      Some(
        ErgoToken(
          hodlBox.getTokens.get(2).getId,
          hodlBox.getTokens.get(2).getValue + amountBaseTokenRequired
        )
      )
    )

    val recipientBox = outBoxObj.hodlMintBox(
      Address.create(
        "9gNYeyfRFUipiWZ3JR1ayDMoeh28E6J7aDQosb7yrzsuGSDqzCC"
      ), // <-- this is changed
      new ErgoToken(hodlTokenId, hodlTokenMintAmount)
    )

    val unsignedTransaction = txHelper.buildUnsignedTransaction(
      inputs = Array(hodlBox, proxyInput),
      outputs = Array(hodlOutBox, recipientBox),
      fee = minerFee
    )

    the[Exception] thrownBy {
      txHelper.signTransaction(unsignedTransaction)
    } should have message "Script reduced to false"

  }

  "PhoenixTokenMintOperationWithProxy" should "fail when offchain code sends to an address that belongs to buyer and a random person (MiM attack)" in {

    val hodlTokenMintAmount = 20

    val hodlTokens = new ErgoToken(hodlTokenId, baseTokenTotalSupply - 1L)
    val baseTokens = new ErgoToken(baseTokenId, startingTVLAmount)

    val hodlBox = outBoxObj
      .hodlBankBox(
        phoenixTokenContract,
        hodlBankSingleton,
        hodlTokens,
        baseTokenTotalSupply,
        precisionFactor,
        minBankValue,
        bankFee,
        devFee,
        bankERGAmount,
        Some(baseTokens)
      )
      .convertToInputWith(fakeTxId1, fakeIndex)

    val amountBaseTokenRequired = mintAmountToken(hodlBox, hodlTokenMintAmount)

    val proxyInput = outBoxObj
      .proxyMintInputBox(
        proxyTokenContract,
        userAddress,
        hodlBankSingleton,
        dummyHodlTokens,
        minBoxValue,
        minerFee,
        minTxOperatorFee,
        minBoxValue + minBoxValue + minerFee + minTxOperatorFee, // <-- extra min box value added to cover extra recipient box
        Some(new ErgoToken(baseTokenId, amountBaseTokenRequired))
      )
      .convertToInputWith(fakeTxId1, fakeIndex)

    val hodlOutBox = outBoxObj.hodlBankBox(
      phoenixTokenContract,
      hodlBankSingleton,
      new ErgoToken(
        hodlTokenId,
        hodlBox.getTokens.get(1).getValue - hodlTokenMintAmount
      ),
      baseTokenTotalSupply,
      precisionFactor,
      minBankValue,
      bankFee,
      devFee,
      bankERGAmount,
      Some(
        ErgoToken(
          hodlBox.getTokens.get(2).getId,
          hodlBox.getTokens.get(2).getValue + amountBaseTokenRequired
        )
      )
    )

    val recipientBox = outBoxObj.hodlMintBox(
      userAddress,
      new ErgoToken(hodlTokenId, hodlTokenMintAmount / 2) // <-- this is changed
    )

    val recipientBox2 = outBoxObj.hodlMintBox(
      Address.create(
        "9gNYeyfRFUipiWZ3JR1ayDMoeh28E6J7aDQosb7yrzsuGSDqzCC"
      ),
      new ErgoToken(hodlTokenId, hodlTokenMintAmount / 2) // <-- this is changed
    )

    val unsignedTransaction = txHelper.buildUnsignedTransaction(
      inputs = Array(hodlBox, proxyInput),
      outputs =
        Array(hodlOutBox, recipientBox, recipientBox2), // <-- this is changed
      fee = minerFee
    )

    the[Exception] thrownBy {
      txHelper.signTransaction(unsignedTransaction)
    } should have message "Script reduced to false"

  }

  "PhoenixTokenBurnOperationWithProxy" should "succeed when all conditions are met" in {

    val hodlTokenBurnAmount = 20

    val hodlTokens =
      new ErgoToken(hodlTokenId, baseTokenTotalSupply - hodlTokenBurnAmount)
    val baseTokens = new ErgoToken(baseTokenId, startingTVLAmount)

    val hodlBox = outBoxObj
      .hodlBankBox(
        phoenixTokenContract,
        hodlBankSingleton,
        hodlTokens,
        baseTokenTotalSupply,
        precisionFactor,
        minBankValue,
        bankFee,
        devFee,
        bankERGAmount,
        Some(baseTokens)
      )
      .convertToInputWith(fakeTxId1, fakeIndex)

    val (userBoxAmount, devFeeAmount, bankFeeAmount) =
      burnTokenAmount(hodlBox, hodlTokenBurnAmount)

    val proxyInput = outBoxObj
      .proxyBurnInputBox(
        proxyTokenContract,
        userAddress,
        hodlBankSingleton,
        new ErgoToken(hodlTokenId, hodlTokenBurnAmount),
        minBoxValue,
        minerFee,
        minTxOperatorFee,
        minerFee + minTxOperatorFee + minBoxValue + minBoxValue // <-- note that minBox value is needed for hodlToken compared to hodlCoin
      )
      .convertToInputWith(fakeTxId1, fakeIndex)

    // #1 minBoxValue is for recipient box
    // #2 minBoxValue is for dev fee box

    val hodlOutBox = outBoxObj.hodlBankBox(
      phoenixTokenContract,
      hodlBankSingleton,
      new ErgoToken(
        hodlTokenId,
        hodlBox.getTokens.get(1).getValue + proxyInput.getTokens.get(0).getValue
      ),
      baseTokenTotalSupply,
      precisionFactor,
      minBankValue,
      bankFee,
      devFee,
      bankERGAmount,
      Some(
        ErgoToken(
          hodlBox.getTokens.get(2).getId,
          hodlBox.getTokens.get(2).getValue - userBoxAmount - devFeeAmount
        )
      )
    )

    val recipientBox = outBoxObj.tokenOutBox(
      Array(ErgoToken(hodlBox.getTokens.get(2).getId, userBoxAmount)),
      userAddress
    )

    val devFeeBox =
      outBoxObj.tokenOutBox(
        Array(ErgoToken(hodlBox.getTokens.get(2).getId, devFeeAmount)),
        defaultFeeTokenContract.toAddress
      )

    val unsignedTransaction = txHelper.buildUnsignedTransaction(
      inputs = Array(hodlBox, proxyInput),
      outputs = Array(hodlOutBox, recipientBox, devFeeBox),
      fee = minerFee
    )

    noException shouldBe thrownBy {
      txHelper.signTransaction(
        unsignedTransaction
      )
    }

  }

  "PhoenixTokenBurnOperationWithProxy" should "succeed when all conditions are met with a generous tx operator fee" in {

    val hodlTokenBurnAmount = 20

    val hodlTokens =
      new ErgoToken(hodlTokenId, baseTokenTotalSupply - hodlTokenBurnAmount)
    val baseTokens = new ErgoToken(baseTokenId, startingTVLAmount)

    val generousTxOperatorFee = 1000000000L // <-- this is changed

    val hodlBox = outBoxObj
      .hodlBankBox(
        phoenixTokenContract,
        hodlBankSingleton,
        hodlTokens,
        baseTokenTotalSupply,
        precisionFactor,
        minBankValue,
        bankFee,
        devFee,
        bankERGAmount,
        Some(baseTokens)
      )
      .convertToInputWith(fakeTxId1, fakeIndex)

    val (userBoxAmount, devFeeAmount, bankFeeAmount) =
      burnTokenAmount(hodlBox, hodlTokenBurnAmount)

    val proxyInput = outBoxObj
      .proxyBurnInputBox(
        proxyTokenContract,
        userAddress,
        hodlBankSingleton,
        new ErgoToken(hodlTokenId, hodlTokenBurnAmount),
        minBoxValue,
        minerFee,
        generousTxOperatorFee, // <-- this is changed
        minerFee + generousTxOperatorFee + minBoxValue + minBoxValue // <-- this is changed
      )
      .convertToInputWith(fakeTxId1, fakeIndex)

    // #1 minBoxValue is for recipient box
    // #2 minBoxValue is for dev fee box

    val hodlOutBox = outBoxObj.hodlBankBox(
      phoenixTokenContract,
      hodlBankSingleton,
      new ErgoToken(
        hodlTokenId,
        hodlBox.getTokens.get(1).getValue + proxyInput.getTokens.get(0).getValue
      ),
      baseTokenTotalSupply,
      precisionFactor,
      minBankValue,
      bankFee,
      devFee,
      bankERGAmount,
      Some(
        ErgoToken(
          hodlBox.getTokens.get(2).getId,
          hodlBox.getTokens.get(2).getValue - userBoxAmount - devFeeAmount
        )
      )
    )

    val recipientBox = outBoxObj.tokenOutBox(
      Array(ErgoToken(hodlBox.getTokens.get(2).getId, userBoxAmount)),
      userAddress
    )

    val devFeeBox =
      outBoxObj.tokenOutBox(
        Array(ErgoToken(hodlBox.getTokens.get(2).getId, devFeeAmount)),
        defaultFeeTokenContract.toAddress
      )

    val unsignedTransaction = txHelper.buildUnsignedTransaction(
      inputs = Array(hodlBox, proxyInput),
      outputs = Array(hodlOutBox, recipientBox, devFeeBox),
      fee = minerFee
    )

    require(
      unsignedTransaction.getOutputs
        .get(4)
        .getValue == generousTxOperatorFee, // <-- this is changed
      "Does not receive generous tx operator fee"
    )

    noException shouldBe thrownBy {
      txHelper.signTransaction(
        unsignedTransaction
      )
    }

  }

  "PhoenixTokenBurnOperationWithProxy" should "fail when offchain code does not use the specified miner fee" in {

    val hodlTokenBurnAmount = 20

    val hodlTokens =
      new ErgoToken(hodlTokenId, baseTokenTotalSupply - hodlTokenBurnAmount)
    val baseTokens = new ErgoToken(baseTokenId, startingTVLAmount)

    val generousMinerFee = 1000000000L // <-- this is changed

    val hodlBox = outBoxObj
      .hodlBankBox(
        phoenixTokenContract,
        hodlBankSingleton,
        hodlTokens,
        baseTokenTotalSupply,
        precisionFactor,
        minBankValue,
        bankFee,
        devFee,
        bankERGAmount,
        Some(baseTokens)
      )
      .convertToInputWith(fakeTxId1, fakeIndex)

    val (userBoxAmount, devFeeAmount, bankFeeAmount) =
      burnTokenAmount(hodlBox, hodlTokenBurnAmount)

    val proxyInput = outBoxObj
      .proxyBurnInputBox(
        proxyTokenContract,
        userAddress,
        hodlBankSingleton,
        new ErgoToken(hodlTokenId, hodlTokenBurnAmount),
        minBoxValue,
        generousMinerFee,
        minTxOperatorFee,
        generousMinerFee + minTxOperatorFee + minBoxValue + minBoxValue // <-- this is changed
      )
      .convertToInputWith(fakeTxId1, fakeIndex)

    // #1 minBoxValue is for recipient box
    // #2 minBoxValue is for dev fee box

    val hodlOutBox = outBoxObj.hodlBankBox(
      phoenixTokenContract,
      hodlBankSingleton,
      new ErgoToken(
        hodlTokenId,
        hodlBox.getTokens.get(1).getValue + proxyInput.getTokens.get(0).getValue
      ),
      baseTokenTotalSupply,
      precisionFactor,
      minBankValue,
      bankFee,
      devFee,
      bankERGAmount,
      Some(
        ErgoToken(
          hodlBox.getTokens.get(2).getId,
          hodlBox.getTokens.get(2).getValue - userBoxAmount - devFeeAmount
        )
      )
    )

    val recipientBox = outBoxObj.tokenOutBox(
      Array(ErgoToken(hodlBox.getTokens.get(2).getId, userBoxAmount)),
      userAddress
    )

    val devFeeBox =
      outBoxObj.tokenOutBox(
        Array(ErgoToken(hodlBox.getTokens.get(2).getId, devFeeAmount)),
        defaultFeeTokenContract.toAddress
      )

    val unsignedTransaction = txHelper.buildUnsignedTransaction(
      inputs = Array(hodlBox, proxyInput),
      outputs = Array(hodlOutBox, recipientBox, devFeeBox),
      fee = minerFee // <-- offchain code does not use the specified miner fee
    )

    the[Exception] thrownBy {
      txHelper.signTransaction(unsignedTransaction)
    } should have message "Script reduced to false"

  }

  "PhoenixTokenBurnOperationWithProxy" should "fail when offchain code sends to an address which does not belong to buyer" in {
    val hodlTokenBurnAmount = 20

    val hodlTokens =
      new ErgoToken(hodlTokenId, baseTokenTotalSupply - hodlTokenBurnAmount)
    val baseTokens = new ErgoToken(baseTokenId, startingTVLAmount)

    val hodlBox = outBoxObj
      .hodlBankBox(
        phoenixTokenContract,
        hodlBankSingleton,
        hodlTokens,
        baseTokenTotalSupply,
        precisionFactor,
        minBankValue,
        bankFee,
        devFee,
        bankERGAmount,
        Some(baseTokens)
      )
      .convertToInputWith(fakeTxId1, fakeIndex)

    val (userBoxAmount, devFeeAmount, bankFeeAmount) =
      burnTokenAmount(hodlBox, hodlTokenBurnAmount)

    val proxyInput = outBoxObj
      .proxyBurnInputBox(
        proxyTokenContract,
        userAddress,
        hodlBankSingleton,
        new ErgoToken(hodlTokenId, hodlTokenBurnAmount),
        minBoxValue,
        minerFee,
        minTxOperatorFee,
        minerFee + minTxOperatorFee + minBoxValue + minBoxValue // <-- note that minBox value is needed for hodlToken compared to hodlCoin
      )
      .convertToInputWith(fakeTxId1, fakeIndex)

    // #1 minBoxValue is for recipient box
    // #2 minBoxValue is for dev fee box

    val hodlOutBox = outBoxObj.hodlBankBox(
      phoenixTokenContract,
      hodlBankSingleton,
      new ErgoToken(
        hodlTokenId,
        hodlBox.getTokens.get(1).getValue + proxyInput.getTokens.get(0).getValue
      ),
      baseTokenTotalSupply,
      precisionFactor,
      minBankValue,
      bankFee,
      devFee,
      bankERGAmount,
      Some(
        ErgoToken(
          hodlBox.getTokens.get(2).getId,
          hodlBox.getTokens.get(2).getValue - userBoxAmount - devFeeAmount
        )
      )
    )

    val recipientBox = outBoxObj.tokenOutBox(
      Array(ErgoToken(hodlBox.getTokens.get(2).getId, userBoxAmount)),
      Address.create(
        "9gNYeyfRFUipiWZ3JR1ayDMoeh28E6J7aDQosb7yrzsuGSDqzCC" // <-- this is changed
      )
    )

    val devFeeBox =
      outBoxObj.tokenOutBox(
        Array(ErgoToken(hodlBox.getTokens.get(2).getId, devFeeAmount)),
        defaultFeeTokenContract.toAddress
      )

    val unsignedTransaction = txHelper.buildUnsignedTransaction(
      inputs = Array(hodlBox, proxyInput),
      outputs = Array(hodlOutBox, recipientBox, devFeeBox),
      fee = minerFee
    )

    the[Exception] thrownBy {
      txHelper.signTransaction(unsignedTransaction)
    } should have message "Script reduced to false"
  }

  "PhoenixTokenBurnOperationWithProxy" should "fail when offchain code sends to an address that belongs to buyer and a random person (MiM attack)" in {
    val hodlTokenBurnAmount = 20

    val hodlTokens =
      new ErgoToken(hodlTokenId, baseTokenTotalSupply - hodlTokenBurnAmount)
    val baseTokens = new ErgoToken(baseTokenId, startingTVLAmount)

    val hodlBox = outBoxObj
      .hodlBankBox(
        phoenixTokenContract,
        hodlBankSingleton,
        hodlTokens,
        baseTokenTotalSupply,
        precisionFactor,
        minBankValue,
        bankFee,
        devFee,
        bankERGAmount,
        Some(baseTokens)
      )
      .convertToInputWith(fakeTxId1, fakeIndex)

    val (userBoxAmount, devFeeAmount, bankFeeAmount) =
      burnTokenAmount(hodlBox, hodlTokenBurnAmount)

    val proxyInput = outBoxObj
      .proxyBurnInputBox(
        proxyTokenContract,
        userAddress,
        hodlBankSingleton,
        new ErgoToken(hodlTokenId, hodlTokenBurnAmount),
        minBoxValue,
        minerFee,
        minTxOperatorFee,
        minerFee + minTxOperatorFee + minBoxValue + minBoxValue + minBoxValue // <-- note that there is an extra minBoxValue to account for extra recipient box
      )
      .convertToInputWith(fakeTxId1, fakeIndex)

    // #1 minBoxValue is for recipient box
    // #2 minBoxValue is for dev fee box

    val hodlOutBox = outBoxObj.hodlBankBox(
      phoenixTokenContract,
      hodlBankSingleton,
      new ErgoToken(
        hodlTokenId,
        hodlBox.getTokens.get(1).getValue + proxyInput.getTokens.get(0).getValue
      ),
      baseTokenTotalSupply,
      precisionFactor,
      minBankValue,
      bankFee,
      devFee,
      bankERGAmount,
      Some(
        ErgoToken(
          hodlBox.getTokens.get(2).getId,
          hodlBox.getTokens.get(2).getValue - userBoxAmount - devFeeAmount
        )
      )
    )

    val recipientBox = outBoxObj.tokenOutBox(
      Array(
        ErgoToken(hodlBox.getTokens.get(2).getId, userBoxAmount / 2)
      ), // <-- this is changed
      userAddress
    )

    val recipientBox2 = outBoxObj.tokenOutBox(
      Array(
        ErgoToken(hodlBox.getTokens.get(2).getId, userBoxAmount / 2)
      ), // <-- this is changed
      Address.create(
        "9gNYeyfRFUipiWZ3JR1ayDMoeh28E6J7aDQosb7yrzsuGSDqzCC" // <-- this is changed
      )
    )

    val devFeeBox =
      outBoxObj.tokenOutBox(
        Array(ErgoToken(hodlBox.getTokens.get(2).getId, devFeeAmount)),
        defaultFeeTokenContract.toAddress
      )

    val unsignedTransaction = txHelper.buildUnsignedTransaction(
      inputs = Array(hodlBox, proxyInput),
      outputs = Array(
        hodlOutBox,
        recipientBox,
        recipientBox2,
        devFeeBox
      ), // <-- this is changed
      fee = minerFee
    )

    the[Exception] thrownBy {
      txHelper.signTransaction(unsignedTransaction)
    } should have message "Script reduced to false"
  }

  "ProxyTokenRefund" should "work correctly when all conditions are satisfied" in {

    val amountBaseTokenRequired = 20
    // note that the buyer has to sign their own refund tx
    val txHelper = new TransactionHelper(
      ctx,
      "pond trick believe salt obscure wool end state thing fringe reunion legend quarter popular oak",
      ""
    )
    val userAddress = txHelper.senderAddress

    val fundingBox = outBoxObj
      .simpleOutBox(userAddress, recommendedMinerFee)
      .convertToInputWith(fakeTxId1, fakeIndex)

    val proxyInput = outBoxObj
      .proxyMintInputBox(
        proxyTokenContract,
        userAddress,
        hodlBankSingleton,
        dummyHodlTokens,
        minBoxValue,
        minerFee,
        minTxOperatorFee,
        minBoxValue + minerFee + minTxOperatorFee,
        Some(new ErgoToken(baseTokenId, amountBaseTokenRequired))
      )
      .convertToInputWith(fakeTxId1, fakeIndex)

    val recipientBox = outBoxObj.tokenOutBox(
      Array(new ErgoToken(baseTokenId, amountBaseTokenRequired)),
      userAddress,
      recommendedMinerFee
    )

    val unsignedTransaction = txHelper.buildUnsignedTransaction(
      inputs = Array(fundingBox, proxyInput),
      outputs = Array(recipientBox),
      fee = recommendedMinerFee
    )

    noException shouldBe thrownBy {
      txHelper.signTransaction(
        unsignedTransaction
      )

    }
  }

  "ProxyTokenRefund" should "function properly irrespective of the input order, given all conditions are met" in {

    val amountBaseTokenRequired = 20
    // note that the buyer has to sign their own refund tx
    val txHelper = new TransactionHelper(
      ctx,
      "pond trick believe salt obscure wool end state thing fringe reunion legend quarter popular oak",
      ""
    )
    val userAddress = txHelper.senderAddress

    val fundingBox = outBoxObj
      .simpleOutBox(userAddress, recommendedMinerFee)
      .convertToInputWith(fakeTxId1, fakeIndex)

    val proxyInput = outBoxObj
      .proxyMintInputBox(
        proxyTokenContract,
        userAddress,
        hodlBankSingleton,
        dummyHodlTokens,
        minBoxValue,
        minerFee,
        minTxOperatorFee,
        minBoxValue + minerFee + minTxOperatorFee,
        Some(new ErgoToken(baseTokenId, amountBaseTokenRequired))
      )
      .convertToInputWith(fakeTxId1, fakeIndex)

    val recipientBox = outBoxObj.tokenOutBox(
      Array(new ErgoToken(baseTokenId, amountBaseTokenRequired)),
      userAddress,
      recommendedMinerFee
    )

    val unsignedTransaction = txHelper.buildUnsignedTransaction(
      inputs = Array(proxyInput, fundingBox),
      outputs = Array(recipientBox),
      fee = recommendedMinerFee
    )

    noException shouldBe thrownBy {
      txHelper.signTransaction(
        unsignedTransaction
      )

    }
  }

  "ProxyTokenRefund" should "fail when signed by someone other than the buyer" in {

    val amountBaseTokenRequired = 20
    // note that the buyer has to sign their own refund tx

    val fundingBox = outBoxObj
      .simpleOutBox(userAddress, recommendedMinerFee)
      .convertToInputWith(fakeTxId1, fakeIndex)

    val proxyInput = outBoxObj
      .proxyMintInputBox(
        proxyTokenContract,
        userAddress,
        hodlBankSingleton,
        dummyHodlTokens,
        minBoxValue,
        minerFee,
        minTxOperatorFee,
        minBoxValue + minerFee + minTxOperatorFee,
        Some(new ErgoToken(baseTokenId, amountBaseTokenRequired))
      )
      .convertToInputWith(fakeTxId1, fakeIndex)

    val recipientBox = outBoxObj.tokenOutBox(
      Array(new ErgoToken(baseTokenId, amountBaseTokenRequired)),
      userAddress,
      recommendedMinerFee
    )

    val unsignedTransaction = txHelper.buildUnsignedTransaction(
      inputs = Array(fundingBox, proxyInput),
      outputs = Array(recipientBox),
      fee = recommendedMinerFee
    )

    the[AssertionError] thrownBy {
      txHelper.signTransaction(unsignedTransaction)
    }

  }

  "ProxyTokenRefundWithMultipleInputs" should "work correctly when all conditions are satisfied" in {

    val ergMintAmount = 40L
    // note that the buyer has to sign their own refund tx
    val txHelper = new TransactionHelper(
      ctx,
      "pond trick believe salt obscure wool end state thing fringe reunion legend quarter popular oak",
      ""
    )
    val userAddress = txHelper.senderAddress

    val fundingBox = outBoxObj
      .simpleOutBox(userAddress, recommendedMinerFee)
      .convertToInputWith(fakeTxId1, fakeIndex)

    val proxyInput = outBoxObj
      .proxyMintInputBox(
        proxyTokenContract,
        userAddress,
        hodlBankSingleton,
        dummyHodlTokens,
        minBoxValue,
        minerFee,
        minTxOperatorFee,
        ergMintAmount + minBoxValue + minerFee + minTxOperatorFee
      )
      .convertToInputWith(fakeTxId1, fakeIndex)

    val proxyInput2 = outBoxObj
      .proxyMintInputBox(
        proxyTokenContract,
        userAddress,
        hodlBankSingleton,
        dummyHodlTokens,
        minBoxValue,
        minerFee,
        minTxOperatorFee,
        100000L + minBoxValue + minerFee + minTxOperatorFee
      )
      .convertToInputWith(fakeTxId2, fakeIndex)

    val proxyInput3 = outBoxObj
      .proxyMintInputBox(
        proxyTokenContract,
        userAddress,
        hodlBankSingleton,
        dummyHodlTokens,
        minBoxValue,
        minerFee,
        minTxOperatorFee,
        10041514300L + minBoxValue + minerFee + minTxOperatorFee
      )
      .convertToInputWith(fakeTxId3, fakeIndex)

    val recipientBox = outBoxObj.simpleOutBox(
      userAddress,
      proxyInput.getValue + proxyInput2.getValue + proxyInput3.getValue
    )

    val unsignedTransaction = txHelper.buildUnsignedTransaction(
      inputs = Array(fundingBox, proxyInput, proxyInput2, proxyInput3),
      outputs = Array(recipientBox),
      fee = recommendedMinerFee
    )

    noException shouldBe thrownBy {
      txHelper.signTransaction(
        unsignedTransaction
      )

    }
  }

  "ProxyTokenRefundWithMultipleInputsAndTokens" should "work correctly when all conditions are satisfied" in {

    val ergMintAmount = 40L
    // note that the buyer has to sign their own refund tx
    val txHelper = new TransactionHelper(
      ctx,
      "pond trick believe salt obscure wool end state thing fringe reunion legend quarter popular oak",
      ""
    )
    val userAddress = txHelper.senderAddress

    val fundingBox = outBoxObj
      .simpleOutBox(userAddress, recommendedMinerFee)
      .convertToInputWith(fakeTxId1, fakeIndex)

    val proxyInput = outBoxObj
      .proxyBurnInputBox(
        proxyTokenContract,
        userAddress,
        hodlBankSingleton,
        dummyHodlTokens,
        minBoxValue,
        minerFee,
        minTxOperatorFee,
        ergMintAmount + minBoxValue + minerFee + minTxOperatorFee
      )
      .convertToInputWith(fakeTxId1, fakeIndex)

    val proxyInput2 = outBoxObj
      .proxyBurnInputBox(
        proxyTokenContract,
        userAddress,
        hodlBankSingleton,
        new ErgoToken(hodlTokenId, 47L),
        minBoxValue,
        minerFee,
        minTxOperatorFee,
        100000L + minBoxValue + minerFee + minTxOperatorFee
      )
      .convertToInputWith(fakeTxId2, fakeIndex)

    val proxyInput3 = outBoxObj
      .proxyBurnInputBox(
        proxyTokenContract,
        userAddress,
        hodlBankSingleton,
        new ErgoToken(hodlTokenId, 52438924L),
        minBoxValue,
        minerFee,
        minTxOperatorFee,
        10041514300L + minBoxValue + minerFee + minTxOperatorFee
      )
      .convertToInputWith(fakeTxId3, fakeIndex)

    val inputs = Array(fundingBox, proxyInput, proxyInput2, proxyInput3)

    val outputs = inputs.tail.map { i =>
      val recipientBox = outBoxObj.optionalTokenOutBox(
        i.getTokens.asScala,
        userAddress,
        i.getValue
      )
      recipientBox
    }

    val unsignedTransaction = txHelper.buildUnsignedTransaction(
      inputs = inputs,
      outputs = outputs,
      fee = recommendedMinerFee
    )

    noException shouldBe thrownBy {
      txHelper.signTransaction(
        unsignedTransaction
      )

    }
  }

  "ProxyTokenRefundWithMultipleInputsAndTokens" should "fail when all tokens are not returned" in {

    val ergMintAmount = 40L
    // note that the buyer has to sign their own refund tx
    val txHelper = new TransactionHelper(
      ctx,
      "pond trick believe salt obscure wool end state thing fringe reunion legend quarter popular oak",
      ""
    )
    val userAddress = txHelper.senderAddress

    val fundingBox = outBoxObj
      .simpleOutBox(userAddress, recommendedMinerFee)
      .convertToInputWith(fakeTxId1, fakeIndex)

    val proxyInput = outBoxObj
      .proxyBurnInputBox(
        proxyContract,
        userAddress,
        hodlBankSingleton,
        dummyHodlTokens,
        minBoxValue,
        minerFee,
        minTxOperatorFee,
        ergMintAmount + minBoxValue + minerFee + minTxOperatorFee
      )
      .convertToInputWith(fakeTxId1, fakeIndex)

    val proxyInput2 = outBoxObj
      .proxyBurnInputBox(
        proxyContract,
        userAddress,
        hodlBankSingleton,
        new ErgoToken(hodlTokenId, 47L),
        minBoxValue,
        minerFee,
        minTxOperatorFee,
        100000L + minBoxValue + minerFee + minTxOperatorFee
      )
      .convertToInputWith(fakeTxId2, fakeIndex)

    val proxyInput3 = outBoxObj
      .proxyBurnInputBox(
        proxyContract,
        userAddress,
        hodlBankSingleton,
        new ErgoToken(hodlTokenId, 52438924L),
        minBoxValue,
        minerFee,
        minTxOperatorFee,
        10041514300L + minBoxValue + minerFee + minTxOperatorFee
      )
      .convertToInputWith(fakeTxId3, fakeIndex)

    val inputs = Array(fundingBox, proxyInput, proxyInput2, proxyInput3)

    val outputs = {
      val outputsList = new ListBuffer[OutBox]
      // remove token from third input so it can be burned
      inputs.tail.zipWithIndex.collect { case (input, index) =>
        val tokens: mutable.Buffer[ErgoToken] =
          if (index == 2) mutable.Buffer.empty[ErgoToken]
          else input.getTokens.asScala
        val recipientBox =
          outBoxObj.optionalTokenOutBox(tokens, userAddress, input.getValue)
        outputsList.append(recipientBox)
      }
      outputsList
    }

    val unsignedTransaction = txHelper.buildUnsignedTransaction(
      inputs = inputs,
      outputs = outputs,
      tokensToBurn = proxyInput3.getTokens.asScala,
      fee = recommendedMinerFee
    )

    the[Exception] thrownBy {
      txHelper.signTransaction(unsignedTransaction)
    } should have message "Script reduced to false"

  }

  "ProxyTokenRefundWithMultipleInputsAndDifferentTokens" should "work correctly when all conditions are satisfied" in {

    val ergMintAmount = 40L
    // note that the buyer has to sign their own refund tx
    val txHelper = new TransactionHelper(
      ctx,
      "pond trick believe salt obscure wool end state thing fringe reunion legend quarter popular oak",
      ""
    )
    val userAddress = txHelper.senderAddress

    val fundingBox = outBoxObj
      .simpleOutBox(userAddress, recommendedMinerFee)
      .convertToInputWith(fakeTxId1, fakeIndex)

    val proxyInput = outBoxObj
      .proxyBurnInputBox(
        proxyContract,
        userAddress,
        hodlBankSingleton,
        dummyHodlTokens,
        minBoxValue,
        minerFee,
        minTxOperatorFee,
        ergMintAmount + minBoxValue + minerFee + minTxOperatorFee
      )
      .convertToInputWith(fakeTxId1, fakeIndex)

    val proxyInput2 = outBoxObj
      .proxyBurnInputBox(
        proxyContract,
        userAddress,
        hodlBankSingleton,
        new ErgoToken(hodlTokenId, 47L),
        minBoxValue,
        minerFee,
        minTxOperatorFee,
        100000L + minBoxValue + minerFee + minTxOperatorFee
      )
      .convertToInputWith(fakeTxId2, fakeIndex)

    val proxyInput3 = outBoxObj
      .proxyBurnInputBox(
        proxyContract,
        userAddress,
        hodlBankSingleton,
        new ErgoToken(dexyUSD, 52438924L),
        minBoxValue,
        minerFee,
        minTxOperatorFee,
        10041514300L + minBoxValue + minerFee + minTxOperatorFee
      )
      .convertToInputWith(fakeTxId3, fakeIndex)

    val inputs = Array(fundingBox, proxyInput, proxyInput2, proxyInput3)

    val outputs = inputs.tail.map { i =>
      val recipientBox = outBoxObj.optionalTokenOutBox(
        i.getTokens.asScala,
        userAddress,
        i.getValue
      )
      recipientBox
    }

    val unsignedTransaction = txHelper.buildUnsignedTransaction(
      inputs = inputs,
      outputs = outputs,
      fee = recommendedMinerFee
    )

    noException shouldBe thrownBy {
      txHelper.signTransaction(
        unsignedTransaction
      )

    }
  }

}

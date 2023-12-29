package mockchain.token

import mockClient.Common
import mockchain.PhoenixCommon
import org.ergoplatform.appkit.{Address, OutBox, Parameters}
import org.ergoplatform.sdk.ErgoToken
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PhoenixTokenSpec
    extends AnyFlatSpec
    with Matchers
    with Common
    with PhoenixCommon {

  val minAmount = 1000000L
  val bankERGAmount: Long = Parameters.OneErg
  val recommendedMinerFee = 1000000L
  override val minBankValue = 1L
  override val precisionFactor: Long = 1L

  val baseTokenTotalSupply: Long = 40000000000000L
  val baseTokenId: String =
    "9a06d9e545a41fd51eeffc5e20d818073bf820c635e2a9d922269913e0de369d"

  val hodlBankSingleton = new ErgoToken(hodlBankNft, 1L)

  val startingTVLAmount: Long = 1000L

  "PhoenixTokenMintOperation" should "work correctly when all conditions are satisfied" in {

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

    val price = hodlTokenPrice(hodlBox)


    require(
      hodlBox.getTokens
        .get(2)
        .getValue >= baseTokenTotalSupply - baseTokenTotalSupply - 1L,
      "never-decreasing theorem does not hold"
    )

    require(
      price == 1000,
      "Price does not correspond to manually calculated value"
    )

    val amountBaseTokenRequired = mintAmountToken(hodlBox, hodlTokenMintAmount)

    require(
      amountBaseTokenRequired == 20000,
      s"Token ($amountBaseTokenRequired) delta does not correspond to manually calculated value "
    )

    val fundingBox = outBoxObj
      .genericContractBox(
        compiler.compileDummyContract(),
        tokenFundingBoxValue,
        Seq(
          ErgoToken(hodlBox.getTokens.get(2).getId, amountBaseTokenRequired)
        )
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
      new ErgoToken(hodlTokenId, hodlTokenMintAmount),
      minAmount
    )

    val unsignedTransaction = txHelper.buildUnsignedTransaction(
      inputs = Array(hodlBox, fundingBox),
      outputs = Array(hodlOutBox, recipientBox)
    )

    noException shouldBe thrownBy {
      txHelper.signTransaction(
        unsignedTransaction
      )
    }
  }

  "PhoenixTokenMintOperation" should "work while hodl price is one to one" in {

    val hodlTokenMintAmount = 1 // can be any amount

    val hodlTokens = new ErgoToken(hodlTokenId, baseTokenTotalSupply - 2000000L) // note same value is subtracted as token amount below
    val baseTokens = new ErgoToken(baseTokenId, 2000000L)


    val precisionFactor = 1L // should be one for tokens with no decimals

    val hodlBox = outBoxObj
      .hodlBankBox(
        phoenixTokenContract,
        hodlBankSingleton,
        hodlTokens,
        baseTokenTotalSupply, // supply of hodlTokens must be same as supply of base tokens
        precisionFactor,
        minBankValue,
        bankFee,
        devFee,
        bankERGAmount,
        Some(baseTokens)
      )
      .convertToInputWith(fakeTxId1, fakeIndex)

    val price = hodlTokenPrice(hodlBox)

    require(
      hodlBox.getTokens
        .get(2)
        .getValue >= baseTokenTotalSupply - baseTokenTotalSupply - 1L,
      "never-decreasing theorem does not hold"
    )

    require(
      price == 1,
      "Price does not correspond to manually calculated value"
    )

    val amountBaseTokenRequired = mintAmountToken(hodlBox, hodlTokenMintAmount)

    require(
      amountBaseTokenRequired == 1,
      s"Token ($amountBaseTokenRequired) delta does not correspond to manually calculated value "
    )

    val fundingBox = outBoxObj
      .genericContractBox(
        compiler.compileDummyContract(),
        tokenFundingBoxValue,
        Seq(
          ErgoToken(hodlBox.getTokens.get(2).getId, amountBaseTokenRequired)
        )
      )
      .convertToInputWith(fakeTxId1, fakeIndex)

    val hodlOutBox = outBoxObj.hodlBankBox(
      phoenixTokenContract,
      hodlBankSingleton,
      new ErgoToken(
        hodlTokenId,
        hodlBox.getTokens.get(1).getValue - hodlTokenMintAmount
      ),
      baseTokenTotalSupply, // supply of hodlTokens must be same as supply of base tokens
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
      new ErgoToken(hodlTokenId, hodlTokenMintAmount),
      minAmount
    )

    val unsignedTransaction = txHelper.buildUnsignedTransaction(
      inputs = Array(hodlBox, fundingBox),
      outputs = Array(hodlOutBox, recipientBox)
    )

    noException shouldBe thrownBy {
      txHelper.signTransaction(
        unsignedTransaction
      )
    }
  }

  "PhoenixTokenMintOperation" should "work with multiple recipients" in {

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

    val price = hodlTokenPrice(hodlBox)

    require(
      hodlBox.getTokens
        .get(2)
        .getValue >= baseTokenTotalSupply - baseTokenTotalSupply - 1L,
      "never-decreasing theorem does not hold"
    )
    require(
      price == 1000,
      "Price does not correspond to manually calculated value"
    )

    val amountBaseTokenRequired = mintAmountToken(hodlBox, hodlTokenMintAmount)
    require(
      amountBaseTokenRequired == 20000,
      s"Token ($amountBaseTokenRequired) delta does not correspond to manually calculated value "
    )

    val fundingBox = outBoxObj
      .genericContractBox(
        compiler.compileDummyContract(),
        tokenFundingBoxValue + 1000000L,
        Seq(
          ErgoToken(hodlBox.getTokens.get(2).getId, amountBaseTokenRequired)
        )
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
      new ErgoToken(hodlTokenId, 10),
      minAmount
    )

    val recipientBox2 = outBoxObj.hodlMintBox(
      Address.create("9ehbiMqN88ctdANajxz8UB2G2MHYcBgbrTFEbgCFKPWrmNnSfjR"),
      new ErgoToken(hodlTokenId, 10),
      minAmount
    )

    val unsignedTransaction = txHelper.buildUnsignedTransaction(
      inputs = Array(hodlBox, fundingBox),
      outputs = Array(hodlOutBox, recipientBox, recipientBox2)
    )

    noException shouldBe thrownBy {
      txHelper.signTransaction(
        unsignedTransaction
      )
    }
  }

  "PhoenixTokenMintOperation" should "fail when excess hodlToken is withdrawn from bank" in {

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

    val price = hodlTokenPrice(hodlBox)

    require(
      hodlBox.getTokens
        .get(2)
        .getValue >= baseTokenTotalSupply - baseTokenTotalSupply - 1L,
      "never-decreasing theorem does not hold"
    )
    require(
      price == 1000,
      "Price does not correspond to manually calculated value"
    )

    val amountBaseTokenRequired = mintAmountToken(hodlBox, hodlTokenMintAmount)
    require(
      amountBaseTokenRequired == 20000,
      s"Token ($amountBaseTokenRequired) delta does not correspond to manually calculated value "
    )

    val fundingBox = outBoxObj
      .genericContractBox(
        compiler.compileDummyContract(),
        tokenFundingBoxValue,
        Seq(
          ErgoToken(hodlBox.getTokens.get(2).getId, amountBaseTokenRequired)
        )
      )
      .convertToInputWith(fakeTxId1, fakeIndex)

    val hodlOutBox = outBoxObj.hodlBankBox(
      phoenixTokenContract,
      hodlBankSingleton,
      new ErgoToken(
        hodlTokenId,
        hodlBox.getTokens
          .get(1)
          .getValue - hodlTokenMintAmount - 1 // <<-- this line has changed
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
      new ErgoToken(
        hodlTokenId,
        hodlTokenMintAmount + 1
      ), // <<-- this line has changed
      minAmount
    )

    val unsignedTransaction = txHelper.buildUnsignedTransaction(
      inputs = Array(hodlBox, fundingBox),
      outputs = Array(hodlOutBox, recipientBox)
    )

    the[Exception] thrownBy {
      txHelper.signTransaction(
        unsignedTransaction
      )
    } should have message "Script reduced to false"
  }

  "PhoenixTokenMintOperation" should "fail when a deficit of hodlToken is withdrawn from bank" in {

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

    val price = hodlTokenPrice(hodlBox)

    require(
      hodlBox.getTokens
        .get(2)
        .getValue >= baseTokenTotalSupply - baseTokenTotalSupply - 1L,
      "never-decreasing theorem does not hold"
    )
    require(
      price == 1000,
      "Price does not correspond to manually calculated value"
    )

    val amountBaseTokenRequired = mintAmountToken(hodlBox, hodlTokenMintAmount)
    require(
      amountBaseTokenRequired == 20000,
      s"Token ($amountBaseTokenRequired) delta does not correspond to manually calculated value "
    )

    val fundingBox = outBoxObj
      .genericContractBox(
        compiler.compileDummyContract(),
        tokenFundingBoxValue,
        Seq(
          ErgoToken(hodlBox.getTokens.get(2).getId, amountBaseTokenRequired)
        )
      )
      .convertToInputWith(fakeTxId1, fakeIndex)

    val hodlOutBox = outBoxObj.hodlBankBox(
      phoenixTokenContract,
      hodlBankSingleton,
      new ErgoToken(
        hodlTokenId,
        hodlBox.getTokens
          .get(1)
          .getValue - (hodlTokenMintAmount + 1) // <<-- this line has changed
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
          hodlBox.getTokens
            .get(2)
            .getValue + amountBaseTokenRequired
        )
      )
    )

    val recipientBox = outBoxObj.hodlMintBox(
      userAddress,
      new ErgoToken(
        hodlTokenId,
        (hodlTokenMintAmount - 1) // <<-- this line has changed
      ),
      minAmount
    )

    val unsignedTransaction = txHelper.buildUnsignedTransaction(
      inputs = Array(hodlBox, fundingBox),
      outputs = Array(hodlOutBox, recipientBox),
      tokensToBurn =
        Array(new ErgoToken(hodlTokenId, 2)) // <<-- this line has changed
    )

    the[Exception] thrownBy {
      txHelper.signTransaction(
        unsignedTransaction
      )
    } should have message "Script reduced to false"
  }

  "PhoenixTokenMintOperation" should "fail if registers are changed" in {

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

    val price = hodlTokenPrice(hodlBox)

    require(
      hodlBox.getTokens
        .get(2)
        .getValue >= baseTokenTotalSupply - baseTokenTotalSupply - 1L,
      "never-decreasing theorem does not hold"
    )
    require(
      price == 1000,
      "Price does not correspond to manually calculated value"
    )

    val amountBaseTokenRequired = mintAmountToken(hodlBox, hodlTokenMintAmount)
    require(
      amountBaseTokenRequired == 20000,
      s"Token ($amountBaseTokenRequired) delta does not correspond to manually calculated value "
    )

    val fundingBox = outBoxObj
      .genericContractBox(
        compiler.compileDummyContract(),
        tokenFundingBoxValue,
        Seq(
          new ErgoToken(hodlBox.getTokens.get(2).getId, amountBaseTokenRequired)
        )
      )
      .convertToInputWith(fakeTxId1, fakeIndex)

    val hodlOutBox = outBoxObj.hodlBankBox(
      phoenixTokenContract,
      hodlBankSingleton,
      new ErgoToken(
        hodlTokenId,
        hodlBox.getTokens.get(1).getValue - hodlTokenMintAmount
      ),
      baseTokenTotalSupply - 1, // <-- this line is changed
      precisionFactor + 10, // <-- this line is changed
      minBankValue + 2, // <-- this line is changed
      bankFee - 1, // <-- this line is changed
      devFee + 5, // <-- this line is changed
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
      new ErgoToken(hodlTokenId, hodlTokenMintAmount),
      minAmount
    )

    val unsignedTransaction = txHelper.buildUnsignedTransaction(
      inputs = Array(hodlBox, fundingBox),
      outputs = Array(hodlOutBox, recipientBox)
    )

    the[Exception] thrownBy {
      txHelper.signTransaction(unsignedTransaction)
    } should have message "Script reduced to false"
  }

  "PhoenixTokenBurnOperation" should "succeed when one unit of token is burned" in {

    val hodlTokenAmount = 40000000 * 1000000000L

    val hodlTokenBurnAmount = 1

    val hodlTokens =
      new ErgoToken(hodlTokenId, hodlTokenAmount)
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

    val fundingBox = outBoxObj
      .tokenOutBox(
        Array(ErgoToken(hodlBox.getTokens.get(1).getId, hodlTokenBurnAmount)),
        compiler.compileDummyContract().toAddress,
        tokenFundingBoxValue
      )
      .convertToInputWith(fakeTxId1, fakeIndex)

    val hodlOutBox = outBoxObj.hodlBankBox(
      phoenixTokenContract,
      hodlBankSingleton,
      new ErgoToken(
        hodlTokenId,
        hodlBox.getTokens.get(1).getValue + fundingBox.getTokens.get(0).getValue
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
      inputs = Array(hodlBox, fundingBox),
      outputs =
        if (userBoxAmount > 0 && devFeeAmount > 0)
          Array(hodlOutBox, recipientBox, devFeeBox)
        else if (userBoxAmount > 0 && devFeeAmount == 0)
          Array(hodlOutBox, recipientBox)
        else Array(hodlOutBox)
    )

//    require(
//      hodlOutBox.getTokens
//        .get(2)
//        .getValue == hodlBox.getTokens.get(2).getValue - 1940
//    )

    noException shouldBe thrownBy {
      txHelper.signTransaction(
        unsignedTransaction
      )
    }

  }

  "PhoenixTokenBurnOperation" should "succeed with two units of tokens" in {

    val hodlTokenAmount = 40000000 * 1000000000L
    val startingTVLAmount: Long = 10000000 * 1000000000L

    val hodlTokenBurnAmount = 2

    val hodlTokens =
      new ErgoToken(hodlTokenId, hodlTokenAmount)
    val baseTokens = new ErgoToken(baseTokenId, startingTVLAmount)

    val hodlBox = outBoxObj
      .hodlBankBox(
        phoenixTokenContract,
        hodlBankSingleton,
        hodlTokens,
        totalSupply,
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

    val fundingBox = outBoxObj
      .tokenOutBox(
        Array(ErgoToken(hodlBox.getTokens.get(1).getId, hodlTokenBurnAmount)),
        compiler.compileDummyContract().toAddress,
        tokenFundingBoxValue
      )
      .convertToInputWith(fakeTxId1, fakeIndex)

    val hodlOutBox = outBoxObj.hodlBankBox(
      phoenixTokenContract,
      hodlBankSingleton,
      new ErgoToken(
        hodlTokenId,
        hodlBox.getTokens.get(1).getValue + fundingBox.getTokens.get(0).getValue
      ),
      totalSupply,
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
      inputs = Array(hodlBox, fundingBox),
      outputs =
        if (devFeeAmount > 0) Array(hodlOutBox, recipientBox, devFeeBox)
        else Array(hodlOutBox, recipientBox)
    )

    //    require(
    //      hodlOutBox.getTokens
    //        .get(2)
    //        .getValue == hodlBox.getTokens.get(2).getValue - 1940
    //    )

    noException shouldBe thrownBy {
      txHelper.signTransaction(
        unsignedTransaction
      )
    }

  }

  "PhoenixTokenBurnOperation" should "fail when registers are changed" in {

    val hodlTokenAmount = 40000000 * 1000000000L
    val startingTVLAmount: Long = 10000000 * 1000000000L

    val hodlTokenBurnAmount = 2000

    val hodlTokens =
      new ErgoToken(hodlTokenId, hodlTokenAmount)
    val baseTokens = new ErgoToken(baseTokenId, startingTVLAmount)

    val hodlBox = outBoxObj
      .hodlBankBox(
        phoenixTokenContract,
        hodlBankSingleton,
        hodlTokens,
        totalSupply - 1, // <-- this line is changed
        precisionFactor + 10, // <-- this line is changed
        minBankValue + 2, // <-- this line is changed
        bankFee - 1, // <-- this line is changed
        devFee + 5, // <-- this line is changed
        bankERGAmount,
        Some(baseTokens)
      )
      .convertToInputWith(fakeTxId1, fakeIndex)

    val (userBoxAmount, devFeeAmount, bankFeeAmount) =
      burnTokenAmount(hodlBox, hodlTokenBurnAmount)

    val fundingBox = outBoxObj
      .tokenOutBox(
        Array(ErgoToken(hodlBox.getTokens.get(1).getId, hodlTokenBurnAmount)),
        compiler.compileDummyContract().toAddress,
        tokenFundingBoxValue + 1000000L // <-- this line is changed (allows for change box)
      )
      .convertToInputWith(fakeTxId1, fakeIndex)

    val hodlOutBox = outBoxObj.hodlBankBox(
      phoenixTokenContract,
      hodlBankSingleton,
      new ErgoToken(
        hodlTokenId,
        hodlBox.getTokens.get(1).getValue + fundingBox.getTokens.get(0).getValue
      ),
      totalSupply,
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
      inputs = Array(hodlBox, fundingBox),
      outputs = Array(hodlOutBox, recipientBox, devFeeBox)
    )

//    require(
//      hodlOutBox.getTokens
//        .get(2)
//        .getValue == hodlBox.getTokens.get(2).getValue - 1940
//    )
//    commented out because burn information is extracted from registers, if they change so do burn values

    the[Exception] thrownBy {
      txHelper.signTransaction(unsignedTransaction)
    } should have message "Script reduced to false"

  }

  "PhoenixTokenBurnOperation" should "fail when user's box takes more tokens than allowed" in {

    val hodlTokenAmount = 40000000 * 1000000000L
    val startingTVLAmount: Long = 10000000 * 1000000000L

    val hodlTokenBurnAmount = 2000

    val hodlTokens =
      new ErgoToken(hodlTokenId, hodlTokenAmount)
    val baseTokens = new ErgoToken(baseTokenId, startingTVLAmount)

    val hodlBox = outBoxObj
      .hodlBankBox(
        phoenixTokenContract,
        hodlBankSingleton,
        hodlTokens,
        totalSupply,
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

    val fundingBox = outBoxObj
      .tokenOutBox(
        Array(ErgoToken(hodlBox.getTokens.get(1).getId, hodlTokenBurnAmount)),
        compiler.compileDummyContract().toAddress,
        tokenFundingBoxValue
      )
      .convertToInputWith(fakeTxId1, fakeIndex)

    val hodlOutBox = outBoxObj.hodlBankBox(
      phoenixTokenContract,
      hodlBankSingleton,
      new ErgoToken(
        hodlTokenId,
        hodlBox.getTokens.get(1).getValue + fundingBox.getTokens.get(0).getValue
      ),
      totalSupply,
      precisionFactor,
      minBankValue,
      bankFee,
      devFee,
      minAmount,
      Some(
        ErgoToken(
          hodlBox.getTokens.get(2).getId,
          hodlBox.getTokens
            .get(2)
            .getValue - userBoxAmount - devFeeAmount - 1L // <-- this line changed
        )
      )
    )

    val recipientBox = outBoxObj.tokenOutBox(
      Array(
        ErgoToken(
          hodlBox.getTokens.get(2).getId,
          userBoxAmount + 1L // <-- this line changed
        )
      ),
      userAddress
    )

    val devFeeBox =
      outBoxObj.tokenOutBox(
        Array(new ErgoToken(hodlBox.getTokens.get(2).getId, devFeeAmount)),
        defaultFeeTokenContract.toAddress
      )

    val unsignedTransaction = txHelper.buildUnsignedTransaction(
      inputs = Array(hodlBox, fundingBox),
      outputs = Array(hodlOutBox, recipientBox, devFeeBox)
    )

    the[Exception] thrownBy {
      txHelper.signTransaction(unsignedTransaction)
    } should have message "Script reduced to false"

  }

  "PhoenixTokenBurnOperation" should "fail when developer's box takes more tokens than allowed" in {

    val hodlTokenAmount = 40000000 * 1000000000L
    val startingTVLAmount: Long = 10000000 * 1000000000L

    val hodlTokenBurnAmount = 2000

    val hodlTokens =
      new ErgoToken(hodlTokenId, hodlTokenAmount)
    val baseTokens = new ErgoToken(baseTokenId, startingTVLAmount)

    val hodlBox = outBoxObj
      .hodlBankBox(
        phoenixTokenContract,
        hodlBankSingleton,
        hodlTokens,
        totalSupply,
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

    val fundingBox = outBoxObj
      .tokenOutBox(
        Array(ErgoToken(hodlBox.getTokens.get(1).getId, hodlTokenBurnAmount)),
        compiler.compileDummyContract().toAddress,
        tokenFundingBoxValue
      )
      .convertToInputWith(fakeTxId1, fakeIndex)

    val hodlOutBox = outBoxObj.hodlBankBox(
      phoenixTokenContract,
      hodlBankSingleton,
      new ErgoToken(
        hodlTokenId,
        hodlBox.getTokens.get(1).getValue + fundingBox.getTokens.get(0).getValue
      ),
      totalSupply,
      precisionFactor,
      minBankValue,
      bankFee,
      devFee,
      minAmount,
      Some(
        ErgoToken(
          hodlBox.getTokens.get(2).getId,
          hodlBox.getTokens.get(2).getValue - userBoxAmount - devFeeAmount
        )
      )
    )

    val recipientBox = outBoxObj.tokenOutBox(
      Array(
        ErgoToken(
          hodlBox.getTokens.get(2).getId,
          userBoxAmount - 1L
        ) // <-- this line changed
      ),
      userAddress
    )

    val devFeeBox =
      outBoxObj.tokenOutBox(
        Array(
          ErgoToken(
            hodlBox.getTokens.get(2).getId,
            devFeeAmount + 1L
          ) // <-- this line changed
        ),
        defaultFeeTokenContract.toAddress
      )

    val unsignedTransaction = txHelper.buildUnsignedTransaction(
      inputs = Array(hodlBox, fundingBox),
      outputs = Array(hodlOutBox, recipientBox, devFeeBox)
    )

    the[Exception] thrownBy {
      txHelper.signTransaction(unsignedTransaction)
    } should have message "Script reduced to false"

  }

  "PhoenixTokenBurnOperation" should "fail when developer's box has an incorrect script" in {

    val hodlTokenAmount = 40000000 * 1000000000L
    val startingTVLAmount: Long = 10000000 * 1000000000L

    val hodlTokenBurnAmount = 2000

    val hodlTokens =
      new ErgoToken(hodlTokenId, hodlTokenAmount)
    val baseTokens = new ErgoToken(baseTokenId, startingTVLAmount)

    val hodlBox = outBoxObj
      .hodlBankBox(
        phoenixTokenContract,
        hodlBankSingleton,
        hodlTokens,
        totalSupply,
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

    val fundingBox = outBoxObj
      .tokenOutBox(
        Array(ErgoToken(hodlBox.getTokens.get(1).getId, hodlTokenBurnAmount)),
        compiler.compileDummyContract().toAddress,
        tokenFundingBoxValue
      )
      .convertToInputWith(fakeTxId1, fakeIndex)

    val hodlOutBox = outBoxObj.hodlBankBox(
      phoenixTokenContract,
      hodlBankSingleton,
      new ErgoToken(
        hodlTokenId,
        hodlBox.getTokens.get(1).getValue + fundingBox.getTokens.get(0).getValue
      ),
      totalSupply,
      precisionFactor,
      minBankValue,
      bankFee,
      devFee,
      minAmount,
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
        compiler.compileDummyContract().toAddress // <-- this line changed
      )

    val unsignedTransaction = txHelper.buildUnsignedTransaction(
      inputs = Array(hodlBox, fundingBox),
      outputs = Array(hodlOutBox, recipientBox, devFeeBox)
    )

    the[Exception] thrownBy {
      txHelper.signTransaction(unsignedTransaction)
    } should have message "Script reduced to false"

  }

}

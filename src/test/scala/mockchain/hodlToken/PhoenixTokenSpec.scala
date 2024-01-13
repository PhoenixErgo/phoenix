package mockchain.token

import mockClient.Common
import mockchain.PhoenixCommon
import org.ergoplatform.appkit.impl.ErgoTreeContract
import org.ergoplatform.appkit.{Address, Parameters}
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

    // note the max hodlToken in bank must be one less than baseTokenTotalSupply
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

    assert(
      hodlBox.getTokens.get(1).getValue < extractBaseTokenSupply(hodlBox),
      "hodlToken amount must be less than baseToken total supply"
    )

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

    val hodlTokens =
      new ErgoToken(
        hodlTokenId,
        baseTokenTotalSupply - 2000000L
      ) // note same value is subtracted as token amount below
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

    assert(
      hodlBox.getTokens.get(1).getValue < extractBaseTokenSupply(hodlBox),
      "hodlToken amount must be less than baseToken total supply"
    )

    val price = hodlTokenPrice(hodlBox)

    assert(price == 1, "price must be 1")

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

    // same as first test but with one additional recipient

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

    assert(
      hodlBox.getTokens.get(1).getValue < extractBaseTokenSupply(hodlBox),
      "hodlToken amount must be less than baseToken total supply"
    )

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
        tokenFundingBoxValue + 1000000L, // extra ERG required for additional box made
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
      new ErgoToken(
        hodlTokenId,
        10
      ), // manually specified how much hodlToken to receive
      minAmount
    )

    val recipientBox2 = outBoxObj.hodlMintBox(
      Address.create(
        "9ehbiMqN88ctdANajxz8UB2G2MHYcBgbrTFEbgCFKPWrmNnSfjR"
      ), // random address
      new ErgoToken(
        hodlTokenId,
        10
      ), // manually specified how much hodlToken to receive
      minAmount
    )

    val unsignedTransaction = txHelper.buildUnsignedTransaction(
      inputs = Array(hodlBox, fundingBox),
      outputs = Array(
        hodlOutBox,
        recipientBox,
        recipientBox2
      ) // extra box added to outputs
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

    assert(
      hodlBox.getTokens.get(1).getValue < extractBaseTokenSupply(hodlBox),
      "hodlToken amount must be less than baseToken total supply"
    )

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
          .getValue - hodlTokenMintAmount - 1 // <<-- this line has changed, extra token taken
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
      ), // <<-- this line has changed, extra token given
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

    assert(
      hodlBox.getTokens.get(1).getValue < extractBaseTokenSupply(hodlBox),
      "hodlToken amount must be less than baseToken total supply"
    )

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
          .getValue - (hodlTokenMintAmount + 1) // <<-- this line has changed, extra hodlToken added
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
        (hodlTokenMintAmount - 1) // <<-- this line has changed, hodlToken removed
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

    assert(
      hodlBox.getTokens.get(1).getValue < extractBaseTokenSupply(hodlBox),
      "hodlToken amount must be less than baseToken total supply"
    )

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

    val hodlTokenBurnAmount = 1

    val hodlTokens =
      new ErgoToken(
        hodlTokenId,
        baseTokenTotalSupply - 2000000L
      ) // note same value is subtracted as token amount below
    val baseTokens = new ErgoToken(baseTokenId, 2000000L)

    // above values of hodlToken and baseTokens allows for price to be 1:1

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

    assert(
      hodlBox.getTokens.get(1).getValue < extractBaseTokenSupply(hodlBox),
      "hodlToken amount must be less than baseToken total supply"
    )

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
    assert(
      userBoxAmount == 0L,
      "user should not receive token when only one unit is burned"
    )
    assert(
      devFeeAmount == 0L,
      "dev should not receive token when only one unit is burned"
    )
    assert(
      bankFeeAmount == 1L,
      "bank must receive token when only one unit is burned"
    )

    noException shouldBe thrownBy {
      txHelper.signTransaction(
        unsignedTransaction
      )
    }

  }

  "PhoenixTokenBurnOperation" should "succeed with two units of tokens" in {

    val hodlTokenBurnAmount = 2

    val hodlTokens =
      new ErgoToken(
        hodlTokenId,
        baseTokenTotalSupply - 2000000L
      ) // note same value is subtracted as token amount below
    val baseTokens = new ErgoToken(baseTokenId, 2000000L)

    // above values of hodlToken and baseTokens allows for price to be 1:1

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

    assert(
      hodlBox.getTokens.get(1).getValue < extractBaseTokenSupply(hodlBox),
      "hodlToken amount must be less than baseToken total supply"
    )

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
        if (devFeeAmount > 0) Array(hodlOutBox, recipientBox, devFeeBox)
        else Array(hodlOutBox, recipientBox)
    )

    assert(
      userBoxAmount == 1L,
      "user must receive tokens when two units are burned"
    )
    assert(
      devFeeAmount == 0L,
      "dev should not receive tokens when two units are burned"
    )
    assert(
      bankFeeAmount == 1L,
      "bank must receive tokens when two units are burned"
    )

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
        totalSupply,
        precisionFactor,
        minBankValue,
        bankFee,
        devFee,
        bankERGAmount,
        Some(baseTokens)
      )
      .convertToInputWith(fakeTxId1, fakeIndex)

    assert(
      hodlBox.getTokens.get(1).getValue < extractBaseTokenSupply(hodlBox),
      "hodlToken amount must be less than baseToken total supply"
    )

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
      totalSupply - 1, // <-- this line is changed
      precisionFactor + 10, // <-- this line is changed
      minBankValue + 2, // <-- this line is changed
      bankFee - 1, // <-- this line is changed
      devFee + 5, // <-- this line is changed
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

    assert(
      userBoxAmount == 1934L,
      "must correspond to manually calculated values"
    )
    assert(
      devFeeAmount == 6L,
      "must correspond to manually calculated values"
    )
    assert(
      bankFeeAmount == 60L,
      "must correspond to manually calculated values"
    )

    the[Exception] thrownBy {
      txHelper.signTransaction(unsignedTransaction)
    } should have message "Script reduced to false"

  }

  "PhoenixTokenBurnOperation" should "fail when user's box takes more tokens than allowed" in {

    val hodlTokenAmount = 97537237623541120L
    val startingTVLAmount: Long = 406609260219332L
    val baseTokenTotalSupply = 97739924000000000L
    // different values used to make price = 2

    val hodlTokenBurnAmount = 2000

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

    assert(
      hodlBox.getTokens.get(1).getValue < extractBaseTokenSupply(hodlBox),
      "hodlToken amount must be less than baseToken total supply"
    )

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

    assert(
      userBoxAmount == 3868L,
      "must correspond to manually calculated values"
    )
    assert(
      devFeeAmount == 12L,
      "must correspond to manually calculated values"
    )
    assert(
      bankFeeAmount == 120L,
      "must correspond to manually calculated values"
    )

    the[Exception] thrownBy {
      txHelper.signTransaction(unsignedTransaction)
    } should have message "Script reduced to false"

  }

  "PhoenixTokenBurnOperation" should "fail when developer's box takes more tokens than allowed" in {

    val hodlTokenAmount = 97537237623541120L
    val startingTVLAmount: Long = 406609260219332L
    val baseTokenTotalSupply = 97739924000000000L
    // different values used to make price = 2

    val hodlTokenBurnAmount = 2000

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

    assert(
      hodlBox.getTokens.get(1).getValue < extractBaseTokenSupply(hodlBox),
      "hodlToken amount must be less than baseToken total supply"
    )

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

    assert(
      userBoxAmount == 3868L,
      "must correspond to manually calculated values"
    )
    assert(
      devFeeAmount == 12L,
      "must correspond to manually calculated values"
    )
    assert(
      bankFeeAmount == 120L,
      "must correspond to manually calculated values"
    )

    the[Exception] thrownBy {
      txHelper.signTransaction(unsignedTransaction)
    } should have message "Script reduced to false"

  }

  "PhoenixTokenBurnOperation" should "fail when developer's box has an incorrect script" in {

    val hodlTokenAmount = 97537237623541120L
    val startingTVLAmount: Long = 406609260219332L
    val baseTokenTotalSupply = 97739924000000000L
    // different values used to make price = 2

    val hodlTokenBurnAmount = 2000

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

    assert(
      hodlBox.getTokens.get(1).getValue < extractBaseTokenSupply(hodlBox),
      "hodlToken amount must be less than baseToken total supply"
    )

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

    assert(
      userBoxAmount == 3868L,
      "must correspond to manually calculated values"
    )
    assert(
      devFeeAmount == 12L,
      "must correspond to manually calculated values"
    )
    assert(
      bankFeeAmount == 120L,
      "must correspond to manually calculated values"
    )

    the[Exception] thrownBy {
      txHelper.signTransaction(unsignedTransaction)
    } should have message "Script reduced to false"

  }

  "PhoenixDepositOperation" should "work correctly when all conditions are satisfied" in {

    val hodlTokenMintAmount = 20
    val baseTokensToAdd = 20

    // note the max hodlToken in bank must be one less than baseTokenTotalSupply
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

    assert(
      hodlBox.getTokens.get(1).getValue < extractBaseTokenSupply(hodlBox),
      "hodlToken amount must be less than baseToken total supply"
    )

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
          ErgoToken(hodlBox.getTokens.get(2).getId, baseTokensToAdd)
        )
      )
      .convertToInputWith(fakeTxId1, fakeIndex)

    val myAddress = new ErgoTreeContract(
      txHelper.senderAddress.getErgoAddress.script,
      this.ctx.getNetworkType
    )

    val hodlOutBox = outBoxObj.hodlBankBox(
      phoenixTokenContract,
      hodlBankSingleton,
      hodlBox.getTokens.get(1),
      baseTokenTotalSupply,
      precisionFactor,
      minBankValue,
      bankFee,
      devFee,
      bankERGAmount,
      Some(
        ErgoToken(
          hodlBox.getTokens.get(2).getId,
          hodlBox.getTokens.get(2).getValue + baseTokensToAdd
        )
      )
    )

    val unsignedTransaction = txHelper.buildUnsignedTransaction(
      inputs = Array(hodlBox, fundingBox),
      outputs = Array(hodlOutBox)
    )

    noException shouldBe thrownBy {
      txHelper.signTransaction(
        unsignedTransaction
      )
    }
  }

  "PhoenixDepositOperation" should "fail when output bank box script is changed" in {

    val hodlTokenMintAmount = 20
    val baseTokensToAdd = 20

    // note the max hodlToken in bank must be one less than baseTokenTotalSupply
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

    assert(
      hodlBox.getTokens.get(1).getValue < extractBaseTokenSupply(hodlBox),
      "hodlToken amount must be less than baseToken total supply"
    )

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
          ErgoToken(hodlBox.getTokens.get(2).getId, baseTokensToAdd)
        )
      )
      .convertToInputWith(fakeTxId1, fakeIndex)

    val myAddress = new ErgoTreeContract(
      txHelper.senderAddress.getErgoAddress.script,
      this.ctx.getNetworkType
    )

    val hodlOutBox = outBoxObj.hodlBankBox(
      myAddress,
      hodlBankSingleton,
      hodlBox.getTokens.get(1),
      baseTokenTotalSupply,
      precisionFactor,
      minBankValue,
      bankFee,
      devFee,
      bankERGAmount,
      Some(
        ErgoToken(
          hodlBox.getTokens.get(2).getId,
          hodlBox.getTokens.get(2).getValue + baseTokensToAdd
        )
      )
    )

    val unsignedTransaction = txHelper.buildUnsignedTransaction(
      inputs = Array(hodlBox, fundingBox),
      outputs = Array(hodlOutBox)
    )

    the[Exception] thrownBy {
      txHelper.signTransaction(unsignedTransaction)
    } should have message "Script reduced to false"
  }

  "PhoenixDepositOperation" should "fail when output bank box registers are changed" in {

    val hodlTokenMintAmount = 20
    val baseTokensToAdd = 20

    // note the max hodlToken in bank must be one less than baseTokenTotalSupply
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

    assert(
      hodlBox.getTokens.get(1).getValue < extractBaseTokenSupply(hodlBox),
      "hodlToken amount must be less than baseToken total supply"
    )

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
          ErgoToken(hodlBox.getTokens.get(2).getId, baseTokensToAdd)
        )
      )
      .convertToInputWith(fakeTxId1, fakeIndex)

    val hodlOutBox = outBoxObj.hodlBankBox(
      phoenixTokenContract,
      hodlBankSingleton,
      hodlBox.getTokens.get(1),
      baseTokenTotalSupply - 1, // <-- this line is changed
      precisionFactor + 10, // <-- this line is changed
      minBankValue + 2, // <-- this line is changed
      bankFee - 1, // <-- this line is changed
      devFee + 5, // <-- this line is changed
      bankERGAmount,
      Some(
        ErgoToken(
          hodlBox.getTokens.get(2).getId,
          hodlBox.getTokens.get(2).getValue + baseTokensToAdd
        )
      )
    )

    val unsignedTransaction = txHelper.buildUnsignedTransaction(
      inputs = Array(hodlBox, fundingBox),
      outputs = Array(hodlOutBox)
    )

    the[Exception] thrownBy {
      txHelper.signTransaction(unsignedTransaction)
    } should have message "Script reduced to false"
  }

  "PhoenixDepositOperation" should "fail when output bank box baseToken amount decreases" in {

    val hodlTokenMintAmount = 20
    val baseTokensToAdd = 20

    // note the max hodlToken in bank must be one less than baseTokenTotalSupply
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

    assert(
      hodlBox.getTokens.get(1).getValue < extractBaseTokenSupply(hodlBox),
      "hodlToken amount must be less than baseToken total supply"
    )

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
        minBoxValue
      )
      .convertToInputWith(fakeTxId1, fakeIndex)

    val hodlOutBox = outBoxObj.hodlBankBox(
      phoenixTokenContract,
      hodlBankSingleton,
      hodlBox.getTokens.get(1),
      baseTokenTotalSupply,
      precisionFactor,
      minBankValue,
      bankFee,
      devFee,
      bankERGAmount,
      Some(
        ErgoToken(
          hodlBox.getTokens.get(2).getId,
          hodlBox.getTokens.get(2).getValue - 1L // <-- this changed
        )
      )
    )

    val unsignedTransaction = txHelper.buildUnsignedTransaction(
      inputs = Array(hodlBox, fundingBox),
      outputs = Array(hodlOutBox),
      tokensToBurn = Array(
        ErgoToken( // token is burned (can be also sent to someone)
          hodlBox.getTokens.get(2).getId,
          1L
        )
      )
    )

    the[Exception] thrownBy {
      txHelper.signTransaction(unsignedTransaction)
    } should have message "Script reduced to false"
  }

}

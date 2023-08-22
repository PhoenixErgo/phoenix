package mockchain.token

import mockClient.Common
import mockchain.PhoenixCommon
import org.ergoplatform.appkit.{OutBox, Parameters}
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

  val baseTokenTotalSupply: Long = totalSupply / 10 * 9
  val baseTokenId: String =
    "9a06d9e545a41fd51eeffc5e20d818073bf820c635e2a9d922269913e0de369d"

  val hodlBankSingleton = new ErgoToken(hodlBankNft, 1L)

  val startingTVLAmount: Long = 10000000 * 1000000000L

  "PhoenixTokenMintOperation" should "work correctly when all conditions are satisfied" in {

    val hodlTokenMintAmount = 20

    val hodlTokens = new ErgoToken(hodlTokenId, baseTokenTotalSupply)
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

    val price = hodlTokenPrice(hodlBox)

    require(
      hodlBox.getTokens
        .get(2)
        .getValue >= baseTokenTotalSupply - baseTokenTotalSupply - 1L,
      "never-decreasing theorem does not hold"
    )
    require(
      price == 2000000,
      "Price does not correspond to manually calculated value"
    )

    val amountBaseTokenRequired = mintAmountToken(hodlBox, hodlTokenMintAmount)
    require(
      amountBaseTokenRequired == 40,
      s"Token ($amountBaseTokenRequired) delta does not correspond to manually calculated value "
    )

    val fundingBox = outBoxObj
      .genericContractBox(
        compiler.compileDummyContract(),
        fundingBoxValue,
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
      totalSupply,
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

  "PhoenixTokenMintOperation" should "fail when excess hodlToken is withdrawn from bank" in {

    val hodlTokenMintAmount = 20

    val hodlTokens = new ErgoToken(hodlTokenId, baseTokenTotalSupply)
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

    val price = hodlTokenPrice(hodlBox)

    require(
      hodlBox.getTokens
        .get(2)
        .getValue >= baseTokenTotalSupply - baseTokenTotalSupply - 1L,
      "never-decreasing theorem does not hold"
    )
    require(
      price == 2000000,
      "Price does not correspond to manually calculated value"
    )

    val amountBaseTokenRequired = mintAmountToken(hodlBox, hodlTokenMintAmount)
    require(
      amountBaseTokenRequired == 40,
      s"Token ($amountBaseTokenRequired) delta does not correspond to manually calculated value "
    )

    val fundingBox = outBoxObj
      .genericContractBox(
        compiler.compileDummyContract(),
        fundingBoxValue,
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
        hodlBox.getTokens.get(1).getValue - hodlTokenMintAmount - 1
      ), // <<-- this line has changed
      totalSupply,
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

    the[Exception] thrownBy {
      txHelper.signTransaction(
        unsignedTransaction
      )
    } should have message "Script reduced to false"
  }

  "PhoenixTokenMintOperation" should "fail when there is a deficit of hodlToken remaining in bank" in {

    val hodlTokenMintAmount = 20

    val hodlTokens = new ErgoToken(hodlTokenId, baseTokenTotalSupply)
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

    val price = hodlTokenPrice(hodlBox)

    require(
      hodlBox.getTokens
        .get(2)
        .getValue >= baseTokenTotalSupply - baseTokenTotalSupply - 1L,
      "never-decreasing theorem does not hold"
    )
    require(
      price == 2000000,
      "Price does not correspond to manually calculated value"
    )

    val amountBaseTokenRequired = mintAmountToken(hodlBox, hodlTokenMintAmount)
    require(
      amountBaseTokenRequired == 40,
      s"Token ($amountBaseTokenRequired) delta does not correspond to manually calculated value "
    )

    val fundingBox = outBoxObj
      .genericContractBox(
        compiler.compileDummyContract(),
        fundingBoxValue,
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
      totalSupply,
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
            .getValue + amountBaseTokenRequired - 1L // <-- this line is changed
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
      txHelper.signTransaction(
        unsignedTransaction
      )
    } should have message "Script reduced to false"
  }

  "PhoenixTokenMintOperation" should "fail if registers are changed" in {

    val hodlTokenMintAmount = 20

    val hodlTokens = new ErgoToken(hodlTokenId, baseTokenTotalSupply)
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

    val price = hodlTokenPrice(hodlBox)

    require(
      hodlBox.getTokens
        .get(2)
        .getValue >= baseTokenTotalSupply - baseTokenTotalSupply - 1L,
      "never-decreasing theorem does not hold"
    )
    require(
      price == 2000000,
      "Price does not correspond to manually calculated value"
    )

    val amountBaseTokenRequired = mintAmountToken(hodlBox, hodlTokenMintAmount)
    require(
      amountBaseTokenRequired == 40,
      s"Token ($amountBaseTokenRequired) delta does not correspond to manually calculated value "
    )

    val fundingBox = outBoxObj
      .genericContractBox(
        compiler.compileDummyContract(),
        fundingBoxValue,
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
      totalSupply - 1,
      precisionFactor + 10,
      minBankValue + 2,
      bankFee - 1,
      devFee + 5,
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

  "PhoenixTokenBurnOperation" should "succeed when all conditions are met" in {

    val hodlTokenAmount = 40000000 * 1000000000L

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
        fundingBoxValue
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
        Array(new ErgoToken(hodlBox.getTokens.get(2).getId, devFeeAmount)),
        defaultFeeTokenContract.toAddress
      )

    val unsignedTransaction = txHelper.buildUnsignedTransaction(
      inputs = Array(hodlBox, fundingBox),
      outputs = Array(hodlOutBox, recipientBox, devFeeBox)
    )

    require(
      hodlOutBox.getTokens
        .get(2)
        .getValue == hodlBox.getTokens.get(2).getValue - 1940
    )

    noException shouldBe thrownBy {
      txHelper.signTransaction(
        unsignedTransaction
      )
    }

  }

  "PhoenixTokenBurnOperation" should "fail when user's box takes more Erg than allowed" in {

    val hodlTokenAmount = 40000000 * 1000000000L

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
        fundingBoxValue
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
        ErgoToken(hodlBox.getTokens.get(2).getId, userBoxAmount + 1L)
      ), // <-- this line changed
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

  "PhoenixTokenBurnOperation" should "fail when developer's box takes more Erg than allowed" in {

    val hodlTokenAmount = 40000000 * 1000000000L

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
        fundingBoxValue
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
        ErgoToken(hodlBox.getTokens.get(2).getId, userBoxAmount - 1L)
      ), // <-- this line changed
      userAddress
    )

    val devFeeBox =
      outBoxObj.tokenOutBox(
        Array(
          ErgoToken(hodlBox.getTokens.get(2).getId, devFeeAmount + 1L)
        ), // <-- this line changed
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
        fundingBoxValue
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

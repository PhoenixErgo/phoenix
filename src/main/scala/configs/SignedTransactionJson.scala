package configs

import java.util


case class Extension(
    Byte: String
)

case class SpendingProof(proofBytes: String, extension: java.util.Map[String, String])

case class Input(boxId: String, spendingProof: SpendingProof)

case class DataInput(boxId: String)

case class SignedAsset(tokenId: String, amount: String)

case class AdditionalRegistersJson(
    R4: String,
    R5: String,
    R6: String,
    R7: String,
    R8: String,
    R9: String
)

case class Output(
    boxId: String,
    var value: String,
    var ergoTree: String,
    creationHeight: Int,
    index: Int,
    transactionId: String,
    assets: Array[SignedAsset],
    additionalRegisters: AdditionalRegistersJson
)

case class SignedTransactionJson(
    id: String,
    inputs: Array[Input],
    dataInputs: Array[DataInput],
    outputs: Array[Output]
)

package configs

case class UnsignedInput(boxId: String)

case class UnsignedTransactionJson(
    id: String,
    inputs: Array[UnsignedInput],
    dataInputs: Array[DataInput],
    outputs: Array[Output]
)

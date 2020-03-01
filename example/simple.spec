type Main = Object @obj {
    intField: Int,
    intField2: Int [ . = obj.intField ],
    numberField: Number,
    stringField: String,
    booleanField: Bool,
    nullField: Null,
    arrayField: Array {
        length: Int,
        get(i): Int [ . = i ]
    }
}
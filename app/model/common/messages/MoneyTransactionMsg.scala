package model.common.messages

case class MoneyTransactionMsg(transType: String = "Cashier",
                               MessageType: String,
                               playerIp: String,
                               rake: Double = 0.0,
                               roundId: Long = 999,
                               amount: Double = 0,
                               oldBalance: Double = 0,
                               newBalance: Double = 0,
                               timestamp: String)

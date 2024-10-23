package model.common.messages

case class MoneyTransaction(transType: String = "Undefined",
                            admin: String,
                            playerIp: String,
                            uid: String = "-1",
                            amount: Double)

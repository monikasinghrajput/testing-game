package model.common.messages


case class Player(clientIp: String = "",
                  clientId: String = "",
                  sessionId: Int = 0,
                  currentPlayerToken: String = "",
                  nickname: String ="Guest",
                  operatorId: Int = 999,
                  currency: String = "INR",
                  uid: String = "-1",
                  vip: Int = 0,
                  status: String = "offline",
                  usage: String = "unlocked",
                  balance: Double = 0) {
}


case class PlayerUpdatedMsg(MessageType: String, player: Player, timestamp: String)
case class PlayerCreatedMsg(MessageType: String, player: Player, timestamp: String)
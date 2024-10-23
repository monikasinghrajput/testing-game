package model.common.messages

case class OperationTransactionMsg(transType: String = "Admin",
                                   MessageType: String = "PLAYER_UPDATED",
                                   uid: String,
                                   client_ip: String,
                                   nickname: String ,
                                   status: String ,
                                   usage: String,
                                   timestamp: String)

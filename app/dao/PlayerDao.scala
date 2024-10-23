/*
* *********************************************************************************************************************************************************************
* Copyright (c) 2017 Tykhe Gaming Private Limited - All Rights Reserved

Licensed under the Software License Agreement (the "License") of Tykhe Gaming Private Limited.
You may not use this file except in compliance with the License.
You may obtain a copy of the License at http://tykhegaming.github.io/LICENSE.txt.

NOTICE
ALL INFORMATION CONTAINED HEREIN IS, AND REMAINS THE PROPERTY OF TYKHE GAMING PRIVATE LIMITED.
THE INTELLECTUAL AND TECHNICAL CONCEPTS CONTAINED HEREIN ARE PROPRIETARY TO TYKHE GAMING PRIVATE LIMITED AND ARE PROTECTED BY TRADE SECRET OR COPYRIGHT LAW.
DISSEMINATION OF THIS INFORMATION OR REPRODUCTION OF THIS MATERIAL IS STRICTLY FORBIDDEN UNLESS PRIOR WRITTEN PERMISSION IS OBTAINED FROM TYKHE GAMING PRIVATE LIMITED.

* **********************************************************************************************************************************************************************
* Change History
* **********************************************************************************************************************************************************************
* |     Date      |     Name     |      Change     |      Details
* |  01/08/2022   | Wilson Sam   |     Created     |  First Milestone
* |  21/11/2023   | Wilson Sam   |     Version     |  Packaged For Demo
* **********************************************************************************************************************************************************************
* */
package dao

import model.common.messages.{MoneyTransactionCodecMsg, MoneyTransactionMsg, OperationTransactionCodecMsg, OperationTransactionMsg, Player, PlayerCodec, RoundTransactionCodecMsg, RoundTransactionMsg}
import play.api.Logger
import play.api.libs.json.{JsValue, Json}

import java.text.SimpleDateFormat
import java.util.Calendar


class PlayerDao extends PlayerCodec
  with MoneyTransactionCodecMsg
  with RoundTransactionCodecMsg
  with OperationTransactionCodecMsg {

  val log = Logger(this.getClass)
  val dateFormat =  new SimpleDateFormat("E, dd MMM yyyy HH:mm:ss.SSS z")
  
  val fileNamePlayers: os.Path = os.home / "holdem" /  "users.json"
  val fileNameOperations: os.Path = os.home / "holdem" /  "operations.json"
  val fileNameTransactions: os.Path = os.home / "holdem" /  "transactions.json"
  val fileNameRoundTransactions: os.Path = os.home / "holdem" / "roundTransactions.json"

  if (!os.exists(fileNamePlayers)) {
    os.write.over(
      fileNamePlayers,
      Json.prettyPrint(Json.toJson(Seq(
        Player(uid = "1", nickname = "Player 1", clientIp = "192.168.1.1", balance = 0),
        Player(uid = "2", nickname = "Player 2", clientIp = "192.168.1.2", balance = 0),
        Player(uid = "3", nickname = "Player 3", clientIp = "192.168.1.3", balance = 0),
        Player(uid = "4", nickname = "Player 4", clientIp = "192.168.1.4", balance = 0),
        Player(uid = "5", nickname = "Player 5", clientIp = "192.168.1.5", balance = 0),
        Player(uid = "6", nickname = "Player 6", clientIp = "192.168.1.6", balance = 0),
        Player(uid = "7", nickname = "Player 7", clientIp = "192.168.1.7", balance = 0),
        Player(uid = "8", nickname = "Player 8", clientIp = "192.168.1.8", balance = 0),
      ))),
      createFolders = true
    )
    log.logger.info(s"Backup Players Missing... Created Success..")
  }

  if (!os.exists(fileNameTransactions)) {
    os.write.over(
      fileNameTransactions,
      Json.prettyPrint(Json.toJson(Seq.empty[MoneyTransactionMsg]
      )),
      createFolders = true
    )
    log.logger.info(s"Backup Transactions Missing... Created Success..")
  }

  if (!os.exists(fileNameOperations)) {
    os.write.over(
      fileNameOperations,
      Json.prettyPrint(Json.toJson(Seq(
        OperationTransactionMsg(MessageType = "PLAYER_CREATED", uid = "1", nickname = "Player 1", client_ip = "192.168.1.1", status = "offline", usage = "unlocked", timestamp = dateFormat.format(Calendar.getInstance().getTime)),
        OperationTransactionMsg(MessageType = "PLAYER_CREATED", uid = "2", nickname = "Player 2", client_ip = "192.168.1.2", status = "offline", usage = "unlocked", timestamp = dateFormat.format(Calendar.getInstance().getTime)),
        OperationTransactionMsg(MessageType = "PLAYER_CREATED", uid = "3", nickname = "Player 3", client_ip = "192.168.1.3", status = "offline", usage = "unlocked", timestamp = dateFormat.format(Calendar.getInstance().getTime)),
        OperationTransactionMsg(MessageType = "PLAYER_CREATED", uid = "4", nickname = "Player 4", client_ip = "192.168.1.4", status = "offline", usage = "unlocked", timestamp = dateFormat.format(Calendar.getInstance().getTime)),
        OperationTransactionMsg(MessageType = "PLAYER_CREATED", uid = "5", nickname = "Player 5", client_ip = "192.168.1.5", status = "offline", usage = "unlocked", timestamp = dateFormat.format(Calendar.getInstance().getTime)),
        OperationTransactionMsg(MessageType = "PLAYER_CREATED", uid = "6", nickname = "Player 6", client_ip = "192.168.1.6", status = "offline", usage = "unlocked", timestamp = dateFormat.format(Calendar.getInstance().getTime)),
        OperationTransactionMsg(MessageType = "PLAYER_CREATED", uid = "7", nickname = "Player 7", client_ip = "192.168.1.7", status = "offline", usage = "unlocked", timestamp = dateFormat.format(Calendar.getInstance().getTime)),
        OperationTransactionMsg(MessageType = "PLAYER_CREATED", uid = "8", nickname = "Player 8", client_ip = "192.168.1.8", status = "offline", usage = "unlocked", timestamp = dateFormat.format(Calendar.getInstance().getTime)),
      ))),
      createFolders = true
    )
    log.logger.info(s"Backup Operations Missing... Created Success..")
  }

  //What if file removed ??
  if (!os.exists(fileNameRoundTransactions)) {
    os.write.over(
      fileNameRoundTransactions,
      Json.prettyPrint(Json.toJson(Seq.empty[RoundTransactionMsg])),
      createFolders = true
    )
    log.logger.info("Backup Round Transactions Missing... Created Success")
  }

  val playersString: String = os.read(fileNamePlayers);
  val playersJson: JsValue = Json.parse(playersString);
  var players: Seq[Player] = playersJson.as[Seq[Player]]

  val transactionsString: String = os.read(fileNameTransactions);
  val transactionsJson: JsValue = Json.parse(transactionsString);
  var transactions: Seq[MoneyTransactionMsg] = transactionsJson.as[Seq[MoneyTransactionMsg]]


  val operationsString: String = os.read(fileNameOperations);
  val operationsJson: JsValue = Json.parse(operationsString);
  var operations: Seq[OperationTransactionMsg] = operationsJson.as[Seq[OperationTransactionMsg]]

  //read file String -> JsValue -> Seq[RoundTransactionMsg]
  val roundTransactionsString: String = os.read(fileNameRoundTransactions)
  val roundTransactionsJson: JsValue = Json.parse(roundTransactionsString)
  var roundTransactions: Seq[RoundTransactionMsg] = roundTransactionsJson.as[Seq[RoundTransactionMsg]]

  def getPlayersData: Seq[Player] = players
  def setPlayers(): Unit = os.write.over(
    fileNamePlayers,
    Json.prettyPrint(Json.toJson(players)),
    createFolders = true
  )
  def getPlayerData(player: String): Player = {players.find(p => p.clientIp == player).getOrElse(Player(clientId = player, clientIp = player, balance = 500000))}
  def addPlayer(player: Player): Seq[Player] = {players = players.+:(player) ;
    os.write.over(
      fileNamePlayers,
      Json.prettyPrint(Json.toJson(players)),
      createFolders = true
    );
    players
  }
  def updatePlayer(player: Player, uid: String): Unit = {
    val foundIndex = players.indexWhere(p => p.uid == uid)
    if(foundIndex != -1) {
      players = players.updated(foundIndex, player)
    } else {
      log.logger.warn("Player not found in updatePlayer! Added a new Player")
      players = players.+:(player);
    }
    os.write.over(
      fileNamePlayers,
      Json.prettyPrint(Json.toJson(players)),
      createFolders = true
    )
  }

  def getTransactionsData(): Seq[MoneyTransactionMsg] = transactions
  def addTransaction(transaction: MoneyTransactionMsg): Unit = {transactions = transactions.+:(transaction)}

  def setTransactions(): Unit = {
    os.write.over(
      fileNameTransactions,
      Json.prettyPrint(Json.toJson(transactions)),
      createFolders = true
    )}

  def getOperationTransactionsData(): Seq[OperationTransactionMsg] = operations

  def addOperationTransaction(operation: OperationTransactionMsg): Unit = {
    operations = operations.+:(operation)
  }
  def setOperationTransactions(): Unit = {
    os.write.over(
      fileNameOperations,
      Json.prettyPrint(Json.toJson(operations)),
      createFolders = true
    )}

  def getRoundTransactionsData(): Seq[RoundTransactionMsg] = roundTransactions

  //add transaction to both RAM and DB
  def addRoundTransaction(roundTransaction: RoundTransactionMsg): Unit = {
    roundTransactions = roundTransactions.+:(roundTransaction) //prepend it
    os.write.over(
      fileNameRoundTransactions,
      Json.prettyPrint(Json.toJson(roundTransactions)),
      createFolders = true
    )
  }

}

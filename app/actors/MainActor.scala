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
package actors

import akka.actor.{Actor, ActorRef, Props}

import scala.collection.mutable.{Map => MMap}
import play.api.Logger
import play.api.libs.json.Json

import java.text.SimpleDateFormat
import java.util.Calendar
import scala.language.postfixOps
import services.{GameService}
import model.common.messages.{AdminClientData, GameTransaction, MoneyTransaction, MoneyTransactionCodecMsg, MoneyTransactionMsg, OperationTransactionCodecMsg, OperationTransactionMsg, Player, PlayerCodec, PlayerCreatedMsg, PlayerUpdatedMsg, RoundTransactionCodecMsg, RoundTransactionMsg, ServerLog}
import model.poker.data.{TableState}


object MainActor {

  val name = "main-actor"
  val path = s"/usr/$name"

  def props(gameService: GameService): Props = Props(new MainActor(gameService))

  case class PlayerMoneyTransaction(moneyTransaction: MoneyTransaction, actor: ActorRef, client: ActorRef, admins: MMap[String, AdminClientData])
  case class PlayerGameTransaction(gameTransaction: GameTransaction, admins: MMap[String, AdminClientData])
  case class PlayerRoundTransaction(roundTransactionRecord: RoundTransactionMsg, admins: MMap[String, AdminClientData])
  case class GuestPlayerConnected(player: Player)

  case class PlayerStatusOnline(uid: String, admins: MMap[String, AdminClientData])
  case class PlayerStatusOffline(uid: String, admins: MMap[String, AdminClientData])
  case class PlayerIpUpdate(uid: String, ip: String)

  case class TableStateUpdated(tableState: TableState)

  case class SeatBalanceUpdated(uid: String, balance: Double = 0)

}


final class MainActor(gameService: GameService) extends Actor
  with PlayerCodec
  with RoundTransactionCodecMsg
  with MoneyTransactionCodecMsg {

  import actors.LogManagerActor._


  val log: Logger = Logger(this.getClass)
  val logManagerActor: ActorRef = gameService.getLoggingActor
  val dateFormat = new SimpleDateFormat("E, dd MMM yyyy HH:mm:ss.SSS z")

  private val tableId = "4000"

  private var players = gameService.getPlayersData(tableId)

  import MainActor._

  override def receive: Receive = {
    case PlayerMoneyTransaction(moneyTransaction, actor, client, admins) => handleMoneyTransaction(moneyTransaction, actor, client, admins)
    case PlayerGameTransaction(gameTransaction, admins)                  => handleGameTransaction(gameTransaction, admins)
    case PlayerRoundTransaction(roundTransactionRecord, admins)          => handleRoundTransaction(roundTransactionRecord, admins)
    case PlayerStatusOnline(uid, admins)                                 => handlePlayerStatus(uid, status = "online", admins = admins)
    case PlayerStatusOffline(uid,admins)                                 => handlePlayerStatus(uid, status = "offline", admins = admins)

    case TableStateUpdated(tableState: TableState)                       => handleTableStateUpdated(tableState)


    case _ => log.logger.info("Unknown Message")
  }


  def handleTableStateUpdated(tableState: TableState): Unit = {
    gameService.setTableData(tableId, tableState)
  }

  def handlePlayerStatus(uid: String, status: String, admins : MMap[String, AdminClientData]): Unit = {
    val player = players.find(p => p.uid == uid).get
    val foundIndex = players.indexWhere(p => p.uid == uid)

    var MessageType = "Unknown"

    status match {
      case "offline" =>
        MessageType = "PLAYER_OFFLINE"
        players = players.updated(foundIndex, player.copy(status = "offline"))
      case "online" =>
        MessageType = "PLAYER_ONLINE"
        players = players.updated(foundIndex, player.copy(status = "online"))
    }

    gameService.updatePlayerData(players.find(p => p.uid == uid).get, uid)

    admins.foreach {
      admin =>
        admin._2.client ! Json.toJson(
          PlayerUpdatedMsg(MessageType = MessageType, player = players.find(p => p.uid == uid).get, timestamp = dateFormat.format(Calendar.getInstance().getTime))
        )
    }

    gameService.addOperationTransaction(
      OperationTransactionMsg(MessageType = MessageType,
        transType = "Operation",
        uid = players.find(p => p.uid == uid).get.uid,
        nickname = players.find(p => p.uid == uid).get.nickname,
        client_ip = players.find(p => p.uid == uid).get.clientIp,
        status = players.find(p => p.uid == uid).get.status,
        usage = players.find(p => p.uid == uid).get.usage,
        timestamp = dateFormat.format(Calendar.getInstance().getTime))
    )


  }


  def handleMoneyTransaction(moneyTransaction: MoneyTransaction, actor: ActorRef, client: ActorRef, admins: MMap[String, AdminClientData]): Unit = {
    val player = players.find(p => p.uid == moneyTransaction.uid).get
    val currentBalance = player.balance

    moneyTransaction.transType match {
      case "DEPOSIT" => {
        client ! Json.toJson(
          MoneyTransactionMsg(MessageType = "DEPOSIT_REQ",
            playerIp = moneyTransaction.uid,
            amount = moneyTransaction.amount,
            oldBalance = currentBalance,
            newBalance = players.find(p => p.uid == moneyTransaction.uid).get.balance,
            timestamp = dateFormat.format(Calendar.getInstance().getTime))
        )
        gameService.addTransaction(
          MoneyTransactionMsg(MessageType = "DEPOSIT_REQ",
            playerIp = moneyTransaction.uid,
            amount = moneyTransaction.amount,
            oldBalance = currentBalance,
            newBalance = players.find(p => p.uid == moneyTransaction.uid).get.balance,
            timestamp = dateFormat.format(Calendar.getInstance().getTime))
        )

        players = players.filter(p => p.uid != moneyTransaction.uid).:+(player.copy(balance = currentBalance + moneyTransaction.amount))

        client ! Json.toJson(
          MoneyTransactionMsg(MessageType = "DEPOSIT_SUCCESS",
            playerIp = moneyTransaction.uid,
            amount = moneyTransaction.amount,
            oldBalance = currentBalance,
            newBalance = players.find(p => p.uid == moneyTransaction.uid).get.balance,
            timestamp = dateFormat.format(Calendar.getInstance().getTime)))

        gameService.addTransaction(
          MoneyTransactionMsg(MessageType = "DEPOSIT_SUCCESS",
            playerIp = moneyTransaction.uid,
            amount = moneyTransaction.amount,
            oldBalance = currentBalance,
            newBalance = players.find(p => p.uid == moneyTransaction.uid).get.balance,
            timestamp = dateFormat.format(Calendar.getInstance().getTime)))

      }
      case "WITHDRAW" => {
        client ! Json.toJson(
          MoneyTransactionMsg(MessageType = "WITHDRAW_REQ",
            playerIp = moneyTransaction.uid,
            amount = moneyTransaction.amount,
            oldBalance = currentBalance,
            newBalance = players.find(p => p.uid == moneyTransaction.uid).get.balance,
            timestamp = dateFormat.format(Calendar.getInstance().getTime)))

        gameService.addTransaction(
          MoneyTransactionMsg(MessageType = "WITHDRAW_REQ",
            playerIp = moneyTransaction.uid,
            amount = moneyTransaction.amount,
            oldBalance = currentBalance,
            newBalance = players.find(p => p.uid == moneyTransaction.uid).get.balance,
            timestamp = dateFormat.format(Calendar.getInstance().getTime)))

        players = players.filter(p => p.uid != moneyTransaction.uid).:+(player.copy(balance = currentBalance - moneyTransaction.amount))

        client ! Json.toJson(
          MoneyTransactionMsg(MessageType = "WITHDRAW_SUCCESS",
            playerIp = moneyTransaction.uid,
            amount = moneyTransaction.amount,
            oldBalance = currentBalance,
            newBalance = players.find(p => p.uid == moneyTransaction.uid).get.balance,
            timestamp = dateFormat.format(Calendar.getInstance().getTime)))
        gameService.addTransaction(
          MoneyTransactionMsg(MessageType = "WITHDRAW_SUCCESS",
            playerIp = moneyTransaction.uid,
            amount = moneyTransaction.amount,
            oldBalance = currentBalance,
            newBalance = players.find(p => p.uid == moneyTransaction.uid).get.balance,
            timestamp = dateFormat.format(Calendar.getInstance().getTime)))

      }
    }

    gameService.getPokerTableActor ! SeatBalanceUpdated(moneyTransaction.uid, players.find(p => p.uid == moneyTransaction.uid).get.balance)
    gameService.updatePlayerData(players.find(p => p.uid == moneyTransaction.uid).get, moneyTransaction.uid)

    admins.foreach {
      admin =>
        admin._2.client ! Json.toJson(
          PlayerUpdatedMsg(MessageType = "PLAYER_UPDATED", player = players.find(p => p.uid == moneyTransaction.uid).get, timestamp = dateFormat.format(Calendar.getInstance().getTime))
        )
    }

  }

  def handleRoundTransaction(roundTransactionRecord: RoundTransactionMsg, admins: MMap[String, AdminClientData]): Unit = {

    admins.foreach {
      admin =>
        admin._2.client ! Json.toJson(roundTransactionRecord)

    }

    gameService.addRoundTransaction(roundTransactionRecord)
  }

  def handleGameTransaction(gameTransaction: GameTransaction, admins: MMap[String, AdminClientData]): Unit = {
    /*
    * TODO in Game Transaction, player field is filled with uid of the seat
    * */
    logManagerActor ! AddLog(runtimeClass = "MainActor", content = s"Game Transaction ${gameTransaction.player} ${gameTransaction.transType} Bet=${gameTransaction.totalBet} Win=${gameTransaction.totalWin}")
    log.info(s"Game Transaction ${gameTransaction.player} ${gameTransaction.transType} Bet=${gameTransaction.totalBet} Win=${gameTransaction.totalWin} Rake=${gameTransaction.rake}" )
    val player = players.find(p => p.uid == gameTransaction.player).get
    val currentBalance = player.balance
    var MessageType = "Unknown"
    var amount = 0.0
    var newBalance = currentBalance

    gameTransaction.transType match {
      case "Bet" =>
        MessageType = "PLAYER_BET_PLACED"
        amount = gameTransaction.totalBet
        newBalance = newBalance - amount
      case "Win" =>
        MessageType = "PLAYER_BET_WON"
        amount = gameTransaction.totalWin
        newBalance = newBalance + amount
      case "NoWin" =>
        MessageType = "PLAYER_BET_LOST"
        amount = gameTransaction.totalBet
    }


    players = players.filter(p => p.uid != gameTransaction.player).:+(player.copy(balance = newBalance))
    gameService.updatePlayerData(players.find(p => p.uid == gameTransaction.player).get, gameTransaction.player)



    admins.foreach {
      admin =>
        admin._2.client ! Json.toJson(
          MoneyTransactionMsg(MessageType = MessageType,
            transType = gameTransaction.transType,
            playerIp = gameTransaction.player,
            rake = gameTransaction.rake,
            roundId = gameTransaction.roundId,
            amount = amount,
            oldBalance = currentBalance,
            newBalance = players.find(p => p.uid == gameTransaction.player).get.balance,
            timestamp = dateFormat.format(Calendar.getInstance().getTime))
        )

        admin._2.client ! Json.toJson(
          PlayerUpdatedMsg(MessageType = "PLAYER_UPDATED", player = players.find(p => p.uid == gameTransaction.player).get, timestamp = dateFormat.format(Calendar.getInstance().getTime))
        )
    }

    gameService.addTransaction(
      MoneyTransactionMsg(MessageType = MessageType,
        transType = gameTransaction.transType,
        playerIp = gameTransaction.player,
        rake = gameTransaction.rake,
        roundId = gameTransaction.roundId,
        amount = amount,
        oldBalance = currentBalance,
        newBalance = players.find(p => p.uid == gameTransaction.player).get.balance,
        timestamp = dateFormat.format(Calendar.getInstance().getTime))
    )

  }




  override def preStart(): Unit = {
    super.preStart()
  }

  override def postStop(): Unit = {
    gameService.saveAllData();
    super.postStop()
  }

}



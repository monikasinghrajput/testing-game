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
package services

import actors.LogManagerActor.AddLog
import actors.MainActor.GuestPlayerConnected
import actors._
import actors.holdem.PokerTable
import akka.actor.{ActorRef, ActorSystem}
import dao.{LogDao, PlayerDao, PokerDao, ServerServicesDao}
import model.common.messages.{MoneyTransactionMsg, OperationTransactionMsg, Player, RoundTransactionMsg, ServerLog, TableLimit, TableLimitCodec}
import model.poker.data.{TableData, TableState}
import play.api.Logger

class GameService(actorSystem: ActorSystem,
                  logDao: LogDao,
                  playerDao: PlayerDao,
                  pokerDao: PokerDao,
                  serverServicesDao: ServerServicesDao) extends TableLimitCodec {

  val log: Logger = Logger(this.getClass)

  var actorMain: ActorRef = _
  var actorAutoRoulette: ActorRef = _
  var actorPokerTable: ActorRef = _
  val actorLogging: ActorRef = actorSystem.actorOf(LogManagerActor.props(logDao))

  def init(): Unit = {
    actorMain = actorSystem.actorOf(MainActor.props(this), MainActor.name)
    actorPokerTable = actorSystem.actorOf(PokerTable.props(actorMain, gameService = this), PokerTable.name)
    actorLogging ! AddLog(content =  s"Actor System is : ${actorSystem.name}");
    actorLogging ! AddLog(content =  s"Actor System Uptime : ${actorSystem.uptime}");
    actorLogging ! AddLog(content = "Roulette 8 Seater is being Initialized")
    actorLogging ! AddLog(content = "Poker 8 Seater Table is being Initialized")
  }

  def getAutoRouletteActor: ActorRef = actorAutoRoulette
  def getPokerTableActor: ActorRef = actorPokerTable
  def getMainActor:ActorRef = actorMain
  def getLoggingActor: ActorRef = actorLogging

  def getTableLimits(tableId: String, limitId: Int): TableLimit = serverServicesDao.getTableLimits(tableId, limitId).as[TableLimit];


  def getTableData(tableId: String) : TableData = {
    tableId match {
      case "4000" => pokerDao.getGameData().toTableData
      case _ => TableData()
    }
  }

  def getTableState(tableId: String) : TableState = {
    tableId match {
      case "4000" => pokerDao.getGameData()
      case _ => TableState()
    }
  }



  def getRoundTransactions(): Seq[RoundTransactionMsg] = playerDao.getRoundTransactionsData()
  def addRoundTransaction(roundTransaction: RoundTransactionMsg) = playerDao.addRoundTransaction(roundTransaction)

  def getTransactions(): Seq[MoneyTransactionMsg] = playerDao.getTransactionsData()
  def addTransaction(transaction: MoneyTransactionMsg) = playerDao.addTransaction(transaction)

  def getOperationTransactions(): Seq[OperationTransactionMsg] = playerDao.getOperationTransactionsData()
  def addOperationTransaction(transaction: OperationTransactionMsg) = playerDao.addOperationTransaction(transaction)

  //TODO do table id specific dao action
  def getPlayersData(tableId: String): Seq[Player] = playerDao.getPlayersData

  def getPlayerData(player: String): Player = {
    val playerData = playerDao.getPlayerData(player)
    if(playerData.nickname == "Guest") {
      val updatedPlayerData = playerData.copy(nickname = s"Guest${player}");
      actorMain ! GuestPlayerConnected(updatedPlayerData)
      actorLogging ! AddLog(logType = "warning",  content = s"Guest Player Allowed to play ${updatedPlayerData}")
      log.logger.warn(s"Guest Player Allowed to play ${updatedPlayerData}")
      updatedPlayerData
    } else playerData
  }

  def createPlayer(player: Player): Seq[Player] = playerDao.addPlayer(player)//Used only for Guest Connection case but uid is still -1!!
  def updatePlayerData(player: Player, uid: String): Unit = playerDao.updatePlayer(player, uid)


  def setTableData(tableId: String, data: TableState): Unit = {
    tableId match {
      case "4000" => pokerDao.setGameData(data)
      case _ =>
    }
  }
  def reloadTableState(tableId: String): Unit = {
    tableId match {
      case "4000" => pokerDao.reloadGameData()
      case _ =>
    }
  }


  def saveAllData(): Unit = {
    playerDao.setTransactions();
    playerDao.setOperationTransactions();
  }

}

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

import play.api.Logger
import play.api.libs.json.{JsValue, Json}
import model.poker.data.{Seat, TableState}
import model.poker.codecs.PokerCodecs
import os.Path

import java.text.SimpleDateFormat
import java.util.Calendar

class PokerDao(playerDao: PlayerDao) extends PokerCodecs {

  val log: Logger = Logger(this.getClass)

  val fileName: Path = os.home / "holdem" /  "game_data.json"

  if (!os.exists(fileName)) {
    val usersBalanceMap: Map[String, Double] = playerDao.players.map(user => user.uid -> user.balance ).toMap
    val usersIpMap: Map[String, String] = playerDao.players.map(user => user.uid -> user.clientIp ).toMap
    val usersNameMap: Map[String, String] = playerDao.players.map(user => user.uid -> user.nickname ).toMap

    log.logger.info(s"Backup Game Missing, creating a fresh one..")

    os.write.over(
      fileName,
      Json.prettyPrint(
        Json.toJson(
          TableState(seats = Seq(
            Seat(id = 0, uid = "1", balance = usersBalanceMap.getOrElse("1", 0), ip = usersIpMap.getOrElse("1", "192.168.1.2"), name = usersNameMap.getOrElse("1", "P1")),
            Seat(id = 1, uid = "2", balance = usersBalanceMap.getOrElse("2", 0), ip = usersIpMap.getOrElse("2", "192.168.1.3"), name = usersNameMap.getOrElse("2", "P2")),
            Seat(id = 2, uid = "3", balance = usersBalanceMap.getOrElse("3", 0), ip = usersIpMap.getOrElse("3", "192.168.1.4"), name = usersNameMap.getOrElse("3", "P3")),
            Seat(id = 3, uid = "4", balance = usersBalanceMap.getOrElse("4", 0), ip = usersIpMap.getOrElse("4", "192.168.1.5"), name = usersNameMap.getOrElse("4", "P4")),
            Seat(id = 4, uid = "5", balance = usersBalanceMap.getOrElse("5", 0), ip = usersIpMap.getOrElse("5", "192.168.1.6"), name = usersNameMap.getOrElse("5", "P5")),
            Seat(id = 5, uid = "6", balance = usersBalanceMap.getOrElse("6", 0), ip = usersIpMap.getOrElse("6", "192.168.1.7"), name = usersNameMap.getOrElse("6", "P6")),
            Seat(id = 6, uid = "7", balance = usersBalanceMap.getOrElse("7", 0), ip = usersIpMap.getOrElse("7", "192.168.1.8"), name = usersNameMap.getOrElse("7", "P7")),
            Seat(id = 7, uid = "8", balance = usersBalanceMap.getOrElse("8", 0), ip = usersIpMap.getOrElse("8", "192.168.1.9"), name = usersNameMap.getOrElse("8", "P8")))
          )
        )
      ),
      createFolders = true
    )
  }
  val dataJson: JsValue = Json.parse(os.read(fileName))

  var gameData: TableState = dataJson.validate[TableState].fold(
    invalid = { fieldErrors =>
      log.logger.info(s"Backup Game Data Read failed..")
      fieldErrors.foreach { x =>
        log.logger.info(s"field: ${x._1}, errors: ${x._2}")
      }

      val usersBalanceMap: Map[String, Double] = playerDao.players.map(user => user.uid -> user.balance ).toMap
      val usersIpMap: Map[String, String] = playerDao.players.map(user => user.uid -> user.clientIp ).toMap
      val usersNameMap: Map[String, String] = playerDao.players.map(user => user.uid -> user.nickname ).toMap

      log.logger.info(s"Backup Data Read Error, creating a fresh one..")

      val freshTableState =   TableState(seats = Seq(
        Seat(id = 0, uid = "1", balance = usersBalanceMap.getOrElse("1", 0), ip = usersIpMap.getOrElse("1", "192.168.1.2"), name = usersNameMap.getOrElse("1", "P1")),
        Seat(id = 1, uid = "2", balance = usersBalanceMap.getOrElse("2", 0), ip = usersIpMap.getOrElse("2", "192.168.1.3"), name = usersNameMap.getOrElse("2", "P2")),
        Seat(id = 2, uid = "3", balance = usersBalanceMap.getOrElse("3", 0), ip = usersIpMap.getOrElse("3", "192.168.1.4"), name = usersNameMap.getOrElse("3", "P3")),
        Seat(id = 3, uid = "4", balance = usersBalanceMap.getOrElse("4", 0), ip = usersIpMap.getOrElse("4", "192.168.1.5"), name = usersNameMap.getOrElse("4", "P4")),
        Seat(id = 4, uid = "5", balance = usersBalanceMap.getOrElse("5", 0), ip = usersIpMap.getOrElse("5", "192.168.1.6"), name = usersNameMap.getOrElse("5", "P5")),
        Seat(id = 5, uid = "6", balance = usersBalanceMap.getOrElse("6", 0), ip = usersIpMap.getOrElse("6", "192.168.1.7"), name = usersNameMap.getOrElse("6", "P6")),
        Seat(id = 6, uid = "7", balance = usersBalanceMap.getOrElse("7", 0), ip = usersIpMap.getOrElse("7", "192.168.1.8"), name = usersNameMap.getOrElse("7", "P7")),
        Seat(id = 7, uid = "8", balance = usersBalanceMap.getOrElse("8", 0), ip = usersIpMap.getOrElse("8", "192.168.1.9"), name = usersNameMap.getOrElse("8", "P8")))
      )

      os.write.over(
        fileName,
        Json.prettyPrint(
          Json.toJson(
            freshTableState
          )
        ),
        createFolders = true
      )

      freshTableState

    },
    valid = { data =>
      log.logger.info(s"Backup Game Data Read Success..")
      data }
  )

  def getGameData(): TableState = gameData

  def setGameData(data: TableState): Unit =  {
    gameData = data

    os.write.over(
      fileName,
      Json.prettyPrint(Json.toJson(data)),
      createFolders = true
    )
  }

  def reloadGameData(): Unit =  {
    log.logger.info(s"Reloading Game Data..")

    val dateFormat = new SimpleDateFormat("E, dd MMM yyyy HH:mm:ss.SSS z")

    val usersBalanceMap: Map[String, Double] = playerDao.players.map(user => user.uid -> user.balance ).toMap
    val usersIpMap: Map[String, String] = playerDao.players.map(user => user.uid -> user.clientIp ).toMap
    val usersNameMap: Map[String, String] = playerDao.players.map(user => user.uid -> user.nickname ).toMap


    val freshTableState =   TableState(seats = Seq(
      Seat(id = 0, uid = "1", balance = usersBalanceMap.getOrElse("1", 0), ip = usersIpMap.getOrElse("1", "192.168.1.2"), name = usersNameMap.getOrElse("1", "P1")),
      Seat(id = 1, uid = "2", balance = usersBalanceMap.getOrElse("2", 0), ip = usersIpMap.getOrElse("2", "192.168.1.3"), name = usersNameMap.getOrElse("2", "P2")),
      Seat(id = 2, uid = "3", balance = usersBalanceMap.getOrElse("3", 0), ip = usersIpMap.getOrElse("3", "192.168.1.4"), name = usersNameMap.getOrElse("3", "P3")),
      Seat(id = 3, uid = "4", balance = usersBalanceMap.getOrElse("4", 0), ip = usersIpMap.getOrElse("4", "192.168.1.5"), name = usersNameMap.getOrElse("4", "P4")),
      Seat(id = 4, uid = "5", balance = usersBalanceMap.getOrElse("5", 0), ip = usersIpMap.getOrElse("5", "192.168.1.6"), name = usersNameMap.getOrElse("5", "P5")),
      Seat(id = 5, uid = "6", balance = usersBalanceMap.getOrElse("6", 0), ip = usersIpMap.getOrElse("6", "192.168.1.7"), name = usersNameMap.getOrElse("6", "P6")),
      Seat(id = 6, uid = "7", balance = usersBalanceMap.getOrElse("7", 0), ip = usersIpMap.getOrElse("7", "192.168.1.8"), name = usersNameMap.getOrElse("7", "P7")),
      Seat(id = 7, uid = "8", balance = usersBalanceMap.getOrElse("8", 0), ip = usersIpMap.getOrElse("8", "192.168.1.9"), name = usersNameMap.getOrElse("8", "P8")))
    )

    gameData = freshTableState

    os.copy(
      fileName,
      os.pwd / "jsons" / "holdem" /  s"game_data_${dateFormat.format(Calendar.getInstance().getTime)}.json",
      replaceExisting = true
    )
    os.write.over(
      fileName,
      Json.prettyPrint(Json.toJson(freshTableState)),
      createFolders = true
    )
  }

}

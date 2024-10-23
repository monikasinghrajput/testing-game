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


import akka.actor.{Actor, Props}
import dao.LogDao
import model.common.messages.ServerLog
import play.api.Logger

import java.text.SimpleDateFormat
import java.time.Instant
import java.util.Calendar

class LogManagerActor(logDao: LogDao) extends Actor {

  import actors.LogManagerActor._

  val log: Logger = Logger(this.getClass);
  private var logs: Seq[ServerLog] = logDao.getLogs
  val dateFormat =  new SimpleDateFormat("E, dd MMM yyyy HH:mm:ss.SSS z")

  override def receive: Receive = {
    case GetAllLogs => sender() ! logs.take(200)
    case AddLog(logType, runtimeClass, content) =>
      logs = ServerLog(logType, runtimeClass, content, dateFormat.format(Calendar.getInstance().getTime)) +: logs
    case _ => log.logger.info("Unknown message!")
  }

  override def preStart(): Unit = {
    super.preStart()
    log.logger.info(s"Log Manager preStart Called")
  }

  override def postStop(): Unit = {
    logDao.setLogs(logs)
    super.postStop()
  }
}

object LogManagerActor {
  val name = "log-manager-actor"
  val path = s"/usr/${name}"
  case object GetAllLogs
  case class AddLog(logType: String = "info", runtimeClass: String = "General", content: String)

  def props(logDao: LogDao): Props = Props(new LogManagerActor(logDao))
}

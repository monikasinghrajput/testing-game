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

import model.common.messages.{LogCodecs, ServerLog}
import os.Path
import play.api.Logger
import play.api.libs.json.{JsValue, Json}

import java.text.SimpleDateFormat
import java.util.Calendar

class LogDao extends LogCodecs {
  val log: Logger = Logger(this.getClass)
  val dateFormat = new SimpleDateFormat("E, dd MMM yyyy HH:mm:ss.SSS z")
  val fileName: Path = os.home / "holdem" /  "logs.json"

  if (!os.exists(fileName)) {
    os.write.over(
      fileName,
      Json.prettyPrint(Json.toJson(
        Seq(ServerLog("info", "System", "File Creation", dateFormat.format(Calendar.getInstance().getTime))))
      ),
      createFolders = true
    )
    log.logger.info(s"Backup Logs Created Success..")
  }

  //Reads the content of a path as a string by default uses utf8 for reading
  val logsFileString: String = os.read(fileName)
  val logsJson: JsValue = Json.parse(logsFileString)

  var logs: Seq[ServerLog] = logsJson.validate[Seq[ServerLog]].fold(
    invalid = { fieldErrors =>
      log.logger.info(s"Backup Logs Read failed..")
      fieldErrors.foreach { x =>
        log.logger.info(s"field: ${x._1}, errors: ${x._2}")
      }

      os.write.over(
        fileName,
        Json.prettyPrint(Json.toJson(
          Seq(ServerLog("info", "System", "File Creation", dateFormat.format(Calendar.getInstance().getTime))))
        ),
        createFolders = true
      )
      log.logger.info(s"Backup Logs Recreated Success..")
      Seq(ServerLog("info", "System", "File ReCreation", dateFormat.format(Calendar.getInstance().getTime)))
    },
    valid = { logs =>
      log.logger.info(s"Backup Logs Read Success..")
      logs
    }
  )

  def getLogs: Seq[ServerLog] = logs
  def setLogs(newLogs: Seq[ServerLog]): Unit = {
    log.logger.info(s"Saving Logs..")
    os.write.over(
      fileName,
      Json.prettyPrint(Json.toJson(newLogs.takeRight(100)
      )),
      createFolders = true
    )
  }

}
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
package controllers

import play.api.libs.json.Json
import play.api.mvc._
import dao.{ServerServicesDao}

class AppApiController(components: ControllerComponents, serverServicesDao: ServerServicesDao) extends AbstractController(components) {

  def sendRequestContentJson: Action[AnyContent] = Action { request =>
    val startTime: Long = System.currentTimeMillis()

    Ok(Json.prettyPrint(
      Json.obj(
        "time_elapsed" -> Json.toJson(s"${System.currentTimeMillis() - startTime} ms"),
        "result" -> Json.obj(
          "remoteAddress" -> Json.toJson(request.remoteAddress),
          "domain" -> Json.toJson(request.domain),
          "host" -> Json.toJson(request.host),
          "charSet" -> Json.toJson(request.charset.getOrElse("")),
          "contentType" -> Json.toJson(request.contentType.getOrElse("")),
          "acceptedTypes" -> Json.toJson(request.acceptedTypes.map(mr => mr.toString())),
          "acceptLanguages" -> Json.toJson(request.acceptLanguages),
          "name" -> Json.toJson(request.getQueryString("name")),
          "id" -> Json.toJson(request.id),
          "path" -> Json.toJson(request.path),
          "qs" -> Json.toJson(request.rawQueryString),
          "version" -> Json.toJson(request.version),
          "uri" -> Json.toJson(request.uri),
          "method" -> Json.toJson(request.method),
        ),
      )
    )
    )
  }

  def sendInitialDataJson: Action[AnyContent] = Action(
    Ok(serverServicesDao.getInitialDataJsonString)
  )

  def sendAuthenticateJson: Action[AnyContent] = Action(
    Ok(serverServicesDao.authenticateJsonString)
  )

  def sendStreamsJson: Action[AnyContent] = Action(
    Ok(serverServicesDao.sendStreamsJsonString)
  )

  def getTableLimits(tableId: String, limitId: Int) = Action(
    Ok(
      Json.prettyPrint(
        Json.obj(
          "result" -> serverServicesDao.getTableLimits(tableId, limitId)
        )
      )
    )
  )

}

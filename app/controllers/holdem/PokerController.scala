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
package controllers.holdem

import akka.actor.ActorSystem
import akka.stream.Materializer
import play.api.Logger
import play.api.libs.json.{JsError, JsValue, Json, Reads}
import play.api.libs.streams.ActorFlow
import play.api.mvc._
import actors.holdem.clients.{AdminActor, ClientActor, TopperActor}
import services.GameService
import actors.holdem.PokerTable.CardDrawn
import model.poker.{Ace, Card, Clubs}
case class Draw(card: String)


class PokerController(components: ControllerComponents,
                    actorSystem: ActorSystem,
                    gameService: GameService,
                    mat: Materializer)
  extends AbstractController(components) {

  val log = Logger(this.getClass)

  implicit val reads: Reads[Draw] = Json.reads[Draw]

  implicit val materializer = mat
  implicit val actorFactory = actorSystem

  /*
  * serving html5 pages /front-end interfaces
  * - player
  * - topper
  * - admin
  * */

  def sendPlayerPage: Action[AnyContent] = Action { request =>
    val ip = request.getQueryString("ip").getOrElse(request.remoteAddress)
    log.logger.info(s"sending player page for ${request.remoteAddress}")
    Ok(views.html.pages.tablet())
  }

  def sendTopperPage: Action[AnyContent] = Action { request =>
    log.logger.info(s"sending topper page for ${request.remoteAddress}")
    Ok(views.html.pages.topper())
  }

  def sendAdminPage: Action[AnyContent] = Action { request =>
    log.logger.info(s"sending admin page for ${request.remoteAddress}")
    Ok(views.html.pages.admin())
  }


  /*
  * serving websocket requests with a 2 way Json Interface
  *  - player
  *  - topper
  *  - admin
  * */

  def holdem2: WebSocket = WebSocket.accept[Array[Byte], Array[Byte]] { request =>
    val ip = request.getQueryString("ip").getOrElse(request.remoteAddress)
    log.logger.info(s"player socket request from ${ip}");

    ActorFlow.actorRef { out =>
      ClientActor.props(out, gameService.getPokerTableActor, gameService.getLoggingActor, ip)
    }
  }
  def holdem: WebSocket = WebSocket.accept[JsValue, JsValue] { request =>
    val ip = request.getQueryString("ip").getOrElse(request.remoteAddress)
    log.logger.info(s"player socket request from ${ip}");

    ActorFlow.actorRef { out =>
      ClientActor.props(out, gameService.getPokerTableActor, gameService.getLoggingActor, ip)
    }
  }


  def topper: WebSocket = WebSocket.accept[JsValue, JsValue] { request =>
    val ip = request.getQueryString("ip").getOrElse(request.remoteAddress)
    log.logger.info(s"topper socket request from ${ip}");

    ActorFlow.actorRef { out =>
      TopperActor.props(out, gameService.getPokerTableActor, gameService.getLoggingActor, ip)
    }
  }

  def admin: WebSocket = WebSocket.accept[JsValue, JsValue] { request =>
    val ip = request.getQueryString("ip").getOrElse(request.remoteAddress)
    log.logger.info(s"admin socket request from ${ip}");

    ActorFlow.actorRef { out =>
      AdminActor.props(out, gameService.getPokerTableActor, gameService.getLoggingActor, ip)
    }
  }



  /*
  * HTTP Rest API (Json as the Content)
  *   - sendInitialDataJson - HTTP GET
  *   - sendAuthenticateJson - HTTP GET
  *   - sendStreamsJson - HTTP GET
  *   - handleCardDrawn - POST
  * */

//  def sendInitialDataJson: Action[AnyContent] = Action { request =>
//
//    val limitId = request.getQueryString("limit_id").getOrElse("")
//    val gameId = request.getQueryString("game_id").getOrElse("38")
//    val tableId = request.getQueryString("table_id").getOrElse("228000")
//    val token = request.getQueryString("token").getOrElse("1")
//    val operatorId = request.getQueryString("operatorId").getOrElse("964")
//
//    Ok(
//      andarbaharSSeaterTableService.getInitialDataJsonString(tableId, token, operatorId)
//    ).withHeaders("Access-Control-Allow-Origin" -> "*",
//      "Allow" -> "*",
//      "Access-Control-Allow-Methods" -> "POST, GET, PUT, DELETE, OPTIONS",
//      "Access-Control-Allow-Headers" -> "Origin, X-Requested-With, Content-Type, Accept, Referrer, User-Agent, X-Auth-Token, X-Api-Key")
//  }
//
//  def sendAuthenticateJson: Action[AnyContent] = Action(
//    Ok(andarbaharSSeaterTableService.authenticateJsonString).withHeaders("Access-Control-Allow-Origin" -> "*",
//      "Allow" -> "*",
//      "Access-Control-Allow-Methods" -> "POST, GET, PUT, DELETE, OPTIONS",
//      "Access-Control-Allow-Headers" -> "Origin, X-Requested-With, Content-Type, Accept, Referrer, User-Agent, X-Auth-Token, X-Api-Key")
//  )
//
//  def sendStreamsJson: Action[AnyContent] = Action(
//    Ok(andarbaharSSeaterTableService.sendStreamsJsonString).withHeaders("Access-Control-Allow-Origin" -> "*",
//      "Allow" -> "*",
//      "Access-Control-Allow-Methods" -> "POST, GET, PUT, DELETE, OPTIONS",
//      "Access-Control-Allow-Headers" -> "Origin, X-Requested-With, Content-Type, Accept, Referrer, User-Agent, X-Auth-Token, X-Api-Key")
//  )
//
//  def sendPayoutJson: Action[AnyContent] = Action(
//    Ok(Json.prettyPrint(
//      Json.obj(
//        "error_code" -> Json.toJson(0),
//        "error_message" -> Json.toJson(""),
//        "result" -> Json.obj(
//          "game_id" -> Json.toJson("38"),
//          "multipliers" -> Json.toJson(Array.empty[Int]),
//          "payouts" -> Json.obj(
//            "1" -> "1",
//            "2" -> "0.9",
//            "3" -> "2.5",
//            "4" -> "3.5",
//            "5" -> "4.5",
//            "6" -> "3.5",
//            "7" -> "14",
//            "8" -> "24",
//            "9" -> "49",
//            "10" -> "119",
//            "11" -> "1",
//            "12" -> "1"),
//        ),
//      )
//    )).withHeaders("Access-Control-Allow-Origin" -> "*",
//      "Allow" -> "*",
//      "Access-Control-Allow-Methods" -> "POST, GET, PUT, DELETE, OPTIONS",
//      "Access-Control-Allow-Headers" -> "Origin, X-Requested-With, Content-Type, Accept, Referrer, User-Agent, X-Auth-Token, X-Api-Key")
//  )


  def handleCardDrawn: Action[JsValue] = Action(parse.json) { request =>
    val drawCard = request.body.validate[Draw]
    drawCard.fold(
      errors => {
        BadRequest(Json.obj("message" -> JsError.toJson(errors)))
      },
      draw => {
        val card: Card = Card.parseBeeTekCard(draw.card).getOrElse(Card(suit = Clubs(), rank = Ace()))
        gameService.getPokerTableActor ! CardDrawn(card)
        Ok(Json.obj("message" -> ("Card is Forwarded Successfully.")))
      }
    )
  }

}

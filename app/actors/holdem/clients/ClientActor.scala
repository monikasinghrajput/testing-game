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
package actors.holdem.clients

//Standard Packages
import actors.LogManagerActor.AddLog
import actors.holdem.PokerTable
import akka.actor.{Actor, ActorRef, Props}
import play.api.Logger
import play.api.libs.json.{JsString, JsValue, Json}

import java.nio.charset.StandardCharsets

object ClientActor {

  def props(out: ActorRef, dealer: ActorRef, logManagerActor: ActorRef, remoteAddress: String): Props = Props(new ClientActor(out, dealer, logManagerActor, remoteAddress))
}

class ClientActor(out: ActorRef, dealer: ActorRef, logManagerActor: ActorRef, remoteAddress: String)
  extends Actor
{

  import PokerTable._

  val log = Logger(this.getClass)

  private var clientIp = remoteAddress

  override def preStart(): Unit = {
//    logManagerActor ! AddLog(logType = "warning", content = s"Client Socket Flow Actor for ${clientIp} Started")
//    log.info(s"Client Socket Flow Actor for ${clientIp} Started")
    super.preStart()
  }

  override def postStop(): Unit = {
//    logManagerActor ! AddLog(logType = "warning", content = s"Client Socket Flow Actor for ${clientIp} Stopped")
//    log.info(s"Client Socket Flow Actor for ${clientIp} Removed")
    if (clientIp != "") dealer ! PlayerDisConnected(clientIp);
    super.postStop()
  }

  override def receive: Receive = {
    case clientMsg: JsValue => {
      if (clientMsg == JsString("PingMessage")) {
      } else {
        clientMsg("MessageType") match {
          case JsString("INITIALIZE_PLAYER") => {
            dealer ! PlayerConnected(clientIp, self, out)
          }

          case JsString("ALL_IN") =>
//            log.info("ALL_IN")
            dealer ! AllInCmd(clientMsg("uid").as[String])

          case JsString("FOLD_HAND") =>
//            log.info("FOLD_HAND")
            dealer ! FoldCmd(clientMsg("uid").as[String])

          case JsString("CHECK_HAND") =>
//            log.info("CHECK_HAND")
            dealer ! CheckCmd(clientMsg("uid").as[String])


          case JsString("CALL_HAND") =>
//            log.info("CALL_HAND")
            dealer ! CallCmd(clientMsg("uid").as[String])

          case JsString("RAISE_HAND") =>
//            log.info("RAISE_HAND")
            dealer ! RaiseCmd(clientMsg("uid").as[String], clientMsg("amount").as[Int])

          case JsString("BET_HAND") =>
//            log.info("BET_HAND")
            dealer ! BetCmd(clientMsg("uid").as[String], clientMsg("amount").as[Int])



          case _ => log.logger.info(s"Unknown MessageType ${clientMsg("MessageType")} Received!")
        }
      }

    }
    case Reconnected =>
      log.logger.info("Client Reconnected Scenario ???!!")
      logManagerActor ! AddLog(logType = "warning", content = s"Client Reconnected Scenario ???!!")
      clientIp = ""
    case bytes: Array[Byte] =>
//      log.logger.info(s"Byte Array Message Received!")
      val msg = new String(bytes, StandardCharsets.UTF_8)
      val clientMsg = Json.parse(msg);
//      log.logger.info(s"${clientMsg}");
      clientMsg("MessageType") match {
        case JsString("INITIALIZE_PLAYER") =>
          dealer ! PlayerConnected(clientIp, self, out, "unity")

        case JsString("ALL_IN") =>
//          log.info("ALL_IN")
          dealer ! AllInCmd(clientMsg("uid").as[String])

        case JsString("FOLD_HAND") =>
//          log.info("FOLD_HAND")
          dealer ! FoldCmd(clientMsg("uid").as[String])

        case JsString("CHECK_HAND") =>
//          log.info("CHECK_HAND")
          dealer ! CheckCmd(clientMsg("uid").as[String])


        case JsString("CALL_HAND") =>
//          log.info("CALL_HAND")
          dealer ! CallCmd(clientMsg("uid").as[String])

        case JsString("RAISE_HAND") =>
//          log.info("RAISE_HAND")
          dealer ! RaiseCmd(clientMsg("uid").as[String], clientMsg("amount").as[Int])

        case JsString("BET_HAND") =>
//          log.info("BET_HAND")
          dealer ! BetCmd(clientMsg("uid").as[String], clientMsg("amount").as[Int])


        case _ => log.logger.info(s"Unknown MessageType ${clientMsg("MessageType")} Received!")
      }



    case _ => log.logger.info("Unknown Message Received!")

  }

}




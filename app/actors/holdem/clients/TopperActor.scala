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
import play.api.libs.json.{JsString, JsValue}

object TopperActor {

  def props(out: ActorRef, dealer: ActorRef, logManagerActor: ActorRef, remoteAddress: String): Props = Props(new TopperActor(out, dealer, logManagerActor, remoteAddress))
}

class TopperActor(out: ActorRef, dealer: ActorRef, logManagerActor: ActorRef, remoteAddress: String)
  extends Actor
{

  import PokerTable._

  val log = Logger(this.getClass)

  private var clientIp = remoteAddress

  override def preStart(): Unit = {
    logManagerActor ! AddLog(logType = "warning", content = s"Topper Socket Flow Actor for ${clientIp} Started")
    super.preStart()
  }

  override def postStop(): Unit = {
    logManagerActor ! AddLog(logType = "warning", content = s"Topper Socket Flow Actor for ${clientIp} Stopped")

    if (clientIp != "") dealer ! TopperDisConnected(clientIp);
    super.postStop()
  }

  override def receive: Receive = {
    case clientMsg: JsValue => {
      if (clientMsg == JsString("PingMessage")) {
      } else {
        clientMsg("MessageType") match {
          case JsString("INITIALIZE_TOPPER") => {
            log.logger.info(s"INITIALIZE_TOPPER received from ${clientIp}!")
            dealer ! TopperConnected(clientIp, self, out)
          }

          case _ => log.logger.info(s"Unknown MessageType ${clientMsg("MessageType")} Received!")
        }
      }

    }

    case Reconnected =>
      log.logger.info("Topper Reconnected Scenario ???!!")
      logManagerActor ! AddLog(logType = "warning", content = s"Topper Reconnected Scenario ???!!")
      clientIp = ""

    case _ => log.logger.info("Unknown Message Received!")

  }

}




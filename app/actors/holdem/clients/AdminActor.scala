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
import akka.actor.{Actor, ActorRef, Props}
import play.api.Logger
import play.api.libs.json.{JsString, JsValue}
import model.common.messages.MoneyTransaction
import model.poker.codecs.PokerCodecs
import model.poker.data.CardsConfig

object AdminActor {

  def props(out: ActorRef, dealer: ActorRef, logManagerActor: ActorRef, remoteAddress: String): Props = Props(new AdminActor(out, dealer, logManagerActor, remoteAddress))
}

class AdminActor(out: ActorRef, dealer: ActorRef, logManagerActor: ActorRef, remoteAddress: String)
  extends Actor
  with PokerCodecs
{

  import actors.holdem.PokerTable._
  import actors.LogManagerActor._

  val log = Logger(this.getClass)

  private var clientIp = remoteAddress

  override def preStart(): Unit = {
    super.preStart()
  }

  override def postStop(): Unit = {
    if (clientIp != "") dealer ! AdminDisConnected(clientIp);
    log.logger.info("admin client actor stopped..")
    super.postStop()
  }

  override def receive: Receive = {
    case clientMsg: JsValue => {
      if (clientMsg == JsString("PingMessage")) {
      } else {
        clientMsg("MessageType") match {
          case JsString("INITIALIZE_ADMIN") => {
            log.logger.info(s"INITIALIZE_ADMIN received from ${clientIp}!")
            dealer ! AdminConnected(clientIp, self, out)
          }

          case JsString("SETTINGS_UPDATE") => {
//            log.logger.info(s"SETTINGS_UPDATE received & decoded" +
//              s"pokerVariant->${clientMsg("pokerVariant")}" +
//              s"betLimit->${clientMsg("betLimit")}" +
//              s"liveDealer->${clientMsg("liveDealer")}" +
//              s"tournamentMode->${clientMsg("tournamentMode")}" +
//              s"playerCardsConfig->${clientMsg("playerCardsConfig").as[CardsConfig]}" +
//              s"communityCardsConfig->${clientMsg("communityCardsConfig").as[CardsConfig]}" +
//              s"blind->${clientMsg("blind")}" +
//              s"rakePercent->${clientMsg("rakePercent")}" + "received from " + s"${clientIp}!")

            dealer ! TableSettingsChange(
              pokerVariant = clientMsg("pokerVariant").as[String],
              betLimit = clientMsg("betLimit").as[String],
              liveDealer = clientMsg("liveDealer").as[Boolean],
              tournamentMode = clientMsg("tournamentMode").as[Boolean],
              playerCardsConfig = clientMsg("playerCardsConfig").as[CardsConfig],
              communityCardsConfig = clientMsg("communityCardsConfig").as[CardsConfig],
              rakePercent = clientMsg("rakePercent").as[Int],
              blind = clientMsg("blind").as[Int] )

          }

          case JsString("DEPOSIT_REQ") => {
//            logManagerActor ! AddLog(content = s"DEPOSIT_REQ ${clientMsg("amount")}  for ${clientMsg("clientIp")} received from ${clientIp}!")
//            log.logger.info(s"DEPOSIT_REQ ${clientMsg("amount")}  for ${clientMsg("clientIp")} received from ${clientIp}!")

            dealer ! AdminMoneyTransaction(
              MoneyTransaction(
                transType = "DEPOSIT",
                admin = clientIp,
                playerIp = clientMsg("clientIp").validate[String].get,
                uid = clientMsg("uid").validate[String].get,
                amount = clientMsg("amount").validate[Double].get,
              ),
              self,
              out
            )
          }
          case JsString("WITHDRAW_REQ") => {
//            logManagerActor ! AddLog(content = s"WITHDRAW_REQ ${clientMsg("amount")}  for ${clientMsg("clientIp")} received from ${clientIp}!")
//            log.logger.info(s"WITHDRAW_REQ ${clientMsg("amount")}  for ${clientMsg("clientIp")}  received from ${clientIp}!")

            dealer ! AdminMoneyTransaction(
              MoneyTransaction(
                transType = "WITHDRAW",
                admin = clientIp,
                playerIp = clientMsg("clientIp").validate[String].get,
                uid = clientMsg("uid").validate[String].get,
                amount = clientMsg("amount").validate[Double].get
              ),
              self,
              out
            )
          }

          case JsString("CASH_IN_REQ") => {
//            logManagerActor ! AddLog(content = s"CASH_IN_REQ ${clientMsg("amount")}  for ${clientMsg("uid")} received from ${clientIp}!")
//            log.logger.info(s"CASH_IN_REQ ${clientMsg("amount")}  for ${clientMsg("uid")} received from ${clientIp}!")

            dealer ! AdminMoneyTransaction(
              MoneyTransaction(
                transType = "DEPOSIT",
                admin = clientIp,
                playerIp = clientIp,
                uid = clientMsg("uid").validate[String].get,
                amount = clientMsg("amount").validate[Double].get,
              ),
              self,
              out
            )
          }
          case JsString("CASH_OUT_REQ") => {
//            logManagerActor ! AddLog(content = s"CASH_OUT_REQ ${clientMsg("amount")}  for ${clientMsg("uid")} received from ${clientIp}!")
//            log.logger.info(s"CASH_OUT_REQ ${clientMsg("amount")}  for ${clientMsg("uid")}  received from ${clientIp}!")

            dealer ! AdminMoneyTransaction(
              MoneyTransaction(
                transType = "WITHDRAW",
                admin = clientIp,
                playerIp = clientIp,
                uid = clientMsg("uid").validate[String].get,
                amount = clientMsg("amount").validate[Double].get
              ),
              self,
              out
            )
          }

          case JsString("ELECTION_REQ") =>
            dealer ! StartElection
          case JsString("CANCEL_GAME_REQ") =>
            dealer ! GameCancel
          case JsString("NEW_GAME_REQ") =>
            log.info("NEW_GAME_REQ")
            dealer ! NewGameCmd
          case JsString("ELECTION_SKIP_REQ") =>
            dealer ! SkipElection
          case JsString("PROCEED_REQ") =>
            dealer ! ContinueToNextRound
          case JsString("DRAW_CARDS") =>
            dealer ! DrawCards

          case JsString("ALL_IN") =>
            dealer ! AllInCmd(clientMsg("uid").as[String])

          case JsString("FOLD_HAND") =>
            dealer ! FoldCmd(clientMsg("uid").as[String])

          case JsString("CHECK_HAND") =>
            dealer ! CheckCmd(clientMsg("uid").as[String])

          case JsString("RAISE_HAND") =>
            dealer ! RaiseCmd(clientMsg("uid").as[String], clientMsg("amount").as[Int])

          case JsString("BET_HAND") =>
            dealer ! BetCmd(clientMsg("uid").as[String], clientMsg("amount").as[Int])

          case JsString("CALL_HAND") =>
            dealer ! CallCmd(clientMsg("uid").as[String])

          case _ => log.logger.info(s"Unknown MessageType ${clientMsg("MessageType")} Received!")
        }
      }

    }
    case Reconnected =>
      log.logger.info("Admin Reconnected Scenario ???!!")
      logManagerActor ! AddLog(logType = "warning", content = s"Admin Reconnected Scenario ???!!")
      clientIp = ""

    case _ => log.logger.info("Unknown Message Received!")

  }

}




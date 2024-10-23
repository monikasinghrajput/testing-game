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
import _root_.controllers._
import _root_.controllers.holdem.PokerController
import com.softwaremill.macwire._

import akka.actor.ActorSystem
import scala.concurrent.Future

import play.api.ApplicationLoader.Context
import play.api._
import play.api.db.{DBComponents, HikariCPComponents}
import play.api.mvc.DefaultControllerComponents
import play.api.routing.Router
import router.Routes

import services._
import dao._


class AppLoader extends ApplicationLoader {

  def load(context: Context) = {
    LoggerConfigurator(context.environment.classLoader).foreach { configurator =>
      configurator.configure(context.environment)
    }
    new AppComponents(context).application
  }
}

class AppComponents(context: Context)
  extends BuiltInComponentsFromContext(context)
    with DBComponents
    with HikariCPComponents
    with AssetsComponents {

  val log: Logger = Logger(this.getClass)

  lazy val default = wire[Default]

  lazy val router: Routes = {
    val prefix = "/"
    wire[Routes] //replace it with constructor if you do manual DI
  }

  override lazy val actorSystem: ActorSystem = ActorSystem("pokerActorSystem")
  lazy val maybeRouter: Option[Router] = Option(router)
  override lazy val httpErrorHandler = wire[ProdErrorHandler]
  override lazy val httpFilters = Seq()

  //Common Controllers
  override lazy val controllerComponents = wire[DefaultControllerComponents]

  //Micro Service 0  - Game Service
  lazy val appController = wire[AppController]
  lazy val apiController = wire[AppApiController]
  lazy val gameService: GameService = wire[GameService]
  lazy val playerDao: PlayerDao = wire[PlayerDao]
  lazy val logDao = wire[LogDao]

  //Micro Service 1 - Poker Hold'em
  lazy val pokerController = wire[PokerController]
  lazy val serverServicesDao: ServerServicesDao = wire[ServerServicesDao]
  lazy val pokerDao: PokerDao = wire[PokerDao]


  applicationLifecycle.addStopHook { () =>
    Future.successful(())
  }

  val onStart: Unit = {
    log.logger.info("AppLoader onStart")

    gameService.init()
  }
}

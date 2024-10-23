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
package actors.holdem

import akka.actor.{Actor, ActorRef, Props, Stash, Timers}
import akka.event.{Logging, LoggingAdapter}
import play.api.libs.json.Json

import scala.concurrent.duration._
import scala.language.postfixOps
import scala.collection.mutable.{Map => MMap}
import java.time.Instant
import services.GameService
import actors.MainActor._
import actors.holdem.PokerTable._
import model.common.messages.{AdminClientData, ClientData, GameTransaction, MoneyTransaction, Player, RoundTransactionMsg}
import model.poker.codecs.{AdminCodecs, PokerCodecs}
import model.poker.data._
import model.poker.data.admin.AdminInitialDataMsg
import model.poker.{Card, Deck52, Hand}
import play.api.Logger

import java.nio.charset.{Charset, StandardCharsets}
import java.text.SimpleDateFormat
import java.util.Calendar

object PokerTable {
  val name = "poker-table-actor"
  val path = s"/usr/$name"

  case class PlayerConnected(name: String, actor: ActorRef, client: ActorRef, clientType: String = "web")

  case class PlayerDisConnected(name: String)

  case class TopperConnected(name: String, actor: ActorRef, client: ActorRef)

  case class TopperDisConnected(name: String)

  case class AdminConnected(name: String, actor: ActorRef, client: ActorRef)

  case class AdminDisConnected(name: String)

  case class AdminMoneyTransaction(moneyTransaction: MoneyTransaction, actor: ActorRef, client: ActorRef)

  case class TableSettingsChange(pokerVariant: String,
                                 betLimit: String,
                                 liveDealer: Boolean,
                                 tournamentMode: Boolean,
                                 playerCardsConfig: CardsConfig,
                                 communityCardsConfig: CardsConfig,
                                 rakePercent: Int,
                                 blind: Int )

  case object Reconnected

  case object StartElection

  case object SkipElection

  case object GameCancel

  case object ReElection

  case object ContinueToNextRound

  case object DrawCards

  case object DrawCard

  case object BurnCard

  case class PlayerBetPlaced(name: String, betList: Seq[Bet], client: ActorRef)


  case object AllInHandCmd

  case class AllInCmd(uid: String)

  case object FoldHandCmd

  case class FoldCmd(uid: String)

  case object CheckHandCmd

  case class CheckCmd(uid: String)

  case object RaiseHandCmd

  case class RaiseCmd(uid: String, amount: Int)

  case object BetHandCmd

  case class BetCmd(uid: String, amount: Int)

  case object CallHandCmd

  case class CallCmd(uid: String)

  case object SettleGame

  case object NewGameCmd

  case class CardDrawn(card: Card)

  case object TimerKey
  case object PollTick
  case object Open

  def props(mainActor: ActorRef, gameService: GameService): Props =
    Props(new PokerTable(gameService, "4000"))
}

class PokerTable(gameService: GameService, tableId: String = "4000") extends Actor
  with Stash
  with PokerUtilities
  with PokerCodecs
  with AdminCodecs
  with Timers {

  import context._

  private val log: LoggingAdapter = Logging(context.system, this)
  val logManagerActor: ActorRef = gameService.getLoggingActor

  private val deck52 = Deck52
  private var clients: MMap[String, ClientData] = MMap.empty[String, ClientData]
  private var toppers: MMap[String, ClientData] = MMap.empty[String, ClientData]
  private var admins: MMap[String, AdminClientData] = MMap.empty[String, AdminClientData]

  private val PRE_FLOP_ROUND = "pre-flop"
  private val FLOP_ROUND = "flop"
  private val TURN_ROUND = "turn"
  private val RIVER_ROUND = "river"


  override def preStart(): Unit = {
    log.info("Poker Table Pre Start...")
    super.preStart()
    timers.startSingleTimer(TimerKey, Open, timeout = 1 second)  }

  override def postStop(): Unit = {
    log.info("Poker Table Stopped...")
    super.postStop()
  }

  override def receive: Receive = TABLE_STATE_0_BOOTING()


  /** *******************************************************************************************************
    *
    *
    *
    *
    *
    *
    */


  /** ******************************** TABLE_STATE_0_BOOTING   ********************************************* */

  def TABLE_STATE_0_BOOTING(): Receive = {
    case Open =>
      log.info(s"Open Received, in TABLE_STATE_0_BOOTING ")
      timers.startTimerWithFixedDelay(TimerKey, PollTick, 500 millis)

      /*
      * 1. Read TableState from DB
      * 2. Read Players from DB
      * 3.
      * */
      val stateRecovered: TableState = gameService.getTableState("4000")
      val playersRecovered = gameService.getPlayersData("4000")

      val playersBalance: Map[String, Double] = playersRecovered.map(p => p.uid -> p.balance).toMap
      val playersNames: Map[String, String] = playersRecovered.map(p => p.uid -> p.nickname).toMap
      val playersIps: Map[String, String] = playersRecovered.map(p => p.uid -> p.clientIp).toMap

      val recoveredData = stateRecovered
        .copy(
          seats = stateRecovered.seats
            .map(seat => seat.copy(connected = false))
            .map(seat => seat.copy(name = playersNames.getOrElse(seat.uid, "")))
            .map(seat => seat.copy(ip = playersIps.getOrElse(seat.uid, ""))),
          action = s"recovering into stage S:${stateRecovered.stage}"
        )

      log.info(s"Recovering into" +
        s"[${recoveredData.seats.map(_.balance.toInt).mkString(",")}]/" +
        s"/R(${recoveredData.roundId})/")


      unstashAll();

      recoveredData.stage match {
        case "1" =>
          val numPlayers = stateRecovered.seats.filter(s => s.balance >= stateRecovered.configData.blind * 2).map(seat => seat.id)
          val updatedData = recoveredData.copy(
            numPlayers = numPlayers,
            dealerId = -1,
            turnId = 0,
            seats = recoveredData.seats.map(seat =>
              if(numPlayers.contains(seat.id)) seat.copy(isPlaying = true, gameStatus = "Playing")
              else seat.copy(isPlaying = false, gameStatus = "Sit Out")),
            action = s"S:${stateRecovered.stage}"

          )
          updatedData.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          become(TABLE_STATE_1_READY(data = updatedData))
        case "2" =>
          become(TABLE_STATE_1_READY(data = recoveredData.prepareForFreshRound()))
        case "3" =>
          recoveredData.printTablePlayersStartState()
          recoveredData.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          become(TABLE_ELECTION_END_STATE_3(data = recoveredData))
        case "4" =>

          val finalState = recoveredData.copy(
            seats = recoveredData.seats.map(seat => seat.copy(cards = Seq.empty[String]))//reset the cards, since flop cards will be drawn again
          )
          finalState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          become(TABLE_HOLE_CARDS_DRAWING_STATE_4(data = finalState))
        case "5" =>
          recoveredData.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          become(TABLE_PRE_FLOP_BETTING_STATE_5(data = recoveredData))
        case "6" =>
          recoveredData.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          become(TABLE_PRE_FLOP_BETTING_CONFIRM_STATE_6(data = recoveredData))
        case "7" =>
          recoveredData.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          become(TABLE_FLOP_CARDS_DRAWING_STATE_7(data = recoveredData))
        case "8" =>
          recoveredData.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          become(TABLE_FLOP_BETTING_STATE_8(data = recoveredData))
        case "9" =>
          recoveredData.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          become(TABLE_FLOP_BETTING_CONFIRM_STATE_9(data = recoveredData))
        case "10" =>
          recoveredData.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          become(TABLE_TURN_CARD_DRAWING_STATE_10(data = recoveredData))
        case "11" =>
          recoveredData.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          become(TABLE_TURN_BETTING_STATE_11(data = recoveredData))
        case "12" =>
          recoveredData.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          become(TABLE_TURN_BETTING_CONFIRM_STATE_12(data = recoveredData))
        case "13" =>
          recoveredData.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          become(TABLE_RIVER_CARD_DRAWING_STATE_13(data = recoveredData))
        case "14" =>
          recoveredData.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          become(TABLE_RIVER_BETTING_STATE_14(data = recoveredData))
        case "15" =>
          recoveredData.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          if(recoveredData.configData.pokerVariant == "Texas"){
            become(TABLE_RIVER_BETTING_CONFIRM_STATE_15(data = recoveredData))
          } else {
            become(TABLE_RIVER_BETTING_CONFIRM_STATE_155(data = recoveredData))
          }
        case "16" =>
          recoveredData.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          become(TABLE_SHOWDOWN_STATE_16(data = recoveredData))
        case "17" =>
          val updatedSeatsWithMucking = recoveredData.seats.map(seat =>
            if(!seat.isPlaying) seat //Skip the sitOut players
            else {
              seat.copy(cards = seat.cards.map(_ => "xx"))
            }
          )
          val updatedStateWithMucking = recoveredData.copy(seats = updatedSeatsWithMucking)

          recoveredData.sendStateUpdateToClients(gameService.getMainActor, toppers = MMap.empty[String, ClientData], clients = clients, admins = MMap.empty[String, AdminClientData])
          updatedStateWithMucking.sendStateUpdateToClients(gameService.getMainActor, toppers = toppers , clients = MMap.empty[String, ClientData], admins = admins)

          become(TABLE_SHOWDOWN_KO_STATE_17(data = recoveredData))
        case "18" =>
          recoveredData.sendStateUpdateToClients(gameService.getMainActor, toppers = toppers, clients, admins = admins)
          become(TABLE_STATE_18_WINNER_SHOWDOWN(data = recoveredData))
        case _ =>
          recoveredData.prepareForFreshRound().sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          become(TABLE_STATE_1_READY(data = recoveredData.prepareForFreshRound()))
      }

    case PollTick =>
    case _ => {
      stash()
      log.error("message received in TABLE_BOOTING_STATE state")
    }
  }

  /** *******************************************************************************************************
    *
    *
    *
    *
    *
    *
    */


  /** ******************************** TABLE_STATE_1_READY   ********************************************* */

  def TABLE_STATE_1_READY(data: TableState): Receive = {
    case AdminMoneyTransaction(moneyTransaction, actor, client) => gameService.getMainActor ! PlayerMoneyTransaction(moneyTransaction, actor, client, admins)
    case TableSettingsChange(pokerVariant, betLimit, liveDealer, tournamentMode, playerCardsConfig, communityCardsConfig, rakePercent, blind) => context.become(TABLE_STATE_1_READY(data = handleTableSettingsChange(data, pokerVariant, betLimit, liveDealer, tournamentMode, playerCardsConfig, communityCardsConfig, rakePercent, blind)))
    case SeatBalanceUpdated(uid, balance)               => context.become(TABLE_STATE_1_READY(data = handlePlayerBalanceUpdated(data, uid, balance)))

    case PlayerConnected(name, actor, client, clientType)           => context.become(TABLE_STATE_1_READY(data = handlePlayerConnected(data, name, actor, client, clientType)))
    case TopperConnected(name, actor, client)           => context.become(TABLE_STATE_1_READY(data = handleTopperConnected(data, name, actor, client)))
    case AdminConnected(name, actor, client)            => context.become(TABLE_STATE_1_READY(data = handleAdminConnected(data, name, actor, client)))
    case PlayerDisConnected(playerIp)                   => context.become(TABLE_STATE_1_READY(data = handlePlayerDisconnected(data, playerIp)))
    case TopperDisConnected(topperIp)                   => context.become(TABLE_STATE_1_READY(data = handleTopperDisconnected(data, topperIp)))
    case AdminDisConnected(adminIp)                     => context.become(TABLE_STATE_1_READY(data = handleAdminDisconnected(data, adminIp)))

    case SkipElection                                   =>
      val updatedData = reloadPlayerBalances(tableState = data)
      val numPlayers = updatedData.numPlayers

      if (numPlayers.size >= 2) {
        context.become(TABLE_ELECTION_END_STATE_3(data = handleSkipElection(tableState = data)))
      }

    case StartElection =>
      deck52.reShuffle()
      deck52.shuffleCards()

      val updatedData = reloadPlayerBalances(tableState = data)
      val numPlayers = updatedData.numPlayers

      if (numPlayers.size >= 2) {
        val finalData = updatedData
          .prepareForFreshRound()
          .copy(stage = "2", turnId = 0, dealerId = -1, action = s"into election process...")

        finalData.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
        context.become(TABLE_ELECTION_STARTED_STATE_2(data = finalData, numPlayers.size))
      }

    case PollTick =>
    case _ => log.error("message not handled in TABLE_STATE_1_READY state")
  }

  /** *******************************************************************************************************
    *
    *
    *
    *
    *
    *
    */


  /** ******************************** TABLE_ELECTION_STARTED_STATE_2   ********************************************* */

  def TABLE_ELECTION_STARTED_STATE_2(data: TableState, playersCount: Int): Receive = {
    case PlayerConnected(name, actor, client, clientType) => context.become(TABLE_ELECTION_STARTED_STATE_2(data = handlePlayerConnected(data, name, actor, client, clientType), playersCount))
    case TopperConnected(name, actor, client) => context.become(TABLE_ELECTION_STARTED_STATE_2(data = handleTopperConnected(data, name, actor, client), playersCount))
    case AdminConnected(name, actor, client) => context.become(TABLE_ELECTION_STARTED_STATE_2(data = handleAdminConnected(data, name, actor, client), playersCount))
    case PlayerDisConnected(playerIp) => context.become(TABLE_ELECTION_STARTED_STATE_2(data = handlePlayerDisconnected(data, playerIp), playersCount))
    case TopperDisConnected(topperIp) => context.become(TABLE_ELECTION_STARTED_STATE_2(data = handleTopperDisconnected(data, topperIp), playersCount))
    case AdminDisConnected(adminIp) => context.become(TABLE_ELECTION_STARTED_STATE_2(data = handleAdminDisconnected(data, adminIp), playersCount))

    case PollTick =>
      if(data.configData.liveDealer) {
        //IGNORE PollTick as it is live dealer mode
        if(playersCount == 0) {
          val dealer = data.detectDealerSeatId()
          val dealerIndex = data.numPlayers.indexWhere(p => p == dealer)

          val (dealerPos, smallBetPos, bigBetPos, initialTurnPos) = data.getPlayerPositions(dealerIndex)
          val dealerId = data.numPlayers(dealerPos)
          val smallBetId = data.numPlayers(smallBetPos)
          val bigBetId = data.numPlayers(bigBetPos)

          val updatedSeats = data.fillSeatsWithElectionResult(dealerId, smallBetId, bigBetId)

          val updatedData = data.copy(
            dealerId = dealerId,
            smallBetId = smallBetId,
            bigBetId = bigBetId,
            turnId = smallBetPos,
            seats = updatedSeats,
          )
            .copy(action = s"election over, Seat${dealerId + 1} is winner")
            .copy(stage = "3")

          updatedData.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)

          updatedData.printTablePlayersStartState()
          become(TABLE_ELECTION_END_STATE_3(data = updatedData))
        }
      } else {
        if (playersCount > 0) {
          val updatedData = data.drawCardAndUpdateSeat(deck52)

          updatedData.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          become(TABLE_ELECTION_STARTED_STATE_2(data = updatedData, playersCount = playersCount - 1))
        }
        else {
          val dealer = data.detectDealerSeatId()
          val dealerIndex = data.numPlayers.indexWhere(p => p == dealer)

          val (dealerPos, smallBetPos, bigBetPos, initialTurnPos) = data.getPlayerPositions(dealerIndex)
          val dealerId = data.numPlayers(dealerPos)
          val smallBetId = data.numPlayers(smallBetPos)
          val bigBetId = data.numPlayers(bigBetPos)

          val updatedSeats = data.fillSeatsWithElectionResult(dealerId, smallBetId, bigBetId)

          val updatedData = data.copy(
            dealerId = dealerId,
            smallBetId = smallBetId,
            bigBetId = bigBetId,
            turnId = smallBetPos,
            seats = updatedSeats,
          )
            .copy(action = s"election over, Seat${dealerId + 1} is winner")
            .copy(stage = "3")

          updatedData.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)

          updatedData.printTablePlayersStartState()
          become(TABLE_ELECTION_END_STATE_3(data = updatedData))
        }
      }
    case CardDrawn(card: Card) =>
      if(data.configData.liveDealer) {
        if(playersCount > 0) {
          //There are still players to receive cards
          val updatedData = data.handleCardAndUpdateSeat(card.toString)

          updatedData.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          become(TABLE_ELECTION_STARTED_STATE_2(data = updatedData, playersCount = playersCount - 1))
        }
      } else {
        log.error(("Check your configuration, cards are being delivered on RNG setup"))
      }

    case GameCancel =>
      val updatedData = data
        .recallBetsWithBetLists()
        .prepareForFreshRound()

      updatedData.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
      become(TABLE_STATE_1_READY(data = updatedData))
    case _ => log.error("message received in TABLE_ELECTION_STARTED_STATE_2 state")
  }

  /** *******************************************************************************************************
    *
    *
    *
    *
    *
    *
    */


  /** ******************************** TABLE_ELECTION_END_STATE_3   ********************************************* */

  def TABLE_ELECTION_END_STATE_3(data: TableState): Receive = {
    case PlayerConnected(name, actor, client, clientType) => context.become(TABLE_ELECTION_END_STATE_3(data = handlePlayerConnected(data, name, actor, client, clientType)))
    case TopperConnected(name, actor, client) => context.become(TABLE_ELECTION_END_STATE_3(data = handleTopperConnected(data, name, actor, client)))
    case AdminConnected(name, actor, client) => context.become(TABLE_ELECTION_END_STATE_3(data = handleAdminConnected(data, name, actor, client)))
    case PlayerDisConnected(playerIp) => context.become(TABLE_ELECTION_END_STATE_3(data = handlePlayerDisconnected(data, playerIp)))
    case TopperDisConnected(topperIp) => context.become(TABLE_ELECTION_END_STATE_3(data = handleTopperDisconnected(data, topperIp)))
    case AdminDisConnected(adminIp) => context.become(TABLE_ELECTION_END_STATE_3(data = handleAdminDisconnected(data, adminIp)))

    case PollTick =>
      self ! DrawCards

    case DrawCards =>
      deck52.reShuffle()
      deck52.shuffleCards()

      val dealerIndex = data.numPlayers.indexWhere(p => p == data.dealerId)

      val (dealerPos, smallBetPos, bigBetPos, initialTurnPos) = data.getPlayerPositions(dealerIndex)
      val dealerId = data.numPlayers(dealerPos)
      val smallBetId = data.numPlayers(smallBetPos)
      val bigBetId = data.numPlayers(bigBetPos)

      val updatedSeats = data.fillSeatsWithElectionResult(dealerId, smallBetId, bigBetId)

      val updatedData = data.copy(
          dealerId = dealerId,
          smallBetId = smallBetId,
          bigBetId = bigBetId,
          turnId = smallBetPos,
          seats = updatedSeats
        )
        .clearSeatCards()
        .startRoundWithBlindBets()
        .copy(action = s"shuffled deck, into hole cards draw, first daw to pos=${smallBetPos}  seatId=${data.numPlayers(smallBetPos) + 1} ")
        .copy(stage = "4")

      updatedData.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
      val numHoleCards: Int = data.configData.pokerVariant match {
        case "Texas" =>  2
        case "Omaha" =>  4
        case "Pinapple" => 3
        case _ => 2
      }
      become(TABLE_HOLE_CARDS_DRAWING_STATE_4(data = updatedData, count = data.numPlayers.size * numHoleCards))

    case GameCancel =>
      val updatedData = data
        .recallBetsWithBetLists()
        .prepareForFreshRound()

      updatedData.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
      become(TABLE_STATE_1_READY(data = updatedData))
    case _ => log.error("some message received in TABLE_ELECTION_END_STATE_3 state")
  }

  /** *******************************************************************************************************
    *
    *
    *
    *
    *
    *
    */


  /** ******************************** TABLE_HOLE_CARDS_DRAWING_STATE_4   ********************************************* */

  def TABLE_HOLE_CARDS_DRAWING_STATE_4(data: TableState, count: Int = -1): Receive = {
    case PlayerConnected(name, actor, client, clientType) => context.become(TABLE_HOLE_CARDS_DRAWING_STATE_4(data = handlePlayerConnected(data, name, actor, client, clientType), count))
    case TopperConnected(name, actor, client) => context.become(TABLE_HOLE_CARDS_DRAWING_STATE_4(data = handleTopperConnected(data, name, actor, client), count))
    case AdminConnected(name, actor, client) => context.become(TABLE_HOLE_CARDS_DRAWING_STATE_4(data = handleAdminConnected(data, name, actor, client), count))
    case PlayerDisConnected(playerIp) => context.become(TABLE_HOLE_CARDS_DRAWING_STATE_4(data = handlePlayerDisconnected(data, playerIp), count))
    case TopperDisConnected(topperIp) => context.become(TABLE_HOLE_CARDS_DRAWING_STATE_4(data = handleTopperDisconnected(data, topperIp), count))
    case AdminDisConnected(adminIp) => context.become(TABLE_HOLE_CARDS_DRAWING_STATE_4(data = handleAdminDisconnected(data, adminIp), count))


    case PollTick =>
      if(data.configData.liveDealer) {
        if (count == 0) {
          /* Hole Cards Draw End */
          /* Select the first betting candidate for PRE_FLOP_ROUND betting*/
          val dealerIndex = data.numPlayers.indexWhere(p => p == data.dealerId)
          val (dealerPos, smallBetPos, bigBetPos, initialTurnPos) = data.getPlayerPositions(dealerIndex)

          val currentTurn = bigBetPos
          val nextTurn = data.getNextTurn(currentTurn)
          val nextSeatId = data.numPlayers(nextTurn)

          val updatedState = data
            .prepareForMatchingBet(nextSeatId, round = PRE_FLOP_ROUND)
            .copy(turnId = nextTurn)
            .copy(stage = "5")

          updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          context.become(TABLE_PRE_FLOP_BETTING_STATE_5(data = updatedState))

        }
      } else {
        if (count > 0) {
          val updatedData = data.drawHoleCard(deck52)
          updatedData.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          context.become(TABLE_HOLE_CARDS_DRAWING_STATE_4(data = updatedData, count = count - 1))
        } else if (count == 0) {
          /* Hole Cards Draw End */
          /* Select the first betting candidate for PRE_FLOP_ROUND betting*/
          val dealerIndex = data.numPlayers.indexWhere(p => p == data.dealerId)
          val (dealerPos, smallBetPos, bigBetPos, initialTurnPos) = data.getPlayerPositions(dealerIndex)

          val currentTurn = bigBetPos
          val nextTurn = data.getNextTurn(currentTurn)
          val nextSeatId = data.numPlayers(nextTurn)

          val updatedState = data
            .prepareForMatchingBet(nextSeatId, round = PRE_FLOP_ROUND)
            .copy(turnId = nextTurn)
            .copy(stage = "5")

          updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          context.become(TABLE_PRE_FLOP_BETTING_STATE_5(data = updatedState))

        }
      }
    case CardDrawn(card: Card) =>
      if(data.configData.liveDealer) {
        if(count> 0) {
          val updatedData = data.handleHoleCard(card.toString)
          updatedData.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          context.become(TABLE_HOLE_CARDS_DRAWING_STATE_4(data = updatedData, count = count - 1))
        }
      } else {
        log.error("Check Your configuration, cards are being delivered externally on a RNG setup")
      }

    case GameCancel =>
      val updatedData = data
        .recallBetsWithBetLists()
        .prepareForFreshRound()

      updatedData.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
      become(TABLE_STATE_1_READY(data = updatedData))
    case DrawCards =>
    case StartElection =>
    case _ => log.error("some message received in READY_FOR_HOLE_CARDS state")
  }

  /** *******************************************************************************************************
    *
    *
    *
    *
    *
    *
    */


  /** ******************************** TABLE_PRE_FLOP_BETTING_STATE_5   ********************************************* */

  def TABLE_PRE_FLOP_BETTING_STATE_5(data: TableState): Receive = {
    case PlayerConnected(name, actor, client, clientType) => context.become(TABLE_PRE_FLOP_BETTING_STATE_5(data = handlePlayerConnected(data, name, actor, client, clientType)))
    case TopperConnected(name, actor, client) => context.become(TABLE_PRE_FLOP_BETTING_STATE_5(data = handleTopperConnected(data, name, actor, client)))
    case AdminConnected(name, actor, client) => context.become(TABLE_PRE_FLOP_BETTING_STATE_5(data = handleAdminConnected(data, name, actor, client)))
    case PlayerDisConnected(playerIp) => context.become(TABLE_PRE_FLOP_BETTING_STATE_5(data = handlePlayerDisconnected(data, playerIp)))
    case TopperDisConnected(topperIp) => context.become(TABLE_PRE_FLOP_BETTING_STATE_5(data = handleTopperDisconnected(data, topperIp)))
    case AdminDisConnected(adminIp) => context.become(TABLE_PRE_FLOP_BETTING_STATE_5(data = handleAdminDisconnected(data, adminIp)))

    /* As soon as an "all-in" comes,
    *  We know that
    *   - a player send "all-in" means his balance < table.betAmount
    *
    *
    * */
    case AllInCmd(uid: String) =>
      if (data.seats(data.numPlayers(data.turnId)).uid != uid) {

      } else {

        //Step 1. Handle Command
        val currentTurn = data.turnId
        val currentSeatId = data.numPlayers(currentTurn)
        val currentSeat = data.getActivePlayers.find(_.id == currentSeatId).get

        val updatedState = data.handleAllInCmd(currentSeatId, round = PRE_FLOP_ROUND)
        updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)

        //Step 2. Analyze result State
        if (updatedState.allSeatsAllIn() || updatedState.allSeatsCalledAlready(round = PRE_FLOP_ROUND)) {

          val finalState = updatedState.copy(
            stage = "6",
            potLimit = 0,
            betAmount = 0,
            raiseAmount = 0,
            action = s"pre-flop betting round ended, as "
          )

          finalState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          context.become(TABLE_PRE_FLOP_BETTING_CONFIRM_STATE_6(data = finalState))
        } else {
          /* there are some more seats to act */
          val nextTurn = updatedState.turnId
          val nextSeatId = updatedState.numPlayers(nextTurn)
          val finalState = updatedState
            .prepareForMatchingBet(nextSeatId, round = PRE_FLOP_ROUND)

          finalState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          context.become(TABLE_PRE_FLOP_BETTING_STATE_5(data = finalState))
        }
      }
    case FoldCmd(uid: String) =>
      if (data.seats(data.numPlayers(data.turnId)).uid != uid) {

      } else {
        val currentTurn = data.turnId

        val updatedState = data
          .handleFoldCmd(data.numPlayers(data.turnId), round = PRE_FLOP_ROUND)
        updatedState
          .sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)

        //Knockout champion ??
        if (updatedState.knockOutWinnerSkipAll()) {
          val finalState = updatedState.handleKnockout()
//          val finalSeatsWithMucking = finalState.seats.map(seat => if (!seat.isPlaying) seat else seat.copy(cards = Seq("xx", "xx")))
          val finalSeatsWithMucking = finalState.seats.map(seat => if (!seat.isPlaying) seat else seat.copy(cards = seat.cards.map(_ => "xx")))
          val finalStateWithMucking = finalState.copy(seats = finalSeatsWithMucking)

          finalState.sendStateUpdateToClients(gameService.getMainActor, toppers = MMap.empty[String, ClientData], clients = clients, admins = MMap.empty[String, AdminClientData])
          finalStateWithMucking.sendStateUpdateToClients(gameService.getMainActor, toppers = toppers, clients = MMap.empty[String, ClientData], admins = admins )

          context.become(TABLE_SHOWDOWN_KO_STATE_17(data = finalState))

        } else if (updatedState.allSeatsAllIn() || updatedState.allSeatsCalledAlready(round = PRE_FLOP_ROUND)) {
          val finalState = updatedState.copy(
            stage = "6",
            potLimit = 0,
            betAmount = 0,
            raiseAmount = 0,
            action = s"R1 betting end, all matched the highest bet ${updatedState.highestAmountInRound(PRE_FLOP_ROUND)}"
          )

          finalState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          context.become(TABLE_PRE_FLOP_BETTING_CONFIRM_STATE_6(data = finalState))

        } else {
          //No knockout champion
          //Not All the active players [numPlayers] matched Highest Bet
          val nextSeatId = updatedState.numPlayers(updatedState.turnId)

          val finalState = updatedState
            .prepareForMatchingBet(nextSeatId, round = PRE_FLOP_ROUND)

          finalState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          context.become(TABLE_PRE_FLOP_BETTING_STATE_5(data = finalState))
        }
      }
    case CheckCmd(uid: String) =>
      if (data.seats(data.numPlayers(data.turnId)).uid != uid) {

      } else {
        val currentTurn = data.turnId

        val updatedStateAfterCheck = data
          .handleCheckCmd(data.numPlayers(currentTurn), round = PRE_FLOP_ROUND)
        updatedStateAfterCheck
          .sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)

        //NOTE: this is only applicable to first round
        //In first round only bb will get a chance to check, once it is done its end of round
        val updatedState = updatedStateAfterCheck.copy(
          stage = "6",
          potLimit = 0,
          betAmount = 0,
          raiseAmount = 0,
          action = s"R1 betting end, all matched the highest bet ${updatedStateAfterCheck.highestAmountInRound(PRE_FLOP_ROUND).toInt}"
        )

        updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
        context.become(TABLE_PRE_FLOP_BETTING_CONFIRM_STATE_6(data = updatedState))
      }
    case RaiseCmd(uid, amount) =>
      if (data.seats(data.numPlayers(data.turnId)).uid != uid) {

      } else {
        val currentTurn = data.turnId

        val updatedStateAfterRaise = data
          .handleRaiseCmd(data.numPlayers(data.turnId), wagerAmount = amount, round = PRE_FLOP_ROUND)
        updatedStateAfterRaise
          .sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)

        val nextTurn = updatedStateAfterRaise.getNextTurn(currentTurn)
        val nextSeatId = updatedStateAfterRaise.numPlayers(nextTurn)
        val updatedState = updatedStateAfterRaise
          .prepareForMatchingBet(nextSeatId, round = PRE_FLOP_ROUND)
          .copy(turnId = nextTurn)

        updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
        context.become(TABLE_PRE_FLOP_BETTING_STATE_5(data = updatedState))
      }
    case BetCmd(uid, amount) =>
      if (data.seats(data.numPlayers(data.turnId)).uid != uid) {

      } else {
        val currentTurn = data.turnId

        val updatedStateAfterBet = data
          .handleBetCmd(data.numPlayers(data.turnId), wagerAmount = amount, round = PRE_FLOP_ROUND)
        updatedStateAfterBet
          .sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)

        val nextTurn = updatedStateAfterBet.getNextTurn(currentTurn)
        val nextSeatId = updatedStateAfterBet.numPlayers(nextTurn)
        val updatedState = updatedStateAfterBet
          .prepareForMatchingBet(nextSeatId, round = PRE_FLOP_ROUND)
          .copy(turnId = nextTurn)

        updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
        context.become(TABLE_PRE_FLOP_BETTING_STATE_5(data = updatedState))
      }
    case CallCmd(uid: String) =>
      if(data.seats(data.numPlayers(data.turnId)).uid != uid ) {

      } else {
        val currentTurn = data.turnId

        val updatedStateAfterCallBet = data
          .handleCallHandCmd(data.numPlayers(data.turnId), round = PRE_FLOP_ROUND)
        updatedStateAfterCallBet
          .sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)

        if (!updatedStateAfterCallBet.allActivePlayersMatchedHighestBet(round = PRE_FLOP_ROUND)) {
          /* Not All the active players [numPlayers] matched Highest Bet*/
          val nextTurn = updatedStateAfterCallBet.getNextTurn(currentTurn)
          val nextSeatId = updatedStateAfterCallBet.numPlayers(nextTurn)
          val updatedState = updatedStateAfterCallBet
            .prepareForMatchingBet(nextSeatId, round = PRE_FLOP_ROUND)
            .copy(turnId = nextTurn)

          updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          context.become(TABLE_PRE_FLOP_BETTING_STATE_5(data = updatedState))

        } else {


          if (data.highestAmountInRound(PRE_FLOP_ROUND) == data.configData.blind) {
            //In first round, the player posted bb will be given a chance to raise if bb bet is the highest bet at the end of round
            val nextTurn = updatedStateAfterCallBet.getNextTurn(currentTurn)
            val nextSeatId = updatedStateAfterCallBet.numPlayers(nextTurn)
            val updatedState = updatedStateAfterCallBet
              .prepareForBet(nextSeatId, round = PRE_FLOP_ROUND)
              .copy(turnId = nextTurn)
            updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
            context.become(TABLE_PRE_FLOP_BETTING_STATE_5(data = updatedState))
          } else {
//            log.info("Please Handle Betting Round End Now!!!")
            val updatedState = updatedStateAfterCallBet.copy(
              stage = "6",
              potLimit = 0,
              betAmount = 0,
              raiseAmount = 0,
              action = s"R1 betting end, all matched the highest bet ${updatedStateAfterCallBet.highestAmountInRound(PRE_FLOP_ROUND)}"
            )

            updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
            context.become(TABLE_PRE_FLOP_BETTING_CONFIRM_STATE_6(data = updatedState))
          }

        }
      }

    case GameCancel =>
      val updatedData = data
        .recallBetsWithBetLists()
        .prepareForFreshRound()

      updatedData.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
      become(TABLE_STATE_1_READY(data = updatedData))
    case PollTick =>
    case _ => log.error("some message received in TABLE_PRE_FLOP_BETTING_STATE_5 state")
  }

  def TABLE_PRE_FLOP_BETTING_CONFIRM_STATE_6(data: TableState): Receive = {
    case PlayerConnected(name, actor, client, clientType) => context.become(TABLE_PRE_FLOP_BETTING_CONFIRM_STATE_6(data = handlePlayerConnected(data, name, actor, client, clientType)))
    case TopperConnected(name, actor, client) => context.become(TABLE_PRE_FLOP_BETTING_CONFIRM_STATE_6(data = handleTopperConnected(data, name, actor, client)))
    case AdminConnected(name, actor, client) => context.become(TABLE_PRE_FLOP_BETTING_CONFIRM_STATE_6(data = handleAdminConnected(data, name, actor, client)))
    case PlayerDisConnected(playerIp) => context.become(TABLE_PRE_FLOP_BETTING_CONFIRM_STATE_6(data = handlePlayerDisconnected(data, playerIp)))
    case TopperDisConnected(topperIp) => context.become(TABLE_PRE_FLOP_BETTING_CONFIRM_STATE_6(data = handleTopperDisconnected(data, topperIp)))
    case AdminDisConnected(adminIp) => context.become(TABLE_PRE_FLOP_BETTING_CONFIRM_STATE_6(data = handleAdminDisconnected(data, adminIp)))

    case PollTick =>
      self ! ContinueToNextRound

    case ContinueToNextRound =>
      if(data.sidePots.isEmpty) {
        val amountToPot = data.seats.map(seat => seat.bets.head).fold(0.0)(_ + _)

        //things to do
        //1.  move bets.head to pre-flop position(1) and reset to 0
        val updatedSeats = data.seats.map(seat => seat.seatSettleRoundBets("PreFlop"))

        val updatedState = data.copy(
          stage = "7",
          potAmount = data.potAmount + amountToPot,
          seats = updatedSeats,
          turnId = 0, //turn as gamecards pos
          potLimit = 0,
          betAmount = 0,
          action = s"into flop cards draw,"
        )
        updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
        context.become(TABLE_FLOP_CARDS_DRAWING_STATE_7(data = updatedState))
      } else {

        val totalSidePotAmount = data.sidePots.map(sp => sp.ids.size* sp.capAmount).sum
        val numPlayerBetAmount = data.numPlayers.map((player) => data.seats(player).betList.map(_.betValue).sum).sum
        val highestSidePotCap = data.sidePots.map(sp => sp.capAmount).sortWith((a,b) => a < b).last


        if(data.allSeatsAllIn()) {
          /*All seats are All In */

          val updatedState = data.copy(
            stage = "7",
            potAmount = totalSidePotAmount + numPlayerBetAmount,
            turnId = 0, //turn as gamecards pos
            potLimit = 0,
            betAmount = 0,
            action = s"into flop cards draw, all seats are all-in"
          )
          updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          context.become(TABLE_FLOP_CARDS_DRAWING_STATE_7(data = updatedState))
        } else {
          //some active players exists

          val updatedState = data.copy(
            stage = "7",
            potAmount = totalSidePotAmount + numPlayerBetAmount,
            turnId = 0, //turn as gamecards pos
            potLimit = 0,
            betAmount = 0,
            action = s"into flop cards draw, all seats are all-in"
          )
          updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          context.become(TABLE_FLOP_CARDS_DRAWING_STATE_7(data = updatedState))

        }
      }


    case GameCancel =>
      val updatedData = data
        .recallBetsWithBetLists()
        .prepareForFreshRound()
      updatedData.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)

      become(TABLE_STATE_1_READY(data = updatedData))


    case _ => log.error("some message received in TABLE_PRE_FLOP_BETTING_CONFIRM_STATE_6 state")
  }

  def TABLE_FLOP_CARDS_DRAWING_STATE_7(data: TableState, count: Int = -1): Receive = {
    case PlayerConnected(name, actor, client, clientType) => context.become(TABLE_FLOP_CARDS_DRAWING_STATE_7(data = handlePlayerConnected(data, name, actor, client, clientType), count))
    case TopperConnected(name, actor, client) => context.become(TABLE_FLOP_CARDS_DRAWING_STATE_7(data = handleTopperConnected(data, name, actor, client), count))
    case AdminConnected(name, actor, client) => context.become(TABLE_FLOP_CARDS_DRAWING_STATE_7(data = handleAdminConnected(data, name, actor, client), count))
    case PlayerDisConnected(playerIp) => context.become(TABLE_FLOP_CARDS_DRAWING_STATE_7(data = handlePlayerDisconnected(data, playerIp), count))
    case TopperDisConnected(topperIp) => context.become(TABLE_FLOP_CARDS_DRAWING_STATE_7(data = handleTopperDisconnected(data, topperIp), count))
    case AdminDisConnected(adminIp) => context.become(TABLE_FLOP_CARDS_DRAWING_STATE_7(data = handleAdminDisconnected(data, adminIp), count))


    case PollTick =>
      if (count == -1) {
        self ! DrawCards
      }
      if (count > 0) {
        if (!data.configData.liveDealer) {
          val updatedData = data.drawCommunityCard(deck52)
          updatedData.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          context.become(TABLE_FLOP_CARDS_DRAWING_STATE_7(data = updatedData, count = count - 1))
        }
      } else if (count == 0) {
        /* Flop Cards Draw End */

        /*Skip betting cases */
        if(data.allSeatsAllIn() || data.allInWinnerSkipBetting()) {

          val updatedSeats = data.seats.map(seat => seat.seatSettleRoundBets("Flop"))

          val updatedState = data.copy(
            stage = "10",
            seats = updatedSeats,
            turnId = 3, //turn as gamecards pos
            potLimit = 0,
            betAmount = 0,
            raiseAmount = 0,
            action = s"Skip FLOP_ROUND betting, All in"
          )

          updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          context.become(TABLE_TURN_CARD_DRAWING_STATE_10(data = updatedState))

        } else {
          /* Select the first betting candidate for FLOP_ROUND betting */
          val dealerIndex = data.numPlayers.indexWhere(p => p == data.dealerId)
          val (dealerPos, smallBetPos, bigBetPos, initialTurnPos) = data.getPlayerPositions(dealerIndex)

          val currentTurn = dealerPos
          val nextTurn = data.getNextTurn(currentTurn)
          val nextSeatId = data.numPlayers(nextTurn)

          val updatedState = data
            .prepareForCheck(nextSeatId, round = FLOP_ROUND)
            .copy(turnId = nextTurn)
            .copy(stage = "8")

          updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          context.become(TABLE_FLOP_BETTING_STATE_8(data = updatedState))
        }

      }
    case DrawCards =>
      if(count == -1) {
        data.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
        context.become(TABLE_FLOP_CARDS_DRAWING_STATE_7(data = data, count = 3))
      }

    case CardDrawn(card: Card) =>
      if (data.configData.liveDealer) {
        if (count > 0) {
          val updatedData = data.handleCommunityCard(card.toString)
          updatedData.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          context.become(TABLE_FLOP_CARDS_DRAWING_STATE_7(data = updatedData, count = count - 1))
        }
      } else {
        log.error("Check Your configuration, cards are being delivered externally on a RNG setup")
      }
    case GameCancel =>
      val updatedData = data
        .recallBetsWithBetLists()
        .prepareForFreshRound()
      updatedData.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)

      become(TABLE_STATE_1_READY(data = updatedData))
    case _ => log.error("some message received in TABLE_FLOP_CARDS_DRAWING_STATE_7 state")
  }

  def TABLE_FLOP_BETTING_STATE_8(data: TableState): Receive = {
    case PlayerConnected(name, actor, client, clientType) => context.become(TABLE_FLOP_BETTING_STATE_8(data = handlePlayerConnected(data, name, actor, client, clientType)))
    case TopperConnected(name, actor, client) => context.become(TABLE_FLOP_BETTING_STATE_8(data = handleTopperConnected(data, name, actor, client)))
    case AdminConnected(name, actor, client) => context.become(TABLE_FLOP_BETTING_STATE_8(data = handleAdminConnected(data, name, actor, client)))
    case PlayerDisConnected(playerIp) => context.become(TABLE_FLOP_BETTING_STATE_8(data = handlePlayerDisconnected(data, playerIp)))
    case TopperDisConnected(topperIp) => context.become(TABLE_FLOP_BETTING_STATE_8(data = handleTopperDisconnected(data, topperIp)))
    case AdminDisConnected(adminIp) => context.become(TABLE_FLOP_BETTING_STATE_8(data = handleAdminDisconnected(data, adminIp)))

    case AllInCmd(uid: String) =>

      if (data.seats(data.numPlayers(data.turnId)).uid != uid) {

      } else {


        //Step 1. Handle Command
        val currentTurn = data.turnId
        val currentSeatId = data.numPlayers(currentTurn)
        val currentSeat = data.getActivePlayers.find(_.id == currentSeatId).get

        val updatedState = data.handleAllInCmd(currentSeatId, round = FLOP_ROUND)
        updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)

        //Step 2. Analyze result State
        if (updatedState.allSeatsAllIn() || updatedState.allSeatsCalledAlready(round = FLOP_ROUND)) {

          val finalState = updatedState.copy(
            stage = "9",
            potLimit = 0,
            betAmount = 0,
            raiseAmount = 0,
            action = s"flop betting round ended, as "
          )

          finalState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          context.become(TABLE_FLOP_BETTING_CONFIRM_STATE_9(data = finalState))
        } else {
          /* there are some more seats to act */
          val nextTurn = updatedState.turnId
          val nextSeatId = updatedState.numPlayers(nextTurn)
          val finalState = updatedState
            .prepareForMatchingBet(nextSeatId, round = FLOP_ROUND)

          finalState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          context.become(TABLE_FLOP_BETTING_STATE_8(data = finalState))
        }

      }
    case FoldCmd(uid: String) =>
      if (data.seats(data.numPlayers(data.turnId)).uid != uid) {

      } else {
        val currentTurn = data.turnId

        val updatedState = data
          .handleFoldCmd(data.numPlayers(data.turnId), round = FLOP_ROUND)
        updatedState
          .sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)

        //Knockout champion ??
        if (updatedState.knockOutWinnerSkipAll()) {
          val finalState = updatedState.handleKnockout()
//          val finalSeatsWithMucking = finalState.seats.map(seat => if (!seat.isPlaying) seat else seat.copy(cards = Seq("xx", "xx")))
          val finalSeatsWithMucking = finalState.seats.map(seat => if (!seat.isPlaying) seat else seat.copy(cards = seat.cards.map(_ => "xx")))
          val finalStateWithMucking = finalState.copy(seats = finalSeatsWithMucking)

          finalState.sendStateUpdateToClients(gameService.getMainActor, toppers = MMap.empty[String, ClientData], clients = clients, admins = MMap.empty[String, AdminClientData])
          finalStateWithMucking.sendStateUpdateToClients(gameService.getMainActor, toppers = toppers, clients = MMap.empty[String, ClientData], admins = admins)

          context.become(TABLE_SHOWDOWN_KO_STATE_17(data = finalState))

        } else if (updatedState.allActivePlayersMatchedHighestBet(round = FLOP_ROUND)) {
//          log.info("Please Handle Betting Round End Now!!!")
          val finalState = updatedState.copy(
            stage = "9",
            potLimit = 0,
            betAmount = 0,
            raiseAmount = 0,
            action = s"R2 betting end, all matched the highest bet ${updatedState.highestAmountInRound(FLOP_ROUND)}"
          )

          finalState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          context.become(TABLE_FLOP_BETTING_CONFIRM_STATE_9(data = finalState))

        } else {
          //No knockout champion
          //Not All the active players [numPlayers] matched Highest Bet
          val nextSeatId = updatedState.numPlayers(updatedState.turnId)

          val finalState = updatedState
            .prepareForMatchingBet(nextSeatId, round = FLOP_ROUND)

          finalState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          context.become(TABLE_FLOP_BETTING_STATE_8(data = finalState))
        }
      }
    case CheckCmd(uid: String) =>
      if (data.seats(data.numPlayers(data.turnId)).uid != uid) {

      } else {
        val currentTurn = data.turnId

        val updatedStateAfterCheck = data
          .handleCheckCmd(data.numPlayers(currentTurn), round = FLOP_ROUND)
        updatedStateAfterCheck
          .sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)


        if (updatedStateAfterCheck.allSeatsCheckedAlready()) {
          val updatedState = updatedStateAfterCheck.copy(
            stage = "9",
            potLimit = 0,
            betAmount = 0,
            raiseAmount = 0,
            action = s"R2 betting end, Free Round"
          )

          updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          context.become(TABLE_FLOP_BETTING_CONFIRM_STATE_9(data = updatedState))
        } else {

          val nextTurn = updatedStateAfterCheck.getNextTurn(currentTurn)
          val nextSeatId = updatedStateAfterCheck.numPlayers(nextTurn)
          val updatedState = updatedStateAfterCheck
            .prepareForBet(nextSeatId, round = FLOP_ROUND) //TODO prepareForCheck
            .copy(turnId = nextTurn)

          updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          context.become(TABLE_FLOP_BETTING_STATE_8(data = updatedState))
        }

      }
    case RaiseCmd(uid, amount) =>
      if (data.seats(data.numPlayers(data.turnId)).uid != uid) {

      } else {
        val currentTurn = data.turnId

        val updatedStateAfterRaise = data
          .handleRaiseCmd(data.numPlayers(data.turnId), wagerAmount = amount, round = FLOP_ROUND)
        updatedStateAfterRaise
          .sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)

        val nextTurn = updatedStateAfterRaise.getNextTurn(currentTurn)
        val nextSeatId = updatedStateAfterRaise.numPlayers(nextTurn)
        val updatedState = updatedStateAfterRaise
          .prepareForMatchingBet(nextSeatId, round = FLOP_ROUND)
          .copy(turnId = nextTurn)

        updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
        context.become(TABLE_FLOP_BETTING_STATE_8(data = updatedState))
      }
    case BetCmd(uid, amount) =>
      if (data.seats(data.numPlayers(data.turnId)).uid != uid) {

      } else {
        val currentTurn = data.turnId

        val updatedStateAfterBet = data
          .handleBetCmd(data.numPlayers(data.turnId), wagerAmount = amount, round = FLOP_ROUND)
        updatedStateAfterBet
          .sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)

        val nextTurn = updatedStateAfterBet.getNextTurn(currentTurn)
        val nextSeatId = updatedStateAfterBet.numPlayers(nextTurn)
        val updatedState = updatedStateAfterBet
          .prepareForMatchingBet(nextSeatId, round = FLOP_ROUND)
          .copy(turnId = nextTurn)

        updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
        context.become(TABLE_FLOP_BETTING_STATE_8(data = updatedState))
      }
    case CallCmd(uid: String) =>
      if (data.seats(data.numPlayers(data.turnId)).uid != uid) {

      } else {


        val currentTurn = data.turnId

        val updatedStateAfterCallBet = data
          .handleCallHandCmd(data.numPlayers(data.turnId), round = FLOP_ROUND)
        updatedStateAfterCallBet
          .sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)

        if (!updatedStateAfterCallBet.allActivePlayersMatchedHighestBet(round = FLOP_ROUND)) {
          /* Not All the active players [numPlayers] matched Highest Bet*/
          val nextTurn = updatedStateAfterCallBet.getNextTurn(currentTurn)
          val nextSeatId = updatedStateAfterCallBet.numPlayers(nextTurn)
          val updatedState = updatedStateAfterCallBet
            .prepareForMatchingBet(nextSeatId, round = FLOP_ROUND)
            .copy(turnId = nextTurn)

          updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          context.become(TABLE_FLOP_BETTING_STATE_8(data = updatedState))

        } else {
//          log.info("Please Handle Betting Round End Now!!!")
          val updatedState = updatedStateAfterCallBet.copy(
            stage = "9",
            potLimit = 0,
            betAmount = 0,
            raiseAmount = 0,
            action = s"R2 betting end, all matched the highest bet ${updatedStateAfterCallBet.highestAmountInRound(FLOP_ROUND)}"
          )

          updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          context.become(TABLE_FLOP_BETTING_CONFIRM_STATE_9(data = updatedState))
        }
      }

    case GameCancel =>
      val updatedData = data
        .recallBetsWithBetLists()
        .prepareForFreshRound()

      updatedData.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
      become(TABLE_STATE_1_READY(data = updatedData))


    case PollTick =>
    case _ => log.error("some message received in TABLE_FLOP_BETTING_STATE_8 state")
  }

  def TABLE_FLOP_BETTING_CONFIRM_STATE_9(data: TableState): Receive = {
    case PlayerConnected(name, actor, client, clientType) => context.become(TABLE_FLOP_BETTING_CONFIRM_STATE_9(data = handlePlayerConnected(data, name, actor, client, clientType)))
    case TopperConnected(name, actor, client) => context.become(TABLE_FLOP_BETTING_CONFIRM_STATE_9(data = handleTopperConnected(data, name, actor, client)))
    case AdminConnected(name, actor, client) => context.become(TABLE_FLOP_BETTING_CONFIRM_STATE_9(data = handleAdminConnected(data, name, actor, client)))
    case PlayerDisConnected(playerIp) => context.become(TABLE_FLOP_BETTING_CONFIRM_STATE_9(data = handlePlayerDisconnected(data, playerIp)))
    case TopperDisConnected(topperIp) => context.become(TABLE_FLOP_BETTING_CONFIRM_STATE_9(data = handleTopperDisconnected(data, topperIp)))
    case AdminDisConnected(adminIp) => context.become(TABLE_FLOP_BETTING_CONFIRM_STATE_9(data = handleAdminDisconnected(data, adminIp)))

    case PollTick =>
      self ! ContinueToNextRound

    case ContinueToNextRound =>
      if(data.sidePots.isEmpty) {


        val amountToPot = data.seats.map(seat => seat.bets.head).fold(0.0)(_ + _)

        //things to do
        //1.  move bets.head to flop position(2) and reset to 0
        val updatedSeats = data.seats.map(seat => seat.seatSettleRoundBets("Flop"))

        val updatedState = data.copy(
          stage = "10",
          potAmount = data.potAmount + amountToPot,
          seats = updatedSeats,
          turnId = 3, //turn as gamecards pos
          potLimit = 0,
          betAmount = 0,
          action = s"into turn card draw,"
        )
        updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
        context.become(TABLE_TURN_CARD_DRAWING_STATE_10(data = updatedState))
      } else {

        val totalSidePotAmount = data.sidePots.map(sp => sp.ids.size * sp.capAmount).sum
        val numPlayerBetAmount = data.numPlayers.map((player) => data.seats(player).betList.map(_.betValue).sum).sum
        val highestSidePotCap = data.sidePots.map(sp => sp.capAmount).sortWith((a, b) => a < b).last


        if (data.allSeatsAllIn()) {
          /*All seats are All In */

          val updatedState = data.copy(
            stage = "10",
            potAmount = totalSidePotAmount + numPlayerBetAmount,
            turnId = 3, //turn as gamecards pos
            potLimit = 0,
            betAmount = 0,
            action = s"into turn card draw, "
          )
          updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          context.become(TABLE_TURN_CARD_DRAWING_STATE_10(data = updatedState))

        } else {
          val updatedState = data.copy(
            stage = "10",
            potAmount = totalSidePotAmount + numPlayerBetAmount,
            turnId = 3, //turn as gamecards pos
            potLimit = 0,
            betAmount = 0,
            action = s"into turn card draw, "
          )
          updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          context.become(TABLE_TURN_CARD_DRAWING_STATE_10(data = updatedState))
        }
      }

    case GameCancel =>
      val updatedData = data
        .recallBetsWithBetLists()
        .prepareForFreshRound()

      updatedData.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
      become(TABLE_STATE_1_READY(data = updatedData))

    case _ => log.error("some message received in TABLE_FLOP_BETTING_CONFIRM_STATE_9 state")
  }

  def TABLE_TURN_CARD_DRAWING_STATE_10(data: TableState, count: Int = -1): Receive = {
    case PlayerConnected(name, actor, client, clientType) => context.become(TABLE_TURN_CARD_DRAWING_STATE_10(data = handlePlayerConnected(data, name, actor, client, clientType), count))
    case TopperConnected(name, actor, client) => context.become(TABLE_TURN_CARD_DRAWING_STATE_10(data = handleTopperConnected(data, name, actor, client), count))
    case AdminConnected(name, actor, client) => context.become(TABLE_TURN_CARD_DRAWING_STATE_10(data = handleAdminConnected(data, name, actor, client), count))
    case PlayerDisConnected(playerIp) => context.become(TABLE_TURN_CARD_DRAWING_STATE_10(data = handlePlayerDisconnected(data, playerIp), count))
    case TopperDisConnected(topperIp) => context.become(TABLE_TURN_CARD_DRAWING_STATE_10(data = handleTopperDisconnected(data, topperIp), count))
    case AdminDisConnected(adminIp) => context.become(TABLE_TURN_CARD_DRAWING_STATE_10(data = handleAdminDisconnected(data, adminIp), count))

    case PollTick =>
      if (count == -1) {
        self ! DrawCards
      }
      if (count > 0) {
        if (!data.configData.liveDealer) {
          val updatedData = data.drawCommunityCard(deck52)
          updatedData.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          context.become(TABLE_TURN_CARD_DRAWING_STATE_10(data = updatedData, count = count - 1))
        }
      } else if (count == 0) {
        /* Turn Card Draw End */

        /*Skip betting cases */
        if(data.allSeatsAllIn() || data.allInWinnerSkipBetting()) {

          val updatedSeats = data.seats.map(seat => seat.seatSettleRoundBets("Turn"))

          val updatedState = data.copy(
            stage = "13",
            seats = updatedSeats,
            turnId = 4, //turn as gamecards pos
            potLimit = 0,
            betAmount = 0,
            raiseAmount = 0,
            action = s"Skip TURN_ROUND betting, All in"
          )

          updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          context.become(TABLE_RIVER_CARD_DRAWING_STATE_13(data = updatedState))

        } else {

          /* Select the first betting candidate for TURN_ROUND betting */
          val dealerIndex = data.numPlayers.indexWhere(p => p == data.dealerId)
          val (dealerPos, smallBetPos, bigBetPos, initialTurnPos) = data.getPlayerPositions(dealerIndex)

          val currentTurn = dealerPos
          val nextTurn = data.getNextTurn(currentTurn)
          val nextSeatId = data.numPlayers(nextTurn)

          val updatedState = data
            .prepareForCheck(nextSeatId, round = TURN_ROUND)
            .copy(turnId = nextTurn)
            .copy(stage = "11")

          updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          context.become(TABLE_TURN_BETTING_STATE_11(data = updatedState))
        }


      }
    case DrawCards =>
      if(count == -1) {
        data.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
        context.become(TABLE_TURN_CARD_DRAWING_STATE_10(data = data, count = 1))
      }
    case CardDrawn(card: Card) =>
      if (data.configData.liveDealer) {
        if (count > 0) {
          val updatedData = data.handleCommunityCard(card.toString)
          updatedData.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          context.become(TABLE_TURN_CARD_DRAWING_STATE_10(data = updatedData, count = count - 1))
        }
      } else {
        log.error("Check Your configuration, cards are being delivered externally on a RNG setup")
      }
    case GameCancel =>
      val updatedData = data
        .recallBetsWithBetLists()
        .prepareForFreshRound()

      updatedData.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
      become(TABLE_STATE_1_READY(data = updatedData))

    case _ => log.error("some message received in TABLE_TURN_CARD_DRAWING_STATE_10 state")
  }

  def TABLE_TURN_BETTING_STATE_11(data: TableState): Receive = {
    case PlayerConnected(name, actor, client, clientType) => context.become(TABLE_TURN_BETTING_STATE_11(data = handlePlayerConnected(data, name, actor, client, clientType)))
    case TopperConnected(name, actor, client) => context.become(TABLE_TURN_BETTING_STATE_11(data = handleTopperConnected(data, name, actor, client)))
    case AdminConnected(name, actor, client) => context.become(TABLE_TURN_BETTING_STATE_11(data = handleAdminConnected(data, name, actor, client)))
    case PlayerDisConnected(playerIp) => context.become(TABLE_TURN_BETTING_STATE_11(data = handlePlayerDisconnected(data, playerIp)))
    case TopperDisConnected(topperIp) => context.become(TABLE_TURN_BETTING_STATE_11(data = handleTopperDisconnected(data, topperIp)))
    case AdminDisConnected(adminIp) => context.become(TABLE_TURN_BETTING_STATE_11(data = handleAdminDisconnected(data, adminIp)))

    case AllInCmd(uid: String) =>

      if (data.seats(data.numPlayers(data.turnId)).uid != uid) {

      } else {
        //Step 1. Handle Command
        val currentTurn = data.turnId
        val currentSeatId = data.numPlayers(currentTurn)
        val currentSeat = data.getActivePlayers.find(_.id == currentSeatId).get

        val updatedState = data.handleAllInCmd(currentSeatId, round = TURN_ROUND)
        updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)

        //Step 2. Analyze result State
        if (updatedState.allSeatsAllIn() || updatedState.allSeatsCalledAlready(round = TURN_ROUND)) {

          val finalState = updatedState.copy(
            stage = "12",
            potLimit = 0,
            betAmount = 0,
            raiseAmount = 0,
            action = s"turn betting round ended, as "
          )

          finalState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          context.become(TABLE_TURN_BETTING_CONFIRM_STATE_12(data = finalState))
        } else {
          /* there are some more seats to act */
          val nextTurn = updatedState.turnId
          val nextSeatId = updatedState.numPlayers(nextTurn)
          val finalState = updatedState
            .prepareForMatchingBet(nextSeatId, round = TURN_ROUND)

          finalState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          context.become(TABLE_TURN_BETTING_STATE_11(data = finalState))
        }

      }

    case FoldCmd(uid: String) =>
      if (data.seats(data.numPlayers(data.turnId)).uid != uid) {

      } else {
        val currentTurn = data.turnId

        val updatedState = data
          .handleFoldCmd(data.numPlayers(data.turnId), round = TURN_ROUND)
        updatedState
          .sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)

        //Knockout champion ??
        if (updatedState.knockOutWinnerSkipAll()) {
          val finalState = updatedState.handleKnockout()
//          val finalSeatsWithMucking = finalState.seats.map(seat => if (!seat.isPlaying) seat else seat.copy(cards = Seq("xx", "xx")))
          val finalSeatsWithMucking = finalState.seats.map(seat => if (!seat.isPlaying) seat else seat.copy(cards = seat.cards.map(_ => "xx")))

          val finalStateWithMucking = finalState.copy(seats = finalSeatsWithMucking)

          finalState.sendStateUpdateToClients(gameService.getMainActor, toppers = MMap.empty[String, ClientData], clients = clients, admins = MMap.empty[String, AdminClientData] )
          finalStateWithMucking.sendStateUpdateToClients(gameService.getMainActor, toppers = toppers, clients = MMap.empty[String, ClientData], admins = admins)

          context.become(TABLE_SHOWDOWN_KO_STATE_17(data = finalState))

        } else if (updatedState.allActivePlayersMatchedHighestBet(round = TURN_ROUND)) {
//          log.info("Please Handle Betting Round End Now!!!")
          val finalState = updatedState.copy(
            stage = "12",
            potLimit = 0,
            betAmount = 0,
            raiseAmount = 0,
            action = s"R3 betting end, all matched the highest bet ${updatedState.highestAmountInRound(TURN_ROUND)}"
          )

          finalState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          context.become(TABLE_TURN_BETTING_CONFIRM_STATE_12(data = finalState))

        } else {
          //No knockout champion
          //Not All the active players [numPlayers] matched Highest Bet
          val nextSeatId = updatedState.numPlayers(updatedState.turnId)

          val finalState = updatedState
            .prepareForMatchingBet(nextSeatId, round = TURN_ROUND)

          finalState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          context.become(TABLE_TURN_BETTING_STATE_11(data = finalState))
        }
      }
    case CheckCmd(uid: String) =>
      if (data.seats(data.numPlayers(data.turnId)).uid != uid) {

      } else {
        val currentTurn = data.turnId

        val updatedStateAfterCheck = data
          .handleCheckCmd(data.numPlayers(currentTurn), round = TURN_ROUND)
        updatedStateAfterCheck
          .sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)

        if (updatedStateAfterCheck.allSeatsCheckedAlready()) {
          val updatedState = updatedStateAfterCheck.copy(
            stage = "12",
            potLimit = 0,
            betAmount = 0,
            raiseAmount = 0,
            action = s"R3 betting end, Free Round"
          )

          updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          context.become(TABLE_TURN_BETTING_CONFIRM_STATE_12(data = updatedState))
        } else {
          val nextTurn = updatedStateAfterCheck.getNextTurn(currentTurn)
          val nextSeatId = updatedStateAfterCheck.numPlayers(nextTurn)
          val updatedState = updatedStateAfterCheck
            .prepareForBet(nextSeatId, round = TURN_ROUND) //TODO prepareForCheck
            .copy(turnId = nextTurn)

          updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          context.become(TABLE_TURN_BETTING_STATE_11(data = updatedState))

        }
      }
    case RaiseCmd(uid, amount) =>
      if (data.seats(data.numPlayers(data.turnId)).uid != uid) {

      } else {
        val currentTurn = data.turnId

        val updatedStateAfterRaise = data
          .handleRaiseCmd(data.numPlayers(data.turnId), wagerAmount = amount, round = TURN_ROUND)
        updatedStateAfterRaise
          .sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)

        val nextTurn = updatedStateAfterRaise.getNextTurn(currentTurn)
        val nextSeatId = updatedStateAfterRaise.numPlayers(nextTurn)
        val updatedState = updatedStateAfterRaise
          .prepareForMatchingBet(nextSeatId, round = TURN_ROUND)
          .copy(turnId = nextTurn)

        updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
        context.become(TABLE_TURN_BETTING_STATE_11(data = updatedState))
      }
    case BetCmd(uid, amount) =>
      if (data.seats(data.numPlayers(data.turnId)).uid != uid) {

      } else {
        val currentTurn = data.turnId

        val updatedStateAfterBet = data
          .handleBetCmd(data.numPlayers(data.turnId), wagerAmount = amount, round = TURN_ROUND)
        updatedStateAfterBet
          .sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)

        val nextTurn = updatedStateAfterBet.getNextTurn(currentTurn)
        val nextSeatId = updatedStateAfterBet.numPlayers(nextTurn)
        val updatedState = updatedStateAfterBet
          .prepareForMatchingBet(nextSeatId, round = TURN_ROUND)
          .copy(turnId = nextTurn)

        updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
        context.become(TABLE_TURN_BETTING_STATE_11(data = updatedState))
      }
    case CallCmd(uid: String) =>
      if (data.seats(data.numPlayers(data.turnId)).uid != uid) {

      } else {


        val currentTurn = data.turnId

        val updatedStateAfterCallBet = data
          .handleCallHandCmd(data.numPlayers(data.turnId), round = TURN_ROUND)
        updatedStateAfterCallBet
          .sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)

        if (!updatedStateAfterCallBet.allActivePlayersMatchedHighestBet(round = TURN_ROUND)) {
          /* Not All the active players [numPlayers] matched Highest Bet*/
          val nextTurn = updatedStateAfterCallBet.getNextTurn(currentTurn)
          val nextSeatId = updatedStateAfterCallBet.numPlayers(nextTurn)
          val updatedState = updatedStateAfterCallBet
            .prepareForMatchingBet(nextSeatId, round = TURN_ROUND)
            .copy(turnId = nextTurn)

          updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          context.become(TABLE_TURN_BETTING_STATE_11(data = updatedState))

        } else {
//          log.info("Please Handle Betting Round End Now!!!")
          val updatedState = updatedStateAfterCallBet.copy(
            stage = "12",
            potLimit = 0,
            betAmount = 0,
            raiseAmount = 0,
            action = s"R3 betting end, all matched the highest bet ${updatedStateAfterCallBet.highestAmountInRound(TURN_ROUND)}"
          )

          updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          context.become(TABLE_TURN_BETTING_CONFIRM_STATE_12(data = updatedState))

        }
      }

    case GameCancel =>
      val updatedData = data
        .recallBetsWithBetLists()
        .prepareForFreshRound()

      updatedData.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
      become(TABLE_STATE_1_READY(data = updatedData))


    case PollTick =>
    case _ => log.error("some message received in TABLE_TURN_BETTING_STATE_11 state")
  }

  def TABLE_TURN_BETTING_CONFIRM_STATE_12(data: TableState): Receive = {
    case PlayerConnected(name, actor, client, clientType) => context.become(TABLE_TURN_BETTING_CONFIRM_STATE_12(data = handlePlayerConnected(data, name, actor, client, clientType)))
    case TopperConnected(name, actor, client) => context.become(TABLE_TURN_BETTING_CONFIRM_STATE_12(data = handleTopperConnected(data, name, actor, client)))
    case AdminConnected(name, actor, client) => context.become(TABLE_TURN_BETTING_CONFIRM_STATE_12(data = handleAdminConnected(data, name, actor, client)))
    case PlayerDisConnected(playerIp) => context.become(TABLE_TURN_BETTING_CONFIRM_STATE_12(data = handlePlayerDisconnected(data, playerIp)))
    case TopperDisConnected(topperIp) => context.become(TABLE_TURN_BETTING_CONFIRM_STATE_12(data = handleTopperDisconnected(data, topperIp)))
    case AdminDisConnected(adminIp) => context.become(TABLE_TURN_BETTING_CONFIRM_STATE_12(data = handleAdminDisconnected(data, adminIp)))


    case PollTick =>
      self ! ContinueToNextRound
    case ContinueToNextRound =>

      if(data.sidePots.isEmpty) {
        val amountToPot = data.seats.map(seat => seat.bets.head).fold(0.0)(_ + _)

        //things to do
        //1.  move bets.head to flop position(2) and reset to 0
        val updatedSeats = data.seats.map(seat => seat.seatSettleRoundBets("Turn"))

        val updatedState = data.copy(
          stage = "13",
          potAmount = data.potAmount + amountToPot,
          seats = updatedSeats,
          turnId = 4, //turn as gamecards pos
          potLimit = 0,
          betAmount = 0,
          action = s"into river card draw, ${amountToPot.toInt} splashed to pot now ${data.potAmount.toInt + amountToPot.toInt}"
        )
        updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
        context.become(TABLE_RIVER_CARD_DRAWING_STATE_13(data = updatedState))
      } else {

        val totalSidePotAmount = data.sidePots.map(sp => sp.ids.size * sp.capAmount).sum
        val numPlayerBetAmount = data.numPlayers.map((player) => data.seats(player).betList.map(_.betValue).sum).sum
        val highestSidePotCap = data.sidePots.map(sp => sp.capAmount).sortWith((a, b) => a < b).last

        if (data.allSeatsAllIn()) {
          /*All seats are All In */

          val updatedSeats = data.seats.map(seat => seat.seatSettleRoundBets("Turn"))

          val updatedState = data.copy(
            stage = "13",
            potAmount = totalSidePotAmount + numPlayerBetAmount,
            seats = updatedSeats,
            turnId = 4, //turn as gamecards pos
            potLimit = 0,
            betAmount = 0,
            action = s"into river card draw, "
          )
          updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          context.become(TABLE_RIVER_CARD_DRAWING_STATE_13(data = updatedState))

        }else {
          val updatedSeats = data.seats.map(seat => seat.seatSettleRoundBets("Turn"))

          val updatedState = data.copy(
            stage = "13",
            potAmount = totalSidePotAmount + numPlayerBetAmount,
            seats = updatedSeats,
            turnId = 4, //turn as gamecards pos
            potLimit = 0,
            betAmount = 0,
            action = s"into river card draw, "
          )
          updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          context.become(TABLE_RIVER_CARD_DRAWING_STATE_13(data = updatedState))
        }

      }
    case GameCancel =>
      val updatedData = data
        .recallBetsWithBetLists()
        .prepareForFreshRound()

      updatedData.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
      become(TABLE_STATE_1_READY(data = updatedData))

    case _ => log.error("some message received in TABLE_TURN_BETTING_CONFIRM_STATE_12 state")
  }

  def TABLE_RIVER_CARD_DRAWING_STATE_13(data: TableState, count: Int = -1): Receive = {
    case PlayerConnected(name, actor, client, clientType) => context.become(TABLE_RIVER_CARD_DRAWING_STATE_13(data = handlePlayerConnected(data, name, actor, client, clientType), count))
    case TopperConnected(name, actor, client) => context.become(TABLE_RIVER_CARD_DRAWING_STATE_13(data = handleTopperConnected(data, name, actor, client), count))
    case AdminConnected(name, actor, client) => context.become(TABLE_RIVER_CARD_DRAWING_STATE_13(data = handleAdminConnected(data, name, actor, client), count))
    case PlayerDisConnected(playerIp) => context.become(TABLE_RIVER_CARD_DRAWING_STATE_13(data = handlePlayerDisconnected(data, playerIp), count))
    case TopperDisConnected(topperIp) => context.become(TABLE_RIVER_CARD_DRAWING_STATE_13(data = handleTopperDisconnected(data, topperIp), count))
    case AdminDisConnected(adminIp) => context.become(TABLE_RIVER_CARD_DRAWING_STATE_13(data = handleAdminDisconnected(data, adminIp), count))

    case PollTick =>
      if (count == -1) {
        self ! DrawCards
      }
      if (count > 0) {
        if (!data.configData.liveDealer) {
          val updatedData = data.drawCommunityCard(deck52)
          updatedData.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          context.become(TABLE_RIVER_CARD_DRAWING_STATE_13(data = updatedData, count = count - 1))
        }
      } else if (count == 0) {
        /* River Card Draw End */

        /*Skip betting cases */
        if(data.allSeatsAllIn() || data.allInWinnerSkipBetting()) {

          if(data.configData.pokerVariant == "Texas"){
            val updatedSeats = data.seats.map(seat => seat.seatSettleRoundBets("River"))

            val updatedState = data.copy(
              stage = "15",
              seats = updatedSeats,
              potLimit = 0,
              betAmount = 0,
              raiseAmount = 0,
              action = s"Skip RIVER_ROUND betting, All in"
            )

            updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
            context.become(TABLE_RIVER_BETTING_CONFIRM_STATE_15(data = updatedState))
          } else {
            val updatedSeats = data.seats.map(seat => seat.seatSettleRoundBets("River"))

            val updatedState = data.copy(
              stage = "15",
              seats = updatedSeats,
              potLimit = 0,
              betAmount = 0,
              raiseAmount = 0,
              action = s"Skip RIVER_ROUND betting, All in"
            )

            updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
            context.become(TABLE_RIVER_BETTING_CONFIRM_STATE_155(data = updatedState))
          }



        } else {
          /* Select the first betting candidate for RIVER_ROUND betting */
          val dealerIndex = data.numPlayers.indexWhere(p => p == data.dealerId)
          val (dealerPos, smallBetPos, bigBetPos, initialTurnPos) = data.getPlayerPositions(dealerIndex)

          val currentTurn = dealerPos
          val nextTurn = data.getNextTurn(currentTurn)
          val nextSeatId = data.numPlayers(nextTurn)

          val updatedState = data
            .prepareForCheck(nextSeatId, round = RIVER_ROUND)
            .copy(turnId = nextTurn)
            .copy(stage = "14")

          updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          context.become(TABLE_RIVER_BETTING_STATE_14(data = updatedState))

        }


      }
    case DrawCards =>
      if(count == -1) {
        data.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
        context.become(TABLE_RIVER_CARD_DRAWING_STATE_13(data = data, count = 1))
      }

    case CardDrawn(card: Card) =>
      if (data.configData.liveDealer) {
        if (count > 0) {
          val updatedData = data.handleCommunityCard(card.toString)
          updatedData.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          context.become(TABLE_RIVER_CARD_DRAWING_STATE_13(data = updatedData, count = count - 1))
        }
      } else {
        log.error("Check Your configuration, cards are being delivered externally on a RNG setup")
      }
    case GameCancel =>
      val updatedData = data
        .recallBetsWithBetLists()
        .prepareForFreshRound()

      updatedData.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
      become(TABLE_STATE_1_READY(data = updatedData))

    case _ => log.error("some message received in TABLE_RIVER_CARD_DRAWING_STATE_13 state")
  }


  def TABLE_RIVER_BETTING_STATE_14(data: TableState): Receive = {
//    case PlayerDisConnected(playerIp)         => context.become(TABLE_RIVER_BETTING_STATE_14(data = handlePlayerDisconnected(data, playerIp)))
//    case PlayerConnected(name, actor, client, clientType) => context.become(TABLE_RIVER_BETTING_STATE_14(data = handlePlayerConnected(data, name, actor, client, clientType)))
//    case TopperConnected(name, actor, client) => handleTopperConnected(data, name, actor, client)


    case PlayerConnected(name, actor, client, clientType) => context.become(TABLE_RIVER_BETTING_STATE_14(data = handlePlayerConnected(data, name, actor, client, clientType)))
    case TopperConnected(name, actor, client) => context.become(TABLE_RIVER_BETTING_STATE_14(data = handleTopperConnected(data, name, actor, client)))
    case AdminConnected(name, actor, client) => context.become(TABLE_RIVER_BETTING_STATE_14(data = handleAdminConnected(data, name, actor, client)))
    case PlayerDisConnected(playerIp) => context.become(TABLE_RIVER_BETTING_STATE_14(data = handlePlayerDisconnected(data, playerIp)))
    case TopperDisConnected(topperIp) => context.become(TABLE_RIVER_BETTING_STATE_14(data = handleTopperDisconnected(data, topperIp)))
    case AdminDisConnected(adminIp) => context.become(TABLE_RIVER_BETTING_STATE_14(data = handleAdminDisconnected(data, adminIp)))


    case AllInCmd(uid: String) =>
      if (data.seats(data.numPlayers(data.turnId)).uid != uid) {

      } else {
        //Step 1. Handle Command
        val currentTurn = data.turnId
        val currentSeatId = data.numPlayers(currentTurn)
        val currentSeat = data.getActivePlayers.find(_.id == currentSeatId).get

        val updatedState = data.handleAllInCmd(currentSeatId, round = RIVER_ROUND)
        updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)

        //Step 2. Analyze result State
        if (updatedState.allSeatsAllIn() || updatedState.allSeatsCalledAlready(round = RIVER_ROUND)) {

          if(data.configData.pokerVariant == "Texas"){
            val finalState = updatedState.copy(
              stage = "15",
              potLimit = 0,
              betAmount = 0,
              raiseAmount = 0,
              seats = updatedState.seats.map(seat => seat.seatSettleRoundBets("River")),
              action = s"river betting round ended, as "
            )

            finalState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
            context.become(TABLE_RIVER_BETTING_CONFIRM_STATE_15(data = finalState))
          } else {
            val finalState = updatedState.copy(
              stage = "15",
              potLimit = 0,
              betAmount = 0,
              raiseAmount = 0,
              seats = updatedState.seats.map(seat => seat.seatSettleRoundBets("River")),
              action = s"river betting round ended, as "
            )

            finalState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
            context.become(TABLE_RIVER_BETTING_CONFIRM_STATE_155(data = finalState))
          }

        } else {
          /* there are some more seats to act */
          val nextTurn = updatedState.turnId
          val nextSeatId = updatedState.numPlayers(nextTurn)
          val finalState = updatedState
            .prepareForMatchingBet(nextSeatId, round = RIVER_ROUND)

          finalState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          context.become(TABLE_RIVER_BETTING_STATE_14(data = finalState))
        }

      }


    case FoldCmd(uid: String) =>
      if (data.seats(data.numPlayers(data.turnId)).uid != uid) {

      } else {
        val currentTurn = data.turnId

        val updatedState = data
          .handleFoldCmd(data.numPlayers(data.turnId), round = RIVER_ROUND)
        updatedState
          .sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)

        //Knockout champion ??
        if (updatedState.knockOutWinnerSkipAll()) {
          val finalState = updatedState.handleKnockout()
//          val finalSeatsWithMucking = finalState.seats.map(seat => if (!seat.isPlaying) seat else seat.copy(cards = Seq("xx", "xx")))
          val finalSeatsWithMucking = finalState.seats.map(seat => if (!seat.isPlaying) seat else seat.copy(cards = seat.cards.map(_ => "xx")))
          val finalStateWithMucking = finalState.copy(seats = finalSeatsWithMucking)

          finalState.sendStateUpdateToClients(gameService.getMainActor, toppers = MMap.empty[String, ClientData], clients = clients, admins = MMap.empty[String, AdminClientData])
          finalStateWithMucking.sendStateUpdateToClients(gameService.getMainActor, toppers = toppers, clients = MMap.empty[String, ClientData], admins = admins)

          context.become(TABLE_SHOWDOWN_KO_STATE_17(data = finalState))

        } else if (updatedState.allActivePlayersMatchedHighestBet(round = RIVER_ROUND)) {
//          log.info("Please Handle Betting Round End Now!!!")
          if(data.configData.pokerVariant == "Texas"){
            val finalState = updatedState.copy(
              stage = "15",
              potLimit = 0,
              betAmount = 0,
              raiseAmount = 0,
              seats = updatedState.seats.map(seat => seat.seatSettleRoundBets("River")),
              action = s"R4 betting end, all matched the highest bet ${updatedState.highestAmountInRound(RIVER_ROUND)}"
            )

            finalState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
            context.become(TABLE_RIVER_BETTING_CONFIRM_STATE_15(data = finalState))
          } else {
            val finalState = updatedState.copy(
              stage = "15",
              potLimit = 0,
              betAmount = 0,
              raiseAmount = 0,
              seats = updatedState.seats.map(seat => seat.seatSettleRoundBets("River")),
              action = s"R4 betting end, all matched the highest bet ${updatedState.highestAmountInRound(RIVER_ROUND)}"
            )

            finalState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
            context.become(TABLE_RIVER_BETTING_CONFIRM_STATE_155(data = finalState))
          }


        } else {
          //No knockout champion
          //Not All the active players [numPlayers] matched Highest Bet
          val nextSeatId = updatedState.numPlayers(updatedState.turnId)

          val finalState = updatedState
            .prepareForMatchingBet(nextSeatId, round = RIVER_ROUND)

          finalState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          context.become(TABLE_RIVER_BETTING_STATE_14(data = finalState))
        }
      }
    case CheckCmd(uid: String) =>
      if (data.seats(data.numPlayers(data.turnId)).uid != uid) {

      } else {
        val currentTurn = data.turnId

        val updatedStateAfterCheck = data
          .handleCheckCmd(data.numPlayers(currentTurn), round = RIVER_ROUND)
        updatedStateAfterCheck
          .sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)

        if (updatedStateAfterCheck.allSeatsCheckedAlready()) {
          if(data.configData.pokerVariant == "Texas"){
            val updatedState = updatedStateAfterCheck.copy(
              stage = "15",
              potLimit = 0,
              betAmount = 0,
              raiseAmount = 0,
              seats = updatedStateAfterCheck.seats.map(seat => seat.seatSettleRoundBets("River")),
              action = s"R4 betting end, Free Round"
            )

            updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
            context.become(TABLE_RIVER_BETTING_CONFIRM_STATE_15(data = updatedState))
          } else {
            val updatedState = updatedStateAfterCheck.copy(
              stage = "15",
              potLimit = 0,
              betAmount = 0,
              raiseAmount = 0,
              seats = updatedStateAfterCheck.seats.map(seat => seat.seatSettleRoundBets("River")),
              action = s"R4 betting end, Free Round"
            )

            updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
            context.become(TABLE_RIVER_BETTING_CONFIRM_STATE_155(data = updatedState))
          }

        } else {

          val nextTurn = updatedStateAfterCheck.getNextTurn(currentTurn)
          val nextSeatId = updatedStateAfterCheck.numPlayers(nextTurn)
          val updatedState = updatedStateAfterCheck
            .prepareForBet(nextSeatId, round = RIVER_ROUND) //TODO prepareForCheck
            .copy(turnId = nextTurn)

          updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          context.become(TABLE_RIVER_BETTING_STATE_14(data = updatedState))
        }
      }

    case RaiseCmd(uid, amount) =>
      if (data.seats(data.numPlayers(data.turnId)).uid != uid) {

      } else {
        val currentTurn = data.turnId

        val updatedStateAfterRaise = data
          .handleRaiseCmd(data.numPlayers(data.turnId), wagerAmount = amount, round = RIVER_ROUND)
        updatedStateAfterRaise
          .sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)

        val nextTurn = updatedStateAfterRaise.getNextTurn(currentTurn)
        val nextSeatId = updatedStateAfterRaise.numPlayers(nextTurn)
        val updatedState = updatedStateAfterRaise
          .prepareForMatchingBet(nextSeatId, round = RIVER_ROUND)
          .copy(turnId = nextTurn)

        updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
        context.become(TABLE_RIVER_BETTING_STATE_14(data = updatedState))
      }
    case BetCmd(uid, amount) =>
      if (data.seats(data.numPlayers(data.turnId)).uid != uid) {

      } else {
        val currentTurn = data.turnId

        val updatedStateAfterBet = data
          .handleBetCmd(data.numPlayers(data.turnId), wagerAmount = amount, round = RIVER_ROUND)
        updatedStateAfterBet
          .sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)

        val nextTurn = updatedStateAfterBet.getNextTurn(currentTurn)
        val nextSeatId = updatedStateAfterBet.numPlayers(nextTurn)
        val updatedState = updatedStateAfterBet
          .prepareForMatchingBet(nextSeatId, round = RIVER_ROUND)
          .copy(turnId = nextTurn)

        updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
        context.become(TABLE_RIVER_BETTING_STATE_14(data = updatedState))
      }
    case CallCmd(uid: String) =>
      if (data.seats(data.numPlayers(data.turnId)).uid != uid) {

      } else {
        val currentTurn = data.turnId

        val updatedStateAfterCallBet = data
          .handleCallHandCmd(data.numPlayers(data.turnId), round = RIVER_ROUND)
        updatedStateAfterCallBet
          .sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)

        if (!updatedStateAfterCallBet.allActivePlayersMatchedHighestBet(round = RIVER_ROUND)) {
          /* Not All the active players [numPlayers] matched Highest Bet*/
          val nextTurn = updatedStateAfterCallBet.getNextTurn(currentTurn)
          val nextSeatId = updatedStateAfterCallBet.numPlayers(nextTurn)
          val updatedState = updatedStateAfterCallBet
            .prepareForMatchingBet(nextSeatId, round = RIVER_ROUND)
            .copy(turnId = nextTurn)

          updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          context.become(TABLE_RIVER_BETTING_STATE_14(data = updatedState))

        } else {
//          log.info("Please Handle Betting Round End Now!!!")
          if(data.configData.pokerVariant == "Texas"){
            val updatedState = updatedStateAfterCallBet.copy(
              stage = "15",
              potLimit = 0,
              betAmount = 0,
              raiseAmount = 0,
              seats = updatedStateAfterCallBet.seats.map(seat => seat.seatSettleRoundBets("River")),
              action = s"R4 betting end, all matched the highest bet ${updatedStateAfterCallBet.highestAmountInRound(RIVER_ROUND)}"
            )

            updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
            context.become(TABLE_RIVER_BETTING_CONFIRM_STATE_15(data = updatedState))
          } else {
            val updatedState = updatedStateAfterCallBet.copy(
              stage = "15",
              potLimit = 0,
              betAmount = 0,
              raiseAmount = 0,
              seats = updatedStateAfterCallBet.seats.map(seat => seat.seatSettleRoundBets("River")),
              action = s"R4 betting end, all matched the highest bet ${updatedStateAfterCallBet.highestAmountInRound(RIVER_ROUND)}"
            )

            updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
            context.become(TABLE_RIVER_BETTING_CONFIRM_STATE_155(data = updatedState))
          }

        }
      }

    case GameCancel =>
      val updatedData = data
        .recallBetsWithBetLists()
        .prepareForFreshRound()

      updatedData.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
      become(TABLE_STATE_1_READY(data = updatedData))


    case PollTick =>


    case _ => log.error("some message received in TABLE_RIVER_BETTING_STATE_14 state")
  }

  def TABLE_RIVER_BETTING_CONFIRM_STATE_15(data: TableState): Receive = {
    case PlayerConnected(name, actor, client, clientType) => context.become(TABLE_RIVER_BETTING_CONFIRM_STATE_15(data = handlePlayerConnected(data, name, actor, client, clientType)))
    case TopperConnected(name, actor, client) => context.become(TABLE_RIVER_BETTING_CONFIRM_STATE_15(data = handleTopperConnected(data, name, actor, client)))
    case AdminConnected(name, actor, client) => context.become(TABLE_RIVER_BETTING_CONFIRM_STATE_15(data = handleAdminConnected(data, name, actor, client)))
    case PlayerDisConnected(playerIp) => context.become(TABLE_RIVER_BETTING_CONFIRM_STATE_15(data = handlePlayerDisconnected(data, playerIp)))
    case TopperDisConnected(topperIp) => context.become(TABLE_RIVER_BETTING_CONFIRM_STATE_15(data = handleTopperDisconnected(data, topperIp)))
    case AdminDisConnected(adminIp) => context.become(TABLE_RIVER_BETTING_CONFIRM_STATE_15(data = handleAdminDisconnected(data, adminIp)))

    case PollTick =>
      self ! ContinueToNextRound

    case ContinueToNextRound =>

      val allBestHands: Seq[(Int, Hand, String)] = data.getAllPlayersHandsTexas()//allBestHands = hands of actionPlayers + hands of allInPlayers
      val hands = allBestHands.map(entry => (entry._1, entry._2.cards.map(_.toString).toList, entry._2.joker.map(_.toString).toList, entry._3))
      allBestHands.foreach(h => println(s"Seat${h._1} => Selected:${h._2.cards} Poker Hand: ${h._3} Dropped:(${h._2.joker}) - "))

      var updatedSidePots = data.sidePots

      if(data.sidePots.isEmpty)
      { /* A Single Winner or a split so, straightforward */

        val currentWinner = detectWinnerIdTexas(hands = allBestHands)
        val totalWin = data.getRoundBets().map(_.betValue).sum
        val winnerHands = detectSameHandsTexas(currentWinner, hands = allBestHands)

        if(winnerHands.size > 1) {
          /* split needed */
          val splittedWinAmount = totalWin / winnerHands.size
          val splittedRakeAmount = (splittedWinAmount * data.configData.rakePercent * .01)
          val splittedTotalWinAfterRakeCollection = splittedWinAmount - splittedRakeAmount
          val splittedRoundedWin = Math.floor(splittedTotalWinAfterRakeCollection)
          val splittedRoundOff = splittedTotalWinAfterRakeCollection - Math.floor(splittedTotalWinAfterRakeCollection)

          val updatedPotAmount = data.potAmount + data.seats.map(seat => seat.bets.head).fold(0.0)(_ + _)
          var updatedSeats = data.seats.map(
            seat =>
              if (!seat.isPlaying) seat //Skip the sitOut players
              else if(winnerHands.contains(seat.id)) seat
              else if (seat.gameStatus == "FOLDED") {
                seat.copy(
                  winAmount = 0,
                  bets = seat.bets.updated(0, 0),
                  gameStatus = s"Lost",
                  cards = seat.cards.map(_ => "xx"),
                  isTurn = false,
                  actions = Seq(false, false, false, false, false)
                )
              } else {
                seat.copy(
                  winAmount = 0,
                  bets = seat.bets.updated(0, 0),
                  gameStatus = s"Lost-${allBestHands.find(h => h._1 == seat.id).get._3}",
                  isTurn = false,
                  actions = Seq(false, false, false, false, false)
                )
              }
          )
          var updatedState = data.copy(
            stage = "16",
            potAmount = updatedPotAmount,
            seats = updatedSeats,
            hands = allBestHands.map(entry => (entry._1, entry._2.cards.map(_.toString).toList, entry._2.joker.map(_.toString).toList, entry._3))

          )
          for (winnerHand <- winnerHands) {

            updatedSeats = updatedSeats.map(
              seat =>
                if (winnerHands.contains(seat.id))
                  seat.copy(
                    winAmount =  splittedRoundedWin,
                    bets = seat.bets.updated(0, splittedRoundedWin),
                    gameStatus = s"Win-${allBestHands.find(h => h._1 == seat.id).get._3}",
                    isTurn = true,
                    actions = Seq(false, false, false, false, false)
                  )
                else seat

            )

            val currentWinningCards = allBestHands.find(item => item._1 == winnerHand).get._2.cards.toList.map(card => card.toString)
            val currentWinningHand = allBestHands.find(item => item._1 == winnerHand).get._3

            updatedState = updatedState.copy(
              winners = updatedState.winners.:+(
                Winner(
                  id = winnerHand,
                  winningPot = 0,
                  winAmount = splittedRoundedWin.toInt,
                  rake = splittedRakeAmount + splittedRoundOff,
                  hand = currentWinningHand,
                  cards = currentWinningCards
                )
              ),
              seats = updatedSeats,
              action = s"seat${winnerHand + 1} won ${splittedRoundedWin.toInt} Pot"
            )

            updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)

          }

          val finalState = updatedState.copy(
            action = s"seats [${winnerHands.map(id=> id+1).mkString(",")}] won ${splittedRoundedWin.toInt} Pot"
          )
          finalState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)

          context.become(TABLE_SHOWDOWN_STATE_16(data = finalState))

        } else {
          /* Single Winner */
          val rakeAmount = (totalWin * data.configData.rakePercent * .01)
          val totalWinAfterRakeCollection = totalWin - rakeAmount
          val roundedWin = Math.floor(totalWinAfterRakeCollection)
          val roundOff = totalWinAfterRakeCollection - Math.floor(totalWinAfterRakeCollection)

          val currentWinningCards = allBestHands.find(item => item._1 == currentWinner).get._2.cards.toList.map(card => card.toString)
          val currentWinningHand = allBestHands.find(item => item._1 == currentWinner).get._3

          val updatedSeats = data.seats.map(
            seat =>
              if (!seat.isPlaying) seat //Skip the sitOut players
              else {
                if (seat.id == currentWinner)
                  seat.copy(
                    winAmount = roundedWin,
                    bets = seat.bets.updated(0, roundedWin),
                    gameStatus = s"Win-${allBestHands.find(h => h._1 == seat.id).get._3}",
                    isTurn = true,
                    actions = Seq(false, false, false, false, false)
                  )
                else if (seat.gameStatus == "FOLDED") {
                  seat.copy(
                    winAmount = 0,
                    bets = seat.bets.updated(0, 0),
                    gameStatus = s"Lost",
                    cards = seat.cards.map(_ => "xx"),
                    isTurn = false,
                    actions = Seq(false, false, false, false, false)
                  )
                } else {
                  seat.copy(
                    winAmount = 0,
                    bets = seat.bets.updated(0, 0),
                    gameStatus = s"Lost-${allBestHands.find(h => h._1 == seat.id).get._3}",
                    isTurn = false,
                    actions = Seq(false, false, false, false, false)
                  )
                }
              }
          )
          val updatedState = data.copy(
            stage = "16",
            potAmount = data.potAmount + data.seats.map(seat => seat.bets.head).fold(0.0)(_ + _),
            winners = data.winners.:+(
              Winner(
                id = currentWinner,
                winningPot = 0,
                winAmount = roundedWin.toInt,
                rake = rakeAmount + roundOff,
                hand = currentWinningHand,
                cards = currentWinningCards
              )
            ),
            seats = updatedSeats,
            action = s"seat${currentWinner + 1} won ${roundedWin.toInt} Pot",
            hands = allBestHands.map(entry => (entry._1, entry._2.cards.map(_.toString).toList, entry._2.joker.map(_.toString).toList, entry._3))
          )

          updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          context.become(TABLE_SHOWDOWN_STATE_16(data = updatedState))
        }



      } else { /* else there are sidePots */


        val totalSidePotAmount = data.sidePots.map(sp => sp.ids.size * sp.capAmount).sum
        val numPlayerBetAmount = data.numPlayers.map((player) => data.seats(player).betList.map(_.betValue).sum).sum
        val numPlayerSidePots = data.numPlayers.map((player) => SidePot(Seq(player), capAmount = data.seats(player).betList.map(_.betValue).sum))

//        log.info(s"numPlayers${numPlayerSidePots.map(_.capAmount)}  SpSum=${totalSidePotAmount}")

        numPlayerSidePots.foreach((sp) => {
           updatedSidePots = splashToSidePots(updatedSidePots, SidePot(ids = sp.ids, capAmount = sp.capAmount))
             .sortWith((a,b) => a.capAmount < b.capAmount)
             .sortWith((a,b) => a.ids.size > b.ids.size)

        })
//        log.info(s"finally ${updatedSidePots}")

        val splashedState = data.copy(
          stage = "16",
          sidePots = updatedSidePots,
          action = s"live players bets are now added to the pots!",
        )

        splashedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)

        val sidePots =  updatedSidePots

        var updatedState = data.copy(winners = Seq.empty[Winner])
        var updatedSeats = data.seats.map(seat => seat.copy(winAmount = 0))

        sidePots.reverse.foreach((sp) => { /*for each "side-pot" add a winner to the winners list*/

          if(sp.ids.size > 1) {
            /*More than one seats fighting for this pot*/

            val handsToFight = allBestHands.filter(hand =>
              data.seats(hand._1).gameStatus != "FOLDED" //filter out folded hands
                && sp.ids.contains(hand._1))


            val currentWinner = detectWinnerIdTexas(hands = handsToFight)
            val totalWin = (sp.ids.size * sp.capAmount) + sp.foldAmount
            val winnerHands = detectSameHandsTexas(currentWinner, hands = handsToFight)

            if (winnerHands.size > 1) {
              /* split needed */

              val splittedWinAmount = totalWin / winnerHands.size
              val splittedRakeAmount = (splittedWinAmount * data.configData.rakePercent * .01)
              val splittedTotalWinAfterRakeCollection = splittedWinAmount - splittedRakeAmount
              val splittedRoundedWin = Math.floor(splittedTotalWinAfterRakeCollection)
              val splittedRoundOff = splittedTotalWinAfterRakeCollection - Math.floor(splittedTotalWinAfterRakeCollection)

              for (winnerHand <- winnerHands) {
                updatedSeats = updatedSeats.map(
                  seat =>
                    if (winnerHands.contains(seat.id))
                      seat.copy(
                        winAmount = seat.winAmount + splittedRoundedWin,
                        bets = seat.bets.updated(0, seat.bets(0) +  splittedRoundedWin),
                        gameStatus = s"Win-${allBestHands.find(h => h._1 == seat.id).get._3}",
                        isTurn = true,
                        actions = Seq(false, false, false, false, false)
                      )
                    else seat

                )

                val currentWinningCards = allBestHands.find(item => item._1 == winnerHand).get._2.cards.toList.map(card => card.toString)
                val currentWinningHand = allBestHands.find(item => item._1 == winnerHand).get._3

                updatedState = updatedState.copy(
                  winners = updatedState.winners.:+(
                    Winner(
                      id = winnerHand,
                      winningPot = 0,
                      winAmount = splittedRoundedWin.toInt,
                      rake = splittedRakeAmount + splittedRoundOff,
                      hand = currentWinningHand,
                      cards = currentWinningCards
                    )
                  ),
                  seats = updatedSeats,
                  action = s"seat${winnerHand + 1} won ${splittedRoundedWin.toInt} Pot!"
                )

                updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)

              }

              val finalState = updatedState.copy(
                action = s"seats [${winnerHands.map(id => id + 1).mkString(",")}] won ${splittedRoundedWin.toInt}"
              )
              finalState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)

              context.become(TABLE_SHOWDOWN_STATE_16(data = finalState))


            } else {
              val currentWinningCards = allBestHands.find(item => item._1 == currentWinner).get._2.cards.toList.map(card => card.toString)
              val currentWinningHand = allBestHands.find(item => item._1 == currentWinner).get._3

              val rakeAmount = (totalWin * data.configData.rakePercent * .01)
              val totalWinAfterRakeCollection = totalWin - rakeAmount
              val roundedWin = Math.floor(totalWinAfterRakeCollection)
              val roundOff = totalWinAfterRakeCollection - Math.floor(totalWinAfterRakeCollection)

              updatedSeats = updatedSeats.map(
                seat =>
                  if (!seat.isPlaying) seat //Skip the sitOut players
                  else {
                    if (seat.id == currentWinner)
                      seat.copy(
                        winAmount = seat.winAmount + roundedWin,
                        bets = seat.bets.updated(0, seat.bets(0) + roundedWin),
                        gameStatus = s"Win-${allBestHands.find(h => h._1 == seat.id).get._3}",
                        isTurn = true,
                        actions = Seq(false, false, false, false, false)
                      )
                    else
                      seat
                  }
              )

              updatedState = updatedState.copy(
                stage = "16",
                winners = updatedState.winners.:+(
                  Winner(
                    id = currentWinner,
                    winningPot = 0,
                    winAmount = roundedWin.toInt,
                    rake = rakeAmount + roundOff,
                    hand = currentWinningHand,
                    cards = currentWinningCards
                  )
                ),
                seats = updatedSeats,
                sidePots = updatedSidePots,
                action = s"seat${currentWinner} win a pot ${totalWin} fought by [${sp.ids.map(_ + 1).mkString(",")}] win=${roundedWin} rake=${rakeAmount + roundOff}!"
              )

              updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)

            }
          } else {/*nobody to fight for this amount, so send it back to balance*/

            updatedSeats = updatedSeats
              .map(seat =>
                if(seat.id == sp.ids.head) seat.copy(
                  balance = seat.balance + sp.capAmount,
                  betList = seat.betList.:+(Bet(updatedState.betIndex, -sp.capAmount, group = RIVER_ROUND, betType = "c"))
                )
                else seat
              )

            updatedState = updatedState
              .copy(
                stage = "16",
                potAmount = updatedState.potAmount - sp.capAmount,
                seats = updatedSeats,
                betIndex = updatedState.betIndex + 1,
                action = s"${sp.capAmount} is reverted back to Seat${sp.ids.head} balance ${updatedSeats(sp.ids.head).balance}"
              )

            updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          }
        })

        /*Prepare for a final showdown state*/
//        log.info(s"Updated winners ${updatedState.winners.map(_.id).mkString(",")}")

        /*You may need to go through each loosing seats and update their state as well ???*/
        val finalSeats = updatedSeats.map(
          seat =>
            if(!seat.isPlaying || updatedState.winners.map(_.id).contains(seat.id)) seat //Skip the sitOut players & Winners
            else {
              if (seat.gameStatus == "FOLDED") {
                seat.copy(
                  winAmount = 0,
                  gameStatus = s"Lost",
                  cards = seat.cards.map(_ => "xx"),
                  isTurn = false,
                  actions = Seq(false, false, false, false, false)
                )
              } else {
                seat.copy(
                  winAmount = 0,
                  gameStatus = s"Lost-${allBestHands.find(h => h._1 == seat.id).get._3}",
                  isTurn = false,
                  actions = Seq(false, false, false, false, false)
                )
              }
            }
        )

        val finalState = updatedState.copy(
          stage = "16",
          seats = finalSeats,
          sidePots = updatedSidePots,
          hands = allBestHands.map(entry => (entry._1, entry._2.cards.map(_.toString).toList, entry._2.joker.map(_.toString).toList, entry._3))
        )

        finalState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
        context.become(TABLE_SHOWDOWN_STATE_16(data = finalState ))

      }




    case GameCancel =>
      val updatedData = data
        .recallBetsWithBetLists()
        .prepareForFreshRound()

      updatedData.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
      become(TABLE_STATE_1_READY(data = updatedData))


    case _ => log.error("some message received in TABLE_RIVER_BETTING_CONFIRM_STATE_15 state")
  }

  def TABLE_RIVER_BETTING_CONFIRM_STATE_155(data: TableState): Receive = {
    case PlayerConnected(name, actor, client, clientType) => context.become(TABLE_RIVER_BETTING_CONFIRM_STATE_155(data = handlePlayerConnected(data, name, actor, client, clientType)))
    case TopperConnected(name, actor, client) => context.become(TABLE_RIVER_BETTING_CONFIRM_STATE_155(data = handleTopperConnected(data, name, actor, client)))
    case AdminConnected(name, actor, client) => context.become(TABLE_RIVER_BETTING_CONFIRM_STATE_155(data = handleAdminConnected(data, name, actor, client)))
    case PlayerDisConnected(playerIp) => context.become(TABLE_RIVER_BETTING_CONFIRM_STATE_155(data = handlePlayerDisconnected(data, playerIp)))
    case TopperDisConnected(topperIp) => context.become(TABLE_RIVER_BETTING_CONFIRM_STATE_155(data = handleTopperDisconnected(data, topperIp)))
    case AdminDisConnected(adminIp) => context.become(TABLE_RIVER_BETTING_CONFIRM_STATE_155(data = handleAdminDisconnected(data, adminIp)))

    case PollTick =>
      self ! ContinueToNextRound

    case ContinueToNextRound =>


      val allBestHands: Seq[(Int, Hand, String)] = data.getAllPlayersHandsOmaha() //allBestHands = hands of actionPlayers + hands of allInPlayers
      allBestHands.foreach(h => println(s"Seat${h._1} => Selected:${h._2.cards} Poker Hand: ${h._3} Dropped:(${h._2.joker}) - "))

      var updatedSidePots = data.sidePots

      if (data.sidePots.isEmpty) {
        /* A Single Winner or a split so, straightforward */

        val currentWinner = detectWinnerIdOmaha(hands = allBestHands)
        val winnerHands = detectSameHandsOmaha(currentWinner, hands = allBestHands)
        val totalWin = data.getRoundBets().map(_.betValue).sum

        if (winnerHands.size > 1) {
          /* split needed */
          val splittedWinAmount = totalWin / winnerHands.size
          val splittedRakeAmount = (splittedWinAmount * data.configData.rakePercent * .01)
          val splittedTotalWinAfterRakeCollection = splittedWinAmount - splittedRakeAmount
          val splittedRoundedWin = Math.floor(splittedTotalWinAfterRakeCollection)
          val splittedRoundOff = splittedTotalWinAfterRakeCollection - Math.floor(splittedTotalWinAfterRakeCollection)

          val updatedPotAmount = data.potAmount + data.seats.map(seat => seat.bets.head).fold(0.0)(_ + _)
          var updatedSeats = data.seats.map(
            seat =>
              if (!seat.isPlaying) seat //Skip the sitOut players
              else if (winnerHands.contains(seat.id)) seat
              else if (seat.gameStatus == "FOLDED") {
                seat.copy(
                  winAmount = 0,
                  bets = seat.bets.updated(0, 0),
                  gameStatus = s"Lost",
                  cards = seat.cards.map(_ => "xx"),
                  isTurn = false,
                  actions = Seq(false, false, false, false, false)
                )
              } else {
                seat.copy(
                  winAmount = 0,
                  bets = seat.bets.updated(0, 0),
                  gameStatus = s"Lost-${allBestHands.find(h => h._1 == seat.id).get._3}",
                  isTurn = false,
                  actions = Seq(false, false, false, false, false)
                )
              }
          )
          var updatedState = data.copy(
            stage = "16",
            potAmount = updatedPotAmount,
            seats = updatedSeats,
            hands = allBestHands.map(entry => (entry._1, entry._2.cards.map(_.toString).toList, entry._2.joker.map(_.toString).toList, entry._3))
          )
          for (winnerHand <- winnerHands) {

            updatedSeats = updatedSeats.map(
              seat =>
                if (winnerHands.contains(seat.id))
                  seat.copy(
                    winAmount = splittedRoundedWin,
                    bets = seat.bets.updated(0, splittedRoundedWin),
                    gameStatus = s"Win-${allBestHands.find(h => h._1 == seat.id).get._3}",
                    isTurn = true,
                    actions = Seq(false, false, false, false, false)
                  )
                else seat

            )

            val currentWinningCards = allBestHands.find(item => item._1 == winnerHand).get._2.cards.toList.map(card => card.toString)
            val currentWinningHand = allBestHands.find(item => item._1 == winnerHand).get._3

            updatedState = updatedState.copy(
              winners = updatedState.winners.:+(
                Winner(
                  id = winnerHand,
                  winningPot = 0,
                  winAmount = splittedRoundedWin.toInt,
                  rake = splittedRakeAmount + splittedRoundOff,
                  hand = currentWinningHand,
                  cards = currentWinningCards
                )
              ),
              seats = updatedSeats,
              action = s"seat${winnerHand + 1} won ${splittedRoundedWin.toInt} Pot"
            )

            updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)

          }

          val finalState = updatedState.copy(
            action = s"seats [${winnerHands.map(id => id + 1).mkString(",")}] won ${splittedRoundedWin.toInt} Pot"
          )
          finalState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)

          context.become(TABLE_SHOWDOWN_STATE_16(data = finalState))

        } else {
          /* Single Winner */
          val rakeAmount = (totalWin * data.configData.rakePercent * .01)
          val totalWinAfterRakeCollection = totalWin - rakeAmount
          val roundedWin = Math.floor(totalWinAfterRakeCollection)
          val roundOff = totalWinAfterRakeCollection - Math.floor(totalWinAfterRakeCollection)

          val currentWinningCards = allBestHands.find(item => item._1 == currentWinner).get._2.cards.toList.map(card => card.toString)
          val currentWinningHand = allBestHands.find(item => item._1 == currentWinner).get._3

          val updatedSeats = data.seats.map(
            seat =>
              if (!seat.isPlaying) seat //Skip the sitOut players
              else {
                if (seat.id == currentWinner)
                  seat.copy(
                    winAmount = roundedWin,
                    bets = seat.bets.updated(0, roundedWin),
                    gameStatus = s"Win-${allBestHands.find(h => h._1 == seat.id).get._3}",
                    isTurn = true,
                    actions = Seq(false, false, false, false, false)
                  )
                else if (seat.gameStatus == "FOLDED") {
                  seat.copy(
                    winAmount = 0,
                    bets = seat.bets.updated(0, 0),
                    gameStatus = s"Lost",
                    cards = seat.cards.map(_ => "xx"),
                    isTurn = false,
                    actions = Seq(false, false, false, false, false)
                  )
                } else {
                  seat.copy(
                    winAmount = 0,
                    bets = seat.bets.updated(0, 0),
                    gameStatus = s"Lost-${allBestHands.find(h => h._1 == seat.id).get._3}",
                    isTurn = false,
                    actions = Seq(false, false, false, false, false)
                  )
                }
              }
          )
          val updatedState = data.copy(
            stage = "16",
            potAmount = data.potAmount + data.seats.map(seat => seat.bets.head).fold(0.0)(_ + _),
            winners = data.winners.:+(
              Winner(
                id = currentWinner,
                winningPot = 0,
                winAmount = roundedWin.toInt,
                rake = rakeAmount + roundOff,
                hand = currentWinningHand,
                cards = currentWinningCards
              )
            ),
            seats = updatedSeats,
            action = s"seat${currentWinner + 1} won ${roundedWin.toInt} Pot",
            hands = allBestHands.map(entry => (entry._1, entry._2.cards.map(_.toString).toList, entry._2.joker.map(_.toString).toList, entry._3))
          )

          updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          context.become(TABLE_SHOWDOWN_STATE_16(data = updatedState))
        }


      } else {
        /* else there are sidePots */


        val totalSidePotAmount = data.sidePots.map(sp => sp.ids.size * sp.capAmount).sum
        val numPlayerBetAmount = data.numPlayers.map((player) => data.seats(player).betList.map(_.betValue).sum).sum
        val numPlayerSidePots = data.numPlayers.map((player) => SidePot(Seq(player), capAmount = data.seats(player).betList.map(_.betValue).sum))

        numPlayerSidePots.foreach((sp) => {
          updatedSidePots = splashToSidePots(updatedSidePots, SidePot(ids = sp.ids, capAmount = sp.capAmount))
            .sortWith((a, b) => a.capAmount < b.capAmount)
            .sortWith((a, b) => a.ids.size > b.ids.size)

        })

        val splashedState = data.copy(
          stage = "16",
          sidePots = updatedSidePots,
          action = s"live players bets are now added to the pots!"
        )

        splashedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)

        val sidePots = updatedSidePots

        var updatedState = data.copy(winners = Seq.empty[Winner])
        var updatedSeats = data.seats.map(seat => seat.copy(winAmount = 0))

        sidePots.reverse.foreach((sp) => {
          /*for each "side-pot" add a winner to the winners list*/

          if (sp.ids.size > 1) {
            /*More than one seats fighting for this pot*/

            val handsToFight = allBestHands.filter(hand =>
              data.seats(hand._1).gameStatus != "FOLDED" //filter out folded hands
                && sp.ids.contains(hand._1))


            val currentWinner = detectWinnerIdOmaha(hands = handsToFight)
            val totalWin = (sp.ids.size * sp.capAmount) + sp.foldAmount
            val winnerHands = detectSameHandsOmaha(currentWinner, hands = handsToFight)

            if (winnerHands.size > 1) {
              /* split needed */

              val splittedWinAmount = totalWin / winnerHands.size
              val splittedRakeAmount = (splittedWinAmount * data.configData.rakePercent * .01)
              val splittedTotalWinAfterRakeCollection = splittedWinAmount - splittedRakeAmount
              val splittedRoundedWin = Math.floor(splittedTotalWinAfterRakeCollection)
              val splittedRoundOff = splittedTotalWinAfterRakeCollection - Math.floor(splittedTotalWinAfterRakeCollection)

              for (winnerHand <- winnerHands) {
                updatedSeats = updatedSeats.map(
                  seat =>
                    if (winnerHands.contains(seat.id))
                      seat.copy(
                        winAmount = seat.winAmount + splittedRoundedWin,
                        bets = seat.bets.updated(0, seat.bets(0) + splittedRoundedWin),
                        gameStatus = s"Win-${allBestHands.find(h => h._1 == seat.id).get._3}",
                        isTurn = true,
                        actions = Seq(false, false, false, false, false)
                      )
                    else seat

                )

                val currentWinningCards = allBestHands.find(item => item._1 == winnerHand).get._2.cards.toList.map(card => card.toString)
                val currentWinningHand = allBestHands.find(item => item._1 == winnerHand).get._3

                updatedState = updatedState.copy(
                  winners = updatedState.winners.:+(
                    Winner(
                      id = winnerHand,
                      winningPot = 0,
                      winAmount = splittedRoundedWin.toInt,
                      rake = splittedRakeAmount + splittedRoundOff,
                      hand = currentWinningHand,
                      cards = currentWinningCards
                    )
                  ),
                  seats = updatedSeats,
                  action = s"seat${winnerHand + 1} won ${splittedRoundedWin.toInt} Pot"
                )

                updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)

              }

              val finalState = updatedState.copy(
                action = s"seats [${winnerHands.map(id => id + 1).mkString(",")}] won ${splittedRoundedWin.toInt} Pot"
              )
              finalState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)

              context.become(TABLE_SHOWDOWN_STATE_16(data = finalState))


            } else {
              val currentWinningCards = allBestHands.find(item => item._1 == currentWinner).get._2.cards.toList.map(card => card.toString)
              val currentWinningHand = allBestHands.find(item => item._1 == currentWinner).get._3

              val rakeAmount = (totalWin * data.configData.rakePercent * .01)
              val totalWinAfterRakeCollection = totalWin - rakeAmount
              val roundedWin = Math.floor(totalWinAfterRakeCollection)
              val roundOff = totalWinAfterRakeCollection - Math.floor(totalWinAfterRakeCollection)

              updatedSeats = updatedSeats.map(
                seat =>
                  if (!seat.isPlaying) seat //Skip the sitOut players
                  else {
                    if (seat.id == currentWinner)
                      seat.copy(
                        winAmount = seat.winAmount + roundedWin,
                        bets = seat.bets.updated(0, seat.bets(0) + roundedWin),
                        gameStatus = s"Win-${allBestHands.find(h => h._1 == seat.id).get._3}",
                        isTurn = true,
                        actions = Seq(false, false, false, false, false)
                      )
                    else
                      seat
                  }
              )

              updatedState = updatedState.copy(
                stage = "16",
                winners = updatedState.winners.:+(
                  Winner(
                    id = currentWinner,
                    winningPot = 0,
                    winAmount = roundedWin.toInt,
                    rake = rakeAmount + roundOff,
                    hand = currentWinningHand,
                    cards = currentWinningCards
                  )
                ),
                seats = updatedSeats,
                sidePots = updatedSidePots,
                action = s"seat${currentWinner} win a pot ${totalWin} fought by [${sp.ids.map(_ + 1).mkString(",")}] win=${roundedWin} rake=${rakeAmount + roundOff}!"
              )

              updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)

            }
          } else {
            /*nobody to fight for this amount, so send it back to balance*/

            updatedSeats = updatedSeats
              .map(seat =>
                if (seat.id == sp.ids.head) seat.copy(
                  balance = seat.balance + sp.capAmount,
                  betList = seat.betList.:+(Bet(updatedState.betIndex, -sp.capAmount, group = RIVER_ROUND, betType = "c"))
                )
                else seat
              )

            updatedState = updatedState
              .copy(
                stage = "16",
                potAmount = updatedState.potAmount - sp.capAmount,
                seats = updatedSeats,
                betIndex = updatedState.betIndex + 1,
                action = s"${sp.capAmount} is reverted back to Seat${sp.ids.head} balance ${updatedSeats(sp.ids.head).balance}"
              )

            updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
          }
        })

        /*Prepare for a final showdown state*/
        log.info(s"Updated winners ${updatedState.winners.map(_.id).mkString(",")}")

        /*You may need to go through each loosing seats and update their state as well ???*/
        val finalSeats = updatedSeats.map(
          seat =>
            if (!seat.isPlaying || updatedState.winners.map(_.id).contains(seat.id)) seat //Skip the sitOut players & Winners
            else {
              if (seat.gameStatus == "FOLDED") {
                seat.copy(
                  winAmount = 0,
                  gameStatus = s"Lost",
                  cards = seat.cards.map(_ => "xx"),
                  isTurn = false,
                  actions = Seq(false, false, false, false, false)
                )
              } else {
                seat.copy(
                  winAmount = 0,
                  gameStatus = s"Lost-${allBestHands.find(h => h._1 == seat.id).get._3}",
                  isTurn = false,
                  actions = Seq(false, false, false, false, false)
                )
              }
            }
        )

        val finalState = updatedState.copy(
          stage = "16",
          seats = finalSeats,
          sidePots = updatedSidePots,
          hands = allBestHands.map(entry => (entry._1, entry._2.cards.map(_.toString).toList, entry._2.joker.map(_.toString).toList, entry._3))
        )

        finalState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
        context.become(TABLE_SHOWDOWN_STATE_16(data = finalState))

      }


    case GameCancel =>
      val updatedData = data
        .recallBetsWithBetLists()
        .prepareForFreshRound()

      updatedData.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
      become(TABLE_STATE_1_READY(data = updatedData))


    case _ => log.error("some message received in TABLE_RIVER_BETTING_CONFIRM_STATE_155 state")
  }

  /*
  * Why a show down state exists because this is the last chance to cancel game and recall bets
  * Once you say go ahead here,
  *  - Send "Bet" transactions of all players playing this game
  *  - Send "Win" transaction for each winners
  *  - Clear all bets from the seats - the source of truth
  *  - Update win details to the winner seat(balance, winAmount)
  *  - Clear the table pots (potAmount, sidePot[], seat bets)
  * */

  def TABLE_SHOWDOWN_STATE_16(data: TableState): Receive = {
    case PlayerConnected(name, actor, client, clientType) => context.become(TABLE_SHOWDOWN_STATE_16(data = handlePlayerConnected(data, name, actor, client, clientType)))
    case TopperConnected(name, actor, client) => context.become(TABLE_SHOWDOWN_STATE_16(data = handleTopperConnected(data, name, actor, client)))
    case AdminConnected(name, actor, client) => context.become(TABLE_SHOWDOWN_STATE_16(data = handleAdminConnected(data, name, actor, client)))
    case PlayerDisConnected(playerIp) => context.become(TABLE_SHOWDOWN_STATE_16(data = handlePlayerDisconnected(data, playerIp)))
    case TopperDisConnected(topperIp) => context.become(TABLE_SHOWDOWN_STATE_16(data = handleTopperDisconnected(data, topperIp)))
    case AdminDisConnected(adminIp) => context.become(TABLE_SHOWDOWN_STATE_16(data = handleAdminDisconnected(data, adminIp)))

    case PollTick =>
      self ! ContinueToNextRound

    case ContinueToNextRound =>

      sendPlayersBetTransactions(
        gameService.getMainActor,
        data.roundId,
        data.seats,
        admins
      );

      sendPlayersWinTransactions(
        actor = gameService.getMainActor,
        gameId = data.roundId,
        winners = data.winners,
        seats = data.seats,
        rake = data.configData.rakePercent,
        admins = admins
      );

      sendRoundTransaction(
        actorRef = gameService.getMainActor,
        data = data,
        admins = admins
      )

      val updatedSeats = updateSeatsBalanceWithResult(winners = data.winners, seats= data.seats)
          .map(seat => seat.clearAllRoundsBets())

      val updatedState = data.copy(stage = "18", potAmount = 0, seats = updatedSeats, action = s"game ${data.roundId} ends")

      updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers = toppers, clients = clients, admins)
      context.become(TABLE_STATE_18_WINNER_SHOWDOWN(data = updatedState))


    case GameCancel =>
      val updatedData = data
        .recallBetsWithBetLists()
        .prepareForFreshRound()

      updatedData.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
      become(TABLE_STATE_1_READY(data = updatedData))


    case _ => log.error("some message received in TABLE_SHOWDOWN_STATE_16 state")
  }


  /*
*
* why a knockdown state exists because anytime a player could become the sole champion during the rounds of betting
* Once you enter here,
*  - Send "Bet" transactions of all players playing this game
*  - Send "Win" transaction for each winners
*  - Update the seats bets, balance
* */
  def TABLE_SHOWDOWN_KO_STATE_17(data: TableState): Receive = {
    case PlayerConnected(name, actor, client, clientType) => context.become(TABLE_SHOWDOWN_KO_STATE_17(data = handlePlayerConnected(data, name, actor, client, clientType)))
    case TopperConnected(name, actor, client) => context.become(TABLE_SHOWDOWN_KO_STATE_17(data = handleTopperConnected(data, name, actor, client)))
    case AdminConnected(name, actor, client) => context.become(TABLE_SHOWDOWN_KO_STATE_17(data = handleAdminConnected(data, name, actor, client)))
    case PlayerDisConnected(playerIp) => context.become(TABLE_SHOWDOWN_KO_STATE_17(data = handlePlayerDisconnected(data, playerIp)))
    case TopperDisConnected(topperIp) => context.become(TABLE_SHOWDOWN_KO_STATE_17(data = handleTopperDisconnected(data, topperIp)))
    case AdminDisConnected(adminIp) => context.become(TABLE_SHOWDOWN_KO_STATE_17(data = handleAdminDisconnected(data, adminIp)))

    case PollTick =>
      self ! ContinueToNextRound
    case ContinueToNextRound =>

      sendPlayersBetTransactions(
        gameService.getMainActor,
        data.roundId,
        data.seats,
        admins
      );


      sendPlayersWinTransactions(
        actor = gameService.getMainActor,
        gameId = data.roundId,
        winners = data.winners,
        seats = data.seats,
        rake = data.configData.rakePercent,
        admins = admins
      )


      sendRoundTransaction(
        actorRef = gameService.getMainActor,
        data = data,
        admins = admins
      )

      val updatedSeats =
        updateSeatsBalanceWithResult(winners = data.winners, seats= data.seats)
        .map(seat => seat.clearAllRoundsBets())

      val updatedSeatsWithMucking = updatedSeats.map(seat => if(!seat.isPlaying) seat else seat.copy(cards = seat.cards.map(_ => "xx")))
      val updatedState = data.copy(stage = "18", potAmount = 0, seats = updatedSeats, action = s"game ${data.roundId} ended")
      val updatedStateWithMucking = updatedState.copy(seats = updatedSeatsWithMucking)

      updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers = MMap.empty[String, ClientData], clients = clients, admins = MMap.empty[String, AdminClientData])
      updatedStateWithMucking.sendStateUpdateToClients(gameService.getMainActor, toppers = toppers , clients = MMap.empty[String, ClientData], admins = admins)

      context.become(TABLE_STATE_18_WINNER_SHOWDOWN(data = updatedState))

    case GameCancel =>
      val updatedData = data
        .recallBetsWithBetLists()
        .prepareForFreshRound()

      updatedData.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
      become(TABLE_STATE_1_READY(data = updatedData))



    case _ => log.error("some message received in TABLE_SHOWDOWN_KO_STATE_17 state")
  }


  def TABLE_STATE_18_WINNER_SHOWDOWN(data: TableState): Receive = {

    case TableSettingsChange(pokerVariant, betLimit, liveDealer, tournamentMode, playerCardsConfig, communityCardsConfig, rakePercent, blind) => context.become(TABLE_STATE_18_WINNER_SHOWDOWN(data = handleTableSettingsChange(data, pokerVariant, betLimit, liveDealer, tournamentMode, playerCardsConfig, communityCardsConfig, rakePercent, blind)))


    case PlayerConnected(name, actor, client, clientType) => context.become(TABLE_STATE_18_WINNER_SHOWDOWN(data = handlePlayerConnected(data, name, actor, client, clientType)))
    case TopperConnected(name, actor, client) => context.become(TABLE_STATE_18_WINNER_SHOWDOWN(data = handleTopperConnected(data, name, actor, client)))
    case AdminConnected(name, actor, client) => context.become(TABLE_STATE_18_WINNER_SHOWDOWN(data = handleAdminConnected(data, name, actor, client)))
    case PlayerDisConnected(playerIp) => context.become(TABLE_STATE_18_WINNER_SHOWDOWN(data = handlePlayerDisconnected(data, playerIp)))
    case TopperDisConnected(topperIp) => context.become(TABLE_STATE_18_WINNER_SHOWDOWN(data = handleTopperDisconnected(data, topperIp)))
    case AdminDisConnected(adminIp) => context.become(TABLE_STATE_18_WINNER_SHOWDOWN(data = handleAdminDisconnected(data, adminIp)))


    case NewGameCmd =>
      val nextSeatIdsArr = data.seats.filter(s => s.balance >= data.configData.blind * 2).map(seat => seat.id) //new set of players with min balance
      val dealerSeatId = data.seats.find(s => s.isDealer).get.id //last  game dealer seat id
      val sbSeatId = data.seats.find(s => s.isSmallBet).get.id //last game sb seat id
      val bbSeatId = data.seats.find(s => s.isBigBet).get.id //last game sb seat id

      val updatedData = data.copy(numPlayers = nextSeatIdsArr)

      //val nextTurnPosition = findNextTurnPosition(dealerSeatId, nextSeatIdsArr)
      val nextTurnPosition: Int = if (nextSeatIdsArr.size >= 2) {
        if(nextSeatIdsArr.contains(dealerSeatId)) updatedData.getPlayerPositions(nextSeatIdsArr.indexWhere(p => p == dealerSeatId))._2
        else if(nextSeatIdsArr.contains(sbSeatId)) updatedData.getPlayerPositions(nextSeatIdsArr.indexWhere(p => p == sbSeatId))._1
        else if(nextSeatIdsArr.contains(bbSeatId)) updatedData.getPlayerPositions(nextSeatIdsArr.indexWhere(p => p == bbSeatId))._1
        else 0
      } else 0

      if(nextSeatIdsArr.size >= 2) {
        val finalData = data
          .resetTableCards()
          .resetSeats()
          .resetTableWinnerDetails()
          .copy(
            stage = "3",
            roundId = data.getRoundId(),
            numPlayers = nextSeatIdsArr,
            dealerId = nextSeatIdsArr(nextTurnPosition),
            turnId = nextTurnPosition,
          )

        finalData.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
        become(TABLE_ELECTION_END_STATE_3(data = finalData))
      } else {
        val finalData = data
          .resetTableCards()
          .resetSeats()
          .resetTableWinnerDetails()
          .copy(
            stage = "1",
            roundId = data.getRoundId(),
            numPlayers = nextSeatIdsArr,
            dealerId = nextSeatIdsArr(nextTurnPosition),
            turnId = nextTurnPosition,
          )

        finalData.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)
        become(TABLE_STATE_1_READY(data = finalData))
      }


    case PollTick =>
    case unknown        => log.error(s"some ${unknown} received in TABLE_STATE_18_WINNER_SHOWDOWN state")
  }







  def reloadPlayerBalances(tableState: TableState): TableState = {
    val players: Seq[Player] = gameService.getPlayersData("4000")
    val playersBalance: Map[String, Double] = players.map(p => p.uid -> p.balance).toMap
    val newTableSeats = tableState.seats.map(seat => seat.copy(balance = playersBalance.getOrElse(seat.uid, seat.balance)))

    //Update seat ids based on seat balances
    val minBalance = tableState.configData.blind * 2
    val activeSeatIdsArr = newTableSeats.filter(s => s.balance >= minBalance).map(seat => seat.id)
    tableState.copy(seats = newTableSeats, numPlayers = activeSeatIdsArr) //reloaded balances
  }

  def handleSkipElection(tableState: TableState): TableState = {
    val updatedData = reloadPlayerBalances(tableState)
    val numPlayers = updatedData.numPlayers

    if (numPlayers.size >= 2) {
      var skipElectionAction = ""
      val selectedDealerIndex = if ((updatedData.dealerId == -1) && (!numPlayers.contains(updatedData.dealerId))) {
        skipElectionAction = s"skip election, dealer is seat${numPlayers.head + 1}"
        0
      } else {
        skipElectionAction = s"dealer is auto selected to seat${updatedData.dealerId + 1}"
        numPlayers.indexWhere(p => p == updatedData.dealerId)
      }
      /*Important: use dataWithNumPlayers not data here to select player positions, isn't it better to move it out of tableState ??*/
      val (dealerPos, smallBetPos, bigBetPos, initialTurnPos) = updatedData.getPlayerPositions(selectedDealerIndex)

      val dealerId = numPlayers(dealerPos)
      val smallBetId = numPlayers(smallBetPos)
      val bigBetId = numPlayers(bigBetPos)
      val seatsWithDealer = updatedData.fillSeatsWithElectionResult(dealerId, smallBetId, bigBetId)

      val finalData = updatedData
        .copy(stage = "3")
        .copy(
          dealerId = dealerId,
          smallBetId = smallBetId,
          bigBetId = bigBetId,
          turnId = smallBetPos,
          numPlayers = numPlayers,
          seats = seatsWithDealer
        )

      finalData.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)

      updatedData.printTablePlayersStartState()

      finalData

    } else {
      updatedData
    }
  }

  def handleTopperConnected(tableState: TableState, playerIp: String, actor: ActorRef, client: ActorRef): TableState = {

    if (toppers.contains(playerIp)) {
      toppers(playerIp) = toppers(playerIp).copy(actor = actor, client = client)
    } else {
      toppers = toppers ++ Map(playerIp -> ClientData(actor = actor, client = client))

    }

    val clientSocket = toppers(playerIp).client

    val tableData = tableState.stage match {
      case "1" | "2" | "16" | "18" => tableState.toTableData
      case _ => tableState.copy(seats = tableState.getSeatsWithCardsMasked()).toTableData
    }

    val initialDataMessage = AdminInitialDataMsg(MessageType = "InitialData",
      tableId = "4000",
      destination = "topper",
      clientId = playerIp,
      roundId = tableState.roundId,
      timestamp = Instant.now().toString,
      data = tableData,
      logs = Seq.empty,
      players = Seq.empty,
      transactions = Seq.empty,
      operations = Seq.empty,
    )

    clientSocket ! Json.toJson(initialDataMessage)

    tableState.copy()

  }

  def handleAdminConnected(tableState: TableState, name: String, actor: ActorRef, client: ActorRef): TableState = {

    if (admins.contains(name)) {
      if (admins(name).actor != null) admins(name).actor ! Reconnected
      admins(name) = admins(name).copy(actor = actor, client = client)
    } else {
      admins = admins ++ Map(name -> AdminClientData(actor = actor, client = client, name = name))
    }

    val clientSocket = admins(name).client
    val clientName = admins(name).name

    val tableData = tableState.stage match {
      case "1" | "2" | "16" | "17" => tableState.toTableData
      case _ => tableState.copy(seats = tableState.getSeatsWithCardsMasked()).toTableData
    }

    val initialDataMessage = AdminInitialDataMsg(MessageType = "InitialData",
      tableId = tableId ,
      clientId = clientName,
      roundId = tableState.roundId,
      timestamp = Instant.now().toString,
      data = tableData,
      logs = Seq.empty,
      players = gameService.getPlayersData(tableId = tableId),
      transactions = gameService.getTransactions(),
      operations = gameService.getOperationTransactions(),
      history = gameService.getRoundTransactions()
    )

    clientSocket ! Json.toJson(initialDataMessage)

    tableState.copy()

  }


  def handleTopperDisconnected(tableState: TableState, topperIp: String) : TableState = {
    if(toppers.contains(topperIp)) {
      toppers.remove(topperIp)
      log.error("Topper Disconnected!!!")
      tableState
    } else {
      tableState
    }
  }
  def handleAdminDisconnected(tableState: TableState, adminIp: String) : TableState = {
    if(admins.contains(adminIp)) {
      admins.remove(adminIp)
      log.error("Admin Disconnected!!!")
      tableState
    } else {
      tableState
    }
  }

  def handlePlayerConnected(tableState: TableState, playerIp: String, actor: ActorRef, client: ActorRef, clientType: String): TableState = {
    val updatedState = if (clients.contains(playerIp)) {
//      log.info("Player Trying to Reconnect...")
      val playerUid = clients(playerIp).uid
      clients(playerIp) = clients(playerIp).copy(actor = actor, client = client, clientType = clientType)


      val clientSocket = clients(playerIp).client


      val tableData = tableState.stage match {
        case "1" | "2" | "16" | "18" => tableState.toTableData
        case _ => tableState.copy(seats = tableState.getSeatsWithCardsMasked(playerUid)).toTableData
      }

      val initialDataMessage = AdminInitialDataMsg(MessageType = "InitialData",
        tableId = "4000",
        destination = "player",
        clientId = playerUid,
        roundId = tableState.roundId,
        timestamp = Instant.now().toString,
        data = tableData,
        logs = Seq.empty,
        players = Seq.empty,
        transactions = Seq.empty,
        operations = Seq.empty,
      )

      if (clientType == "web") {
        clientSocket ! Json.toJson(initialDataMessage)
      } else {
        clientSocket ! Json.stringify(Json.toJson(initialDataMessage)).getBytes(StandardCharsets.UTF_8)
      }
      
//      log.info(s"ReConnected as Player ${playerUid} from ${playerIp}")

      tableState.copy(
        seats = tableState.seats.map(seat => if (seat.uid == playerUid) seat.copy(connected = true) else seat)
      )


    } else {
//      log.info("A Player Trying to connect...")

      val playerOpt = gameService.getPlayersData(tableId = "4000").find(_.clientIp == playerIp)
      playerOpt match {
        case Some(matchingPlayer) =>
          /*Straight Case - A Player Trying to connect from a known ip*/
          val playerUid = matchingPlayer.uid
          clients = clients ++ Map(playerIp -> ClientData(playerIp = playerIp, uid = playerUid, actor = actor, client = client, clientType = clientType))
          gameService.getMainActor ! PlayerStatusOnline(playerUid, admins)

          val clientSocket = clients(playerIp).client

          val tableData = tableState.stage match {
            case "1" | "2" | "16" | "18" => tableState.toTableData
            case _ => tableState.copy(seats = tableState.getSeatsWithCardsMasked(playerUid)).toTableData
          }

          val initialDataMessage = AdminInitialDataMsg(MessageType = "InitialData",
            tableId = "4000",
            destination = "player",
            clientId = playerUid,
            roundId = tableState.roundId,
            timestamp = Instant.now().toString,
            data = tableData,
            logs = Seq.empty,
            players = gameService.getPlayersData(tableId = "4000"),
            transactions = Seq.empty,
            operations = Seq.empty,
          )

          if(clientType == "web"){
            clientSocket ! Json.toJson(initialDataMessage)
          } else {
            clientSocket ! Json.stringify(Json.toJson(initialDataMessage)).getBytes(StandardCharsets.UTF_8)
          }

          log.info(s"Connected as Player ${playerUid} from ${playerIp}")
          tableState.copy(
            seats = tableState.seats.map(seat => if (seat.uid == playerUid) seat.copy(connected = true) else seat)
          )

        case None =>
          /*Special Case - A Player Trying to connect from an unknown ip
          * 1. find a seat which is not connected and has empty balance
          * */

          val freeSeat = tableState.getAnEmptySeatWithZeroBalance()

          if (freeSeat.id != -1) {
            log.info(s"A Free Seat found with uid => ${freeSeat.uid}")

            val freeSeatPlayerOpt = gameService.getPlayersData(tableId = "4000").find(_.uid == freeSeat.uid)

            freeSeatPlayerOpt match {
              case Some(foundPlayer) =>
                val playerUid = foundPlayer.uid

                clients = clients ++ Map(playerIp -> ClientData(playerIp = playerIp, uid = playerUid, actor = actor, client = client, clientType = clientType))

                gameService.getMainActor ! PlayerIpUpdate(playerUid, playerIp)
                gameService.getMainActor ! PlayerStatusOnline(playerUid, admins)

                val clientSocket = clients(playerIp).client


                val tableData = tableState.stage match {
                  case "1" | "2" | "16" | "18" => tableState.toTableData
                  case _ => tableState.copy(seats = tableState.getSeatsWithCardsMasked(playerUid)).toTableData
                }

                val initialDataMessage = AdminInitialDataMsg(MessageType = "InitialData",
                  tableId = "4000",
                  destination = "player",
                  clientId = playerUid,
                  roundId = tableState.roundId,
                  timestamp = Instant.now().toString,
                  data = tableData,
                  logs = Seq.empty,
                  players = gameService.getPlayersData(tableId = "4000"),
                  transactions = Seq.empty,
                  operations = Seq.empty,
                )

                if (clientType == "web") {
                  clientSocket ! Json.toJson(initialDataMessage)
                } else {
                  clientSocket ! Json.stringify(Json.toJson(initialDataMessage)).getBytes(StandardCharsets.UTF_8)
                }

                log.info(s"Connected as Player ${playerUid} from ${playerIp}")

                tableState.copy(
                  seats = tableState.seats.map(seat => if (seat.uid == playerUid) seat.copy(connected = true) else seat)
                )
              case None =>
                log.info("Strange case , somehow a free seat uid is not found in users list")
                tableState
            }

          } else {
            log.info("Failed...No Empty Seat")
            tableState
          }
      }

    }

    updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)

    updatedState

  }

  def handlePlayerDisconnected(tableState: TableState, playerIp: String): TableState = {
    val updatedState = if (clients.contains(playerIp)) {
      val playerUid = clients(playerIp).uid

      clients.remove(playerIp)
      gameService.getMainActor ! PlayerStatusOffline(playerUid, admins)

      log.info(s"Client ${playerIp} Disconnecting..")

      if (tableState.seats.exists(_.uid == playerUid)) {
        log.info(s"Client ${playerIp} Disconnected From Seat uid${playerUid}")

        tableState.copy(
          seats = tableState.seats.map(seat => if (seat.uid == playerUid) seat.copy(connected = false) else seat)
        )


      } else {
        tableState
      }

    } else {
      tableState
    }

    updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)

    updatedState
  }


  def handlePlayerBalanceUpdated(tableState: TableState, uid: String, balance: Double): TableState = {
    val updatedState = if (tableState.seats.exists(_.uid == uid)) {
      log.info(s"Seat ${uid} Balance Updated to ${balance}")

      //1. update seat balance
      val updatedTableState = tableState.copy(
        seats = tableState.seats.map(seat =>
          if (seat.uid == uid) {
            seat.copy(balance = balance)
          }
          else
            seat
        )
      )
      //2. update table
      //updating numPlayers array here, be careful :-(
      val numPlayers = updatedTableState.seats.filter(s => s.balance >= updatedTableState.configData.blind * 2).map(seat => seat.id)

      updatedTableState.copy(
        numPlayers = numPlayers,
        dealerId = -1,
        turnId = 0,
        seats = updatedTableState.seats.map(seat =>
          if(numPlayers.contains(seat.id)) seat.copy(isPlaying = true, gameStatus = "Playing")
          else seat.copy(isPlaying = false, gameStatus = "Sit Out")),
        action = s"seat${uid} balance updated to ${balance.toInt}, "
      )
    } else {
      tableState
    }
    updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)

    updatedState
  }

  def handleTableSettingsChange(tableState: TableState, pokerVariant: String, betLimit: String, liveDealer: Boolean, tournamentMode: Boolean, playerCardsConfig: CardsConfig, communityCardsConfig: CardsConfig, rakePercent: Int, blind: Int): TableState = {
    val updatedConfigData = tableState.configData.copy(pokerVariant, betLimit, liveDealer, tournamentMode, playerCardsConfig, communityCardsConfig, rakePercent, blind)
    val updatedState = tableState.copy(
      configData = updatedConfigData,
      action = s"Settings Changed Variant->${pokerVariant}, BetLimit->${betLimit}, Min Bet->${blind}"
    )
    updatedState.sendStateUpdateToClients(gameService.getMainActor, toppers, clients, admins)

    updatedState
  }


}


trait PokerUtilities extends PokerCodecs {


  def detectWinnerIdTexas(hands: Seq[(Int, Hand, String)]): Int = {

    /*Recursion is cool way to solve "looping" problems in a functional way
    * Walk over all of the elements to return a final single value - a sign that you may want to use reduce in scala*/
    val winner = hands.reduce((winner, cur) => if (cur._2 beatPoker winner._2) cur else winner)
    winner._1
  }

  def detectWinnerIdOmaha(hands: Seq[(Int, Hand, String)]): Int = {

    /*Recursion is cool way to solve "looping" problems in a functional way
    * Walk over all of the elements to return a final single value - a sign that you may want to use reduce in scala*/
    val winner = hands.reduce((winner, cur) => if (cur._2 beatPoker winner._2) cur else winner)
    winner._1
  }


  def detectSameHandsTexas(winner: Int, hands: Seq[(Int, Hand, String)]): Seq[Int] = {
    /*Function is to filter and find the seats with score equal to winner hand*/
    val winnerHand = hands.find(hand => hand._1 == winner).get

    hands.filter((hand) => {
      println(s"###########################TEXAS START ${hand._1} vs ${winnerHand._1} ############################")
      val result = Hand.orderingPoker5Card.compare(hand._2, winnerHand._2)
      println(s"###########################END ${hand._1} - ${winnerHand._1} = ${result}############################")
      result == 0
    }).map(_._1)


  }


  def detectSameHandsOmaha(winner: Int, hands: Seq[(Int, Hand, String)]): Seq[Int] = {
    /*Function is to filter and find the seats with score equal to winner hand*/
    val winnerHand = hands.find(hand => hand._1 == winner).get

    hands.filter((hand) => {
      println(s"###########################OMAHA START ${hand._1} vs ${winnerHand._1} ############################")
      val result = Hand.orderingPoker5Card.compare(hand._2, winnerHand._2)
      println(s"###########################END ${hand._1} - ${winnerHand._1} = ${result}############################")
      result == 0
    }).map(_._1)

  }





  def sidePotsString(sidePots: Seq[SidePot]): String = {
    s"[${sidePots.map(p => ("[" + p.ids.mkString(",") + "]", p.capAmount, "[" + p.fids.mkString(",") + "]", p.foldAmount )).mkString(",")}]"
  }


  def sidePotString(sidePot: SidePot): String = {
    s"[${sidePot.ids.mkString(",")}] (${sidePot.capAmount})"

  }

  def splashToSidePots(sidePots: Seq[SidePot], allInSp: SidePot): Seq[SidePot] = {

    println(sidePotString(allInSp) + "=> " + sidePotsString(sidePots))

    if(allInSp.capAmount == 0 ) {
      println("<=" + sidePotsString(sidePots))
      return sidePots //end case for call
    } else if(sidePots.exists(sp => sp.capAmount <= allInSp.capAmount && !sp.ids.contains(allInSp.ids.last) ))  {
      //smaller pots not yet joined
      //join and splash again with reduced amount
      val foundIndex = sidePots.indexWhere(sp => sp.capAmount <= allInSp.capAmount && !sp.ids.contains(allInSp.ids.last))
      val foundPot = sidePots(foundIndex)

      splashToSidePots(
        sidePots.updated(foundIndex, foundPot.copy(ids = (allInSp.ids ++ foundPot.ids).distinct)),
        allInSp.copy(capAmount = allInSp.capAmount - foundPot.capAmount),
      )
    } else if(sidePots.exists(sp => sp.capAmount > allInSp.capAmount && !sp.ids.contains(allInSp.ids.last)))  {
      //Swap it with the first Bigger pot
      val foundIndex = sidePots.indexWhere(sp => sp.capAmount > allInSp.capAmount && !sp.ids.contains(allInSp.ids.last))
      val foundPot = sidePots(foundIndex)

      val updatedSidePots = sidePots.updated(foundIndex, allInSp.copy(ids = (allInSp.ids ++ foundPot.ids).distinct))
      val updatedSidePot = foundPot.copy(capAmount = foundPot.capAmount - allInSp.capAmount)
      println("<=" + sidePotsString(updatedSidePots.:+(updatedSidePot)))
      return updatedSidePots.:+(updatedSidePot)
    } else {
      println("<=" + sidePotsString(sidePots.:+(allInSp)))
      return sidePots.:+(allInSp)
    }

  }


  def sendPlayersBetTransactions(actor : ActorRef, gameId: Long, seats: Seq[Seat], admins: MMap[String, AdminClientData]): Unit = {
    seats
      .filter(_.isPlaying)//very important, bets are sent for each playing seat even though they folded without bets
      .foreach(seat => {
        val totalBet = seats(seat.id).betList.map(_.betValue).sum
        actor ! PlayerGameTransaction(
          GameTransaction(
            roundId = gameId,
            player = seat.uid,
            transType = "Bet",
            oldBalance = seat.balance - totalBet,
            balance = seat.balance,
            totalBet = totalBet,
          ),
          admins = admins
        )
      })
  }


  /*
  *
  *
  * */
  def sendRoundTransaction(actorRef: ActorRef, data: TableState, admins: MMap[String, AdminClientData]) = {
    val dateFormat = new SimpleDateFormat("E, dd MMM yyyy HH:mm:ss.SSS z")


    val TableState(
      roundId,
      configData,
      action,
      dealerId, smallBetId, bigBetId, turnId,
      numPlayers,
      potAmount, potLimit,
      betIndex, betAmount, raiseAmount,
      stage,
      winners,
      gameCards,
      cards,
      sidePots,
      seats,
      hands) = data

    val tableId = (configData.pokerVariant, configData.betLimit) match {
      case ("Texas", "Limit") => "71"
      case ("Texas", "Pot Limit") => "72"
      case ("Texas", "No Limit") => "73"
      case ("Omaha", "Limit") => "81"
      case ("Omaha", "Pot Limit") => "82"
      case ("Omaha", "No Limit") => "83"
      case ("Pineapple", "Limit") => "91"
      case ("Pineapple", "Pot Limit") => "92"
      case ("Pineapple", "No Limit") => "93"
      case _ => "99"
    }
    val gameName = (configData.pokerVariant, configData.betLimit) match {
      case ("Texas", "Limit") => "Texas Fixed Limit"
      case ("Texas", "Pot Limit") => "Texas Pot Limit"
      case ("Texas", "No Limit") => "Texas No Limit"
      case ("Omaha", "Limit") => "Omaha Fixed Limit"
      case ("Omaha", "Pot Limit") => "Omaha Pot Limit"
      case ("Omaha", "No Limit") => "Omaha No Limit"
      case ("Pineapple", "Limit") => "Pineapple Fixed Limit"
      case ("Pineapple", "Pot Limit") => "Pineapple Pot Limit"
      case ("Pineapple", "No Limit") => "Pineapple No Limit"
      case _ => "Community Poker"
    }

//    def hand(id: Int): Seq[String] = configData.pokerVariant match {
//      case "Texas" =>
//        val allBestHands: Seq[(Int, Hand, String)] = data.getAllPlayersHandsTexas()
//        if(data.seats(id).gameStatus.contains("Win-") || data.seats(id).gameStatus.contains("Lost-") )  {
//          if(allBestHands.find(item => item._1 == id).get._2.cards.nonEmpty) {
//            allBestHands.find(item => item._1 == id).get._2.cards.map(card => card.toString)
//          } else {
//            Seq.empty[String]
//          }
//        } else {
//          Seq.empty[String]
//        }
//      case "Omaha" =>
//        val allBestHands: Seq[(Int, Hand, String)] = data.getAllPlayersHandsOmaha()
//        if(data.seats(id).gameStatus.contains("Win-") || data.seats(id).gameStatus.contains("Lost-") )  {
//          if (allBestHands.find(item => item._1 == id).get._2.cards.nonEmpty) {
//            allBestHands.find(item => item._1 == id).get._2.cards.map(card => card.toString)
//          } else {
//            Seq.empty[String]
//          }
//        } else {
//          Seq.empty[String]
//        }
//      case _ => Seq("xx", "xx", "xx", "xx", "xx")
//    }
//
//    def score(id: Int) = configData.pokerVariant match {
//      case "Texas" =>
//        val allBestHands: Seq[(Int, Hand, String)] = data.getAllPlayersHandsTexas()
//        if(data.seats(id).gameStatus.contains("Win-") || data.seats(id).gameStatus.contains("Lost-") )   {
//          allBestHands.find(item => item._1 == id).get._3
//        } else {
//          ""
//        }
//      case "Omaha" =>
//        val allBestHands: Seq[(Int, Hand, String)] = data.getAllPlayersHandsOmaha()
//        if(data.seats(id).gameStatus.contains("Win-") || data.seats(id).gameStatus.contains("Lost-") )   {
//          allBestHands.find(item => item._1 == id).get._3
//
//        } else {
//          ""
//        }
//      case _ => "Holdem??"
//    }

    val gameResult = GameResult(
      roundId = roundId,
      configData = configData,
      gameCards = gameCards,
      winners = winners,
      seats = seats.map(s =>
        PokerSeat(
          id = s.id,
          uid = s.uid,
          cards = s.cards,
          hand  = if(data.seats(s.id).gameStatus.contains("Win-") || data.seats(s.id).gameStatus.contains("Lost-") ) hands.find(_._1 == s.id).get._2 else Seq.empty[String],
          score = if(data.seats(s.id).gameStatus.contains("Win-") || data.seats(s.id).gameStatus.contains("Lost-") ) hands.find(_._1 == s.id).get._4 else "Folded",
          betList = s.betList,
          winAmount = s.winAmount,
          isDealer = s.isDealer,
          isPlaying = s.isPlaying,
          gameStatus = s.gameStatus))
    )

    actorRef ! PlayerRoundTransaction(
      roundTransactionRecord = RoundTransactionMsg(
        MessageType = "ROUND_RESULT",
        transType = "Win",
        tableId = tableId,
        gameName = gameName,
        roundId = roundId,
        winningHand = winners.map(w => w.hand),
        gameResult = Json.toJson(gameResult).toString(),
        playersTotalBet = seats.map(s => (s.uid, s.betList.map(_.betValue).sum)).toList,
        playerBetsList = Json.toJson(seats.flatMap(s => s.betList)).toString(),
        timestamp = dateFormat.format(Calendar.getInstance().getTime)
      ),
      admins = admins
    )
  }

  def sendPlayersWinTransactions(actor: ActorRef, gameId: Long, winners: Seq[Winner], seats: Seq[Seat], rake: Int, admins: MMap[String, AdminClientData]): Unit = {

    //for each winner
    winners.foreach(winner => {
      val oldBalance = seats(winner.id).balance

      val totalWin = winner.winAmount
      val totalBet = seats(winner.id).betList.map(_.betValue).sum
      val rakeCollected = winner.rake

        actor ! PlayerGameTransaction(
          GameTransaction(
            roundId = gameId,
            rake = rakeCollected ,
            player = seats(winner.id).uid,
            transType = "Win",
            oldBalance = oldBalance,
            balance = oldBalance + totalWin,
            totalBet = totalBet,
            totalWin = totalWin,
          ),
          admins = admins
        )
      })
  }

  def updateSeatsBalanceWithResult(winners: Seq[Winner], seats: Seq[Seat]): Seq[Seat] = {

    var updatedSeats = seats //TODO shall be updated later with a var less solution

    //for each winner, seats shall be updated with balance
    winners.foreach(winner => {
      updatedSeats = updatedSeats.map(seat =>
          if (seat.id == winner.id) {
            seat.copy(balance = seat.balance + winner.winAmount)
          }
          else seat
        )
    })

    updatedSeats.map(seat =>
      if (winners.map(_.id).contains(seat.id) ) {
        seat.copy(
          totalBet = seat.betList.map(_.betValue).sum,
          lastWin = winners.filter(w => w.id == seat.id).map(_.winAmount).sum
        )
      }
      else seat.copy(totalBet = seat.betList.map(_.betValue).sum )
    );

  }

  def updateSeatsBalanceWithResultRefactored(winners: Seq[Winner], seats: Seq[Seat]): Seq[Seat] = {
    val winnersSeats = winners.map(_.id)

    //map through seats if winners seats has its id then update its balance with the corresponding winAmount
    seats.map(seat =>
      if(winnersSeats.contains(seat.id))
        seat.copy(balance = seat.balance + winners.find(w => w.id == seat.id).get.winAmount)
      else
        seat
    )

  }


}
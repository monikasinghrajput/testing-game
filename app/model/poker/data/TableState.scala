package model.poker.data

import akka.actor.ActorRef
import model.common.messages.{AdminClientData, ClientData, GameTransaction}
import model.poker.{Card, Deck52, Hand, Selection}
import play.api.Logger
import play.api.libs.json.Json

import java.text.SimpleDateFormat
import java.util.Calendar
import scala.collection.mutable.{Map => MMap}
import actors.MainActor.{PlayerGameTransaction, TableStateUpdated}
import model.poker.codecs.AdminCodecs

import java.nio.charset.StandardCharsets

case class TableState(roundId: Long = 1,
                      configData: ConfigData = ConfigData(),
                      action: String = "",
                      dealerId: Int = -1, //position of dealer in numPlayers array
                      smallBetId: Int = -1,//position of sb in numPlayers array
                      bigBetId: Int = -1,//position of sb in numPlayers array
                      turnId: Int = 0,//position of current turn in numPlayers array
                      numPlayers: Seq[Int] = Seq(0,1,2,3,4,5,6,7),
                      potAmount: Double = 0.0,
                      potLimit: Double = 0.0,
                      betIndex: Int = 1,
                      betAmount: Double = 0.0,
                      raiseAmount: Double = 0.0,
                      stage: String = "1",
                      winners: Seq[Winner] = Seq.empty[Winner],
                      gameCards: Seq[String] = Seq.empty[String],
                      cards: List[String] = Deck52.freshCards.map(card => card.toString),
                      sidePots: Seq[SidePot] = Seq.empty[SidePot],
                      seats: Seq[Seat] = Seq.empty[Seat],
                      hands: Seq[(Int, List[String], List[String], String)] = Seq.empty[(Int, List[String], List[String], String)]
                     )
  extends AdminCodecs
  with PokerUtilities {

  val log = Logger(this.getClass)
  val dateFormat = new SimpleDateFormat("E, dd MMM yyyy HH:mm:ss.SSS z")

  def getRoundId(): Long = {
    val game = (configData.pokerVariant, configData.betLimit) match {
      case ("Texas", "Limit") => "71"
      case ("Texas", "Pot Limit") => "72"
      case ("Texas", "No Limit") => "73"
      case ("Omaha", "Limit") => "81"
      case ("Omaha", "Pot Limit") => "82"
      case ("Omaha", "No Limit") => "83"
      case _ => "99"
    }
    val gameId = game.toInt
    val week = java.time.LocalDateTime.now().getDayOfYear
    val hour = java.time.LocalDateTime.now().getHour
    val minute = java.time.LocalDateTime.now().getMinute
    val second = java.time.LocalDateTime.now().getSecond / 10

    String.format("%02d%03d%02d%02d%1d", gameId, week, hour, minute, second).toLong
  }

  /*############################## POKER METHODS  ################################### */

  //numPlayers -- seats fighting for the main pot created at the start of every game
  //remove a seat from the list for "fold", and "all-in"
  //


  def getAllPlayersHandsTexas(): Seq[(Int, Hand, String)] = {
    val allPlayersHandsTexas = for {
      seat <- this.seats
      if this.numPlayers.contains(seat.id) || seat.gameStatus == "ALL IN" || seat.gameStatus.contains("Lost-") || seat.gameStatus.contains("Win-")
    } yield (
      seat.id,
      Hand(
        cards = Hand(
          cards = List(
            Card.parseCard(this.gameCards(0)).get,
            Card.parseCard(this.gameCards(1)).get,
            Card.parseCard(this.gameCards(2)).get,
            Card.parseCard(this.gameCards(3)).get,
            Card.parseCard(this.gameCards(4)).get
          ),
          cardsJoker = List(
            Card.parseCard(seat.cards(0)).get,
            Card.parseCard(seat.cards(1)).get
          )
        ).selectedCardsTexasArr.head.selected.productIterator.toList.map(card => Card.parseCard(card.toString).get),
        cardsJoker = Hand(
          cards = List(
            Card.parseCard(this.gameCards(0)).get,
            Card.parseCard(this.gameCards(1)).get,
            Card.parseCard(this.gameCards(2)).get,
            Card.parseCard(this.gameCards(3)).get,
            Card.parseCard(this.gameCards(4)).get
          ),
          cardsJoker = List(
            Card.parseCard(seat.cards(0)).get,
            Card.parseCard(seat.cards(1)).get
          )
        ).selectedCardsTexasArr.head.remaining.toList.map(card => Card.parseCard(card.toString).get)

      ),
      Hand(
        cards = Hand(
          cards = List(
            Card.parseCard(this.gameCards(0)).get,
            Card.parseCard(this.gameCards(1)).get,
            Card.parseCard(this.gameCards(2)).get,
            Card.parseCard(this.gameCards(3)).get,
            Card.parseCard(this.gameCards(4)).get
          ),
          cardsJoker = List(
            Card.parseCard(seat.cards(0)).get,
            Card.parseCard(seat.cards(1)).get
          )
        ).selectedCardsTexasArr.head.selected.productIterator.toList.map(card => Card.parseCard(card.toString).get),
        cardsJoker = Hand(
          cards = List(
            Card.parseCard(this.gameCards(0)).get,
            Card.parseCard(this.gameCards(1)).get,
            Card.parseCard(this.gameCards(2)).get,
            Card.parseCard(this.gameCards(3)).get,
            Card.parseCard(this.gameCards(4)).get
          ),
          cardsJoker = List(
            Card.parseCard(seat.cards(0)).get,
            Card.parseCard(seat.cards(1)).get
          )
        ).selectedCardsTexasArr.head.remaining.toList.map(card => Card.parseCard(card.toString).get)

      ).score5CardPoker
    )



    allPlayersHandsTexas

  }



  def getAllPlayersHandsOmaha(): Seq[(Int, Hand, String)] = {

    val allPlayersHandsOmaha = for {
      seat <- this.seats
      if this.numPlayers.contains(seat.id) || seat.gameStatus == "ALL IN" || seat.gameStatus.contains("Lost-") || seat.gameStatus.contains("Win-")
    } yield (
      seat.id,
      Hand(
        cards = Hand(
          cards = List(
            Card.parseCard(this.gameCards(0)).get,
            Card.parseCard(this.gameCards(1)).get,
            Card.parseCard(this.gameCards(2)).get,
            Card.parseCard(this.gameCards(3)).get,
            Card.parseCard(this.gameCards(4)).get
          ),
          cardsJoker = List(
            Card.parseCard(seat.cards(0)).get,
            Card.parseCard(seat.cards(1)).get,
            Card.parseCard(seat.cards(2)).get,
            Card.parseCard(seat.cards(3)).get,
          )
        ).selectedCardsOmahaArr.head.selected.productIterator.toList.map(card => Card.parseCard(card.toString).get),
        cardsJoker = Hand(
          cards = List(
            Card.parseCard(this.gameCards(0)).get,
            Card.parseCard(this.gameCards(1)).get,
            Card.parseCard(this.gameCards(2)).get,
            Card.parseCard(this.gameCards(3)).get,
            Card.parseCard(this.gameCards(4)).get
          ),
          cardsJoker = List(
            Card.parseCard(seat.cards(0)).get,
            Card.parseCard(seat.cards(1)).get,
            Card.parseCard(seat.cards(2)).get,
            Card.parseCard(seat.cards(3)).get,
          )
        ).selectedCardsOmahaArr.head.remaining.toList.map(card => Card.parseCard(card.toString).get)

      ),
      Hand(
        cards = Hand(
          cards = List(
            Card.parseCard(this.gameCards(0)).get,
            Card.parseCard(this.gameCards(1)).get,
            Card.parseCard(this.gameCards(2)).get,
            Card.parseCard(this.gameCards(3)).get,
            Card.parseCard(this.gameCards(4)).get
          ),
          cardsJoker = List(
            Card.parseCard(seat.cards(0)).get,
            Card.parseCard(seat.cards(1)).get,
            Card.parseCard(seat.cards(2)).get,
            Card.parseCard(seat.cards(3)).get,
          )
        ).selectedCardsOmahaArr.head.selected.productIterator.toList.map(card => Card.parseCard(card.toString).get),
        cardsJoker = Hand(
          cards = List(
            Card.parseCard(this.gameCards(0)).get,
            Card.parseCard(this.gameCards(1)).get,
            Card.parseCard(this.gameCards(2)).get,
            Card.parseCard(this.gameCards(3)).get,
            Card.parseCard(this.gameCards(4)).get
          ),
          cardsJoker = List(
            Card.parseCard(seat.cards(0)).get,
            Card.parseCard(seat.cards(1)).get,
            Card.parseCard(seat.cards(2)).get,
            Card.parseCard(seat.cards(3)).get,
          )
        ).selectedCardsOmahaArr.head.remaining.toList.map(card => Card.parseCard(card.toString).get)

      ).score5CardPoker,

    )


    allPlayersHandsOmaha

  }

  def prepareNumPlayersForNextTurn(currentSeat: Seat, cause: String = "fold"):TableState = {
//    log.logger.info(s"turn ${this.turnId}")
    log.logger.info(s"numPlayers ${this.numPlayers}")
    val updatedNumPlayers = this.numPlayers.filter(x => x != currentSeat.id)//Remove it from numPlayers list

    if(updatedNumPlayers.nonEmpty) {
      val nextIndex = this.getNextTurn(this.turnId)
      val nextSeatId = this.numPlayers(nextIndex)
      val updatedTurnId = updatedNumPlayers.indexWhere(id => id == nextSeatId)
      //    log.logger.info(s"nextIndex  ${nextIndex}")
      //    log.logger.info(s"nextSeatId  ${nextSeatId}")
      //    log.logger.info(s"numPlayers ${updatedNumPlayers}")
      //    log.logger.info(s"turn now ${updatedTurnId}")
      this.copy(
        numPlayers = updatedNumPlayers,
        turnId = updatedTurnId,
        action = s"${cause}"
      )

    } else {
      this.copy(
        numPlayers = updatedNumPlayers,
        action = s"${cause}"
      )
    }




  }


  def handleAllInCmd(seatId: Int, round: String):TableState = {

    val currentSeat = this.getActivePlayers.find(_.id == seatId).get
    val currentSeatWagersForRound = this.getSeatBetsSum(seatId = currentSeat.id, round = round)
    val entireStackInCurrentRound = currentSeat.balance + currentSeatWagersForRound

    val updatedCurrentSeat = currentSeat.copy(
      isTurn = false,
      actions = Seq(false, false, false, false, false),
      betList = currentSeat.betList.:+(Bet(index = this.betIndex, betValue = currentSeat.balance, group = round, betType = "a" )),
      bets = currentSeat.bets.updated(0, entireStackInCurrentRound),
      balance = 0,
      gameStatus = "ALL IN",
    )
    val updatedSeats = this.seats.updated(currentSeat.id, updatedCurrentSeat)

    val entireStack = this.getSeatBetsSum(seatId = seatId) + currentSeat.balance
    val updatedSidePots = splashToSidePots(this.sidePots, SidePot(ids = Seq(seatId), capAmount = entireStack))
      .sortWith((a,b) => a.capAmount < b.capAmount)
      .sortWith((a,b) => a.ids.size > b.ids.size)

    log.info(s"finally ${updatedSidePots}")

    //is this the first side pot? splash the previous folded hands wagers if any
    var finalSidePots = updatedSidePots
    if(this.sidePots.isEmpty) {
      this.seats.filter(seat => seat.gameStatus == "FOLDED").foreach(foldedSeat => {
        finalSidePots = splashFoldHandToSidePots(finalSidePots,
          SidePot(
          fids = Seq(foldedSeat.id),
          foldAmount = this.getSeatBetsSum(seatId = foldedSeat.id)
          )
        )
      })
    }

    this.copy(
      seats = updatedSeats,
      betIndex = this.betIndex + 1,
      sidePots = finalSidePots,
    ).prepareNumPlayersForNextTurn(currentSeat, cause = s"seat${currentSeat.uid} all-in(${entireStackInCurrentRound})")

  }


  def handleFoldCmd(seatId: Int, round: String):TableState = {
    val currentSeat = this.getActivePlayers.find(_.id == seatId).get

    val updatedCurrentSeat = currentSeat.copy(
      isTurn = false,
      actions = Seq(false, false, false, false, false),
      betList = currentSeat.betList.:+(Bet(index = this.betIndex, betValue = 0, group = round, betType = "f" )),
      gameStatus = "FOLDED",
    )
    val updatedSeats = this.seats.updated(currentSeat.id, updatedCurrentSeat)
    val entireStack = this.getSeatBetsSum(seatId = seatId)

    if(this.sidePots.isEmpty) {
      this.copy(
        seats = updatedSeats,
        betIndex = this.betIndex + 1,
      ).prepareNumPlayersForNextTurn(currentSeat, cause = s"seat${currentSeat.uid + 1}, folded")
    }else{
      val updatedSidePots = splashFoldHandToSidePots(
        this.sidePots,
        SidePot(
          fids = Seq(seatId),
          foldAmount = entireStack,
        )
      )
      .sortWith((a,b) => a.capAmount < b.capAmount)
      .sortWith((a,b) => a.ids.size > b.ids.size)

      log.info(s"finally ${updatedSidePots}")

      this.copy(
        seats = updatedSeats,
        betIndex = this.betIndex + 1,
        sidePots = updatedSidePots,
      ).prepareNumPlayersForNextTurn(currentSeat, cause = s"seat${currentSeat.uid + 1}, folded")

    }


  }

  def handleCheckCmd(seatId: Int, round: String):TableState = {
    val highestBet = highestAmountInRound(round)

    val currentSeat = this.getActivePlayers.find(_.id == seatId).get

    val updatedCurrentSeat = currentSeat.copy(
      isTurn = false,
      gameStatus = "CHECKED",
      betList = currentSeat.betList.:+(Bet(index = this.betIndex, betValue = 0, group = round, betType = "c" )),
      actions = Seq(false, false, false, false, false)
    )
    val updatedSeats = this.seats.updated(currentSeat.id, updatedCurrentSeat)

    this.copy(
      seats = updatedSeats,
      betIndex = this.betIndex + 1,
//      action = s"seat${seatId + 1} did check for ${highestBet.toInt} bet"
      action = s"seat${seatId + 1}, checked"
    )
  }

  def handleRaiseCmd(seatId: Int, wagerAmount: Double, round: String):TableState = {

    val currentSeat = this.getActivePlayers.find(_.id == seatId).get
    val currentSeatWagerForTheRound = this.getRoundBets(round, seatId).map(_.betValue).sum
    val betTotalTo = currentSeatWagerForTheRound + wagerAmount

    val updatedCurrentSeat = currentSeat.copy(
      isTurn = false,
      gameStatus = "RAISED",
      balance = currentSeat.balance - wagerAmount,
      betList = currentSeat.betList.:+(Bet(index = this.betIndex, betValue = wagerAmount, group = round, betType = "r" )),
      bets = currentSeat.bets.updated(0, betTotalTo ),
      actions = Seq(false, false, false, false, false)
    )
    val updatedSeats = this.seats.updated(currentSeat.id, updatedCurrentSeat)

    this.copy(
      seats = updatedSeats,
      betAmount = betTotalTo,
      raiseAmount = betTotalTo,
      betIndex = this.betIndex + 1,
//      action = s"seat${seatId + 1} raised by ${wagerAmount.toInt} to ${betTotalTo}"
      action = s"seat${seatId + 1}, raised"
    )
  }

  def handleBetCmd(seatId: Int, wagerAmount: Double, round: String):TableState = {
    val highestBet = highestAmountInRound(round)

    val currentSeat = this.getActivePlayers.find(_.id == seatId).get

    val betTotalTo = highestBet + wagerAmount

    val updatedCurrentSeat = currentSeat.copy(
      isTurn = false,
      gameStatus = "BET OPENED",
      balance = currentSeat.balance - wagerAmount,
      betList = currentSeat.betList.:+(Bet(index = this.betIndex, betValue = wagerAmount, group = round, betType = "b" )),
      bets = currentSeat.bets.updated(0, betTotalTo ),
      actions = Seq(false, false, false, false, false)
    )
    val updatedSeats = this.seats.updated(currentSeat.id, updatedCurrentSeat)

    this.copy(
      seats = updatedSeats,
      betAmount = betTotalTo,
      raiseAmount = betTotalTo,
      betIndex = this.betIndex + 1,
//      action = s"seat${seatId + 1} bet by ${wagerAmount.toInt} to ${betTotalTo}"
      action = s"seat${seatId + 1}, bet"
    )
  }

  def handleCallHandCmd(seatId: Int, round: String):TableState = {
    val highestBet = highestAmountInRound(round)

    val currentSeat = this.getActivePlayers.find(_.id == seatId).get
    val currentSeatWagerForTheRound = this.getRoundBets(round, seatId).map(_.betValue).sum
    val callBetCost = highestBet - currentSeatWagerForTheRound

    val updatedCurrentSeat = currentSeat.copy(
      isTurn = false,
      gameStatus = "CALLED",
      balance = currentSeat.balance - callBetCost,
      betList = currentSeat.betList.:+(Bet(index = this.betIndex, betValue = callBetCost, group = round, betType = "c" )),
      bets = currentSeat.bets.updated(0, highestBet ),
      actions = Seq(false, false, false, false, false)
    )
    val updatedSeats = this.seats.updated(currentSeat.id, updatedCurrentSeat)

    this.copy(
      seats = updatedSeats,
      betIndex = this.betIndex + 1,
//      action = s"seat${seatId + 1} called ${callBetCost.toInt}"
      action = s"seat${seatId + 1}, called"
    )
  }



  def handleKnockout(): TableState = {
    val currentWinner = if(this.numPlayers.isEmpty) {
      this.getAllPlayingPlayers.find(seat => seat.gameStatus == "ALL IN").head.id
    } else this.numPlayers.head

    val totalWin = this.getRoundBets().map(_.betValue).sum
    val rakeAmount = (totalWin * this.configData.rakePercent * .01)
    val totalWinAfterRakeCollection = totalWin - (totalWin * this.configData.rakePercent * .01)
    val roundedWin = Math.floor(totalWinAfterRakeCollection)
    val roundOff = totalWinAfterRakeCollection - Math.floor(totalWinAfterRakeCollection)


    val updatedSeats = this.seats.map(
      seat =>
        if(!seat.isPlaying) seat //Skip the sitOut players
        else {
          if (seat.id == currentWinner)
            seat.copy(
              winAmount = roundedWin,
              bets = seat.bets.updated(0, roundedWin),
              gameStatus = "Win",
              isTurn = true,
            )
          else {
            seat.copy(
              winAmount = 0,
              gameStatus = "Lost",
              isTurn = false,
            )
          }
        }
    )


    this.copy(
      stage = "17",
      potAmount = this.potAmount + this.seats.map(seat => seat.bets.head).fold(0.0)(_ + _),
      winners = this.winners.:+(
        Winner(
          id = currentWinner,
          winningPot = 0,
          winAmount = roundedWin.toInt,
          rake = rakeAmount + roundOff,
          hand = "Knockout Champion",
          cards = Seq("xx", "xx", "xx", "xx", "xx")
        )
      ),
      seats = updatedSeats,
      action = s"seat${currentWinner + 1} win a pot=${roundedWin} rake=${rakeAmount + roundOff}!"
    )
  }



  def prepareForMatchingBet(seatId: Int, round: String):TableState = {
    val isBetOpen = isBetOpenForRound(round)

    //highest raise in a round is - single largest raise in a round &&
    //highest bet in a round is - calculated by comparing total bets by each seat coulld be all => openBet + raise + re-raise + ...
    val highestRaise = highestRaiseInRound(round)//meaning highest single raise so far in this round
    val highestBet = highestAmountInRound(round)//highest total bet in round

    val currentSeat = this.getActivePlayers.find(_.id == seatId).get
    val currentSeatWagerForTheRound = this.getRoundBets(round, seatId).map(_.betValue).sum
    val totalWagersForTheRound = this.getRoundBets(round, -1).map(_.betValue).sum
    val callBetCost = highestBet - currentSeatWagerForTheRound
    val nextRaiseAmount = highestRaise  * 2
    val nextPotLimitAmount = potAmount + (2 * highestBet) + totalWagersForTheRound - currentSeatWagerForTheRound


    val nextActions = Seq(
      isBetOpen, //Fold
      !isBetOpen, //until the first voluntary bet is made
      isBetOpen && (currentSeat.balance >= nextRaiseAmount), //Increase the previous high bet with a min (callAmount + previous Raise)
      !isBetOpen, //until the first voluntary bet is made
      isBetOpen && (currentSeat.balance >= callBetCost), //call - match the highest bet
      true //all in
    )

    val updatedCurrentSeat = currentSeat.copy(
      isTurn = true,
      gameStatus = "TO CALL",
      actions = nextActions,
    )
    val updatedSeats = this.seats.updated(currentSeat.id, updatedCurrentSeat)

    this.copy(
      seats = updatedSeats,
      potLimit = nextPotLimitAmount,
      betAmount = callBetCost, // this shall be used by the client for call
      raiseAmount = nextRaiseAmount, //this shall be used by the client to make a min raise
//      action = s"seat${seatId + 1} to call by ${callBetCost.toInt} or raise by ${nextRaiseAmount.toInt} to ${nextRaiseAmount.toInt + currentSeatWagerForTheRound.toInt} "
      action = s"seat${seatId + 1} to call "
    )
  }

  def prepareForBet(seatId: Int, round: String):TableState = {
    val isBetOpen = false
    val highestBet = highestAmountInRound(round)//highest total bet in round

    val currentSeat = this.getActivePlayers.find(_.id == seatId).get
    val currentSeatWagerForTheRound = this.getRoundBets(round, seatId).map(_.betValue).sum //only for round 1 this would be > 0 ???
    val totalWagersForTheRound = this.getRoundBets(round, -1).map(_.betValue).sum
    val nextBetCost = highestBet + this.configData.blind - currentSeatWagerForTheRound
    val nextPotLimitAmount = potAmount + (2 * highestBet) + totalWagersForTheRound - currentSeatWagerForTheRound

    val nextActions = Seq(
      isBetOpen, //Fold
      !isBetOpen, //Check - until the first voluntary bet is made
      isBetOpen, //Increase the previous high bet with a min (callAmount + previous Raise)
      !isBetOpen && (currentSeat.balance >= nextBetCost), //Bet - until the first voluntary bet is made
      isBetOpen, //call - match the highest bet
      true //all in - match the highest bet but not enough chips
    )

    val updatedCurrentSeat = currentSeat.copy(
      isTurn = true,
      gameStatus = "TO CALL",
      actions = nextActions,
    )
    val updatedSeats = this.seats.updated(currentSeat.id, updatedCurrentSeat)

    this.copy(
      seats = updatedSeats,
      potLimit = nextPotLimitAmount,
      betAmount = nextBetCost,// this shall be used by the client for a min bet
      raiseAmount = nextBetCost,
//      action = s"option open for seat${seatId + 1}, to check or bet by ${this.configData.blind} to ${nextBetCost.toInt}"
      action = s"seat${seatId + 1} option "
    )
  }

  def prepareForCheck(seatId: Int, round: String):TableState = {
    val isBetOpen = false
    val highestBet = highestAmountInRound(round)//highest total bet in round

    val currentSeat = this.getActivePlayers.find(_.id == seatId).get
    val currentSeatWagerForTheRound = this.getRoundBets(round, seatId).map(_.betValue).sum //only for round 1 this would be > 0 ???
    val totalWagersForTheRound = this.getRoundBets(round, -1).map(_.betValue).sum
    val nextBetCost = highestBet + this.configData.blind - currentSeatWagerForTheRound
    val nextPotLimitAmount = potAmount + (2 * highestBet) + totalWagersForTheRound - currentSeatWagerForTheRound

    val nextActions = Seq(
      false, //Fold
      !isBetOpen, //Check - until the first voluntary bet is made
      false, //Increase the previous high bet with a min (callAmount + previous Raise)
      !isBetOpen && (currentSeat.balance >= nextBetCost), //Bet - until the first voluntary bet is made
      false, //call - match the highest bet
      true //all in - match the highest bet but not enough chips
    )


    val statusUpdatedSeats = this.seats
      .map(seat =>
        if(numPlayers.contains(seat.id))
          seat.copy(gameStatus = "TO CHECK", isTurn = false, actions = Seq(false, false, false, false, false))
        else seat
      )

    val updatedCurrentSeat = currentSeat.copy(
      isTurn = true,
      gameStatus = "TO CHECK",
      actions = nextActions,
    )
    val updatedSeats = statusUpdatedSeats
      .updated(currentSeat.id, updatedCurrentSeat)

    this.copy(
      seats = updatedSeats,
      potLimit = nextPotLimitAmount,
      betAmount = nextBetCost,// this shall be used by the client for a min bet
      raiseAmount = nextBetCost,
//      action = s"seat${seatId + 1}, to check or bet by ${this.configData.blind} to ${nextBetCost.toInt}"
      action = s"seat${seatId + 1}, to check"
    )
  }


  def knockOutWinnerSkipAll(): Boolean = {
    log.info(s"Live Players=${this.numPlayers.mkString(",")} All In Players=${this.getAllPlayingPlayers.filter(seat => seat.gameStatus == "ALL IN").map(_.id).mkString(",")}")

    ((this.numPlayers.size == 1) && (this.sidePots.isEmpty)) ||
      ((this.numPlayers.isEmpty) && (this.getAllPlayingPlayers.count(seat => seat.gameStatus == "ALL IN") == 1))
  }

  def allInWinnerSkipBetting(): Boolean = {
    (this.numPlayers.size == 1) && (this.sidePots.nonEmpty)
  }

  def allSeatsAllIn() : Boolean = {
    this.getAllPlayingPlayers.forall(seat => {
      (seat.gameStatus == "ALL IN") || (seat.gameStatus == "FOLDED")
    })
  }


  def allSeatsCalledAlready(round: String) : Boolean = {
    val highestBet= highestAmountInRound(round)

    this.getActivePlayers.forall(seat => {
      val currentSeatWagerForTheRound = wagerForTheRound(round, seat.id)
      currentSeatWagerForTheRound == highestBet
    })
  }
  def allSeatsCheckedAlready() : Boolean = {
    this.getActivePlayers.forall(seat => {
      seat.gameStatus == "CHECKED"
    })
  }

  def allActivePlayersMatchedHighestBet(round: String) : Boolean = {
    val highestBet= highestAmountInRound(round)

    this.getActivePlayers.forall(seat => {
      val currentSeatWagerForTheRound = wagerForTheRound(round, seat.id)
      currentSeatWagerForTheRound == highestBet
    })

  }

  def isBetOpenForRound(round: String): Boolean = getRoundBets(round).nonEmpty
  def highestAmountInRound(round: String): Double = if(isBetOpenForRound(round)) getBetsOfSeatsForARoundSorted(round).last else 0
  def highestRaiseInRound(round: String): Double = if(isBetOpenForRound(round)) getRoundBets(round).sortWith((a, b) => a.betValue < b.betValue).last.betValue else 0
  def wagerForTheRound(round: String, seatId: Int): Double = if(getRoundBets(round, seatId).nonEmpty) getRoundBets(round, seatId).map(_.betValue).sum else 0

  def getAllInSeatIds = this.seats.filter(seat => seat.gameStatus == "ALL IN").map(_.id)

  def getAnEmptySeatWithZeroBalance() = this.seats.find(seat => !seat.connected && seat.balance == 0).getOrElse(Seat(id = -1))

  def getActivePlayers: Seq[Seat] = this.seats.filter(seat => numPlayers.contains(seat.id))

  def getAllPlayingPlayers: Seq[Seat] = this.seats.filter(seat => seat.isPlaying)

  def isOnePlayerRemains: Boolean = this.numPlayers.size == 1

  def getNextActiveSeat(currentTurnId: Int): Seat = {
    val result =this.seats.filter(seat => numPlayers.contains(seat.id)) match {
      case seat0 :: seat1 :: Nil if seat0.id == currentTurnId => (seat0, seat1, seat0, seat1)
      case seat0 :: seat1 :: Nil if seat1.id == currentTurnId => (seat1, seat0, seat1, seat0)

      case seat0 :: seat1 :: seat2 :: Nil if seat0.id == currentTurnId => (seat0, seat1, seat2, seat0)
      case seat0 :: seat1 :: seat2 :: Nil if seat1.id == currentTurnId => (seat1, seat2, seat0, seat1)
      case seat0 :: seat1 :: seat2 :: Nil if seat2.id == currentTurnId => (seat2, seat0, seat1, seat2)

      case seat0 :: seat1 :: seat2 :: seat3 :: Nil if seat0.id == currentTurnId => (seat0, seat1, seat2, seat3)
      case seat0 :: seat1 :: seat2 :: seat3 :: Nil if seat1.id == currentTurnId => (seat1, seat2, seat3, seat0)
      case seat0 :: seat1 :: seat2 :: seat3 :: Nil if seat2.id == currentTurnId => (seat2, seat3, seat0, seat1)
      case seat0 :: seat1 :: seat2 :: seat3 :: Nil if seat3.id == currentTurnId => (seat3, seat0, seat1, seat2)

      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: Nil if seat0.id == currentTurnId => (seat0, seat1, seat2, seat3)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: Nil if seat1.id == currentTurnId => (seat1, seat2, seat3, seat4)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: Nil if seat2.id == currentTurnId => (seat2, seat3, seat4, seat0)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: Nil if seat3.id == currentTurnId => (seat3, seat4, seat0, seat1)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: Nil if seat4.id == currentTurnId => (seat4, seat0, seat1, seat2)

      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: Nil if seat0.id == currentTurnId => (seat0, seat1, seat2, seat3)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: Nil if seat1.id == currentTurnId => (seat1, seat2, seat3, seat4)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: Nil if seat2.id == currentTurnId => (seat2, seat3, seat4, seat5)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: Nil if seat3.id == currentTurnId => (seat3, seat4, seat5, seat0)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: Nil if seat4.id == currentTurnId => (seat4, seat5, seat0, seat1)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: Nil if seat5.id == currentTurnId => (seat5, seat0, seat1, seat2)

      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: Nil if seat0.id == currentTurnId => (seat0, seat1, seat2, seat3)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: Nil if seat1.id == currentTurnId => (seat1, seat2, seat3, seat4)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: Nil if seat2.id == currentTurnId => (seat2, seat3, seat4, seat5)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: Nil if seat3.id == currentTurnId => (seat3, seat4, seat5, seat6)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: Nil if seat4.id == currentTurnId => (seat4, seat5, seat6, seat0)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: Nil if seat5.id == currentTurnId => (seat5, seat6, seat0, seat1)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: Nil if seat6.id == currentTurnId => (seat6, seat0, seat1, seat2)

      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: seat7 :: Nil if seat0.id == currentTurnId => (seat0, seat1, seat2, seat3)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: seat7 :: Nil if seat1.id == currentTurnId => (seat1, seat2, seat3, seat4)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: seat7 :: Nil if seat2.id == currentTurnId => (seat2, seat3, seat4, seat5)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: seat7 :: Nil if seat3.id == currentTurnId => (seat3, seat4, seat5, seat6)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: seat7 :: Nil if seat4.id == currentTurnId => (seat4, seat5, seat6, seat7)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: seat7 :: Nil if seat5.id == currentTurnId => (seat5, seat6, seat7, seat0)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: seat7 :: Nil if seat6.id == currentTurnId => (seat6, seat7, seat0, seat1)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: seat7 :: Nil if seat7.id == currentTurnId => (seat7, seat0, seat1, seat2)

      case _ => (this.getAllPlayingPlayers.head, this.getAllPlayingPlayers.head, this.getAllPlayingPlayers.head, this.getAllPlayingPlayers.head)
    }
    result._2
  }

  def getNextActiveSeatId(currentTurnId: Int): Int = {
    getNextActiveSeat(currentTurnId).id
  }

  def getNextSeatId(currentSeatId: Int): Int = {
    val result = numPlayers.toList match {
      case seat0 :: seat1 :: Nil if seat0 == currentSeatId => (seat0, seat1, seat0, seat1)
      case seat0 :: seat1 :: Nil if seat1 == currentSeatId => (seat1, seat0, seat1, seat0)

      case seat0 :: seat1 :: seat2 :: Nil if seat0 == currentSeatId => (seat0, seat1, seat2, seat0)
      case seat0 :: seat1 :: seat2 :: Nil if seat1 == currentSeatId => (seat1, seat2, seat0, seat1)
      case seat0 :: seat1 :: seat2 :: Nil if seat2 == currentSeatId => (seat2, seat0, seat1, seat2)

      case seat0 :: seat1 :: seat2 :: seat3 :: Nil if seat0 == currentSeatId => (seat0, seat1, seat2, seat3)
      case seat0 :: seat1 :: seat2 :: seat3 :: Nil if seat1 == currentSeatId => (seat1, seat2, seat3, seat0)
      case seat0 :: seat1 :: seat2 :: seat3 :: Nil if seat2 == currentSeatId => (seat2, seat3, seat0, seat1)
      case seat0 :: seat1 :: seat2 :: seat3 :: Nil if seat3 == currentSeatId => (seat3, seat0, seat1, seat2)

      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: Nil if seat0 == currentSeatId => (seat0, seat1, seat2, seat3)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: Nil if seat1 == currentSeatId => (seat1, seat2, seat3, seat4)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: Nil if seat2 == currentSeatId => (seat2, seat3, seat4, seat0)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: Nil if seat3 == currentSeatId => (seat3, seat4, seat0, seat1)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: Nil if seat4 == currentSeatId => (seat4, seat0, seat1, seat2)

      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: Nil if seat0 == currentSeatId => (seat0, seat1, seat2, seat3)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: Nil if seat1 == currentSeatId => (seat1, seat2, seat3, seat4)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: Nil if seat2 == currentSeatId => (seat2, seat3, seat4, seat5)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: Nil if seat3 == currentSeatId => (seat3, seat4, seat5, seat0)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: Nil if seat4 == currentSeatId => (seat4, seat5, seat0, seat1)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: Nil if seat5 == currentSeatId => (seat5, seat0, seat1, seat2)

      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: Nil if seat0 == currentSeatId => (seat0, seat1, seat2, seat3)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: Nil if seat1 == currentSeatId => (seat1, seat2, seat3, seat4)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: Nil if seat2 == currentSeatId => (seat2, seat3, seat4, seat5)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: Nil if seat3 == currentSeatId => (seat3, seat4, seat5, seat6)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: Nil if seat4 == currentSeatId => (seat4, seat5, seat6, seat0)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: Nil if seat5 == currentSeatId => (seat5, seat6, seat0, seat1)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: Nil if seat6 == currentSeatId => (seat6, seat0, seat1, seat2)

      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: seat7 :: Nil if seat0 == currentSeatId => (seat0, seat1, seat2, seat3)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: seat7 :: Nil if seat1 == currentSeatId => (seat1, seat2, seat3, seat4)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: seat7 :: Nil if seat2 == currentSeatId => (seat2, seat3, seat4, seat5)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: seat7 :: Nil if seat3 == currentSeatId => (seat3, seat4, seat5, seat6)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: seat7 :: Nil if seat4 == currentSeatId => (seat4, seat5, seat6, seat7)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: seat7 :: Nil if seat5 == currentSeatId => (seat5, seat6, seat7, seat0)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: seat7 :: Nil if seat6 == currentSeatId => (seat6, seat7, seat0, seat1)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: seat7 :: Nil if seat7 == currentSeatId => (seat7, seat0, seat1, seat2)

      case _ => (0, 1, 0, 1)
    }
    result._2
  }

  def getNextTurn(currentTurn: Int): Int = {
    val result = numPlayers.toList match {
      case seat0 :: seat1 :: Nil if 0 == currentTurn => (0, 1, 0, 1)
      case seat0 :: seat1 :: Nil if 1 == currentTurn => (1, 0, 1, 0)

      case seat0 :: seat1 :: seat2 :: Nil if 0 == currentTurn => (0, 1, 2, 0)
      case seat0 :: seat1 :: seat2 :: Nil if 1 == currentTurn => (1, 2, 0, 1)
      case seat0 :: seat1 :: seat2 :: Nil if 2 == currentTurn => (2, 0, 1, 2)

      case seat0 :: seat1 :: seat2 :: seat3 :: Nil if 0 == currentTurn => (0, 1, 2, 3)
      case seat0 :: seat1 :: seat2 :: seat3 :: Nil if 1 == currentTurn => (1, 2, 3, 0)
      case seat0 :: seat1 :: seat2 :: seat3 :: Nil if 2 == currentTurn => (2, 3, 0, 1)
      case seat0 :: seat1 :: seat2 :: seat3 :: Nil if 3 == currentTurn => (3, 0, 1, 2)

      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: Nil if 0 == currentTurn => (0, 1, 2, 3)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: Nil if 1 == currentTurn => (1, 2, 3, 4)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: Nil if 2 == currentTurn => (2, 3, 4, 0)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: Nil if 3 == currentTurn => (3, 4, 0, 1)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: Nil if 4 == currentTurn => (4, 0, 1, 2)

      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: Nil if 0 == currentTurn => (0, 1, 2, 3)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: Nil if 1 == currentTurn => (1, 2, 3, 4)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: Nil if 2 == currentTurn => (2, 3, 4, 5)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: Nil if 3 == currentTurn => (3, 4, 5, 0)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: Nil if 4 == currentTurn => (4, 5, 0, 1)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: Nil if 5 == currentTurn => (5, 0, 1, 2)

      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: Nil if 0 == currentTurn => (0, 1, 2, 3)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: Nil if 1 == currentTurn => (1, 2, 3, 4)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: Nil if 2 == currentTurn => (2, 3, 4, 5)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: Nil if 3 == currentTurn => (3, 4, 5, 6)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: Nil if 4 == currentTurn => (4, 5, 6, 0)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: Nil if 5 == currentTurn => (5, 6, 0, 1)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: Nil if 6 == currentTurn => (6, 0, 1, 2)

      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: seat7 :: Nil if 0 == currentTurn => (0, 1, 2, 3)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: seat7 :: Nil if 1 == currentTurn => (1, 2, 3, 4)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: seat7 :: Nil if 2 == currentTurn => (2, 3, 4, 5)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: seat7 :: Nil if 3 == currentTurn => (3, 4, 5, 6)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: seat7 :: Nil if 4 == currentTurn => (4, 5, 6, 7)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: seat7 :: Nil if 5 == currentTurn => (5, 6, 7, 0)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: seat7 :: Nil if 6 == currentTurn => (6, 7, 0, 1)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: seat7 :: Nil if 7 == currentTurn => (7, 0, 1, 2)

      case _ => (0, 0, 0, 0)
    }
    result._2
  }


  def getPlayerPositions(dealerIndex: Int): (Int, Int, Int, Int) = {
    val result = numPlayers.toList match {
      case seat0 :: seat1 :: Nil if 0 == dealerIndex => (0, 1, 0, 1)
      case seat0 :: seat1 :: Nil if 1 == dealerIndex => (1, 0, 1, 0)

      case seat0 :: seat1 :: seat2 :: Nil if 0 == dealerIndex => (0, 1, 2, 0)
      case seat0 :: seat1 :: seat2 :: Nil if 1 == dealerIndex => (1, 2, 0, 1)
      case seat0 :: seat1 :: seat2 :: Nil if 2 == dealerIndex => (2, 0, 1, 2)

      case seat0 :: seat1 :: seat2 :: seat3 :: Nil if 0 == dealerIndex => (0, 1, 2, 3)
      case seat0 :: seat1 :: seat2 :: seat3 :: Nil if 1 == dealerIndex => (1, 2, 3, 0)
      case seat0 :: seat1 :: seat2 :: seat3 :: Nil if 2 == dealerIndex => (2, 3, 0, 1)
      case seat0 :: seat1 :: seat2 :: seat3 :: Nil if 3 == dealerIndex => (3, 0, 1, 2)

      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: Nil if 0 == dealerIndex => (0, 1, 2, 3)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: Nil if 1 == dealerIndex => (1, 2, 3, 4)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: Nil if 2 == dealerIndex => (2, 3, 4, 0)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: Nil if 3 == dealerIndex => (3, 4, 0, 1)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: Nil if 4 == dealerIndex => (4, 0, 1, 2)

      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: Nil if 0 == dealerIndex => (0, 1, 2, 3)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: Nil if 1 == dealerIndex => (1, 2, 3, 4)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: Nil if 2 == dealerIndex => (2, 3, 4, 5)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: Nil if 3 == dealerIndex => (3, 4, 5, 0)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: Nil if 4 == dealerIndex => (4, 5, 0, 1)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: Nil if 5 == dealerIndex => (5, 0, 1, 2)

      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: Nil if 0 == dealerIndex => (0, 1, 2, 3)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: Nil if 1 == dealerIndex => (1, 2, 3, 4)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: Nil if 2 == dealerIndex => (2, 3, 4, 5)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: Nil if 3 == dealerIndex => (3, 4, 5, 6)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: Nil if 4 == dealerIndex => (4, 5, 6, 0)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: Nil if 5 == dealerIndex => (5, 6, 0, 1)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: Nil if 6 == dealerIndex => (6, 0, 1, 2)

      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: seat7 :: Nil if 0 == dealerIndex => (0, 1, 2, 3)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: seat7 :: Nil if 1 == dealerIndex => (1, 2, 3, 4)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: seat7 :: Nil if 2 == dealerIndex => (2, 3, 4, 5)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: seat7 :: Nil if 3 == dealerIndex => (3, 4, 5, 6)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: seat7 :: Nil if 4 == dealerIndex => (4, 5, 6, 7)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: seat7 :: Nil if 5 == dealerIndex => (5, 6, 7, 0)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: seat7 :: Nil if 6 == dealerIndex => (6, 7, 0, 1)
      case seat0 :: seat1 :: seat2 :: seat3 :: seat4 :: seat5 :: seat6 :: seat7 :: Nil if 7 == dealerIndex => (7, 0, 1, 2)

      case _ => (0, 1, 0, 1)
    }

    (result._1, result._2, result._3, result._4)
  }


  def detectDealerSeatId(): Int = {
    val filteredSeats = this.seats.filter(seat => this.numPlayers.contains(seat.id))
    /* In imperative programming languages , we use loops such as for-loop & while-loop to iterate over collections
    * Scala introduced a new kind of loop called for-comprehension to manage collections using
    * Like most of the other constructs this also comes from the Haskell
    * It goes far beyond simple looping over collections */
    val hands = for {
      seat <- filteredSeats
      if seat.cards.nonEmpty
    } yield (seat.id, Hand(Card.parseCard(seat.cards.head).get))

    /*Recursion is cool way to solve "looping" problems in a functional way
    * Walk over all of the elements to return a final single value - a sign that you may want to use reduce in scala*/
    val winner = hands.reduce((x, y) => if (x._2 beatOne  y._2) x else if (y._2 beatOne x._2) y else x)

    val dealerId = winner._1
    dealerId
  }

//  def detectWinnerId(hands: Seq[(Int, Hand)]): Int = {
//
//    /*Recursion is cool way to solve "looping" problems in a functional way
//    * Walk over all of the elements to return a final single value - a sign that you may want to use reduce in scala*/
//    val winner = hands.reduce((x, y) => if (x._2 beat y._2) x else if (y._2 beat x._2) y else x)
//
//    winner._1
//  }


  def sendStateUpdateToClients(dealer: ActorRef,
                               toppers: MMap[String, ClientData] = MMap.empty[String, ClientData],
                               clients: MMap[String, ClientData] = MMap.empty[String, ClientData],
                               admins: MMap[String, AdminClientData]): Unit = {

    val tableState = this.stage match {
      case "2" => this
      case "3" => this
      case "16" => this
      case "17" => this
      case "18" => this
      case _ => this.copy(seats = this.getSeatsWithCardsMasked())
    }

    admins.foreach {
      admin =>
        admin._2.client ! Json.toJson(TableDataUpdatedMsg(
          data = tableState.toTableData, timestamp = dateFormat.format(Calendar.getInstance().getTime)))
    }

    toppers.foreach {
      topper =>
        topper._2.client ! Json.toJson(TableDataUpdatedMsg(
          data = tableState.toTableData, timestamp = dateFormat.format(Calendar.getInstance().getTime)))
    }

    clients.foreach {
      client =>
        val maskedTableState = this.stage match {
          case "1" => this
          case "2" => this
          case "16" => this
          case "18" => this
          case _ => this.copy(seats = this.getSeatsWithCardsMasked(client._2.uid))
        }


        if(client._2.clientType == "web") {
          client._2.client ! Json.toJson(TableDataUpdatedMsg(
            data = maskedTableState.toTableData,
            timestamp = dateFormat.format(Calendar.getInstance().getTime)))

        } else {
          client._2.client ! Json.stringify(
            Json.toJson(TableDataUpdatedMsg(
              data = maskedTableState
                .toTableData,
              timestamp = dateFormat.format(Calendar.getInstance().getTime)))
          ).getBytes(StandardCharsets.UTF_8)
        }



    }

    log.logger.info(
      s"R:${this.roundId}/" +
      s"S:${this.stage}/" +
      s"[${this.getAllPlayingPlayers.map(seat => {if(seat.isPlaying) s"${seat.balance.toInt}(${seat.id+1})" else ""}).mkString(",")}]/" +
      s"A:${this.action}" +
      s"[${this.getAllPlayingPlayers.map(seat => {
          (seat.gameStatus, numPlayers.indexWhere(p => p == seat.id) == this.turnId) match {
            case ("Sit Out", _) => ""
            case (status, true) if status.contains("Win") => "w*"
            case (status, false) if status.contains("Win") => "w"
            case (status, true) if status.contains("Lost") => "l*"
            case (status, false) if status.contains("Lost") => "l"
            case ("CALLED", false) => "c"
            case ("CALLED", true) => "c*"
            case ("ALL IN", false) => "a"
            case ("ALL IN", true) => "a*"
            case ("CHECKED", false) => "c"
            case ("CHECKED", true) => "c*"
            case ("RAISED", false) => "r"
            case ("RAISED", true) => "r*"
            case ("OPEN BET", false) => "b"
            case ("OPEN BET", true) => "b*"
            case ("BET OPENED", false) => "b"
            case ("BET OPENED", true) => "b*"
            case ("FOLDED", false) => "f"
            case ("FOLDED", true) => "f*"
            case (_, false) if(seat.id == dealerId) => "d"
            case (_, true) if(seat.id == dealerId) => "d*"
            case ("Playing", false) => "p"
            case ("Playing", true) => "p*"
            case ("TO CALL", false) => "t"
            case ("TO CALL", true) => "t*"
            case ("Posted SB", false) => "sb"
            case ("Posted SB", true) => "sb*"
            case ("Posted BB", false) => "bb"
            case ("Posted BB", true) => "bb*"
            case (status, _) => status(0).toString
          }
        }).mkString(",")}]/"  +
      s"|${this.getRoundBets("pre-flop").map(bet => if(bet.betValue > 0) s"${bet.betType}${bet.betValue.toInt}" else s"${bet.betType}").mkString("")}|" +
      s"${this.getRoundBets("flop").map(bet => if(bet.betValue > 0) s"${bet.betType}${bet.betValue.toInt}" else s"${bet.betType}").mkString("")}|" +
      s"${this.getRoundBets("turn").map(bet => if(bet.betValue > 0) s"${bet.betType}${bet.betValue.toInt}" else s"${bet.betType}").mkString("")}|" +
      s"${this.getRoundBets("river").map(bet => if(bet.betValue > 0) s"${bet.betType}${bet.betValue.toInt}" else s"${bet.betType}").mkString("")}|/" +
      s"|${this.getAllPlayingPlayers.map(seat => {if(seat.isPlaying) seat.cards.mkString("") else ""}).mkString("|")}|"
    )

    dealer ! TableStateUpdated(this)

  }


  def toTableData: TableData = {
    TableData(
      roundId = this.roundId,
      configData = this.configData,
      action = this.action,
      potAmount = this.potAmount,
      potLimit = this.potLimit,
      betAmount = this.betAmount,
      raiseAmount = this.raiseAmount,
      stage = this.stage,
      winners = this.winners,
      gameCards = this.gameCards,
      sidePots = this.sidePots,
      seats = this.seats,
      hands = this.hands,
    )
  }

  def clearSeatCards(): TableState = this.copy(
    seats = this.seats.map(
      seat =>
        seat.copy(cards = Seq.empty[String])
    )
  )

  def getBetsOfSeatsForARoundSorted(round: String = ""): Seq[Double] = {
    this
      .getAllPlayingPlayers
      .map(seat => seat.betList.filter(b => (round == "")  || (round == b.group)))
      .map(betList => betList.map(bet => bet.betValue).sum)
      .sortWith((a, b) => a < b)
  }

  def getRoundBetsSorted(round: String = "", seatId: Int = -1): Seq[Bet] = {
    this
      .getAllPlayingPlayers
      .filter(s => (seatId == -1) || (seatId == s.id))
      .flatMap(seat => seat.betList)
      .filter(b => (round == "")  || (round == b.group))
      .sortWith((a, b) => a.index < b.index)
  }

  def getRoundBets(round: String = "", seatId: Int = -1): Seq[Bet] = {
    this
      .getAllPlayingPlayers
      .filter(s => (seatId == -1) || (seatId == s.id))
      .flatMap(seat => seat.betList)
      .filter(b => (round == "")  || (round == b.group))
      .sortWith((a, b) => a.index < b.index)
  }

  def getSeatBetsSum(seatId: Int, round: String = ""): Double = {
    this
      .getAllPlayingPlayers
      .filter(s => seatId == s.id)
      .flatMap(seat => seat.betList)
      .filter(b => (round == "")  || (round == b.group))
      .map(bet => bet.betValue )
      .sum
  }

  def recallBetsWithBetLists(): TableState = {
    val betsReCalledStr = this.seats.map(seat => {
      this.getSeatBetsSum(seatId = seat.id)
    }).map(_.intValue()).mkString("game cancelled, bets recalled (", ",", ")")

    this.copy(
      seats = this.seats.map(seat => {
        val allBet = this.getSeatBetsSum(seatId = seat.id)
        seat.copy(
          betList = Seq.empty[Bet],
          bets = Seq(0, 0, 0, 0),
          balance = seat.balance + allBet
        )
      }
      ),
      action = betsReCalledStr
    )
  }

  def recallBets(): TableState = {
    val betsReCalledStr = this.seats.map(seat => {
      seat.bets.sum
    }).map(_.intValue()).mkString("game cancelled, bets recalled (", ",", ")")

    this.copy(
      seats = this.seats.map(seat => {
        val allBet = seat.bets.sum
        seat.copy(
          betList = Seq.empty[Bet],
          bets = Seq(0, 0, 0, 0),
          balance = seat.balance + allBet
        )
      }
      ),
      action = betsReCalledStr
    )
  }

  /*
  * handleHoleCard: Handle the Hole Card received from Shoe
  * Input: Card in String format
  *
  * */
  def handleHoleCard(drawnCard: String): TableState = {
    //Get the seat Id using the turnId
    val seatId        = numPlayers(this.turnId)
    val currentCards  = this.seats(seatId).cards
    val updatedCards  = if(currentCards.nonEmpty) currentCards.:+(drawnCard) else Seq(drawnCard)
    val updatedSeat   = this.seats(seatId).copy(cards = updatedCards)
    val updatedSeats  = this.seats.updated(seatId, updatedSeat)

    val nextTurnId = getNextTurn(this.turnId)
    val nextSeatId    = numPlayers(nextTurnId)


    this.copy(
      seats = updatedSeats,
      turnId = nextTurnId,
      action = s"drawn hole card to pos=${this.turnId} seat${seatId + 1} next turn pos=${nextTurnId} seat${nextSeatId + 1}"
    )
  }

  /*
  * drawHoleCard : Draw a card from the deck and add to the current turn seat
  * Input: Deck
  *
  * */
  def drawHoleCard(cards: Deck52.type): TableState = {
    val drawnCard: String = cards.drawCard.toString
    val seatId = numPlayers(this.turnId)
    val oldCards = this.seats(seatId).cards
    val updatedSeat = this.seats(seatId).copy(cards = if (oldCards.nonEmpty) oldCards.:+(drawnCard) else Seq(drawnCard))

    val nextTurn = getNextTurn(this.turnId)
    val nextTurnId = numPlayers(nextTurn)

    this.copy(
      seats = this.seats.updated(seatId, updatedSeat),
      turnId = nextTurn,
      action = s"drawn hole card to pos=${this.turnId} seat${seatId + 1} next turn pos=${nextTurn} seat${nextTurnId + 1}"
    )
  }

  /*
  * handleCommunityCard: Handle the Community Card received from Shoe
  * */
  def handleCommunityCard(drawnCard: String): TableState = {
    val oldCards = this.gameCards
    val updatedCards = if (oldCards.nonEmpty) oldCards.:+(drawnCard) else Seq(drawnCard)
    this.copy(gameCards = updatedCards,
      action = s"drawn community card ${drawnCard}"
    )
  }

  def drawCommunityCard(cards: Deck52.type): TableState = {
    val drawnCard = cards.drawCard.toString
    val oldCards = this.gameCards
    val updatedCards = if (oldCards.nonEmpty) oldCards.:+(drawnCard) else Seq(drawnCard)
    this.copy(gameCards = updatedCards,
      action = s"drawn community card ${drawnCard}"
    )
  }

  def drawCardAndUpdateSeat(cards: Deck52.type): TableState = {
    val drawnCard = cards.drawCard.toString
    val currentTurnSeatId = numPlayers(this.turnId)
    val nextTurn = this.getNextTurn(this.turnId)
    val nextTurnSeatId = numPlayers(nextTurn)

    val updatedSeat = this.seats.find(seat => seat.id == currentTurnSeatId).get.copy(cards = Seq(drawnCard))
    this.copy(
      seats = this.seats.updated(currentTurnSeatId, updatedSeat),
      turnId = nextTurn,
      action = s"drawn election card ${drawnCard} to pos=${this.turnId} seatId${currentTurnSeatId + 1}, next is pos=${nextTurn} seatId${nextTurnSeatId + 1}"
    )
  }

  def handleCardAndUpdateSeat(drawnCard: String): TableState = {
    val currentTurnSeatId = numPlayers(this.turnId)
    val nextTurn = this.getNextTurn(this.turnId)
    val nextTurnSeatId = numPlayers(nextTurn)

    val updatedSeat = this.seats.find(seat => seat.id == currentTurnSeatId).get.copy(cards = Seq(drawnCard))
    this.copy(
      seats = this.seats.updated(currentTurnSeatId, updatedSeat),
      turnId = nextTurn,
      action = s"drawn election card ${drawnCard} to pos=${this.turnId} seatId${currentTurnSeatId + 1}, next is pos=${nextTurn} seatId${nextTurnSeatId + 1}"
    )
  }

  def resetTableWinnerDetails(): TableState =
    this.copy(
      winners = Seq.empty[Winner],
      sidePots = Seq.empty[SidePot],
      potAmount = 0,
      potLimit = 0,
      betAmount = 0,
      raiseAmount = 0,
    )

  def resetTableCards(): TableState = this.copy(
    turnId = 0,
    betIndex = 1,
    gameCards = Seq.empty[String],
    cards = Deck52.freshCards.map(card => card.toString),
    seats = this.seats.map(seat => seat.copy(cards = Seq.empty[String])),
    hands = Seq.empty[(Int, List[String], List[String], String)]
  )

  def resetSeats(): TableState = this.copy(
    seats = this.seats.map(seat =>
      seat.copy(
        gameStatus = if(seat.isPlaying) "Playing" else seat.gameStatus,
        hint = "",
        isTurn = false,
        isDealer = false,
        isSmallBet = false,
        isBigBet = false,
        betList = Seq.empty[Bet],
        winningBets = Seq.empty[WinBet],
        bets = Seq(0, 0, 0, 0, 0),
        actions = Seq(false, false, false, false, false),
        winAmount = 0,
      )
    )
  )


  def prepareForFreshRound(): TableState = {
    this
      .resetTableCards()
      .resetTableWinnerDetails()
      .resetSeats()
      .copy(stage = "1")
  }

  def startRoundWithBlindBets(): TableState = {
    val smallBlindAmount = this.configData.blind / 2
    val bigBlindAmount = this.configData.blind
    val betAmountPreFlop = this.configData.blind
    val raiseAmountPreFlop = this.configData.blind * 2
    val totalWagersForTheRound = smallBlindAmount + bigBlindAmount
    val currentSeatWagerForTheRound = 0
    val nextPotLimitAmount = potAmount + (2 * bigBlindAmount) + totalWagersForTheRound - currentSeatWagerForTheRound

    val updatedState =this
          .copy(seats = this.seats.map(seat => {
            if (seat.isSmallBet) seat.copy(balance = seat.balance - smallBlindAmount, bets = Seq(smallBlindAmount, 0, 0, 0, 0))
            else if (seat.isBigBet) seat.copy(balance = seat.balance - bigBlindAmount, bets = Seq(bigBlindAmount, 0, 0, 0, 0))
            else seat.copy(bets = Seq(0, 0, 0, 0, 0))
          }))
          .copy(potLimit = nextPotLimitAmount)
          .copy(betAmount = betAmountPreFlop)
          .copy(raiseAmount = raiseAmountPreFlop)


    updatedState
      .copy(
        seats = updatedState.seats.map(seat => {
          if (seat.isSmallBet) seat.copy(betList = seat.betList.:+(Bet(index = 1, betValue = smallBlindAmount, group = "pre-flop", betType = "b" )) ) //appended Bet
          else if (seat.isBigBet) seat.copy(betList = seat.betList.:+(Bet(index = 2, betValue = bigBlindAmount, group = "pre-flop", betType = "r" ))) //appended Bet
          else seat.copy(betList = Seq.empty[Bet])
          }),
        betIndex = 3
      )
  }

  def printTablePlayersStartState(): Unit =
    log.logger.info(
      s"START GAME R:${this.roundId}/${this.numPlayers.size}P/" +
      s"[${if(this.numPlayers.size > 1)
        this.numPlayers.map(_ + 1).mkString(",")
        else ""}]/" +
      s"[${this.getAllPlayingPlayers.map(seat => (seat.id + 1,seat.balance.toInt)).mkString(",")}]/"
    )



  /*############################## SEATS UTILITY METHODS ################################### */

  //Fill Seats With Election Result
  def fillSeatsWithElectionResult(dealerId: Int, smallBetId: Int, bigBetId: Int): Seq[Seat] =
    this.seats
      .map(seat => {
        if (this.numPlayers.contains(seat.id)) seat.copy(gameStatus = "TO CALL", isPlaying = true) else seat.copy(gameStatus = "Sit Out", isPlaying = false)
      })
      .map(seat => {
        if (seat.id == dealerId) seat.copy(isDealer = true) else seat
      })
      .map(seat => {
        if (seat.id == smallBetId) seat.copy(isSmallBet = true, gameStatus = "Posted SB") else seat
      })
      .map(seat => {
        if (seat.id == bigBetId) seat.copy(isBigBet = true, gameStatus = "Posted BB") else seat
      })


  //TODO fillOtherSeatsCardsWithMask
  def getSeatsWithCardsMasked(skipASeat: String = "-1"): Seq[Seat] =
    this.seats
      .map(seat => {
        if (seat.uid != skipASeat) {
          seat.cards.length match {
            case 4 => seat.copy(cards = Seq("xx", "xx", "xx", "xx"))
            case 3 => seat.copy(cards = Seq("xx", "xx", "xx"))
            case 2 => seat.copy(cards = Seq("xx", "xx"))
            case 1 => seat.copy(cards = Seq("xx"))
            case _ => seat
          }
        } else {
          seat
        }
      })


  //TODO fillAllSeatsWithHint
  def fillSeatsWithHint: Seq[Seat] =
  /*
  * Fill Hint For seats
  * Skip cases
  * 1. seat == "FOLDED"
  * 2. cards < 2
  * 3. community cards is empty
  *
  * Must Return all seats, so
  */

    this.seats
      .map(seat => {
        if ((seat.cards.length == 2) && (!seat.cards.contains("xx")) && this.gameCards.nonEmpty) {
          val hand = Hand(
            cards = (this.gameCards ++ seat.cards).map(card => Card.parseCard(card).get).toVector,
            joker = seat.cards.map(card => Card.parseCard(card).get).toVector
          )
          val hint = hand.score5CardPoker
          seat.copy(hint = hint)
        } else {
          seat
        }

      })


}

trait PokerUtilities {


  def sidePotsString(sidePots: Seq[SidePot]): String = {
    s"[${sidePots.map(p => ("[" + p.ids.mkString(",") + "]", p.capAmount, "[" + p.fids.mkString(",") + "]", p.foldAmount )).mkString(",")}]"
  }

  def sidePotString(sidePot: SidePot): String = {
    s"[${sidePot.ids.mkString(",")}] (${sidePot.capAmount})"

  }

  def sidePotFoldString(sidePot: SidePot): String = {
    s"[${sidePot.fids.mkString(",")}] (${sidePot.foldAmount})"

  }

  def splashFoldHandToSidePots(sidePots: Seq[SidePot], allInSp: SidePot): Seq[SidePot] = {

    /*Distribute the fold hand contribution*/
    /*Step 1: distribute it to all smaller pots one by one*/
    /*Step 2: smaller than all other pots, so put it to the first(least) bigger pot*/

    println(sidePotFoldString(allInSp) + "=> " + sidePotsString(sidePots))

    if(allInSp.foldAmount == 0 ) {
      println("<=" + sidePotsString(sidePots))
      return sidePots //end case for call
    } else if(sidePots.exists(sp => sp.capAmount <= allInSp.foldAmount && !sp.fids.contains(allInSp.fids.last) ))  {
      //smaller pots not yet splashed
      //splash again with reduced amount
      val foundIndex = sidePots.indexWhere(sp => sp.capAmount <= allInSp.foldAmount && !sp.fids.contains(allInSp.fids.last))
      val foundPot = sidePots(foundIndex)

      splashToSidePots(
        sidePots.updated(foundIndex,
          foundPot.copy(
            fids = (allInSp.fids ++ foundPot.fids).distinct, //added fid
            foldAmount = foundPot.foldAmount + foundPot.capAmount //added capAmount to its foldAmount
          )
        ),
        allInSp.copy(foldAmount = allInSp.foldAmount - foundPot.capAmount),//subtracted foldAmount with capAmount
      )
    } else if(sidePots.exists(sp => sp.capAmount > allInSp.foldAmount && !sp.fids.contains(allInSp.fids.last)))  {
      //Add it to the first Bigger pot
      val foundIndex = sidePots.indexWhere(sp => sp.capAmount > allInSp.foldAmount && !sp.fids.contains(allInSp.fids.last))
      val foundPot = sidePots(foundIndex)

      val updatedSidePots = sidePots.updated(foundIndex,
        foundPot.copy(
          fids = (allInSp.fids ++ foundPot.fids).distinct, //Added fid
          foldAmount = foundPot.foldAmount + allInSp.foldAmount // added foldAmount to its foldAmount
        )
      )
      println("<=" + sidePotsString(updatedSidePots))
      return updatedSidePots
    } else {
      println("<=" + sidePotsString(sidePots.:+(allInSp)))
      return sidePots.:+(allInSp)
    }

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
        val totalBet = seat.bets.fold(0.0)(_ + _)
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
  def sendPlayersWinTransactions(actor: ActorRef, gameId: Long, winners: Seq[Winner], seats: Seq[Seat], rake: Int, admins: MMap[String, AdminClientData]): Unit = {

    //for each winner
    winners.foreach(winner => {
      val totalBet = seats(winner.id).betList.map(_.betValue).sum
      val oldBalance = seats(winner.id).balance
      val totalWin = winner.winAmount
      val rakeCollected = seats.flatMap(seat => seat.betList).map(_.betValue).sum - totalWin

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
      val totalBet = seats(winner.id).betList.map(_.betValue).sum
      val totalWin = winner.winAmount

      updatedSeats = updatedSeats
        .map(seat =>
          if (seat.id == winner.id) seat.copy(balance = seat.balance + totalWin)
          else seat
        )
    })

    updatedSeats.map(seat =>
      if (winners.map(_.id).contains(seat.id)) {
        seat.copy(
          totalBet = seat.betList.map(_.betValue).sum,
          lastWin = winners.filter(w => w.id == seat.id).map(_.winAmount).sum
        )
      }
      else seat.copy(totalBet = seat.betList.map(_.betValue).sum)
    );

  }

  def updateSeatsBalanceWithResultRefactored(winners: Seq[Winner], seats: Seq[Seat]): Seq[Seat] = {
    val winnersSeats = winners.map(_.id)

    //map through seats if winners seats has its id then update its balance with the curresponding winAmount
    seats.map(seat =>
      if(winnersSeats.contains(seat.id))
        seat.copy(balance = seat.balance + winners.find(w => w.id == seat.id).get.winAmount)
      else
        seat
    )

  }


}

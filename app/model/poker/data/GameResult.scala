package model.poker.data

/*
* Define a GameResult ideal for any Community Poker game
* - Round Id
* - ConfigData
* - GameCards
* - Seq[Winner]
* - Seq[PokerSeat]
*
* */
case class GameResult(roundId: Long, configData: ConfigData, gameCards: Seq[String], winners: Seq[Winner], seats: Seq[PokerSeat])

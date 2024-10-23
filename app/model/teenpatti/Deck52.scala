package model.teenpatti

import model.teenpatti.Card.Deck

import scala.collection.mutable.ListBuffer

case object Deck52 {
  val freshCards: Deck = for {
    s <- Card.suits
    r <- Card.ranks
  } yield Card(s,r)

  var cards: ListBuffer[Card] = freshCards.to(ListBuffer)

  def shuffleCards(): Unit = {
    cards = scala.util.Random.shuffle(cards)
  }

  def reShuffle(): Unit = {
    cards = freshCards.to(ListBuffer)
  }

  def drawCard: Card = {
    shuffleCards()
    cards.remove(0)
  }

}

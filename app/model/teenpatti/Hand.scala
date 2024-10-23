package model.teenpatti

import model.teenpatti.syntax._
import teenpatti.Ranking

case class Hand(cards: Vector[Card]) {
  def +(card: Card): Hand = copy(cards = cards :+ card)
  def -(card: Card): Hand = copy(cards = cards.filterNot(_ == card))

  def beat(that: Hand): Boolean = Hand.ordering.compare(this, that) > 0

  def compare(that: Hand): Int = Hand.ordering.compare(this, that)

  def toList: List[Card] = cards.toList
 
  def chooseCard: Vector[Selection[Card]] = cards.sorted.map(card => Selection(card, this - card))

  def chooseHighCard: Vector[Selection[(Card, Card, Card)]] = {
    for {
      Selection(a, rest) <- this.chooseCard
      Selection(b, rest) <- rest.chooseCard
      Selection(c, rest) <- rest.chooseCard
    } yield Selection((a, b, c), rest)
  }

  def choosePair: Vector[Selection[(Card, Card, Card)]] = {
    for {
      Selection(a, rest) <- this.chooseCard
      Selection(b, rest) <- rest.chooseCard if a.rank.value == b.rank.value
      Selection(c, rest) <- rest.chooseCard
    } yield Selection((a, b, c), rest)
  }

  def chooseFlush: Vector[Selection[(Card, Card, Card)]] = {
    for {
      Selection(a, rest) <- this.chooseCard
      Selection(b, rest) <- rest.chooseCard if a.suit == b.suit
      Selection(c, rest) <- rest.chooseCard if a.suit == c.suit
    } yield Selection((a, b, c), rest)
  }

  def chooseStraight(ranking: Ranking[Card]): Vector[Selection[(Card, Card, Card)]] = {
    for {
      Selection(a, rest) <- this.chooseCard
      Selection(b, rest) <- rest.chooseCard if ranking.pred(b, a)
      Selection(c, rest) <- rest.chooseCard if ranking.pred(c, b)
    } yield Selection((a, b, c), rest)
  }

  def chooseTrio: Vector[Selection[(Card, Card, Card)]] = {
    for {
      Selection(a, rest) <- this.chooseCard
      Selection(b, rest) <- rest.chooseCard if a.rank.value == b.rank.value
      Selection(c, rest) <- rest.chooseCard if a.rank.value == c.rank.value
    } yield Selection((a, b, c), rest)
  }

  def chooseStraightFlush(ranking: Ranking[Card]): Vector[Selection[(Card, Card, Card)]] = {
    for {
      Selection(a, rest) <- this.chooseCard
      Selection(b, rest) <- rest.chooseCard if a.suit == b.suit && ranking.pred(b, a)
      Selection(c, rest) <- rest.chooseCard if a.suit == c.suit && ranking.pred(c, b)
    } yield Selection((a, b, c), rest)
  }

  def score: String = {
    if (chooseStraightFlush(Card.acesHigh).nonEmpty | chooseStraightFlush(Card.acesLow).nonEmpty) "Straight Flush"
    else if (chooseTrio.nonEmpty) "Set"
    else if (chooseStraight(Card.acesHigh).nonEmpty | chooseStraight(Card.acesLow).nonEmpty) "Straight"
    else if (chooseFlush.nonEmpty) "Flush"
    else if (choosePair.nonEmpty) "Pair"
    else "High Card"
  }

}

object Hand {
  val empty: Hand = Hand()

  def getOdds(hand: String):Double = {
    hand match {
      case "High Card" => 2.94
      case "Pair" => 5.0
      case "Flush" => 12.0
      case "Straight" => 21.0
      case "Set" => 126.0
      case "Straight Flush" => 151.0
      case _ => 0
    }
  }

  // val highestCard: Ordering[Hand] = Ordering.by(_.chooseCard.sorted)
  val highCard: Ordering[Hand] = Ordering.by(_.chooseHighCard)
  val singlePair: Ordering[Hand] = Ordering.by(_.choosePair)
  val flush: Ordering[Hand] = Ordering.by(_.chooseFlush)
  val straight: Ordering[Hand] = Ordering.by((hand: Hand) => hand.chooseStraight(Card.acesHigh)) orElse
    Ordering.by((hand: Hand) => hand.chooseStraight(Card.acesLow))
  val threeOfAKind: Ordering[Hand] = Ordering.by(_.chooseTrio)
  val straightFlush: Ordering[Hand] =
    Ordering.by((hand: Hand) => hand.chooseStraightFlush(Card.acesHigh)) orElse
      Ordering.by((hand: Hand) => hand.chooseStraightFlush(Card.acesLow))

  def apply(cards: Card*): Hand = Hand(cards.toVector)
  def apply(cards: List[Card]): Hand = Hand(cards.toVector)

  implicit val ordering: Ordering[Hand] =
    straightFlush orElse
      threeOfAKind orElse
      straight orElse
      flush orElse
      singlePair orElse
      highCard
}

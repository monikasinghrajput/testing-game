package model.teenpatti

import teenpatti.Ranking

//data type Suit - subs(Hearts,Diamonds,Clubs,Spades)
sealed trait Suit
sealed case class Hearts() extends Suit {override def toString: String = "h"}
sealed case class Diamonds() extends Suit {override def toString: String = "d"}
sealed case class Clubs() extends Suit {override def toString: String = "c"}
sealed case class Spades() extends Suit {override def toString: String = "s"}

//data type Rank(value) - subs(Two,Three,Four,Five,Six,Seven,Eight,Nine,Ten,Jack,Queen,King,Ace)
sealed abstract class Rank(val value: Int) {override def toString: String = s"$value"}
sealed case class Ace() extends Rank(14) {override def toString: String = "A"}
sealed case class King() extends Rank(13){override def toString: String = "K"}
sealed case class Queen() extends Rank(12){override def toString: String = "Q"}
sealed case class Jack() extends Rank(11){override def toString: String = "J"}
sealed case class Ten() extends Rank(10)
sealed case class Nine() extends Rank(9)
sealed case class Eight() extends Rank(8)
sealed case class Seven() extends Rank(7)
sealed case class Six() extends Rank(6)
sealed case class Five() extends Rank(5)
sealed case class Four() extends Rank(4)
sealed case class Three() extends Rank(3)
sealed case class Two() extends Rank(2)


object Rank {
  implicit val acesLow: Ranking[Rank] =
    Ranking.by[Rank] {
      case Ace()   =>  1
      case Two()   =>  2
      case Three() =>  3
      case Four()  =>  4
      case Five()  =>  5
      case Six()   =>  6
      case Seven() =>  7
      case Eight() =>  8
      case Nine()  =>  9
      case Ten()   => 10
      case Jack()  => 11
      case Queen() => 12
      case King()  => 13
    }

  implicit val acesHigh: Ranking[Rank] =
    Ranking.by[Rank] {
      case Ace()   => 14
      case Two()   =>  2
      case Three() =>  3
      case Four()  =>  4
      case Five()  =>  5
      case Six()   =>  6
      case Seven() =>  7
      case Eight() =>  8
      case Nine()  =>  9
      case Ten()   => 10
      case Jack()  => 11
      case Queen() => 12
      case King()  => 13
    }
}

case class Card(suit: Suit, rank: Rank) {
  override def toString: String = suit.toString + rank.toString
}

object Card {
  val cardRegex = "([a-z])([A-Z0-9]*)".r

  def parseCard(str: String): Option[Card] =
    str match {
      case cardRegex(suit, rank) =>
        for {
          suit  <- parseSuit(suit)
          rank <- parseRank(rank)
        } yield Card(suit, rank)

      case _ => None
    }
  def parseRank(str: String): Option[Rank] =
    str match {
      case "A"   => Some(Ace())
      case "2"   => Some(Two())
      case "3" => Some(Three())
      case "4"  => Some(Four())
      case "5"  => Some(Five())
      case "6"   => Some(Six())
      case "7" => Some(Seven())
      case "8" => Some(Eight())
      case "9"  => Some(Nine())
      case "10"   => Some(Ten())
      case "J"  => Some(Jack())
      case "Q" => Some(Queen())
      case "K"  => Some(King())
      case _          => None
    }

  def parseSuit(str: String): Option[Suit] =
    str match {
      case "c"    => Some(Clubs())
      case "d" => Some(Diamonds())
      case "h"   => Some(Hearts())
      case "s"   => Some(Spades())
      case _             => None
    }

  def apply(str: String) = {
    parseCard(str)
  }
  type Deck = List[Card]
  type Hand = List[Card]

  def suits: List[Suit] = List(Hearts(),Diamonds(),Clubs(),Spades())
  def ranks: List[Rank] = List(Ace(),King(),Queen(),Jack(),Ten(),Nine(),Eight(),Seven(),Six(),Five(),Four(),Three(),Two())


  val acesHigh: Ranking[Card] = Ranking.by(card => Rank.acesHigh(card.rank))
  val acesLow: Ranking[Card] = Ranking.by(card => Rank.acesLow(card.rank))

  implicit val ordering: Ordering[Card] = acesHigh
}

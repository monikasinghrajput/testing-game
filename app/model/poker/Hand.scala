package model.poker

import model.poker.syntax._
import poker.Ranking

case class Hand(cards: Vector[Card], joker: Vector[Card] = Vector[Card]()) {

  def +(card: Card): Hand = copy(cards = cards :+ card)

  def :+(card: Card): Hand = copy(joker = joker :+ card)
  
  def +++(that: Hand): Hand = copy(joker = that.cards)

  def ++(that: Hand): Hand = copy(cards = cards ++ that.cards)

  def size: Int = cards.size

//  def compare(that: Hand): Int = Hand.ordering.compare(this, that)

//  def beat(that: Hand): Boolean = Hand.ordering.compare(this, that) > 0

  def beatOne(that: Hand): Boolean = Hand.highCardOne.compare(this, that) > 0

//  def beatTexas(that: Hand): Boolean = Hand.orderingTexasHand.compare(this, that) > 0

//  def beatOmaha(that: Hand): Boolean = Hand.orderingOmahaHand.compare(this, that) > 0

  def beatPoker(that: Hand): Boolean = Hand.orderingPoker5Card.compare(this, that) > 0

  def toList: List[Card] = cards.toList

  def -(card: Card): Hand = copy(cards = cards.filterNot(_ == card))

  def chooseLowCard: Vector[Selection[Card]] = cards.sorted.reverse.map(card => Selection(card, this - card))
  def chooseCard: Vector[Selection[Card]] = cards.sorted.map(card => Selection(card, this - card))

  def remove(card: Card): Hand = copy(joker = joker.filterNot(_ == card))
  def chooseJoker: Vector[Selection[Card]] = joker.sorted.map(card => Selection(card, this remove card))


  /*
  * Election Single Card Hand
  * */

  def chooseHighCardOne: Vector[Selection[(Card)]] = {
    for {
      Selection(a, rest) <- this.chooseCard
    } yield Selection((a), rest)
  }

  /*
  * Poker 5 Cards
  * Input - 2 Joker Cards, 5 Com Cards,
  * Selection - Only 5 Com Cards Selection, All jokers in to remaining
  * */


    def chooseHighCardPoker5Card: Vector[Selection[(Card, Card, Card, Card, Card)]] = {
      for {
        Selection(a, rest) <- this.chooseCard
        Selection(b, rest) <- rest.chooseCard
        Selection(c, rest) <- rest.chooseCard
        Selection(d, rest) <- rest.chooseCard
        Selection(e, rest) <- rest.chooseCard
      } yield Selection((a, b, c, d, e), Hand(this.joker))
    }

    def chooseOnePairPoker5Card: Vector[Selection[(Card, Card, Card, Card, Card)]] = {
      for {
        Selection(a, rest) <- this.chooseCard
        Selection(b, rest) <- rest.chooseCard if a.rank.value == b.rank.value
        Selection(c, rest) <- rest.chooseCard
        Selection(d, rest) <- rest.chooseCard
        Selection(e, rest) <- rest.chooseCard
      } yield Selection((a, b, c, d, e), Hand(this.joker))
    }

    def chooseTwoPairPoker5Card: Vector[Selection[(Card, Card, Card, Card, Card)]] = {
      for {
        Selection(a, rest) <- this.chooseCard
        Selection(b, rest) <- rest.chooseCard if a.rank.value == b.rank.value
        Selection(c, rest) <- rest.chooseCard
        Selection(d, rest) <- rest.chooseCard if c.rank.value == d.rank.value
        Selection(e, rest) <- rest.chooseCard
      } yield Selection((a, b, c, d, e), Hand(this.joker))
    }

    def chooseThreeOfAKindPoker5Card: Vector[Selection[(Card, Card, Card, Card, Card)]] = {
      for {
        Selection(a, rest) <- this.chooseCard
        Selection(b, rest) <- rest.chooseCard if a.rank.value == b.rank.value
        Selection(c, rest) <- rest.chooseCard if a.rank.value == c.rank.value
        Selection(d, rest) <- rest.chooseCard
        Selection(e, rest) <- rest.chooseCard
      } yield Selection((a, b, c, d, e), Hand(this.joker))
    }

    def chooseStraightPoker5Card(ranking: Ranking[Card]): Vector[Selection[(Card, Card, Card, Card, Card)]] = {
      for {
        Selection(a, rest) <- this.chooseCard
        Selection(b, rest) <- rest.chooseCard if ranking.pred(b, a)
        Selection(c, rest) <- rest.chooseCard if ranking.pred(c, b)
        Selection(d, rest) <- rest.chooseCard if ranking.pred(d, c)
        Selection(e, rest) <- rest.chooseCard if ranking.pred(e, d)
      } yield Selection((a, b, c, d, e), Hand(this.joker))
    }

    def chooseFlushPoker5Card: Vector[Selection[(Card, Card, Card, Card, Card)]] = {
      for {
        Selection(a, rest) <- this.chooseCard
        Selection(b, rest) <- rest.chooseCard if a.suit == b.suit
        Selection(c, rest) <- rest.chooseCard if a.suit == c.suit
        Selection(d, rest) <- rest.chooseCard if a.suit == d.suit
        Selection(e, rest) <- rest.chooseCard if a.suit == e.suit
      } yield Selection((a, b, c, d, e), Hand(this.joker))
    }

    def chooseFullHousePoker5Card: Vector[Selection[(Card, Card, Card, Card, Card)]] = {
      for {
        Selection(a, rest) <- this.chooseCard
        Selection(b, rest) <- rest.chooseCard if a.rank.value == b.rank.value
        Selection(c, rest) <- rest.chooseCard if a.rank.value == c.rank.value
        Selection(d, rest) <- rest.chooseCard
        Selection(e, rest) <- rest.chooseCard if d.rank.value == e.rank.value
      } yield Selection((a, b, c, d, e), Hand(this.joker))
    }

    def chooseFourOfaKindPoker5Card: Vector[Selection[(Card, Card, Card, Card, Card)]] = {
      for {
        Selection(a, rest) <- this.chooseCard
        Selection(b, rest) <- rest.chooseCard if a.rank.value == b.rank.value
        Selection(c, rest) <- rest.chooseCard if a.rank.value == c.rank.value
        Selection(d, rest) <- rest.chooseCard if a.rank.value == d.rank.value
        Selection(e, rest) <- rest.chooseCard
      } yield Selection((a, b, c, d, e), Hand(this.joker))
    }

    def chooseStraightFlushPoker5Card(ranking: Ranking[Card]): Vector[Selection[(Card, Card, Card, Card, Card)]] = {
      for {
        Selection(a, rest) <- this.chooseCard
        Selection(b, rest) <- rest.chooseCard if a.suit == b.suit && ranking.pred(b, a)
        Selection(c, rest) <- rest.chooseCard if a.suit == c.suit && ranking.pred(c, b)
        Selection(d, rest) <- rest.chooseCard if a.suit == d.suit && ranking.pred(d, c)
        Selection(e, rest) <- rest.chooseCard if a.suit == e.suit && ranking.pred(e, d)
      } yield Selection((a, b, c, d, e), Hand(this.joker))
    }


  /*
  * Some Special Poker
  * Input - 2 Joker Cards, 7 Com Cards,
  * Selection - 2 Jokers Must Present Selection
  * */


  def chooseHighCard: Vector[Selection[(Card, Card, Card, Card, Card)]] = {
    for {
      Selection(jokerA, rest) <- this.chooseJoker
      Selection(jokerB, rest) <- rest.chooseJoker
      Selection(x, rest) <- this.chooseLowCard  if((jokerA != x) && (jokerB != x))
      Selection(y, rest) <- rest.chooseLowCard  if((jokerA != y) && (jokerB != y))
      Selection(a, rest) <- rest.chooseCard
      Selection(b, rest) <- rest.chooseCard
      Selection(c, rest) <- rest.chooseCard
      Selection(d, rest) <- rest.chooseCard
      Selection(e, rest) <- rest.chooseCard
    } yield Selection((a, b, c, d, e), rest)
  }

  def chooseOnePair: Vector[Selection[(Card, Card, Card, Card, Card)]] = {
    for {
      Selection(jokerA, rest) <- this.chooseJoker
      Selection(jokerB, rest) <- rest.chooseJoker
      Selection(x, rest) <- this.chooseLowCard  if((jokerA != x) && (jokerB != x))
      Selection(y, rest) <- rest.chooseLowCard  if((jokerA != y) && (jokerB != y))
      Selection(a, rest) <- rest.chooseCard
      Selection(b, rest) <- rest.chooseCard if a.rank.value == b.rank.value
      Selection(c, rest) <- rest.chooseCard
      Selection(d, rest) <- rest.chooseCard
      Selection(e, rest) <- rest.chooseCard
    } yield Selection((a, b, c, d, e), rest)
  }

  def chooseTwoPair: Vector[Selection[(Card, Card, Card, Card, Card)]] = {
    for {
      Selection(jokerA, rest) <- this.chooseJoker
      Selection(jokerB, rest) <- rest.chooseJoker
      Selection(x, rest) <- this.chooseLowCard  if((jokerA != x) && (jokerB != x))
      Selection(y, rest) <- rest.chooseLowCard  if((jokerA != y) && (jokerB != y))
      Selection(a, rest) <- rest.chooseCard
      Selection(b, rest) <- rest.chooseCard if a.rank.value == b.rank.value
      Selection(c, rest) <- rest.chooseCard
      Selection(d, rest) <- rest.chooseCard if c.rank.value == d.rank.value
      Selection(e, rest) <- rest.chooseCard
    } yield Selection((a, b, c, d, e), rest)
  }

  def chooseThreeOfAKind: Vector[Selection[(Card, Card, Card, Card, Card)]] = {
    for {
      Selection(jokerA, rest) <- this.chooseJoker
      Selection(jokerB, rest) <- rest.chooseJoker
      Selection(x, rest) <- this.chooseLowCard  if((jokerA != x) && (jokerB != x))
      Selection(y, rest) <- rest.chooseLowCard  if((jokerA != y) && (jokerB != y))
      Selection(a, rest) <- rest.chooseCard
      Selection(b, rest) <- rest.chooseCard if a.rank.value == b.rank.value
      Selection(c, rest) <- rest.chooseCard if a.rank.value == c.rank.value
      Selection(d, rest) <- rest.chooseCard
      Selection(e, rest) <- rest.chooseCard
    } yield Selection((a, b, c, d, e), rest)
  }

  def chooseStraight(ranking: Ranking[Card]): Vector[Selection[(Card, Card, Card, Card, Card)]] = {
    for {
      Selection(jokerA, rest) <- this.chooseJoker
      Selection(jokerB, rest) <- rest.chooseJoker
      Selection(x, rest) <- this.chooseLowCard  if((jokerA != x) && (jokerB != x))
      Selection(y, rest) <- rest.chooseLowCard  if((jokerA != y) && (jokerB != y))
      Selection(a, rest) <- rest.chooseCard
      Selection(b, rest) <- rest.chooseCard if ranking.pred(b, a)
      Selection(c, rest) <- rest.chooseCard if ranking.pred(c, b)
      Selection(d, rest) <- rest.chooseCard if ranking.pred(d, c)
      Selection(e, rest) <- rest.chooseCard if ranking.pred(e, d)
    } yield Selection((a, b, c, d, e), rest)
  }

  def chooseFlush: Vector[Selection[(Card, Card, Card, Card, Card)]] = {
    for {
      Selection(jokerA, rest) <- this.chooseJoker
      Selection(jokerB, rest) <- rest.chooseJoker
      Selection(x, rest) <- this.chooseLowCard  if((jokerA != x) && (jokerB != x))
      Selection(y, rest) <- rest.chooseLowCard  if((jokerA != y) && (jokerB != y))
      Selection(a, rest) <- rest.chooseCard
      Selection(b, rest) <- rest.chooseCard if a.suit == b.suit
      Selection(c, rest) <- rest.chooseCard if a.suit == c.suit
      Selection(d, rest) <- rest.chooseCard if a.suit == d.suit
      Selection(e, rest) <- rest.chooseCard if a.suit == e.suit
    } yield Selection((a, b, c, d, e), rest)
  }

  def chooseFullHouse: Vector[Selection[(Card, Card, Card, Card, Card)]] = {
    for {
      Selection(jokerA, rest) <- this.chooseJoker
      Selection(jokerB, rest) <- rest.chooseJoker
      Selection(x, rest) <- this.chooseLowCard  if((jokerA != x) && (jokerB != x))
      Selection(y, rest) <- rest.chooseLowCard  if((jokerA != y) && (jokerB != y))
      Selection(a, rest) <- rest.chooseCard
      Selection(b, rest) <- rest.chooseCard if a.rank.value == b.rank.value
      Selection(c, rest) <- rest.chooseCard if a.rank.value == c.rank.value
      Selection(d, rest) <- rest.chooseCard
      Selection(e, rest) <- rest.chooseCard if d.rank.value == e.rank.value
    } yield Selection((a, b, c, d, e), rest)
  }

  def chooseFourOfaKind: Vector[Selection[(Card, Card, Card, Card, Card)]] = {
    for {
      Selection(jokerA, rest) <- this.chooseJoker
      Selection(jokerB, rest) <- rest.chooseJoker
      Selection(x, rest) <- this.chooseLowCard  if((jokerA != x) && (jokerB != x))
      Selection(y, rest) <- rest.chooseLowCard  if((jokerA != y) && (jokerB != y))
      Selection(a, rest) <- rest.chooseCard
      Selection(b, rest) <- rest.chooseCard if a.rank.value == b.rank.value
      Selection(c, rest) <- rest.chooseCard if a.rank.value == c.rank.value
      Selection(d, rest) <- rest.chooseCard if a.rank.value == d.rank.value
      Selection(e, rest) <- rest.chooseCard
    } yield Selection((a, b, c, d, e), rest)
  }

  def chooseStraightFlush(ranking: Ranking[Card]): Vector[Selection[(Card, Card, Card, Card, Card)]] = {
    for {
      Selection(jokerA, rest) <- this.chooseJoker
      Selection(jokerB, rest) <- rest.chooseJoker
      Selection(x, rest) <- this.chooseLowCard  if((jokerA != x) && (jokerB != x))
      Selection(y, rest) <- rest.chooseLowCard  if((jokerA != y) && (jokerB != y))
      Selection(a, rest) <- rest.chooseCard
      Selection(b, rest) <- rest.chooseCard if a.suit == b.suit && ranking.pred(b, a)
      Selection(c, rest) <- rest.chooseCard if a.suit == c.suit && ranking.pred(c, b)
      Selection(d, rest) <- rest.chooseCard if a.suit == d.suit && ranking.pred(d, c)
      Selection(e, rest) <- rest.chooseCard if a.suit == e.suit && ranking.pred(e, d)
    } yield Selection((a, b, c, d, e), rest)
  }



  /*
  * Texas Poker
  * Input - 2 Joker Cards, 5 Com Cards,
  * Selection - Any 5 from Joker Cards ++ 5 Com Cards
  * */


  def chooseHighCardTexas: Vector[Selection[(Card, Card, Card, Card, Card)]] = {
    for {
      Selection(a, rest) <- (Hand(this.cards) ++ Hand(this.joker)).chooseCard
      Selection(b, rest) <- rest.chooseCard
      Selection(c, rest) <- rest.chooseCard
      Selection(d, rest) <- rest.chooseCard
      Selection(e, rest) <- rest.chooseCard
    } yield Selection((a, b, c, d, e), rest)
  }

  def chooseOnePairTexas: Vector[Selection[(Card, Card, Card, Card, Card)]] = {
    for {
      Selection(a, rest) <- (Hand(this.cards) ++ Hand(this.joker)).chooseCard
      Selection(b, rest) <- rest.chooseCard if a.rank.value == b.rank.value
      Selection(c, rest) <- rest.chooseCard
      Selection(d, rest) <- rest.chooseCard
      Selection(e, rest) <- rest.chooseCard
    } yield Selection((a, b, c, d, e), rest)
  }

  def chooseTwoPairTexas: Vector[Selection[(Card, Card, Card, Card, Card)]] = {
    for {
      Selection(a, rest) <- (Hand(this.cards) ++ Hand(this.joker)).chooseCard
      Selection(b, rest) <- rest.chooseCard if a.rank.value == b.rank.value
      Selection(c, rest) <- rest.chooseCard
      Selection(d, rest) <- rest.chooseCard if c.rank.value == d.rank.value
      Selection(e, rest) <- rest.chooseCard
    } yield Selection((a, b, c, d, e), rest)
  }

  def chooseThreeOfAKindTexas: Vector[Selection[(Card, Card, Card, Card, Card)]] = {
    for {
      Selection(a, rest) <- (Hand(this.cards) ++ Hand(this.joker)).chooseCard
      Selection(b, rest) <- rest.chooseCard if a.rank.value == b.rank.value      
      Selection(c, rest) <- rest.chooseCard if a.rank.value == c.rank.value
      Selection(d, rest) <- rest.chooseCard
      Selection(e, rest) <- rest.chooseCard
    } yield Selection((a, b, c, d, e), rest)
  }

  def chooseStraightTexas(ranking: Ranking[Card]): Vector[Selection[(Card, Card, Card, Card, Card)]] = {
    for {
      Selection(a, rest) <- (Hand(this.cards) ++ Hand(this.joker)).chooseCard
      Selection(b, rest) <- rest.chooseCard if ranking.pred(b, a)
      Selection(c, rest) <- rest.chooseCard if ranking.pred(c, b)
      Selection(d, rest) <- rest.chooseCard if ranking.pred(d, c)
      Selection(e, rest) <- rest.chooseCard if ranking.pred(e, d)
    } yield Selection((a, b, c, d, e), rest)
  }

  def chooseFlushTexas: Vector[Selection[(Card, Card, Card, Card, Card)]] = {
    for {
      Selection(a, rest) <- (Hand(this.cards) ++ Hand(this.joker)).chooseCard
      Selection(b, rest) <- rest.chooseCard if a.suit == b.suit
      Selection(c, rest) <- rest.chooseCard if a.suit == c.suit
      Selection(d, rest) <- rest.chooseCard if a.suit == d.suit
      Selection(e, rest) <- rest.chooseCard if a.suit == e.suit
    } yield Selection((a, b, c, d, e), rest)
  }

  def chooseFullHouseTexas: Vector[Selection[(Card, Card, Card, Card, Card)]] = {
    for {
      Selection(a, rest) <- (Hand(this.cards) ++ Hand(this.joker)).chooseCard
      Selection(b, rest) <- rest.chooseCard if a.rank.value == b.rank.value
      Selection(c, rest) <- rest.chooseCard if a.rank.value == c.rank.value
      Selection(d, rest) <- rest.chooseCard 
      Selection(e, rest) <- rest.chooseCard if d.rank.value == e.rank.value
    } yield Selection((a, b, c, d, e), rest)
  }

  def chooseFourOfaKindTexas: Vector[Selection[(Card, Card, Card, Card, Card)]] = {
    for {
      Selection(a, rest) <- (Hand(this.cards) ++ Hand(this.joker)).chooseCard
      Selection(b, rest) <- rest.chooseCard if a.rank.value == b.rank.value
      Selection(c, rest) <- rest.chooseCard if a.rank.value == c.rank.value
      Selection(d, rest) <- rest.chooseCard if a.rank.value == d.rank.value
      Selection(e, rest) <- rest.chooseCard
    } yield Selection((a, b, c, d, e), rest)
  }

  def chooseStraightFlushTexas(ranking: Ranking[Card]): Vector[Selection[(Card, Card, Card, Card, Card)]] = {
    for {
      Selection(a, rest) <- (Hand(this.cards) ++ Hand(this.joker)).chooseCard
      Selection(b, rest) <- rest.chooseCard if a.suit == b.suit && ranking.pred(b, a)
      Selection(c, rest) <- rest.chooseCard if a.suit == c.suit && ranking.pred(c, b)
      Selection(d, rest) <- rest.chooseCard if a.suit == d.suit && ranking.pred(d, c)
      Selection(e, rest) <- rest.chooseCard if a.suit == e.suit && ranking.pred(e, d)
    } yield Selection((a, b, c, d, e), rest)
  }



  /*
  * Omaha Poker
  * Input - 4 Joker Cards, 5 Com Cards,
  * Selection - 2 from Jokers & 3 from Com Cards Must
  * */



  def chooseHighCardOmaha: Vector[Selection[(Card, Card, Card, Card, Card)]] = {
    for {
      Selection(jokerA, restJoker) <- this.chooseJoker
      Selection(jokerB, restJoker) <- restJoker.chooseJoker
      Selection(jokerC, restJoker) <- restJoker.chooseJoker
      Selection(jokerD, restJoker) <- restJoker.chooseJoker
      Selection(v, rest) <- this.chooseLowCard
      Selection(w, rest) <- rest.chooseLowCard
      Selection(a, rest) <- (rest ++ Hand(jokerA, jokerB)).chooseCard
      Selection(b, rest) <- rest.chooseCard
      Selection(c, rest) <- rest.chooseCard
      Selection(d, rest) <- rest.chooseCard
      Selection(e, rest) <- rest.chooseCard
    } yield Selection((a, b, c, d, e), Hand(v, w) ++ Hand(jokerC, jokerD))
  }

  def chooseOnePairOmaha: Vector[Selection[(Card, Card, Card, Card, Card)]] = {
    for {
      Selection(jokerA, restJoker) <- this.chooseJoker
      Selection(jokerB, restJoker) <- restJoker.chooseJoker
      Selection(jokerC, restJoker) <- restJoker.chooseJoker
      Selection(jokerD, restJoker) <- restJoker.chooseJoker
      Selection(v, rest) <- this.chooseLowCard
      Selection(w, rest) <- rest.chooseLowCard
      Selection(a, rest) <- (rest ++ Hand(jokerA, jokerB)).chooseCard
      Selection(b, rest) <- rest.chooseCard if a.rank.value == b.rank.value
      Selection(c, rest) <- rest.chooseCard
      Selection(d, rest) <- rest.chooseCard
      Selection(e, rest) <- rest.chooseCard
    } yield Selection((a, b, c, d, e), Hand(v, w) ++ Hand(jokerC, jokerD))
  }

  def chooseTwoPairOmaha: Vector[Selection[(Card, Card, Card, Card, Card)]] = {
    for {
      Selection(jokerA, restJoker) <- this.chooseJoker
      Selection(jokerB, restJoker) <- restJoker.chooseJoker
      Selection(jokerC, restJoker) <- restJoker.chooseJoker
      Selection(jokerD, restJoker) <- restJoker.chooseJoker
      Selection(v, rest) <- this.chooseLowCard
      Selection(w, rest) <- rest.chooseLowCard
      Selection(a, rest) <- (rest ++ Hand(jokerA, jokerB)).chooseCard
      Selection(b, rest) <- rest.chooseCard if a.rank.value == b.rank.value
      Selection(c, rest) <- rest.chooseCard
      Selection(d, rest) <- rest.chooseCard if c.rank.value == d.rank.value
      Selection(e, rest) <- rest.chooseCard
    } yield Selection((a, b, c, d, e), Hand(v, w) ++ Hand(jokerC, jokerD))
  }

  def chooseThreeOfAKindOmaha: Vector[Selection[(Card, Card, Card, Card, Card)]] = {
    for {
      Selection(jokerA, restJoker) <- this.chooseJoker
      Selection(jokerB, restJoker) <- restJoker.chooseJoker
      Selection(jokerC, restJoker) <- restJoker.chooseJoker
      Selection(jokerD, restJoker) <- restJoker.chooseJoker
      Selection(v, rest) <- this.chooseLowCard
      Selection(w, rest) <- rest.chooseLowCard
      Selection(a, rest) <- (rest ++ Hand(jokerA, jokerB)).chooseCard
      Selection(b, rest) <- rest.chooseCard if a.rank.value == b.rank.value
      Selection(c, rest) <- rest.chooseCard if a.rank.value == c.rank.value
      Selection(d, rest) <- rest.chooseCard
      Selection(e, rest) <- rest.chooseCard
    } yield Selection((a, b, c, d, e), Hand(v, w) ++ Hand(jokerC, jokerD))

  }

  def chooseStraightOmaha(ranking: Ranking[Card]): Vector[Selection[(Card, Card, Card, Card, Card)]] = {
    for {
      Selection(jokerA, restJoker) <- this.chooseJoker
      Selection(jokerB, restJoker) <- restJoker.chooseJoker
      Selection(jokerC, restJoker) <- restJoker.chooseJoker
      Selection(jokerD, restJoker) <- restJoker.chooseJoker
      Selection(v, rest) <- this.chooseLowCard
      Selection(w, rest) <- rest.chooseLowCard
      Selection(a, rest) <- (rest ++ Hand(jokerA, jokerB)).chooseCard
      Selection(b, rest) <- rest.chooseCard if ranking.pred(b, a)
      Selection(c, rest) <- rest.chooseCard if ranking.pred(c, b)
      Selection(d, rest) <- rest.chooseCard if ranking.pred(d, c)
      Selection(e, rest) <- rest.chooseCard if ranking.pred(e, d)
    } yield Selection((a, b, c, d, e), Hand(v, w) ++ Hand(jokerC, jokerD))
  }

  def chooseFlushOmaha: Vector[Selection[(Card, Card, Card, Card, Card)]] = {
    for {
      Selection(jokerA, restJoker) <- this.chooseJoker
      Selection(jokerB, restJoker) <- restJoker.chooseJoker
      Selection(jokerC, restJoker) <- restJoker.chooseJoker
      Selection(jokerD, restJoker) <- restJoker.chooseJoker
      Selection(v, rest) <- this.chooseLowCard
      Selection(w, rest) <- rest.chooseLowCard
      Selection(a, rest) <- (rest ++ Hand(jokerA, jokerB)).chooseCard
      Selection(b, rest) <- rest.chooseCard if a.suit == b.suit
      Selection(c, rest) <- rest.chooseCard if a.suit == c.suit
      Selection(d, rest) <- rest.chooseCard if a.suit == d.suit
      Selection(e, rest) <- rest.chooseCard if a.suit == e.suit
    } yield Selection((a, b, c, d, e), Hand(v, w) ++ Hand(jokerC, jokerD))
  }

  def chooseFullHouseOmaha: Vector[Selection[(Card, Card, Card, Card, Card)]] = {
    for {
      Selection(jokerA, restJoker) <- this.chooseJoker
      Selection(jokerB, restJoker) <- restJoker.chooseJoker
      Selection(jokerC, restJoker) <- restJoker.chooseJoker
      Selection(jokerD, restJoker) <- restJoker.chooseJoker
      Selection(v, rest) <- this.chooseLowCard
      Selection(w, rest) <- rest.chooseLowCard
      Selection(a, rest) <- (rest ++ Hand(jokerA, jokerB)).chooseCard
      Selection(b, rest) <- rest.chooseCard if a.rank.value == b.rank.value
      Selection(c, rest) <- rest.chooseCard if a.rank.value == c.rank.value
      Selection(d, rest) <- rest.chooseCard
      Selection(e, rest) <- rest.chooseCard if d.rank.value == e.rank.value
    } yield Selection((a, b, c, d, e), Hand(v, w) ++ Hand(jokerC, jokerD))
  }

  def chooseFourOfaKindOmaha: Vector[Selection[(Card, Card, Card, Card, Card)]] = {

    for {
      Selection(jokerA, restJoker) <- this.chooseJoker
      Selection(jokerB, restJoker) <- restJoker.chooseJoker
      Selection(jokerC, restJoker) <- restJoker.chooseJoker
      Selection(jokerD, restJoker) <- restJoker.chooseJoker
      Selection(v, rest) <- this.chooseLowCard
      Selection(w, rest) <- rest.chooseLowCard
      Selection(a, rest) <- (rest ++ Hand(jokerA, jokerB)).chooseCard
      Selection(b, rest) <- rest.chooseCard if a.rank.value == b.rank.value
      Selection(c, rest) <- rest.chooseCard if a.rank.value == c.rank.value
      Selection(d, rest) <- rest.chooseCard if a.rank.value == d.rank.value
      Selection(e, rest) <- rest.chooseCard
    } yield Selection((a, b, c, d, e), Hand(v, w) ++ Hand(jokerC, jokerD))
  }

  def chooseStraightFlushOmaha(ranking: Ranking[Card]): Vector[Selection[(Card, Card, Card, Card, Card)]] = {
    for {
      Selection(jokerA, restJoker) <- this.chooseJoker
      Selection(jokerB, restJoker) <- restJoker.chooseJoker
      Selection(jokerC, restJoker) <- restJoker.chooseJoker
      Selection(jokerD, restJoker) <- restJoker.chooseJoker
      Selection(v, rest) <- this.chooseLowCard
      Selection(w, rest) <- rest.chooseLowCard
      Selection(a, rest) <- (rest ++ Hand(jokerA, jokerB)).chooseCard
      Selection(b, rest) <- rest.chooseCard if a.suit == b.suit && ranking.pred(b, a)
      Selection(c, rest) <- rest.chooseCard if a.suit == c.suit && ranking.pred(c, b)
      Selection(d, rest) <- rest.chooseCard if a.suit == d.suit && ranking.pred(d, c)
      Selection(e, rest) <- rest.chooseCard if a.suit == e.suit && ranking.pred(e, d)
    } yield Selection((a, b, c, d, e), Hand(v, w) ++ Hand(jokerC, jokerD))
  }



  def score: String = {
    if (chooseStraightFlush(Card.acesHigh).nonEmpty) "Straight Flush"  
    else if (chooseStraightFlush(Card.acesLow).nonEmpty) "Straight Flush"
    else if (chooseFourOfaKind.nonEmpty) "4ofA Kind"
    else if (chooseFullHouse.nonEmpty) "Full House"
    else if (chooseFlush.nonEmpty) "Flush"
    else if (chooseStraight(Card.acesHigh).nonEmpty) "Straight" 
    else if (chooseStraight(Card.acesLow).nonEmpty) "Straight"
    else if (chooseThreeOfAKind.nonEmpty) "3ofA Kind"
    else if (chooseTwoPair.nonEmpty) "Two Pair"
    else if (chooseOnePair.nonEmpty) "One Pair"
    else "High Card"
  }


  def score5CardPoker: String = {
    if (chooseStraightFlushPoker5Card(Card.acesHigh).nonEmpty) "Straight Flush"
    else if (chooseStraightFlushPoker5Card(Card.acesLow).nonEmpty) "Straight Flush-Low Ace"
    else if (chooseFourOfaKindPoker5Card.nonEmpty) "4ofA Kind"
    else if (chooseFullHousePoker5Card.nonEmpty) "Full House"
    else if (chooseFlushPoker5Card.nonEmpty) "Flush"
    else if (chooseStraightPoker5Card(Card.acesHigh).nonEmpty) "Straight"
    else if (chooseStraightPoker5Card(Card.acesLow).nonEmpty) "Straight-Low Ace"
    else if (chooseThreeOfAKindPoker5Card.nonEmpty) "3ofA Kind"
    else if (chooseTwoPairPoker5Card.nonEmpty) "Two Pair"
    else if (chooseOnePairPoker5Card.nonEmpty) "One Pair"
    else "High Card"
  }
//
//  def scoreTexas: String = {
//    if (chooseStraightFlushTexas(Card.acesHigh).nonEmpty) "Straight Flush"
//    else if (chooseStraightFlushTexas(Card.acesLow).nonEmpty) "Straight Flush"
//    else if (chooseFourOfaKindTexas.nonEmpty) "4ofA Kind"
//    else if (chooseFullHouseTexas.nonEmpty) "Full House"
//    else if (chooseFlushTexas.nonEmpty) "Flush"
//    else if (chooseStraightTexas(Card.acesHigh).nonEmpty) "Straight Ace High"
//    else if (chooseStraightTexas(Card.acesLow).nonEmpty) "Straight Ace Low"
//    else if (chooseThreeOfAKindTexas.nonEmpty) "3ofA Kind"
//    else if (chooseTwoPairTexas.nonEmpty) "Two Pair"
//    else if (chooseOnePairTexas.nonEmpty) "One Pair"
//    else "High Card"
//  }

//  def scoreOmaha: String = {
//    if (chooseStraightFlushOmaha(Card.acesHigh).nonEmpty) "Straight Flush"
//    else if (chooseStraightFlushOmaha(Card.acesLow).nonEmpty) "Straight Flush"
//    else if (chooseFourOfaKindOmaha.nonEmpty) "4ofA Kind"
//    else if (chooseFullHouseOmaha.nonEmpty) "Full House"
//    else if (chooseFlushOmaha.nonEmpty) "Flush"
//    else if (chooseStraightOmaha(Card.acesHigh).nonEmpty) "Straight Ace High"
//    else if (chooseStraightOmaha(Card.acesLow).nonEmpty) "Straight Ace Low"
//    else if (chooseThreeOfAKindOmaha.nonEmpty) "3ofA Kind"
//    else if (chooseTwoPairOmaha.nonEmpty) "Two Pair"
//    else if (chooseOnePairOmaha.nonEmpty) "One Pair"
//    else "High Card"
//  }

  def selectedCardsTexasArr: Seq[Selection[(Card, Card, Card, Card, Card)]] = {
    if (chooseStraightFlushTexas(Card.acesHigh).nonEmpty) chooseStraightFlushTexas(Card.acesHigh)
    else if (chooseStraightFlushTexas(Card.acesLow).nonEmpty) chooseStraightFlushTexas(Card.acesLow)
    else if (chooseFourOfaKindTexas.nonEmpty) chooseFourOfaKindTexas
    else if (chooseFullHouseTexas.nonEmpty) chooseFullHouseTexas
    else if (chooseFlushTexas.nonEmpty) chooseFlushTexas
    else if (chooseStraightTexas(Card.acesHigh).nonEmpty) chooseStraightTexas(Card.acesHigh)
    else if (chooseStraightTexas(Card.acesLow).nonEmpty) chooseStraightTexas(Card.acesLow)
    else if (chooseThreeOfAKindTexas.nonEmpty) chooseThreeOfAKindTexas
    else if (chooseTwoPairTexas.nonEmpty) chooseTwoPairTexas
    else if (chooseOnePairTexas.nonEmpty) chooseOnePairTexas
    else chooseHighCardTexas
  }



  def selectedCardsOmahaArr: Seq[Selection[(Card, Card, Card, Card, Card)]] = {
    if (chooseStraightFlushOmaha(Card.acesHigh).nonEmpty) chooseStraightFlushOmaha(Card.acesHigh)
    else if (chooseStraightFlushOmaha(Card.acesLow).nonEmpty) chooseStraightFlushOmaha(Card.acesLow)
    else if (chooseFourOfaKindOmaha.nonEmpty) chooseFourOfaKindOmaha
    else if (chooseFullHouseOmaha.nonEmpty) chooseFullHouseOmaha
    else if (chooseFlushOmaha.nonEmpty) chooseFlushOmaha
    else if (chooseStraightOmaha(Card.acesHigh).nonEmpty) chooseStraightOmaha(Card.acesHigh)
    else if (chooseStraightOmaha(Card.acesLow).nonEmpty) chooseStraightOmaha(Card.acesLow)
    else if (chooseThreeOfAKindOmaha.nonEmpty) chooseThreeOfAKindOmaha
    else if (chooseTwoPairOmaha.nonEmpty) chooseTwoPairOmaha
    else if (chooseOnePairOmaha.nonEmpty) chooseOnePairOmaha
    else chooseHighCardOmaha
  }



}

object Hand {
  val empty: Hand = Hand()
  def apply(cards: Card*): Hand = Hand(cards.toVector)
  def apply(cards: List[Card]): Hand = Hand(cards.toVector)
  def apply(cards: List[Card],cardsJoker: List[Card]): Hand = Hand(cards.toVector,cardsJoker.toVector)


  val highCardOne: Ordering[Hand] = Ordering.by(_.chooseHighCardOne)

  val highCard: Ordering[Hand] = Ordering.by(_.chooseHighCard)
  val onePair: Ordering[Hand] = Ordering.by(_.chooseOnePair)
  val twoPair: Ordering[Hand] = Ordering.by(_.chooseTwoPair)
  val threeOfAKind: Ordering[Hand] = Ordering.by(_.chooseThreeOfAKind)
  val straight: Ordering[Hand] = Ordering.by((hand: Hand) => hand.chooseStraight(Card.acesHigh)) orElse Ordering.by((hand: Hand) => hand.chooseStraight(Card.acesLow))
  val flush: Ordering[Hand] = Ordering.by(_.chooseFlush)
  val fullHouse: Ordering[Hand] = Ordering.by(_.chooseFullHouse)
  val fourOfaKind: Ordering[Hand] = Ordering.by(_.chooseFourOfaKind)
  val straightFlush: Ordering[Hand] = Ordering.by((hand: Hand) => hand.chooseStraightFlush(Card.acesHigh)) orElse Ordering.by((hand: Hand) => hand.chooseStraightFlush(Card.acesLow))


  val highCardPoker5Card: Ordering[Hand] = Ordering.by(_.chooseHighCardPoker5Card)
  val onePairPoker5Card: Ordering[Hand] = Ordering.by(_.chooseOnePairPoker5Card)
  val twoPairPoker5Card: Ordering[Hand] = Ordering.by(_.chooseTwoPairPoker5Card)
  val threeOfAKindPoker5Card: Ordering[Hand] = Ordering.by(_.chooseThreeOfAKindPoker5Card)
  val straightAceLowPoker5Card: Ordering[Hand] = Ordering.by((hand: Hand) => hand.chooseStraightPoker5Card(Card.acesLow))
  val straightAceHighPoker5Card: Ordering[Hand] = Ordering.by((hand: Hand) => hand.chooseStraightPoker5Card(Card.acesHigh))
  val flushPoker5Card: Ordering[Hand] = Ordering.by(_.chooseFlushPoker5Card)
  val fullHousePoker5Card: Ordering[Hand] = Ordering.by(_.chooseFullHousePoker5Card)
  val fourOfaKindPoker5Card: Ordering[Hand] = Ordering.by(_.chooseFourOfaKindPoker5Card)
  val straightFlushAceLowPoker5Card: Ordering[Hand] = Ordering.by((hand: Hand) => hand.chooseStraightFlushPoker5Card(Card.acesLow))
  val straightFlushAceHighPoker5Card: Ordering[Hand] = Ordering.by((hand: Hand) => hand.chooseStraightFlushPoker5Card(Card.acesHigh))



  val highCardTexas: Ordering[Hand] = Ordering.by(_.chooseHighCardTexas)
  val onePairTexas: Ordering[Hand] = Ordering.by(_.chooseOnePairTexas)
  val twoPairTexas: Ordering[Hand] = Ordering.by(_.chooseTwoPairTexas)
  val threeOfAKindTexas: Ordering[Hand] = Ordering.by(_.chooseThreeOfAKindTexas)
  val straightTexasAceLow: Ordering[Hand] = Ordering.by((hand: Hand) => hand.chooseStraightTexas(Card.acesLow))
  val straightTexasAceHigh: Ordering[Hand] = Ordering.by((hand: Hand) => hand.chooseStraightTexas(Card.acesHigh))
  val flushTexas: Ordering[Hand] = Ordering.by(_.chooseFlushTexas)
  val fullHouseTexas: Ordering[Hand] = Ordering.by(_.chooseFullHouseTexas)
  val fourOfaKindTexas: Ordering[Hand] = Ordering.by(_.chooseFourOfaKindTexas)
  val straightFlushTexasAceLow: Ordering[Hand] = Ordering.by((hand: Hand) => hand.chooseStraightFlushTexas(Card.acesLow))
  val straightFlushTexasAceHigh: Ordering[Hand] = Ordering.by((hand: Hand) => hand.chooseStraightFlushTexas(Card.acesHigh))



  val highCardOmaha: Ordering[Hand] = Ordering.by(_.chooseHighCardOmaha)
  val onePairOmaha: Ordering[Hand] = Ordering.by(_.chooseOnePairOmaha )
  val twoPairOmaha: Ordering[Hand] = Ordering.by(_.chooseTwoPairOmaha)
  val threeOfAKindOmaha: Ordering[Hand] = Ordering.by(_.chooseThreeOfAKindOmaha)
  val straightOmahaAceLow: Ordering[Hand] = Ordering.by((hand: Hand) => hand.chooseStraightOmaha(Card.acesLow))
  val straightOmahaAceHigh: Ordering[Hand] = Ordering.by((hand: Hand) => hand.chooseStraightOmaha(Card.acesHigh))
  val flushOmaha: Ordering[Hand] = Ordering.by(_.chooseFlushOmaha)
  val fullHouseOmaha: Ordering[Hand] = Ordering.by(_.chooseFullHouseOmaha)
  val fourOfaKindOmaha: Ordering[Hand] = Ordering.by(_.chooseFourOfaKindOmaha)
  val straightFlushOmahaAceLow: Ordering[Hand] = Ordering.by((hand: Hand) => hand.chooseStraightFlushOmaha(Card.acesHigh))
  val straightFlushOmahaAceHigh: Ordering[Hand] = Ordering.by((hand: Hand) => hand.chooseStraightFlushOmaha(Card.acesLow))

  implicit val ordering: Ordering[Hand] =
    straightFlush orElse
      fourOfaKind orElse
      fullHouse orElse
      flush orElse
      straight orElse
      threeOfAKind orElse
      twoPair orElse
      onePair orElse
      highCard

  val orderingPoker5Card: Ordering[Hand] =
    straightFlushAceHighPoker5Card orElse
      straightFlushAceLowPoker5Card orElse
      fourOfaKindPoker5Card orElse
      fullHousePoker5Card orElse
      flushPoker5Card orElse
      straightAceHighPoker5Card orElse
      straightAceLowPoker5Card orElse
      threeOfAKindPoker5Card orElse
      twoPairPoker5Card orElse
      onePairPoker5Card orElse
      highCardPoker5Card

//  val orderingTexasHand: Ordering[Hand] =
//    straightFlushTexasAceHigh orElse
//      straightFlushTexasAceLow orElse
//      fourOfaKindTexas orElse
//      fullHouseTexas orElse
//      flushTexas orElse
//      straightTexasAceHigh orElse
//      straightTexasAceLow orElse
//      threeOfAKindTexas orElse
//      twoPairTexas orElse
//      onePairTexas orElse
//      highCardTexas
//
//  val orderingOmahaHand: Ordering[Hand] = {
//    straightFlushOmahaAceHigh orElse
//      straightFlushOmahaAceLow orElse
//      fourOfaKindOmaha orElse
//      fullHouseOmaha orElse
//      flushOmaha orElse
//      straightOmahaAceHigh orElse
//      straightOmahaAceLow orElse
//      threeOfAKindOmaha orElse
//      twoPairOmaha orElse
//      onePairOmaha orElse
//      highCardOmaha
//  }




}

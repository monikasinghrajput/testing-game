package model.teenpatti

object syntax {
  implicit class OrderingOps[A](ordering1: Ordering[A]) {
    def orElse[B <: A](ordering2: Ordering[B]): Ordering[B] =
      new Ordering[B] {
        def compare(x: B, y: B): Int = {
          ordering1.compare(x, y) match {
            case 0 => ordering2.compare(x, y)
            case n => n
          }
        }
      }
  }


  implicit class StringOps(val sc: StringContext) {

    def str = sc.parts.mkString

    def h(args: Any*): Hand =
      parseHand(str).getOrElse(throw new Exception(s"Could not parse hand: $str"))

    def c(args: Any*): Card =
      parseCard(str).getOrElse(throw new Exception(s"Could not parse card: $str"))

    def parseHand(str: String): Option[Hand] =
      Some(Hand(str.split(" ").toVector.flatMap(parseCard)))

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
  }

}

package model.poker

//Data Structure representing a "selection"
case class Selection[A](selected: A,remaining: Hand)

object Selection {
  implicit def ordering[A](implicit ordering: Ordering[A]): Ordering[Selection[A]] = {
    Ordering.by(hand => {
      hand.selected
    })
  }

  /**
    * Ordering that compares vectors of selections.
    * If the first entry in one vector beats the first entry in the other, it wins.
    * Else, if the second entry in one vector beats the second entry in the other, it wins.
    * Etc...
    */
  implicit def vectorOrdering[A](implicit ord: Ordering[Selection[A]]): Ordering[Vector[Selection[A]]] = new Ordering[Vector[Selection[A]]] {
      def compare(x: Vector[Selection[A]], y: Vector[Selection[A]]): Int = {
        print("****************************************compare x with y");

        (x.headOption, y.headOption) match {
          case (None, None) => println("(None, None)"); 0
          case (None, Some(b)) => println(s"Some(b) : ${b.selected}");-10000
          case (Some(a), None) => println(s"Some(a) : ${a.selected}");+10000
          case (Some(a), Some(b)) => {
            val comp = ord.compare(b, a)
            println(s"Some(a), Some(b)) : ${a.selected} x ${b.selected} => ${comp}")
            if (comp == 0) {
              //              compare(x.tail, y.tail)
              comp
            } else comp
          }

        }
      }
    }
}
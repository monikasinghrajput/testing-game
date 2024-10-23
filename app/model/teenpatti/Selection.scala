package model.teenpatti

//Data Structure representing a "selection"
case class Selection[A](selected: A,remaining: Hand)

object Selection {
  implicit def ordering[A](implicit ordering: Ordering[A]): Ordering[Selection[A]] =
    Ordering.by(_.selected)

  /**
    * Ordering that compares vectors of selections.
    * If the first entry in one vector beats the first entry in the other, it wins.
    * Else, if the second entry in one vector beats the second entry in the other, it wins.
    * Etc...
    */
  implicit def vectorOrdering[A](implicit ord: Ordering[Selection[A]]): Ordering[Vector[Selection[A]]] =
    new Ordering[Vector[Selection[A]]] {
      def compare(x: Vector[Selection[A]], y: Vector[Selection[A]]): Int =
        (x.headOption, y.headOption) match {
          case (None   , None   ) =>  0
          case (None   , Some(b)) => -1
          case (Some(a), None   ) => +1
          case (Some(a), Some(b)) => {
            val comp = ord.compare(b, a)
            if(comp == 0) compare(x.tail, y.tail) else comp
          }

        }
    }
}
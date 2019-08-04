package model


sealed trait Direction{}

case object Buy extends Direction {}

case object Sell extends Direction {}

object Direction{
  def apply(input: String): Direction = input match {
    case "b" => Buy
    case  _ => Sell
  }
}



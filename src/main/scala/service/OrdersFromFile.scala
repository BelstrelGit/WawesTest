package service

import config.Files
import model.{Buy, Order, Sell}

import scala.io.Source

class OrdersFromFile  extends OrderService{
  private def parse(string: String) : Order = {
  string.split("\t") match {
    case Array(client, direction, ticket, price, count) =>
      model.Order(OrderService.increasedId, client, if (direction == "b") Buy else Sell , ticket ,
        price.toInt , count.toInt)
  }

  }
  val inputData = Source.fromFile(Files.ORDERS).getLines
  override def next: Order = parse(inputData.next())
  override def isEmpty: Boolean = inputData.isEmpty



}



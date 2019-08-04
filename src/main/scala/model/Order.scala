package model


case class Order(id: Int, clientId: String, direction: Direction,
                 ticket: String, price: Int, amount: Int)

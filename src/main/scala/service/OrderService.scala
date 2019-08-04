package service

import model.Order


trait OrderService {
  def next:Order
  def isEmpty:Boolean
}

object OrderService {
  private var idCounter = 0

  def increasedId = {
    idCounter = idCounter + 1
    idCounter

  }
}

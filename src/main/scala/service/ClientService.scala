package service

import model.Client

trait ClientService {
  def next: Client
  def isEmpty: Boolean
}


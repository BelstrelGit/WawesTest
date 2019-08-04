import service.{ClientsFromFile, OrdersFromFile, TradeSystem}

object TradeProcessing  extends App {
  println("Processing started ...")
  val orderStore = new OrdersFromFile
  val clientStore = new ClientsFromFile
  TradeSystem.loadAllClients(clientStore)
  while(!orderStore.isEmpty){
    val currentOrder = orderStore.next
    TradeSystem.execute(currentOrder)
  }
  TradeSystem.backUnfinishedOrders
  TradeSystem.writeStateToFile
  println("Processing stop")


}


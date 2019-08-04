package service

import config.Files
import model._

import scala.collection.mutable

object TradeSystem {
  //загрузка клиент мап из файла
  private val clientStore = mutable.HashMap.empty[String, Client]
  def loadClient(client: Client) =
    clientStore.put(client.id, client)
  def loadAllClients(clients: ClientService) =
    while(!clients.isEmpty) loadClient(clients.next)

  //создание очередей на сделки по ключу     ценная бумага/  покупку -продажу
  val market :
    Map[(String, Direction), scala.collection.mutable.PriorityQueue[Order]] =
    Map(
      ("A", Buy) -> CreateBuyingQue,
      ("B", Buy) -> CreateBuyingQue,
      ("C", Buy) -> CreateBuyingQue,
      ("D", Buy) -> CreateBuyingQue,
      ("A", Sell) -> CreateSellingQue,
      ("B", Sell) -> CreateSellingQue,
      ("C", Sell) -> CreateSellingQue,
      ("D", Sell) -> CreateSellingQue
    )

  // используется "куча(пирамида)" для эффективной сортировки порядка заявок в стакане
  // с учетом очередности их подачи
  def CreateBuyingQue: scala.collection.mutable.PriorityQueue[Order] =
    scala.collection.mutable.PriorityQueue.empty[Order](Ordering.by(buyOrder))
  def CreateSellingQue: scala.collection.mutable.PriorityQueue[Order] =
    scala.collection.mutable.PriorityQueue
      .empty[Order](Ordering.by(sellOrder))


  // заявки обрабатываются согласно очередности добавления заявки и ее цене, есть ограничение
  // по которому мы отслеживаием порядок, оно указанно в config.Files.ORDERS_LIMIT
  def buyOrder(o: Order) =
    o.price.toDouble + ((Files.ORDERS_LIMIT - o.id) / Files.ORDERS_LIMIT)
  def sellOrder(o: Order) = -o.price.toDouble - (o.id / Files.ORDERS_LIMIT)


  // Выполнение ордера - помещение ордера в стакан сделок
  def execute(order: Order ) = {
    // проверяем баланс денег и цб, если труе то матчим
  if(reserve(order)) order match {
    //при достаточном колличестве бумаг уменьшаем их из мапы
    case Order(_, _, Sell, ticket, _, _) =>
    //отравляем их на сделку, помещаем в стакан наш ордер и ордер с очереди по той же бумаге  на покупку
    putToStakan(order, getOrderFromMarket(ticket, Buy))
    // если хотим купить - ложим в стакан ордер из очереди на продажу и наш ордер на покупку
    case Order(_, _, Buy, ticket, _, _) =>
    putToStakan(getOrderFromMarket(ticket, Sell), order)
  }
  }
  // резервирование колличества ценных бумаг на счёте клиента / денег
  def reserve(order: Order): Boolean = {
  order match {
    case Order(_, _, Sell, ticket, _, amount) =>
      val clientStock = clientStore(order.clientId).stocks
      if(clientStock(ticket) >= amount) {  // если бумаг достаточно
      clientStock(ticket) -= amount        // уменьшаем колличество у клиента
    true
  } else false
    case Order(_, _, Buy, _ , price, amount) =>
      val client = clientStore(order.clientId)
      if(client.balance >= (amount* price)){
        client.balance -= (amount* price)
        true
      } else false
  }
}

  //  получение ордера c очереди
  def getOrderFromMarket(ticket: String, direction: Direction): Order = {
     //если очередь пустая берем пустую заявку, иначе с очереди
    if(market(ticket, direction).isEmpty)
      Order(1, "C1", direction, ticket, 0, 0)
    else
      market(ticket, direction).dequeue()
}

  def fromMarketToClient(order: Order): Boolean = {
    // ордер в процессе обработки,изменяем колличество бумаг и счета у клиента
    println("processed: " + order.toString)
  order match {
    case Order(_, _, Sell, ticket, _ , amount) =>
    //изменяем колличесво бумаг у клиента по id
      clientStore(order.clientId).stocks(ticket) += amount
      true
    //на продажу- изменяем размер баланса
    case Order(_, _, Buy, _, price, amount) =>
      clientStore(order.clientId).balance += (amount*price)
      true
  }

  }
  //Стакан -DOM(Depth of Market)
  // в стакане  два ордера - одни наш другой из очереди по той же бумаге
  // принимаем ордер на продажу и покупку  sell / buy
  def putToStakan(sell: Order, buy: Order): Unit = {
  // определяем цену  - используется цена той заявки которая была раньше в системе
  val usedPrice = if (sell.id < buy.id) sell.price else buy.price
  // проверяем колличество к продаже /покупке ценных бумаг и цену продажи, она  должна быть <= цене покупки
  if (sell.amount != 0 && buy.amount != 0 && sell.price <= buy.price)
  //если колличество на продажу меньше чем на покупку
    if (sell.amount < buy.amount) {
      // ордер в процессе обработки,изменяем колличество бумаг и счета у клиента
      fromMarketToClient(
        sell.copy(price = usedPrice, direction = Buy))
      fromMarketToClient(
        buy.copy(price = usedPrice, direction = Sell, amount = sell.amount)
      )
      putToStakan(
        getOrderFromMarket(sell.ticket, Sell),
        buy.copy(amount = buy.amount - sell.amount)
      )
    }
    // если колличество на покупку меньше чем на продажу
    else {
      // ордер в процессе обработки,изменяем колличество бумаг и счета у клиента
      fromMarketToClient(
        sell.copy(price = usedPrice, direction = Buy, amount = buy.amount)
      )
      fromMarketToClient(
        buy.copy(price = usedPrice, direction = Sell))
      putToStakan(
        sell.copy(amount = sell.amount - buy.amount),
        getOrderFromMarket(buy.ticket, Buy)
      )
    }
  //иначе прибавляем значения этого ордера в очереди на покупку/продажу
    else {
    market(sell.ticket, Sell) += sell
    market(buy.ticket, Buy) += buy
  }
  }
  //возвращаем незаконченные  ордера
   def backUnfinishedOrders : Unit = {
     for{
       stakan <- market.values
       order <- stakan.dequeueAll
     }fromMarketToClient(order)
   }
  //записать результат в итоговый файл
  def writeStateToFile = {
    import java.io.{File, PrintWriter}
    val writer = new PrintWriter(new File(Files.RESULT))
    clientStore.values.toList.sortBy(_.id).foreach(client => writer.write(client.toString + "\n"))
    writer.close()
  }
  // подготовка системы для тестов
  def prepareSystemForTest: Unit = {
    for {
      stakan <- market.values
    } stakan.dequeueAll
  }
}






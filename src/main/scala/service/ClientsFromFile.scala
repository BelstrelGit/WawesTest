package service
import config.Files
import model.Client
import scala.io.Source

class ClientsFromFile extends ClientService {
  private val inputData = Source.fromFile(Files.CLIENTS).getLines
  private def parse(inputData: String) : Client = {
    inputData.split("\t") match {
      case Array(clientId, dollars, a, b, c, d) =>
        val client = Client(clientId)
        client.balance = dollars.toInt
        client.stocks("A") = a.toInt
        client.stocks("B") = b.toInt
        client.stocks("C") = c.toInt
        client.stocks("D") = d.toInt
        client
    }
  }
  override def next = parse(inputData.next)

  override def isEmpty: Boolean = inputData.isEmpty



}



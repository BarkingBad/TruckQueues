package system

import akka.actor.{Actor, ActorRef, Props}

import scala.collection.immutable.Queue

object DocumentsGate {
  def props(cargoGate: ActorRef): Props = Props(new DocumentsGate(cargoGate))
  final case class Arrive(truck: Truck)
  case object CheckDocuments
  case object DepartureTruckToCargoGate
  case object StateLog
  case object AskIfCanSendTruck
}


class DocumentsGate(cargoGate: ActorRef) extends Actor {
  import CargoGate._
  import DocumentsGate._

  def receive: Receive  = mailbox(Queue.empty, null)

  def mailbox(queue: Queue[Truck], truck: Truck): Receive = {
    case Arrive(arrivingTruck: Truck) => arriveHandler(queue, truck, arrivingTruck)
    case CheckDocuments               => checkDocumentsHandler(queue, truck)
    case AskIfCanSendTruck            => askIfCanSendTheTruckHandler(queue, truck)
    case DepartureTruckToCargoGate    => departureTruckToCargoGateHandler(queue, truck)
    case DocumentsGate.StateLog       => stateLogHandler(queue, truck)
  }

  private def arriveHandler(queue: Queue[Truck], truck: Truck, arrivingTruck: Truck) = {
    context.become(mailbox(queue :+ arrivingTruck, null))
  }

  private def checkDocumentsHandler(queue: Queue[Truck], truck: Truck) = {
    if(queue.nonEmpty && truck == null) {
      context.become(mailbox(queue.tail, queue.head))
    } else {
      context.become(mailbox(queue, truck))
    }
  }

  private def askIfCanSendTheTruckHandler(queue: Queue[Truck], truck: Truck) = {
    cargoGate ! AskForTruckIfPossible
    context.become(mailbox(queue, truck))
  }

  private def departureTruckToCargoGateHandler(queue: Queue[Truck], truck: Truck) = {
    if(truck != null) {
      cargoGate ! AppendTheTruck(truck)

    }
    context.become(mailbox(queue, null))
    self ! CheckDocuments
  }

  private def stateLogHandler(queue: Queue[Truck], truck: Truck) = {
    val string1 = "Documents Gate\n"
    val string2 = if(truck != null) s"Current truck being checked $truck\nDocuments queue: " else ""
    val string3 = queue.toString() + "\n"

    println(string1 + string2 + string3)
  }

}

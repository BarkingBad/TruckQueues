package system

import akka.actor.{Actor, ActorLogging, ActorRef, Props}

import scala.collection.immutable.Queue

object DocumentsGate {
  def props(cargoGate: ActorRef): Props = Props(new DocumentsGate(cargoGate))
  private[system] final case class Arrive(truck: Truck)
  private[system] case object CheckDocuments
  private[system] case object DepartureTruckToCargoGate
  private[system] case object StateLog
}


class DocumentsGate(cargoGate: ActorRef) extends Actor with ActorLogging {
  import CargoGate._
  import DocumentsGate._

  def receive: Receive  = mailbox(Queue[Truck](), null)

  def mailbox(queue: Queue[Truck], truck: Truck): Receive = {
    case Arrive(arrivingTruck: Truck) => {
      context.become(mailbox(queue :+ arrivingTruck, null))
    }

    case CheckDocuments => {
      if(queue.size == 0) {
        mailbox(queue, truck)
      } else {
        val truckTuple = queue.dequeue
        context.become(mailbox(truckTuple._2, truckTuple._1))
      }
    }

    case DepartureTruckToCargoGate => {
      if(truck != null) cargoGate ! AppendTheTruck(truck)
      context.become(mailbox(queue, null))
    }

    case DocumentsGate.StateLog => {
      val outputString = new StringBuilder("Documents Gate\n")
      if(truck != null) outputString ++= s"Current truck being checked $truck\nDocuments queue: "
      outputString ++= queue.toString() + "\n"

      println(outputString)
    }
  }
}

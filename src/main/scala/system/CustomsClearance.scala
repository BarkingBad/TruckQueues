package system

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import system.DocumentsGate.{CheckDocuments, DepartureTruckToCargoGate}

object CustomsClearance {
  def props(documentsGate: ActorRef, cargoGate: ActorRef): Props = Props(new CustomsClearance(documentsGate, cargoGate))
  final case class Arrive(truck: Truck)
  case object Step
  case object StateLog
  case object AverageWaitingTime
}

class CustomsClearance(documentsGate: ActorRef, cargoGate: ActorRef) extends Actor with ActorLogging {

  import CargoGate._
  import CustomsClearance._

  def receive: Receive = mailbox(0)

  def mailbox(time: Int): Receive = {
    case Step => {
      step(time)
      context.become(mailbox(time + 1))
    }

    case CustomsClearance.StateLog => {
      println(s"Current time is $time\n")
      cargoGate ! CargoGate.StateLog
      documentsGate ! DocumentsGate.StateLog
      context.become(mailbox(time))
    }

    case CustomsClearance.Arrive(truck: Truck) => {
      documentsGate ! DocumentsGate.Arrive(truck)
      context.become(mailbox(time))
    }

    case CustomsClearance.AverageWaitingTime => {
      cargoGate ! CargoGate.AverageWaitingTime
      context.become(mailbox(time))
    }
  }

  private def step(currentTime: Int): Unit = {
    documentsGate ! DepartureTruckToCargoGate

    documentsGate ! CheckDocuments

    cargoGate ! ProgressSearchingLeft
    cargoGate ! ProgressSearchingRight

    cargoGate ! TryToSwap(1)
  }
}

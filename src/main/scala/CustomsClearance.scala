import DocumentsGate.{CheckDocuments, DepartureTruckToCargoGate}
import akka.actor.{Actor, ActorLogging, ActorRef, Props}

object CustomsClearance {
  def props(documentsGate: ActorRef, cargoGate: ActorRef): Props = Props(new CustomsClearance(documentsGate, cargoGate))
  case object Step
  case object StateLog
}

class CustomsClearance(documentsGate: ActorRef, cargoGate: ActorRef) extends Actor with ActorLogging {

  import CustomsClearance._
  import CargoGate._

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
    }
  }

  def step(currentTime: Int): Unit = {
    documentsGate ! DepartureTruckToCargoGate

    documentsGate ! CheckDocuments

    cargoGate ! ProgressSearchingLeft
    cargoGate ! ProgressSearchingRight

    cargoGate ! TryToSwap(1)
  }
}

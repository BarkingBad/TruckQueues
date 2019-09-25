import DocumentsGate.{CheckDocuments, DepartureTruckToCargoGate}
import akka.actor.{Actor, ActorLogging, ActorRef, Props}

object CustomsClearance {
  def props(documentsGate: ActorRef, cargoGate: ActorRef): Props = Props(new CustomsClearance(documentsGate, cargoGate))
  case object Step
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
  }

  def step(currentTime: Int): Unit = {
    println(s"Current time is $currentTime")

    documentsGate ! DepartureTruckToCargoGate

    documentsGate ! CheckDocuments

    cargoGate ! ProgressSearchingLeft
    cargoGate ! ProgressSearchingRight

    cargoGate ! TryToSwap(1)
  }
}

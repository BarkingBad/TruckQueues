package system

import akka.actor.{Actor, ActorRef, Props}

object CustomsClearance {
  def props(documentsGate: ActorRef, cargoGate: ActorRef): Props = Props(new CustomsClearance(documentsGate, cargoGate))
  final case class Arrive(truck: Truck)
  case object Step
  case object StateLog
  case object AverageWaitingTime
}

class CustomsClearance(documentsGate: ActorRef, cargoGate: ActorRef) extends Actor {

  import CargoGate._
  import CustomsClearance._
  import DocumentsGate._

  def receive: Receive = mailbox(0)

  def mailbox(time: Int): Receive = {
    case Step                                  => stepHandler(time)
    case CustomsClearance.StateLog             => stateLogHandler(time)
    case CustomsClearance.Arrive(truck: Truck) => arriveHandler(time, truck)
    case CustomsClearance.AverageWaitingTime   => averageWaitingTimeHandler(time)
  }

  private def stepHandler(time: Int) = {
    step(time)
    context.become(mailbox(time + 1))
  }

  private def stateLogHandler(time: Int) = {
    println(s"Current time is $time\n")
    cargoGate ! CargoGate.StateLog
    documentsGate ! DocumentsGate.StateLog
    context.become(mailbox(time))
  }

  private def arriveHandler(time: Int, truck: Truck) = {
    documentsGate ! DocumentsGate.Arrive(truck)
    context.become(mailbox(time))
  }

  private def averageWaitingTimeHandler(time: Int) = {
    cargoGate ! CargoGate.AverageWaitingTime
    context.become(mailbox(time))
  }

  private def step(currentTime: Int): Unit = {
    documentsGate ! AskIfCanSendTruck

    cargoGate ! ProgressSearchingLeft
    cargoGate ! ProgressSearchingRight

    cargoGate ! TryToSwap
  }
}

package system

import akka.actor.{Actor, Props}
import system.DocumentsGate.DepartureTruckToCargoGate

import scala.collection.immutable.Queue

object CargoGate {
  def props: Props = Props[CargoGate]
  private[system] case object AskForTruckIfPossible
  private[system] case object AverageWaitingTime
  private[system] case object ProgressSearchingLeft
  private[system] case object ProgressSearchingRight
  private[system] final case class AppendTheTruck(truck: Truck)
  private[system] final case class TryToSwap(index: Int)
  private[system] case object StateLog
}

class CargoGate extends Actor {

  import CargoGate._

  def receive: Receive = mailbox(Queue[Truck](), 0, Queue[Truck](), 0)

  def mailbox(trucks1: Queue[Truck], timeSpent1: Int, trucks2: Queue[Truck], timeSpent2: Int): Receive = {

    case ProgressSearchingLeft => {
      if(trucks1.isEmpty) {
        context.become(mailbox(trucks1, timeSpent1, trucks2, timeSpent2))
      } else if(isSearchingFinished(timeSpent1, trucks1)) {
        context.become(mailbox(trucks1.dequeue._2, 0, trucks2, timeSpent2))
      } else {
        context.become(mailbox(trucks1, timeSpent1 + 1, trucks2, timeSpent2))
      }
    }

    case ProgressSearchingRight => {
      if(trucks2.isEmpty) {
        context.become(mailbox(trucks1, timeSpent1, trucks2, timeSpent2))
      } else if(isSearchingFinished(timeSpent2, trucks2)) {
        context.become(mailbox(trucks1, timeSpent1, trucks2.dequeue._2, 0))
      } else {
        context.become(mailbox(trucks1, timeSpent1, trucks2, timeSpent2 + 1))
      }
    }

    case AverageWaitingTime => {
      println("" + averageWaitingTime(trucks1, timeSpent1, trucks2, timeSpent2))
      context.become(mailbox(trucks1, timeSpent1, trucks2, timeSpent2))
    }

    case AskForTruckIfPossible => {
      if(canAppendToQueue(trucks1) || canAppendToQueue(trucks2)) {
        sender ! DepartureTruckToCargoGate
      }
      context.become(mailbox(trucks1, timeSpent1, trucks2, timeSpent2))
    }

    case AppendTheTruck(truck: Truck) => {
      if(trucks1.isEmpty && trucks2.isEmpty) {
        context.become(mailbox(trucks1 :+ truck, timeSpent1, trucks2, timeSpent2))
      } else if(trucks1.size == 1 && trucks2.isEmpty) {
        if(trucks1.front.weight - timeSpent1 > truck.weight) {
          context.become(mailbox(trucks1, timeSpent1, trucks2 :+ truck, timeSpent2))
        } else {
          context.become(mailbox(trucks2 :+ truck, timeSpent2, trucks1, timeSpent1))
        }
      } else if(canAppendToQueue(trucks1) && canAppendToQueue(trucks2) && truck.weight >= averageWaitingTime(trucks1, timeSpent1, trucks2, timeSpent2)) {
        context.become(mailbox(trucks1 :+ truck, timeSpent1, trucks2, timeSpent2))
      } else if(canAppendToQueue(trucks2)) {
        context.become(mailbox(trucks1, timeSpent1, trucks2 :+ truck, timeSpent2))
      } else {
        context.become(mailbox(trucks1 :+ truck, timeSpent1, trucks2, timeSpent2))
      }
    }

    case TryToSwap(index: Int) => {
      if(index < 5) {
        if(isSwapProfitable(index, trucks1, timeSpent1, trucks2, timeSpent2)) {
          val queuesTuple = swapTrucks(index, trucks1, trucks2)
          context.become(mailbox(queuesTuple._1, timeSpent1, queuesTuple._2, timeSpent2))
        } else {
          context.become(mailbox(trucks1, timeSpent1, trucks2, timeSpent2))
        }
        self ! TryToSwap(index + 1)
      }
    }

    case StateLog => {
      val outputString = new StringBuilder("Cargo Gate\n")
      if(trucks1.nonEmpty) outputString ++= s"Left queue progress $timeSpent1/${trucks1.front.weight}\n"
      outputString ++= "Left cargo queue: "  + trucks1.toString() + "\n"
      if(trucks2.nonEmpty) outputString ++= s"Right queue progress $timeSpent2/${trucks2.front.weight}\n"
      outputString ++= "Right cargo queue: " + trucks2.toString() + "\n"

      println(outputString)
    }
  }

  private def isSwapProfitable(id: Int, queue1: Queue[Truck], timeSpent1: Int, queue2: Queue[Truck], timeSpent2: Int): Boolean = {
    if(id >= queue1.size || id >= queue2.size) return false
    val previousState = averageWaitingTime(queue1, timeSpent1, queue2, timeSpent2)
    val queuesTuple = swapTrucks(id, queue1, queue2)
    val currentState = averageWaitingTime(queuesTuple._1, timeSpent1, queuesTuple._2, timeSpent2)
    previousState > currentState
  }

  private def swapTrucks(id: Int, that: Queue[Truck], other: Queue[Truck]): (Queue[Truck], Queue[Truck]) = {
    val thatValue = that(id)
    val otherVaule = other(id)
    (that.updated(id, otherVaule), other.updated(id, thatValue))
  }

  private def canAppendToQueue(queue: Queue[Truck]): Boolean = queue.size < 5

  private def averageWaitingTime(trucks1: Queue[Truck], timeSpent1: Int, trucks2: Queue[Truck], timeSpent2: Int): Double = {
    (averageWaitingTimeForOneQueue(trucks1, timeSpent1) + averageWaitingTimeForOneQueue(trucks2, timeSpent2))/2
  }

  private def averageWaitingTimeForOneQueue(queue: Queue[Truck], timeSpent: Int): Double = {
    if(queue.isEmpty) return 0
    val weights = queue.toList.map(_.weight)
    val updatedWeights = weights.updated(0, weights.head - timeSpent)
    val res = updatedWeights.reverse.zipWithIndex.reduce((acc, truck) => (acc._1 + truck._1 * (truck._2 + 1), 0))
    res._1.toFloat / queue.length
  }

  private def isSearchingFinished(timeSpent: Int, queue: Queue[Truck]): Boolean = {
    if (timeSpent == queue.front.weight) {
      return true
    }
    return false
  }
}

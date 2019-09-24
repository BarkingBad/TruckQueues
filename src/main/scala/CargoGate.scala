import scala.collection.immutable.Queue
import akka.actor.{Actor, ActorLogging, Props}

object CargoGate {
  def props: Props = Props[CargoGate]
  case object AverageWaitingTime
  case object ProgressSearchingLeft
  case object ProgressSearchingRight
  final case class AppendTheTruck(truck: Truck)
}

class CargoGate extends Actor with ActorLogging {

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

    case AppendTheTruck(truck: Truck) => {
      if(canAppendToQueue(trucks1)) context.become(mailbox(trucks1 :+ truck, timeSpent1, trucks2, timeSpent2))
      else if(canAppendToQueue(trucks2)) context.become(mailbox(trucks1, timeSpent1, trucks2 :+ truck, timeSpent2))
    }

  }

//  def isSwapProfitable(id: Int, other: CargoGate): Boolean = {
//    if(id > this.queue.size - 1 || other.queue.size < id) return false
//    val previousState = (averageWaitingTime + other.averageWaitingTime)/2
//    val queuesTuple = swapTrucks(id, this.queue, other.queue)
//    val currentState = (averageWaitingTimeById(queuesTuple._1.size, queuesTuple._1) + other.averageWaitingTimeById(queuesTuple._2.size, queuesTuple._2))/2
//    previousState > currentState
//  }
//
//  def swapTrucksAndMessage(other: CargoGate): Unit = {
//    val id = this.queue.size - 1
//    println(s"Swapping truck ${this.queue(id).id} that weighs ${this.queue(id).weight} with truck ${other.queue(id).id} that weighs ${other.queue(id).weight} at position $id")
//    swapTrucks(id, this.queue, other.queue)
//  }
//
//  private def swapTrucks(id: Int, that: Queue[Truck], other: Queue[Truck]): (Queue[Truck], Queue[Truck]) = {
//    val thatValue = that(id)
//    val otherVaule = other(id)
//    (that.updated(id, otherVaule), other.updated(id, thatValue))
//  }

  def canAppendToQueue(queue: Queue[Truck]): Boolean = queue.size < 5





  def averageWaitingTime(trucks1: Queue[Truck], timeSpent1: Int, trucks2: Queue[Truck], timeSpent2: Int): Double = {
    (averageWaitingTimeForOneQueue(trucks1, timeSpent1) + averageWaitingTimeForOneQueue(trucks2, timeSpent2))/2
  }

  def averageWaitingTimeForOneQueue(queue: Queue[Truck], timeSpent: Int): Double = {
    val weights = queue.toList.map(_.weight)
    val updatedWeights = weights.updated(0, weights.head - timeSpent)
    val res = updatedWeights.reverse.zipWithIndex.reduce((acc, truck) => (acc._1 + truck._1 * (truck._2 + 1), 0))
    res._1.toFloat / queue.length
  }

  def isSearchingFinished(timeSpent: Int, queue: Queue[Truck]): Boolean = {
    println(s"Truck ${queue.front.id} searching progress $timeSpent/${queue.front.weight}")
    if (timeSpent == queue.front.weight) {
      println(s"Truck ${queue.front.id} has been successfully searched and departures!")
      return true
    }
    return false
  }
}

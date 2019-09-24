import scala.collection.mutable

class CargoGate() {
  private val queue: mutable.Queue[Truck] = mutable.Queue()
  private var timeSpent: Int = 0

  def averageWaitingTimeById(index: Int): Double = {
    val weights = queue.toList.slice(0, index).map(_.weight)
    val updatedWeights = weights.updated(0, weights.head - timeSpent)
    val res = updatedWeights.reverse.zipWithIndex.reduce((acc, truck) => (acc._1 + truck._1 * (truck._2 + 1), 0))
    res._1.toFloat / queue.length
  }

  def averageWaitingTime(): Double = averageWaitingTimeById(queue.size)

  def areTrucksWaiting() = queue.nonEmpty

  def isTruckBeingSearched(): Boolean = {
    queue.length != 0
  }

  def progressSearching(): Unit = {
    timeSpent += 1
    println(s"Truck ${queue.front.id} searching progress $timeSpent/${queue.front.weight}")
    if(timeSpent == queue.front.weight) {
      println(s"Truck ${queue.front.id} has been successfully searched and departures!")
      timeSpent = 0
      queue.dequeue()
    }
  }

  def canAppendTruck(): Boolean = {
    queue.size < 5
  }

  def isSwapProfitable(other: CargoGate): Boolean = {
    val id = this.queue.size - 1
    if(other.queue.size < this.queue.size) return false
    val previousState = (averageWaitingTimeById(id) + other.averageWaitingTimeById(id))/2
    val thisQueueCopy = this.queue.clone
    val otherQueueCopy = other.queue.clone
    swapTrucks(id, thisQueueCopy, otherQueueCopy)
    val currentState = (averageWaitingTimeById(id) + other.averageWaitingTimeById(id))/2
    previousState > currentState
  }

  def swapTrucksAndMessage(other: CargoGate): Unit = {
    val id = this.queue.size - 1
    println(s"Swapping truck ${this.queue(id).id} that weighs ${this.queue(id).weight} with truck ${other.queue(id).id} that weighs ${other.queue(id).weight} at position $id")
    swapTrucks(id, this.queue, other.queue)
  }

  private def swapTrucks(id: Int, that: mutable.Queue[Truck], other: mutable.Queue[Truck]): Unit = {
    val tmp = that(id)
    that(id) = other(id)
    other(id) = tmp
  }

  def appendTruck(truck: Truck) = {
    if(canAppendTruck) queue.enqueue(truck)
  }
}

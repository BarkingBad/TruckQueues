import scala.collection.mutable

class CargoGate() {
  private val queue: mutable.Queue[Truck] = mutable.Queue()
  private var timeSpent: Int = 0

  def averageWaitingTime(): Double = {
    val collection = queue.toList.map(_.weight)
    val updatedCollection = collection.updated(0, collection(0) - timeSpent)
    val res = updatedCollection.reverse.zipWithIndex.reduce((acc, truck) => (acc._1 + truck._1 * (truck._2 + 1), 0))
    res._1.toFloat / queue.length
  }

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

  def appendTruck(truck: Truck) = {
    if(canAppendTruck()) queue.enqueue(truck)
  }


}

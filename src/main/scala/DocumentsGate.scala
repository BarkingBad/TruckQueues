import scala.collection.mutable

class DocumentsGate() {
  private val queue: mutable.Queue[Truck] = mutable.Queue()
  private var truck: Truck = null

  def addAll(trucks: List[Truck]) = {
    queue ++= trucks
  }

  def areTrucksWaiting() = queue.nonEmpty

  def isTruckChecked() = truck != null

  def getCheckedTruck(): Truck = {
    val result = truck
    truck = null
    result
  }

  def checkWaitingTruck(): Unit = {
    println(s"Truck ${queue.front.id} has documents checked")
    truck = queue.dequeue()
  }



}

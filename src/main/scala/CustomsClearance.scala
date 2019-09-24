import scala.collection.mutable

object CustomsClearance {

  val documentsGateQueue = new DocumentsGate
  val leftCargoGate = new CargoGate
  val rightCargoGate = new CargoGate

  private var currentTime = 0

  def step(): Unit = {
    println(s"Current time is $currentTime")

    if(leftCargoGate.isTruckBeingSearched) {
      leftCargoGate.progressSearching
    }

    if(rightCargoGate.isTruckBeingSearched) {
      rightCargoGate.progressSearching
    }

    if(documentsGateQueue.isTruckChecked) {
      if(leftCargoGate.canAppendTruck && !leftCargoGate.isTruckBeingSearched) {
        leftCargoGate.appendTruck(documentsGateQueue.getCheckedTruck())
      }
      else if (rightCargoGate.canAppendTruck && !rightCargoGate.isTruckBeingSearched) {
        rightCargoGate.appendTruck(documentsGateQueue.getCheckedTruck)
      }
      else {
        if(leftCargoGate.averageWaitingTime < documentsGateQueue.getCheckedTruckEstimatedTime) {
          leftCargoGate.appendTruck(documentsGateQueue.getCheckedTruck)
        }
        else {
          rightCargoGate.appendTruck(documentsGateQueue.getCheckedTruck)
          if(rightCargoGate.isSwapProfitable(leftCargoGate)) {
            rightCargoGate.swapTrucksAndMessage(leftCargoGate)
          }
        }
      }
    }

    if(documentsGateQueue.areTrucksWaiting && !documentsGateQueue.isTruckChecked) {
      documentsGateQueue.checkWaitingTruck
    }

    currentTime += 1
  }

  def main(args: Array[String]): Unit = {
    documentsGateQueue.addAll(args.toList.zipWithIndex.map{ case (elem, index) => new Truck(index, elem.toInt) })

    while(documentsGateQueue.areTrucksWaiting || leftCargoGate.areTrucksWaiting || rightCargoGate.areTrucksWaiting) {
      step()
    }
  }

}

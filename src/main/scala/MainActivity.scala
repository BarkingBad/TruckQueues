import akka.actor.{ActorRef, ActorSystem}

object MainActivity {

  def main(args: Array[String]): Unit = {
    import DocumentsGate._
    import CustomsClearance._

    val system: ActorSystem = ActorSystem("TruckQueues")

    val cargoGate: ActorRef = system.actorOf(CargoGate.props, "CargoGate")
    val documentsGate: ActorRef = system.actorOf(DocumentsGate.props(cargoGate), "DocumentsGate")
    val customsClearance: ActorRef = system.actorOf(CustomsClearance.props(documentsGate, cargoGate), "CustomsClearance")

    documentsGate ! Arrive(Truck(1, 4))
    documentsGate ! Arrive(Truck(2, 6))
    documentsGate ! Arrive(Truck(3, 7))
    documentsGate ! Arrive(Truck(4, 2))
    documentsGate ! Arrive(Truck(5, 2))
    documentsGate ! Arrive(Truck(6, 4))
    documentsGate ! Arrive(Truck(7, 8))

    for(i <- 1 to 30) {
      customsClearance ! Step
      customsClearance ! CustomsClearance.StateLog
      Thread.sleep(100)

    }
  }

}

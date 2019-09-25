package cli

import akka.actor.{ActorRef, ActorSystem, PoisonPill}
import system.{CargoGate, CustomsClearance, DocumentsGate, Truck}

object MainActivity {

  def main(args: Array[String]): Unit = {

    val system: ActorSystem = ActorSystem("TruckQueues")

    val cargoGate: ActorRef = system.actorOf(CargoGate.props, "system.CargoGate")
    val documentsGate: ActorRef = system.actorOf(DocumentsGate.props(cargoGate), "system.DocumentsGate")
    val customsClearance: ActorRef = system.actorOf(CustomsClearance.props(documentsGate, cargoGate), "system.CustomsClearance")

    val indicies = Stream.from(1).iterator


    while(true) {
      val input = scala.io.StdIn.readLine()
      val splitInput = input.split(' ')

      splitInput(0) match {
        case "ARRIVE"   => arriveHandler(customsClearance, indicies.next, splitInput(1))
        case "STEP"     => stepHandler(customsClearance)
        case "STATUS"   => statusHandler(customsClearance)
        case "AVGTIME"  => averageTimeHandler(customsClearance)
        case "EXIT"     => exitHandler
        case _          => defaultHandler
      }
    }

//    COMMENT LOOP AND UNCOMMENT FOLLOWING CODE TO GET AUTOMATED EXAMPLE OF SYSTEM

//    customsClearance ! CustomsClearance.Arrive(Truck(1, 4))
//    customsClearance ! CustomsClearance.Arrive(Truck(2, 6))
//    customsClearance ! CustomsClearance.Arrive(Truck(3, 7))
//    customsClearance ! CustomsClearance.Arrive(Truck(4, 2))
//    customsClearance ! CustomsClearance.Arrive(Truck(5, 2))
//    customsClearance ! CustomsClearance.Arrive(Truck(6, 4))
//    customsClearance ! CustomsClearance.Arrive(Truck(7, 8))
//
//    for(i <- 1 to 30) {
//      customsClearance ! CustomsClearance.Step
//      Thread.sleep(50)
//      customsClearance ! CustomsClearance.StateLog
//      Thread.sleep(50)
//    }
  }

  private def arriveHandler(customsClearance: ActorRef, index: Int, argument: String) = {
    try {
      val weight = argument.toInt
      customsClearance ! CustomsClearance.Arrive(Truck(index, weight))
      println(s"Successfully added new truck to queue!")
    } catch {
      case _: NumberFormatException => println("Provide second argument to be number!")
    }
  }

  private def stepHandler(customsClearance: ActorRef) = customsClearance ! CustomsClearance.Step

  private def statusHandler(customsClearance: ActorRef) = customsClearance ! CustomsClearance.StateLog

  private def averageTimeHandler(customsClearance: ActorRef) = customsClearance ! CustomsClearance.AverageWaitingTime

  private def exitHandler = System.exit(0)

  private def defaultHandler = println("Unknown command, use one of the following\nARRIVE $weight\nSTEP\nSTATUS\nAVGTIME\n")

}

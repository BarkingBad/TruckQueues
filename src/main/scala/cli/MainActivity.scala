package cli

import akka.actor.{ActorRef, ActorSystem, PoisonPill}
import system.CustomsClearance.Step
import system.{CargoGate, CustomsClearance, DocumentsGate, Truck}

object MainActivity {

  def main(args: Array[String]): Unit = {

    val system: ActorSystem = ActorSystem("TruckQueues")

    val cargoGate: ActorRef = system.actorOf(CargoGate.props, "system.CargoGate")
    val documentsGate: ActorRef = system.actorOf(DocumentsGate.props(cargoGate), "system.DocumentsGate")
    val customsClearance: ActorRef = system.actorOf(CustomsClearance.props(documentsGate, cargoGate), "system.CustomsClearance")

    var inc = 1


    for(i <- 1 to 30) {
      customsClearance ! CustomsClearance.Arrive(Truck(i, 20))
    }


    while(true) {
      val input = scala.io.StdIn.readLine()
      val splitInput = input.split(' ')

      splitInput(0) match {
        case "ARRIVE"   => {
          try {
            val weight = splitInput(1).toInt
            customsClearance ! CustomsClearance.Arrive(Truck(inc, weight))
            println(s"Successfully added new truck to queue!")
            inc += 1
          } catch {
            case _: NumberFormatException => println("Provide second argument to be number!")
          }
        }

        case "STEP"     => {
          customsClearance ! CustomsClearance.Step
        }

        case "STATUS"   => {
          customsClearance ! CustomsClearance.StateLog
        }

        case "AVGTIME"  => {
          customsClearance ! CustomsClearance.AverageWaitingTime
        }

        case "EXIT"     => {
          documentsGate ! PoisonPill
          cargoGate ! PoisonPill
          customsClearance ! PoisonPill
          System.exit(0)
        }

        case _          => {
          println("Unknown command, use one of the following\nARRIVE $weight\nSTEP\nSTATUS\nAVGTIME\n")
        }
      }
    }

//    COMMENT LOOP AND UNCOMMENT FOLLOWING CODE TO GET AUTOMATED EXAMPLE OF SYSTEM
//
//    documentsGate ! Arrive(system.Truck(1, 4))
//    documentsGate ! Arrive(system.Truck(2, 6))
//    documentsGate ! Arrive(system.Truck(3, 7))
//    documentsGate ! Arrive(system.Truck(4, 2))
//    documentsGate ! Arrive(system.Truck(5, 2))
//    documentsGate ! Arrive(system.Truck(6, 4))
//    documentsGate ! Arrive(system.Truck(7, 8))
//
//    for(i <- 1 to 30) {
//      customsClearance ! Step
//      customsClearance ! CustomsClearance.StateLog
//      Thread.sleep(100)
//    }
  }

}


object GarageDoorOpener {

  sealed trait State
  object Open extends State
  object Closed extends State
  object Opening extends State
  object Closing extends State
  object StoppedWhileOpening extends State
  object StoppedWhileClosing extends State

  val startState: State = Closed
  var currentState: State = startState

  def getState: State = currentState

  def changeStateOnClick = getState match {
    case Open => currentState = Closing
    case Closed => currentState = Opening
    case Opening => currentState = StoppedWhileOpening
    case Closing => currentState = StoppedWhileClosing
    case StoppedWhileOpening => currentState = Closing
    case StoppedWhileClosing => currentState = Opening
  }

  def changeStateOnComplete = getState match {
    case Opening => currentState = Open
    case Closing => currentState = Closed
    case _ => println("Error")
  }

  def parseInput(input: String) = input match {
      case "button_clicked" => {
        println("> Button clicked")
        changeStateOnClick
        printState(currentState)
      }
      case "cycle_complete" => {
        println("> Cycle complete.")
        changeStateOnComplete
        printState(currentState)
      }
      case _ => println("Error in command")
  }

  def printState (state: State) = state match {
    case Open => println("Door: OPEN")
    case Closed => println("Door: CLOSED")
    case Opening => println("Door: OPENING")
    case Closing => println("Door: CLOSING")
    case StoppedWhileOpening => println("Door: STOPPED WHILE OPENING")
    case StoppedWhileClosing => println("Door: STOPPED WHILE CLOSING")
  }

  val inputCommands = List("button_clicked", "cycle_complete", "button_clicked", "button_clicked", "button_clicked", "button_clicked", "button_clicked", "cycle_complete")

  for (x <- inputCommands) parseInput(x)
}


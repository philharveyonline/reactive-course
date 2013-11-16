package simulations

import common._

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal

  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () => afterDelay(0) {
        println(
          "  " + currentTime + ": " + name + " -> " +  wire.getSignal)
      }
    }
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output.setSignal(!inputSig) }
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig & a2Sig) }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  //
  // to complete with orGates and demux...
  //

  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(OrGateDelay) { output.setSignal(a1Sig | a2Sig) }
    }
    a1 addAction orAction
    a2 addAction orAction
  }

  def orGate2(a1: Wire, a2: Wire, output: Wire) {
	  val notA1, notA2, notOutput = new Wire
	  inverter(a1, notA1)
	  inverter(a2, notA2)
	  andGate(notA1, notA2, notOutput)
	  inverter(notOutput, output)
  }



  /**
   * A demux with n controls is composed of the following side by side:
   *
   * First half outputs:     A smaller demux whose input    is: AND(I, NOT(cn))
   *
   * Second half of outputs: A smaller demux whose controls are AND'ed with cn
   */
  def demux(in: Wire, c: List[Wire], out: List[Wire]) {
    def straightThroughAction() {
      afterDelay(0) { out(0).setSignal(in.getSignal) }
    }

    c match {
      case Nil => in addAction(straightThroughAction)
      case highControl :: rest => {

        val (highOut, lowOut) = out splitAt(c.size)
        val notHighControl, lowIn, highIn = new Wire

        // demux for low output bits
        inverter(highControl, notHighControl)
        andGate(notHighControl, in, lowIn)
        demux(lowIn, rest, lowOut)

        // demux for high output bits
        andGate(in, highControl, highIn)
        demux(highIn, rest, highOut)
      }
    }
  }

}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  //
  // to complete with orGateExample and demuxExample...
  //
}

object CircuitMain extends App {
  // You can write tests either here, or better in the test class CircuitSuite.
  Circuit.andGateExample
}

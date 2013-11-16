package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }

  //
  // to complete with tests for orGate, demux, ...
  //
  
  test("orGate example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    testOrGate(in1, in2, out)
  }
  
  test("orGate2 example") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    testOrGate(in1, in2, out)
  }
  
  private def testOrGate(in1: Wire, in2: Wire, out: Wire): Unit = {
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "0 OR 0 = 0")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === true, "1 OR 0 = 1")


    in1.setSignal(false)
    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "0 OR 1 = 1")

    
    in1.setSignal(true)
    run
    
    assert(out.getSignal === true, "1 OR 1 = 1")
  }
  
  
  test("demux 0") {
    val in = new Wire
    val c = List()
    val out = new Wire
    val outWires = List(out)
    demux(in, c, outWires)
    
    in.setSignal(false)
    run
    assert(out.getSignal === false, "0 demux [] == 0")
    
    in.setSignal(true)
    run
    assert(out.getSignal === true, "1 demux [] == 1")
  }
  
  private def toSignals(outs: List[Wire]): List[Int] = outs map(out => if(out.getSignal) 1 else 0)
  
  test("demux 1") {
    val in, c0, out0, out1 = new Wire
    val outs = List(out1, out0)
    demux(in, List(c0), outs)
    
    in.setSignal(false)
    c0.setSignal(false)
    run
    assert(toSignals(outs) === List(0, 0), "0 demux (0) = (0, 0)")
        
    in.setSignal(true)
    c0.setSignal(false)
    run
    assert(toSignals(outs) === List(0, 1), "1 demux (0) = (0, 1)")
    
    in.setSignal(true)
    c0.setSignal(true)
    run
    assert(toSignals(outs) === List(1, 0), "1 demux (1) = (1, 0)")
  }

    
  test("demux 2") {
    val in, c0, c1, out0, out1, out2, out3 = new Wire
    val controls = List(c1, c0) 
    val outs: List[Wire] = List(out3, out2, out1, out0)
    
    demux(in, controls, outs)
    
    in.setSignal(false)
    c0.setSignal(true)
    run
    assert(toSignals(outs) === List(0, 0, 0, 0), "0 demux (0, 1) = (0, 0, 0, 0)")
        
    in.setSignal(true)
    c0.setSignal(false)
    c1.setSignal(false)
    run
    assert(toSignals(outs) === List(0, 0, 0, 1),  "1 demux (0, 0) = (0, 0, 0, 1)")
    
    in.setSignal(true)
    c0.setSignal(true)
    c1.setSignal(false)
    run
    assert(toSignals(outs) === List(0, 0, 1, 0),  "1 demux (0, 1) = (0, 0, 1, 0)")
    
    in.setSignal(true)
    c0.setSignal(false)
    c1.setSignal(true)
    run
    assert(toSignals(outs) === List(0, 1, 0, 0),  "1 demux (1, 0) = (0, 1, 0, 0)")
    
    in.setSignal(true)
    c0.setSignal(true)
    c1.setSignal(true)
    run
    assert(toSignals(outs) === List(1, 0, 0, 0),  "1 demux (1, 1) = (1, 0, 0, 0)")
  }
}

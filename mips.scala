// Main class. Will perform execution, will store the data that will get changed
// Will run everything.

object mips {
    // reads in the data
    def ReadIn(fname1 : String, fname2 : String):Unit = {
      val dis = new DataInputStream(new BufferedInputStream(new
      FileInputStream(fname)))
      val mem1 = Array.fill(1 << 13)(dis.readInt)
      val mem2 = Array.fill(1 << 13)(dis.readInt)
      dis.close
      val dis2 = new DataInputStream(new BufferedInputStream(new
      FileInputStream(fname2)))
      val regs = Array.fill(32)(dis2.readInt)
      dis2.close
    }

    // writes out all the data
    def WriteOut(fname1 : String, fname2 : String):Unit = {
      val dos = new DataOutputStream(new BufferedOutputStream(new
      FileOutputStream(fname)))
      for (i <- memory) dos.writeInt(i)
      dos.close()
      val dos2 = new DataOutputStream(new BufferedOutputStream(new
      FileOutputStream(fname2)))
      for (i <- regs) dos2.writeInt(i)
      dos2.close()
    }



  def main(args : Array[String]) = {
    val alu = new Alu()
    alu.AluTest();
    val extract = new ExtractBits()
    extract.Test()
  }
}

// memory will be 16 bits long
// registers will be 32 long
class Executor(memory : Array[Int],registers : Array[Int]) {
  var pc = 0
  val extracter = new ExtractBits()
  val controller = new Control()

  val alu = new Alu()

  // placeholder
  def sign_extender(inp : Int) : Int = {
    39
  }

  def Execute():Unit = {

    // while the pc counter isn't at the last instruction
    while (pc != 0x3FFC) {
      val pct = aly.ALUControl(pc,4,2)
      val instruction = memory(pct)
      //  Output[0] = 31 - 26 opcode
      //  Output[1] = 25 - 21 rs
      //  Output[2] = 20 - 16 rt
      //  Output[3] = 15 - 11
      //  Output[4] = 15 - 0
      //  Output[5] = 5 - 0
      // Need to double check this for correctness
      val (zero,one,two,three,four,five) = extracter.Extract(instruction)
      // this gross line just gets all of the control wires
      val (regDst, alu_src,memto_reg,reg_write,mem_read,mem_write,branch,alu_op)
          = controller.Outs(zero)
      val reg_one = registers(one) // ALU input 1
      val reg_two = registers(two)
      val alu_control = alu.ALUControl(alu_op,five)
      // MUX for write register
      val write_reg = if (regDst) three else two
      val sgned_extended = sign_extender(four)
      // MUX for alu_input
      val alu_input_two = if (alu_src) sgned_extended else reg_two
      val (alu_result,zero_boolean) = alu.Execute(reg_one,alu_input_two,alu_control)

      // THIS SECTION IS FOR THE PC
      val pc_change_value = sgned_extended << 1
      val (pc_branch,_) = alu.Execute(pct,pc_change_value,2)
      // Mux for pc
      pc = if (branch && zero_boolean) pc_branch else pct
      // END OF PC SECTION
      // far write mux
      val write_data = if (memto_reg && mem_read) memory(alu_result) else alu_result
      if (mem_write) memory(alu_result) = reg_two
      if (reg_write) registers(write_reg) = write_data

    }


  }
}

// Untested
class ExtractBits {


  // Extracts the kth through n bits.
  // For example GetKn(56323,15,0) should get the 15 least significant digits
  def GetKn(instruction : Int,k : Int, n : Int) : Int = {
    println(instruction.toBinaryString)
    val shft = instruction >>> n
    println(shft.toBinaryString)
    shft & ((1 << (k - n)) - 1)
  }
  // Output:
  //  Output[0] = 31 - 26
  //  Output[1] = 25 - 21
  //  Output[2] = 20 - 16
  //  Output[3] = 15 - 11
  //  Output[4] = 15 - 0
  def Extract(instruction : Int) : (Int,Int,Int,Int,Int,Int) = {
    val out5 = GetKn(instruction,5,0)
    val out4 = GetKn(instruction,15, 0)
    val out3 = GetKn(instruction,15,11)
    val out2 = GetKn(instruction,20,16)
    val out1 = GetKn(instruction,25,21)
    val out0 = GetKn(instruction,31,26)
    (out0,out1,out2,out3,out4,out5)
  }

  def Test():Unit = {
    // fill in with test code later, get from textbook
  }
}
// Tested execute, not control
class Alu {
  def Execute(data_one : Int, data_two : Int, ftype : Int) : (Int,Boolean) = {
    val res = ftype match {
      case 0 => data_one & data_two
      case 1 => data_one | data_two
      case 2 => data_one + data_two
      case 6 => data_one - data_two
      case 7 => if (data_one < data_two) 0 else 1
      case _ =>  ~(data_one | data_two)
    }

    (res,res == 0)
  }
  // finished
  def ALUControl(alu_op : Int, funct : Int) : Int = {
    // implements truth table figure 4.13
    (alu_op,funct) match {
      // row 1
      case (0,_) => 2
      // row 2 does not need 3
      case (1,_) => 6
      // row 3
      case (_, 0) => 2
      case (_,16) => 2
      case (_,32) => 2
      case (_,48) => 2
      // row 4
      case (_, 2) => 6
      case (_,18) => 6
      case (_,34) => 6
      case (_,50) => 6
      // row 5
      case (_, 4) => 0
      case (_,20) => 0
      case (_,36) => 0
      case (_,52) => 0
      // row 6
      case (_, 5) => 1
      case (_,21) => 1
      case (_,37) => 1
      case (_,53) => 1
      // row 7
      case (_,10) => 7
      case (_,26) => 7
      case (_,42) => 7
      case (_,58) => 7

    }
  }

  def AluTest() : Unit = {
    println("  Beginning AluTest")
    if (Execute(5,5,2) != (10,false)) {
      println("    Failure on test One")
      return;
    }
    if (Execute(5,5,3) != (0,true)) {
      println("    Failure on test Two")
      return;
    }
    if (Execute(5,6,2) != (11,false)) {
      println("    Failure on test Three")
      return;
    }
    if (Execute(5,-5,2) != (0,true)) {
      println("    Failure on test One")
      return;
    }
    println("  Success of AluTest")
  }
}

// If it doesn't work, this is a good place to look
class Control {
  // The first output will be a 7 long tuple of Booleans:
  //    entirely so it can be pattern matched.
  // The second output will be Int:
  //    ALUOp1 and ALUOp0, encoded as a single int of figure 4.18
  def Outs(instruction : Int) : (Boolean,Boolean,Boolean,Boolean,Boolean,Boolean,Boolean,Int) = {
    // itype will hold which row for figure 4.18 is appropriate
    // cases are the different possibilities for the various types
    // opcodes come from here
    //   http://www-inst.eecs.berkeley.edu/~cs61c/resources/MIPS_Green_Sheet.pdf
    // rformat = 0
    // lw = 0
    // sw = 0
    // beq = 0
    val itype = instruction match {
      case 35 => 1
      case 43 => 2
      case 4 => 3
      // everything else is rformat
      case _ => 0
    }
    // derived from figure 4.18
    itype match {
      case 0 => (true, false,false,true,false,false,false,2)
      case 1 => (false,true, true, true,true, false,false,0)
      case 2 => (false,true, true, false,false,true,false,0)
      case 3 => (false,false,false,false,false,false,true,1)
    }
  }
}

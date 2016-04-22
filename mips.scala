// Main class. Will perform execution, will store the data that will get changed
// Will run everything.
import java.io.FileInputStream
import java.io.BufferedInputStream
import java.io.DataInputStream
import java.io.FileOutputStream
import java.io.BufferedOutputStream
import java.io.DataOutputStream


object mips {
    // reads in the data
    def read_in(fname1 : String, fname2 : String):(Array[Int],Array[Int]) = {
      val dis = new DataInputStream(new BufferedInputStream(new FileInputStream(fname1)))
      val memory = Array.fill(1 << 14)(dis.readInt)
      dis.close
      val dis2 = new DataInputStream(new BufferedInputStream(new FileInputStream(fname2)))
      val regs = Array.fill(32)(dis2.readInt)
      dis2.close
      return (memory,regs)
    }

    // writes out all the data
    def write_out(memory : Array[Int], regs : Array[Int], fname1 : String, fname2 : String):Unit = {
      val dos = new DataOutputStream(new BufferedOutputStream(new FileOutputStream(fname1)))
      for (i <- memory) dos.writeInt(i)
      dos.close()
      val dos2 = new DataOutputStream(new BufferedOutputStream(new FileOutputStream(fname2)))
      for (i <- regs) dos2.writeInt(i)
      dos2.close()
    }



  def test():Unit = {
    println("Beginning Tests")
    val alu = new Alu()
    alu.AluTest();
    println("Beginning extractor test")
    val extracter = new ExtractBits()
    extracter.Test()

  }

  def run(fname1 : String, fname2 : String):Unit = {
    val (memory, registers) = read_in(fname1 + ".in",fname2 + ".in")
    val (memory_test,registers_test) =
      read_in(fname1 + "_test.out",fname2 + "_test.out")
    val executor = new Executor(memory,registers)
    executor.Execute()

    if (memory_test.deep != memory.deep) {
      println("Failure on memory test")

    }
    if (registers_test.deep != registers.deep) {
      println("Failure on registers")
    }

    write_out(executor.memory,executor.registers, fname1 + ".out",fname2 + ".out")

  }
  val tst = true
  def main(args : Array[String]) = {
    if (tst) run("memory","regs")
    else test()
  }
}

// memory will be 16 bits long
// registers will be 32 long
class Executor(var memory : Array[Int],var registers : Array[Int]) {
  var pc = 0
  val extracter = new ExtractBits()
  val controller = new Control()

  val alu = new Alu()

  // placeholder
  def sign_extender(inp : Int) : Int = {
    return inp
  }

  def Execute():Unit = {

    // while the pc counter isn't at the last instruction
    while (pc < 0x3FFC) {
      //println(pc)
      var counter = 1
      for (i <- registers) {
        print(i + "  ")
        if (counter % 4 == 0) println("")
        counter += 1
      }
      //println("")
      val (pct,_) = alu.Execute(pc,4,2)
      val instruction = memory(pct)

      //  Output[0] = 31 - 26 opcode
      //  Output[1] = 25 - 21 rs
      //  Output[2] = 20 - 16 rt
      //  Output[3] = 15 - 11
      //  Output[4] = 15 - 0
      //  Output[5] = 5 - 0
      // Need to double check this for correctness
      // rformat add, sub, etc.
      //         0 -> opcode
      //         1 -> rs
      //         2 -> rt
      ///        3 -> rd
      //         5 -> funct
      // iformart (beq, sw, lw)
      //         0 -> opcode
      //         1 -> rs
      //         2 -> rt
      //         4 -> immediate
      val (zero,one,two,three,four,five) = extracter.Extract(instruction)
      //println("rformat : R[" +  three + "] = R[" + one + "] + " + two)
      // this gross line just gets all of the control wires
      val (regDst, alu_src,memto_reg,reg_write,mem_read,mem_write,branch,alu_op)
          = controller.Outs(zero)

      val reg_one = registers(one) // Read register 1
      val reg_two = registers(two) // read register 2
      val alu_control = alu.ALUControl(alu_op,five) // gets value on wire for
                                                    // alu control
      // MUX for write register
      val which_write_reg = if (regDst) three else two

      val sgned_extended = sign_extender(four)
      // MUX for alu_input
      val alu_input_two = if (alu_src) sgned_extended else reg_two

      val (alu_result,zero_boolean) = alu.Execute(reg_one,alu_input_two,alu_control)

      // THIS SECTION IS FOR THE PC
      val pc_change_value = sgned_extended << 2
      val (pc_branch,_) = alu.Execute(pct,pc_change_value,2)
      // Mux for pc
      pc = if (branch && zero_boolean) pc_branch else pct
      // END OF PC SECTION
      // far write mux
      val write_data = if (memto_reg && mem_read) memory(alu_result) else alu_result
      if (mem_write) memory(alu_result) = reg_two
      if (reg_write) registers(which_write_reg) = write_data
      val p = readInt
      //println("")
    }

  }
}

// Tested
class ExtractBits {

  // Extracts the kth through n bits.
  // For example GetKn(56323,15,0) should get the 15 least significant digits
  def GetKn(value : Int,end : Int, begin : Int) : Int = {
    //val mask = (1 << (end - begin)) - 1;
    //(value >> (begin)) & mask;
    var t : Int = 0;
    if (begin == 0)
       t = (value << (32 - end)) >>> (begin + 32 - end)
    else t = (value << (32 - end)) >>> (begin + 32 - end - 1)
    return (t << 16) >> 16;
  }
  // Output:
  //  Output[0] = 31 - 26
  //  Output[1] = 25 - 21
  //  Output[2] = 20 - 16
  //  Output[3] = 15 - 11
  //  Output[4] = 15 - 0
  //  Output[5] = 5 - 0
  def Extract(instruction : Int) : (Int,Int,Int,Int,Int,Int) = {
    val out0 = GetKn(instruction,31,26)
    val out1 = GetKn(instruction,25,21)
    val out2 = GetKn(instruction,20,16)
    val out3 = GetKn(instruction,15,11)
    val out4 = GetKn(instruction,15, 0)
    val out5 = GetKn(instruction,5,0)
    (out0,out1,out2,out3,out4,out5)
  }

  def Test():Unit = {
    // fill in with test code later, get from textbook
    //101011 01010 01001 0000000000 01000
    //val (a,b,c,d,e,f) = Extract(1453621256)
    val instruction = 1453621256
    print("It should be: 101011\n Result:")
    println(GetKn(instruction,31,26).toBinaryString)
    print("It should be: 01010\n Result:")
    println(GetKn(instruction,25,21).toBinaryString)
    print("It should be: 01001\n Result:")
    println(GetKn(instruction,20,16).toBinaryString)
    print("It should be: 0\n Result:")
    println(GetKn(instruction,15,11).toBinaryString)
    print("It should be: 01000\n Result:")
    println(GetKn(instruction,15,0).toBinaryString)
    print("It should be: 01000\n Result:")
    println(GetKn(instruction,5,0).toBinaryString)
  }
}
// Tested execute, not control (control however is just a large case match)
class Alu {
  def Execute(data_one : Int, data_two : Int, ftype : Int) : (Int,Boolean) = {
    // take from table at the beginning of 4.4
    val res = ftype match {
      case 0 => data_one & data_two
      case 1 => data_one | data_two
      case 2 => {
        //println("add")
        data_one + data_two
      }
      case 6 => {
        println("Subtraction")
        data_one - data_two
      }
      // could be wrong direction
      case 7 => if (data_one < data_two) 0 else 1
      // this in theory shouldn't be used.
      case _ =>  ~(data_one | data_two)
    }

    (res,res == 0)
  }
  // finished
  def ALUControl(alu_op : Int, funct : Int) : Int = {
    // implements truth table figure 4.13
    val sfunct = funct & ((1 << 4) - 1)
    (alu_op,sfunct) match {
      // row 1
      case (0,_) => 2
      // row 2
      case (1,_) => 6
      // row 3
      case (2, 0) => 2
      // row 4
      case (2, 2) => 6
      // row 5
      case (2, 4) => 0
      // row 6
      case (2, 5) => 1
      // row 7: Should never execute
      case (2,10) => 7

    }
  }

  def AluTest() : Unit = {
    println("  Beginning AluTest")
    if (Execute(5,5,2) != (10,false)) {
      println("    Failure on test One")
      return;
    }
    if (Execute(5,5,6) != (0,true)) {
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
    // lw = 1
    // sw = 2
    // beq = 3
    val itype = instruction match {
      case 35 => {
        print("lw ")
        1
      }
      case 43 => {
        print("sw ")
        2
      }
      case 4 => {
        print("beq ")
        3
      }
      // everything else is rformat
      case _ => {
        //print("rformat ")
        0
      }
    }
    // derived from figure 4.18
    /*
      regDst,
      alu_src,
      memto_reg,
      reg_write,
      mem_read,
      mem_write,
      branch,
      alu_op)
    */
    itype match {
      //  (        1,     0,   0,    1,   0,    0,    0,  1,0)
      // rformart
      case 0 => (true, false,false,true,false,false,false,2)
      //  (        0,    1,    1,    1,   1,   0,     0  ,0,0)
      // lw
      case 1 => (false,true, true, true,true, false,false,0)
      //  (        x,    1,    x,    0,    0,    1,   0,  0,0)
      // sw
      case 2 => (false,true, false, false,false,true,false,0)
      //  (         x,    0,   x,    0,    0,    0,   1,  0,1)
      // beq
      case 3 => (false,false,false,false,false,false,true,1)
    }
  }
}
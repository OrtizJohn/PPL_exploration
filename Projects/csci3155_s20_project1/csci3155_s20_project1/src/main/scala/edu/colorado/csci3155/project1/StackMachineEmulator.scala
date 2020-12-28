package edu.colorado.csci3155.project1

import scala.::


/* -- Here are all the instructions to be supported --*/
sealed trait StackMachineInstruction
case class LoadI(s: String) extends StackMachineInstruction                         // stack -1, map(newV)
case class  StoreI(s: String) extends StackMachineInstruction                       // stack +1, env
case class PushI(f: Double) extends StackMachineInstruction                         // stack +1, env
case object AddI extends StackMachineInstruction                                    // stack -2 +1, env
case object SubI extends StackMachineInstruction                                    // stack -2 +1, env
case object MultI extends StackMachineInstruction                                    // stack -2 +1, env
case object DivI extends StackMachineInstruction                                    // stack -2 +1, env
case object ExpI extends StackMachineInstruction                                    // stack -1 +1, env
case object LogI extends StackMachineInstruction                                    // stack -1 +1, env
case object SinI extends StackMachineInstruction                                    // stack -1 +1, env
case object CosI extends StackMachineInstruction                                    // stack -1 +1, env
case object PopI extends StackMachineInstruction                                    // stack -1 , env


object StackMachineEmulator {

    def helperFor2par(stack: List[Double], op: (Double,Double)=>Double): List[Double]={
        assert(stack.length >=2 )   //make sure length of stack is >=2)
        val res = op(stack(0),stack(1))
        //create new list[double]
        val newL = List(res)
        val chopL = stack.drop(2)

        //first element should be result, rest should be original stack from the third element
        List(res) ++ chopL
    }

    /* Function emulateSingleInstruction
        Given a list of doubles to represent a stack
              a map from string to double precision numbers for the environment
        and   a single instruction of type StackMachineInstruction
        Return a tuple that contains the
              modified stack that results when the instruction is executed.
              modified environment that results when the instruction is executed.

        Make sure you handle the error cases: eg., stack size must be appropriate for the instruction
        being executed. Division by zero, log of a non negative number
        Throw an exception or assertion violation when error happens.
     */
    def emulateSingleInstruction(stack: List[Double],
                                 env: Map[String, Double],
                                 ins: StackMachineInstruction): (List[Double], Map[String, Double]) = {
        // TODO: StoreI, PushI, AddI, SubI, MultI, DivI,ExpI,LogI,SinI,CosI, PopI





        ins match {
            case LoadI( s:String) => {

                val v = stack(0)
                val envNew = env + (s -> v)
                (stack.drop(1), envNew)
//                if (env.isEmpty) {
//                    throw new IllegalArgumentException("Empty env (LoadI)")
//                }
//                else {
//                    val v = stack(0)
//                    val envNew = env + (s -> v)
//                    (stack.drop(1), envNew)
//                }
            }
            case StoreI( s:String) => {                                                                                                 //StoreI
                if(env.isEmpty) {throw new IllegalArgumentException("Empty env (storeI)") } //empty env
                else if(!env.contains(s)){                                                  //identifier not present
                    throw new IllegalArgumentException("Does not contain s. (storeI)")
                }
                else{                                                                       //add v to stack
                    val v = env(s)
                    (List(v) ++ stack , env) //unchanged env
                }
            }
            case PushI(d:Double) =>  ((List(d) ++ stack, env))                                        //add d to stack
            case PopI =>  {                                                                                                             //PopI
                if(env.isEmpty){
                    throw new IllegalArgumentException("Empty env (PopI)")
                }else{
                    (stack.drop(1), env)
                }
            }
            case SubI => (helperFor2par(stack, (v1,v2)=>v2-v1), env)
            case AddI => (helperFor2par(stack, (v1,v2)=>v2+v1), env)
            case MultI => (helperFor2par(stack, (v1,v2)=>v2*v1), env)
            case DivI => (helperFor2par(stack, (v1,v2)=>v1 / v2), env)
            case LogI => {                                                                                                             //LogI
                val v1 = stack(0)
                val newStack = stack.drop(1)//pop one number from stack
                val v1_log = math.log(v1)
                if(v1_log >= 0){   //log has to be positive
                    (List(v1_log) ++ newStack, env)
                }else{
                    throw new IllegalArgumentException("Log is negative")
                }
            }
            case ExpI => {                                                                                                             //ExpI
                if(stack.length < 0){
                    throw new IllegalArgumentException("Stack is Empty")
                }
                val v1 = stack(0)
                val newStack = stack.drop(1)//pop one number from stack
                val v1_exp = math.exp(v1)
                (List(v1_exp) ++ newStack, env)

            }
            case SinI => {                                                                                                             //SinI
                if(stack.length < 0){
                    throw new IllegalArgumentException("Stack is Empty")
                }
                val v1 = stack(0)
                val newStack = stack.drop(1)//pop one number from stack
                val v1_sin = math.sin(v1)
                (List(v1_sin) ++ newStack, env)

            }
            case CosI => {                                                                                                             //CosI
                if(stack.length < 0){
                    throw new IllegalArgumentException("Stack is Empty")
                }
                val v1 = stack(0)
                val newStack = stack.drop(1)//pop one number from stack
                val v1_cos = math.cos(v1)
                (List(v1_cos) ++ newStack, env)

            }

        }





    }

    /* Function emulateStackMachine
       Execute the list of instructions provided as inputs using the
       emulateSingleInstruction function.
       Use foldLeft over list of instruction rather than a for loop if you can.
       Return value must be the final environment.

       Hint: accumulator for foldLeft must be a tuple (List[Double], Map[String,Double])
             initial value of this accumulator must be (Nil, Map.empty)
             You should use emulateSingleInstruction to update the accmulator.
             It will all fit nicely once you figure it out.
     */
    def emulateStackMachine(instructionList: List[StackMachineInstruction]): Map[String, Double] =
        {
            //TODO: Your Code here.
            //foldleft here on the list
            // 1.Type of accumulater? (List[Double], Map[String,Double])
            // 2.Initial value of acc: Map.empty[Int, String]
            // 3.What does the function for fold need to do? List[StackMachineInstruction] =>
            val (_ , result) = instructionList.foldLeft[(List[Double], Map[String,Double])] ((Nil, Map.empty))  {(acc,inst) => emulateSingleInstruction(acc._1,acc._2,inst)  }
            result

        }
}
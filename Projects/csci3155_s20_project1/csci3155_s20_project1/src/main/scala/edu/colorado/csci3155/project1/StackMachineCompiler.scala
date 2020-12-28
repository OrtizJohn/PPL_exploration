package edu.colorado.csci3155.project1

object StackMachineCompiler {


    /* Function compileToStackMachineCode
        Given expression e as input, return a corresponding list of stack machine instructions.
        The type of stack machine instructions are in the file StackMachineEmulator.scala in this same directory
        The type of Expr is in the file Expr.scala in this directory.
     */

    def helperfor2par( e1:Expr, e2:Expr, i:StackMachineInstruction): List[StackMachineInstruction] ={
        val l1 = compileToStackMachineCode(e1)
        val l2 = compileToStackMachineCode(e2)
        l1 ++ l2 ++ List(i)

    }
    def compileToStackMachineCode(e: Expr): List[StackMachineInstruction] = {
        //TODO: Your code here
        e match {
            case Const(f: Double) => List(PushI(f))
            case Ident(id: String) => List(StoreI(id))
            case Plus(e1: Expr, e2: Expr) => helperfor2par(e1, e2, AddI)
            case Minus(e1: Expr, e2: Expr) => helperfor2par(e1, e2, SubI)
            case Mult(e1: Expr, e2: Expr) => helperfor2par(e1, e2, MultI)
            case Div(e1: Expr, e2: Expr) => helperfor2par(e1, e2, DivI)
            case Exp(e: Expr) => {
                val l1 = compileToStackMachineCode(e)
                val l2 = List(ExpI)
                l1 ++ l2
            }
            case Log(e: Expr) => {
                val l1 = compileToStackMachineCode(e)
                val l2 = List(LogI)
                l1 ++ l2
            }

            case Sine(e: Expr) => {
                val l1 = compileToStackMachineCode(e)
                val l2 = List(SinI)
                l1 ++ l2
            }
            case Cosine(e: Expr) => {
                val l1 = compileToStackMachineCode(e)
                val l2 = List(CosI)
                l1 ++ l2
            }
            case Let(ident: String, e1: Expr, e2: Expr) => {
                val l1 = compileToStackMachineCode(e1)
                val l2 = compileToStackMachineCode(e2)
                val l3 = List(LoadI(ident))
                l1 ++ l3 ++ l2

            }

        }
    }
}

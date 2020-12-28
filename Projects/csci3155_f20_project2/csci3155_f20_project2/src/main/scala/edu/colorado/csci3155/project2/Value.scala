package edu.colorado.csci3155.project2

import scala.util.Failure

/* Define the values used in our interpreter */
sealed trait Value
case class NumValue(f: Double) extends Value
case class FigValue(c: MyCanvas) extends Value
case class Closure(x: String, e: Expr, env: Environment) extends Value
case class BoolValue(b: Boolean) extends Value

/* Create some utility functions to operate on values.  */
object ValueOps {

    def minus(v1: Value, v2: Value): Value = (v1, v2) match {
        case (NumValue(f1), NumValue(f2)) => NumValue(f1 - f2)
        case _ => throw new IllegalArgumentException("Cannot subtract figures, numbers and closures")
    }



    def geq(v1: Value, v2: Value): Value = (v1, v2) match {
        case (NumValue(f1), NumValue(f2)) => BoolValue(f1 >= f2)
        case _ => throw new IllegalArgumentException("Cannot compare non numeric expressions with the geq comparator")
    }

    def gt(v1: Value, v2: Value): Value = (v1, v2) match {
        case (NumValue(f1), NumValue(f2)) => BoolValue(f1 > f2)
        case _ => throw new IllegalArgumentException("Cannot compare non numeric expressions with the eq comparator")
    }

    def equal(v1: Value, v2: Value): Value = (v1, v2) match {
        case (NumValue(f1), NumValue(f2)) => BoolValue(f1 == f2)
        case (BoolValue(b1), BoolValue(b2)) => BoolValue(b1 == b2)
        case _ =>  throw new IllegalArgumentException("Cannot compare non numeric/boolean expressions with the eq comparator")
    }

    def notEqual(v1: Value, v2: Value): Value = {
        val v = equal(v1, v2)
        v match {
            case BoolValue(b) => BoolValue(!b)
            case _ => throw new IllegalArgumentException("Internal error: something is really wrong") // This should never happen
        }
    }
    def division(v1: Value, v2: Value): Value = (v1,v2) match{
        case (NumValue(f1), NumValue(f2)) => NumValue(f1 / f2)
        case (FigValue(c1), FigValue(c2) )=> FigValue(c1.placeTop(c2))
        case (FigValue(c1), NumValue(f1) )=> {
            //val vertex= new Polygon(List((0,0)))
            FigValue(c1.rotate(f1))
        }
        case _ => throw new IllegalArgumentException("Value v1 and v2 dont match any case -- Div") // This should never happen
    }
    def multiplication(v1: Value, v2: Value): Value = (v1,v2) match{
        case (NumValue(f1), NumValue(f2)) => NumValue(f1 * f2)
        case (FigValue(c1), FigValue(c2) )=> FigValue(c1.placeRight(c2))
        case _ => throw new IllegalArgumentException("Value v1 and v2 dont match any case -- Mult") // This should never happen
    }
    def plus(v1: Value, v2: Value): Value = (v1,v2) match{
        case (NumValue(f1), NumValue(f2)) => NumValue(f1 * f2)
        case (FigValue(c1), FigValue(c2) )=> FigValue(c1.overlap(c2))
        case _ => throw new IllegalArgumentException("Value v1 and v2 dont match any case -- plus") // This should never happen
    }


}
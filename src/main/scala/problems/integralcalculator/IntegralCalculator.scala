package problems.integralcalculator

import problems.integralcalculator.Expressions._
import problems.integralcalculator.Expressions.Implicits._
import Operators._

import scala.language.implicitConversions


object IntegralCalculator extends App {

  val expr1: Expression = Expression(List(__, __, mul))
  println(expr1, expr1.definiteIntegral(0, 1))

  val expr2 = Expression(List(__, __, mul, __,  plus, sin))
  println(expr2, expr2.definiteIntegral(0, 1))

}

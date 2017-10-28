package problems.integralcalculator

import Operators._

import scala.language.implicitConversions


object Expressions {

  type Element = Either[Expression, Operator]
  type RPNExpr = List[Element]

  object Implicits {
    implicit def numberIsExpression[A: Numeric](n: A): Expression = Number(implicitly[Numeric[A]].toDouble(n))
    implicit def expressionIsElement[A](expr: A)(implicit ev: A => Expression): Element = Left(expr)
    implicit def operatorIsElement(op: Operator): Element = Right(op)
  }


  trait Expression {
    def evaluate(unknown: Double): Double

    def definiteIntegral(beginning: Double, end: Double, d: Double = 0.001): Double = {
      def loop(current: Double, acc: Double): Double = {
        if (current > end) acc * d
        else loop(current + d, acc + this.evaluate(current))
      }
      loop(beginning, 0)
    }
  }

  object Expression {

    def reduce(rpn: RPNExpr, stack: List[Expression]): Expression = {
      (rpn, stack) match {
        case (Left(operand) :: _, _) =>
          reduce(rpn.tail, operand :: stack)
        case (Right(op: UnaryOperator) :: _, x :: rest) =>
          reduce(rpn.tail, UnaryExpression(op, x) :: rest)
        case (Right(op: BinaryOperator) :: _, x :: y :: rest) =>
          reduce(rpn.tail, BinaryExpression(op, x, y) :: rest)
        case (Nil, Nil) => Number(0)
        case (Nil, x :: Nil) => x
        case _ => throw new IllegalArgumentException("Invalid expression")
      }
    }

    def apply(rpn: RPNExpr): Expression = reduce(rpn, List())

  }

  case class Number(n: Double) extends Expression {
    override def evaluate(unknown: Double): Double = n
    override def toString: String = n.toString
  }

  case object __ extends Expression {
    override def evaluate(unknown: Double): Double = unknown
    override def toString: String = "x"
  }

  case class UnaryExpression(op: UnaryOperator, right: Expression) extends Expression {
    override def evaluate(unknown: Double): Double = op(right.evaluate(unknown))
    override def toString: String = s"$op($right)"
  }

  case class BinaryExpression(op: BinaryOperator, left: Expression, right: Expression) extends Expression {
    override def evaluate(unknown: Double): Double = op(left.evaluate(unknown), right.evaluate(unknown))
    override def toString: String = s"($left $op $right)"
  }


}

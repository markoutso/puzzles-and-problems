package problems.integralcalculator


object Operators {

  trait Operator

  trait UnaryOperator extends Operator with (Double => Double)

  trait BinaryOperator extends Operator with ((Double, Double) => Double)

  case object plus extends BinaryOperator {
    override def apply(v1: Double, v2: Double): Double = v1 + v2
    override def toString: String = "+"
  }

  case object minus extends BinaryOperator {
    override def apply(v1: Double, v2: Double): Double = v1 - v2
    override def toString: String = "-"
  }

  case object mul extends BinaryOperator {
    override def apply(v1: Double, v2: Double): Double = v1 * v2
    override def toString: String = "*"
  }

  case object div extends BinaryOperator {
    override def apply(v1: Double, v2: Double): Double = v1 / v2
    override def toString: String = "/"
  }

  case object cos extends UnaryOperator {
    override def toString: String = "cos"
    override def apply(v1: Double): Double = Math.cos(v1)
  }

  case object sin extends UnaryOperator {
    override def toString: String = "sin"
    override def apply(v1: Double): Double = Math.sin(v1)
  }

  case object ln extends UnaryOperator {
    override def toString: String = "ln"
    override def apply(v1: Double): Double = Math.log(v1)
  }

  case object abs extends UnaryOperator {
    override def toString: String = "abs"
    override def apply(v1: Double): Double = Math.abs(v1)
  }

  case object negate extends UnaryOperator {
    override def toString: String = "-"
    override def apply(v1: Double): Double = -v1
  }


}

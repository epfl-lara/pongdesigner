package ch.epfl.lara.synthesis.kingpong.rules

import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.common.Implicits._
import ch.epfl.lara.synthesis.kingpong.common.History
import ch.epfl.lara.synthesis.kingpong.common.RingBuffer
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.expression.Interpreter
import ch.epfl.lara.synthesis.kingpong.expression.TypeChecker
import ch.epfl.lara.synthesis.kingpong.expression.Trees._
import ch.epfl.lara.synthesis.kingpong.expression.Trees

object Rules {
  /*sealed trait Rule {
    def cond: Expr
    def action: Stat
    def setBinding(n: String, o: GameObject): this.type
  }
  sealed trait NoBinding extends Rule {
    def setBinding(n: String, o: GameObject) = { this }
  }*/
  
  object Whenever {
    def apply(cond: Expr, action: Stat) = Trees.If(cond, action, NOP)
  }

  /*case class Whenever(cond: Expr, action: Stat) extends Rule {
    def setBinding(n: String, o: GameObject) = {
      cond.setBinding(n, o)
      action.setBinding(n, o)
      this
    }
  }*/
  /*trait CondAction {
    def cond: Expr
    def action: Expr
    def setBinding(n: String, o: GameObject): this.type = {cond.setBinding(n, o); action.setBinding(n, o); this }
  }*/

  

  /*case class NoCategory(rule: Stat) extends RuleIterator {
    protected type Keys = Seq[(String, GameObject)]
    protected def keys = List(List[(String, GameObject)](("", null: GameObject)))
    protected def generator = new UniqueGenerator(rule)
  }*/

  

  /*case class Foreach2(category1: Category,
                 category2: Category,
                 binding1: String,
                 binding2: String,
                 protected val rule: Stat) extends RuleIterator {
    protected type Keys = Seq[(String, GameObject)] // Initially a Tuple2 ((String, GameObject), (String, GameObject))
    protected def keys = for {
      o1 <- category1.objects
      o2 <- category2.objects
    } yield List((binding1, o1), (binding2, o2))
    def generator: BindingGenerator = rule
  }*/

}

package ch.epfl.lara.synthesis.kingpong.rules

import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.common.Implicits._
import ch.epfl.lara.synthesis.kingpong.common.History
import ch.epfl.lara.synthesis.kingpong.common.RingBuffer
import ch.epfl.lara.synthesis.kingpong.objects._
import ch.epfl.lara.synthesis.kingpong.expression.Interpreter
import ch.epfl.lara.synthesis.kingpong.expression.TypeChecker
import ch.epfl.lara.synthesis.kingpong.expression.Trees._

object Rules {


  sealed trait Rule {
    def cond: Expr
    def action: Stat
    def setBinding(n: String, o: GameObject): this.type
  }
  trait NoBinding extends Rule {
    def setBinding(n: String, o: GameObject) = { this }
  }

  case class Whenever(cond: Expr, action: Stat) extends Rule {
    def setBinding(n: String, o: GameObject) = {
      cond.setBinding(n, o)
      action.setBinding(n, o)
      this
    }
  }
  trait CondAction {
    def cond: Expr
    def action: Expr
    def setBinding(n: String, o: GameObject): this.type = {cond.setBinding(n, o); action.setBinding(n, o); this }
  }

  sealed trait RuleIterator extends History {

    private var state: Map[Keys, Boolean] = Map.empty.withDefaultValue(false)
    
    /** Contains the history. The head corresponds to the most recent value. */
    private val history: RingBuffer[(Long, Map[Keys, Boolean])] = new RingBuffer(History.MAX_HISTORY_SIZE)

    implicit class BindingGenerator(r: Rule) extends (Keys => Rule) {
      def apply(keys: Keys): Rule = {
        keys foreach { case (name, obj) =>
          r.setBinding(name, obj)
        }
        r
      }
    }
    protected type Keys <: Iterable[(String, GameObject)]
    protected def generator: BindingGenerator
    protected def keys: Iterable[Keys]

    def typeCheck(typechecker: TypeChecker): Unit = {
      (keys map generator) foreach typechecker.typeCheck
    }

    /** Evaluate the rules according to the previous evaluations flags. */
    def evaluate(interpreter: Interpreter)(implicit context: Context): Unit = {
      keys foreach { key =>
        generator(key) match {
          case Whenever(cond, action) =>
            if (interpreter.eval(cond).as[Boolean]) {
              interpreter.eval(action)
            }
/*
          case On(cond, action) =>
            val b = interpreter.eval(cond).as[Boolean]
            if (!state(key) && b) {
              interpreter.eval(action)
              state += key -> true
            } else if (!b) {
              state += key -> false
            }

          case Once(cond, action) =>
            if (!state(key) && interpreter.eval(cond).as[Boolean]) {
              interpreter.eval(action)
              state += key -> true
            }
          
            */
        }
      }
    }

    /** Reset the rules evaluation flags. */
    def reset(): Unit = state = Map.empty.withDefaultValue(false)

    def save(t: Long): Unit = {
      if (history.isEmpty || history.last._2 != state) {
        history += (t, state)
      }
    }

    def restore(t: Long): Unit = {
      history.findLast(_._1 <= t) match {
        case Some((_, s)) => state = s
        case None => sys.error(s"The timestamp $t doesn't exist in the history.")
      }
    }

    def clear(): Unit = history.clear()

  }

  class NoCategory(rule: Rule) extends RuleIterator {
    protected type Keys = Seq[(String, GameObject)]
    protected def keys = Seq()
    protected def generator = rule
  }

  class Foreach1(category: Category, nameBinding: String, protected val rule: Rule) extends RuleIterator {
    protected type Keys = Seq[(String, GameObject)]
    protected def keys = category.objects.map(o => List((nameBinding, o)))
    def generator: BindingGenerator = rule
  }

  class Foreach2(category1: Category,
                 category2: Category,
                 binding1: String,
                 binding2: String,
                 protected val rule: Rule) extends RuleIterator {
    protected type Keys = Seq[(String, GameObject)] // Initially a Tuple2 ((String, GameObject), (String, GameObject))
    protected def keys = for {
      o1 <- category1.objects
      o2 <- category2.objects
    } yield List((binding1, o1), (binding2, o2))
    def generator: BindingGenerator = rule
  }

}

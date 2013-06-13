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
  }

  case class Whenever(cond: Expr, action: Stat) extends Rule
  case class On(cond: Expr, action: Stat) extends Rule
  case class Once(cond: Expr, action: Stat) extends Rule

  sealed trait RuleIterator extends History {

    private var state: Map[Key, Boolean] = Map.empty.withDefaultValue(false)
    
    /** Contains the history. The head corresponds to the most recent value. */
    private val history: RingBuffer[(Long, Map[Key, Boolean])] = new RingBuffer(History.MAX_HISTORY_SIZE)


    protected type Key
    protected def generator: Key => Rule
    protected def keys: Iterable[Key]

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
    protected type Key = Unit
    protected def keys = Seq(Unit)
    protected def generator = Unit => rule
  }

  class Foreach1(category: Category, protected val generator: GameObject => Rule) extends RuleIterator {

    protected type Key = GameObject
    protected def keys = category.objects

  }

  class Foreach2(category1: Category, 
                 category2: Category, 
                 protected val generator: ((GameObject, GameObject)) => Rule) extends RuleIterator {

    protected type Key = (GameObject, GameObject)
    protected def keys = for {
      o1 <- category1.objects
      o2 <- category2.objects
    } yield (o1, o2)
  }

}

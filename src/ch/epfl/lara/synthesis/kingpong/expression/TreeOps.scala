package ch.epfl.lara.synthesis.kingpong.expression

import ch.epfl.lara.synthesis.kingpong.objects._

object TreeOps {  
  import Trees._
  import Types._
  import Extractors._
  
  /**
   * Do a right tree fold on statement
   *
   * f takes the current node, as well as the seq of results form the subtrees.
   *
   * Usages of views makes the computation lazy. (which is useful for
   * contains-like operations)
   */
  def foldRight[T](f: (Stat, Seq[T]) => T)(s: Stat): T = {
    val rec = foldRight(f) _

    s match {
      case ParExpr(stats) =>
        f(s, stats.view.map(rec))
      case Block(stats) =>
        f(s, stats.view.map(rec))
      case If(cond, ifTrue, None) =>
        f(s, List(ifTrue).view.map(rec))
      case If(cond, ifTrue, Some(ifFalse)) =>
        f(s, List(ifTrue, ifFalse).view.map(rec))
      case Foreach(_, _, body) =>
        f(s, List(body).view.map(rec))
      case Assign(_, _) =>
        f(s, Stream.empty)
      case Copy(_, _, stat) =>
        f(s, List(stat).view.map(rec))
      case Delete(_) =>
        f(s, Stream.empty)
      case NOP =>
        f(s, Stream.empty)
    }
  }
  
  //TODO add other operations to Stat tree, like traverse and map.
  
  
  
  /**
   * Do a right tree fold
   *
   * f takes the current node, as well as the seq of results form the subtrees.
   *
   * Usages of views makes the computation lazy. (which is useful for
   * contains-like operations)
   */
  def foldRight[T](f: (Expr, Seq[T]) => T)(e: Expr): T = {
    val rec = foldRight(f) _

    e match {
      case t: Terminal =>
        f(t, Stream.empty)

      case u @ UnaryOperator(e, _) =>
        f(u, List(e).view.map(rec))

      case b @ BinaryOperator(e1, e2, _) =>
        f(b, List(e1, e2).view.map(rec))

      case n @ NAryOperator(es, _) =>
        f(n, es.view.map(rec))
    }
  }

  /**
   * pre-traversal of the tree, calls f() on every node *before* visiting
   * children.
   *
   * e.g.
   *
   *   Add(a, Minus(b, c))
   *
   * will yield, in order:
   *
   *   f(Add(a, Minus(b, c))), f(a), f(Minus(b, c)), f(b), f(c)
   */
  def preTraversal(f: Expr => Unit)(e: Expr): Unit = {
    val rec = preTraversal(f) _
    f(e)

    e match {
      case t: Terminal =>

      case u @ UnaryOperator(e, builder) =>
        List(e).foreach(rec)

      case b @ BinaryOperator(e1, e2, builder) =>
        List(e1, e2).foreach(rec)

      case n @ NAryOperator(es, builder) =>
        es.foreach(rec)
    }
  }

  /**
   * post-traversal of the tree, calls f() on every node *after* visiting
   * children.
   *
   * e.g.
   *
   *   Add(a, Minus(b, c))
   *
   * will yield, in order:
   *
   *   f(a), f(b), f(c), f(Minus(b, c)), f(Add(a, Minus(b, c)))
   */
  def postTraversal(f: Expr => Unit)(e: Expr): Unit = {
    val rec = postTraversal(f) _

    e match {
      case t: Terminal =>

      case u @ UnaryOperator(e, builder) =>
        List(e).foreach(rec)

      case b @ BinaryOperator(e1, e2, builder) =>
        List(e1, e2).foreach(rec)

      case n @ NAryOperator(es, builder) =>
        es.foreach(rec)
    }

    f(e)
  }

  /**
   * pre-transformation of the tree, takes a partial function of replacements.
   * Substitutes *before* recursing down the trees.
   *
   * e.g.
   *
   *   Add(a, Minus(b, c)) with replacements: Minus(b,c) -> d, b -> e, d -> f
   *
   * will yield:
   *
   *   Add(a, d)   // And not Add(a, f) because it only substitute once for each level.
   */
  def preMap(f: Expr => Option[Expr])(e: Expr): Expr = {
    val rec = preMap(f) _

    val newV = f(e).getOrElse(e)

    newV match {
      case u @ UnaryOperator(e, builder) =>
        val newE = rec(e)

        if (newE ne e) {
          builder(newE).copiedFrom(u)
        } else {
          u
        }

      case b @ BinaryOperator(e1, e2, builder) =>
        val newE1 = rec(e1)
        val newE2 = rec(e2)

        if ((newE1 ne e1) || (newE2 ne e2)) {
          builder(newE1, newE2).copiedFrom(b)
        } else {
          b
        }

      case n @ NAryOperator(es, builder) =>
        val newEs = es.map(rec)

        if ((newEs zip es).exists { case (bef, aft) => aft ne bef }) {
          builder(newEs).copiedFrom(n)
        } else {
          n
        }

      case t: Terminal =>
        t
    }
  }

  /**
   * post-transformation of the tree, takes a partial function of replacements.
   * Substitutes *after* recursing down the trees.
   *
   * e.g.
   *
   *   Add(a, Minus(b, c)) with replacements: Minus(b,c) -> d, b -> e, d -> f
   *
   * will yield:
   *
   *   Add(a, Minus(e, c))
   */
  def postMap(f: Expr => Option[Expr])(e: Expr): Expr = {
    val rec = postMap(f) _

    val newV = e match {
      case u @ UnaryOperator(e, builder) =>
        val newE = rec(e)

        if (newE ne e) {
          builder(newE).copiedFrom(u)
        } else {
          u
        }

      case b @ BinaryOperator(e1, e2, builder) =>
        val newE1 = rec(e1)
        val newE2 = rec(e2)

        if ((newE1 ne e1) || (newE2 ne e2)) {
          builder(newE1, newE2).copiedFrom(b)
        } else {
          b
        }

      case n @ NAryOperator(es, builder) =>
        val newEs = es.map(rec)

        if ((newEs zip es).exists { case (bef, aft) => aft ne bef }) {
          builder(newEs).copiedFrom(n)
        } else {
          n
        }

      case t: Terminal =>
        t
    }

    f(newV).getOrElse(newV)
  }

  /**
   * Apply 'f' on 'e' as long as until it stays the same (value equality)
   */
  def fixpoint[T](f: T => T)(e: T): T = {
    var v1 = e
    var v2 = f(v1)
    while(v2 != v1) {
      v1 = v2
      v2 = f(v2)
    }
    v2
  }
  
  def exists(matcher: Expr => Boolean)(e: Expr): Boolean = {
    foldRight[Boolean]({ (e: Expr, subs: Seq[Boolean]) =>  matcher(e) || subs.contains(true) } )(e)
  }

  def collect[T](matcher: Expr => Set[T])(e: Expr): Set[T] = {
    foldRight[Set[T]]({ (e: Expr, subs: Seq[Set[T]]) => matcher(e) ++ subs.foldLeft(Set[T]())(_ ++ _) } )(e)
  }

  def filter(matcher: Expr => Boolean)(e: Expr): Set[Expr] = {
    collect[Expr] { e => if (matcher(e)) Set(e) else Set() }(e)
  }

  def count(matcher: Expr => Int)(e: Expr): Int = {
    foldRight[Int]({ (e: Expr, subs: Seq[Int]) =>  matcher(e) + subs.foldLeft(0)(_ + _) } )(e)
  }

  def replace(substs: Map[Expr,Expr], expr: Expr) : Expr = {
    postMap(substs.lift)(expr)
  }

  
}
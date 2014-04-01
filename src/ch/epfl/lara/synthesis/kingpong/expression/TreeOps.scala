package ch.epfl.lara.synthesis.kingpong.expression

import ch.epfl.lara.synthesis.kingpong.objects._

object TreeOps {  
  import Trees._
  import Types._
  import Extractors._
   
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
    foldRight[Boolean]({ (e, subs) =>  matcher(e) || subs.contains(true) } )(e)
  }

  def collect[T](matcher: Expr => Set[T])(e: Expr): Set[T] = {
    foldRight[Set[T]]({ (e, subs) => matcher(e) ++ subs.foldLeft(Set[T]())(_ ++ _) } )(e)
  }

  def filter(matcher: Expr => Boolean)(e: Expr): Set[Expr] = {
    collect[Expr] { e => if (matcher(e)) Set(e) else Set() }(e)
  }

  def count(matcher: Expr => Int)(e: Expr): Int = {
    foldRight[Int]({ (e, subs) =>  matcher(e) + subs.foldLeft(0)(_ + _) } )(e)
  }

  def replace(substs: Map[Expr,Expr], expr: Expr) : Expr = {
    postMap(substs.lift)(expr)
  }
  
  /**
   * Flatten an expression.
   * All unnecessary blocks are removed nested ones flatten. 
   * All nested ParExpr are flatten.
   */
  def flatten(expr: Expr): Expr = {
    val f = (e: Expr) => e match {
      case Block(es) => 
        def flattenBlock(expr: Expr): Seq[Expr] = expr match {
          case Block(es2) => es2
          case _ => Seq(expr)
        }
        flattenNOP(es.flatMap(flattenBlock)) match {
          case Seq()  => Some(NOP)
          case Seq(e) => Some(e)
          case exprs  => Some(Block(exprs))
        }
        
      case ParExpr(es) => 
        def flattenParallel(expr: Expr): List[Expr] = expr match {
          case ParExpr(es2) => es2
          case _ => List(expr)
        }
        flattenNOP(es.flatMap(flattenParallel).toSeq) match {
          case Seq()  => Some(NOP)
          case Seq(e) => Some(e)
          case exprs  => Some(ParExpr(exprs.toList))
        }
        
      case _ => None
    }
    postMap(f)(expr)
  }
  
  /**
   * Remove NOP from a sequence of expressions.
   */
  def flattenNOP(exprs: Seq[Expr]): Seq[Expr] = {
    exprs.filterNot(_ == NOP)
  }
  
  def collectObjects(e: Expr): Set[GameObject] = {
    collect[GameObject] { _ match {
      case ObjectLiteral(o) => Set(o)
      case _ => Set()
    }}(e)
  }
  
  def generalizeToCategory(e: Expr, obj: GameObject): Expr = {
    val cat = obj.category
    val id = FreshIdentifier(cat.name)
    val body = replace(Map(ObjectLiteral(obj) -> Variable(id)), e)
    Foreach(cat, id, body)
  }
  
}
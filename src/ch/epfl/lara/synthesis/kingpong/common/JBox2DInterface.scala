package ch.epfl.lara.synthesis.kingpong.common

// Remove implicit warnings
import language.implicitConversions

import ch.epfl.lara.synthesis.kingpong.objects._

import org.jbox2d.collision.WorldManifold
import org.jbox2d.common.MathUtils

object JBox2DInterface {

  // --------------------------------------------------------------------------
  // Interface classes
  // --------------------------------------------------------------------------  
  
  type Vec2 = org.jbox2d.common.Vec2

  object Vec2 {
    def apply(other: Vec2) = new Vec2(other)
    def apply(x: Float, y: Float) = new Vec2(x, y)
    def unapply(v: Vec2): Option[(Float, Float)] = Some((v.x, v.y))
  }

  implicit class Vec2Extended(val v: Vec2) extends AnyVal {
    def +(other: Vec2) = v.add(other)
    def distance(to: Vec2) = MathUtils.distance(v, to)
    def distanceSquared(to: Vec2) = MathUtils.distanceSquared(v, to)
  }

  case class Transform(pos: Vec2, angle: Float)

  type Contact = org.jbox2d.dynamics.contacts.Contact

  object Contact {
    def unapply(contact: Contact): Option[(PhysicalObject, PhysicalObject)] = {
      Some(contact.objectA, contact.objectB)
    }
  }
  
  implicit class ContactExtended(val c: Contact) extends AnyVal {
  
    def objectA: PhysicalObject = {
      c.getFixtureA().getBody().getUserData().asInstanceOf[PhysicalObject]
    }

    def objectB: PhysicalObject = {
      c.getFixtureB().getBody().getUserData().asInstanceOf[PhysicalObject]
    }

    def point(implicit m: WorldManifold): Vec2 = {
      c.getWorldManifold(m)
      m.points(0).clone // Clone to be sure we have a fresh Vec2
    }

    def normal(implicit m: WorldManifold): Vec2 = {
      c.getWorldManifold(m)
      m.normal.clone // Clone to be sure we have a fresh Vec2
    }
    
  }
  
  type AABB = org.jbox2d.collision.AABB

  object AABB {
    // return the lower left bound and the upper right bound
    def unapply(aabb: AABB): Option[(Vec2, Vec2)] = {
      Some(aabb.aabb.lowerBound, aabb.aabb.upperBound)
    }
  }
  
  implicit class AABBExtended(val aabb: AABB) extends AnyVal {
    
    def center = aabb.getCenter()

    def contains(point: Vec2) = {
      point.x > aabb.lowerBound.x &&
      point.y > aabb.lowerBound.y &&
      point.x < aabb.upperBound.x &&
      point.y < aabb.upperBound.y
    }
    
    def overlaps(other: AABB) = {
      org.jbox2d.collision.AABB.testOverlap(aabb, other.aabb)
    }
    
  }

  // --------------------------------------------------------------------------
  // Implicits functions
  // --------------------------------------------------------------------------  
  
  implicit def TransformJBox2D(t: org.jbox2d.common.Transform) = Transform(t.p, t.q.getAngle())
  implicit def JBox2TransformD(t: Transform) = new org.jbox2d.common.Transform().set(t.pos, t.angle)
  
}
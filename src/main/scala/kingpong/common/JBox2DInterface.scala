package ch.epfl.lara.synthesis.kingpong.common

// Remove implicit warnings
import language.implicitConversions

import ch.epfl.lara.synthesis.kingpong.objects._

import org.jbox2d.collision.WorldManifold

object JBox2DInterface {

  // --------------------------------------------------------------------------
  // Interface classes
  // --------------------------------------------------------------------------  
  
  case class Vec2(x: Float, y: Float) extends WithPoint {
    protected def point = this
    
    def add(other: Vec2) = Vec2(x + other.x, y + other.y)
  }

  case class Transform(pos: Vec2, angle: Float) extends WithPoint {
    protected def point = pos
  }
  
  // --------------------------------------------------------------------------
  // Implicits functions
  // --------------------------------------------------------------------------  
  
  implicit def TransformJBox2D(t: org.jbox2d.common.Transform) = Transform(t.p, t.q.getAngle())
  implicit def JBox2TransformD(t: Transform) = new org.jbox2d.common.Transform().set(t.pos, t.angle)
  
  implicit def Vec2JBox2D(v: org.jbox2d.common.Vec2): Vec2 = Vec2(v.x, v.y)
  implicit def JBox2DVec2(v: Vec2): org.jbox2d.common.Vec2 = new org.jbox2d.common.Vec2(v.x, v.y)
  
  object Contact {
    def unapply(contact: Contact): Option[(PhysicalObject, PhysicalObject)] = {
      val o1 = contact.c.getFixtureA().getBody().getUserData().asInstanceOf[PhysicalObject] 
      val o2 = contact.c.getFixtureB().getBody().getUserData().asInstanceOf[PhysicalObject] 
      Some(o1, o2)
    }
  }
  
  implicit class Contact(val c: org.jbox2d.dynamics.contacts.Contact) extends AnyVal with WithPoint {
  
    def point: Vec2 =  {
      val m = new WorldManifold()
      c.getWorldManifold(m)
      m.points(0)
    }
    
  }
  
  object AABB {
    // return the lower left bound and the upper right bound
    def unapply(aabb: AABB): Option[(Vec2, Vec2)] = {
      Some(aabb.aabb.lowerBound, aabb.aabb.upperBound)
    }
  }
  
  implicit class AABB(val aabb: org.jbox2d.collision.AABB) extends AnyVal with WithPoint {
    
    protected def point = center
    
    def lowerBound: Vec2 = aabb.lowerBound
    def upperBound: Vec2 = aabb.upperBound
    
    def center: Vec2 = {
      aabb.getCenter()
    }
    
    def contains(other: AABB) = {
      aabb.contains(other.aabb)
    }
    
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
  
}
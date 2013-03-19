package ch.epfl.lara.synthesis.kingpong

import scala.collection.mutable.{Set => MSet}

import org.jbox2d.dynamics.World
import org.jbox2d.callbacks.{ContactListener => JBox2DContactListener}
import org.jbox2d.dynamics.contacts.{Contact => JBox2DContact}
import org.jbox2d.collision.{Manifold => JBox2DManifold}
import org.jbox2d.callbacks.{ContactImpulse => JBox2DContactImpulse}
import org.jbox2d.callbacks.{ContactFilter => JBox2DContactFilter}
import org.jbox2d.dynamics.Fixture
import org.jbox2d.callbacks.{ContactFilter => JBox2DContactFilter}
import org.jbox2d.callbacks.{ContactImpulse => JBox2DContactImpulse}
import org.jbox2d.callbacks.{ContactListener => JBox2DContactListener}
import org.jbox2d.collision.{Manifold => JBox2DManifold}
import org.jbox2d.dynamics.contacts.{Contact => JBox2DContact}

import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.objects._

class PhysicalWorld(g: Vec2) {
  
  val world = new World(g, true)
  world.setContactFilter(ContactFilter)
  world.setContactListener(ContactListener)
  
  private val begin_contacts = MSet.empty[Contact]
  private val end_contacts = MSet.empty[Contact]
  
  def step(dt: Float) = synchronized {
    begin_contacts.clear()
    end_contacts.clear()
    world.step(dt, 10, 8)
  }
  
  def beginContacts = synchronized {
    begin_contacts.toSet
  }
  
  def endContacts = synchronized {
    end_contacts.toSet
  }
  
  def setGravity(g: Vec2) = synchronized {
    world.setGravity(g)
  }
  
  def getGravity = synchronized {
    world.getGravity()
  }
  
  object ContactListener extends JBox2DContactListener {
    
    def beginContact(c: JBox2DContact) = {
      // TODO do we need to test if the contact is enabled ?
      // c.isEnabled() 
      begin_contacts += c
    }
    
    def endContact(c: JBox2DContact) = {
      // TODO do we need to test if the contact is enabled ?
      // c.isEnabled() 
      end_contacts += c
    }
    
    def preSolve(c: JBox2DContact, m: JBox2DManifold) = {
      // nothing ?
    }
    
    def postSolve(c: JBox2DContact, i: JBox2DContactImpulse) = {
      // nothing ?
    }
  }
 
  object ContactFilter extends JBox2DContactFilter {
    
    override def shouldCollide(f1: Fixture, f2: Fixture) = {
      
      true
    }
  }
}


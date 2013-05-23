package ch.epfl.lara.synthesis.kingpong

import scala.collection.mutable.{Set => MSet}

import org.jbox2d.dynamics.World
import org.jbox2d.callbacks.{ContactListener => JBox2DContactListener}
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
  
  val world = new World(g)
  world.setContactFilter(ContactFilter)
  world.setContactListener(ContactListener)
  
  private val begin_contacts = MSet.empty[Contact]
  private val current_contacts = MSet.empty[Contact]
  private val end_contacts = MSet.empty[Contact]
  
  def step() = {
    begin_contacts.clear()
    end_contacts.clear()
    world.step(GameLoop.FRAME_PERIOD_S, 3, 2)
  }
  
  def beginContacts = begin_contacts.iterator
  
  def currentContacts = current_contacts.iterator
  
  def endContacts = end_contacts.iterator
  
  def setGravity(g: Vec2) = world.setGravity(g)
  
  def getGravity = world.getGravity()
  
  object ContactListener extends JBox2DContactListener {
    
    def beginContact(c: Contact) = {
      // TODO do we need to test if the contact is enabled ?
      // c.isEnabled() 
      begin_contacts += c
      current_contacts += c
    }
    
    def endContact(c: Contact) = {
      // TODO do we need to test if the contact is enabled ?
      // c.isEnabled() 
      end_contacts += c
      current_contacts -= c
    }
    
    def preSolve(c: Contact, m: JBox2DManifold) = {
      // nothing ?
    }
    
    def postSolve(c: Contact, i: JBox2DContactImpulse) = {
      // nothing ?
    }
  }
 
  object ContactFilter extends JBox2DContactFilter {
    
    override def shouldCollide(f1: Fixture, f2: Fixture) = {
      true
    }
  }
}


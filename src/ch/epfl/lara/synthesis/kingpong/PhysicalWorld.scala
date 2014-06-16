package ch.epfl.lara.synthesis.kingpong

import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.rules.Events._
import org.jbox2d.callbacks.{ContactFilter => JBox2DContactFilter, ContactImpulse, ContactListener => JBox2DContactListener}
import org.jbox2d.collision.{WorldManifold, Manifold}
import org.jbox2d.dynamics.{Fixture, World}

import scala.collection.mutable.{Set => MSet}

class PhysicalWorld(g: Vec2) {
  private implicit val worldManifold = new WorldManifold()

  val world = new World(g)
  world.setContactFilter(ContactFilter)
  world.setContactListener(ContactListener)
  
  private val begin_contacts = MSet.empty[Contact]
  private val current_contacts = MSet.empty[Contact]
  private val end_contacts = MSet.empty[Contact]

  def step() = {
    begin_contacts.clear()
    end_contacts.clear()
    try {
      world.step(GameLoop.FRAME_PERIOD_S, 3, 2)
    } catch {
      case e: java.lang.ArrayIndexOutOfBoundsException => if(e != null) e.printStackTrace()
    }
  }
  
  def clear(): Unit = {
    begin_contacts.clear()
    current_contacts.clear()
    end_contacts.clear()
  }

  val contactEvents: Traversable[Event] = {
    val begin = begin_contacts.view.map(c => BeginContact(c.point, c.objectA, c.objectB))
    val current = current_contacts.view.map(c => CurrentContact(c.point, c.objectA, c.objectB))
    val end = end_contacts.view.map(c => EndContact(c.point, c.objectA, c.objectB))
    begin ++ current ++ end
  }

  def beginContacts = begin_contacts.iterator
  
  def currentContacts = current_contacts.iterator
  
  def endContacts = end_contacts.iterator
  
  def setGravity(g: Vec2) = world.setGravity(g)
  
  def getGravity = world.getGravity
  
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
    
    def preSolve(c: Contact, m: Manifold) = {
      // nothing ?
    }
    
    def postSolve(c: Contact, i: ContactImpulse) = {
      // nothing ?
    }
  }
 
  object ContactFilter extends JBox2DContactFilter {
    
    override def shouldCollide(f1: Fixture, f2: Fixture) = {
      true
    }
  }
}


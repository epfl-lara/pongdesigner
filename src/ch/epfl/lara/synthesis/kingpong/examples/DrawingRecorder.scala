package ch.epfl.lara.synthesis.kingpong.examples

import org.jbox2d.dynamics.BodyType
import ch.epfl.lara.synthesis.kingpong.Game
import ch.epfl.lara.synthesis.kingpong.PhysicalWorld
import ch.epfl.lara.synthesis.kingpong.common.JBox2DInterface._
import ch.epfl.lara.synthesis.kingpong.expression.TreeDSL._
import ch.epfl.lara.synthesis.kingpong.objects._

class DrawingRecorder extends Game {
  val world = new PhysicalWorld(Vec2(0, 0))

  val static_objects = Category("Static_objects")()
  val color_objects = Category("Color_objects")()
  val width_objects = Category("Width_objects")()
  
  val drawing = drawingObject(static_objects)(name="drawingZone", x=0, y=0, width=20, height=15, stroke_width=3)
  val soundrec = soundRecorder(static_objects)(name="soundrec", x= -10.5f, y=3f, width=1, height=1)
  val preview_drawing = circle(static_objects)(name="Preview", x= -10.5f, y= -7f, radius = 0.5f, color = 0xFF000000, tpe = BodyType.STATIC)
  val drawing_color1 = rectangle(color_objects)(name="Color2", x= -10.5f, y= -6f, width = 1, height = 1, color = 0xFF000000, tpe = BodyType.STATIC)
  val drawing_color2 = rectangle(color_objects)(name="Color3", x= -10.5f, y= -5f, width = 1, height = 1, color = 0xFFAA0000, tpe = BodyType.STATIC)
  val drawing_color3 = rectangle(color_objects)(name="Color4", x= -10.5f, y= -4f, width = 1, height = 1, color = 0xFF00AA00, tpe = BodyType.STATIC)
  val drawing_color4 = rectangle(color_objects)(name="Color5", x= -10.5f, y= -3f, width = 1, height = 1, color = 0xFF0000AA, tpe = BodyType.STATIC)
  val drawing_radius1 = rectangle(width_objects)(name="Radius1", x= -10.5f, y= -2f, width= 1, height = 0.5, color = 0xFF000000, tpe = BodyType.STATIC)
  val drawing_radius2 = rectangle(width_objects)(name="Radius2", x= -10.5f, y= -1f, width= 1, height = 0.375, color = 0xFF000000, tpe = BodyType.STATIC)
  val drawing_radius3 = rectangle(width_objects)(name="Radius3", x= -10.5f, y= -0f, width= 1, height = 0.25, color = 0xFF000000, tpe = BodyType.STATIC)
  val drawing_radius4 = rectangle(width_objects)(name="Radius4", x= -10.5f, y= 1f, width= 1, height = 0.125, color = 0xFF000000, tpe = BodyType.STATIC)
  val drawing_radius5 = rectangle(width_objects)(name="Radius5", x= -10.5f, y= 2f, width= 1, height = 0.075, color = 0xFF000000, tpe = BodyType.STATIC)
  
  //val ruleColor = drawing.color := drawing_color.color
  
  /*val ruleWidthFinger = fingerMoveOver(drawing_circle) { move => Seq(
    drawing_circle.radius := max(move._2._1 - drawing_circle.x, move._2._2 - drawing_circle.y)
  )}*/
  //finger
  //val ruleWidth = drawing.stroke_width := preview_drawing.radius * 5 
  
  val ruleColor = foreach(color_objects) { obj =>
    fingerDownOver(obj){ pos =>
      whenever(pos in obj)(
	      drawing.color_drawing := obj.color,
	      preview_drawing.color := obj.color
      )
    }
  }
  
  val ruleWidth = foreach(width_objects) { obj =>
     fingerDownOver(obj){ pos =>
      whenever(pos in obj)(
	      drawing.stroke_width := obj.height * 20,
	      preview_drawing.radius := obj.height / 2
	    )
     }
  }
  
  register(ruleColor)
  register(ruleWidth)
}

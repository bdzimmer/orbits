// Copyright (c) 2019 Ben Zimmer. All rights reserved.

// Utility classes for debugging orbits (long overdue).

package bdzimmer.orbits

import java.awt.Font                    // scalastyle:ignore illegal.imports
import java.awt.{GridLayout, BorderLayout}
import javax.swing.{JFrame, JLabel, JTextField, JPanel, SwingConstants}
import javax.swing.border.EmptyBorder



object Debug {
  val ENABLED = true
}


object DebugValues {

  val FieldWidth = 20
  val FieldFont = new Font("Monospace", Font.PLAIN, 16)

  val store = scala.collection.mutable.LinkedHashMap[String, Any]()
  // this does not need to be mutable, but I want it to be fast
  var fields = scala.collection.mutable.LinkedHashMap[String, JTextField]()

  var window: Option[JFrame] = None
  var contents = new JPanel(new BorderLayout())

  def set(key: String, value: Any): Unit = {
    store(key) = value
  }


  def show(): Unit = {
    fields = rebuild()
    val jframe = new JFrame("Debug Values")
    jframe.add(contents, BorderLayout.CENTER)
    jframe.pack()
    jframe.setVisible(true)
    jframe.setDefaultCloseOperation(0) // TODO: find actual constant
    window = Some(jframe)
  }


  def update(): Unit = {
    window.foreach(jframe => {
      if (fields.keySet != store.keySet) {
        rebuild()
        jframe.pack()
      }

      store.foreach({case (key, value) => {
        fields(key).setText(value.toString)
      }})

      jframe.repaint()

    })

  }


  def rebuild(): scala.collection.mutable.LinkedHashMap[String, JTextField] = {

    println("rebuilding DebugValues panel")

    val emptyborder = new EmptyBorder(10, 10, 10, 10)

    // ditch all objects in the frame and rebuild

    contents.removeAll()

    // fields map
    val fields = store.map({case (key, value) => {
     val field = new JTextField(
        value.toString,  // TODO: potentially trim to FieldWidth
        FieldWidth)
      field.setFont(FieldFont)
     key -> field
    }})

    // create UI using fields map to ensure that we are looking at
    // what we think we are

    // labels
    contents.add({
      val res = new JPanel(new GridLayout(store.size, 1))
      res.setBorder(emptyborder)
      fields.foreach({case (key, _) => {
        val label = new JLabel(key)
        label.setHorizontalAlignment(SwingConstants.LEFT)
        res.add(label)
      }})
      res
    }, BorderLayout.WEST)

    // fields
    contents.add({
      val res = new JPanel(new GridLayout(store.size, 1))
      res.setBorder(emptyborder)
      fields.foreach(x => res.add(x._2))
      res
    }, BorderLayout.CENTER)

    fields

  }


}

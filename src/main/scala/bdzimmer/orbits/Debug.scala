// Copyright (c) 2019 Ben Zimmer. All rights reserved.

// Utility classes for debugging orbits (long overdue).

package bdzimmer.orbits

import java.awt.{BorderLayout, Dimension, Font, GridLayout}

import bdzimmer.orbits.DebugDisplay.{FieldFont, FieldWidth, contents, fields, rebuild, store, window}
import javax.swing.{JFrame, JLabel, JPanel, JSlider, JTextField, SwingConstants}
import javax.swing.border.EmptyBorder
import javax.swing.event.{ChangeEvent, ChangeListener}



object Debug {
  val ENABLED = true
}


object DebugDisplay {

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
    jframe.setResizable(false)
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


object DebugInput {

  val store = scala.collection.mutable.LinkedHashMap[String, (Double, Double, Double)]()

  var window: Option[JFrame] = None
  var contents = new JPanel(new BorderLayout())
  var callback: Option[() => Unit] = None


  def setCallback(callback: () => Unit): Unit = {
    DebugInput.callback = Some(callback)
  }


  def show(): Unit = {
    rebuild()
    val jframe = new JFrame("Debug Values")
    jframe.add(contents, BorderLayout.CENTER)
    jframe.pack()
    jframe.setResizable(false)
    jframe.setVisible(true)
    jframe.setDefaultCloseOperation(0) // TODO: find actual constant
    window = Some(jframe)
  }


  def get(key: String, range: (Double, Double, Double)): Double = {

    store.get(key).map(_._1).getOrElse({
      println("new DebugInput key: " + key)
      store(key) = range
      window.foreach(jframe => {
        rebuild()
        jframe.pack()
      })
      range._1
    })

  }


  def rebuild(): Unit = {

    println("rebuilding DebugInputs panel")

    val emptyborder = new EmptyBorder(10, 10, 10, 10)

    // ditch all objects in the frame and rebuild

    contents.removeAll()

    // create UI using fields map to ensure that we are looking at
    // what we think we are

    // labels
    contents.add({
      val res = new JPanel(new GridLayout(store.size, 1))
      res.setBorder(emptyborder)
      store.foreach({case (key, _) => {
        val label = new JLabel(key)
        label.setHorizontalAlignment(SwingConstants.LEFT)
        res.add(label)
      }})
      res
    }, BorderLayout.WEST)


    // controls
    val controls = store.map({case (key, range) => {
      val field = new JTextField(range._1.toString, FieldWidth)
      field.setFont(FieldFont)
      val slider = new JSlider(
        SwingConstants.HORIZONTAL,
        (range._2 * 100.0).toInt,
        (range._3 * 100.0).toInt,
        (range._1 * 100.0).toInt)
      slider.setPreferredSize(new Dimension(480, 32))

      slider.addChangeListener(new ChangeListener {
        override def stateChanged(e: ChangeEvent): Unit = {
          val newVal: Double = slider.getValue / 100.0
          field.setText(newVal.toString)
          val storeVal = store(key)
          store(key) = (newVal, storeVal._2, storeVal._3)
        }
      })

      slider.addChangeListener(new ChangeListener {
        override def stateChanged(e: ChangeEvent): Unit = {
          // TODO: update slider? not urgent
          callback.foreach(_.apply())  // SOON
        }
      })

      // TODO: wire stuff together

      (field, slider)

    }})

    // fields
    contents.add({
      val res = new JPanel(new GridLayout(store.size, 1))
      res.setBorder(emptyborder)
      controls.foreach(x => res.add(x._1))
      res
    }, BorderLayout.CENTER)

    // sliders
    contents.add({
      val res = new JPanel(new GridLayout(store.size, 1))
      res.setBorder(emptyborder)
      controls.foreach(x => res.add(x._2))
      res
    }, BorderLayout.EAST)

  }

}
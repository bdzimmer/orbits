// Copyright (c) 2018 Ben Zimmer. All rights reserved.

// Odds and ends for nicer GUI code.

package bdzimmer.orbits

import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.{JComboBox, JSlider}
import javax.swing.event.{ChangeEvent, ChangeListener}

import scala.util.Try


trait Disableable {
  var enabled = true
}


class DisableableActionListener(callback: ActionEvent => Unit) extends ActionListener with Disableable {
  override def actionPerformed(event: ActionEvent): Unit = {
    if (enabled) {
      callback(event)
    }
  }
}


class DisableableChangeListener(callback: ChangeEvent => Unit) extends ChangeListener with Disableable {
  override def stateChanged(event: ChangeEvent): Unit = {
    if (enabled) {
      callback(event)
    }
  }
}


object Disable {

  def apply[A, E](comboBox: JComboBox[E], expr: => A): Option[A] = {
    val disableables: Array[Disableable] = comboBox.getActionListeners.collect({case x: Disableable => x})
    apply(disableables, expr)
  }

  def apply[A](slider: JSlider, expr: => A): Option[A] = {
    val disableables: Array[Disableable] = slider.getChangeListeners.collect({case x: Disableable => x})
    apply(disableables, expr)
  }

  def apply[A](disableables: Array[Disableable], expr: => A): Option[A] = {
    disableables.foreach(_.enabled = false)
    val result = Try(expr).toOption
    disableables.foreach(_.enabled = true)
    result
  }

}


class RunAtInterval(fn: () => Unit, sec: Double) extends Runnable {
  override def run(): Unit = {
    var isRunning = true
    while (isRunning) {
      fn()
      try
        Thread.sleep((sec * 1000.0).toInt)
      catch {
        case e: InterruptedException => {
          isRunning = false
          // e.printStackTrace()
        }
      }
    }
  }
}

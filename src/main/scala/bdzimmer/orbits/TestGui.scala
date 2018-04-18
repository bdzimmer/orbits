// Copyright (c) 2017 Ben Zimmer. All rights reserved.

package bdzimmer.orbits

import java.awt.{BorderLayout, Color, Dimension, Graphics, GridLayout, Image}
import java.awt.event.{ActionListener, ActionEvent}

import java.awt.image.BufferedImage
import javax.swing.{JButton, JCheckBox, JFrame, JPanel, JTextField, JSpinner, SpinnerNumberModel}
import javax.swing.event.{ChangeListener, ChangeEvent}

import bdzimmer.util.StringUtils._


class TestGui extends JFrame {

  setTitle("Orbits Testing GUI")

  val ship = Spacecraft("EOE Compass", 30000.0, 0.2)
  val startDate = CalendarDateTime(2016, 7, 27, 0)
  val endDate   = CalendarDateTime(2016, 7, 31, 0)

  val orig = MeeusPlanets.Mars
  val origName = "Mars"
  val dest = MeeusPlanets.Earth
  val destName = "Earth"

  val imWidth = 800
  val imHeight = 600

  val im = new BufferedImage(800, 600, BufferedImage.TYPE_INT_ARGB)

  /// ///

  val startDateJulian = startDate.julian
  val endDateJulian = endDate.julian

  // one tick per hour
  val res = 1.0 / 24
  val ticks = (startDateJulian to endDateJulian by res).toList.toIndexedSeq // don't ask

  // find positions of origin and destination bodies
  val origStates = ticks.map(tick => Orbits.planetState(orig, tick))
  val destStates = ticks.map(tick => Orbits.planetState(dest, tick))

  val roughFlightFn =  RoughFlightFn(
    origStates.head.position, destStates.last.position,
    startDateJulian, endDateJulian - startDateJulian)
  val flightStates = ticks.map(tick => roughFlightFn(tick))

  val distance = Vec3.length(
      Vec3.sub(destStates.last.position, origStates.head.position))

  val vel = distance / (endDateJulian - startDateJulian) // average velocity

  val allStates = origStates ++ destStates
  val planetMax = RenderFlight.maxPosition(allStates)

  /// ///

  this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

  val imagePanel = new ImagePanel(im)
  add(imagePanel, BorderLayout.CENTER)

  val controls = new JPanel()
  controls.setLayout(new GridLayout(6, 1))

  val anglesPanel = new JPanel()
  anglesPanel.setLayout(new GridLayout(1, 4))
  
  val updateChangeListener = new ChangeListener {
    def stateChanged(event: ChangeEvent): Unit = {
      update()
    }
  }
  
  val xAngleField = new JSpinner(new SpinnerNumberModel(0.0, -360.0, 360.0, 0.25))
  val yAngleField = new JSpinner(new SpinnerNumberModel(0.0, -360.0, 360.0, 0.25))
  val zAngleField = new JSpinner(new SpinnerNumberModel(0.0, -360.0, 360.0, 0.25)) 
  xAngleField.setValue(45.0)
  yAngleField.setValue(0.0)
  zAngleField.setValue(180.0)
  xAngleField.addChangeListener(updateChangeListener)
  yAngleField.addChangeListener(updateChangeListener)
  zAngleField.addChangeListener(updateChangeListener)
  
  val isIntrinsic = new JCheckBox("Int")
  isIntrinsic.addChangeListener(updateChangeListener)
  
  anglesPanel.add(xAngleField)
  anglesPanel.add(yAngleField)
  anglesPanel.add(zAngleField)
  anglesPanel.add(isIntrinsic)
  controls.add(anglesPanel)

  val posPanel = new JPanel()
  posPanel.setLayout(new GridLayout(1, 3))
  
  val xPosField = new JSpinner(new SpinnerNumberModel(0.0, -10.0, 10.0, 0.2))
  val yPosField = new JSpinner(new SpinnerNumberModel(0.0, -10.0, 10.0, 0.2))
  val zPosField = new JSpinner(new SpinnerNumberModel(0.0, -10.0, 10.0, 0.2))
  xPosField.setValue(0.0)
  yPosField.setValue(-5.0)
  zPosField.setValue(5.0)
  xPosField.addChangeListener(updateChangeListener)
  yPosField.addChangeListener(updateChangeListener)
  zPosField.addChangeListener(updateChangeListener)
  
  posPanel.add(xPosField)
  posPanel.add(yPosField)
  posPanel.add(zPosField)
  controls.add(posPanel)

  add(controls, BorderLayout.EAST)
  pack()

  update()

  setVisible(true)


  def update(): Unit = {
    
   
    val xAngle = xAngleField.getValue.asInstanceOf[Integer] * math.Pi / 180
    val yAngle = yAngleField.getValue.asInstanceOf[Integer] * math.Pi / 180
    val zAngle = zAngleField.getValue.asInstanceOf[Integer] * math.Pi / 180
    val theta = Vec3(xAngle, yAngle, zAngle)

    val camRotation = if (!isIntrinsic.isSelected()) {
      View.rotationXYZ(theta)
    } else {
      View.rotationZYX(theta)
    }

    val xPos = xPosField.getValue.asInstanceOf[Double]
    val yPos = yPosField.getValue.asInstanceOf[Double]
    val zPos = zPosField.getValue.asInstanceOf[Double]
    val camPos = Vec3(xPos, yPos, zPos)

    val camTrans = View.cameraTransform(camRotation, camPos)

    val viewPos = Vec3(0, 0, imWidth * 1.0)
    val view = new Viewer(camTrans, viewPos)

    val gr = im.getGraphics()
    gr.setColor(Color.BLACK)
    gr.fillRect(0, 0, imWidth, imHeight)

    val gridLim = (planetMax * 4).toInt
    val origFullPeriod = Orbits.planetMotionPeriod(orig, startDateJulian)
    val destFullPeriod = Orbits.planetMotionPeriod(dest, startDateJulian)

    RenderFlight.drawRoughFlightAtTime(
        ship,
        view,
        im,
        List((origName, origFullPeriod), (destName, destFullPeriod)),
        origName, destName,
        startDate.dateString, endDate.dateString,
        origStates,
        destStates,
        flightStates,
        gridLim)

    // draw flight summary
    RenderFlight.drawFlightSummary(
        im, ship, distance, vel, roughFlightFn.accel, origName, destName, startDate, endDate)

    imagePanel.repaint()
    println("done redrawing")

  }

}



object TestGui {

  val ImageWidth = 800
  val ImageHeight = 600

  def main(args: Array[String]): Unit = {
    new TestGui()
  }

}




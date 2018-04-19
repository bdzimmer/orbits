// Copyright (c) 2018 Ben Zimmer. All rights reserved.

// Orbits Editor

package bdzimmer.orbits

import java.awt.{BorderLayout, Color, Dimension, FlowLayout, Graphics, GridLayout, Image}
import java.awt.event.{ActionListener, ActionEvent, ComponentAdapter, ComponentEvent, MouseWheelListener, MouseWheelEvent}
import java.awt.image.BufferedImage
import javax.swing.{BorderFactory, JCheckBox, JComboBox, JFrame, JPanel, JSlider, JSpinner, SpinnerNumberModel, SwingConstants}
import javax.swing.event.{ChangeListener, ChangeEvent}


// this is sort of a parallel version of how flights are represented in Secondary
case class FlightParams(
    ship: Spacecraft,
    origName: String,
    destName: String,
    orig: OrbitalElementsEstimator,
    dest: OrbitalElementsEstimator,
    startDate: CalendarDateTime,
    endDate: CalendarDateTime,
    passengers: List[String],
    description: String) {

  override def toString(): String = {
    startDate.dateString + " - " + ship.name.replace("*", "") + " - " + origName + " - " + destName
  }

}


class Editor(
    flights: List[FlightParams],
    ships: List[Spacecraft]
  ) extends JFrame {

  setTitle("Orbits Editor")

  // TODO: add menu bar / toolbars

  ///

  var imWidth = Editor.ViewWidth
  var imHeight = Editor.ViewHeight
  var im = new BufferedImage(imWidth, imHeight, BufferedImage.TYPE_INT_ARGB)
  var imagePanel = new ImagePanel(im)

  /// ///

  val viewPanel = new JPanel()
  viewPanel.add(imagePanel)
  viewPanel.addMouseWheelListener(new MouseWheelListener() {
    def mouseWheelMoved(event: MouseWheelEvent): Unit = {
      val notches = event.getWheelRotation()
      zViewPosField.setValue(zViewPosField.getValue.asInstanceOf[Double] + notches * Editor.ZoomSpeed)
      // don't need to redraw here, since it seems that the above triggers change listener
    }
  });

  add(viewPanel, BorderLayout.CENTER)

  ///


  val controls = new JPanel()
  // controls.setLayout(new GridLayout(6, 1))
  controls.setPreferredSize(new Dimension(Editor.ControlsWidth, imHeight))

  val redrawChangeListener = new ChangeListener {
    def stateChanged(event: ChangeEvent): Unit = {
      redraw()
    }
  }

  ///

  val flightsPanel = new JPanel(new GridLayout(2, 1))
  flightsPanel.setBorder(BorderFactory.createTitledBorder("Flights"))
  flightsPanel.setPreferredSize(new Dimension(Editor.ControlsWidth, 64))

  val flightsComboBox = new JComboBox(flights.map(_.toString).toArray)
  flightsComboBox.addActionListener(new ActionListener {
    def actionPerformed(event: ActionEvent): Unit = {
      redraw()
    }
  })
  flightsPanel.add(flightsComboBox)

  val flightsSlider = new JSlider(SwingConstants.HORIZONTAL, 0, 100, 100)
  flightsSlider.addChangeListener(redrawChangeListener)
  flightsPanel.add(flightsSlider)

  controls.add(flightsPanel)

  ///

  val shipsPanel = new JPanel(new GridLayout(1, 1))
  shipsPanel.setBorder(BorderFactory.createTitledBorder("Ships"))
  shipsPanel.setPreferredSize(new Dimension(Editor.ControlsWidth, 64))

  val shipsComboBox = new JComboBox(ships.map(_.name).toArray)
  shipsComboBox.addActionListener(new ActionListener {
    def actionPerformed(event: ActionEvent): Unit = {
      // TODO: do stuff
    }
  })
  shipsPanel.add(shipsComboBox)

  controls.add(shipsPanel)

  /// layer visibility (flight summary, planets, other flights)
  val layersPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
  layersPanel.setBorder(BorderFactory.createTitledBorder("Layers"))
  layersPanel.setPreferredSize(new Dimension(Editor.ControlsWidth, 256))

  val planetsPanel = new JPanel()
  planetsPanel.setBorder(BorderFactory.createTitledBorder("Planets"))
  val planetCheckboxes = MeeusPlanets.Planets.map(x => (x._1, (new JCheckBox(x._1, false), x._2)))
  planetsPanel.setLayout(new GridLayout(planetCheckboxes.size, 1))

  planetCheckboxes.foreach(x => {
    x._2._1.addChangeListener(redrawChangeListener)
    planetsPanel.add(x._2._1)
  })

  // TODO: more layer visibility options
  // TODO: flight summary or status
  layersPanel.add(planetsPanel)
  controls.add(layersPanel)

  /// camera controls

  val cameraPanel = new JPanel(new GridLayout(1, 8))
  cameraPanel.setBorder(BorderFactory.createTitledBorder("Camera Angle and Position"))
  cameraPanel.setPreferredSize(new Dimension(Editor.ControlsWidth, 64))

  val xAngleField = new JSpinner(new SpinnerNumberModel(0.0, -360.0, 360.0, 0.25))
  val yAngleField = new JSpinner(new SpinnerNumberModel(0.0, -360.0, 360.0, 0.25))
  val zAngleField = new JSpinner(new SpinnerNumberModel(0.0, -360.0, 360.0, 0.25))
  xAngleField.setValue(45.0)
  yAngleField.setValue(0.0)
  zAngleField.setValue(180.0)
  xAngleField.addChangeListener(redrawChangeListener)
  yAngleField.addChangeListener(redrawChangeListener)
  zAngleField.addChangeListener(redrawChangeListener)
  val isIntrinsic = new JCheckBox("Int")
  isIntrinsic.addChangeListener(redrawChangeListener)

  val xPosField = new JSpinner(new SpinnerNumberModel(0.0, -100.0, 100.0, 0.2))
  val yPosField = new JSpinner(new SpinnerNumberModel(0.0, -100.0, 100.0, 0.2))
  val zPosField = new JSpinner(new SpinnerNumberModel(0.0, -100.0, 100.0, 0.2))
  val zViewPosField = new JSpinner(new SpinnerNumberModel(0.0, 0.0, 10000.0, 10.0))
  xPosField.setValue(0.0)
  yPosField.setValue(-5.0)
  zPosField.setValue(5.0)
  zViewPosField.setValue(imWidth * 1.0)
  xPosField.addChangeListener(redrawChangeListener)
  yPosField.addChangeListener(redrawChangeListener)
  zPosField.addChangeListener(redrawChangeListener)
  zViewPosField.addChangeListener(redrawChangeListener)

  cameraPanel.add(xAngleField)
  cameraPanel.add(yAngleField)
  cameraPanel.add(zAngleField)
  cameraPanel.add(isIntrinsic)
  cameraPanel.add(xPosField)
  cameraPanel.add(yPosField)
  cameraPanel.add(zPosField)
  cameraPanel.add(zViewPosField)

  controls.add(cameraPanel)

  ///

  add(controls, BorderLayout.WEST)

  pack()
  redraw()

  addComponentListener(new ComponentAdapter {
    override def componentResized(event: ComponentEvent): Unit = {
      rebuildImagePanel()
      redraw()
    }
  })

  setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  setResizable(true)
  setVisible(true)

  /// ///

  def rebuildImagePanel(): Unit = {
    imWidth = viewPanel.getWidth
    imHeight = viewPanel.getHeight
    if (imHeight > 0 && imWidth > 0 ) {
      viewPanel.remove(imagePanel)
      im = new BufferedImage(imWidth, imHeight, BufferedImage.TYPE_INT_ARGB)
      imagePanel = new ImagePanel(im)
      viewPanel.add(imagePanel)
      viewPanel.revalidate()
    }
  }


  def redraw(): Unit = {

    val idx = flightsComboBox.getSelectedIndex
    val fp = flights(idx)

    val startDateJulian = fp.startDate.julian
    val endDateJulian = fp.endDate.julian

    // one tick per hour
    val res = 1.0 / 24
    val ticks = (startDateJulian to endDateJulian by res).toList.toIndexedSeq // don't ask

    // find positions of origin and destination bodies
    val origStates = ticks.map(tick => Orbits.planetState(fp.orig, tick))
    val destStates = ticks.map(tick => Orbits.planetState(fp.dest, tick))

    val roughFlightFn =  RoughFlightFn(
      origStates.head.position, destStates.last.position,
      startDateJulian, endDateJulian - startDateJulian)

    // take a fraction of the ticks based on the slider
    val flightPercent = flightsSlider.getValue() / 100.0
    val ticksSubset = ticks.take((flightPercent * ticks.size).toInt)

    val distance = Vec3.length(
        Vec3.sub(destStates.last.position, origStates.head.position))

    val vel = distance / (endDateJulian - startDateJulian) // average velocity

    val allStates = origStates ++ destStates
    val planetMax = RenderFlight.maxPosition(allStates)

    val gridLim = (planetMax * 4).toInt

    // TODO: deal with empty ticksSubset

    val flightStates = ticksSubset.map(tick => roughFlightFn(tick))
    val planets = planetCheckboxes.toList.filter(_._2._1.isSelected).map(x => {
      (x._1, Orbits.planetMotionPeriod(x._2._2, ticksSubset.last))
    })

    val camTrans = getCamera()
    val viewPos = getViewPos()
    val view = new Viewer(camTrans, viewPos)

    val gr = im.getGraphics()
    gr.setColor(Color.BLACK)
    gr.fillRect(0, 0, imWidth, imHeight)

    RenderFlight.drawRoughFlightAtTime(
        fp.ship,
        view,
        im,
        planets,
        fp.origName, fp.destName,
        fp.startDate.dateString, fp.endDate.dateString,
        origStates,
        destStates,
        flightStates,
        gridLim)

    // draw flight summary
    // RenderFlight.drawFlightSummary(
    //    im, fp.ship, distance, vel, roughFlightFn.accel, fp.origName, fp.destName, fp.startDate, fp.endDate)

    // draw flight status with current datetime, distance, and velocity
    val curDateJulian = ticksSubset.last
    val velEps = 0.01
    val curVel = Vec3.length(
        Vec3.mul(
            Vec3.sub(
                roughFlightFn(curDateJulian),
                roughFlightFn(curDateJulian - velEps)),
            1.0 / velEps))
    RenderFlight.drawFlightStatus(
        im,
        fp.ship,
        Conversions.julianToCalendarDate(curDateJulian),
        Vec3.length(Vec3.sub(flightStates.last, flightStates.head)),
        curVel)

    imagePanel.repaint()
    System.out.print(".")

  }


  // get camera matrix from UI
  private def getCamera(): Mat44 = {

    val xAngle = xAngleField.getValue.asInstanceOf[Double] * math.Pi / 180
    val yAngle = yAngleField.getValue.asInstanceOf[Double] * math.Pi / 180
    val zAngle = zAngleField.getValue.asInstanceOf[Double] * math.Pi / 180
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

    View.cameraTransform(camRotation, camPos)

  }

  // get viewer position from UI
  private def getViewPos(): Vec3 = {
    val zViewPos = zViewPosField.getValue.asInstanceOf[Double]
    Vec3(0, 0, zViewPos)
  }

}



object Editor {

  val ViewWidth = 800
  val ViewHeight = 600
  val ZoomSpeed = 10
  val ControlsWidth = 400

}



class ImagePanel(val image: Image) extends JPanel {
  setPreferredSize(
    new Dimension(image.getWidth(null), image.getHeight(null)))
  setVisible(true)

  override def paintComponent(gr: Graphics): Unit = {
    super.paintComponent(gr)
    gr.drawImage(image, 0, 0, null)
  }

}

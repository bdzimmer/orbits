// Copyright (c) 2018 Ben Zimmer. All rights reserved.

// Orbits Editor

package bdzimmer.orbits

import java.awt.{BorderLayout, Color, Dimension, FlowLayout, Graphics, GridLayout, Image}
import java.awt.event.{ActionListener, ActionEvent, ComponentAdapter, ComponentEvent, MouseWheelListener, MouseWheelEvent}
import java.awt.image.BufferedImage
import javax.swing.{
    BorderFactory, JCheckBox, JComboBox, JFrame,
    JLabel, JMenuBar, JMenu, JMenuItem, JPanel,
    JSeparator, JSlider, JSpinner, JTextArea, JToolBar, JToggleButton, JTextField,
    SpinnerNumberModel, SwingConstants}
import javax.swing.event.{ChangeListener, ChangeEvent, DocumentListener, DocumentEvent}

import scala.util.Try


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


case class CameraControls(
    xAngleField: JSpinner,
    yAngleField: JSpinner,
    zAngleField: JSpinner,
    isIntrinsic: JCheckBox,
    xPosField: JSpinner,
    yPosField: JSpinner,
    zPosField: JSpinner,
    zViewPosField: JSpinner
)


class Editor(
    flights: List[FlightParams],
    ships: List[Spacecraft]
  ) extends JFrame {

  setTitle("Orbits Editor")

  /// /// image for view

  var imWidth = Editor.ViewWidth
  var imHeight = Editor.ViewHeight
  var im = new BufferedImage(imWidth, imHeight, BufferedImage.TYPE_INT_ARGB)
  var imagePanel = new ImagePanel(im)

  /// /// build menu bar

  setJMenuBar(Editor.buildMenuBar())

  /// /// build toolbars

  val redrawChangeListener = new ChangeListener {
    def stateChanged(event: ChangeEvent): Unit = {
      redraw()
    }
  }

  val toolbarsPanel = new JPanel(new FlowLayout(FlowLayout.LEFT))
  toolbarsPanel.add(Editor.buildUnitConverterToolbar())
  val (cameraToolbar, cameraControls) = Editor.buildCameraToolbar(imWidth, redrawChangeListener)
  toolbarsPanel.add(cameraToolbar)

  add(toolbarsPanel, BorderLayout.NORTH)

  /// /// build view panel

  val viewPanel = new JPanel()
  viewPanel.add(imagePanel)
  viewPanel.addMouseWheelListener(new MouseWheelListener() {
    def mouseWheelMoved(event: MouseWheelEvent): Unit = {
      val notches = event.getWheelRotation()
      cameraControls.zViewPosField.setValue(
          cameraControls.zViewPosField.getValue.asInstanceOf[Double] + notches * Editor.ZoomSpeed)
      // don't need to redraw here, since it seems that the above triggers change listener
    }
  });

  add(viewPanel, BorderLayout.CENTER)

  /// /// build controls

  val controls = new JPanel()
  // controls.setLayout(new GridLayout(6, 1))
  controls.setPreferredSize(new Dimension(Editor.ControlsWidth, imHeight))

  ///

  val flightsPanel = new JPanel(new GridLayout(3, 1))
  flightsPanel.setBorder(BorderFactory.createTitledBorder("Flights"))
  flightsPanel.setPreferredSize(new Dimension(Editor.ControlsWidth, 128))

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

  layersPanel.add(planetsPanel)

  val flightStatusComboBox = new JComboBox(Array("Status", "Summary", "None"))
  flightStatusComboBox.addActionListener(new ActionListener {
    def actionPerformed(event: ActionEvent): Unit = {
      redraw()
    }
  })
  layersPanel.add(flightStatusComboBox)

  // TODO: more layer visibility options

  controls.add(layersPanel)

  add(controls, BorderLayout.WEST)

  /// ///

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


    val statusOption = flightStatusComboBox.getSelectedIndex()

    if (statusOption == 0) {
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
    } else if (statusOption == 1) {
      // draw flight summary
      RenderFlight.drawFlightSummary(
          im, fp.ship, distance, vel, roughFlightFn.accel,
          fp.origName, fp.destName, fp.startDate, fp.endDate)
    }

    imagePanel.repaint()
    System.out.print(".")

  }


  // get camera matrix from UI
  private def getCamera(): Mat44 = {

    val xAngle = cameraControls.xAngleField.getValue.asInstanceOf[Double] * math.Pi / 180
    val yAngle = cameraControls.yAngleField.getValue.asInstanceOf[Double] * math.Pi / 180
    val zAngle = cameraControls.zAngleField.getValue.asInstanceOf[Double] * math.Pi / 180
    val theta = Vec3(xAngle, yAngle, zAngle)

    val camRotation = if (!cameraControls.isIntrinsic.isSelected()) {
      View.rotationXYZ(theta)
    } else {
      View.rotationZYX(theta)
    }

    val xPos = cameraControls.xPosField.getValue.asInstanceOf[Double]
    val yPos = cameraControls.yPosField.getValue.asInstanceOf[Double]
    val zPos = cameraControls.zPosField.getValue.asInstanceOf[Double]
    val camPos = Vec3(xPos, yPos, zPos)

    View.cameraTransform(camRotation, camPos)

  }


  // get viewer position from UI
  private def getViewPos(): Vec3 = {
    val zViewPos = cameraControls.zViewPosField.getValue.asInstanceOf[Double]
    Vec3(0, 0, zViewPos)
  }


}



object Editor {

  val ViewWidth = 800
  val ViewHeight = 600
  val ZoomSpeed = 10
  val ControlsWidth = 400


  def buildMenuBar(): JMenuBar = {

    val menuBar = new JMenuBar()
    val fileMenu = new JMenu("File")
    val reloadItem = new JMenuItem("Reload")
    val exportItem = new JMenuItem("Export")
    val exitItem = new JMenuItem("Exit")

    reloadItem.addActionListener(new ActionListener() {
      override def actionPerformed(event: ActionEvent): Unit = {
        // TODO: implement
      }
    })

    exportItem.addActionListener(new ActionListener() {
      override def actionPerformed(event: ActionEvent): Unit = {
        // TODO: implement
      }
    })

    exitItem.addActionListener(new ActionListener() {
      override def actionPerformed(event: ActionEvent): Unit = {
        sys.exit()
      }
    })

    fileMenu.add(reloadItem)
    fileMenu.add(exportItem)
    fileMenu.add(exitItem)
    menuBar.add(fileMenu)

    menuBar
  }


  def buildUnitConverterToolbar(): JToolBar = {

    val toolbar = new JToolBar()
    toolbar.setLayout(new FlowLayout(FlowLayout.LEFT))
    toolbar.setBorder(BorderFactory.createTitledBorder(toolbar.getBorder, "Unit Converter"))

    val auPerDaySqText = new JTextField("", 10)
    val gText = new JTextField("", 10)
    auPerDaySqText.setMaximumSize(auPerDaySqText.getPreferredSize)
    gText.setMaximumSize(gText.getPreferredSize)

    var converterEnabled = true

    val auPerDaySqListener = new DocumentListener {
      override def changedUpdate(event: DocumentEvent): Unit = {}
      override def insertUpdate(event: DocumentEvent): Unit = update()
      override def removeUpdate(event: DocumentEvent): Unit = update()
      def update(): Unit = {
        if (converterEnabled) {
          println("editing au")
          Try(auPerDaySqText.getText.toDouble).foreach(x => {
            val aud2ToMs2 = Conversions.AuToMeters / (Conversions.DayToSec * Conversions.DayToSec)
            val accelG = x * aud2ToMs2 / Conversions.GToMetersPerSecond
            val accelGRound = math.rint(accelG * 1000.0) / 1000.0
            converterEnabled = false
            gText.setText(accelGRound.toString)
            converterEnabled= true
          })
        }
      }
    }

    val gListener = new DocumentListener {
      override def changedUpdate(event: DocumentEvent): Unit = {}
      override def insertUpdate(event: DocumentEvent): Unit = update()
      override def removeUpdate(event: DocumentEvent): Unit = update()
      def update(): Unit = {
        if (converterEnabled) {
          println("editing g")
          Try(gText.getText.toDouble).foreach(x => {
            val aud2ToMs2 = Conversions.AuToMeters / (Conversions.DayToSec * Conversions.DayToSec)
            val accelAud2 = x / aud2ToMs2 * Conversions.GToMetersPerSecond
            val accelAud2Round = math.rint(accelAud2 * 1000.0) / 1000.0
            converterEnabled = false
            auPerDaySqText.setText(accelAud2Round.toString)
            converterEnabled = true
          })
        }
      }
    }

    auPerDaySqText.getDocument.addDocumentListener(auPerDaySqListener)
    gText.getDocument.addDocumentListener(gListener)

    toolbar.add(auPerDaySqText)
    toolbar.add(new JLabel("AU/day²  "))
    toolbar.add(gText)
    toolbar.add(new JLabel("g  "))

    toolbar
  }


  def buildCameraToolbar(
      zViewPos: Double, redrawChangeListener: ChangeListener): (JToolBar, CameraControls) = {

    val spinnerWidth = 3

    val toolbar = new JToolBar()
    toolbar.setBorder(BorderFactory.createTitledBorder(toolbar.getBorder, "Camera"))

    val xAngleField = new JSpinner(new SpinnerNumberModel(0.0, -360.0, 360.0, 0.25))
    val yAngleField = new JSpinner(new SpinnerNumberModel(0.0, -360.0, 360.0, 0.25))
    val zAngleField = new JSpinner(new SpinnerNumberModel(0.0, -360.0, 360.0, 0.25))
    xAngleField.setValue(45.0)
    yAngleField.setValue(0.0)
    zAngleField.setValue(180.0)
    xAngleField.addChangeListener(redrawChangeListener)
    yAngleField.addChangeListener(redrawChangeListener)
    zAngleField.addChangeListener(redrawChangeListener)
    xAngleField.getEditor.asInstanceOf[JSpinner.DefaultEditor].getTextField.setColumns(spinnerWidth)
    yAngleField.getEditor.asInstanceOf[JSpinner.DefaultEditor].getTextField.setColumns(spinnerWidth)
    zAngleField.getEditor.asInstanceOf[JSpinner.DefaultEditor].getTextField.setColumns(spinnerWidth)
    val isIntrinsic = new JCheckBox("Int")
    isIntrinsic.addChangeListener(redrawChangeListener)

    val xPosField = new JSpinner(new SpinnerNumberModel(0.0, -100.0, 100.0, 0.2))
    val yPosField = new JSpinner(new SpinnerNumberModel(0.0, -100.0, 100.0, 0.2))
    val zPosField = new JSpinner(new SpinnerNumberModel(0.0, -100.0, 100.0, 0.2))
    val zViewPosField = new JSpinner(new SpinnerNumberModel(0.0, 0.0, 10000.0, 10.0))
    xPosField.setValue(0.0)
    yPosField.setValue(-5.0)
    zPosField.setValue(5.0)
    zViewPosField.setValue(zViewPos)
    xPosField.addChangeListener(redrawChangeListener)
    yPosField.addChangeListener(redrawChangeListener)
    zPosField.addChangeListener(redrawChangeListener)
    zViewPosField.addChangeListener(redrawChangeListener)
    xPosField.getEditor.asInstanceOf[JSpinner.DefaultEditor].getTextField.setColumns(spinnerWidth)
    yPosField.getEditor.asInstanceOf[JSpinner.DefaultEditor].getTextField.setColumns(spinnerWidth)
    zPosField.getEditor.asInstanceOf[JSpinner.DefaultEditor].getTextField.setColumns(spinnerWidth)
    zViewPosField.getEditor.asInstanceOf[JSpinner.DefaultEditor].getTextField.setColumns(spinnerWidth)

    toolbar.add(xAngleField)
    toolbar.add(yAngleField)
    toolbar.add(zAngleField)
    toolbar.add(isIntrinsic)
    toolbar.add(new JSeparator(SwingConstants.VERTICAL))
    toolbar.add(xPosField)
    toolbar.add(yPosField)
    toolbar.add(zPosField)
    toolbar.add(zViewPosField)

    (toolbar, CameraControls(
        xAngleField, yAngleField, zAngleField, isIntrinsic,
        xPosField, yPosField, zPosField, zViewPosField))

  }

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

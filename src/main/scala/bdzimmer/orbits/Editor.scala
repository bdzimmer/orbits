// Copyright (c) 2018 Ben Zimmer. All rights reserved.

// Orbits Editor

package bdzimmer.orbits

import java.awt.{BorderLayout, Color, Dimension, FlowLayout, Graphics, GridLayout, Image}
import java.awt.event.{
  ActionListener, ActionEvent, ComponentAdapter, ComponentEvent,
  InputEvent,
  MouseEvent, MouseWheelListener, MouseWheelEvent}
import java.awt.image.BufferedImage
import javax.swing.{
    BorderFactory, ButtonGroup, JButton, JCheckBox, JCheckBoxMenuItem, JComboBox, JFrame,
    JLabel, JMenuBar, JMenu, JMenuItem, JPanel, JRadioButtonMenuItem,
    JSeparator, JSlider, JSpinner, JTextArea, JToolBar, JToggleButton, JTextField,
    SpinnerNumberModel, SwingConstants}
import javax.swing.event.{ChangeListener, ChangeEvent, DocumentListener, DocumentEvent, MouseInputAdapter}

import scala.util.Try
import scala.collection.JavaConverters._

import org.apache.commons.io.FileUtils

import bdzimmer.util.StringUtils._


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
    faction: String,
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

  val factions = Editor.loadFactions(Editor.FactionsFilename)

  /// /// image for view

  var imWidth = Editor.ViewWidth
  var imHeight = Editor.ViewHeight
  var im = new BufferedImage(imWidth, imHeight, BufferedImage.TYPE_INT_ARGB)
  var imagePanel = new ImagePanel(im)

  val redrawChangeListener = new ChangeListener {
    def stateChanged(event: ChangeEvent): Unit = {
      redraw()
    }
  }

  val redrawActionListener = new ActionListener {
    def actionPerformed(event: ActionEvent): Unit = {
      redraw()
    }
  }

  /// /// build menu bar

  val (
    mainMenuBar,
    planetCheckboxes,
    lagrangePointCheckBox,
    asteroidBeltCheckBox,
    flightStatusRadioButtons) = Editor.buildMenuBar(redrawChangeListener, redrawActionListener)

  setJMenuBar(mainMenuBar)

  /// /// build toolbars

  val toolbarsPanel = new JPanel(new BorderLayout())

  val toolbarRow0 = new JPanel(new FlowLayout(FlowLayout.LEFT))

  val (flightsToolbar, flightsComboBox, flightsSlider) = Editor.buildFlightsToolbar(
      flights, ships, redrawChangeListener, redrawActionListener)
  toolbarRow0.add(flightsToolbar)

  toolbarsPanel.add(toolbarRow0, BorderLayout.NORTH)


  val toolbarRow1 = new JPanel(new FlowLayout(FlowLayout.LEFT))

  val (cameraToolbar, cameraControls) = Editor.buildCameraToolbar(imWidth, redrawChangeListener)
  toolbarRow1.add(cameraToolbar)
  toolbarRow1.add(Editor.buildUnitConverterToolbar())

  // toolbarsPanel.add(toolbarRow1, BorderLayout.SOUTH)

  add(toolbarsPanel, BorderLayout.NORTH)

  /// /// build view panel

  val viewPanel = new JPanel()
  viewPanel.add(imagePanel)
  viewPanel.addMouseWheelListener(new MouseWheelListener() {
    def mouseWheelMoved(event: MouseWheelEvent): Unit = {
      val notches = event.getWheelRotation()
      val zViewPos = cameraControls.zViewPosField.getValue.asInstanceOf[Double] - notches * Editor.ZoomSpeed
      cameraControls.zViewPosField.setValue(math.max(zViewPos, 0.0))
      // don't need to redraw here, since it seems that the above triggers change listener
    }
  })

  val mousePanListener = new MouseInputAdapter() {
    var x = 0.0
    var y = 0.0
    var cx = 0.0
    var cy = 0.0

    override def mousePressed(event: MouseEvent): Unit = {
      x = event.getX()
      y = event.getY()
      if (event.getButton() == MouseEvent.BUTTON1) {
        cx = cameraControls.xAngleField.getValue.asInstanceOf[Double]
        cy = cameraControls.yAngleField.getValue.asInstanceOf[Double]
      } else {
        cx = cameraControls.xPosField.getValue.asInstanceOf[Double]
        cy = cameraControls.yPosField.getValue.asInstanceOf[Double]
      }
    }

    override def mouseDragged(event: MouseEvent): Unit = {
      val dx = event.getX() - x
      val dy = event.getY() - y
      // println(dx + " " + dy)
      // TODO: adjust and rotate direction based on camera angle
      if ((event.getModifiersEx & InputEvent.BUTTON1_DOWN_MASK) != 0) {
        // println("changing angles")
        cameraControls.xAngleField.setValue(cx + dy * Editor.RotateSpeed)
        cameraControls.yAngleField.setValue(cy + dx * Editor.RotateSpeed)
      } else {
        // println("changing pan")
        cameraControls.xPosField.setValue(cx - dx * Editor.PanSpeed)
        cameraControls.yPosField.setValue(cy + dy * Editor.PanSpeed)
      }
    }
  }

  viewPanel.addMouseListener(mousePanListener)
  viewPanel.addMouseMotionListener(mousePanListener)

  add(viewPanel, BorderLayout.CENTER)

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

    val (roughFlightFn, ticks) = Editor.paramsToFun(fp)

    // to plot how the origin and desination change curing the flight
    val origStates = ticks.map(tick => Orbits.planetState(fp.orig, tick))
    val destStates = ticks.map(tick => Orbits.planetState(fp.dest, tick))

    // take a fraction of the ticks based on the slider
    val flightPercent = flightsSlider.getValue() / 100.0
    val ticksSubset = ticks.take((flightPercent * ticks.size).toInt)
    val curDateJulian = ticksSubset.last

    val distance = Vec3.length(
        Vec3.sub(destStates.last.position, origStates.head.position))

    // average velocity
    val vel = distance / (fp.endDate.julian - fp.startDate.julian)

    // val allStates = origStates ++ destStates
    // val planetMax = RenderFlight.maxPosition(allStates)
    // val gridLim = (planetMax * 4).toInt

    val gridLim = 50 // radius of solar system is about 50 AU

    val flightStates = ticksSubset.map(tick => roughFlightFn(tick))
    val planets = planetCheckboxes.toList.filter(_._2._1.isSelected).map(x => {
      (x._1, x._2._2)
    })
    val planetMotions = planets.map(x => {
      (x._1, Orbits.planetMotionPeriod(x._2, curDateJulian))
    })

    // find other flights that are active at the same time as the current one
    val activeFlights = flights.filter(x =>
      !x.equals(fp) && x.startDate.julian <= curDateJulian && x.endDate.julian >= curDateJulian)
    val activeFlightFns = activeFlights.map(af => Editor.paramsToFun(af))
    val otherFlights = activeFlightFns.map({case (afFn, afTicks) => {
      afTicks.filter(x => x <= curDateJulian).map(tick => afFn(tick))
    }})

    val camTrans = getCamera()
    val viewPos = getViewPos()
    val view = new Viewer(camTrans, viewPos)

    val gr = im.getGraphics()
    gr.setColor(Color.BLACK)
    gr.fillRect(0, 0, imWidth, imHeight)

    val flightColor = factions.get(fp.faction).getOrElse(Color.GREEN)
    val otherFlightsColors = activeFlights.map(x => factions.get(x.faction).getOrElse(Color.GRAY))

    RenderFlight.drawRoughFlightAtTime(
        view,
        im,
        planetMotions,
        fp.origName,
        fp.destName,
        fp.startDate.dateString,
        fp.endDate.dateString,
        origStates,
        destStates,
        flightStates,
        flightColor,
        otherFlights.zip(otherFlightsColors),
        gridLim)

    // draw asteroid belt - main belt lies between 2.06 and 3.27 AU
    if (asteroidBeltCheckBox.isSelected) {
      view.drawRing(im, 2.06, 3.27, new Color(64, 64, 64, 64))
    }

    // draw L3, L4 and L5 points of visible planets
    if (lagrangePointCheckBox.isSelected) {
      planets.foreach(p => {
        view.drawPosition(
          im, Orbits.planetState(new MeeusPlanets.L3Estimator(p._2), curDateJulian).position,
          "L3", "", Color.GRAY, false)
        view.drawPosition(
          im, Orbits.planetState(new MeeusPlanets.L4Estimator(p._2), curDateJulian).position,
          "L4", "", Color.GRAY, false)
        view.drawPosition(
          im, Orbits.planetState(new MeeusPlanets.L5Estimator(p._2), curDateJulian).position,
          "L5", "", Color.GRAY, false)
      })
    }

    // draw velocity direction arrows

    def drawVelocityArrow(flightFn: RoughFlightFn, color: Color): Double = {
      val velEps = 0.01
      if (curDateJulian - velEps > flightFn.startTime) {
        val curState = flightFn(curDateJulian)
        val curVelVec = Vec3.mul(
          Vec3.sub(curState, flightFn(curDateJulian - velEps)),
          1.0 / velEps)
        val curVel = Vec3.length(curVelVec)
        if (curVel > 1.0e-9) {
          view.drawArrow(im, OrbitalState(curState, curVelVec), color)
        }
        curVel
      } else {
        0.0
      }
    }

    activeFlightFns.zip(otherFlightsColors).foreach({case (x, y) => drawVelocityArrow(
        x._1, y)})

    val curVel = drawVelocityArrow(
        roughFlightFn, flightColor)

    val statusOption = flightStatusRadioButtons.indexWhere(_.isSelected)

    if (statusOption == 0) {
      // draw flight status with current datetime, distance, and velocity
      RenderFlight.drawFlightStatus(
          im,
          fp.ship,
          fp.faction,
          Conversions.julianToCalendarDate(curDateJulian),
          Vec3.length(Vec3.sub(flightStates.last, flightStates.head)),
          curVel)
    } else if (statusOption == 1) {
      // draw flight summary
      RenderFlight.drawFlightSummary(
          im, fp.ship, fp.faction, distance, vel, roughFlightFn.accel,
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
  val InitialVisiblePlanets = List("Earth", "Mars", "Saturn", "Uranus")

  val ZoomSpeed = 50
  val PanSpeed = 0.01
  val RotateSpeed = 0.1
  val ControlsWidth = 400

  val FactionsFilename = "factions.txt"


  def paramsToFun(fp: FlightParams): (RoughFlightFn, scala.collection.immutable.Seq[Double]) = {

    val startDateJulian = fp.startDate.julian
    val endDateJulian = fp.endDate.julian

    // one tick per hour
    val res = 1.0 / 24
    val ticks = (startDateJulian to endDateJulian by res).toList.toIndexedSeq // don't ask

    // find positions of origin and destination bodies
    val origState = Orbits.planetState(fp.orig, startDateJulian)
    val destState = Orbits.planetState(fp.dest, endDateJulian)

    val roughFlightFn =  RoughFlightFn(
      origState.position, destState.position,
      startDateJulian, endDateJulian - startDateJulian)

    (roughFlightFn, ticks)
  }


  def buildMenuBar(
      redrawChangeListener: ChangeListener,
      redrawActionListener: ActionListener): (
    JMenuBar,
    scala.collection.immutable.ListMap[String, (JCheckBoxMenuItem, OrbitalElementsEstimator)],
    JCheckBoxMenuItem,
    JCheckBoxMenuItem,
    List[JRadioButtonMenuItem]) = {

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

    val viewMenu = new JMenu("View")

    val planetCheckBoxes = MeeusPlanets.Planets.map(x => (x._1, (new JCheckBoxMenuItem(x._1, false), x._2)))
    InitialVisiblePlanets.foreach(x => planetCheckBoxes.get(x).foreach(_._1.setSelected(true)))
    planetCheckBoxes.foreach(x => {
      x._2._1.addChangeListener(redrawChangeListener)
    })
    planetCheckBoxes.foreach(x => viewMenu.add(x._2._1))
    // TODO: sections for toggling inner and outer planets
    viewMenu.add(new JSeparator(SwingConstants.HORIZONTAL))

    val lagrangePointCheckBox = new JCheckBoxMenuItem("Lagrange Points", true)
    lagrangePointCheckBox.addChangeListener(redrawChangeListener)
    viewMenu.add(lagrangePointCheckBox)
    viewMenu.add(new JSeparator(SwingConstants.HORIZONTAL))

    val asteroidBeltCheckBox = new JCheckBoxMenuItem("Asteroid Belt", true)
    asteroidBeltCheckBox.addChangeListener(redrawChangeListener)
    viewMenu.add(asteroidBeltCheckBox)
    viewMenu.add(new JSeparator(SwingConstants.HORIZONTAL))

    val flightStatusButtonGroup = new ButtonGroup()
    val flightStatusRadioButtons = List("Status", "Summary", "None").map(x => new JRadioButtonMenuItem(x))
    flightStatusRadioButtons(0).setSelected(true)
    flightStatusRadioButtons.foreach(x => x.addActionListener(redrawActionListener))
    flightStatusRadioButtons.map(x => flightStatusButtonGroup.add(x))
    flightStatusRadioButtons.foreach(x => viewMenu.add(x))

    menuBar.add(fileMenu)
    menuBar.add(viewMenu)

    (menuBar, planetCheckBoxes, lagrangePointCheckBox, asteroidBeltCheckBox, flightStatusRadioButtons)
  }


  def buildFlightsToolbar(
      flights: List[FlightParams],
      ships: List[Spacecraft],
      redrawChangeListener: ChangeListener,
      redrawActionListener: ActionListener): (JToolBar, JComboBox[String], JSlider) = {

    val toolbar = new JToolBar()
    toolbar.setLayout(new FlowLayout(FlowLayout.LEFT))
    toolbar.setBorder(BorderFactory.createTitledBorder(toolbar.getBorder, "Flights"))

    val flightsComboBox = new JComboBox(flights.map(_.toString).toArray)
    toolbar.add(flightsComboBox)

    val flightsSlider = new JSlider(SwingConstants.HORIZONTAL, 1, 100, 100)
    flightsSlider.addChangeListener(redrawChangeListener)
    toolbar.add(flightsSlider)

    /// ///

    val shipsComboBox = new JComboBox(ships.map(_.name.replace("*", "")).toArray)

    val planetsList = MeeusPlanets.Planets.keys.toList
    val planetsArray = planetsList.toArray

    val startLocComboBox = new JComboBox(planetsArray)

    val startDateText = new JTextField("", 12)
    startDateText.setMaximumSize(startDateText.getPreferredSize)

    val endLocComboBox = new JComboBox(planetsArray)

    val endDateText = new JTextField("", 12)
    endDateText.setMaximumSize(endDateText.getPreferredSize)

    def update(): Unit = {
      // for now
      // build a new flightparams from the UI and print it out

      val origName = startLocComboBox.getSelectedItem.asInstanceOf[String]
      val destName = endLocComboBox.getSelectedItem.asInstanceOf[String]

      val fp = FlightParams(
        ship=ships(shipsComboBox.getSelectedIndex),
        origName=origName,
        destName=destName,
        orig=MeeusPlanets.Planets.getOrElse(origName, MeeusPlanets.Earth),
        dest=MeeusPlanets.Planets.getOrElse(destName, MeeusPlanets.Earth),
        startDate=DateTime.parse(startDateText.getText),
        endDate=DateTime.parse(endDateText.getText),
        passengers=List(),
        faction="none",
        description=""
      )
      println(fp)
    }

    def updateFlightEditFields(): Unit = {
      val fp = flights(flightsComboBox.getSelectedIndex)
      shipsComboBox.setSelectedIndex(ships.indexOf(fp.ship))
      startLocComboBox.setSelectedIndex(planetsList.indexOf(fp.origName)) // TODO: this isn't quite right
      endLocComboBox.setSelectedIndex(planetsList.indexOf(fp.destName))
      startDateText.setText(fp.startDate.dateTimeString)
      endDateText.setText(fp.endDate.dateTimeString)
    }

    val updateActionListener = new ActionListener {
      override def actionPerformed(event: ActionEvent): Unit = {
        update()
      }
    }

    val updateDocumentListener = new DocumentListener {
      override def changedUpdate(event: DocumentEvent): Unit = {}
      override def insertUpdate(event: DocumentEvent): Unit = update()
      override def removeUpdate(event: DocumentEvent): Unit = update()
    }

    flightsComboBox.addActionListener(new ActionListener{
      override def actionPerformed(event: ActionEvent): Unit = {
        // update the flight editing fields
        updateFlightEditFields()
        redrawActionListener.actionPerformed(event)
      }
    })
    updateFlightEditFields()

    shipsComboBox.addActionListener(updateActionListener)
    startLocComboBox.addActionListener(updateActionListener)
    endLocComboBox.addActionListener(updateActionListener)
    startDateText.getDocument.addDocumentListener(updateDocumentListener)
    endDateText.getDocument.addDocumentListener(updateDocumentListener)

    val solveButton = new JButton("Solve")
    val newButton = new JButton("New")
    val deleteButton = new JButton("Delete")

    toolbar.add(shipsComboBox)
    toolbar.add(startLocComboBox)
    toolbar.add(startDateText)
    toolbar.add(endLocComboBox)
    toolbar.add(endDateText)
    toolbar.add(solveButton)
    toolbar.add(newButton)
    toolbar.add(deleteButton)

    /// ///

    (toolbar, flightsComboBox, flightsSlider)

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


  def loadFactions(inputFilename: String): Map[String, Color] = {
    val inputFile = new java.io.File(inputFilename)
    Try({
      val lines = FileUtils.readLines(inputFile)
      lines.asScala.map(line => {
        val splitted = line.split(",\\s+")
        val name = splitted(0)
        val color = new Color(
          splitted(1).toIntSafe(0),
          splitted(2).toIntSafe(0),
          splitted(3).toIntSafe(0)
        )
        (name, color)
      }).toMap
    }).getOrElse(Map())
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

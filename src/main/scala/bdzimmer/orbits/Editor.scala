// Copyright (c) 2018 Ben Zimmer. All rights reserved.

// Orbits Editor

package bdzimmer.orbits

import java.awt.{BorderLayout, Color, Dimension, FlowLayout, GridLayout, Graphics, Image, Font}
import java.awt.event._
import java.awt.image.BufferedImage

import javax.swing._
import javax.swing.event._

import scala.util.Try

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

  override def toString: String = {
    startDate.dateString + " - " + ship.name.replace("*", "") + " - " + origName + " - " + destName
  }

}


case class CameraControls(
    cameraType: JComboBox[String],
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
    flightsList: List[FlightParams],
    ships: List[Spacecraft]
  ) extends JFrame {

  // make mutable copy of flights list
  val flights: scala.collection.mutable.Buffer[FlightParams] = flightsList.toBuffer

  setTitle("Orbits Editor")

  val factions: Map[String, Color] = IO.loadFactions(Editor.FactionsFilename)

  /// /// image for view

  var imWidth: Int = Editor.ViewWidth
  var imHeight: Int = Editor.ViewHeight
  var im = new BufferedImage(imWidth, imHeight, BufferedImage.TYPE_INT_ARGB)
  var imagePanel = new ImagePanel(im)

  val redrawChangeListener: ChangeListener = new ChangeListener {
    def stateChanged(event: ChangeEvent): Unit = {
      redraw()
    }
  }

  val redrawActionListener: ActionListener = new ActionListener {
    def actionPerformed(event: ActionEvent): Unit = {
      redraw()
    }
  }

  /// /// build toolbars

  val toolbarsPanel = new JPanel(new BorderLayout())

  val toolbarRow0 = new JPanel(new FlowLayout(FlowLayout.LEFT))
  val (flightsToolbar, flightsComboBox, flightsSlider, getTimelineTime, timelineButton, rebuildFlights) = Editor.buildFlightsToolbar(
      flights, ships, redraw)
  toolbarRow0.add(flightsToolbar)
  toolbarRow0.add(
    Editor.buildExportToolbar(() => flights(flightsComboBox.getSelectedIndex)))
  toolbarsPanel.add(toolbarRow0, BorderLayout.NORTH)

  val toolbarRow1 = new JPanel(new FlowLayout(FlowLayout.LEFT))
  val (cameraToolbar, cameraControls) = Editor.buildCameraToolbar(imWidth, redrawChangeListener)
  cameraControls.cameraType.addActionListener(redrawActionListener)
  toolbarRow1.add(cameraToolbar)
  // toolbarRow1.add(Editor.buildUnitConverterToolbar())
  toolbarsPanel.add(toolbarRow1, BorderLayout.SOUTH)

  add(toolbarsPanel, BorderLayout.NORTH)

  /// /// build menu bar

  val (
    mainMenuBar,
    planetCheckboxes,
    lagrangePointsCheckBox,
    asteroidBeltCheckBox,
    flightStatusRadioButtons) = Editor.buildMenuBar(
    redrawChangeListener, redrawActionListener, flights, ships.map(x => (x.name, x)).toMap, rebuildFlights)

  setJMenuBar(mainMenuBar)

  /// /// build view panel

  val viewPanel = new JPanel()
  viewPanel.add(imagePanel)
  viewPanel.addMouseWheelListener(new MouseWheelListener() {
    def mouseWheelMoved(event: MouseWheelEvent): Unit = {
      val notches = event.getWheelRotation
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
      x = event.getX
      y = event.getY
      if (event.getButton == MouseEvent.BUTTON1) {
        cx = cameraControls.xAngleField.getValue.asInstanceOf[Double]
        cy = cameraControls.yAngleField.getValue.asInstanceOf[Double]
      } else {
        cx = cameraControls.xPosField.getValue.asInstanceOf[Double]
        cy = cameraControls.yPosField.getValue.asInstanceOf[Double]
      }
    }

    override def mouseDragged(event: MouseEvent): Unit = {
      val dx = event.getX - x
      val dy = event.getY - y
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

  toFront()
  setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  setResizable(true)
  setVisible(true)

  /// ///


  def redraw(): Unit = {

    // find selected planets
    // TODO: update this when changing planet checkboxes
    val planets = planetCheckboxes.toList.filter(_._2._1.isSelected).map(x => {
      (x._1, x._2._2)
    })


    if (timelineButton.isSelected) {

      // timeline mode

      val curDateJulian = getTimelineTime()
      val activeFlights = flights.filter(x => curDateJulian > x.startDate.julian && curDateJulian < x.endDate.julian)
      val fpOption = activeFlights.reduceOption((x, y) => if (x.startDate.julian < y.startDate.julian) x else y)

      Draw.redraw(
        fpOption,
        curDateJulian,
        planets,
        flights.toList,
        factions,
        asteroidBeltCheckBox.isSelected,
        lagrangePointsCheckBox.isSelected,
        flightStatusRadioButtons.indexWhere(_.isSelected),
        getCamera,
        getViewPos,
        im
      )

    } else {

      // individual flight mode

      val idx = flightsComboBox.getSelectedIndex
      val fp = flights(idx)
      val flightPercent = flightsSlider.getValue / 100.0
      val curDateJulian = fp.startDate.julian + (fp.endDate.julian - fp.startDate.julian) * flightPercent


      // get camera and viewer position
      val (camTrans, viewPos) = cameraControls.cameraType.getSelectedItem.asInstanceOf[String] match {
        case "Manual" => (getCamera, getViewPos)
        case _        => {

          val (flightFn, _) = Editor.paramsToFun(fp)
          val curState = flightFn(curDateJulian)

          // camera placed at 2 AU directly above the sun
          // val camPos = Vec3(0, 0, 2)
          val camPos = getCamPos
          val viewPos = getViewPos

          val camToShip = Vec2(curState.x - camPos.x, curState.y - camPos.y)
          val camAngleX = math.atan2(curState.z - camPos.z, Vec2.length(camToShip))
          val camAngleZ = math.atan2(camToShip.x, camToShip.y)

          val camOrient = Vec3(-math.Pi * 0.5 - camAngleX, 0.0, math.Pi - camAngleZ)
          val camRot = View.rotationZYX(camOrient)

          val camTrans = View.cameraTransform(camRot, camPos)
          // val xshiftAmount =  -imWidth * 0.1
          // val viewPos = Vec3(xshiftAmount, 0, imWidth * 1.0)

          (camTrans, viewPos)

        }
      }



      Draw.redraw(
        Some(fp),
        curDateJulian,
        planets,
        flights.toList,
        factions,
        asteroidBeltCheckBox.isSelected,
        lagrangePointsCheckBox.isSelected,
        flightStatusRadioButtons.indexWhere(_.isSelected),
        camTrans,
        viewPos,
        im
      )

    }

    imagePanel.repaint()
    System.out.print(".")
  }

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


  // get camera matrix from UI
  private def getCamera: Mat44 = {

    val xAngle = cameraControls.xAngleField.getValue.asInstanceOf[Double] * math.Pi / 180
    val yAngle = cameraControls.yAngleField.getValue.asInstanceOf[Double] * math.Pi / 180
    val zAngle = cameraControls.zAngleField.getValue.asInstanceOf[Double] * math.Pi / 180
    val theta = Vec3(xAngle, yAngle, zAngle)

    val camRotation = if (!cameraControls.isIntrinsic.isSelected()) {
      View.rotationXYZ(theta)
    } else {
      View.rotationZYX(theta)
    }

    View.cameraTransform(camRotation, getCamPos)

  }

  private def getCamPos: Vec3 = {
    val xPos = cameraControls.xPosField.getValue.asInstanceOf[Double]
    val yPos = cameraControls.yPosField.getValue.asInstanceOf[Double]
    val zPos = cameraControls.zPosField.getValue.asInstanceOf[Double]
    Vec3(xPos, yPos, zPos)
  }


  // get viewer position from UI
  private def getViewPos: Vec3 = {
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
  val ExportFilename = "flights_export"


  def paramsToFun(fp: FlightParams): (FlightFn, scala.collection.immutable.Seq[Double]) = {

    val startDateJulian = fp.startDate.julian
    val endDateJulian = fp.endDate.julian


    val res = if ((endDateJulian - startDateJulian) > 1.0) {
      // one tick per hour
      1.0 / 24.0
    } else {
      // one tick per minute
      1.0 / 24.0 / 60.0
    }
    val ticks = (startDateJulian until endDateJulian by res).toList.toIndexedSeq // don't ask

    // find positions of origin and destination bodies
    val origState = Orbits.planetState(fp.orig, startDateJulian)
    val destState = Orbits.planetState(fp.dest, endDateJulian)

    val flightFn = fp.ship match {
      case _: ConstAccelCraft => ConstAccelFlightFn(
        origState.position, destState.position,
        startDateJulian, endDateJulian - startDateJulian)
      case _: ConstVelCraft => ConstVelFlightFn(
        origState.position, destState.position,
        startDateJulian, endDateJulian - startDateJulian
      )
    }

    (flightFn, ticks)
  }


  def buildMenuBar(
      redrawChangeListener: ChangeListener,
      redrawActionListener: ActionListener,
      flights: scala.collection.mutable.Buffer[FlightParams],
      ships: Map[String, Spacecraft],
      rebuildFlights: () => Unit): (
    JMenuBar,
    scala.collection.immutable.ListMap[String, (JCheckBoxMenuItem, OrbitalElementsEstimator)],
    JCheckBoxMenuItem,
    JCheckBoxMenuItem,
    List[JRadioButtonMenuItem]) = {

    val menuBar = new JMenuBar()

    val fileMenu = new JMenu("File")
    val reloadItem = new JMenuItem("Reload")
    val exportItem = new JMenuItem("Export")
    val importItem = new JMenuItem("Import")
    val exitItem = new JMenuItem("Exit")

    reloadItem.addActionListener(new ActionListener() {
      override def actionPerformed(event: ActionEvent): Unit = {
        // TODO: implement
      }
    })

    exportItem.addActionListener(new ActionListener() {
      override def actionPerformed(event: ActionEvent): Unit = {
        // TODO: add date
        val outputPrefix = ExportFilename

        IO.saveFlightsSec(flights, outputPrefix + ".sec")
        IO.saveFlightsTsv(flights, outputPrefix + ".tsv")
      }
    })

    importItem.addActionListener(new ActionListener() {
      override def actionPerformed(event: ActionEvent): Unit = {
        // TODO: get filename from dialog box
        val inputFilename = ExportFilename + ".tsv"
        val flightsLoaded = IO.loadFlightsTsv(inputFilename, ships)
        flights.clear()
        flightsLoaded.foreach(x => flights.append(x))
        rebuildFlights()
        redrawActionListener.actionPerformed(event) // TODO: awkward

      }
    })

    exitItem.addActionListener(new ActionListener() {
      override def actionPerformed(event: ActionEvent): Unit = {
        sys.exit()
      }
    })

    fileMenu.add(reloadItem)
    fileMenu.add(new JSeparator(SwingConstants.HORIZONTAL))
    fileMenu.add(exportItem)
    fileMenu.add(importItem)
    fileMenu.add(new JSeparator(SwingConstants.HORIZONTAL))
    fileMenu.add(exitItem)

    /// /// ///

    val viewMenu = new JMenu("View")

    val planetCheckBoxes = MeeusPlanets.Planets.map(x => (x._1, (new JCheckBoxMenuItem(x._1, false), x._2)))
    InitialVisiblePlanets.foreach(x => planetCheckBoxes.get(x).foreach(_._1.setSelected(true)))
    planetCheckBoxes.foreach(x => {
      x._2._1.addChangeListener(redrawChangeListener)
    })
    planetCheckBoxes.foreach(x => viewMenu.add(x._2._1))
    // TODO: sections for toggling inner and outer planets
    viewMenu.add(new JSeparator(SwingConstants.HORIZONTAL))

    val lagrangePointsCheckBox = new JCheckBoxMenuItem("Lagrange Points", true)
    lagrangePointsCheckBox.addChangeListener(redrawChangeListener)
    viewMenu.add(lagrangePointsCheckBox)
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

    (menuBar, planetCheckBoxes, lagrangePointsCheckBox, asteroidBeltCheckBox, flightStatusRadioButtons)
  }


  def buildFlightsToolbar(
      flights: scala.collection.mutable.Buffer[FlightParams],
      ships: List[Spacecraft],
      redraw: () => Unit): (JToolBar, JComboBox[String], JSlider, () => Double, JToggleButton, () => Unit) = {

    val toolbar = new JToolBar()
    toolbar.setLayout(new FlowLayout(FlowLayout.LEFT))
    toolbar.setBorder(BorderFactory.createTitledBorder(toolbar.getBorder, "Flights"))

    // TODO: set maximum size
    val flightsComboBoxModel = new DefaultComboBoxModel(
      flights.zipWithIndex.map(x => (x._2 + 1) + ". " + x._1.toString).toArray)
    val flightsComboBox = new JComboBox(flightsComboBoxModel)
    toolbar.add(flightsComboBox)

    val flightsSlider = new JSlider(SwingConstants.HORIZONTAL, 1, 100, 100)
    flightsSlider.addChangeListener(new ChangeListener {
      def stateChanged(event: ChangeEvent): Unit = {
        redraw()
      }
    })
    toolbar.add(flightsSlider)


    /// edit flights JFrame

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
      // build a new flightparams from the UI

      val origName = startLocComboBox.getSelectedItem.asInstanceOf[String]
      val destName = endLocComboBox.getSelectedItem.asInstanceOf[String]

      val idx = flightsComboBox.getSelectedIndex

      val startDate = DateTime.parse(startDateText.getText)
      val endDate = DateTime.parse(endDateText.getText)

      // TODO: come up with a more robust way of telling if CalendarDateTime parsing failed

      if (startDate.year > 0 && endDate.year > 0) {

        val fp = flights(idx).copy(
          ship = ships(shipsComboBox.getSelectedIndex),
          origName = origName,
          destName = destName,
          orig = MeeusPlanets.Planets.getOrElse(origName, MeeusPlanets.Earth),
          dest = MeeusPlanets.Planets.getOrElse(destName, MeeusPlanets.Earth),
          startDate = startDate,
          endDate = endDate,
          passengers = List()
        )

        flights(idx) = fp
        // println(fp, DateTime.parse(startDateText.getText).julian, DateTime.parse(endDateText.getText).julian)

        Disable(flightsComboBox, {
          flightsComboBox.removeItemAt(idx)
          flightsComboBox.insertItemAt((idx + 1) + ". " + fp.toString, idx)
          flightsComboBox.setSelectedIndex(idx)
          toolbar.revalidate()
          toolbar.repaint()
        })

      }
    }

    def rebuildFlights(): Unit = {
      Disable(flightsComboBox, {
        // println("rebuilding flights combobox")
        flightsComboBoxModel.removeAllElements()
        flights.zipWithIndex.foreach(
          x => flightsComboBoxModel.addElement((x._2 + 1) + ". " + x._1.toString))
        toolbar.revalidate()
        toolbar.repaint()
      })
    }

    val updateActionListener = new ActionListener {
      var enabled = true
      override def actionPerformed(event: ActionEvent): Unit = {
        if (enabled) {
          update()
          redraw()
        }
      }
    }

    val updateDocumentListener = new DocumentListener {
      var enabled = true
      override def changedUpdate(event: DocumentEvent): Unit = {}
      override def insertUpdate(event: DocumentEvent): Unit = {
        if (enabled) {
          update()
          redraw()
        }
      }
      override def removeUpdate(event: DocumentEvent): Unit = {
        if (enabled) {
          update()
          redraw()
        }
      }
    }

    def updateFlightEditFields(): Unit = {
      updateActionListener.enabled = false
      updateDocumentListener.enabled = false
      val idx = flightsComboBox.getSelectedIndex
      // println("updating flight fields:" + idx)
      val fp = flights(idx)
      shipsComboBox.setSelectedIndex(ships.indexOf(fp.ship))
      startLocComboBox.setSelectedIndex(planetsList.indexOf(fp.origName)) // TODO: this isn't quite right
      endLocComboBox.setSelectedIndex(planetsList.indexOf(fp.destName))
      startDateText.setText(fp.startDate.dateTimeString)
      endDateText.setText(fp.endDate.dateTimeString)
      updateActionListener.enabled = true
      updateDocumentListener.enabled = true
    }

    val flightsActionListener = new DisableableActionListener(_ => {
      updateFlightEditFields()
      redraw()
    })

    flightsComboBox.addActionListener(flightsActionListener)
    updateFlightEditFields()

    shipsComboBox.addActionListener(updateActionListener)
    startLocComboBox.addActionListener(updateActionListener)
    endLocComboBox.addActionListener(updateActionListener)
    startDateText.getDocument.addDocumentListener(updateDocumentListener)
    endDateText.getDocument.addDocumentListener(updateDocumentListener)


    val newButton = new JButton("New")
    newButton.addActionListener(new ActionListener {
      override def actionPerformed(event: ActionEvent): Unit = {
        Disable(flightsComboBox, {
          val idx = flightsComboBox.getSelectedIndex
          val fp = flights(idx)
          flights.insert(idx + 1, fp)
          // println("new flight; set selected index: " + (idx + 1))
          flightsComboBox.setSelectedIndex(idx + 1)
          // println("new flight; get selected index: " + flightsComboBox.getSelectedIndex)
        })
        rebuildFlights()
        updateFlightEditFields()
        toolbar.repaint()
      }
    })

    val deleteButton = new JButton("Delete")
    deleteButton.addActionListener(new ActionListener {
      def actionPerformed(event: ActionEvent): Unit = {
        val idx = flightsComboBox.getSelectedIndex
        Disable(flightsComboBox, {
          flights.remove(idx)
        })
        rebuildFlights()
        if (flights.nonEmpty) {
          if (idx < flights.length) {
            flightsComboBox.setSelectedIndex(idx)
          } else {
            flightsComboBox.setSelectedIndex(idx - 1)
          }
        }
        toolbar.repaint()
      }
    })

    val solveStartButton = new JButton("Solve Start")
    solveStartButton.addActionListener(new ActionListener {
      override def actionPerformed(event: ActionEvent): Unit = {
        val fp = flights(flightsComboBox.getSelectedIndex)
        val endDate = fp.endDate.julian
        val startDate = SolveFlight.startDate(
          fp.ship,
          t => Orbits.planetState(fp.orig, t).position,
          Orbits.planetState(fp.dest, endDate).position,
          endDate)
        println(startDate)
        startDate.foreach(t => println(Conversions.julianToCalendarDate(t).dateTimeString))
        startDate.foreach(t => startDateText.setText(Conversions.julianToCalendarDate(t).dateTimeString))

      }
    })

    val solveEndButton = new JButton("Solve End")
    solveEndButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val fp = flights(flightsComboBox.getSelectedIndex)
        val startDate = fp.startDate.julian
        val endDate = SolveFlight.endDate(
          fp.ship,
          Orbits.planetState(fp.orig, startDate).position,
          t => Orbits.planetState(fp.dest, t).position,
          startDate)
        println(endDate)
        endDate.foreach(t => println(Conversions.julianToCalendarDate(t).dateTimeString))
        endDate.foreach(t => endDateText.setText(Conversions.julianToCalendarDate(t).dateTimeString))
      }
    })

    /// /// flight edit window

    val flightEditWindow = new JFrame()

    val emptyBorder = BorderFactory.createEmptyBorder(10, 10, 10, 10)

    val labelsPanel = new JPanel(new GridLayout(6, 1))
    List("Ship:", "Start location:", "Start date:", "End location:", "End date:", "").foreach(x =>
      labelsPanel.add(new JLabel(x))
    )
    labelsPanel.setBorder(emptyBorder)
    flightEditWindow.add(labelsPanel, BorderLayout.WEST)

    val buttonsPanel = new JPanel(new GridLayout(1, 4))
    List(newButton, deleteButton, solveStartButton, solveEndButton).foreach(x => buttonsPanel.add(x))

    val controlsPanel = new JPanel(new GridLayout(6, 1))
    List(shipsComboBox, startLocComboBox, startDateText, endLocComboBox, endDateText, buttonsPanel).foreach(x =>
      controlsPanel.add(x)
    )
    controlsPanel.setBorder(emptyBorder)
    flightEditWindow.add(controlsPanel, BorderLayout.EAST)

    flightEditWindow.setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE)
    flightEditWindow.setAlwaysOnTop(true)
    flightEditWindow.setTitle("Edit Flight")
    flightEditWindow.pack()

    val editButton = new JToggleButton("Edit")
    editButton.addActionListener(new ActionListener {
      override def actionPerformed(event: ActionEvent): Unit = {
        flightEditWindow.setVisible(editButton.isSelected)
      }
    })
    toolbar.add(editButton)

    /// /// timeline window

    val timelineWindow = new JFrame()

    var timelineTime: Double = 0.0 // yep
    var runAtIntervalThread: Thread = null

    val allPanel = new JPanel(new GridLayout(3, 1))

    val timelineDateTimeText = new JTextField("", 19)
    timelineDateTimeText.setFont(new Font("monospaced", Font.BOLD, 48))
    timelineDateTimeText.setBackground(Color.BLACK)
    timelineDateTimeText.setForeground(Color.GREEN)
    timelineDateTimeText.setMaximumSize(timelineDateTimeText.getPreferredSize)

    def updateTimelineTime(newTime: Double): Unit = {
      timelineTime = newTime
      timelineDateTimeText.setText(Conversions.julianToCalendarDate(timelineTime).dateTimeString)
    }

    allPanel.add(timelineDateTimeText)

    val sliderMax: Int = 1000
    val timelineSlider = new JSlider(SwingConstants.HORIZONTAL, 1, sliderMax, sliderMax)
    timelineSlider.setMajorTickSpacing(100)
    timelineSlider.setMinorTickSpacing(20)
    timelineSlider.setPaintTicks(true)
    allPanel.add(timelineSlider)

    val skipPanel = new JPanel(new GridLayout(1, 7))
    allPanel.add(skipPanel)

    val delayMsText = new JTextField("50", 4)
    delayMsText.setMaximumSize(delayMsText.getPreferredSize)

    // TODO: eventually, calculate this whenever flights changes
    // TODO: unsafe if flights empty
    def dateRange(): (Double, Double) = {
      val startDate = flights.map(_.startDate.julian).min
      val endDate = flights.map(_.endDate.julian).max
      (startDate, endDate)
    }

    // day, hour, minute
    val skipAmounts = List(-1.0, -1.0 / 24.0, -1.0 / 1440.0, 1.0 / 1440.0, 1.0 / 24.0, 1.0)
    val skipLabels = List("<<<", "<<", "<", ">", ">>", ">>>")
    skipAmounts.zip(skipLabels).foreach({case (skipAmount, skipLabel) => {
      val skipButton = new JButton(skipLabel)

      // skipButton.addActionListener(new ActionListener {
      //   override def actionPerformed(e: ActionEvent): Unit = {
      //     updateTimelineTime(timelineTime + skipAmount)
      //     redraw()
      //   }
      // })

      skipButton.addMouseListener(new MouseAdapter() {
        override def mousePressed(e: MouseEvent): Unit = {
          runAtIntervalThread = new Thread(new RunAtInterval(() => {
            // println("updating in thread")
            val newTime = timelineTime + skipAmount
            updateTimelineTime(newTime)
            // update slider position
            Disable(
              timelineSlider,
              {
                val (startDate, endDate) = dateRange()
                val sliderPercent = (timelineTime - startDate) / (endDate - startDate)
                timelineSlider.setValue((sliderPercent * sliderMax).toInt)
              }
            )
            redraw()
          }, delayMsText.getText.toIntSafe(50) / 1000.0))
          runAtIntervalThread.start()
        }
        override def mouseReleased(e: MouseEvent): Unit = {
          // could potentially update slider position here
          if (runAtIntervalThread != null) {
            runAtIntervalThread.interrupt()
          }
        }
      })
      skipPanel.add(skipButton)
    }})
    skipPanel.add(delayMsText)

    timelineSlider.addChangeListener(new DisableableChangeListener(_ => {
      // println("fired timelineSlider stateChanged")
      val (startDate, endDate) = dateRange()
      val sliderPercent = timelineSlider.getValue / sliderMax.toDouble
      updateTimelineTime(startDate + (endDate - startDate) * sliderPercent)
      redraw()
    }))
    updateTimelineTime(flights.map(_.startDate.julian).min)

    timelineWindow.add(allPanel)

    timelineWindow.setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE)
    timelineWindow.setAlwaysOnTop(true)
    timelineWindow.setTitle("Timeline")
    timelineWindow.pack()

    val timelineButton = new JToggleButton("Timeline")
    timelineButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        timelineWindow.setVisible(timelineButton.isSelected)
      }
    })
    toolbar.add(timelineButton)

    /// ///

    (toolbar, flightsComboBox, flightsSlider, () => timelineTime, timelineButton, rebuildFlights)

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

    val auPerDaySqListener: DocumentListener = new DocumentListener {
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

    val gListener: DocumentListener = new DocumentListener {
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

    val cameraType = new JComboBox[String](List("Manual", "Follow Active").toArray)

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

    toolbar.add(cameraType)
    toolbar.add(new JSeparator(SwingConstants.VERTICAL))
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
        cameraType,
        xAngleField, yAngleField, zAngleField, isIntrinsic,
        xPosField, yPosField, zPosField, zViewPosField))

  }



  def buildExportToolbar(getCurrentFlight: () => FlightParams): JToolBar = {

    val toolbar = new JToolBar()
    toolbar.setLayout(new FlowLayout(FlowLayout.LEFT))
    toolbar.setBorder(BorderFactory.createTitledBorder(toolbar.getBorder, "Unit Converter"))

    val exportButton = new JButton("Export")

    exportButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {

        val flight = getCurrentFlight()

        val outputDir = new java.io.File("temp")
        outputDir.mkdirs()

        RenderFlight.animateFlight(
          flight.ship,
          flight.origName, flight.destName,
          flight.orig, flight.dest,
          flight.startDate, flight.endDate,
          outputDir.getAbsolutePath)

        val outputFile = new java.io.File("temp.mp4")
        RenderFlight.imagesToVideo(
          outputDir.getAbsolutePath, outputFile.getAbsolutePath, 800, 600)

      }
    })

    toolbar.add(exportButton)
    toolbar
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

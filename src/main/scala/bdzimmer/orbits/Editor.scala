// Copyright (c) 2018 Ben Zimmer. All rights reserved.

// Orbits Editor

package bdzimmer.orbits

import java.awt.{BorderLayout, Color, FlowLayout, Font, GridLayout}
import java.awt.event._

import bdzimmer.orbits.InteractiveView.InitialVisiblePlanets
import javax.swing._
import javax.swing.event._

import scala.util.Try
import bdzimmer.util.StringUtils._

import scala.collection.immutable.Seq
import scala.collection.mutable.{Buffer => MutableBuffer}


class Editor(
    flightsList: List[FlightParams],
    ships: List[Spacecraft],
    epochs: List[(String, Double, Double)],
    styles: Map[String, ViewerSettings],
    factions: Map[String, Color]
  ) {

  val showSettings = Editor.ShowSettingsDefault.copy()
  var viewerSettings = Style.ViewerSettingsDefault

  // make mutable copy of flights list
  val flights: MutableBuffer[FlightParams] = flightsList.toBuffer

  /// /// build toolbars

  val (
      flightsToolbar, flightsComboBox,
      flightsSlider, getTimelineTime, timelineButton, skipButtons, rebuildFlights) = Editor.buildFlightsToolbar(
      flights, ships, epochs, redraw)

  /// /// build menu bar

  val (mainMenuBar, fpsCheckBox) = Editor.buildMenuBar(
    showSettings, redraw, flights,
    ships.map(x => (x.name, x)).toMap,
    rebuildFlights,
    styles,
    x => {viewerSettings = x}) // TODO: if this works, redo camerasettings like this

  /// /// Create base InteractiveView to modify

  val iv = new InteractiveView(
    "Orbits Edtior",
    getCurDateJulian,
    () => timelineButton.isSelected,
    getVisiblePlanets,
    () => flights,
    () => flights(flightsComboBox.getSelectedIndex),
    () => factions,
    () => fpsCheckBox.isSelected,
    () => !timelineButton.isSelected || skipButtons.getSelection == null,
    showSettings,
    viewerSettings
  )

  val ivs = MutableBuffer[InteractiveView]()
  ivs += iv

  /// /// add additional toolbars


  val toolbarRow0 = new JPanel(new FlowLayout(FlowLayout.LEFT))
  toolbarRow0.add(flightsToolbar)
  toolbarRow0.add(
    InteractiveView.buildExportToolbar(iv.redrawGeneric))
  toolbarRow0.add(
    buildAddViewToolbar(ivs)
  )

  iv.toolbarsPanel.removeAll()

  iv.toolbarsPanel.add(toolbarRow0, BorderLayout.NORTH)
  iv.toolbarsPanel.add(iv.cameraToolbar, BorderLayout.SOUTH)

  iv.setJMenuBar(mainMenuBar)


  /// ///

  def getVisiblePlanets(): Seq[(String, Planet)] = {
    showSettings.planets.filter(_._2).flatMap(
      x => MeeusPlanets.Planets.get(x._1).map(y => (x._1, y))).toList
  }


  def getCurDateJulian(): Double = {

    if (timelineButton.isSelected) {
      getTimelineTime()
    } else {
      val idx = flightsComboBox.getSelectedIndex
      val fp = flights(idx)
      val flightPercent = flightsSlider.getValue / 100.0
      fp.startDate.julian + (fp.endDate.julian - fp.startDate.julian) * flightPercent
    }

  }


  def redraw(): Unit = {
    ivs.foreach(_.redraw())
  }


  def buildAddViewToolbar(
     ivs: MutableBuffer[InteractiveView]): JToolBar = {

    val toolbar = new JToolBar()
    toolbar.setLayout(new FlowLayout(FlowLayout.LEFT))
    toolbar.setBorder(BorderFactory.createTitledBorder(toolbar.getBorder, "Views"))

    val addButton = new JButton("New")
    addButton.addActionListener(new ActionListener {
      override def actionPerformed(actionEvent: ActionEvent): Unit = {

        val iv = new InteractiveView(
          "View " + (ivs.length + 1),
          getCurDateJulian,
          () => timelineButton.isSelected,
          getVisiblePlanets,
          () => flights,
          () => flights(flightsComboBox.getSelectedIndex),
          () => factions,
          () => fpsCheckBox.isSelected,
          () => !timelineButton.isSelected || skipButtons.getSelection == null,
          showSettings,
          viewerSettings
        )

        iv.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
        iv.addWindowListener(new WindowAdapter {
          override def windowClosed(windowEvent: WindowEvent): Unit = {
            super.windowClosed(windowEvent)
            val idx = ivs.indexOf(iv)
            ivs.remove(idx)
            println("total interactive views: " + ivs.length)
          }
        })

        ivs += iv

        println("total interactive views: " + ivs.length)

      }
    })

    toolbar.add(addButton)
    toolbar
  }





}



object Editor {


  val FactionsFilename = "orbits_factions.txt"
  val StylesFilename = "orbits_styles.txt"

  val ExportFilename = "flights_export"


  val ShowSettingsDefault = ShowSettings(
    planets = MeeusPlanets.Planets.map(x => (x._1, InitialVisiblePlanets.contains(x._1))),
    lagrangePoints = true, // TODO: false
    asteroidBelt = true,
    orbitInfo = false,
    motionVerticals = false,
    flightStatus = 1
  )

  def buildMenuBar(
      showSettings: ShowSettings,
      redraw: () => Unit,
      flights: MutableBuffer[FlightParams],
      ships: Map[String, Spacecraft],
      rebuildFlights: () => Unit,
      styles: Map[String, ViewerSettings],
      updateViewerSettings: ViewerSettings => Unit): (JMenuBar, JCheckBoxMenuItem) = {

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

        // TODO: filter and save only simple flights for now
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
        redraw()
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

    showSettings.planets.foreach(x => planetCheckBoxes.get(x._1).foreach(_._1.setSelected(x._2)))

    planetCheckBoxes.foreach(x => {
      x._2._1.addItemListener(new ItemListener {
        def itemStateChanged(event: ItemEvent): Unit = {
          println("updating visibility of " + x._1)
          showSettings.planets = showSettings.planets + (x._1 -> x._2._1.isSelected)
          redraw()
        }
      })
    })
    planetCheckBoxes.foreach(x => viewMenu.add(x._2._1))

    // TODO: sections for toggling inner and outer planets
    viewMenu.add(new JSeparator(SwingConstants.HORIZONTAL))

    val lagrangePointsCheckBox = new JCheckBoxMenuItem("Lagrange Points", showSettings.lagrangePoints)
    lagrangePointsCheckBox.addItemListener(new ItemListener {
      override def itemStateChanged(e: ItemEvent): Unit = {
        showSettings.lagrangePoints = lagrangePointsCheckBox.isSelected
        redraw()
      }
    })
    viewMenu.add(lagrangePointsCheckBox)

    val asteroidBeltCheckBox = new JCheckBoxMenuItem("Asteroid Belt", showSettings.asteroidBelt)
    asteroidBeltCheckBox.addItemListener(new ItemListener {
      override def itemStateChanged(e: ItemEvent): Unit = {
        showSettings.asteroidBelt = asteroidBeltCheckBox.isSelected
        redraw()
      }
    })
    viewMenu.add(asteroidBeltCheckBox)

    val orbitInfoCheckBox = new JCheckBoxMenuItem("Orbit Info", showSettings.orbitInfo)
    orbitInfoCheckBox.addItemListener(new ItemListener {
      override def itemStateChanged(e: ItemEvent): Unit = {
        showSettings.orbitInfo = orbitInfoCheckBox.isSelected
        redraw()
      }
    })
    viewMenu.add(orbitInfoCheckBox)

    val verticalsCheckBox = new JCheckBoxMenuItem("Verticals", showSettings.motionVerticals)
    verticalsCheckBox.addItemListener(new ItemListener {
      override def itemStateChanged(e: ItemEvent): Unit = {
        showSettings.motionVerticals = verticalsCheckBox.isSelected
        redraw()
      }
    })
    viewMenu.add(verticalsCheckBox)

    viewMenu.add(new JSeparator(SwingConstants.HORIZONTAL))

    // TODO: do this with a single foreach
    val flightStatusButtonGroup = new ButtonGroup()
    val flightStatusRadioButtons = List("Status", "Summary", "None").map(x => new JRadioButtonMenuItem(x))
    flightStatusRadioButtons(showSettings.flightStatus).setSelected(true)
    flightStatusRadioButtons.foreach(x => x.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        showSettings.flightStatus = flightStatusRadioButtons.indexWhere(_.isSelected)
        redraw()
      }
    }))
    flightStatusRadioButtons.foreach(flightStatusButtonGroup.add)
    flightStatusRadioButtons.foreach(viewMenu.add)

    viewMenu.add(new JSeparator(SwingConstants.HORIZONTAL))

    val fpsCheckBox = new JCheckBoxMenuItem("FPS", false)
    viewMenu.add(fpsCheckBox)

    // ~~~~

    val styleMenu = new JMenu("Style")
    val styleButtonGroup = new ButtonGroup()
    val stylesList = ("Default", Style.ViewerSettingsDefault) :: styles.toList
    stylesList.zipWithIndex.foreach({ case ((key, value), idx) => {
      val button = new JRadioButtonMenuItem(key)
      button.addActionListener(new ActionListener {
        override def actionPerformed(e: ActionEvent): Unit = {
          updateViewerSettings(value)
          redraw()
        }
      })
      if (idx == 0) {
        button.setSelected(true)
      }
      styleButtonGroup.add(button)
      styleMenu.add(button)
    }
    })

    // ~~~~

    menuBar.add(fileMenu)
    menuBar.add(viewMenu)
    menuBar.add(styleMenu)

    (menuBar, fpsCheckBox)
  }


  def buildFlightsToolbar(
      flights: MutableBuffer[FlightParams],
      ships: List[Spacecraft],
      epochs: List[(String, Double, Double)],
      redraw: () => Unit): (JToolBar, JComboBox[String], JSlider, () => Double,
      JToggleButton, ClearableButtonGroup, () => Unit) = {

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

    // val planetsList = MeeusPlanets.Planets.keys.toList
    // val planetsArray = planetsList.toArray
    val locsArray = Locations.All.toArray

    val startLocComboBox = new JComboBox(locsArray)

    val startDateText = new JTextField("", 12)
    startDateText.setMaximumSize(startDateText.getPreferredSize)

    val endLocComboBox = new JComboBox(locsArray)

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

        flights(idx) match {
          case simpleFight: SimpleFlightParams => {

            val fp = simpleFight.copy(
              ship = ships(shipsComboBox.getSelectedIndex),
              origName = origName,
              destName = destName,
              orig = Locations.StatesMap.getOrElse(origName, Locations.DefaultFun),
              dest = Locations.StatesMap.getOrElse(destName, Locations.DefaultFun),
              startDate = startDate,
              endDate = endDate,
              passengers = List()
            )

            flights(idx) = simpleFight
            // println(fp, DateTime.parse(startDateText.getText).julian, DateTime.parse(endDateText.getText).julian)

            Disable(flightsComboBox, {
              flightsComboBox.removeItemAt(idx)
              flightsComboBox.insertItemAt((idx + 1) + ". " + fp.toString, idx)
              flightsComboBox.setSelectedIndex(idx)
              toolbar.revalidate()
              toolbar.repaint()
            })

          }
          case _ => {
            println("Can only edit simple flights.")
          }

        }

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
      startLocComboBox.setSelectedIndex(locsArray.indexOf(fp.origName)) // TODO: this isn't quite right
      endLocComboBox.setSelectedIndex(locsArray.indexOf(fp.destName))
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
          t => fp.orig(t).position,
          fp.dest(endDate).position,
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
          fp.orig(startDate).position,
          t => fp.dest(t).position,
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

    val allPanel = new JPanel(new GridLayout(6, 1))

    val timelineDateTimeText = new JTextField("", 19)
    timelineDateTimeText.setFont(FontUtil.font("monospaced", Font.BOLD, 48))
    timelineDateTimeText.setBackground(Color.BLACK)
    timelineDateTimeText.setForeground(Color.GREEN)
    timelineDateTimeText.setMaximumSize(timelineDateTimeText.getPreferredSize)

    allPanel.add(timelineDateTimeText)

    val sliderMax: Int = 1000
    val timelineSlider = new JSlider(SwingConstants.HORIZONTAL, 1, sliderMax, 1)
    timelineSlider.setMajorTickSpacing(100)
    timelineSlider.setMinorTickSpacing(20)
    timelineSlider.setPaintTicks(true)
    allPanel.add(timelineSlider)

    val minSlider = new JSlider(SwingConstants.HORIZONTAL, 1, sliderMax, 1)
    minSlider.setMajorTickSpacing(100)
    minSlider.setMinorTickSpacing(20)
    minSlider.setPaintTicks(true)
    allPanel.add(minSlider)

    val maxSlider = new JSlider(SwingConstants.HORIZONTAL, 1, sliderMax, sliderMax)
    maxSlider.setMajorTickSpacing(100)
    maxSlider.setMinorTickSpacing(20)
    maxSlider.setPaintTicks(true)
    allPanel.add(maxSlider)

    val skipPanel = new JPanel(new GridLayout(1, 7))
    allPanel.add(skipPanel)

    val delayMsText = new JTextField("33", 4)
    delayMsText.setMaximumSize(delayMsText.getPreferredSize)

    val epochsComboBox = new JComboBox[String](
      epochs.zipWithIndex.map({ case (x, idx) =>
        (idx + 1) + ". " +
          x._1 + " - " +
          Conversions.julianToCalendarDate(x._2).dateTimeString + " - " +
          Conversions.julianToCalendarDate(x._3).dateTimeString
      }).toArray)

    epochsComboBox.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        redraw()
      }
    })
    allPanel.add(epochsComboBox)

    // TODO: eventually, calculate this whenever flights changes
    // TODO: unsafe if flights empty
    def dateRange(): (Double, Double) = {
      //      val startDate = flights.map(_.startDate.julian).min
      //      val endDate = flights.map(_.endDate.julian).max
      //      (startDate, endDate)
      val epoch = epochs(epochsComboBox.getSelectedIndex)
      (epoch._2, epoch._3)
    }


    def updateTimelineTime(newTime: Double, startDate: Double, endDate: Double): Unit = {
      val timeMin = startDate + (endDate - startDate) * minSlider.getValue / sliderMax.toDouble
      val timeMax = startDate + (endDate - startDate) * maxSlider.getValue / sliderMax.toDouble
      if (newTime < timeMin) {
        timelineTime = timeMax // - (timeMin - newTime)
      } else if (newTime > timeMax) {
        timelineTime = timeMin // + (newTime - timeMax)
      } else {
        timelineTime = newTime
      }
      timelineDateTimeText.setText(Conversions.julianToCalendarDate(timelineTime).dateTimeString)
    }

    // day, hour, minute
    val skipAmounts = List(-1.0, -1.0 / 24.0, -1.0 / 1440.0, 1.0 / 1440.0, 1.0 / 24.0, 1.0)
    val skipLabels = List("<<<", "<<", "<", ">", ">>", ">>>")
    val skipButtons = new ClearableButtonGroup()
    skipAmounts.zip(skipLabels).foreach({ case (skipAmount, skipLabel) => {

      val skipButton = new JToggleButton(skipLabel)

      skipButton.addItemListener(new ItemListener {
        override def itemStateChanged(e: ItemEvent): Unit = {
          if (e.getStateChange == ItemEvent.SELECTED) {
            runAtIntervalThread = new Thread(new RunAtInterval(() => {
              // println("updating in thread")
              val newTime = timelineTime + skipAmount
              val (startDate, endDate) = dateRange()
              updateTimelineTime(newTime, startDate, endDate)
              // update slider position
              Disable(
                timelineSlider,
                {
                  val sliderPercent = (timelineTime - startDate) / (endDate - startDate)
                  timelineSlider.setValue((sliderPercent * sliderMax).toInt)
                }
              )
              redraw()
            }, delayMsText.getText.toIntSafe(50) / 1000.0))
            runAtIntervalThread.start()
          } else if (e.getStateChange == ItemEvent.DESELECTED) {
            // could potentially update slider position here
            if (runAtIntervalThread != null) {
              runAtIntervalThread.interrupt()
            }
          }

        }
      })

      skipButton.addActionListener(new ActionListener {
        override def actionPerformed(e: ActionEvent): Unit = {
          if (!skipButton.isSelected) {
            skipButtons.clearSelection()
          }
        }
      })

      skipButtons.add(skipButton)
      skipPanel.add(skipButton)
    }
    })

    skipPanel.add(delayMsText)

    timelineSlider.addChangeListener(new DisableableChangeListener(_ => {
      // println("fired timelineSlider stateChanged")
      val (startDate, endDate) = dateRange()
      val sliderPercent = timelineSlider.getValue / sliderMax.toDouble
      updateTimelineTime(startDate + (endDate - startDate) * sliderPercent, startDate, endDate)
      redraw()
    }))
    val (startDate, endDate) = dateRange()
    updateTimelineTime(flights.map(_.startDate.julian).min, startDate, endDate)

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

    (toolbar, flightsComboBox, flightsSlider, () => timelineTime, timelineButton, skipButtons, rebuildFlights)

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
            converterEnabled = true
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

}
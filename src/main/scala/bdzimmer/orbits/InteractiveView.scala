// Copyright (c) 2018 Ben Zimmer. All rights reserved.

// Orbits InteractiveView

package bdzimmer.orbits

import java.awt.{BorderLayout, Color, Dimension, FlowLayout, Font, Graphics, GridLayout, Image, Toolkit}
import java.awt.event._
import java.awt.image.BufferedImage
import java.text.SimpleDateFormat
import java.util.Calendar

import javax.swing._
import javax.swing.event._

import scala.util.Try
import org.apache.commons.imaging.{ImageFormats, Imaging}
import bdzimmer.util.StringUtils._

import scala.collection.immutable.Seq


// this is sort of a parallel version of how flights are represented in Secondary
case class FlightParams(
    ship: Spacecraft,
    origName: String,
    destName: String,
    // orig: OrbitalElementsEstimator,
    // dest: OrbitalElementsEstimator,
    orig: Double => OrbitalState,
    dest: Double => OrbitalState,
    startDate: CalendarDateTime,
    endDate: CalendarDateTime,
    passengers: List[String],
    faction: String,
    description: String) {

  override def toString: String = {
    startDate.dateString + " - " +
    ship.name.replace("*", "") + " - " +
    origName + " - " + destName  // TODO: use an arrow here
  }

}


case class CameraSettings(
    var cameraPosType: String,
    var cameraPointType: String,
    var xAngle: Double,
    var yAngle: Double,
    var zAngle: Double,
    var isIntrinsic: Boolean,
    var xPos: Double,   // TODO: make pos a Vec3
    var yPos: Double,
    var zPos: Double,
    var zViewPos: Double
)


case class UpdateCameraControls(
    setXAngle: Double => Unit,
    setYAngle: Double => Unit,
    setZAngle: Double => Unit,
    setXPos: Double => Unit,
    setYPos: Double => Unit,
    setZPos: Double => Unit,
    setZViewPos: Double => Unit
)


class InteractiveView(
    title: String,

    // allow the relevant stuff to controlled elsewhere
    getCurDateJulian: () => Double,
    getTimelineMode: () => Boolean,
    getPlanets: () => Seq[(String, Planet)],
    getFlights: () => scala.collection.mutable.Buffer[FlightParams], // because reasons
    getFlight: () => FlightParams,
    getFactions: () => Map[String, Color],
    getFpsEnabled: () => Boolean,
    getCameraControlsEnabled: () => Boolean,

    // mutable settings (these should only be read though)
    showSettings: ShowSettings,
    viewerSettings: ViewerSettings

  ) extends JFrame {

  Toolkit.getDefaultToolkit().setDynamicLayout(false)

  // val showSettings = InteractiveView.ShowSettingsDefault.copy()
  val cameraSettings = InteractiveView.CameraSettingsDefault.copy()
  // var viewerSettings = Style.ViewerSettingsDefault

  var prevPos = Vec3(0.0, 0.0, 0.0)

  // for clicking on objects
  // ugh
  var selectedObjects = scala.collection.mutable.Map[String, (Vec2, Boolean)]()

  /// /// image for view

  var imWidth: Int = InteractiveView.ViewWidth
  var imHeight: Int = InteractiveView.ViewHeight
  var im = new BufferedImage(imWidth, imHeight, BufferedImage.TYPE_INT_ARGB)
  var imagePanel = new ImagePanel(im)

  /// /// build toolbars

  val toolbarsPanel = new JPanel(new BorderLayout())

  val toolbarRow0 = new JPanel(new FlowLayout(FlowLayout.LEFT))

  val (cameraToolbar, updateCameraControls) = InteractiveView.buildCameraToolbar(
    cameraSettings,
    // () => if (!timelineButton.isSelected || skipButtons.getSelection() == null) redraw()
    () => if (getCameraControlsEnabled()) redraw()
  )

  toolbarRow0.add(cameraToolbar)
  toolbarsPanel.add(toolbarRow0, BorderLayout.NORTH)
  add(toolbarsPanel, BorderLayout.NORTH)

  /// /// build view panel

  val viewPanel = new JPanel()
  viewPanel.add(imagePanel)
  viewPanel.addMouseWheelListener(new MouseWheelListener() {
    def mouseWheelMoved(event: MouseWheelEvent): Unit = {
      val notches = event.getWheelRotation
      val multiplier = if (event.isControlDown) { 100.0 } else {1.0}
      val zViewPos = cameraSettings.zViewPos - notches * InteractiveView.ZoomSpeed * multiplier
      updateCameraControls.setZViewPos(math.max(zViewPos, 0.0))
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
        cx = cameraSettings.xAngle
        cy = cameraSettings.yAngle

        // look for stuff to click on

        // look for matches in objects and update
        val tx = x - im.getWidth / 2
        val ty = (im.getHeight - y) - im.getHeight / 2
        var found = false
        selectedObjects.foreach({case (name, (pos, selected)) => {
          if ((pos.x - tx).abs < 32 && (pos.y - ty).abs < 32) {
            selectedObjects(name) = (pos, !selected)
            println(name + " " + !selected)
            found = true
          }
        }})
        if (found) {
          redraw()
        }

      } else {
        cx = cameraSettings.xPos
        cy = cameraSettings.yPos
      }
    }

    override def mouseDragged(event: MouseEvent): Unit = {
      val dx = event.getX - x
      val dy = event.getY - y
      // println(dx + " " + dy)
      // TODO: adjust and rotate direction based on camera angle
      if ((event.getModifiersEx & InputEvent.BUTTON1_DOWN_MASK) != 0) {
        // println("changing angles")
        updateCameraControls.setXAngle(cx + dy * InteractiveView.RotateSpeed)
        updateCameraControls.setYAngle(cy + dx * InteractiveView.RotateSpeed)
        // println("done")
      } else {
        // println("changing pan")
        updateCameraControls.setXPos(cx - dx * InteractiveView.PanSpeed)
        updateCameraControls.setYPos(cy + dy * InteractiveView.PanSpeed)
        // println("done")
      }
    }

  }

  viewPanel.addMouseListener(mousePanListener)
  viewPanel.addMouseMotionListener(mousePanListener)

  add(viewPanel, BorderLayout.CENTER)

  /// ///

  setTitle(title)
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

  if (Debug.ENABLED) {
    DebugDisplay.show()
    DebugInput.setCallback(redraw)
    DebugInput.show()
  }

  /// ///


  def redraw(): Unit = {

    val startTime = System.nanoTime // System.currentTimeMillis
    redrawGeneric(im)
    imagePanel.repaint()

    val endTime = System.nanoTime // System.currentTimeMillis

    if (getFpsEnabled()) {
      val fps = 1.0e9 / (endTime - startTime)
      println(fps)
      val gr = im.getGraphics
      gr.setColor(Color.GREEN)
      gr.drawString(fps.round.toString, im.getWidth - 50, 25)
    }

    System.out.print(".")
  }


  def redrawGeneric(
      image: BufferedImage
    ): Unit =  {


    // for now, we'll get these values in with

    val curDateJulian = getCurDateJulian()
    val viewInfo = getViewInfo()
    val planets = getPlanets()
    val flights = getFlights().toList  // ugh
    val factions = getFactions()

    // extra layer of abstraction; while this grabs uses all of the UI options,
    // it allows rendering to an arbitrary image; ie, an image of known
    // resolution that will be saved to disk
    // val (camRot, camPos, viewPos, fpOption) = getViewInfo(curDateJulian, timelineMode, flights)

    val (camRot, camPos, viewPos, fpOption) = viewInfo

    val objects = Draw.redraw(
      fpOption,
      curDateJulian,
      planets,
      flights,
      factions,
      showSettings.asteroidBelt,
      showSettings.lagrangePoints,
      showSettings.orbitInfo,
      showSettings.motionVerticals,
      showSettings.flightStatus,
      viewerSettings,
      (camRot, camPos),
      viewPos,
      image
    )

    // update available objects to click on
    val selectedObjectsNew = scala.collection.mutable.Map[String, (Vec2, Boolean)]()
    objects.foreach({case (name, pos) => {
      selectedObjectsNew(name) = (pos, selectedObjects.get(name).exists(_._2))
    }})
    selectedObjects = selectedObjectsNew

    // ~~~~ draw ephemeral InteractiveView stuff
    // some of this could be before Draw.redraw if it didn't clear the image...food for thought

    val camTrans = View.cameraTransform(camRot, camPos)
    val view = new Viewer(camTrans, viewPos, viewerSettings)

    selectedObjects.foreach({case (name, (_, selected)) => {
      if (selected) {
        // is it a planet?
        MeeusPlanets.Planets.get(name).foreach(x => {
          RenderFlight.drawOrbitInfo(image, x.planet(curDateJulian), Transformations.IdentityTransformation, view)
        })

        // is it a moon?
        Moons.Moons.get(name).foreach(x => {
          // val laplacePlane = x.laplacePlane.map(
          //   y => Orbits.laplacePlaneICRFTransformation(y.rightAscension, y.declination))

          // TODO: remove ICRF transformation from calculation
          val laplacePlane = Some(
            x.laplacePlane.map(
              y => Orbits.laplacePlaneICRFTransformation(y.rightAscension, y.declination)
            ).getOrElse(
              // Conversions.ICRFToEcliptic
              Transformations.Identity3
            ))

          val preTrans = Transformations.transformation(
            laplacePlane.getOrElse(Transformations.Identity3),
            Orbits.planetState(x.primary, curDateJulian).position)

          RenderFlight.drawOrbitInfo(
            image,
            x.moon(curDateJulian),
            preTrans,
            view)

          val pMotion = RenderFlight.precessionPeriod(x.moon, curDateJulian, preTrans)
          RenderFlight.drawOrbit(
            image, pMotion, view,
            RenderFlight.ColorOrbital, showSettings.motionVerticals, true)
        })

        // TODO: is it a flight?
      }
    }})

    // DEBUG CALCULATIONS

    if (Debug.ENABLED) {

      /*
      DebugDisplay.set(
        "Earth - Inc to Eclip",
        MeeusPlanets.Earth.planet(curDateJulian).inclination / Conversions.DegToRad)
      DebugDisplay.set(
        "Mars - Inc to Eclip",
        MeeusPlanets.Mars.planet(curDateJulian).inclination / Conversions.DegToRad)
      DebugDisplay.set(
        "Saturn - Inc to Eclip",
        MeeusPlanets.Saturn.planet(curDateJulian).inclination / Conversions.DegToRad)
      */

      def angleDegrees(a: Vec3, b: Vec3): Double = {
        val res = math.acos(Vec3.dot(a, b)) / Conversions.DegToRad
        math.rint(res * 1000.0) / 1000.0
      }

      def axisInfo(planet: Planet): String = {

        val zAxis = Transformations.UnitZ
        val oe = planet.planet(curDateJulian)

        val orbitalToIntertial = Orbits.transformOrbitalInertial(oe)

        val axisOrbital = orbitalToIntertial.mul(zAxis)

        val planetAxis = Orbits.laplacePlaneICRFTransformation(
          planet.axialTilt.rightAscension, planet.axialTilt.declination).mul(zAxis)

        val orbitEcliptic = angleDegrees(zAxis, axisOrbital)
        val axialTiltOrbit =  angleDegrees(planetAxis, axisOrbital)
        val axialTiltEcliptic = angleDegrees(planetAxis, zAxis)

        axialTiltOrbit.toString + " " + axialTiltEcliptic.toString + " " + orbitEcliptic
        // Vec3.length(zAxis).toString + " " + Vec3.length(axisOrbital).toString + " " + Vec3.length(planetAxis).toString
      }

      DebugDisplay.set("Mercury", axisInfo(MeeusPlanets.Mercury))
      DebugDisplay.set("Venus", axisInfo(MeeusPlanets.Venus))
      DebugDisplay.set("Earth", axisInfo(MeeusPlanets.Earth))
      DebugDisplay.set("Mars", axisInfo(MeeusPlanets.Mars))
      DebugDisplay.set("Jupiter", axisInfo(MeeusPlanets.Jupiter))
      DebugDisplay.set("Saturn", axisInfo(MeeusPlanets.Saturn))
      DebugDisplay.set("Uranus", axisInfo(MeeusPlanets.Uranus))
      DebugDisplay.set("Neptune", axisInfo(MeeusPlanets.Neptune))

      DebugDisplay.update()

    }


  }

  def getViewInfo(): (Mat33, Vec3, Vec3, Option[FlightParams]) = {
      val curDateJulian = getCurDateJulian()
      if (getTimelineMode()) {
        getViewInfo(curDateJulian, getFlights().toList)
      } else {
        getViewInfo(curDateJulian, getFlight())
      }
  }

  // TODO: clean up

  // Get view information (camera position and rotation) at active time given camera settings
  def getViewInfo(
      curDateJulian: Double,
      flights: Seq[FlightParams]
      ): (Mat33, Vec3, Vec3, Option[FlightParams]) = {

    // TODO: separate logic for optional returning active flight...that makes this too complex

    // TODO: combine timeline / non-timeline type logic if possible; it will get more complicated
    // TODO: separate manual from both modes would be a start

    val camPos = cameraSettings.cameraPosType match {
      case "Manual" => getManualCamPos
      case _ => InteractiveView.findPosition(cameraSettings.cameraPosType, curDateJulian)
    }

    val activeFlights = flights.filter(x => curDateJulian > x.startDate.julian && curDateJulian < x.endDate.julian)
    val fpOption = activeFlights.reduceOption((x, y) => if (x.startDate.julian < y.startDate.julian) x else y)

    val camRot = cameraSettings.cameraPointType match {

      case "Manual"        => getManualCamRot

      case "Follow Active" => {

        // TODO: extract damping logic?

        val initial = Vec3.mul(prevPos, InteractiveView.Damping)

        // average positions of active flights with initial
        val curState = Vec3.mul(
          activeFlights.map(fp => {
            val (flightFn, _) = InteractiveView.paramsToFun(fp)
            flightFn(curDateJulian)
          }).foldLeft(initial)(Vec3.add),
          1.0 / (activeFlights.length + InteractiveView.Damping)
        )

        // update previous position
        prevPos = curState

        InteractiveView.pointCamera(curState, camPos)
      }

      case _ => InteractiveView.pointCamera(InteractiveView.findPosition(cameraSettings.cameraPointType, curDateJulian), camPos)

    }

    (camRot, camPos, getManualViewPos, fpOption)


  }


  def getViewInfo(
      curDateJulian: Double,
      flight: FlightParams
  ): (Mat33, Vec3, Vec3, Option[FlightParams]) = {


    // TODO: refactor to remove duplicate
    val camPos = cameraSettings.cameraPosType match {
      case "Manual" => getManualCamPos
      case _ => InteractiveView.findPosition(cameraSettings.cameraPosType, curDateJulian)
    }

    // individual flight mode

    val (flightFn, _) = InteractiveView.paramsToFun(flight)

    val camRot = cameraSettings.cameraPointType match {
      case "Manual"        => getManualCamRot
      case "Follow Active" => InteractiveView.pointCamera(flightFn(curDateJulian), camPos)
      case _               => {
        InteractiveView.pointCamera(InteractiveView.findPosition(cameraSettings.cameraPointType, curDateJulian), camPos)
      }

    }

    (camRot, camPos, getManualViewPos, Some(flight))


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


//  // get camera matrix from UI
//  private def getCamera: Mat44 = {
//    View.cameraTransform(getCamRot, getCamPos)
//  }


  private def getManualCamRot: Mat33 = {
    val xAngle = cameraSettings.xAngle * math.Pi / 180
    val yAngle = cameraSettings.yAngle * math.Pi / 180
    val zAngle = cameraSettings.zAngle * math.Pi / 180
    val theta = Vec3(xAngle, yAngle, zAngle)

    if (cameraSettings.isIntrinsic) {
      Transformations.rotationZYX(theta)
    } else {
      Transformations.rotationXYZ(theta)
    }
  }


  private def getManualCamPos: Vec3 = {
    Vec3(cameraSettings.xPos, cameraSettings.yPos, cameraSettings.zPos)
  }


  // get viewer position from UI
  private def getManualViewPos: Vec3 = {
    Vec3(0, 0, cameraSettings.zViewPos)
  }


}



object InteractiveView {

  // TODO: consider moving some of this to a separate config
  // so secondary isn't taking the orbits InteractiveView as a dep

  val CameraSettingsDefault = CameraSettings(
    cameraPosType = "Manual",
    cameraPointType = "Manual",
    xAngle = 45.0,
    yAngle = 0.0,
    zAngle = 180.0,
    isIntrinsic = true,
    xPos = 0.0,
    yPos = -5.0,
    zPos = 5.0,
    zViewPos = 800.0
  )

  // TODO: damping becomes a part of CameraSettings
  val Damping = 20.0

  // TODO: move to ShowSettingsDefault?
  val InitialVisiblePlanets = List("Earth", "Mars", "Saturn", "Uranus")

  val ViewWidth = 800
  val ViewHeight = 600


  val ZoomSpeed = 500
  val PanSpeed = 0.01
  val RotateSpeed = 0.1
  val ControlsWidth = 400


  // find position of a celestial body by name at a certain date
  def findPosition(fullName: String, curDateJulian: Double): Vec3 = {
    Locations.StatesMap.get(fullName).map(
      func => func(curDateJulian).position).getOrElse(Transformations.Vec3Zero)
  }


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
    // val origState = Orbits.planetState(fp.orig, startDateJulian)
    // val destState = Orbits.planetState(fp.dest, endDateJulian)

    val origState = fp.orig(startDateJulian)
    val destState = fp.dest(endDateJulian)

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


  def pointCamera(point: Vec3, camPos: Vec3): Mat33 = {
    val camToPoint = Vec2(point.x - camPos.x, point.y - camPos.y)
    val camAngleX = math.atan2(point.z - camPos.z, Vec2.length(camToPoint))
    val camAngleZ = math.atan2(camToPoint.x, camToPoint.y)
    val camOrient = Vec3(math.Pi * 0.5 + camAngleX, 0.0, math.Pi + camAngleZ)
//    println(
//      camOrient.x / Conversions.DegToRad,
//      camOrient.y / Conversions.DegToRad,
//      camOrient.z / Conversions.DegToRad)
    Transformations.rotationZYX(camOrient)
  }

  def buildCameraToolbar(
      cameraSettings: CameraSettings, redraw: () => Unit): (JToolBar, UpdateCameraControls) = {

    val cameraSettingsOrg = cameraSettings.copy()

    val spinnerWidth = 3

    val toolbar = new JToolBar()
    toolbar.setBorder(BorderFactory.createTitledBorder(toolbar.getBorder, "Camera"))

    // TODO: better programatic creation of camera types

    val cameraPosType = new JComboBox[String]((
        List("Manual") ++
        Locations.Bodies ++
        Locations.LagrangePoints
    ).toArray)

    cameraPosType.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        cameraSettings.cameraPosType = cameraPosType.getSelectedItem.asInstanceOf[String]
        redraw()
      }
    })

    val cameraPointType = new JComboBox[String]((
        List("Manual", "Follow Active") ++
        Locations.Bodies ++
        Locations.LagrangePoints
    ).toArray)

    cameraPointType.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
          cameraSettings.cameraPointType = cameraPointType.getSelectedItem.asInstanceOf[String]
          redraw()
      }
    })

    val xAngleField = new JSpinner(new SpinnerNumberModel(cameraSettings.xAngle, -360.0, 360.0, 0.25))
    val yAngleField = new JSpinner(new SpinnerNumberModel(cameraSettings.yAngle, -360.0, 360.0, 0.25))
    val zAngleField = new JSpinner(new SpinnerNumberModel(cameraSettings.zAngle, -360.0, 360.0, 0.25))
    xAngleField.addChangeListener(new ChangeListener {
      override def stateChanged(e: ChangeEvent): Unit = {
        cameraSettings.xAngle = xAngleField.getValue.asInstanceOf[Double]
        redraw()
      }
    })
    yAngleField.addChangeListener(new ChangeListener {
      override def stateChanged(e: ChangeEvent): Unit = {
        cameraSettings.yAngle = yAngleField.getValue.asInstanceOf[Double]
        redraw()
      }
    })
    zAngleField.addChangeListener(new ChangeListener {
      override def stateChanged(e: ChangeEvent): Unit = {
        cameraSettings.zAngle = zAngleField.getValue.asInstanceOf[Double]
        redraw()
      }
    })
    xAngleField.getEditor.asInstanceOf[JSpinner.DefaultEditor].getTextField.setColumns(spinnerWidth)
    yAngleField.getEditor.asInstanceOf[JSpinner.DefaultEditor].getTextField.setColumns(spinnerWidth)
    zAngleField.getEditor.asInstanceOf[JSpinner.DefaultEditor].getTextField.setColumns(spinnerWidth)
    val isIntrinsic = new JCheckBox("Int")
    isIntrinsic.setSelected(cameraSettings.isIntrinsic)
    isIntrinsic.addChangeListener(new ChangeListener {
      override def stateChanged(e: ChangeEvent): Unit = {
        cameraSettings.isIntrinsic = isIntrinsic.isSelected
        redraw()
      }
    })

    val xPosField = new JSpinner(new SpinnerNumberModel(cameraSettings.xPos, -100.0, 100.0, 0.2))
    val yPosField = new JSpinner(new SpinnerNumberModel(cameraSettings.yPos, -100.0, 100.0, 0.2))
    val zPosField = new JSpinner(new SpinnerNumberModel(cameraSettings.zPos, -100.0, 100.0, 0.2))
    val zViewPosField = new JSpinner(new SpinnerNumberModel(cameraSettings.zViewPos, 0.0, 100000.0, 100.0))

    xPosField.addChangeListener(new ChangeListener {
      override def stateChanged(e: ChangeEvent): Unit = {
        cameraSettings.xPos = xPosField.getValue.asInstanceOf[Double]
        redraw()
      }
    })
    yPosField.addChangeListener(new ChangeListener {
      override def stateChanged(e: ChangeEvent): Unit = {
        cameraSettings.yPos = yPosField.getValue.asInstanceOf[Double]
        redraw()
      }
    })
    zPosField.addChangeListener(new ChangeListener {
      override def stateChanged(e: ChangeEvent): Unit = {
        cameraSettings.zPos = zPosField.getValue.asInstanceOf[Double]
        redraw()
      }
    })
    zViewPosField.addChangeListener(new ChangeListener {
      override def stateChanged(e: ChangeEvent): Unit = {
        cameraSettings.zViewPos = zViewPosField.getValue.asInstanceOf[Double]
        redraw()
      }
    })
    zViewPosField.setValue(cameraSettings.zViewPos)
    xPosField.getEditor.asInstanceOf[JSpinner.DefaultEditor].getTextField.setColumns(spinnerWidth)
    yPosField.getEditor.asInstanceOf[JSpinner.DefaultEditor].getTextField.setColumns(spinnerWidth)
    zPosField.getEditor.asInstanceOf[JSpinner.DefaultEditor].getTextField.setColumns(spinnerWidth)
    zViewPosField.getEditor.asInstanceOf[JSpinner.DefaultEditor].getTextField.setColumns(spinnerWidth)

    val zoomSlider = new JSlider(
      SwingConstants.HORIZONTAL, 0, 100, 0)
    zoomSlider.addChangeListener(new ChangeListener {
      def stateChanged(event: ChangeEvent): Unit = {
        val scale = zoomSlider.getValue / 100.0
        zViewPosField.setValue(
          cameraSettingsOrg.zViewPos + math.pow(scale * 7000.0, 2))
      }
    })

    val resetButton = new JButton("Reset")
    resetButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        // update stuff
        // println("resetting camera")
        xAngleField.setValue(cameraSettingsOrg.xAngle)
        yAngleField.setValue(cameraSettingsOrg.yAngle)
        zAngleField.setValue(cameraSettingsOrg.zAngle)
        xPosField.setValue(cameraSettingsOrg.xPos)
        yPosField.setValue(cameraSettingsOrg.yPos)
        zPosField.setValue(cameraSettingsOrg.zPos)
        zViewPosField.setValue(cameraSettingsOrg.zViewPos)
        // println("done")
      }
    })

    toolbar.add(cameraPosType)
    toolbar.add(cameraPointType)
    toolbar.add(new JSeparator(SwingConstants.VERTICAL))
    toolbar.add(xPosField)
    toolbar.add(yPosField)
    toolbar.add(zPosField)
    toolbar.add(new JSeparator(SwingConstants.VERTICAL))
    toolbar.add(xAngleField)
    toolbar.add(yAngleField)
    toolbar.add(zAngleField)
    toolbar.add(isIntrinsic)
    toolbar.add(zViewPosField)
    toolbar.add(new JSeparator(SwingConstants.VERTICAL))
    toolbar.add(zoomSlider)
    toolbar.add(resetButton)

    val updateCameraControls = UpdateCameraControls(
      setXAngle = x => xAngleField.setValue(x),
      setYAngle = x => yAngleField.setValue(x),
      setZAngle = x => zAngleField.setValue(x),
      setXPos = x => xPosField.setValue(x),
      setYPos = x => yPosField.setValue(x),
      setZPos = x => zPosField.setValue(x),
      setZViewPos = x => zViewPosField.setValue(x)
    )

    (toolbar, updateCameraControls)

  }



  def buildExportToolbar(
      drawImage: BufferedImage => Unit): JToolBar = {

    // TODO: make image width and height parameters

    val toolbar = new JToolBar()
    toolbar.setLayout(new FlowLayout(FlowLayout.LEFT))
    toolbar.setBorder(BorderFactory.createTitledBorder(toolbar.getBorder, "Export"))

    val exportButton = new JButton("Export")

    exportButton.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {

        val writeTimeStart = System.currentTimeMillis

        val sdf = new SimpleDateFormat("YYYYMMdd_HHmmss")
        val formatted = sdf.format(Calendar.getInstance().getTime)

        val outputFile = new java.io.File("orbits_" + formatted + ".png")

        val image = new BufferedImage(1280, 720, BufferedImage.TYPE_INT_ARGB)
        drawImage(image)
        Imaging.writeImage(
          image, outputFile, ImageFormats.PNG, new java.util.HashMap[String, Object]())

        val writeTime = System.currentTimeMillis - writeTimeStart

        print("exported " + outputFile.getName + " in " + writeTime + " ms")
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

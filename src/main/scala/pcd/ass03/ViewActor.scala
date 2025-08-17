package pcd.ass03

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors

import scala.swing.*

object ViewActor:
  private val width: Int = 1000
  private val height: Int = 800

  private val frame: Frame = new MainFrame:
    title = "Boids Simulation"
    centerOnScreen()

  def apply(): Behavior[BoidMessage] =
    this.createSimulationPanel()
    Behaviors.receive: (context, message) =>
      message match
        case UpdateView(from) =>
          context.log.info(s"${context.self}: Updating view, from $from")
          from ! UpdatedView()
          Behaviors.same

  private val _simulationPanel = new BorderPanel()
  private val boidsPanel = BoidsPanel(width, height)
  private val enteringPanel = EnteringPanel(frame)
  private val cohesionSlider = makeSlider
  private val separationSlider = makeSlider
  private val alignmentSlider = makeSlider

  def createSimulationPanel(): Unit =
    val slidersPanel = new FlowPanel()
    slidersPanel.contents ++= List(new Label("Separation"), separationSlider, Label("Alignment"), alignmentSlider,
      Label("Cohesion"), cohesionSlider)
    val buttonsPanel = createButtonsPanel
    simulationPanel.layout ++= List((buttonsPanel, BorderPanel.Position.North),
      (boidsPanel, BorderPanel.Position.Center), (slidersPanel, BorderPanel.Position.South))
    //boidsPanel = new BoidsPanel(this, controller)

  private def createButtonsPanel: FlowPanel = {
    val buttonsPanel = new FlowPanel()
    val stopButton = new Button("Stop")
    /*stopButton.addActionListener((e: ActionEvent) => {
      this.enteringPanel = new EnteringPanel(this, this.controller, this.frame)
      this.controller.stopSimulation()
    })*/
    val suspendResumeButton = new Button("Suspend")
    /*suspendResumeButton.addActionListener((e: ActionEvent) => {
      if (suspendResumeButton.getText == "Suspend") {
        suspendResumeButton.setText("Resume")
        this.controller.suspendSimulation()
      }
      else {
        suspendResumeButton.setText("Suspend")
        this.controller.resumeSimulation()
      }
    })*/
    buttonsPanel.contents ++= List(stopButton, suspendResumeButton)
    buttonsPanel
  }

  def setPanel(panel: Panel): Unit =
    frame.peer.getContentPane.removeAll()
    frame.contents = panel
    frame.peer.revalidate()
    frame.size = new Dimension(width, height)
    frame.repaint()
    frame.centerOnScreen()

  private def makeSlider: Slider =
    new Slider():
      orientation = Orientation.Horizontal
      min = 0
      max = 20
      value = 10
      majorTickSpacing = 10
      minorTickSpacing = 1
      paintTicks = true
      paintLabels = true
      labels = Map(0 -> Label("0"), 10 -> Label("1"), 20 -> Label("2"))
      //slider.addChangeListener(this)

  def update(frameRate: Int): Unit =
    boidsPanel.frameRate = frameRate
    boidsPanel.repaint()

  /*override def stateChanged(e: ChangeEvent): Unit =
    var `val`: Int = 0
    if (e.getSource eq this.separationSlider) {
      `val` = this.separationSlider.getValue
      this.controller.setSeparationWeight(0.1 * `val`)
    }
    else if (e.getSource eq this.cohesionSlider) {
      `val` = this.cohesionSlider.getValue
      this.controller.setCohesionWeight(0.1 * `val`)
    }
    else {
      `val` = this.alignmentSlider.getValue
      this.controller.setAlignmentWeight(0.1 * `val`)
    }*/

  def simulationPanel: BorderPanel = _simulationPanel
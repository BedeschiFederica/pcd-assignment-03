package pcd.ass03

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors

import scala.swing.*

trait ViewMessage
final case class SendManager(manager: ActorRef[ManagerMessage]) extends ViewMessage
final case class UpdateView(from: ActorRef[ManagerMessage]) extends ViewMessage
final case class InitDrawer(boids: List[ActorRef[BoidMessage]]) extends ViewMessage

object ViewActor:
  private var managerActor: Option[ActorRef[ManagerMessage]] = Option.empty
  private var drawerActor: Option[ActorRef[DrawMessage]] = Option.empty

  private val width: Int = 1000
  private val height: Int = 800

  private val frame: Frame = new MainFrame:
    title = "Boids Simulation"
    centerOnScreen()
  private val simulationPanel = new BorderPanel()
  private var boidsPanel: Option[FlowPanel] = Option.empty
  private val cohesionSlider = makeSlider
  private val separationSlider = makeSlider
  private val alignmentSlider = makeSlider

  def apply(): Behavior[ViewMessage] =
    Behaviors.receive: (context, message) =>
      message match
        case SendManager(manager) =>
          managerActor = Some(manager)
          context.spawnAnonymous(EnteringPanelActor(manager, frame))
          Behaviors.same
        case InitDrawer(boids) =>
          drawerActor = Some(context.spawnAnonymous(DrawerActor(managerActor, boids, width, height)))
          boidsPanel = Some(DrawerActor.panel())
          createSimulationPanel()
          Behaviors.same
        case UpdateView(from) =>
          context.log.info(s"${context.self}: Updating view, from $from")
          drawerActor.get ! DrawBoids()
          Behaviors.same

  private def createSimulationPanel(): Unit =
    val slidersPanel = new FlowPanel()
    slidersPanel.contents ++= List(new Label("Separation"), separationSlider, Label("Alignment"), alignmentSlider,
      Label("Cohesion"), cohesionSlider)
    val buttonsPanel = createButtonsPanel
    simulationPanel.layout ++= List((buttonsPanel, BorderPanel.Position.North),
      (boidsPanel.get, BorderPanel.Position.Center), (slidersPanel, BorderPanel.Position.South))
    setPanel(simulationPanel)

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

  private def setPanel(panel: Panel): Unit =
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

  /*def update(frameRate: Int): Unit =
    boidsPanel.frameRate = frameRate
    boidsPanel.repaint()*/

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

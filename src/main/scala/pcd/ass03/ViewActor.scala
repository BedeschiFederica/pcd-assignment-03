package pcd.ass03

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}

import scala.concurrent.Future
import scala.swing.*
import scala.swing.event.{ButtonClicked, ValueChanged}
import scala.util.Success

trait ViewMessage
final case class SendManager(manager: ActorRef[ManagerMessage]) extends ViewMessage
final case class UpdateView(from: ActorRef[ManagerMessage]) extends ViewMessage
final case class InitDrawer(boids: List[ActorRef[BoidMessage]]) extends ViewMessage
final case class Stop() extends ViewMessage
final case class SuspendResume() extends ViewMessage

object ViewActor:
  private val Width: Int = 1000
  private val Height: Int = 800
  private val SliderDefaultValue = 10
  private val SuspendText = "Suspend"
  private val ResumeText = "Resume"

  private var managerActor: Option[ActorRef[ManagerMessage]] = Option.empty
  private var enteringPanelActor: Option[ActorRef[Click]] = Option.empty
  private var drawerActor: Option[ActorRef[DrawMessage]] = Option.empty

  private val frame: Frame = new MainFrame:
    title = "Boids Simulation"
    centerOnScreen()
  private val simulationPanel = new BorderPanel()
  private var boidsPanel: Option[FlowPanel] = Option.empty
  private val cohesionSlider = makeSlider
  private val separationSlider = makeSlider
  private val alignmentSlider = makeSlider
  private val stopButton = new Button("Stop")
  private val suspendResumeButton = new Button(SuspendText)

  def apply(): Behavior[ViewMessage] =
    Behaviors.receive: (context, message) =>
      message match
        case SendManager(manager) =>
          managerActor = Some(manager)
          enteringPanelActor = Some(context.spawnAnonymous(EnteringPanelActor(manager, frame)))
          addListeners(context)
          Behaviors.same
        case InitDrawer(boids) =>
          suspendResumeButton.text = SuspendText
          Set(separationSlider, alignmentSlider, cohesionSlider).foreach(_.value = SliderDefaultValue)
          drawerActor = Some(context.spawnAnonymous(DrawerActor(managerActor, boids, Width, Height)))
          boidsPanel = Some(DrawerActor.panel())
          createSimulationPanel()
          Behaviors.same
        case UpdateView(from) =>
          drawerActor.get ! DrawBoids()
          Behaviors.same
        case Stop() =>
          context.stop(enteringPanelActor.get)
          context.stop(drawerActor.get)
          enteringPanelActor = Some(context.spawnAnonymous(EnteringPanelActor(managerActor.get, frame)))
          managerActor.get ! StopSimulation()
          Behaviors.same
        case SuspendResume() =>
          suspendResumeButton.text = suspendResumeButton.text match
            case SuspendText =>
              managerActor.get ! SuspendSimulation()
              ResumeText
            case _ =>
              managerActor.get ! ResumeSimulation()
              SuspendText
          Behaviors.same

  private def createSimulationPanel(): Unit =
    val slidersPanel = new FlowPanel():
      contents ++= List(new Label("Separation"), separationSlider, Label("Alignment"), alignmentSlider,
        Label("Cohesion"), cohesionSlider)
      listenTo(separationSlider, alignmentSlider, cohesionSlider)
    val buttonsPanel = new FlowPanel():
      contents ++= List(stopButton, suspendResumeButton)
    simulationPanel.layout ++= List((buttonsPanel, BorderPanel.Position.North),
      (boidsPanel.get, BorderPanel.Position.Center), (slidersPanel, BorderPanel.Position.South))
    setPanel(simulationPanel)

  private def setPanel(panel: Panel): Unit =
    frame.peer.getContentPane.removeAll()
    frame.contents = panel
    frame.peer.revalidate()
    frame.size = new Dimension(Width, Height)
    frame.repaint()
    frame.centerOnScreen()

  private def makeSlider: Slider =
    new Slider():
      orientation = Orientation.Horizontal
      min = 0
      max = 20
      value = SliderDefaultValue
      majorTickSpacing = 10
      minorTickSpacing = 1
      paintTicks = true
      paintLabels = true
      labels = Map(min -> Label("0"), SliderDefaultValue -> Label("1"), max -> Label("2"))

  private def addListeners(context: ActorContext[ViewMessage]): Unit =
    stopButton.addListener(context, Stop())
    suspendResumeButton.addListener(context, SuspendResume())
    Set(separationSlider, alignmentSlider, cohesionSlider).foreach(_.addListener())

  extension (button: Button)
    private def addListener(context: ActorContext[ViewMessage], message: ViewMessage): Unit =
      button.listenTo(button.mouse.clicks)
      button.reactions += {
        case _: ButtonClicked =>
          context.pipeToSelf(Future.unit):
            case Success(_) => message
            case _ => throw IllegalStateException("Future unsuccessful.")
      }

  extension (slider: Slider)
    private def addListener(): Unit =
      val ValueFactor = 0.1
      slider.reactions += {
        case _: ValueChanged => managerActor.get ! ChangeWeights(separationSlider.value * ValueFactor,
          alignmentSlider.value * ValueFactor, cohesionSlider.value * ValueFactor)
      }

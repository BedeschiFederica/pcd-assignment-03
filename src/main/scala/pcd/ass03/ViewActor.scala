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
  private var managerActor: Option[ActorRef[ManagerMessage]] = Option.empty
  private var enteringPanelActor: Option[ActorRef[Click]] = Option.empty
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
  private val stopButton = new Button("Stop")
  private val suspendResumeButton = new Button("Suspend")

  def apply(): Behavior[ViewMessage] =
    Behaviors.receive: (context, message) =>
      message match
        case SendManager(manager) =>
          managerActor = Some(manager)
          enteringPanelActor = Some(context.spawnAnonymous(EnteringPanelActor(manager, frame)))
          addListeners(context)
          Behaviors.same
        case InitDrawer(boids) =>
          suspendResumeButton.text = "Suspend"
          Set(separationSlider, alignmentSlider, cohesionSlider).foreach(_.value = 10)
          drawerActor = Some(context.spawnAnonymous(DrawerActor(managerActor, boids, width, height)))
          boidsPanel = Some(DrawerActor.panel())
          createSimulationPanel()
          Behaviors.same
        case UpdateView(from) =>
          //context.log.info(s"${context.self}: Updating view, from $from")
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
            case "Suspend" =>
              managerActor.get ! SuspendSimulation()
              "Resume"
            case _ =>
              managerActor.get ! ResumeSimulation()
              "Suspend"
          Behaviors.same

  private def createSimulationPanel(): Unit =
    val slidersPanel = new FlowPanel()
    slidersPanel.contents ++= List(new Label("Separation"), separationSlider, Label("Alignment"), alignmentSlider,
      Label("Cohesion"), cohesionSlider)
    slidersPanel.listenTo(separationSlider, alignmentSlider, cohesionSlider)
    val buttonsPanel = createButtonsPanel
    simulationPanel.layout ++= List((buttonsPanel, BorderPanel.Position.North),
      (boidsPanel.get, BorderPanel.Position.Center), (slidersPanel, BorderPanel.Position.South))
    setPanel(simulationPanel)

  private def createButtonsPanel: FlowPanel = {
    val buttonsPanel = new FlowPanel()
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

  private def addListeners(context: ActorContext[ViewMessage]): Unit =
    stopButton.listenTo(stopButton.mouse.clicks)
    stopButton.reactions += {
      case _: ButtonClicked =>
        context.pipeToSelf(Future.unit) {
          case Success(_) => Stop()
          case _ => throw IllegalStateException("Future unsuccessful.")
        }
    }
    suspendResumeButton.listenTo(suspendResumeButton.mouse.clicks)
    suspendResumeButton.reactions += {
      case _: ButtonClicked =>
        context.pipeToSelf(Future.unit) {
          case Success(_) => SuspendResume()
          case _ => throw IllegalStateException("Future unsuccessful.")
        }
    }
    Set(separationSlider, alignmentSlider, cohesionSlider).foreach(addSliderListener)

  private def addSliderListener(slider: Slider): Unit =
    slider.reactions += {
      case _: ValueChanged => managerActor.get ! ChangeWeights(separationSlider.value * 0.1, alignmentSlider.value * 0.1,
        cohesionSlider.value * 0.1)
    }

package pcd.ass03

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors

import scala.concurrent.Future
import scala.swing.*
import scala.swing.event.ButtonClicked
import scala.util.Success

final case class Click()

object EnteringPanelActor:
  private val TextFieldColumns = 25
  private val FontSize = 14
  private val XLayoutAlignment = 0.5
  private val _panel = BorderPanel()
  private val field = new TextField(TextFieldColumns):
    horizontalAlignment = Alignment.Center
    maximumSize = new Dimension(200, 30)
    xLayoutAlignment = XLayoutAlignment
  private val startButton = new Button("Start"):
    xLayoutAlignment = XLayoutAlignment
    font = new Font(Font.SansSerif, Font.Style.Bold.id, FontSize)

  def apply(manager: ActorRef[ManagerMessage], frame: Frame): Behavior[Click] =
    createPanel(frame)
    Behaviors.setup: context =>
      startButton.listenTo(startButton.mouse.clicks)
      startButton.reactions += {
        case event: ButtonClicked =>
          context.pipeToSelf(Future.successful(event)) {
            case Success(_) => Click()
            case _ => throw IllegalStateException("Future unsuccessful.")
          }
      }
      Behaviors.receiveMessage:
        case Click() =>
          try
            val nBoids = field.text.toInt
            if nBoids > 0 then
              println("Starting simulation")
              manager ! Start(nBoids)
            else println("Illegal value inserted! Positive integer is requested.")
          catch case ex: NumberFormatException => println("Illegal value inserted! Integer is requested.")
          context.log.info(s"Click")
          Behaviors.same

  def panel(): BorderPanel = _panel

  private def createPanel(frame: Frame): Unit =
    val label = new Label("Number of boids: "):
      xLayoutAlignment = XLayoutAlignment
      font = new Font(Font.SansSerif, Font.Style.Bold.id, FontSize)
    val centerPanel = new BoxPanel(Orientation.Vertical):
      contents ++= List(label, field, startButton)
    _panel.layout += ((centerPanel, BorderPanel.Position.Center))
    frame.peer.setContentPane(_panel.peer)
    frame.peer.pack()
    frame.centerOnScreen()
    frame.visible = true

package pcd.ass03

import scala.swing.*
import scala.swing.event.ButtonClicked

class EnteringPanel(frame: Frame) extends BorderPanel:
  private val TextFieldColumns = 25
  private val FontSize = 14

  private val field = new TextField(TextFieldColumns):
    horizontalAlignment = Alignment.Center
    maximumSize = new Dimension(200, 30)
    xLayoutAlignment = 0.5
  private val label = new Label("Number of boids: "):
    xLayoutAlignment = 0.5
    font = new Font(Font.SansSerif, Font.Style.Bold.id, FontSize)
  private val startButton = new Button("Start"):
    xLayoutAlignment = 0.5
    font = new Font(Font.SansSerif, Font.Style.Bold.id, FontSize)
  private val centerPanel = new BoxPanel(Orientation.Vertical):
    contents ++= List(label, field, startButton)
  layout += ((centerPanel, BorderPanel.Position.Center))
  frame.peer.setContentPane(peer)
  frame.peer.pack()
  frame.centerOnScreen()
  frame.visible = true
  startButton.listenTo(startButton.mouse.clicks)
  startButton.reactions += {
    case _: ButtonClicked =>
      try
        val nBoids = field.text.toInt
        if nBoids > 0 then
          ViewActor.createSimulationPanel()
          ViewActor.setPanel(ViewActor.simulationPanel)
          println("Starting simulation")
          //boidManager ! Start(nBoids)
        else println("Illegal value inserted! Positive integer is requested.")
      catch case ex: NumberFormatException => println("Illegal value inserted! Integer is requested.")
  }

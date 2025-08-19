package pcd.ass03

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorSystem, Behavior}
import com.typesafe.config.ConfigFactory

object Configuration:
  val PerceptionRadius = 50.0
  val AvoidRadius = 20.0
  val SeparationWeight = 1.0
  val AlignmentWeight = 1.0
  val CohesionWeight = 1.0
  val MaxSpeed = 4.0
  val EnvironmentWidth = 1000
  val EnvironmentHeight = 1000
  val MinX: Double = -EnvironmentWidth / 2
  val MaxX: Double = EnvironmentWidth / 2
  val MinY: Double = -EnvironmentHeight / 2
  val MaxY: Double = EnvironmentHeight / 2

object MainActor:
  def apply(): Behavior[Nothing] =
    Behaviors.setup: context =>
      val viewActor = context.spawn(ViewActor(), "view-actor")
      val boidsManager = context.spawn(BoidsManager(viewActor), "boids-manager")
      viewActor ! SendManager(boidsManager)
      Behaviors.empty

object Launcher extends App:
  private val blockingScheduler =
    """
      |my-blocking-dispatcher {
      |  type = Dispatcher
      |  executor = "thread-pool-executor"
      |  thread-pool-executor {
      |    fixed-pool-size = 16
      |  }
      |  throughput = 1
      |}
      |""".stripMargin
  private val myConfig = ConfigFactory.parseString(blockingScheduler)
  private val system: ActorSystem[Nothing] = ActorSystem(MainActor(), "main-actor", myConfig)

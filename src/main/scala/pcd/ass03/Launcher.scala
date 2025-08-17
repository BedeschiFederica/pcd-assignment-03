package pcd.ass03

import akka.actor.typed.ActorSystem

object Configuration:
  val NumBoids = 10
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

object Launcher extends App:
  val system: ActorSystem[ManagerMessage] = ActorSystem(BoidsManager(), name = "boid-manager")
  Thread.sleep(5000)
  system.terminate()
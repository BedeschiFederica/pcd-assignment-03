package pcd.ass03

import akka.actor.typed.scaladsl.*
import akka.actor.typed.{Behavior, SupervisorStrategy}
import akka.cluster.*
import akka.cluster.typed.{Cluster, ClusterSingleton, SingletonActor}
import pcd.ass03.model.{PlayerActor, Position, WorldManager}
import pcd.ass03.view.PlayerView

import scala.concurrent.duration.DurationInt
import scala.language.postfixOps
import scala.util.Random

object Roles:
  val player: String = "player"
  val world: String = "world"

object Root:
  private val Width = 1000
  private val Height = 1000
  private val InitialPlayerMass = 120.0
  private val StandardFrameRate = 60 milliseconds

  def apply(id: String): Behavior[Nothing] = Behaviors.setup:
    ctx =>
      val cluster = Cluster(ctx.system)
      ctx.system.whenTerminated.onComplete { _ => Thread.sleep(5000); System.exit(0) } (using ctx.executionContext)
      if cluster.selfMember.hasRole(Roles.player) then
        val playerViewName = PlayerView.getClass.getSimpleName.dropRight(1)
        val playerId = s"p${id.take(1)}"
        ctx.spawn(PlayerView(Width, Height)(StandardFrameRate), s"$playerViewName${playerId.drop(1)}")
        ctx.spawn(PlayerActor(playerId, Position(Random.nextInt(Width), Random.nextInt(Height)), InitialPlayerMass)
          (id.toLowerCase.contains("ai"))(Width, Height), playerId)
      else
        ClusterSingleton(ctx.system).init(
          SingletonActor(
            Behaviors.supervise(WorldManager(Width, Height)(StandardFrameRate))
              .onFailure[Exception](SupervisorStrategy.restart),
            "worldSingleton"
          )
        )
      Behaviors.empty

val NumPlayers = 2

private def startPlayer(nPlayer: Int): Unit = startupWithRole(Roles.player, seeds(nPlayer))(Root(nPlayer.toString))

@main def main(): Unit =
  startupWithRole(Roles.world, seeds.head)(Root("World"))
  (1 to NumPlayers).foreach(startPlayer)

@main def addPlayer(): Unit = startPlayer(NumPlayers + 1)

package pcd.ass03

/**
 * @author Bedeschi Federica   federica.bedeschi4@studio.unibo.it
 * @author Pracucci Filippo    filippo.pracucci@studio.unibo.it
 */

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

  def apply(id: String): Behavior[Nothing] = Behaviors.setup: ctx =>
    val cluster = Cluster(ctx.system)
    ctx.system.whenTerminated.onComplete { _ => Thread.sleep(5000); System.exit(0) } (using ctx.executionContext)
    if cluster.selfMember.hasRole(Roles.player) then
      val playerId = s"p${id.take(1)}"
      ctx.spawn(Behaviors.supervise(
        PlayerActor(playerId, Position(Random.nextInt(Width), Random.nextInt(Height)), InitialPlayerMass)
        (id.toLowerCase.contains("ai"))(Width, Height)
      ).onFailure[Exception](SupervisorStrategy.restart), playerId)
      val playerViewName = PlayerView.getClass.getSimpleName.dropRight(1)
      ctx.spawn(Behaviors.supervise(
        PlayerView(Width, Height)(StandardFrameRate)
      ).onFailure[Exception](SupervisorStrategy.restart), s"$playerViewName${playerId.drop(1)}")
    else
      ClusterSingleton(ctx.system).init(
        SingletonActor(
          Behaviors.supervise(WorldManager(Width, Height)(StandardFrameRate))
            .onFailure[Exception](SupervisorStrategy.restart),
          "worldSingleton"
        )
      )
    Behaviors.empty

val NumPlayers = 3

private def startPlayer(nPlayer: Int, ai: Boolean = false): Unit =
  startupWithRole(Roles.player, seeds(nPlayer))(Root(nPlayer.toString + (if ai then "ai" else "")))

@main def main(): Unit =
  startupWithRole(Roles.world, seeds.head)(Root("World"))
  (1 until NumPlayers).foreach(startPlayer(_))
  startPlayer(NumPlayers, ai = true)


@main def addPlayer(): Unit = startPlayer(NumPlayers + 1)

package pcd.ass03

import akka.actor.typed.scaladsl.*
import akka.actor.typed.Behavior
import akka.cluster.*
import akka.cluster.typed.{Cluster, ClusterSingleton, SingletonActor}
import pcd.ass03.model.EatingManager.EndGameManager
import pcd.ass03.model.{EatingManager, PlayerActor, Position, WorldManager}
import pcd.ass03.view.{GlobalView, PlayerView}
import pcd.ass03.view.PlayerView.*

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.language.postfixOps
import scala.util.Random

object Roles:
  val player: String = "player"
  val world: String = "world"

object Root:
  private val width = 1000
  private val height = 1000

  def apply(id: String, period: FiniteDuration = 60 milliseconds): Behavior[Nothing] = Behaviors.setup: ctx =>
    val cluster = Cluster(ctx.system)
    ctx.system.whenTerminated.onComplete { _ => Thread.sleep(5000); System.exit(0) } (using ctx.executionContext)
    if cluster.selfMember.hasRole(Roles.player) then
      val playerViewName = PlayerView.getClass.getSimpleName.dropRight(1)
      val playerId = s"p${id.take(1)}"
      ctx.spawn(PlayerView(width, height)(), s"$playerViewName${playerId.drop(1)}")
      ctx.spawn(PlayerActor(playerId, Position(Random.nextInt(width), Random.nextInt(height)), mass = 120)
        (id.toLowerCase.contains("ai"))(width, height), playerId)
    else
      ctx.spawnAnonymous(WorldManager(width, height))
      ctx.spawnAnonymous(EatingManager())
      ctx.spawnAnonymous(EndGameManager())
      ctx.spawnAnonymous(GlobalView(width, height)())
    Behaviors.empty

@main def mainTest(): Unit =
  startupWithRole(Roles.world, seeds.head)(Root("World"))
  startupWithRole(Roles.player, seeds(1))(Root("1"))
  startupWithRole(Roles.player, seeds(2))(Root("2ai"))

@main def addPlayer(): Unit =
  startupWithRole(Roles.player, seeds(3))(Root("3"))

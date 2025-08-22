package pcd.ass03

import akka.actor.typed.scaladsl.*
import akka.actor.typed.Behavior
import akka.cluster.*
import akka.cluster.typed.Cluster
import pcd.ass03.model.{FoodManager, PlayerActor, Position, WorldManager}
import pcd.ass03.view.PlayerView
import pcd.ass03.view.PlayerView.*

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.language.postfixOps
import scala.util.Random

object Roles:
  val player: String = "player"
  val world: String = "world"

object Root:
  def apply(id: String, period: FiniteDuration = 60 milliseconds): Behavior[Nothing] = Behaviors.setup: ctx =>
    val cluster = Cluster(ctx.system)
    if cluster.selfMember.hasRole(Roles.player) then
      given random: Random = Random()
      val (x, y) = (random.nextInt(300), random.nextInt(300))
      val playerViewName =
        PlayerView.getClass.getSimpleName.dropRight(1)
      ctx.spawn(PlayerView(), s"$playerViewName$id")
      val playerId = s"p$id"
      ctx.spawn(PlayerActor(playerId, Position(x, y), mass = 120), playerId)
    else
      ctx.spawnAnonymous(WorldManager(400, 400))
      ctx.spawnAnonymous(FoodManager())
    Behaviors.empty

@main def mainTest(): Unit =
  startupWithRole(Roles.world, seeds.head)(Root("World"))
  startupWithRole(Roles.player, seeds(1))(Root("1"))
  startupWithRole(Roles.player, seeds(2))(Root("2"))

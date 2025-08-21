package pcd.ass03

import akka.actor.typed.scaladsl.*
import akka.actor.typed.Behavior
import akka.cluster.*
import akka.cluster.typed.Cluster
import pcd.ass03.model.{Player, Position, World}
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
      ctx.spawn(PlayerView(), id)
      ctx.spawnAnonymous(Player(id, Position(x, y), mass = 120))
    else
      ctx.spawnAnonymous(World(400, 400))
    Behaviors.empty

@main def mainTest(): Unit =
  startupWithRole(Roles.world, seeds.head)(Root("World"))
  startupWithRole(Roles.player, seeds(1))(Root("Player1"))
  startupWithRole(Roles.player, seeds(2))(Root("Player2"))

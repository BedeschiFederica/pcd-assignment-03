package pcd.ass03

import akka.actor.typed.scaladsl.*
import akka.actor.typed.Behavior
import akka.cluster.*
import akka.cluster.typed.Cluster
import pcd.ass03.model.{Player, Position}
import pcd.ass03.view.PlayerView
import pcd.ass03.view.PlayerView.*

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.language.postfixOps
import scala.util.Random

object Roles:
  val player: String = "player"

object Root:
  def apply(playerId: String, period: FiniteDuration = 60 milliseconds): Behavior[Nothing] = Behaviors.setup: ctx =>
    val cluster = Cluster(ctx.system)
    if cluster.selfMember.hasRole(Roles.player) then
      given random: Random = Random()
      val (x, y) = (random.nextInt(300), random.nextInt(300))
      ctx.spawnAnonymous(PlayerView())
      ctx.spawnAnonymous(Player(playerId, Position(x, y), mass = 120))
    Behaviors.empty

@main def mainTest(): Unit =
  startupWithRole(Roles.player, seeds.head)(Root("Player 1"))
  startupWithRole(Roles.player, seeds(1))(Root("Player 2"))

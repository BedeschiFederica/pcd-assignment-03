package pcd.ass03

import akka.actor.testkit.typed.scaladsl.ActorTestKit
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import model.{Player, Position}
import pcd.ass03.model.PlayerActor.PlayerMessage
import pcd.ass03.model.PlayerActor.PlayerMessage.Move
import pcd.ass03.view.PlayerView
import pcd.ass03.view.PlayerView.PlayerViewMessage.{Render, UpdatePlayer}

import scala.concurrent.duration.DurationInt

class PlayerTest extends AnyFlatSpec with Matchers:
  private val testKit = ActorTestKit()

  "A testkit" should "verify player movement" in:
    val probe = testKit.createTestProbe[PlayerMessage]()
    val playerView = testKit.spawn(PlayerView(1000, 1000)(60.milliseconds), "PlayerView1")
    playerView ! Render(Player("p1", Position(0, 0), 120.0), probe.ref)
    playerView ! UpdatePlayer(1, 1)
    probe expectMessage Move(1, 1)

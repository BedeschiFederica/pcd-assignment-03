package pcd.ass03.model

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.BeforeAndAfterEach
import org.scalatest.BeforeAndAfterAll
import akka.actor.testkit.typed.scaladsl.ActorTestKit
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.ActorRef

class BoidsInteractionTest extends AnyFlatSpec with Matchers with BeforeAndAfterAll with BeforeAndAfterEach:
  private var testKit = ActorTestKit()
  private val NBoids = 10
  private val NIterations = 100

  override def beforeEach(): Unit = testKit = ActorTestKit()

  override def afterAll(): Unit = testKit.shutdownTestKit()

  "A testkit" should "verify update messages exchange" in:
    val probe = testKit.createTestProbe[ManagerMessage]()
    var boidActors: List[ActorRef[BoidMessage]] = List.empty
    for
      i <- 0 until NBoids
    yield boidActors +:= testKit.spawn[BoidMessage](Behaviors.setup((context: ActorContext[BoidMessage]) =>
      BoidActor(probe.ref, NBoids)))
    boidActors.foreach(boid => boid ! SendBoids(boidActors.filterNot(_ == boid)))

    for i <- 1 to NIterations do
      boidActors.foreach: boid =>
        boid ! UpdateVel(probe.ref)
        probe expectMessage UpdatedVel()
        boid ! UpdatePos(probe.ref)
        probe expectMessage UpdatedPos()

  it should "verify messages exchange between boid actors" in:
    val probe = testKit.createTestProbe[BoidMessage]()
    val boidManager = testKit.spawn[ManagerMessage](Behaviors.empty)
    val boidActor = testKit.spawn[BoidMessage](Behaviors.setup((context: ActorContext[BoidMessage]) =>
      BoidActor(boidManager, NBoids)))

    boidActor ! Ask(probe.ref)
    probe.expectMessageType[Send]

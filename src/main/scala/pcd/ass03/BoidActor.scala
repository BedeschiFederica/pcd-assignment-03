package pcd.ass03

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}

import scala.util.Random

trait BoidMessage
final case class SendBoids(boids: List[ActorRef[BoidMessage]]) extends BoidMessage
final case class UpdateVel(from: ActorRef[UpdatedVel]) extends BoidMessage
final case class UpdatePos(from: ActorRef[UpdatedPos]) extends BoidMessage
final case class UpdateView(from: ActorRef[UpdatedView]) extends BoidMessage
final case class SendPos(pos: P2d) extends BoidMessage
final case class SendVel(vel: V2d) extends BoidMessage
final case class AskPos(from: ActorRef[SendPos]) extends BoidMessage
final case class AskVel(from: ActorRef[SendVel]) extends BoidMessage

trait ManagerMessage
final case class UpdatedVel() extends ManagerMessage
final case class UpdatedPos() extends ManagerMessage
final case class UpdatedView() extends ManagerMessage

val NumBoids = 10

object BoidActor:
  def apply(manager: ActorRef[ManagerMessage]): Behavior[BoidMessage] =
    Behaviors.setup(context => new BoidActor().boidReceive)

class BoidActor():
  private var boids: List[ActorRef[BoidMessage]] = List.empty
  private var vel: V2d = V2d(Random.nextDouble(), Random.nextDouble())
  private var pos: P2d = P2d(Random.nextDouble(), Random.nextDouble())

  val boidReceive: Behavior[BoidMessage] = Behaviors.receive: (context, message) =>
    message match
      case SendBoids(boidsList) =>
        boids = boidsList
        Behaviors.same
      case UpdateVel(from) =>
        context.log.info(s"${context.self}: Updating vel, from $from")
        updateVelocity()
        from ! UpdatedVel()
        Behaviors.same
      case UpdatePos(from) =>
        context.log.info(s"${context.self}: Updating pos, from $from")
        updatePosition()
        from ! UpdatedPos()
        Behaviors.same
      case SendPos(pos) => ???
      case SendVel(vel) => ???
      case AskPos(from) =>
        from ! SendPos(pos)
        Behaviors.same
      case AskVel(from) =>
        from ! SendVel(vel)
        Behaviors.same

  def updateVelocity() = ???

  def updatePosition() = ???
  
  private def getNearbyBoids(): List[ActorRef[BoidMessage]] = ???

object ViewActor:
  def apply(): Behavior[BoidMessage] = Behaviors.receive: (context, message) =>
    message match
      case UpdateView(from) =>
        context.log.info(s"${context.self}: Updating view, from $from")
        from ! UpdatedView()
        Behaviors.same

object BoidsManager:
  def apply(): Behavior[ManagerMessage] = Behaviors.setup:
    context =>
      var boidActors: List[ActorRef[BoidMessage]] = List.empty
      for
        i <- 0 until NumBoids
      yield boidActors = context.spawnAnonymous(BoidActor(context.self)) +: boidActors
      boidActors.foreach:
        boid =>
          boid ! SendBoids(boidActors.filterNot(_ == boid))
          boid ! UpdateVel(context.self)
      new BoidsManager(context, boidActors).waitingVel

class BoidsManager(ctx: ActorContext[ManagerMessage], boids: List[ActorRef[BoidMessage]]):
  private var _counter = 0
  val viewActor: ActorRef[BoidMessage] = ctx.spawnAnonymous(ViewActor())

  val waitingVel: Behavior[ManagerMessage] =
    Behaviors.receiveMessagePartial:
      case UpdatedVel() =>
        _counter += 1
        ctx.log.info(s"BoidActor #$_counter has updated velocity")
        if _counter == NumBoids then
          _counter = 0
          boids.foreach(_ ! UpdatePos(ctx.self))
          waitingPos
        else
          Behaviors.same

  val waitingPos: Behavior[ManagerMessage] =
    Behaviors.receiveMessagePartial:
      case UpdatedPos() =>
        _counter += 1
        ctx.log.info(s"BoidActor #$_counter has updated position")
        if _counter == NumBoids then
          _counter = 0
          viewActor ! UpdateView(ctx.self)
          waitingView
        else
          Behaviors.same

  val waitingView: Behavior[ManagerMessage] =
    Behaviors.receiveMessagePartial:
      case UpdatedView() =>
        ctx.log.info(s"ViewActor has updated view")
        boids.foreach(_ ! UpdateVel(ctx.self))
        waitingVel

object BoidMain extends App:
  val system: ActorSystem[ManagerMessage] = ActorSystem(BoidsManager(), name = "boid-manager")
  Thread.sleep(5000)
  system.terminate()

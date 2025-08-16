package pcd.ass03

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}

trait Update
final case class UpdateVel(from: ActorRef[UpdatedVel]) extends Update
final case class UpdatePos(from: ActorRef[UpdatedPos]) extends Update
final case class UpdateView(from: ActorRef[UpdatedView]) extends Update

trait Updated
final case class UpdatedVel() extends Updated
final case class UpdatedPos() extends Updated
final case class UpdatedView() extends Updated

val NumBoids = 10

object BoidActor:
  def apply(): Behavior[Update] = Behaviors.receive: (context, message) =>
    message match
      case UpdateVel(from) =>
        context.log.info(s"${context.self}: Updating vel, from $from")
        from ! UpdatedVel()
        Behaviors.same
      case UpdatePos(from) =>
        context.log.info(s"${context.self}: Updating pos, from $from")
        from ! UpdatedPos()
        Behaviors.same

object ViewActor:
  def apply(): Behavior[Update] = Behaviors.receive: (context, message) =>
    message match
      case UpdateView(from) =>
        context.log.info(s"${context.self}: Updating view, from $from")
        from ! UpdatedView()
        Behaviors.same

object BoidsManager:
  def apply(): Behavior[Updated] = Behaviors.setup:
    context =>
      var boidActors: List[ActorRef[Update]] = List.empty
      for
        i <- 0 until NumBoids
      yield boidActors = context.spawnAnonymous(BoidActor()) +: boidActors
      boidActors.foreach(_ ! UpdateVel(context.self))
      new BoidsManager(context, boidActors).waitingVel

class BoidsManager(ctx: ActorContext[Updated], boids: List[ActorRef[Update]]):
  private var _counter = 0
  val viewActor: ActorRef[Update] = ctx.spawnAnonymous(ViewActor())

  val waitingVel: Behavior[Updated] =
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

  val waitingPos: Behavior[Updated] =
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

  val waitingView: Behavior[Updated] =
    Behaviors.receiveMessagePartial:
      case UpdatedView() =>
        _counter += 1
        ctx.log.info(s"ViewActor #$_counter has updated view")
        Behaviors.same

object BoidMain extends App:
  val system: ActorSystem[Updated] = ActorSystem(BoidsManager(), name = "boid-manager")
  Thread.sleep(5000)
  system.terminate()

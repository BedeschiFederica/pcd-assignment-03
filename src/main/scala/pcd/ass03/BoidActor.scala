package pcd.ass03

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}

trait Update
final case class UpdateVel(from: ActorRef[UpdatedVel]) extends Update
final case class UpdatePos(from: ActorRef[UpdatedPos]) extends Update

trait Updated
final case class UpdatedVel() extends Updated
final case class UpdatedPos() extends Updated

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

object BoidsManager:
  def apply(): Behavior[Updated] = Behaviors.setup:
    context =>
      val boidActor = context.spawn(BoidActor(), "boidActor")
      boidActor ! UpdateVel(context.self)
      Behaviors.same

  /*def apply(): Behavior[Updated] = Behaviors.receive: (context, message) =>
    message match
      case UpdatedVel() =>
        context.log.info("Updating vel")
        from ! UpdatedVel()
        Behaviors.same
      case UpdatedPos() =>
        context.log.info("Updating pos")
        from ! UpdatedPos()
        Behaviors.same*/

class BoidsManager
  /*val waiting: Behavior[Command] =
    Behaviors.receiveMessagePartial:
      case Think =>
        ctx.log.info("{} starts to think", name)
        startThinking(ctx, 5.seconds)*/

object BoidMain extends App:
  val system: ActorSystem[Updated] = ActorSystem(BoidsManager(), name = "boid-manager")
  //system ! HelloActor.Greet("Akka Typed", system.ignoreRef)
  Thread.sleep(5000)
  system.terminate()

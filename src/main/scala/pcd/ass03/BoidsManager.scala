package pcd.ass03

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}

import Configuration.*

trait ManagerMessage
final case class UpdatedVel() extends ManagerMessage
final case class UpdatedPos() extends ManagerMessage
final case class UpdatedView() extends ManagerMessage

object BoidsManager:
  def apply(): Behavior[ManagerMessage] = Behaviors.setup:
    context =>
      var boidActors: List[ActorRef[BoidMessage]] = List.empty
      for
        i <- 0 until NumBoids
      yield boidActors +:= context.spawnAnonymous(BoidActor(context.self))
      boidActors.foreach:
        boid =>
          boid ! SendBoids(boidActors.filterNot(_ == boid))
          boid ! UpdateVel(context.self)
      new BoidsManager(context, boidActors).waitingVel

  private class BoidsManager(ctx: ActorContext[ManagerMessage], boids: List[ActorRef[BoidMessage]]):
    private var counter = 0
    private val viewActor: ActorRef[BoidMessage] = ctx.spawnAnonymous(ViewActor())

    val waitingVel: Behavior[ManagerMessage] =
      Behaviors.receiveMessagePartial:
        case UpdatedVel() =>
          counter += 1
          ctx.log.info(s"BoidActor #$counter has updated velocity")
          if counter == NumBoids then
            counter = 0
            boids.foreach(_ ! UpdatePos(ctx.self))
            waitingPos
          else
            Behaviors.same

    private val waitingPos: Behavior[ManagerMessage] =
      Behaviors.receiveMessagePartial:
        case UpdatedPos() =>
          counter += 1
          ctx.log.info(s"BoidActor #$counter has updated position")
          if counter == NumBoids then
            counter = 0
            viewActor ! UpdateView(ctx.self)
            waitingView
          else
            Behaviors.same

    private val waitingView: Behavior[ManagerMessage] =
      Behaviors.receiveMessagePartial:
        case UpdatedView() =>
          ctx.log.info(s"ViewActor has updated view")
          boids.foreach(_ ! UpdateVel(ctx.self))
          waitingVel
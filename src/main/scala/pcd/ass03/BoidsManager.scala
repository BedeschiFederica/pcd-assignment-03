package pcd.ass03

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}

import Configuration.*

trait ManagerMessage
final case class UpdatedVel() extends ManagerMessage
final case class UpdatedPos() extends ManagerMessage
final case class UpdatedView() extends ManagerMessage
final case class Start(nBoids: Int) extends ManagerMessage

object BoidsManager:
  def apply(viewActor: ActorRef[ViewMessage]): Behavior[ManagerMessage] = Behaviors.receivePartial:
    (context, message) => message match
      case Start(nBoids) =>
        var boidActors: List[ActorRef[BoidMessage]] = List.empty
          for
            i <- 0 until nBoids
          yield boidActors +:= context.spawnAnonymous(BoidActor(context.self, nBoids))
          boidActors.foreach:
            boid =>
              boid ! SendBoids(boidActors.filterNot(_ == boid))
              boid ! UpdateVel(context.self)
          new BoidsManager(context, viewActor, boidActors, nBoids).waitingVel

  private class BoidsManager(ctx: ActorContext[ManagerMessage], viewActor: ActorRef[ViewMessage],
                             boids: List[ActorRef[BoidMessage]], nBoids: Int):
    private var counter = 0

    val waitingVel: Behavior[ManagerMessage] =
      Behaviors.receiveMessagePartial:
        case UpdatedVel() =>
          counter += 1
          ctx.log.info(s"BoidActor #$counter has updated velocity")
          if counter == nBoids then
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
          if counter == nBoids then
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
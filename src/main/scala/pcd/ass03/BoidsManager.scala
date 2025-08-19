package pcd.ass03

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}

import Configuration.*

trait ManagerMessage
final case class UpdatedVel() extends ManagerMessage
final case class UpdatedPos() extends ManagerMessage
final case class UpdatedView() extends ManagerMessage
final case class Start(nBoids: Int) extends ManagerMessage
final case class StopSimulation() extends ManagerMessage
final case class SuspendSimulation() extends ManagerMessage
final case class ResumeSimulation() extends ManagerMessage
final case class ChangeWeights(separation: Double, alignment: Double, cohesion: Double) extends ManagerMessage

object BoidsManager:
  def apply(viewActor: ActorRef[ViewMessage]): Behavior[ManagerMessage] = Behaviors.setup: context =>
    initialBehavior(viewActor, new BoidsManager(context, viewActor))

  private val initialBehavior: (ActorRef[ViewMessage], BoidsManager) => Behavior[ManagerMessage] =
    (viewActor, manager) => Behaviors.receivePartial:
      (context, message) => message match
        case Start(nBoids) =>
          context.log.info("START")
          var boidActors: List[ActorRef[BoidMessage]] = List.empty
          for
            i <- 0 until nBoids
          yield boidActors +:= context.spawnAnonymous(BoidActor(context.self, nBoids))
          viewActor ! InitDrawer(boidActors)
          boidActors.foreach:
            boid =>
              boid ! SendBoids(boidActors.filterNot(_ == boid))
              boid ! UpdateVel(context.self)
          manager.boids = boidActors
          manager.waitingVel
        case _ => Behaviors.same

  private class BoidsManager(ctx: ActorContext[ManagerMessage], viewActor: ActorRef[ViewMessage]):
    private var _boids: List[ActorRef[BoidMessage]] = List.empty
    private var counter = 0

    def boids: List[ActorRef[BoidMessage]] = _boids
    def boids_=(boids: List[ActorRef[BoidMessage]]): Unit = _boids = boids

    private def nBoids: Int = _boids.size

    val waitingVel: Behavior[ManagerMessage] =
      Behaviors.receiveMessagePartial:
        case UpdatedVel() =>
          counter += 1
          //ctx.log.info(s"BoidActor #$counter has updated velocity")
          if counter == nBoids then
            counter = 0
            boids.foreach(_ ! UpdatePos(ctx.self))
            waitingPos
          else
            Behaviors.same
        case StopSimulation() => stopSimulation()
        case SuspendSimulation() => suspend(waitingVel, UpdatedVel())
        case ChangeWeights(separation, alignment, cohesion) =>
          boids.foreach(_ ! UpdateWeights(separation, alignment, cohesion))
          Behaviors.same

    private val suspend: (Behavior[ManagerMessage], ManagerMessage) => Behavior[ManagerMessage] = (oldState, message) =>
      Behaviors.withStash[ManagerMessage](nBoids): stash =>
        Behaviors.receiveMessagePartial:
          case ResumeSimulation() => stash.unstashAll(oldState)
          case StopSimulation() => stopSimulation()
          case message =>
            stash.stash(message)
            Behaviors.same

    private val waitingPos: Behavior[ManagerMessage] =
      Behaviors.receiveMessagePartial:
        case UpdatedPos() =>
          counter += 1
          //ctx.log.info(s"BoidActor #$counter has updated position")
          if counter == nBoids then
            counter = 0
            viewActor ! UpdateView(ctx.self)
            waitingView
          else
            Behaviors.same
        case StopSimulation() => stopSimulation()
        case SuspendSimulation() => suspend(waitingPos, UpdatedPos())
        case ChangeWeights(separation, alignment, cohesion) =>
          boids.foreach(_ ! UpdateWeights(separation, alignment, cohesion))
          Behaviors.same

    private val waitingView: Behavior[ManagerMessage] =
      Behaviors.receiveMessagePartial:
        case UpdatedView() =>
          //ctx.log.info(s"ViewActor has updated view")
          boids.foreach(_ ! UpdateVel(ctx.self))
          waitingVel
        case StopSimulation() => stopSimulation()
        case SuspendSimulation() => suspend(waitingView, UpdatedView())
        case ChangeWeights(separation, alignment, cohesion) =>
          boids.foreach(_ ! UpdateWeights(separation, alignment, cohesion))
          Behaviors.same

    private def stopSimulation(): Behavior[ManagerMessage] =
      ctx.log.info(s"STOP")
      boids.foreach(ctx.stop)
      initialBehavior(viewActor, this)
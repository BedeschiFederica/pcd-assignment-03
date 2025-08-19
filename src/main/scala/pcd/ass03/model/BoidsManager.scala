package pcd.ass03.model

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}
import pcd.ass03.*
import pcd.ass03.view.{InitDrawer, UpdateView, ViewMessage}

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
          var boidActors: List[ActorRef[BoidMessage]] = List.empty
          for
            i <- 0 until nBoids
          yield boidActors +:= context.spawnAnonymous(BoidActor(context.self, nBoids))
          viewActor ! InitDrawer(boidActors)
          boidActors.foreach: boid =>
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
      Behaviors.receiveMessage:
        case UpdatedVel() =>
          counter += 1
          if counter == nBoids then
            counter = 0
            boids.foreach(_ ! UpdatePos(ctx.self))
            waitingPos
          else
            Behaviors.same
        case message => handleCommonMessages(waitingVel)(message)

    private val waitingPos: Behavior[ManagerMessage] =
      Behaviors.receiveMessagePartial:
        case UpdatedPos() =>
          counter += 1
          if counter == nBoids then
            counter = 0
            viewActor ! UpdateView(ctx.self)
            waitingView
          else
            Behaviors.same
        case message => handleCommonMessages(waitingPos)(message)

    private val waitingView: Behavior[ManagerMessage] =
      Behaviors.receiveMessage:
        case UpdatedView() =>
          boids.foreach(_ ! UpdateVel(ctx.self))
          waitingVel
        case message => handleCommonMessages(waitingView)(message)

    private val suspend: Behavior[ManagerMessage] => Behavior[ManagerMessage] = oldState =>
      Behaviors.withStash[ManagerMessage](nBoids * 2): stash =>
        Behaviors.receiveMessagePartial:
          case ResumeSimulation() => stash.unstashAll(oldState)
          case StopSimulation() => stopSimulation()
          case message =>
            stash.stash(message)
            Behaviors.same

    private def handleCommonMessages(oldState: Behavior[ManagerMessage]): ManagerMessage => Behavior[ManagerMessage] =
      case StopSimulation() => stopSimulation()
      case SuspendSimulation() => suspend(oldState)
      case ChangeWeights(separation, alignment, cohesion) =>
        boids.foreach(_ ! UpdateWeights(separation, alignment, cohesion))
        oldState
      case _ => Behaviors.unhandled

    private def stopSimulation(): Behavior[ManagerMessage] =
      boids.foreach(ctx.stop)
      initialBehavior(viewActor, this)

package pcd.ass03

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}

import scala.util.Random

trait BoidMessage
final case class SendBoids(boids: List[ActorRef[BoidMessage]]) extends BoidMessage
final case class UpdateVel(from: ActorRef[ManagerMessage]) extends BoidMessage
final case class UpdatePos(from: ActorRef[ManagerMessage]) extends BoidMessage
final case class UpdateView(from: ActorRef[ManagerMessage]) extends BoidMessage
final case class Send(pos: P2d, vel: V2d, from: ActorRef[BoidMessage]) extends BoidMessage
final case class Ask(from: ActorRef[BoidMessage]) extends BoidMessage

trait ManagerMessage
final case class UpdatedVel() extends ManagerMessage
final case class UpdatedPos() extends ManagerMessage
final case class UpdatedView() extends ManagerMessage

val NumBoids = 10
val PerceptionRadius = 50.0
val AvoidRadius = 20.0
val SeparationWeight = 1.0
val AlignmentWeight = 1.0
val CohesionWeight = 1.0
val MaxSpeed = 4.0
val EnvironmentWidth = 1000
val EnvironmentHeight = 1000
val MinX = -EnvironmentWidth / 2
val MaxX = EnvironmentWidth / 2
val MinY = -EnvironmentHeight / 2
val MaxY = EnvironmentHeight / 2

object BoidActor:
  def apply(manager: ActorRef[ManagerMessage]): Behavior[BoidMessage] =
    Behaviors.setup(context => new BoidActor().boidReceive)

class BoidActor:
  private var boids: List[ActorRef[BoidMessage]] = List.empty
  private var vel: V2d = V2d(Random.nextDouble(), Random.nextDouble())
  private var pos: P2d = P2d(Random.nextDouble(), Random.nextDouble())
  private var nearbyBoids: Map[ActorRef[BoidMessage], (P2d, V2d)] = Map.empty
  private var counter = 0

  val boidReceive: Behavior[BoidMessage] = Behaviors.receive: (context, message) =>
    message match
      case SendBoids(boidsList) =>
        boids = boidsList
        Behaviors.same
      case UpdateVel(from) =>
        context.log.info(s"${context.self}: Updating vel, from $from")
        nearbyBoids = Map.empty
        boids.foreach(_ ! Ask(context.self))
        getNearbyBoids(from)
      case UpdatePos(from) =>
        context.log.info(s"${context.self}: Updating pos, from $from")
        updatePosition()
        from ! UpdatedPos()
        Behaviors.same
      case Ask(from) =>
        from ! Send(pos, vel, context.self)
        Behaviors.same
  
  private val getNearbyBoids: ActorRef[ManagerMessage] => Behavior[BoidMessage] = from =>
    Behaviors.receive: (context, message) =>
      message match
        case Send(otherPos, otherVel, otherBoid) =>
          counter += 1
          val distance = pos.distance(otherPos)
          if distance < PerceptionRadius then
            nearbyBoids += (otherBoid, (otherPos, otherVel))
          if counter == NumBoids - 1 then
            counter = 0
            updateVelocity()
            context.log.info(s"new vel: $vel")
            from ! UpdatedVel()
            boidReceive
          else
            Behaviors.same
        case Ask(from) =>
          from ! Send(pos, vel, context.self)
          Behaviors.same

  private def updateVelocity(): Unit =
    val separation = calculateSeparation()
    val alignment = calculateAlignment()
    val cohesion = calculateCohesion()
    vel = vel + (alignment * AlignmentWeight) + (separation * SeparationWeight) + (cohesion * CohesionWeight)
    if vel.abs > MaxSpeed then
      vel = vel.getNormalized * MaxSpeed

  private def updatePosition(): Unit =
    pos += vel
    pos += (
      pos match
        case P2d(x, y) if x < MinX => V2d(EnvironmentWidth, 0)
        case P2d(x, y) if x >= MaxX => V2d(-EnvironmentWidth, 0)
        case P2d(x, y) if y < MinY => V2d(0, EnvironmentHeight)
        case P2d(x, y) if y >= MaxY => V2d(0, -EnvironmentHeight)
        case _ => V2d(0, 0)
    )

  private def calculateSeparation(): V2d =
    var dx: Double = 0
    var dy: Double = 0
    var count = 0
    nearbyBoids.foreach:
      boid =>
        val otherPos = boid._2._1
        if pos.distance(otherPos) < AvoidRadius then
          dx += pos.x - otherPos.x
          dy += pos.y - otherPos.y
          count += 1
    if count > 0 then
      dx /= count
      dy /= count
      V2d(dx, dy).getNormalized
    else
      V2d(0, 0)

  private def calculateAlignment(): V2d =
    var avgVx: Double = 0
    var avgVy: Double = 0
    if nearbyBoids.nonEmpty then
      nearbyBoids.foreach:
        boid =>
          val otherVel = boid._2._2
          avgVx += otherVel.x
          avgVy += otherVel.y
      avgVx /= nearbyBoids.size
      avgVy /= nearbyBoids.size
      V2d(avgVx - vel.x, avgVy - vel.y).getNormalized
    else
      V2d(0, 0)

  private def calculateCohesion(): V2d =
    var centerX: Double = 0
    var centerY: Double = 0
    if nearbyBoids.nonEmpty then
      nearbyBoids.foreach:
        boid =>
          val otherPos = boid._2._1
          centerX += otherPos.x
          centerY += otherPos.y
      centerX /= nearbyBoids.size
      centerY /= nearbyBoids.size
      V2d(centerX - pos.x, centerY - pos.y).getNormalized
    else
      V2d(0, 0)

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
      yield boidActors +:= context.spawnAnonymous(BoidActor(context.self))
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

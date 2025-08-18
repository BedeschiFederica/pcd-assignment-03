package pcd.ass03

import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}

import scala.util.Random

import Configuration.*

trait BoidMessage
final case class SendBoids(boids: List[ActorRef[BoidMessage]]) extends BoidMessage
final case class UpdateVel(from: ActorRef[ManagerMessage]) extends BoidMessage
final case class UpdatePos(from: ActorRef[ManagerMessage]) extends BoidMessage

trait BoidDrawMessage extends BoidMessage with DrawMessage
final case class Send(pos: P2d, vel: V2d, from: ActorRef[BoidDrawMessage]) extends BoidDrawMessage
final case class Ask(from: ActorRef[BoidDrawMessage]) extends BoidDrawMessage

object BoidActor:
  def apply(manager: ActorRef[ManagerMessage], nBoids: Int): Behavior[BoidMessage] =
    Behaviors.setup(context => new BoidActor(context, nBoids).boidReceive)

  private class BoidActor(ctx: ActorContext[BoidMessage], nBoids: Int):
    private var boids: List[ActorRef[BoidMessage]] = List.empty
    private var vel: V2d = V2d(Random.nextDouble(), Random.nextDouble())
    private var pos: P2d = P2d(Random.nextDouble(), Random.nextDouble())
    private var nearbyBoids: Map[ActorRef[BoidDrawMessage], (P2d, V2d)] = Map.empty
    private var counter = 0

    val boidReceive: Behavior[BoidMessage] = Behaviors.receiveMessagePartial:
      case SendBoids(boidsList) =>
        boids = boidsList
        Behaviors.same
      case UpdateVel(from) =>
        ctx.log.info(s"${ctx.self}: Updating vel, from $from")
        nearbyBoids = Map.empty
        boids.foreach(_ ! Ask(ctx.self))
        getNearbyBoids(from)
      case UpdatePos(from) =>
        ctx.log.info(s"${ctx.self}: Updating pos, from $from")
        updatePosition()
        from ! UpdatedPos()
        Behaviors.same
      case Ask(from) =>
        from ! Send(pos, vel, ctx.self)
        Behaviors.same

    private val getNearbyBoids: ActorRef[ManagerMessage] => Behavior[BoidMessage] = from =>
      Behaviors.receiveMessagePartial:
        case Send(otherPos, otherVel, otherBoid) =>
          counter += 1
          val distance = pos.distance(otherPos)
          if distance < PerceptionRadius then
            nearbyBoids += (otherBoid, (otherPos, otherVel))
          if counter == nBoids - 1 then
            counter = 0
            updateVelocity()
            ctx.log.info(s"new vel: $vel")
            from ! UpdatedVel()
            boidReceive
          else
            Behaviors.same
        case Ask(from) =>
          from ! Send(pos, vel, ctx.self)
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

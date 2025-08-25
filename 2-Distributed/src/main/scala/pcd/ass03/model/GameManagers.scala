package pcd.ass03.model

import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import pcd.ass03.Message
import pcd.ass03.model.WorldManager.WorldMessage

object EatingManager:
  import WorldMessage.UpdatedWorld

  trait EatingManagerMessage extends Message
  object EatingManagerMessage:
    case class UpdateWorld(world: World, from: ActorRef[WorldMessage]) extends EatingManagerMessage

  import EatingManagerMessage.*

  val Service: ServiceKey[EatingManagerMessage] = ServiceKey[EatingManagerMessage]("FoodManagerService")
  def apply(): Behavior[EatingManagerMessage] = Behaviors.setup: context =>
    context.system.receptionist ! Receptionist.Register(Service, context.self)
    Behaviors.receiveMessagePartial:
      case UpdateWorld(newWorld, from) =>
        from ! UpdatedWorld(EatingLogic.updateWorld(newWorld))
        Behaviors.same

object EndGameManager:
  import WorldMessage.EndGame

  trait EndGameManagerMessage extends Message
  object EndGameManagerMessage:
    case class CheckEndGame(world: World, from: ActorRef[WorldMessage]) extends EndGameManagerMessage

  import EndGameManagerMessage.*

  val Service: ServiceKey[EndGameManagerMessage] = ServiceKey[EndGameManagerMessage]("EndGameManagerService")
  def apply(): Behavior[EndGameManagerMessage] = Behaviors.setup: context =>
    context.system.receptionist ! Receptionist.Register(Service, context.self)
    Behaviors.receiveMessagePartial:
      case CheckEndGame(world, from) =>
        val winner = EndGameLogic.getGameWinner(world.players)
        winner.foreach(from ! EndGame(_))
        Behaviors.same

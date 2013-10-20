package uk.co.sprily.scalaTalk

import scala.concurrent.duration._

import akka.actor.{Actor, ActorRef, FSM, LoggingFSM, ActorLogging, Props}

/**
 * Received messages
 */
sealed trait RcvMsg
case object OpenBidding extends RcvMsg
case class  Bid(amount: Double) extends RcvMsg
case object CloseBidding extends RcvMsg

/**
 * External state
 */
sealed trait DealState
case object Draft extends DealState
case object AcceptingOffers extends DealState

/**
 * Internal state
 */
case class Ledger(reserve: Double, offers: Map[ActorRef, Bid])

/**
 * Result messages
 */
sealed trait ResultMsg
case class AuctionWon(product: Any) extends ResultMsg
case class AuctionLost(product: Any) extends ResultMsg

class Auction(product: Any,
              reserve: Double,
              auctioneer: ActorRef) extends Actor
                                       with ActorLogging
                                       with FSM[DealState, Ledger] {

  startWith(Draft, Ledger(reserve, Map.empty))

  when(Draft) {
    case Event(OpenBidding, ledger) =>
      if (sender == auctioneer) {
        goto(AcceptingOffers) using ledger forMax(10.seconds)
      } else {
        stay
      }
  }

  when(AcceptingOffers) {
    case Event(b: Bid, ledger) =>
      stay using (ledger.copy(offers = ledger.offers + (sender -> b))) forMax(10.seconds)

    case Event(CloseBidding, ledger) =>
      if (sender == auctioneer) {
        stop
      } else {
        stay
      }

    case Event(StateTimeout, ledger) =>
      log.warning("Timed out")
      stop
  }

  whenUnhandled {
    case Event(e,s) =>
      log.warning("received unhandled message {} in state {}/{}",
                    e, stateName, s)
      stay
  }

  onTermination {
    case StopEvent(FSM.Normal, _, ledger) => {
      winner(ledger) match {
        case None    => notifyLoss(ledger.offers.keys.toSeq, product)
        case Some(w) =>
          notifyLoss(ledger.offers.keys.filter(_ != w).toSeq, product)
          notifyWin(w, product)
      }
    }
  }

  private def winner(ledger: Ledger): Option[ActorRef] = {
    ledger.offers.filter { case (actor, bid) =>
      bid.amount >= reserve
    }.toList
     .sortBy(_._2.amount)
     .lastOption
     .map(_._1)
  }

  private def notifyLoss(actors: Seq[ActorRef], product: Any) = {
    notify(actors, AuctionLost(product))
  }

  private def notifyWin(actor: ActorRef, product: Any) = {
    log.warning(s"Notifying ${actor} of win")
    notify(List(actor), AuctionWon(product))
  }

  private def notify(actors: Seq[ActorRef], msg: ResultMsg) = {
    actors.foreach { a =>
      log.warning(s"Notifying ${a} of loss")
      a ! msg
    }
  }

  /**
   * Start up the actor, including transition into initial state
   */
  initialize()

}

sealed trait AuctioneerCommand
case class CreateAuction(product: Any, reserve: Double) extends AuctioneerCommand
case class BidOn(product: Any, amount: Double) extends AuctioneerCommand
case object AuctionClosed extends AuctioneerCommand

class Auctioneer extends Actor {

  private var auctions: Map[Any, ActorRef] = Map.empty

  def receive = {
    case CreateAuction(product, reserve) =>
      val auction = context.actorOf(Props(new Auction(product, reserve, self)))
      auctions += (product -> auction)
      auction ! FSM.SubscribeTransitionCallBack(self)
      auction ! OpenBidding

    case AuctionClosed =>
      auctions = auctions.filterNot { case (p, a) => a == sender }

    case BidOn(product, amount) =>
      auctions.get(product) match {
        case Some(auction) => auction forward Bid(amount)
        case _             => {}
      }
  }

}

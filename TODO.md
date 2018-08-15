# TODO
* tryAttach needs to know when *visually* the attach will happen, namely 1 or 2 ticks after the intent started, depending on the duration of the EntityMovedEvent. This is so properly delayed events can be scheduled on attach,
e.g. BalloonPopped should happen when a move *completes*, not when it starts. (Probably applies to other Tile-/EntityBehavior functions too.)

# Open issues

## BalloonColored/BalloonPopped - in tryAttach or in update?
* emitting this in tryAttach guarantees that nothing (i.e. no other intent) can prevent the balloon being colored/popped, but we need to schedule it for the right time

## When to schedule TileUpdateIntents?
* What event sequence do we expect here if the player moves right? Think of sound effects... what would feel right?
  ```
  [button*player][button]
  ```
  * (0: ButtonReleased)(0: ButtonPressed)(0..1:   EntityMoved) - no, but that's how it currently works
  * (0..1: EntityMoved)(1: ButtonReleased)(1:   ButtonPressed) - feels weird
  * (0: ButtonReleased)(0..1: EntityMoved)(1:   ButtonPressed) - the best option, but then   EntityDetached (time 0) and EntityAttached (time 1)   must yield affected points, not EntityMoved

* (tile updates need to be scheduled for the time when an event *completes*, not when it *starts* or "when it starts + 1", so the tiles involved in an entity move are updated at the right time)

* tile updates need the ability to schedule intents. This way we could realize delayed teleportation: A player can move onto a teleporter and wait to be teleported, but the player could also move *over* the teleporter without being teleported. We could realize this with a *IntentScheduledEvent { intent, time }*.

  * this could simplify updating dependent tiles: When handling a TileUpdateIntent, just emit *IntentScheduledEvent { intent = TileUpdateIntent ... }* for all tiles depending on the updated tile

  * this could allow us to get rid of the `=||=>` operator... MovePlayerIntent would just emit a *PlayerRotatedEvent* and a *IntentScheduledEvent { intent = MoveEntityIntent ... }*

* tile updates then also must be able to cancel their scheduled intents (in case the player moves away from the teleporter, the TeleportIntent must be canceled, otherwise the next entity stepping onto the teleporter a little later would immediately be teleported - we had this problem in OrangeBugReloaded). Does this require a field TileEntry.scheduledIntents, similar to dynamicDependencies? Or could 'cancellation kind' be part of the scheduled intent?

## What about the order of scheduled events?
* When moving onto a teleporter, a TileUpdateIntent is scheduled for time 't' to update the teleporter and eventually teleport the player. Suppose I press an arrow key quickly so we also schedule a MovePlayerIntent for time 't'. Should the MovePlayerIntent be handled first, thus allowing me to "escape" the teleporter and avoid teleportation? Probably not!

## Why do scheduled events even have a duration?
* We could also model non-instantaneous events as, for example, GateOpeningEvent + GateOpenedEvent
* But that way we wouldn't keep the GateTile locked all the time
* Which event produces the TileUpdateEffect to actually open the gate?
* After all, duration shouldn't be that hard to handle
    * process events at their start time, but keep them in queue (i.e. keep tiles locked)
    * unlock and update events at their end time
{-# LANGUAGE TypeFamilies #-}

{- | Description: Actor pattern utilities. -}
module OM.Actor (

  {- * Implementing an actor. -}
  {- | Types/functions in this section help you implement the actor iteself. -}
  Actor(..),
  Responder,
  Responded,
  respond,

  {- * Communicating with an actor. -}
  {- | These functions allow you to communicate with an actor. -}
  call,
  cast,

  {- * Example. -}
  -- $example
) where

import Control.Concurrent (Chan, newEmptyMVar, putMVar, takeMVar,
  writeChan)
import Control.Monad.IO.Class (MonadIO, liftIO)

{- $example
  This is an example actor that consist of a thread looping over the messages
  received in a 'Chan' and responding to each one.

  Note that 'Chan' is already an instance of 'Actor'.

  > import Control.Concurrent (forkIO)
  > import OM.Actor (Responder, respond, call)
  >
  > data Message
  >   = AddOne Int (Responder Int)
  >   = IntToString Int (Responder String)
  > 
  > startActor :: IO (Chan Message)
  > startActor = do
  >     chan <- newChan
  >     forkIO (loop chan)
  >     return chan
  >   where
  >     loop :: Chan Message -> IO void
  >     loop chan = do
  >       readChan >>= handleMessage
  >       loop
  > 
  >     handleMessage :: Msg -> IO Responded
  >     handleMessage (AddOne n responder) =
  >       respond responder (n + 1)
  >     handleMessage (IntToString n responder) =
  >       respond responder (show n)
  > 
  > main :: IO ()
  > main = do
  >   actor <- startActor
  >   print =<< call actor (AddOne 41)
  >   print =<< call actor (IntToString 42)
-}


{- |
  An opaque data structure given to the actor that allows it to respond
  to a message.
-}
newtype Responder a = Responder {
    unResponder :: a -> IO ()
  }
instance Show (Responder a) where
  show _ = "<Responder>"


{- | The class of types that can act as the handle for an asynchronous actor. -}
class Actor a where
  {- | The type of messages that can be sent to the actor. -}
  type Msg a
  {- | The channel through which messages can be sent to the actor. -}
  actorChan :: a -> Msg a -> IO ()
instance Actor (Chan m) where
  type Msg (Chan m) = m
  actorChan = writeChan


{- | As an actor, respond to an asynchronous message. -}
respond :: (MonadIO m) => Responder a -> a -> m Responded
respond responder val = do
  liftIO (unResponder responder val)
  return Responded


{- |
  Send a message to an actor, and wait for a response.

  The second argument, @mkMessage@ tells 'call' how to construct a message,
  given a responder. You will typically want to package that responder into the
  message itself, so that whatever actor is processing the messages has a way
  to respond.

  For instance, you message may look like this:

  > data Message
  >   = AddOne Int (Responder Int)
  >   | IntToString Int (Responder String)

  Then you can use 'call' like so:

  > call actor (AddOne 1) :: IO Int
  >
  > call actor (IntToString 1) :: IO String

-}
call
  :: (Actor actor, MonadIO m)
  => actor {- ^ The actor to which to send the message. -}
  -> (Responder a -> Msg actor) 
     {- ^ @mkMessage@: How to construct the message, given a responder. -}
  -> m a
call actor mkMessage = liftIO $ do
  mVar <- newEmptyMVar
  actorChan actor (mkMessage (Responder (putMVar mVar)))
  takeMVar mVar


{- | Send a message to an actor, but do not wait for a response. -}
cast :: (Actor actor, MonadIO m) => actor -> Msg actor -> m ()
cast actor = liftIO . actorChan actor


{- |
  Proof that 'respond' was called. Actor implementations can use this in
  a type signature when they require that 'respond' be called at least
  once, because calling 'respond' is the only way to generate values of
  this type. For actors that respond to messages, this helps the compiler
  ensure that a response is in fact generated for every case.
-}
data Responded = Responded


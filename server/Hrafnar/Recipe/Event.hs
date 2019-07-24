module Hrafnar.Recipe.Event(Event) where

import           Data.Extensible

-- | Event.
type Event = Variant
  '[ "odin" >: ()
   , "hrafnar" >: ()
   , "local" >: LocalEvent
   , "init" >: ()
   , "noop" >: ()
   , "testEvent" >: ()
   ]

type LocalEvent = Variant
  '[ "exec" >: String
   ]

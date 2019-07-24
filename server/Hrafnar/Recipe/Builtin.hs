{-|
Description : Built-ins for recipe.
Module      : Hrafnar.Recipe.Builtin
Copyright   : REALGLOBE INC. (c) REALGLOBE 2018
License     : BSD3

Maintainer  : REALGLOBE INC.
-}

module Hrafnar.Recipe.Builtin
  ( builtins
  , ops
  , operators
  )
where

import           Hrafnar.Recipe.Core
import           Hrafnar.Recipe.Types

import           Control.Lens         hiding (List)
import qualified Data.Map.Strict      as MA
import           GHC.IO.Handle        (hGetContents, hPutStr)
import           System.Process       (CreateProcess (..), StdStream (..), createProcess, shell)

--import           Debug.Trace

builtins :: MA.Map String (Value, Scheme)
builtins = MA.fromList
  [ ("id", (id_, Forall [TV "a"] (TyFun (TyVar $ TV "a") (TyVar $ TV "a"))))
  , ("eq", (eq_, Forall [TV "a"] (TyFun (TyVar $ TV "a") (TyFun (TyVar $ TV "a") (TyVar $ TV "a")))))
  , ("add", (add_, Forall [] (TyFun tyInt (TyFun tyInt tyInt))))
  , ("+", (add_, Forall [] (TyFun tyInt (TyFun tyInt tyInt))))
  ]

ops :: [(String, Int, OpAssoc, Value)]
ops = [("+", 6, OpL, add_)]

operators :: MA.Map String (Int, OpAssoc)
operators = MA.fromList $ fmap (\(k, p, a, _) -> (k, (p, a))) ops

-- | Identifical function
id_ :: Value
id_ = combI

-- | Equal
eq_ :: Value
eq_ = VLam $ \x -> VLam $ \y -> VBool $ x == y

-- | Addition
add_ :: Value
add_ = VLam $ \(VInt i) -> VLam $ \(VInt i') -> VInt $ i + i'

-- | Exec external command.
--   This function is experimental.
exec_ ::  Value
exec_ = VLam $ \(VString cmd) -> VLam $ \(VString input) -> VEff . Effect $  do
  (Just inHandle, Just outHandle, _, _) <-
    createProcess $ (shell cmd){ std_out = CreatePipe, std_in = CreatePipe }
  hPutStr inHandle input
  output <- hGetContents outHandle
  pure $ #local # #exec # output
{-
ops :: [(String, Int, OpAssoc, Func2)]
ops =
  [ ("+", 6, OpL, add_)
  , ("*", 7, OpL, multi_)
  ]

-- | Precedence and correspond function of operators.
operators :: MA.Map String (Int, OpAssoc)
operators = MA.fromList $ fmap (\(k, p, a, _) -> (k, (p, a))) ops

-- | Built-in @Implant@s.
builtins :: ExpTable
builtins = MA.fromList $
  [ ("id", Func id_ )
  , ("eval", Func eval_ )
  , ("eq", Func2 eq_ )
  , ("add", Func2 add_)
  , ("multi", Func2 multi_)
  , ("if", Func3 if_)
  , ("map", Func2 map_)
  , ("guard", Func guard_)
  , ("tryCases", Func tryCases_)
  , ("lookup", Func2 lookup_)
  , ("upsert", Func3 upsert_)
  , ("context", Val context_)
  , ("event", Val event_)
  , ("modify", Func2 modify_)
  , ("modifyDict", Func modifyDict_)
  , ("call", Func call_)
  , ("whenOdin", Val whenOdin_)
  , ("odinHandler", Func odinHandler_)
  , ("testEventHandler", Func testEventHandler_)
  ] <> fmap (\(k, _, _, f) -> (ST.pack k, Func2 f)) ops


-- | Eval @Exp@ in @Exp@.
eval_ :: Func
eval_ = eval



-- | Multiplication
multi_ :: Func2
multi_ (Number x) (Number y) = pure . Number $ x * y
multi_ x@(Apply _ _) y       = eval x >>= multi_ y
multi_ (Val x) y             = x >>= multi_ y
multi_ x y@(Apply _ _)       = eval y >>= multi_ x
multi_ x (Val y)             = y >>= multi_ x
multi_ x y                   = throw $ TypeError "multi / unmatched types. " [x, y]



-- | Map
map_ :: Func2
map_ (Func f) (List xs) = List <$> mapM f xs
map_ exp@(Apply _ _) xs = eval exp >>= flip map_ xs
map_ (Val exp) xs       = exp >>= flip map_ xs
map_ f exp@(Apply _ _)  = eval exp >>= map_ f
map_ f (Val exp)        = exp >>= map_ f
map_ f x                = throw $ TypeError "map / require Func and List. " [f, x]

-- | Guard
guard_ :: Func
guard_ (List (Tuple (Cons cond (Cons exp Nil)):xs)) = eval cond >>= \case
  Bool cond' -> if cond' then eval exp else guard_ (List xs)
  cond -> throw $ TypeError "guard / condition must be Bool" [cond]

guard_ (List []) = throwString "guard / pattern exhausted"

guard_ exp@(Apply _ _) = eval exp >>= guard_
guard_ (Val exp) = exp >>= guard_
guard_ cond = throw $ TypeError "guard / 1st argument must be List" [cond]

-- | Try cases may fail. Return @Exp@ which success to be eval first.
--   If all cases fail, throw exception.
tryCases_ :: Func
tryCases_ (List (x:xs)) =
  tryAny (eval x) >>= \case
  Right r -> pure r
  Left _ -> tryCases_ (List xs)
tryCases_ (List []) = pure Null
tryCases_ x = throw $ TypeError "tryCases / 1st argument must be List" [x]

-- | Look up @Exp@ from Dict.
lookup_ :: Func2
lookup_ (Text k) (Dict d) =
  case MA.lookup k d of
    Just v  -> pure v
    Nothing -> throwString "fail to lokk up"
lookup_ e@(Apply _ _) d = eval e >>= flip lookup_ d
lookup_ (Val e) d = e >>= flip lookup_ d
lookup_ k e@(Apply _ _) = eval e >>= lookup_ k
lookup_ k (Val e) = e >>= lookup_ k
lookup_ k _ = throw $ KeyError "loookup / invalid key" [k]

-- | Insert @Exp@ to Dict. If the key exists already, overwrite value.
upsert_ :: Func3
upsert_ (Text k) v (Dict d) = pure . Dict $ MA.insert k v d

upsert_ e@(Apply _ _) v d   = eval e >>= \k -> upsert_ k v d
upsert_ (Val e) v d         = e  >>= \k -> upsert_ k v d

upsert_ k e@(Apply _ _) d   = eval e >>= \v -> upsert_ k v d
upsert_ k (Val e) d         = e  >>= \v -> upsert_ k v d

upsert_ k v e@(Apply _ _)   = eval e >>= \d -> upsert_ k v d
upsert_ k v (Val e)         = e  >>= \d -> upsert_ k v d

upsert_ k _ _               = throw $ KeyError "upsert / invalid key" k

-- | Get @Context@.
context_ :: Val
context_ = asks (Dict . view #context)

-- | Modify a value in the new context.
--   NOT modify given context.
--   If the key doesn't exist in given context, throw exception.
modify_ :: Func2
modify_ (Text k) v = do
  ask <&> MA.lookup k . view #context >>=  \case
    Just _ -> pure ()
    _ -> throwString $ "modify / \"" <> ST.unpack k <> "\" is not found in given context."
  tell $  Endo (\(c, ef) -> (MA.insert k v c, ef))
  pure $ Tuple Nil
modify_ exp@(Apply _ _) v = eval exp >>= flip modify_ v
modify_ (Val exp) v = exp >>= flip modify_ v
modify_ k _ = throw $ TypeError "modify / key must be Text. " [k]

-- | Modify the new context together with Dict.
modifyDict_ :: Func
modifyDict_ (Dict kv) = do
  mapM_ (\(k, v) -> modify_ (Text k) v) $ MA.toList kv
  pure $ Tuple Nil
modifyDict_ x = throw $ TypeError "modifyDict / requires Dict" [x]


-- | Get @Event@.
event_ :: Val
event_ = asks (Event . view #event)

whenOdin_ :: Val
whenOdin_ = do
  ev <- event_ >>= \case
    Event ev' -> pure ev'
    _ -> throwString "whenOdin / something wrong"
  case strike @(Field Identity) @("odin" >: Odin) ev of
    Just _  -> pure $ Bool True
    Nothing -> pure $ Bool False

odinHandler_ :: Func
odinHandler_ (Dict kh) = do
  ev <- event_ >>= \case
    Event ev' -> pure ev'
    _ -> throwString "odinHandler / something wrong"
  odin <- case strike @(Field Identity) @("odin" >: Odin) ev of
    Just (Field (Identity x)) -> pure x
    Nothing                   -> throwString "not odin"
  case odin of
    OdinWebhook service body -> do
      h <- case MA.lookup "webhook" kh of
        Just (Func2 f) -> pure f
        _              -> throwString "odinHandler / handler not found"
      h (Text $ STE.decodeUtf8 service) (jsonToExp body)
    _ -> throwString "not implemented"
odinHandler_ x = throw $ TypeError "odinHandler / require Dict of Func" [x]

testEventHandler_ :: Func
testEventHandler_ exp = do
  ev <- event_ >>= \case
    Event ev' -> pure ev'
    _ -> throwString "testEventHandler / something wrong"
  case strike @(Field Identity) @("testEvent" >: ()) ev of
    Just _  -> eval exp
    Nothing -> throwString "not testEvent"

-}

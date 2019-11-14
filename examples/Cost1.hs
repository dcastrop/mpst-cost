module Cost1 where

import Language.SessionTypes.Cost

example1 :: CGT
example1 = gclose $ do
  p <- mkRole
  q <- mkRole
  grec 3 $ \x -> do
    message p q (Var "s1") (CVar "c1")
    x

example2 :: CGT
example2 = gclose $ do
  p <- mkRole
  q <- mkRole
  grec 3 $ \x -> do
    message p q (Var "s1") (CVar "c1")
    message q p (Var "s2") (CVar "c2")
    x

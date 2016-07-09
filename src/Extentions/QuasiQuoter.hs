{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Extentions.QuasiQuoter where

-- Reference: https://wiki.haskell.org/Quasiquotation

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as THQ
import qualified Language.Haskell.TH.Syntax as THS
import Text.Parsec
import Text.Parsec.Language (haskell)
import Text.Parsec.Token (integer, parens, symbol)


---------------------------------------
-- syntax definition and interpreter --

data HExp = HIntE Integer
          | HBinOpE HExp HBinOp HExp
          deriving (Show, Eq)

data HBinOp = HAddO | HSubO | HMulO | HDivO
           deriving (Show, Eq)

eval :: HExp -> Integer
eval (HIntE n) = n
eval (HBinOpE x b y) =
  (case b of
    HAddO -> (+)
    HSubO -> (-)
    HMulO -> (*)
    HDivO -> div
  ) (eval x) (eval y)


-------------------
-- parsec parser --

pHExp :: Parsec String () HExp
pHExp =
      HIntE <$> integer'
  <|> parens' (HBinOpE <$> pHExp <*> pHBinOp <*> pHExp)
  where
    parens' = parens haskell
    integer' = integer haskell

pHBinOp :: Parsec String () HBinOp
pHBinOp =
      pure HAddO <* symbol' "+"
  <|> pure HSubO <* symbol' "-"
  <|> pure HMulO <* symbol' "*"
  <|> pure HDivO <* symbol' "/"
  where
    symbol' = symbol haskell

doParse :: Monad m => (String, Int, Int) -> String -> m HExp
doParse (file, line, col) s =
  case runParser p () "" s of
    Left err -> fail $ show err
    Right e  -> return e
  where
    p = do
      setPosition .
        (`setSourceName` file) .
        (`setSourceLine` line) .
        (`setSourceColumn` col) =<< getPosition
      spaces *> pHExp <* eof


----------------------------------------------
-- define quasiquoter by using above parser --

hexpr :: THQ.QuasiQuoter
hexpr = THQ.QuasiQuoter { THQ.quoteExp = quoteHExp
                         , THQ.quotePat  = undefined
                         , THQ.quoteType = undefined
                         , THQ.quoteDec  = undefined
                         }

quoteHExp :: String -> TH.ExpQ
quoteHExp s = do
  loc <- TH.location
  let (line, col) = TH.loc_start loc
      file = TH.loc_filename loc
  THS.lift =<< doParse (file, line, col) s

-- NOTE:
--   `liftData :: Data a => a -> Q Exp` is available from template-haskell-2.11.0.0,
--   but, currently 2.10.0.0 is used

instance THS.Lift HExp where
  lift (HIntE i) = [| HIntE $(THS.lift i) |]
  lift (HBinOpE e0 b e1) = [| HBinOpE $(THS.lift e0) $(THS.lift b) $(THS.lift e1) |]

instance THS.Lift HBinOp where
  lift HAddO = [| HAddO |]
  lift HSubO = [| HSubO |]
  lift HMulO = [| HMulO |]
  lift HDivO = [| HDivO |]

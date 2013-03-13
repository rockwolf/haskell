{- |
Module : GInvade.hs
Description : Investment and trading calculations.
Copyright : (c) <Andy Nagels>
License : See LICENSE file for license details.

Maintainer : <no@e-mail.given>
Stability : unstable
Portability : portable

Gui interface to invade.
-}

--module GInvade where

import Invade
import System.IO (stdin, stderr, stdout, hSetEcho, hPutStrLn, hSetBuffering, BufferMode(..))
import System.Exit
import Control.Monad
import Graphics.UI.WX hiding (Event)
import Reactive.Banana
import Reactive.Banana.WX

{- GUI interfacing -}

main :: IO ()
main = do
    --opts <- foldl (>>=) (return defaultInput) actions
   
    let varInput = Input {
            i_verbose = verbose
            ,i_account = account
            ,i_pool = pool 
            ,i_money_to_use = money_to_use
            ,i_long_short = long_short
            ,i_price = price
            ,i_shares = shares
            ,i_commission = commission
            ,i_tax = tax
            ,i_risk = risk
            ,i_market = market
            ,i_stockname = stockname
            ,i_spread = spread
            ,i_currency_from = currency_from
            ,i_currency_to = currency_to
            ,i_exchange_rate = exchange_rate
    }
    
    putStrLn $ show (varInput)  
    putStrLn $ show (setOutput varInput)

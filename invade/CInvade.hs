{- |
Module : CInvade.hs
Description : Investment and trading calculations.
Copyright : (c) <Andy Nagels>
License : See LICENSE file for license details.

Maintainer : <no@e-mail.given>
Stability : unstable
Portability : portable

Command line interface to invade.
-}

--module CInvade where

import Invade
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.IO (stdin, stderr, stdout, hSetEcho, hPutStrLn, hSetBuffering, BufferMode(..))
import System.Exit
import Control.Monad

{- CLI interfacing -}

options :: [ OptDescr (Input -> IO Input) ]
options =
    [ Option "a" ["account"]
        (ReqArg (\arg opt -> return opt { i_account = arg }) "<account name>")
        "account"

    , Option "o" ["pool"]
        (ReqArg (\arg opt -> return opt { i_pool = read arg }) "<pool at start>")
        "pool at start"

    , Option "u" ["money_to_use"]
        (ReqArg (\arg opt -> return opt { i_money_to_use = read arg }) "<money to use>")
        "money to use"

    , Option "p" ["price"]
        (ReqArg (\arg opt -> return opt { i_price = read arg }) "<price>")
        "price"
  
    , Option "l" ["long_short"]
        (ReqArg (\arg opt -> return opt { i_long_short = head arg }) "<L/S>")
        "'L' or 'S' for long or short"

    , Option "s" ["shares"]
        (ReqArg (\arg opt -> return opt { i_shares = read arg }) "<number of shares>")
        "shares"

    , Option "c" ["commission"]
        (ReqArg (\arg opt -> return opt { i_commission = read arg }) "<commission>")
        "commission"
    
    , Option "t" ["tax"]
        (ReqArg (\arg opt -> return opt { i_tax = read arg }) "<tax>")
        "tax"

    , Option "r" ["risk"]
        (ReqArg (\arg opt -> return opt { i_risk = read arg }) "<risk>")
        "risk you are willing to take"
 
    , Option "m" ["market"]
        (ReqArg (\arg opt -> return opt { i_market = arg }) "<market name>")
        "market name"
 
    , Option "n" ["stockname"]
        (ReqArg (\arg opt -> return opt { i_stockname = arg }) "<stock name>")
        "stock name"
 
    , Option "d" ["spread"]
        (ReqArg (\arg opt -> return opt { i_spread = read arg }) "<spread>")
        "spread"

    , Option "x" ["currency_from"]
        (ReqArg (\arg opt -> return opt { i_currency_from = arg }) "<currency from>")
        "currency from"
 
    , Option "y" ["currency_to"]
        (ReqArg (\arg opt -> return opt { i_currency_to = arg }) "<currency to>")
        "currency to"

    , Option "e" ["exchange_rate"]
        (ReqArg (\arg opt -> return opt { i_exchange_rate = read arg }) "<exchange rate>")
        "exchange_rate"
 
    , Option "v" ["verbose"]
        (NoArg (\opt -> return opt { i_verbose = True }))
        "Enable verbose messages"
 
    , Option "V" ["version"]
        (NoArg (\_ -> do
                hPutStrLn stderr "Version 0.01"
                exitWith ExitSuccess))
        "Print version"
 
    , Option "h" ["help"]
        (NoArg (\_ -> do
                prg <- getProgName
                hPutStrLn stderr (usageInfo prg options)
                exitWith ExitSuccess))
        "Show help"
    ]

main :: IO ()
main = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
   
    args <- getArgs
 
    -- Parse options, getting a list of option actions
    let (actions, nonOptions, errors) = getOpt RequireOrder options args
 
    -- Here we thread defaultInput through all supplied option actions
    opts <- foldl (>>=) (return defaultInput) actions
   
    let Input { i_verbose = verbose
                , i_account = account
                , i_pool = pool
                , i_money_to_use = money_to_use
                , i_price = price
                , i_long_short = long_short
                , i_shares = shares
                , i_commission = commission
                , i_tax = tax
                , i_risk = risk
                , i_market = market
                , i_stockname = stockname
                , i_spread = spread
                , i_currency_from = currency_from
                , i_currency_to = currency_to
                , i_exchange_rate = exchange_rate } = opts
 
    {-when i_verbose (hPutStrLn stderr "Yoyoma's cousin, little Nepetiz..." -}
 
    -- NOTE: excellent info on option parsing:
    -- http://stackoverflow.com/questions/6321728/haskell-command-line-options
    -- http://leiffrenzel.de/papers/commandline-options-in-haskell.html
    -- http://en.literateprograms.org/Word_count_(Haskell)
    
    -- TODO: change some options through functions, so we can get calculate some stuff in advance
    -- It might be possible to leave out those options, so startOptions are used for them.
    -- Then you specify startOptions through functions.
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

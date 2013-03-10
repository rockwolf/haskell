{- |
Module      :  invade.hs
Description :  Investment and trading calculations.
Copyright   :  (c) <Andy Nagels>
License     :  See LICENSE file for license details.

Maintainer  :  <no@e-mail.given>
Stability   :  unstable
Portability :  portable

Makes the necessary calculations with regards to trading, investing and money management/risk management.
-}

module Main (main) where

import System.Console.GetOpt
import Control.Monad
import Data.List
import System.IO
import System.Exit
import Data.Char
import Data.Time.Clock
import Data.Time.Calendar
import System.Environment (getArgs, getProgName)

data Input = Input {i_verbose           :: Bool
                    ,i_account          :: String
                    ,i_pool             :: Double -- Retrieve from db later
                    ,i_money_to_use     :: Double
                    ,i_long_short       :: Char
                    ,i_price            :: Double
                    ,i_shares           :: Int
                    ,i_commission       :: Double
                    ,i_tax              :: Double
                    ,i_risk             :: Double
                    ,i_market           :: String --Needed for calculating with the correct tax/commission
                    ,i_stockname        :: String
                    ,i_spread           :: Double
                    ,i_currency_from    :: String
                    ,i_currency_to      :: String
                    ,i_exchange_rate    :: Double
             } deriving (Show)

data Output = Output {
                -- general
                o_account                   :: String
                -- buying
                ,o_price_buy                :: Double -- just the input price
                ,o_shares_buy               :: Int -- This is what you need to know
                ,o_amount_buy_simple        :: Double
                ,o_commission_buy           :: Double
                ,o_tax_buy                  :: Double
                ,o_cost_buy                 :: Double
                {- risk theoretical, just using the input
                    2% -> 2% from i_pool -}
                ,o_risk_input               :: Double
                ,o_risk_input_percentage    :: Double
                -- selling at stoploss
                ,o_stoploss                 :: Double
                ,o_shares_sell              :: Int
                ,o_amount_sell_simple       :: Double -- Can also be input when shorting
                ,o_commission_sell          :: Double
                ,o_tax_sell                 :: Double
                ,o_cost_sell                :: Double
                {- risk taken, when minimum stoploss is reached -}
                ,o_risk_initial             :: Double
                ,o_risk_initial_percentage  :: Double
                -- extra info at buying
                --,o_date_buy :: IO (Integer, Int, Int)-- extra, not really necessary
                ,o_pool_at_start            :: Double
                ,o_pool_new                 :: Double
                ,o_long_short               :: Char
                ,o_currency_from            :: String
                ,o_currency_to              :: String
                ,o_exchange_rate            :: Double
                -- extra info for close at stoploss
                ,o_profit_loss              :: Double
                ,o_profit_loss_percent      :: Double
                ,o_cost_total               :: Double
                ,o_cost_other               :: Double
            }
            deriving (Show) --, Eq, Ord)

setOutput :: Input -> Output
setOutput varInput = 
    Output {
        -- general
        o_account                  = i_account varInput
        -- buying
        ,o_price_buy               = varPriceBuy
        ,o_shares_buy              = varSharesBuy
        ,o_amount_buy_simple       = varAmountBuySimple
        ,o_commission_buy          = varCommissionBuy--calc later
        ,o_tax_buy                 = varTaxBuy -- calc later
        ,o_cost_buy                = costTransaction "buy" varPriceBuy varSharesBuy varTaxBuy varCommissionBuy
        ,o_risk_input              = defaultDecimal--calcRiskInput (i_risk varInput)
        ,o_risk_input_percentage   = defaultDecimal--calcPercentageOf oRiskInput (i_pool varInput)
        -- selling at stoploss
        ,o_stoploss                = varStoploss
        ,o_shares_sell             = varSharesSell
        ,o_amount_sell_simple      = varAmountSellSimple
        ,o_commission_sell         = varCommissionSell --calc later
        ,o_tax_sell                = varTaxSell --calc later
        ,o_cost_sell               = costTransaction "sell" varStoploss varSharesSell varTaxSell varCommissionSell
        ,o_risk_initial            = defaultDecimal--calcRiskInitial
        ,o_risk_initial_percentage = defaultDecimal--calcRiskInitialPercentage
        -- extra info at buying
        --,oDateBuy                = currentDate
        ,o_pool_at_start           = varPoolAtStart
        ,o_pool_new                = varPoolNew
        ,o_long_short              = i_long_short varInput
        ,o_currency_from           = i_currency_from varInput
        ,o_currency_to             = i_currency_to varInput
        ,o_exchange_rate           = i_exchange_rate varInput
        -- extra info for close at stoploss
        ,o_profit_loss             = defaultDecimal--calcProfitLoss
        ,o_profit_loss_percent     = defaultDecimal--calcProfitLossPercentage
        ,o_cost_total              = varCostTotal
        ,o_cost_other              = defaultDecimal--calcCostOther
    }
    where
        defaultDecimal = 0.0
        varPriceBuy = i_price varInput
        varSharesBuy = i_shares varInput
        varAmountBuySimple = calcAmountSimple varPriceBuy varSharesBuy
        varCommissionBuy = i_commission varInput
        varTaxBuy = i_tax varInput
        varStoploss = defaultDecimal -- o_stoploss from Output
        varSharesSell = varSharesBuy
        varAmountSellSimple = calcAmountSimple varStoploss varSharesSell
        varCommissionSell = varCommissionBuy
        varTaxSell = varTaxBuy
        varCostTotal = defaultDecimal --calcCostTotal
        varPoolAtStart = i_pool varInput
        varPoolNew = varPoolAtStart - varAmountSellSimple - varCostTotal

{-- Helper functions --}
calcPercentage :: Double -> Double
calcPercentage value = value / 100.0

calcPercentageOf :: Double -> Double -> Double
calcPercentageOf value from_value = (value / 100.0) * from_value 

{-- CalculatorFinance --}
-- NOTE: amount_buy = with tax and everything included, amount_buy_simple = without tax and commission!
-- NOTE: ((risk/100 * pool_at_start - amount_buy_simple) - commission_buy)/(shares_buy * (tax_buy/100 - 1))
-- NOTE: ((R * P - A) - C) / (S * (T - 1))
calcStoploss :: Double -> Int -> Double -> Double -> Double -> Double -> Double 
calcStoploss amount_buy_simple shares_buy tax_buy commission_buy i_risk pool_at_start =
    (((var_R * var_P) - var_A) - var_C) / (var_S * (var_T - 1))
    where
        var_R = calcPercentage i_risk
        var_P = pool_at_start
        var_A = amount_buy_simple
        var_S = fromIntegral shares_buy
        var_T = tax_buy / 100.0
        var_C = commission_buy

-- TODO: only allow positive numbers
calcRiskInput :: Double -> Double -> Double
calcRiskInput i_risk i_pool =
    var_R * var_Po
    where
        var_R = calcPercentage i_risk
        var_Po = i_pool

calcRiskInitial :: Double -> Int -> Double -> Double
calcRiskInitial price_buy shares_buy stoploss =
    (price_buy * shares_buy_) - (stoploss * shares_buy_)
    where
        shares_buy_ = fromIntegral shares_buy

-- NOTE: price_sell > stoploss = max risk was the initial risk
calcRiskActual :: Double -> Int -> Double -> Int -> Double -> Double -> Double
calcRiskActual price_buy shares_buy price_sell shares_sell stoploss risk_initial =
    if price_sell < stoploss
    then (price_buy * shares_buy_) - (price_sell * shares_sell_)
    else risk_initial
    where
        shares_buy_ = fromIntegral shares_buy
        shares_sell_ = fromIntegral shares_sell

calcRMultiple :: Double -> Double -> Double -> Double
calcRMultiple price_buy price_sell stoploss =
    (price_sell - price_buy) / (price_buy - stoploss)

calcCostTotal :: Double -> Double -> Double -> Double -> Double
calcCostTotal tax_buy commission_buy tax_sell commission_sell =
    tax_buy + commission_buy + tax_sell + commission_sell

-- NOTE: commission + tax = seperate = costs
calcAmountSimple :: Double -> Int -> Double
calcAmountSimple price shares = price * fromIntegral shares

-- cost of transaction (tax and commission)
costTransaction :: String -> Double -> Int -> Double -> Double -> Double
costTransaction transaction price shares tax commission =
    case lowerCase transaction of
        []      -> error errorMsgEmpty
        "buy"   -> (price * (fromIntegral shares) * (1 + tax)) + commission
        "sell"  -> (price * (fromIntegral shares) * (1 - tax)) - commission
    where
        errorMsgEmpty = "Error in costTransaction: buy or sell not specified!"

upperCase, lowerCase :: String -> String
upperCase = map toUpper
lowerCase = map toLower

--currentDate :: IO (Integer,Int,Int) -- :: (year,month,day)
--currentDate = getCurrentTime >>= return . toGregorian . utctDay

calcCommission :: String -> String -> String -> Double -> Int -> Double
calcCommission  account market stockname price shares =
    case lowerCase account of
        "binb00" -> getBinb00Commission market stockname amount_simple
        "whsi00" -> 4.50 + 0.023 * fromIntegral shares
        _ -> 0.0
    where
        amount_simple = calcAmountSimple price shares
        -- WHSI00 SHARE CFDs and ETFs - GB, FR, DE, BE, DK, F, IT, NL, N, P, S, CH, ES
        -- TODO: use function to convert currency? Might nod be needed, usd is entered as usd!
        --4.50 + calcPercentageOf 0.054 amount_simple
        -- WHSI00 SHARE CFDs and ETFs - AU, AUS
        --4.50 + calcPercentageOf 0.09 amount_simple
        -- WHSI00 SHARE CFDs and ETFs - China, PL, Singapore
        --4.50 + calcPercentageOf 0.19 amount_simple
        -- WHSI00 SHARE CFDs and ETFs - US markets (incl. ADR and ETF)
        -- NOTE: this is in USD and not in EUR!
        -- WHSI00 SHARE CFDs and ETFs - non-share CFDs (oil, gold, indices...)
        --3.00

getBinb00Commission :: String -> String -> Double -> Double
getBinb00Commission _ _ 0.0 = 0.0
getBinb00Commission "" "" _ = 0.0
getBinb00Commission _ _ amount_simple 
    | amount_simple <= 2500.0 = 7.25 -- TODO: take market into consideration
    | amount_simple > 2500.0 && amount_simple <= 5000.0 = 9.75
    | amount_simple > 5000.0 && amount_simple <= 25000.0 = 13.75
    | amount_simple > 25000.0 && amount_simple <= 50000.0 = 19.75
    | amount_simple > 50000.0 = perDiscNumber * 19.75
    where
        perDiscNumber = fromIntegral (ceiling $ amount_simple / 50000.0)

getWhsi00Commission :: String -> String -> Double -> Double
getWhsi00Commission _ _ 0.0 = 0.0
getWhsi00Commission "" "" _ = 0.0
getWhsi00Commission market _ amount_simple = 0.0
    -- TODO: make a complete list of market abbreviations, to use in both lisa and here!
    | market = "ebr" amount_simple

getPool :: Double
getPool = 100000.0

{- CLI interfacing -}

startOptions :: Input
startOptions = Input  { i_verbose         = False
                        , i_account       = "whsi00"
                        , i_pool          = getPool
                        , i_money_to_use  = getPool / 10.0 -- use 1 tenth of our total pool by default
                        , i_long_short    = 'L'
                        , i_price         = 0.0
                        , i_shares        = 0
                        , i_commission    = 7.50
                        , i_tax           = 0.25
                        , i_risk          = 2.0
                        , i_market        = "world."
                        , i_stockname     = "gold"
                        , i_spread        = 0.0
                        , i_currency_from = "EUR"
                        , i_currency_to   = "EUR"
                        , i_exchange_rate = 1.0
                        }

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
    -- TODO: finish and test option parsing
    args <- getArgs
 
    -- Parse options, getting a list of option actions
    let (actions, nonOptions, errors) = getOpt RequireOrder options args
 
    -- Here we thread startOptions through all supplied option actions
    opts <- foldl (>>=) (return startOptions) actions
 
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
    putStrLn $ show (getBinb00Commission "" "" 1000)
    putStrLn $ show (getBinb00Commission "test2" "test2" 2600)
    putStrLn $ show (getBinb00Commission "test3" "test3" 60000)

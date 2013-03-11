{- |
Module      :  Invade.hs
Description :  Investment and trading calculations.
Copyright   :  (c) <Andy Nagels>
License     :  See LICENSE file for license details.

Maintainer  :  <no@e-mail.given>
Stability   :  unstable
Portability :  portable

Makes the necessary calculations with regards to trading, investing and money management/risk management.
-}

module Invade (Input
               ,Output
               ) where

import Control.Monad
import Data.List
import System.IO
import System.Exit
import Data.Char
import Data.Time.Clock
import Data.Time.Calendar

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

defaultInput :: Input
defaultInput = Input { i_verbose = False
                       , i_account = "whsi00"
                       , i_pool = getPool
                       , i_money_to_use = getPool / 10.0 -- use 1 tenth of our total pool by default
                       , i_long_short = 'L'
                       , i_price = 0.0
                       , i_shares = 0
                       , i_commission = 7.50
                       , i_tax = 0.25
                       , i_risk = 2.0
                       , i_market = "cfd .gold"
                       , i_stockname = ".goldo2" -- TODO: add sane name from whsi
                       , i_spread = 0.0
                       , i_currency_from = "EUR"
                       , i_currency_to = "EUR"
                       , i_exchange_rate = 1.0
                       }

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
        "whsi00" -> getWhsi00Commission market stockname price shares
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
getBinb00Commission market stockname amount_simple 
    | amount_simple <= 2500.0 = getBinb00Commission2500 market stockname
    | amount_simple > 2500.0 && amount_simple <= 5000.0 = getBinb00Commission5000 market stockname
    | amount_simple > 5000.0 && amount_simple <= 25000.0 = getBinb00Commission25000 market stockname
    | amount_simple > 25000.0 && amount_simple <= 50000.0 = getBinb00Commission50000 market stockname
    | amount_simple > 50000.0 = perDiscNumber * getBinb00Commission50000Plus market stockname
    | otherwise = 0.0
    where
        perDiscNumber = fromIntegral (ceiling $ amount_simple / 50000.0)

isEuronextBrussels :: String -> Bool
isEuronextBrussels market
    | market == "ebr"   = True
    | otherwise         = False

isEuronextOther :: String -> Bool
isEuronextOther market
    | market == "ams"   = True
    | market == "epa"   = True
    | market == "eli"   = True
    | otherwise         = False

isUS :: String -> Bool
isUS market
    -- TODO: add NYSE, nasdaq, amex, OTC BB en pink sheets to abbreviations
    | market == "dummy" = True
    | otherwise         = False

isEuroExchange :: String -> Bool
isEuroExchange market
    | market == "dummy" = True
    | otherwise         = False

isCanadaExchange :: String -> Bool
isCanadaExchange market
    | market == "dummy" = True
    | otherwise         = False

isSwissScandinavianExchange :: String -> Bool
isSwissScandinavianExchange market
    | market == "dummy" = True
    | otherwise         = False

getBinb00Commission2500 :: String -> String -> Double
getBinb00Commission2500 market _
    | isEuronextBrussels market = 7.25        
    | isEuronextOther market    = 9.75
    | isUS market               = 9.75

getBinb00Commission5000 :: String -> String -> Double
getBinb00Commission5000 market _
    | isEuronextBrussels market = 9.75        
    | isEuronextOther market    = 9.25
    | otherwise                 = 0.0

getBinb00Commission25000 :: String -> String -> Double
getBinb00Commission25000 market _
    | isEuronextBrussels market = 13.75        
    | isEuronextOther market    = 9.25
    | otherwise                 = 0.0

getBinb00Commission50000 :: String -> String -> Double
getBinb00Commission50000 market _
    | isEuronextBrussels market = 19.75        
    | isEuronextOther market    = 9.25
    | otherwise                 = 0.0

getBinb00Commission50000Plus :: String -> String -> Double
getBinb00Commission50000Plus market _
    | isEuronextBrussels market = 19.75        
    | isEuronextOther market = 9.25
    | otherwise = 0.0

getWhsi00Commission :: String -> String -> Double -> Int -> Double
getWhsi00Commission market stockname price shares
    | isNonShareCfd market = 3.0
    | isShareCfd market = 4.50 + (calcPercentageOf 0.054 amount_simple)
    | isShareCfdDev1 market = 4.50 + (calcPercentageOf 0.09 amount_simple)
    | isShareCfdDev2 market = 4.50 + (calcPercentageOf 0.19 amount_simple)
    | isShareCfdUS market = 4.50 + 0.023 * fromIntegral shares
    | otherwise = 0.0
    where
        amount_simple = calcAmountSimple price shares

isNonShareCfd :: String -> Bool
isNonShareCfd market
    | market == "cfd .gold"     = True
    | market == "cfd .silver"   = True
    | market == "cfd oil"       = True
    -- TODO: check if there are others
    | market == "cfd index"     = True
    | otherwise                 = False

isShareCfd :: String -> Bool
isShareCfd market
    | market == "cfd Belgium" = True
    | market == "cfd France"   = True
    -- TODO: complete this
    | otherwise                 = False

isShareCfdDev1 :: String -> Bool
isShareCfdDev1 market
    | market == "cfd Australia" = True
    | market == "cfd Austria"   = True
    | otherwise                 = False

isShareCfdDev2 :: String -> Bool
isShareCfdDev2 market
    | market == "cfd China"     = True
    | market == "cfd Poland"    = True
    | market == "cfd Singapore" = True
    | otherwise                 = False

isShareCfdUS :: String -> Bool
isShareCfdUS market
    -- TODO: figure out whats in here
    -- TODO: sync this file with lisa
    | market == "cfd US"        = True
    | otherwise                 = False

getPool :: Double
getPool = 100000.0
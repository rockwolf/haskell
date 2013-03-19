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

module Invade (Input(..)
               ,Output(..)
               ,setOutput
               ,defaultInput
               ,markets
               ) where

import Data.List
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
                ,o_shares_recommended       :: Int
            }
            deriving (Show) --, Eq, Ord)

markets = merge markets_euronext_brussels $
          merge markets_euronext_other $
          merge markets_us $
          merge markets_cfd_share $
          merge markets_cfd_dev1 $
          merge markets_cfd_dev2 $
          merge markets_cfd_non_share markets_cfd_us

-- binb00
-- NOTE: see list of country codes at:
-- http://www.iso.org/iso/country_codes/iso_3166_code_lists/country_names_and_code_elements.htm
markets_euronext_brussels = [
        "ebr"
    ]

markets_euronext_other = [
           "ams"
           ,"etr"
           ,"epa"
           ,"other"
           ,"eli"
           ,"lse"
           ,"ise"
           ,"mil"
           ,"bma"
           ,"vse"
           ,"other"
        ]
-- TODO: sync this with lisa.
markets_us = [
        "nyse"
        ,"nasdaq"
        ,"otc bb & pinksheets"
        ,"amex"
        ,"other us"
    ]

-- TODO: incorporate this into the calculations
-- but only when we plan on doing options trading.
markets_options_euronext = [
        "options ams"
        ,"options ebr"
    ]
-- /binb00

-- whsi00
markets_cfd_share = [
           "cfd BE"
           ,"cfd FR"
           ,"cfd DE"
           ,"cfd UK"
           ,"cfd DK"
           ,"cfd FI"
           ,"cfd IT"
           ,"cfd NL"
           ,"cfd NO"
           ,"cfd PT"
           ,"cfd SE"
           ,"cfd CH"
           ,"cfd ES"
           ,"cfd other share"
        ]

-- NOTE: read "first deviation: Australia/Austria"
markets_cfd_dev1 = [
           "cfd AU"
           ,"cfd AT"
           ]

markets_cfd_dev2 = [
           "cfd PL"
           ,"cfd CN"
           ,"cfd SG"
           ]

markets_cfd_non_share = [
           "cfd .gold"
           ,"cfd .silver"
           ,"cfd oil"
           ,"cfd index"
           ,"cfd other non-share"
           ]

markets_cfd_us = [
           "cfd US"
           ]
-- /whsi00

merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

defaultInput :: Input
defaultInput = Input { i_verbose = False
                       , i_account = "whsi00"
                       , i_pool = getPool
                       , i_money_to_use = (getPool * (1.0 - calcPercentage getMargin)) / 10.0  -- use 1 tenth of our total pool by default
                       , i_long_short = 'L'
                       , i_price = 25.0
                       , i_shares = 99
                       , i_commission = 7.50
                       , i_tax = 0.25
                       , i_risk = 2.0
                       , i_market = "cfd .gold"
                       , i_stockname = ""
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
        ,o_risk_input              = varRiskInput
        ,o_risk_input_percentage   = calcPercentageOf varRiskInput (i_pool varInput)
        -- selling at stoploss
        ,o_stoploss                = varStoploss
        ,o_shares_sell             = varSharesSell
        ,o_amount_sell_simple      = varAmountSellSimple
        ,o_commission_sell         = varCommissionSell
        ,o_tax_sell                = varTaxSell
        ,o_cost_sell               = costTransaction "sell" varStoploss varSharesSell varTaxSell varCommissionSell
        ,o_risk_initial            = varRiskInitial
        ,o_risk_initial_percentage = calcPercentageOf varRiskInitial varPoolAtStart
        ,o_pool_at_start           = varPoolAtStart
        ,o_pool_new                = varPoolNew
        ,o_long_short              = i_long_short varInput
        ,o_currency_from           = i_currency_from varInput
        ,o_currency_to             = i_currency_to varInput
        ,o_exchange_rate           = i_exchange_rate varInput
        -- extra info for close at stoploss
        ,o_profit_loss             = varProfitLoss
        ,o_profit_loss_percent     = varProfitLossPercentage
        ,o_cost_total              = varCostTotal
        ,o_cost_other              = varCostOther
        ,o_shares_recommended      = varSharesRecommended
    }
    where
        defaultDecimal = 0.0
        varPriceBuy = i_price varInput
        varSharesBuy = i_shares varInput
        varAmountBuySimple = calcAmountSimple varPriceBuy varSharesBuy
        varCommissionBuy = i_commission varInput
        varTaxBuy = i_tax varInput
        varStoploss = calcStoploss varAmountBuySimple varSharesBuy varTaxBuy varCommissionBuy (i_risk varInput) (i_pool varInput)
        varSharesSell = varSharesBuy
        varAmountSellSimple = calcAmountSimple varStoploss varSharesSell
        varCommissionSell = varCommissionBuy
        varTaxSell = varTaxBuy
        varCostTotal = calcCostTotal varTaxBuy varCommissionBuy varTaxSell varCommissionSell
        varPoolAtStart = i_pool varInput
        varPoolNew = varPoolAtStart - varAmountSellSimple - varCostTotal
        varProfitLoss = calcProfitLoss varAmountSellSimple varAmountBuySimple varCostTotal
        varProfitLossPercentage = calcPercentage varProfitLoss
        varRiskInitial = calcRiskInitial varPriceBuy varSharesBuy varStoploss
        varCostOther = varCostTotal - varProfitLoss
        varSharesRecommended = calcSharesRecommended -- TODO: finish + write func
        varRiskInput = calcRiskInput (i_risk varInput) (i_pool varInput)

{-- Helper functions --}
calcSharesRecommended :: Int
calcSharesRecommended = 0

calcCostOther :: Double -> Double -> Double
calcCostOther totalCost profitLoss =
    if diffCostProfit > defaultDecimal
    then diffCostProfit
    else defaultDecimal
    where
        diffCostProfit = totalCost - profitLoss
        defaultDecimal = 0.0

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

calcProfitLoss :: Double -> Double -> Double -> Double
calcProfitLoss amount_sell_simple amount_buy_simple totalcost =
    amount_sell_simple - amount_buy_simple - totalcost

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

-- TODO: get 2500 etc values from T_PARAMETER
getBinb00Commission :: String -> String -> Double -> Double
getBinb00Commission _ _ 0.0 = 0.0
getBinb00Commission "" _ _ = 0.0
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
    | market `elem` markets_euronext_brussels   = True
    | otherwise                                 = False

isEuronextOther :: String -> Bool
isEuronextOther market
    | market `elem` markets_euronext_other  = True
    | otherwise                             = False

isUS :: String -> Bool
isUS market
    | market `elem` markets_us  = True
    | otherwise                 = False

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

-- TODO: get these values from T_PARAMETER
getBinb00Commission2500 :: String -> String -> Double
getBinb00Commission2500 market _
    | isEuronextBrussels market          = 7.25        
    | isEuronextOther market             = 9.75
    | isEuroExhange market               = 12.75
    | isUS market                        = 12.75
    | isCanadaExchange market            = 19.75
    | isSwissScandinavianExchange market = 29.75
    | otherwise                          = 0.0

-- TODO: find the correct values for canada/scand/euroexch
getBinb00Commission5000 :: String -> String -> Double
getBinb00Commission5000 market _
    | isEuronextBrussels market          = 9.75        
    | isEuronextOther market             = 9.25
    | isEuroExhange market               = 12.75
    | isUs                               = 12.75
    | isCanadaExchange market            = 19.75
    | isSwissScandinavianExchange market = 29.75
    | otherwise                          = 0.0

getBinb00Commission25000 :: String -> String -> Double
getBinb00Commission25000 market _
    | isEuronextBrussels market          = 13.75        
    | isEuronextOther market             = 13.75
    | isEuroExhange market               = 16.75
    | isUs                               = 16.75
    | isCanadaExchange market            = 24.75
    | isSwissScandinavianExchange market = 29.75
    | otherwise                          = 0.0

getBinb00Commission50000 :: String -> String -> Double
getBinb00Commission50000 market _
    | isEuronextBrussels market          = 19.75        
    | isEuronextOther market             = 19.75
    | isEuroExhange market               = 22.75
    | isUs                               = 22.75
    | isCanadaExchange market            = 29.75
    | isSwissScandinavianExchange market = 59.75
    | otherwise                          = 0.0

getBinb00Commission50000Plus :: String -> String -> Double
getBinb00Commission50000Plus market _
    | isEuronextBrussels market          = 19.75        
    | isEuronextOther market             = 19.75
    | isEuroExhange market               = 19.75
    | isUs                               = 19.72
    | isCanadaExchange market            = 29.75
    | isSwissScandinavianExchange market = 29.75
    | otherwise                          = 0.0

getBinb00CommissionOptions :: String -> String -> Int -> Double
getBinb00Commission market _ contracts
    | isOptionsEuronext market         = varOptEur contracts
    | otherwise                        = 0.0
    where
        -- TODO: 2.50 is valid for the number of contracts above 10,
        -- so do a mod div, when not dividable by 10,
        -- the rest at division has a price of 3 eur per contract
        -- So mod10val > 0 => (contracts - mod10val) * 2.50 + (mod10val * 3.0)
        varOptEur c = if c <= 10
                      then 3.0
                      else 2.50

isOptionsEuronext :: String -> Bool
isOptionsEuronext market
    | market `elem` markets_options_euronext = True
    | otherwise                              = False

getWhsi00Commission :: String -> String -> Double -> Int -> Double
getWhsi00Commission market stockname price shares
    | isNonShareCfd market         = 3.0
    | isShareCfd market            = 4.50 + (calcPercentageOf 0.054 amount_simple)
    | isShareCfdDev1 market        = 4.50 + (calcPercentageOf 0.09 amount_simple)
    | isShareCfdDev2 market        = 4.50 + (calcPercentageOf 0.19 amount_simple)
    | isShareCfdUS market          = 4.50 + 0.023 * fromIntegral shares
    | otherwise                    = 0.0
    where
        amount_simple = calcAmountSimple price shares

isNonShareCfd :: String -> Bool
isNonShareCfd market
    | market `elem` markets_cfd_non_share = True
    | otherwise                           = False

isShareCfd :: String -> Bool
isShareCfd market
    | market `elem` markets_cfd_share     = True
    | otherwise                           = False

isShareCfdDev1 :: String -> Bool
isShareCfdDev1 market
    | market `elem` markets_cfd_dev1      = True
    | otherwise                           = False

isShareCfdDev2 :: String -> Bool
isShareCfdDev2 market
    | market `elem` markets_cfd_dev2      = True
    | otherwise                           = False

isShareCfdUS :: String -> Bool
isShareCfdUS market
    | market `elem` markets_cfd_us        = True
    | otherwise                           = False

getPool :: Double
getPool = 100000.0

getMargin :: Double
getMargin = 25.0

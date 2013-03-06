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

import Data.Char
import Data.Time.Clock
import Data.Time.Calendar

data Input = Input {
                    i_pool :: Double -- Retrieve from db later
                    ,i_money_to_use :: Double
                    ,i_long_short :: Char
                    ,i_price :: Double
                    ,i_shares :: Int
                    ,i_commission :: Double
                    ,i_tax :: Double
                    ,i_risk :: Double
                    ,i_market :: String --Needed for calculating with the correct tax/commission
                    ,i_stockname :: String
                    ,i_spread :: Double
                    ,i_currency_from :: String
                    ,i_currency_to :: String
                    ,i_exchange_rate :: Double
             } deriving (Show)

data Output = Output {
                -- buying
                o_price_buy :: Double -- just the input price
                ,o_shares_buy :: Int -- This is what you need to know
                ,o_amount_buy_simple :: Double
                ,o_commission_buy :: Double
                ,o_tax_buy :: Double
                ,o_cost_buy :: Double
                {- risk theoretical, just using the input
                    2% -> 2% from iPool -}
                ,o_risk_input :: Double
                ,o_risk_input_percentage :: Double
                -- selling at stoploss
                ,o_stoploss :: Double
                ,o_shares_sell :: Int
                ,o_amount_sell_simple :: Double -- Can also be input when shorting
                ,o_commission_sell :: Double
                ,o_tax_sell :: Double
                ,o_cost_sell :: Double
                {- risk taken, when minimum stoploss is reached -}
                ,o_risk_initial :: Double
                ,o_risk_initial_percentage :: Double
                -- extra info at buying
                --,o_date_buy :: IO (Integer, Int, Int)-- extra, not really necessary
                ,o_pool_at_start :: Double
                ,o_pool_new :: Double
                ,o_long_short :: Char
                ,o_currency_from :: String
                ,o_currency_to :: String
                ,o_exchange_rate :: Double
                -- extra info for close at stoploss
                ,o_profit_loss :: Double
                ,o_profit_loss_percent :: Double
                ,o_cost_total :: Double
                ,o_cost_other :: Double
            }
            deriving (Show) --, Eq, Ord)

getPrice :: Input -> String -> Double
getPrice varInput transaction =
    case lowerCase transaction of
        []      -> error errorMsgEmpty
        "buy"   -> iPrice varInput
        "sell"  -> 0.0--calcStoploss
    where
        errorMsgEmpty = "Error in getPrice: buy or sell not specified!"

setOutput :: Input -> Output
setOutput varInput = 
    Output {
        -- buying
        o_price_buy                = varPriceBuy
        ,o_shares_buy              = varSharesBuy
        ,o_amount_buy_simple       = varAmountBuySimple
        ,o_commission_buy          = varCommissionBuy--calc later
        ,o_tax_buy                 = varTaxBuy -- calc later
        ,o_cost_buy                = costTransaction "buy" varPriceBuy varSharesBuy varTaxBuy varCommissionBuy
        ,o_risk_input              = defaultDecimal--calcRiskInput (iRisk varInput)
        ,o_risk_input_percentage   = defaultDecimal--calcPercentageOf oRiskInput (iPool varInput)
        -- selling at stoploss
        ,o_stoploss                = varStoploss
        ,o_sharesSell              = varSharesSell
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
        ,o_long_short              = iLongShort varInput
        ,o_currency                = iCurrency varInput
        ,o_exchange_rate           = iExchangeRate varInput
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
        varPoolNew = varPoolAtStart - varSold - varCostTotal

{-- Helper functions --}
calcPercentage :: Double -> Double
calcPercentage value = value / fromIntegral 100.0

calcPercentageOf :: Double -> Double -> Double
calcPercentageOf value from_value = value / fromIntegral 100.0 * from_value 

{-- CalculatorFinance --}
-- NOTE: amount_buy = with tax and everything included, amount_buy_simple = without tax and commission!
-- NOTE: ((risk/100 * pool_at_start - amount_buy_simple) - commission_buy)/(shares_buy * (tax_buy/100 - 1))
-- NOTE: ((R * P - A) - C) / (S * (T - 1))
calcStoploss :: Double -> Int -> Double -> Double -> Double -> Double -> Double 
calcStoploss amount_buy_simple shares_buy tax_buy commission_buy i_risk pool_at_start =
    ((R * P -A) - C) / (S * (T - 1))
    where
        R = calcPercentage i_risk
        P = amount_buy_simple
        A = amount_buy_simple
        S = shares_buy
        T = tax_buy / fromIntegral 100.0
        C = commission_buy

-- TODO: only allow positive numbers
calcRiskInput :: Double -> Double -> Double
calcRiskInput i_risk i_pool =
    R * Po
    where
        R = calcPercentage i_risk
        Po = i_pool

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
    tax_buy + commission_buy + tax_sell + cmmission_sell

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

calcCommission :: String -> String -> Double -> Int -> Double
calcCommission  market stockname price shares =
    -- TODO: getPredefined commission, based on type of input/commodity/market
    defaultDecimal

main = do
    -- TODO: option parsing
    let varInput = Input {
            i_pool = 104000.0 
            ,i_money_to_use = 5000.0
            ,i_long_short = 'L'
            ,i_price = 25.0
            ,i_shares = 100
            ,i_commission = 7.5
            ,i_tax = 0.0025
            ,i_risk = 0.02 
            ,i_market = "world"
            ,i_stockname = ".Gold"
            ,i_spread = 4.0
            ,i_currency_from = "EUR"
            ,i_currency_to = "EUR"
            ,i_exchange_rate = 1.0
    }
    
    putStrLn $ show (setOutput varInput)  
    -- TODO: is show necessary here? 
    -- bought
    {-putStrLn $ show (
        calcBoughtSold oPriceBuy oSharesBuy)
    -- sold
    putStrLn $ show (
        calcBoughtSold oStoploss oSharesSell)
    -- cost_buy
    -- TODO: determine tax and commission based on market/stockname
    putStrLn $ show (
        costTransaction "buy" oPriceBuy oSharesBuy oTaxBuy oCommissionBuy)
    -- cost_sell
    putStrLn $ show (
        costTransaction "sell" oStoploss oSharesBuy oTaxBuy oCommissionBuy)
    -}

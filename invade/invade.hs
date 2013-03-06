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
                    iPool :: Double -- Retrieve from db later
                    ,iMoneyToUse :: Double
                    ,iLongShort :: Char
                    ,iPrice :: Double
                    ,iShares :: Int
                    ,iCommission :: Double
                    ,iTax :: Double
                    ,iRisk :: Double
                    ,iMarket :: String --Needed for calculating with the correct tax/commission
                    ,iStockname :: String
                    ,iSpread :: Double
                    ,iCurrency :: String
                    ,iExchangeRate :: Double
             } deriving (Show)

data Output = Output {
                -- buying
                oPriceBuy :: Double -- just the input price
                ,oSharesBuy :: Int -- This is what you need to know
                ,oAmountBuySimple :: Double
                ,oCommissionBuy :: Double
                ,oTaxBuy :: Double
                ,oCostBuy :: Double
                {- risk theoretical, just using the input
                    2% -> 2% from iPool -}
                ,oRiskInput :: Double
                ,oRiskInputPercentage :: Double
                -- selling at stoploss
                ,oStoploss :: Double
                ,oSharesSell :: Int
                ,oAmountSellSimple :: Double -- Can also be input when shorting
                ,oCommissionSell :: Double
                ,oTaxSell :: Double
                ,oCostSell :: Double
                {- risk taken, when minimum stoploss is reached -}
                ,oRiskInitial :: Double
                ,oRiskInitialPercentage :: Double
                -- extra info at buying
                --,oDateBuy :: IO (Integer, Int, Int)-- extra, not really necessary
                ,oPoolAtStart :: Double
                ,oPoolNew :: Double
                ,oLongShort :: Char
                ,oCurrency :: String
                ,oExchangeRate :: Double
                -- extra info for close at stoploss
                ,oProfitLoss :: Double
                ,oProfitLossPercent :: Double
                ,oCostTotal :: Double
                ,oCostOther :: Double
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
        oPriceBuy               = varPriceBuy
        ,oSharesBuy             = varSharesBuy
        ,oAmountBuySimple       = varAmountBuySimple
        ,oCommissionBuy         = varCommissionBuy--calc later
        ,oTaxBuy                = varTaxBuy -- calc later
        ,oCostBuy               = costTransaction "buy" varPriceBuy varSharesBuy varTaxBuy varCommissionBuy
        ,oRiskInput             = defaultDecimal--calcRiskInput (iRisk varInput)
        ,oRiskInputPercentage   = defaultDecimal--calcPercentageOf oRiskInput (iPool varInput)
        -- selling at stoploss
        ,oStoploss              = varStoploss
        ,oSharesSell            = varSharesSell
        ,oAmountSellSimple      = varAmountSellSimple
        ,oCommissionSell        = varCommissionSell --calc later
        ,oTaxSell               = varTaxSell --calc later
        ,oCostSell              = costTransaction "sell" varStoploss varSharesSell varTaxSell varCommissionSell
        ,oRiskInitial           = defaultDecimal--calcRiskInitial
        ,oRiskInitialPercentage = defaultDecimal--calcRiskInitialPercentage
        -- extra info at buying
        --,oDateBuy               = currentDate
        ,oPoolAtStart           = varPoolAtStart
        ,oPoolNew               = varPoolNew
        ,oLongShort             = iLongShort varInput
        ,oCurrency              = iCurrency varInput
        ,oExchangeRate          = iExchangeRate varInput
        -- extra info for close at stoploss
        ,oProfitLoss            = defaultDecimal--calcProfitLoss
        ,oProfitLossPercent     = defaultDecimal--calcProfitLossPercentage
        ,oCostTotal             = varCostTotal
        ,oCostOther             = defaultDecimal--calcCostOther
    }
    where
        defaultDecimal = 0.0
        varPriceBuy = iPrice varInput
        varSharesBuy = iShares varInput
        varAmountBuySimple = calcAmountSimple varPriceBuy varSharesBuy
        varCommissionBuy = iCommission varInput
        varTaxBuy = iTax varInput
        varStoploss = defaultDecimal -- oStoploss from Output
        varSharesSell = varSharesBuy
        varAmountSellSimple = calcAmountSimple varStoploss varSharesSell
        varCommissionSell = varCommissionBuy
        varTaxSell = varTaxBuy
        varCostTotal = defaultDecimal --calcCostTotal
        varPoolAtStart = iPool varInput
        varPoolNew = varPoolAtStart - varSold - varCostTotal

calcStoploss :: Double -> Double 
calcStoploss 

{--calcRiskInput :: Double -> Double -> Double
calcRiskInput undefined

calcPercentageOf :: Double -> Double -> Double
calcPercentageOf undefined

calcRiskInitial :: Double -> Double -> Double
calcRiskInitial = undefined

calcCostTotal :: Double -> Double -> Double
calccostTotal = undefined

calcCostOther :: Double -> Double -> Double
calcCostOther = undefined
-}

-- calcBoughtSold
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

main = do
    -- TODO: option parsing
    let varInput = Input {
            iPool = 104000.0 
            ,iMoneyToUse = 5000.0
            ,iLongShort = 'L'
            ,iPrice = 25.0
            ,iShares = 100
            ,iCommission = 7.5
            ,iTax = 0.0025
            ,iRisk = 0.02 
            ,iMarket = "world"
            ,iStockname = ".Gold"
            ,iSpread = 4.0
            ,iCurrency = "EUR"
            ,iExchangeRate = 1.0
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

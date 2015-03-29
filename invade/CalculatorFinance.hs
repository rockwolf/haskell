------------------------------------------------------------------------------
-- | This module does financial calculations for trading and investing.
-- | It can also be compiled as a library via the FFI.
--   See LICENSE file for copyright and license info.
------------------------------------------------------------------------------
{-# LANGUAGE ForeignFunctionInterface #-}

module CalculatorFinance where

-----------------------------------------------------------------------------
-- ||| Imports
-----------------------------------------------------------------------------
import Foreign.C.Types
import Foreign.C.String
import Data.List
import Data.Char

-----------------------------------------------------------------------------
-- ||| Other functions
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- | Calculate what percentage value is from from_value.
-----------------------------------------------------------------------------
calcPercentageOf :: Double -> Double -> Double
calcPercentageOf value from_value = (value / from_value) * 100.0

-----------------------------------------------------------------------------
-- | ConvertFromOrig:
--
-- Returns a price, with an exchange rate applied to it.
-- Used to convert a given currency to a new currency.
-----------------------------------------------------------------------------
convertFromOrig :: CDouble -> CDouble -> IO CDouble
convertFromOrig a_price a_exchange_rate =
  return $ a_price * a_exchange_rate

-----------------------------------------------------------------------------
-- |   ConvertToOrig:
--
-- Returns a price in the original currency, with the
-- exchange rate no longer applied to it.
-----------------------------------------------------------------------------
convertToOrig :: CDouble -> CDouble -> IO CDouble
convertToOrig a_converted_price a_exchange_rate =
  return $ a_converted_price / a_exchange_rate

-----------------------------------------------------------------------------
-- |   upperCase and lowerCase:
--
-- upperCase returns a string with lowerCase characters converted to upperCase
-- lowerCase returns a string with upperCase characters converted to lowerCase
-----------------------------------------------------------------------------
upperCase, lowerCase :: String -> String
upperCase = map toUpper
lowerCase = map toLower

-----------------------------------------------------------------------------
-- |   Merge:
-- Merges 2 lists together.
-----------------------------------------------------------------------------
merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys


-----------------------------------------------------------------------------
-- ||| Before trade
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- | Calculates the stoploss.
--
-- Note:
-- Long
-- ----
-- amount selling at stoploss - amount at buying = initial risk of pool
-- (S.Pb + S.Pb.T + C) - (S.Ps - S.Ps.T - C) = R/100 * pool
-- Short
-- -----
-- amount selling - amount buying at stoploss = initial risk of pool
-- (S.Psl + S.Psl.T + C) - (S.Ps - S.Ps.T - C) = R/100 * pool
-----------------------------------------------------------------------------
calcStoploss :: CDouble -> CInt -> CDouble -> CDouble -> CDouble -> CDouble -> CBoolean -> IO CDouble
calcStoploss a_price a_shares a_tax a_commission a_risk a_pool a_is_long =
    return (if a_is_long
    then l_numerator_long / l_denominator_long
    else l_numerator_short / l_denomitator_short)
    where
        l_risk = realToFrac a_risk / 100.0
        l_amount = realToFrac $ calcAmount' (realToFrac a_price) (fromIntegral a_shares)
        l_shares = fromIntegral a_shares
        l_tax = realToFrac (a_tax / 100.0)
        l_numerator_long = l_shares * a_price * (1.0 + a_tax / 100.0) - a_risk / 100.0 * a_pool + 2.0 * a_commission
        l_denominator_long = a_shares * 1.0 - a_tax / 100.0
        l_numerator_short = l_shares * a_price * (1.0 - a_tax / 100.0) + a_risk / 100.0 * a_pool - 2.0 * a_commission
        l_denominator_short = a_shares * 1.0 + a_tax / 100.0

-----------------------------------------------------------------------------
-- |  Calculates the risk based on total pool and input.
--
-- Consider this the theoretical risk we want to take.
-----------------------------------------------------------------------------
calcRiskInput :: CDouble -> CDouble -> IO CDouble
calcRiskInput a_risk a_pool =
    return $ l_risk * l_pool
    where
        l_risk = realToFrac $ a_risk / 100.0
        l_pool = realToFrac a_pool

-----------------------------------------------------------------------------
-- |   Calculates the initial risk.
--
-- This is the risk we will take if our stoploss is reached.
-- This should be equal to the risk_input if everything was
-- correctly calculated.
-- Note:
-- Long
-- ----
-- S.Pb + S.Pb.T + C - (S.Psl - S.Psl.T - C)
-- Short
-- -----
-- S.Ps + S.Psl.T + C - (S.Ps - S.Ps.T - C)
-----------------------------------------------------------------------------
calcRiskInitial :: CDouble -> CInt -> CDouble -> CBoolean -> IO CDouble
calcRiskInitial a_price a_shares a_stoploss a_is_long =
    return (if a_is_long
    then l_numerator_long / l_denominator_long  
    else l_numerator_short / l_denominator_short)
    where
        l_shares = fromIntegral a_shares
        l_numerator_long = a_shares * a_price * (1.0 + a_tax / 100.0) - a_risk / 100.0 * a_pool + 2.0 * a_commission
        l_denominator_long = a_shares * 1.0 - a_tax / 100.0
        l_numerator_short = a_shares * a_price * (1.0 - a_tax / 100.0) + a_risk / 100.0 * a_pool - 2.0 * a_commission
        l_denominator_short = a_shares * 1.0 + a_tax / 100.0

-----------------------------------------------------------------------------
-- |   Calculates the risk we actually took,
-- | based on the data at the end of the trade.
--
-- It's the same for long and short.
-- Note:
-- risk_actual = S.Pb + S.Pb.T + Cb - (S.Ps - S.Ps.T - Cs)
-----------------------------------------------------------------------------
-- NOTE: price_sell > stoploss = max risk was the initial risk
calcRiskActual :: CDouble -> CInt -> CDouble -> CInt -> CDouble -> CDouble -> IO CDouble
calcRiskActual a_price_buy a_shares_buy a_price_sell a_shares_sell a_stoploss a_risk_initial =
    return $ if (a_profit_loss < 0.0) and (abs a_profit_loss < a_risk_initial) or (a_profit_loss >= 0.0)
    then a_risk_initial
    else a_price_buy * l_shares_buy - a_price_sell * l_shares_sell
    where
        l_shares_buy = fromIntegral a_shares_buy
        l_shares_sell = fromIntegral a_shares_sell

-----------------------------------------------------------------------------
-- |   Function to calculate R-multiple.
-----------------------------------------------------------------------------
calcRMultiple :: CDouble -> CDouble -> CDouble -> IO CDouble
calcRMultiple a_price_buy a_price_sell a_stoploss =
    return $ (a_price_sell - a_price_buy) / (a_price_buy - a_stoploss)

calcCostTotal :: CDouble -> CDouble -> CDouble -> CDouble -> IO CDouble
calcCostTotal a_tax_buy a_commission_buy a_tax_sell a_commission_sell =
    return $ a_tax_buy + a_commission_buy + a_tax_sell + a_commission_sell

-- NOTE: commission + tax = seperate = costs
calcAmount :: CDouble -> CInt -> IO CDouble
calcAmount a_price a_shares =
    return $ realToFrac (calcAmount' (realToFrac a_price) (fromIntegral a_shares))

--- internal function that does not return an IO monad
calcAmount' :: Double -> Int -> Double
calcAmount' a_price a_shares = a_price * fromIntegral a_shares

-- cost of transaction (tax and commission)
costTransaction :: CInt -> CDouble -> CInt -> CDouble -> CDouble -> IO CDouble
costTransaction a_transactionid a_price a_shares a_tax a_commission =
    -- Note: transactionid = 
    -- 0: buy
    -- 1: sell
    --var_transaction <- peekCString transaction
    --case lowerCase var_transaction of
    case a_transactionid of
        --[] -> error errorMsgEmpty
        0 -> return ((a_price * l_shares * (1 + a_tax)) + a_commission)
        1 -> return ((a_price * l_shares * (1 - a_tax)) - a_commission)
    where
        l_shares = fromIntegral a_shares
    --    errorMsgEmpty = "Error in costTransaction: buy or sell not specified!"

calcProfitLoss :: CDouble -> CDouble -> CDouble -> IO CDouble
calcProfitLoss amount_sell_simple amount_buy_simple totalcost =
    return $ amount_sell_simple - amount_buy_simple - totalcost

calcCostOther :: CDouble -> CDouble -> IO CDouble
calcCostOther totalCost profitLoss =
    return $ if diffCostProfit > defaultDecimal
    then diffCostProfit
    else defaultDecimal
    where
        diffCostProfit = totalCost - profitLoss
        defaultDecimal = 0.0

calcSharesRecommended :: IO CInt
calcSharesRecommended =
    return 0

-----------------------------------------------------------------------------
-- |   calcPrice:
--
-- Returns the price a commodity must have, for the given amount, tax, commission
-- and number of shares.
-----------------------------------------------------------------------------
calcPrice :: CDouble -> CDouble -> CDouble -> CInt -> IO CDouble
calcPrice a_amount a_commission a_tax a_shares =
    return $ (a_amount - a_commission) / ((1 + a_tax) * l_shares)
    where
        l_shares = fromIntegral a_shares

--
-- Market information
--

markets = merge marketsEuronextBrussels $
          merge marketsEuronextOther $
          merge marketsUS $
          merge marketsCfdShare $
          merge marketsCfdDev1 $
          merge marketsCfdDev2 $
          merge marketsCfdNonShare marketsCfdUS

-- TODO: this market information should come from the database.
-- Make a library so that it does not need a db connection?
-- Or get db credentials from a file <- easiest!

-- binb00
-- NOTE: see list of country codes at:
-- http://www.iso.org/iso/country_codes/iso_3166_code_lists/country_names_and_code_elements.htm
marketsEuronextBrussels = [
        "ebr"
    ]

marketsEuronextOther = [
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

marketsUS = [
        "nyse"
        ,"nasdaq"
        ,"otc bb & pinksheets"
        ,"amex"
        ,"other us"
    ]

-- TODO: incorporate this into the calculations
-- but only when we plan on doing options trading.
marketsOptionsEuronext = [
        "options ams"
        ,"options ebr"
    ]
-- /binb00

-- whsi00
-- TODO: where are commodities like OJ situated?
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

marketsCfdNonShare = [
           "cfd .gold"
           ,"cfd .silver"
           ,"cfd oil"
           ,"cfd index"
           ,"cfd other non-share"
           ]

marketsCfdUS = [
           "cfd US"
           ]
-- /whsi00

--
-- Commission calculations
--

calcCommission :: CString -> CString -> CString -> CDouble -> CInt -> IO CDouble
calcCommission  account market stockname price shares = do
    -- NOTE: peekCString makes the CString an IO String
    -- NOTE: we 'pull' the market out of the IO monad
    var_account <- peekCString account
    var_market <- peekCString market
    var_stockname <- peekCString stockname
    var_amount_simple <- calcAmountSimple price shares
    case lowerCase var_account of
        "binb00" -> return (realToFrac (getBinb00Commission var_market var_stockname (realToFrac var_amount_simple)))
        "whsi00" -> return (realToFrac $ getWhsi00Commission var_market var_stockname (realToFrac price) (fromIntegral shares))
        _ -> return (realToFrac 0.0)

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
        perDiscNumber = realToFrac (ceiling $ amount_simple / 50000.0)

isEuronextBrussels :: String -> Bool
isEuronextBrussels market = market `elem` markets_euronext_brussels

isEuronextOther :: String -> Bool
isEuronextOther market = market `elem` markets_euronext_other

isUS :: String -> Bool
isUS market = market `elem` markets_us

isEuroExchange :: String -> Bool
isEuroExchange market = market == "dummy"

isCanadaExchange :: String -> Bool
isCanadaExchange market = market == "dummy"

isSwissScandinavianExchange :: String -> Bool
isSwissScandinavianExchange market = market == "dummy"

-- TODO: get these values from T_PARAMETER
getBinb00Commission2500 :: String -> String -> Double
getBinb00Commission2500 market _
    | isEuronextBrussels market          = 7.25        
    | isEuronextOther market             = 9.75
    | isEuroExchange market              = 12.75
    | isUS market                        = 12.75
    | isCanadaExchange market            = 19.75
    | isSwissScandinavianExchange market = 29.75
    | otherwise                          = 0.0

-- TODO: find the correct values for canada/scand/euroexch
getBinb00Commission5000 :: String -> String -> Double
getBinb00Commission5000 market _
    | isEuronextBrussels market          = 9.75        
    | isEuronextOther market             = 9.25
    | isEuroExchange market              = 12.75
    | isUS market                        = 12.75
    | isCanadaExchange market            = 19.75
    | isSwissScandinavianExchange market = 29.75
    | otherwise                          = 0.0

getBinb00Commission25000 :: String -> String -> Double
getBinb00Commission25000 market _
    | isEuronextBrussels market          = 13.75        
    | isEuronextOther market             = 13.75
    | isEuroExchange market              = 16.75
    | isUS market                        = 16.75
    | isCanadaExchange market            = 24.75
    | isSwissScandinavianExchange market = 29.75
    | otherwise                          = 0.0

getBinb00Commission50000 :: String -> String -> Double
getBinb00Commission50000 market _
    | isEuronextBrussels market          = 19.75        
    | isEuronextOther market             = 19.75
    | isEuroExchange market              = 22.75
    | isUS market                        = 22.75
    | isCanadaExchange market            = 29.75
    | isSwissScandinavianExchange market = 59.75
    | otherwise                          = 0.0

getBinb00Commission50000Plus :: String -> String -> Double
getBinb00Commission50000Plus market _
    | isEuronextBrussels market          = 19.75        
    | isEuronextOther market             = 19.75
    | isEuroExchange market              = 19.75
    | isUS market                        = 19.72
    | isCanadaExchange market            = 29.75
    | isSwissScandinavianExchange market = 29.75
    | otherwise                          = 0.0

getBinb00CommissionOptions :: String -> String -> Int -> Double
getBinb00CommissionOptions market _ contracts
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
isOptionsEuronext market = market `elem` markets_options_euronext

getWhsi00Commission :: String -> String -> Double -> Int -> Double
getWhsi00Commission market stockname price shares
    | isNonShareCfd market         = 3.0
    | isShareCfd market            = 4.50 + calcPercentageOf 0.054 amount_simple
    | isShareCfdDev1 market        = 4.50 + calcPercentageOf 0.09 amount_simple
    | isShareCfdDev2 market        = 4.50 + calcPercentageOf 0.19 amount_simple
    | isShareCfdUS market          = 4.50 + 0.023 * fromIntegral shares
    | otherwise                    = 0.0
    where
        amount_simple = calcAmountSimple' (realToFrac price) (fromIntegral shares)
        

isNonShareCfd :: String -> Bool
isNonShareCfd market = market `elem` marketsCfdNonShare

isShareCfd :: String -> Bool
isShareCfd market = market `elem` marketsCfdShare

isShareCfdDev1 :: String -> Bool
isShareCfdDev1 market = market `elem` marketsCfdDev1

isShareCfdDev2 :: String -> Bool
isShareCfdDev2 market = market `elem` marketsCfdDev2

isShareCfdUS :: String -> Bool
isShareCfdUS market = market `elem` marketsCfdUS

-----------------------------------------------------------------------------
-- ||| Export functions
-----------------------------------------------------------------------------
foreign export ccall
    calcStoploss :: CDouble -> CInt -> CDouble -> CDouble -> CDouble -> CDouble -> IO CDouble

{-
foreign export ccall
  calcRiskInput :: CDouble -> CDouble -> IO CDouble

foreign export ccall
  calcRiskInitial :: CDouble -> CInt -> CDouble -> IO CDouble

foreign export ccall
  calcRiskActual :: CDouble -> CInt -> CDouble -> CInt -> CDouble -> CDouble -> IO CDouble

foreign export ccall
  calcRMultiple :: CDouble -> CDouble -> CDouble -> IO CDouble

foreign export ccall
  calcCostTotal :: CDouble -> CDouble -> CDouble -> CDouble -> IO CDouble

foreign export ccall
  calcAmountSimple :: CDouble -> CInt -> IO CDouble

foreign export ccall
  costTransaction :: CInt -> CDouble -> CInt -> CDouble -> CDouble -> IO CDouble

foreign export ccall
  calcProfitLoss :: CDouble -> CDouble -> CDouble -> IO CDouble

foreign export ccall
  calcCostOther :: CDouble -> CDouble -> IO CDouble

foreign export ccall
    calcPrice :: CDouble -> CDouble -> CDouble -> CInt -> IO CDouble
-}
--
-- TEST
--
{-
test :: CDouble -> IO CDouble
test ivar = do
    return(ivar * 2.0)

foreign export ccall
    test :: CDouble -> IO CDouble
-}

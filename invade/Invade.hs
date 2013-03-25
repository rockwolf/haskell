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

--currentDate :: IO (Integer,Int,Int) -- :: (year,month,day)
--currentDate = getCurrentTime >>= return . toGregorian . utctDay

getPool :: Double
getPool = 100000.0

getMargin :: Double
getMargin = 25.0

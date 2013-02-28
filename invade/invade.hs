{- |
Module      :  invade.hs
Description :  Investment and trading calculations.
Copyright   :  (c) <Andy Nagels>
License     :  See LICENSE file for license details.

Maintainer  :  <no@e-mail.given>
Stability   :  unstable
Portability :  portable

Makes the necessary calculations with regards to trading, investing and money management.
-}

import Data.Char

-- calcBoughtSold
calcBoughtSold :: Double -> Double -> Double
calcBoughtSold price shares = price * shares

-- cost of transaction (tax and commission)
costTransaction :: String -> Double -> Double -> Double -> Double -> Double
costTransaction transaction price shares tax commission =
    case lowerCase transaction of
        [] ->
            error errorMsgEmpty
        "buy" ->
            (price * shares * (1 + tax)) + commission
        "sell" ->
            (price * shares * (1 - tax)) - commission
    where
        errorMsgEmpty = "Error in costTransaction: buy or sell not specified!"

upperCase, lowerCase :: String -> String
upperCase = map toUpper
lowerCase = map toLower

main = do
    -- TODO: what input do we expect
    -- And what output?
    {-
        Inputs:
            pool, could later be retrieved from the db
            risk percentage,
            price,
            shares
        Output:
            shares to buy,
            minimum stoploss
            bought,
            sold,
            cost_buy
            cost_sell --sell at minimum stoploss
    -}
    -- bought
    let priceBuy = 25.0
    let sharesBuy = 100
    putStrLn $ show (
        calcBoughtSold priceBuy sharesBuy)
    -- sold
    let priceSell = 25.0
    let sharesSell = sharesBuy
    putStrLn $ show (
        calcBoughtSold priceSell sharesSell)
    -- cost_buy
    let taxBuy = 0.0025
    let commissionBuy = 7.5
    putStrLn $ show (
        costTransaction "buy" priceBuy sharesBuy taxBuy commissionBuy)
    -- cost_sell
    let taxSell = taxBuy
    let commissionSell = commissionBuy
    putStrLn $ show (
        costTransaction "sell" priceSell sharesSell taxSell commissionSell)

{-# LANGUAGE ForeignFunctionInterface #-}

module CalculatorFinance where

import Foreign.C.Types

--
-- test function
--

fibonacci :: Int -> Int
fibonacci n = fibs !! n
    where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibonacci_hs :: CInt -> CInt
fibonacci_hs = fromIntegral . fibonacci . fromIntegral

--
-- internal functions
--

-- cost of transaction (tax and commission)
costTransaction :: String -> Double -> Int -> Double -> Double -> Double
costTransaction transaction price shares tax commission =
    case transaction of
        "buy" -> (price * (fromIntegral shares) * (1 + tax)) + commission
        "sell" -> (price * (fromIntegral shares) * (1 - tax)) - commission

--
-- to interface to the outside
--

-- calcBoughtSold
calcBoughtSold :: Double -> Int -> Double
calcBoughtSold price shares = price * fromIntegral shares

calcBoughtSold_hs :: CDouble -> CInt -> CDouble
calcBoughtSold_hs price shares =
    fromIntegral . calcBoughtSold $ (fromIntegral price) (fromIntegral shares)

-- costTransactionBuy
costTransactionBuy :: Double -> Int -> Double -> Double -> Double
costTransactionBuy price shares tax commission =
    costTransaction "buy" price shares tax commission

costTransactionBuy_hs :: CDouble -> CInt -> CDouble -> CDouble -> CDouble
costTransactionBuy_hs price shares tax commission =
    fromIntegral . costTransactionBuy $ (fromIntegral price) (fromIntegral shares) (fromIntegral commission)
    
-- costTransactionSell
costTransactionSell :: Double -> Int -> Double -> Double -> Double
costTransactionSell price shares tax commission =
    costTransaction "sell" price shares tax commission

costTransactionSell_hs :: CDouble -> CInt -> CDouble -> CDouble -> CDouble
costTransactionSell_hs price shares tax commission =
    fromIntegral . costTransactionBuy $ (fromIntegral price) (fromIntegral shares) (fromIntegral commission)

-- costTotal
costTotal :: Double -> Double -> Double -> Double -> Double
costTotal tax_buy commission_buy tax_sell commission_sell =
    tax_buy + commisssion_buy + tax_sell + commission_sell

costTotal_hs :: CDouble -> CDouble -> CDouble -> CDouble -> CDouble
costTotal_hs tax_buy commission_buy tax_sell commission_sell = 
        fromIntegral . costTotal $ (fromIntegral tax_buy) (fromIntegral commission_buy) (fromIntegral tax_sell) (fromIntegral commission_sell)
    
--
-- exports
--

foreign export ccall fibonacci_hs :: CInt -> CInt

foreign export ccall calcBoughtSold :: CDouble -> CInt -> CDouble

foreign export ccall costTransactionBuy :: CDouble -> CInt -> CDouble -> CDouble

foreign export ccall costTransactionSell :: CDouble -> CInt -> CDouble -> CDouble

foreign export ccall costTotal :: CDouble -> CDouble -> CDouble -> CDouble -> CDouble

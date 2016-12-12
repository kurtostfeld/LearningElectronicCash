{-# LANGUAGE ScopedTypeVariables #-}

module BankMember where

import Bank

import BinaryUtilities
import ECashCommon
import Modulo
import RandomUtilities
import SimpleRSA

import Data.Bits
import Data.List ((\\), sortBy)
import Data.Maybe (fromJust)
import System.Random

-- BankMember. Data contains data that the bank member or a computing device of the member has.
data BankMember = BankMember
  { bank :: Bank
  , memberName :: String
  , memberId :: Int
  , transactionCounter :: Int
  , challengeSequencePrefix :: [Bool]
  }

instance Show BankMember where
  show a = "BankMember {"
    ++ "id:" ++ (show $ memberId a)
    ++ ", name:" ++ (show $ memberName a)
    ++ ", transaction:" ++ (show $ transactionCounter a)
    ++ ", challengeSequencePrefix:" ++ (show $ challengeSequencePrefix a)
    ++ "}"

createBankMember :: RandomGen g => Bank -> String -> g -> ((Bank, BankMember), g)
createBankMember bank name gen1 =
  let ((bank', id, challengeSequencePrefix), gen2) = openBankAccount bank name gen1
      bankMember = BankMember
        { bank = bank
        , memberName = name
        , memberId = id
        , transactionCounter = 0
        , challengeSequencePrefix = challengeSequencePrefix
        }
  in ((bank', bankMember), gen2)

-- This is the data that the user withdrawing the coin would have before it is spent.
-- This is not anonymized. This should be spent once and discarded.
data PayerCoin = PayerCoin
  { coinBankRsaPublicKey :: SimpleRsaPublicKey
  , coinA :: [Integer]
  , coinC :: [Integer]
  , coinD :: [Integer]
  , coinX :: [Integer]
  , coinY :: [Integer]
  -- This is the raw user + transaction.
  , coinUCatV :: [Int]
  -- This is the bank authentication.
  --, coinFCubeRoot :: [Integer]
  , coinFCubeRootProduct :: Integer
  } deriving (Show)

-- This serves internal assertion and diagnostic purpose only.
-- This check is redundant
verifyPayerCoin :: PayerCoin -> Either String String
verifyPayerCoin coin =
  let (bankRsaN, _) = coinBankRsaPublicKey coin
      f_xi_yi = zipWith (chaumF bankRsaN) (coinX coin) (coinY coin)
      f_xi_yi_product = foldl (modMultiply bankRsaN) 1 f_xi_yi
      f_xi_yi_product_bank = modExponentiation bankRsaN (coinFCubeRootProduct coin) 3
      --f_xi_yi_bank = fmap (\x -> modExponentiation bankRsaN x 3) (coinFCubeRoot coin)
  in if f_xi_yi_product /= f_xi_yi_product_bank then Left "cube root values do not match"
     else Right "coin validates"

       -- withdrawCoin
withdrawCoin :: RandomGen g => Bank -> BankMember -> g -> (Either String (Bank, BankMember, PayerCoin), g)
withdrawCoin bank bankMember gen1 =
  let bankRsaPublicKey'@(bankRsaN, bankRsaE) = bankRsaPublicKey bank
      memberId' = memberId bankMember
      transactionCounter' = transactionCounter bankMember
      modNBounds = (0, bankRsaN - 1)
      (ai, gen2) = randomRNextN modNBounds bankK gen1
      (ci, gen3) = randomRNextN modNBounds bankK gen2
      (di, gen4) = randomRNextN modNBounds bankK gen3
      (ri, gen5) = randomRNextN modNBounds bankK gen4
      -- Calculate blinds
      xi = zipWith (chaumG bankRsaN) ai ci
      -- vi = [v, v+1, ..., v+bankK]
      vi = fmap (+transactionCounter') [1..bankK]
      -- (u || (v + i))
      ucatvi = zipWith pack32x2To64 (repeat memberId') vi
      -- a_i `xor` (u || (v + i))
      ai_xor_ucatvi = zipWith xor ai $ fmap fromIntegral ucatvi
      yi = zipWith (chaumG bankRsaN) ai_xor_ucatvi di
      f_xi_yi = zipWith (chaumF bankRsaN) xi yi
      ri_cubed = fmap (\x -> modExponentiation bankRsaN x 3) ri
      blinds = zipWith (modMultiply bankRsaN) ri_cubed f_xi_yi                       
      -- Send blinded candidates and initial request to bank. Get back index selection.
      (withdrawlRequest, gen6) = bankWithdrawlRequestPart1 bank memberId' blinds gen5
      indexSelectionR' = indexSelectionR withdrawlRequest
      indexSelectionRInv = [0..(bankK-1)] \\ indexSelectionR'
      withdrawlResponse = bankWithdrawlRequestPart2 bank withdrawlRequest
        (sublistByIndices indexSelectionR' ai)
        (sublistByIndices indexSelectionR' ci)
        (sublistByIndices indexSelectionR' di)
        (sublistByIndices indexSelectionR' ri)
  in case withdrawlResponse of
    Left bankError -> (Left $ "The bank rejected the withdrawl request: " ++ bankError, gen6)
    Right (bank', blindedCubeRootProduct) ->
      let riNotInR = sublistByIndices indexSelectionRInv ri
          riNotInRInv = fmap fromJust $ fmap (modMultInverse bankRsaN) riNotInR
          riNotInRInvProduct = foldl (modMultiply bankRsaN) 1 riNotInRInv
          fCubeRootProduct = modMultiply bankRsaN blindedCubeRootProduct riNotInRInvProduct
          f_xi_yi_notr = sublistByIndices indexSelectionRInv f_xi_yi
          f_xi_yi_product = foldl (modMultiply bankRsaN) 1 f_xi_yi_notr
          f_xi_yi_product_bank = modExponentiation bankRsaN fCubeRootProduct 3
--          fCubeRoots = zipWith (modMultiply bankRsaN) blindedCubeRoots riNotInRInv
--          f_xi_yi_notr = sublistByIndices indexSelectionRInv f_xi_yi
--          f_xi_yi_bank = fmap (\x -> modExponentiation bankRsaN x 3) fCubeRoots
      in if f_xi_yi_product == f_xi_yi_product_bank then
        -- Sort everything by f_xi_yi order.
        let sortOrder = getSortedIndices f_xi_yi_notr
            coin = PayerCoin
              { coinBankRsaPublicKey = bankRsaPublicKey'
              , coinA = sublistByIndices sortOrder $ sublistByIndices indexSelectionRInv ai
              , coinC = sublistByIndices sortOrder $ sublistByIndices indexSelectionRInv ci
              , coinD = sublistByIndices sortOrder $ sublistByIndices indexSelectionRInv di
              , coinX = sublistByIndices sortOrder $ sublistByIndices indexSelectionRInv xi
              , coinY = sublistByIndices sortOrder $ sublistByIndices indexSelectionRInv yi
              , coinUCatV = sublistByIndices sortOrder $ sublistByIndices indexSelectionRInv ucatvi
              --, coinFCubeRoot = sublistByIndices sortOrder fCubeRoots
              , coinFCubeRootProduct = fCubeRootProduct
              }
            bankMember' = bankMember { transactionCounter = (transactionCounter bankMember) + bankK }
        in (Right (bank', bankMember', coin), gen6)
      else (Left "Bank cube root responses do not validate", gen6)


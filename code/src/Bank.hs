{-# LANGUAGE ScopedTypeVariables #-}

module Bank
(
  bankK,
  bankKDiv2,
  bankKDiv4,
  Bank,
  bankRsaPublicKey,
  createBank,
  openBankAccount,
  bankWithdrawlRequestPart1,
  WithdrawlRequestToken,
  indexSelectionR,
  bankWithdrawlRequestPart2,
  RecipientCoin(..),
  verifyChallengeResponse,
  depositCoin
) where

import BinaryUtilities
import ECashCommon
import Modulo
import RandomUtilities
import SimpleRSA

import Data.Bits
import Data.List ((\\), intercalate, sort, zip4)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map
import System.Random

       -- Bank
bankK :: Int = 32
bankKDiv2 :: Int = bankK `quot` 2
bankKDiv4 :: Int = bankK `quot` 4

-- Bank-side data on a given member.
data BankMemberBankSide = BankMemberBankSide
  {
    memberName :: String
  , memberId :: Int
  , transactionCounter :: Int
  , challengeSignaturePrefix :: [Bool]
  } deriving (Show)

data Bank = Bank
  { bankName :: String
  , memberMap :: Map.Map Int BankMemberBankSide
  , bankRsaPrivate :: SimpleRsaPrivateKey
  , depositLogByC :: Map.Map Integer (Int, RecipientCoin)
  } deriving (Show)

bankRsaPublicKey :: Bank -> SimpleRsaPublicKey
bankRsaPublicKey bank = simpleRsaCreatePublic $ bankRsaPrivate bank

createBank :: RandomGen g => String -> g -> (Bank, g)
createBank name gen =
  let (rsaPrivate, gen2) = simpleRsaPrivateKeyGen gen
  in (Bank
  { bankName = name
  , memberMap = Map.empty
  , bankRsaPrivate = rsaPrivate
  , depositLogByC = Map.empty
  }, gen2)

openBankAccount :: RandomGen g => Bank -> String -> g -> ((Bank, Int, [Bool]), g)
openBankAccount bank name gen1 =
  let (id, gen2) = randomR (0, maxInt32) gen1
      (challengeSignaturePrefix, gen3) = randomNextN bankKDiv4 gen2
      bankMember = BankMemberBankSide
        { -- owningBank = bank
          memberName = name
        , memberId = id
        , transactionCounter = 0
        , challengeSignaturePrefix = challengeSignaturePrefix
        }
      bank' = bank { memberMap = Map.insert id bankMember $ memberMap bank }
  in ((bank', id, challengeSignaturePrefix), gen2)

data WithdrawlRequestToken = WithdrawlRequestToken
  { bankMemberId :: Int
  , blindedCandidates :: [Integer]
  , indexSelectionR :: [Int] -- 0 indexed. bankKDiv2 values in [0,bankK-1]
  }

bankWithdrawlRequestPart1 :: RandomGen g => Bank -> Int -> [Integer] -> g -> (WithdrawlRequestToken, g)
bankWithdrawlRequestPart1 bank bankMemberId blindedCandidates gen1 =
  let (indexSelectionRUnsorted, gen2) = randomRUniqueN (0, bankK - 1) bankKDiv2 gen1
      indexSelectionR = sort indexSelectionRUnsorted
      requestToken = WithdrawlRequestToken
        { bankMemberId = bankMemberId
        , blindedCandidates = blindedCandidates
        , indexSelectionR = indexSelectionR
        }
  in (requestToken, gen2)

-- In a proper server implementation, the request data would be stored server side and referenced by a cookie.
-- As-is, the user might potentially modify the data in the request record, which the server should prohibit.
bankWithdrawlRequestPart2 :: Bank -> WithdrawlRequestToken ->
    [Integer] -> [Integer] -> [Integer] -> [Integer] -> Either String (Bank, Integer)
bankWithdrawlRequestPart2 bank request ai ci di ri =
  let (bankRsaN, bankRsaE) = bankRsaPublicKey bank
      indexSelectionRInv = [0..(bankK-1)] \\ (indexSelectionR request)
      userBlindsInR = sublistByIndices (indexSelectionR request) (blindedCandidates request)
      userBlindsNotInR = sublistByIndices indexSelectionRInv (blindedCandidates request)
      bankMember = fromJust $ Map.lookup (bankMemberId request) (memberMap bank)
      -- Calculate bank side blind values. See if they match.
      bankCalculatedVerifyBlinds = calculateBlinds (bankMemberId request) (transactionCounter bankMember) bankRsaN
          (indexSelectionR request) ai ci di ri
      blindsMatch = bankCalculatedVerifyBlinds == userBlindsInR
  in if blindsMatch then
    -- Do withdrawl and give user coin.
    -- Optionally deduct a coin from the members account and add to the transaction log.
    let cubeRootValues = fmap (simpleRsaCubeRoot (bankRsaPrivate bank)) userBlindsNotInR
        bankMember' = bankMember { transactionCounter = (transactionCounter bankMember) + bankK }
        bank' = bank { memberMap = Map.insert (bankMemberId request) bankMember' $ memberMap bank }
        cubeRootProduct = foldl (modMultiply bankRsaN) 1 cubeRootValues
    in Right (bank', cubeRootProduct)
  else Left "Blinds do not match."

-- This represents a coin received by a recipient after completing the peer to peer challenge.
data RecipientCoin = RecipientCoin
  { rcIssuingBankRsaPublicKey :: SimpleRsaPublicKey
  --, rcPaymentFCubeRoot :: [Integer]
  , rcPaymentFCubeRootProduct :: Integer
  , rcChallengeSequence :: [Bool]
  , rcChallengeResponse :: [(Integer, Integer, Integer)]
  } deriving (Show, Eq)

paymentChallengeVerifyCalculateFXY :: Integer -> Bool -> (Integer, Integer, Integer) -> Integer
paymentChallengeVerifyCalculateFXY issuingBankRsaN False challengeResponse =
  let (di, xi, aXorUiVi) = challengeResponse
      yi = chaumG issuingBankRsaN aXorUiVi di
      fi = chaumF issuingBankRsaN xi yi
  in fi
paymentChallengeVerifyCalculateFXY issuingBankRsaN True challengeResponse =
  let (ai, ci, yi) = challengeResponse
      xi = chaumG issuingBankRsaN ai ci
      fi = chaumF issuingBankRsaN xi yi
  in fi

verifyChallengeResponse :: SimpleRsaPublicKey -> [Bool] -> [(Integer, Integer, Integer)] -> Integer -> Bool
verifyChallengeResponse issuingBankRsaPublicKey challengeSequence challengeResponse fCubeRootProduct =
  let issuingBankRsaN = fst issuingBankRsaPublicKey
      fvalues = fmap (uncurry $ paymentChallengeVerifyCalculateFXY issuingBankRsaN) $
        zip challengeSequence challengeResponse
      sortedF = sort fvalues
      fProduct = foldl (modMultiply issuingBankRsaN) 1 fvalues
      fProductMatch = modExponentiation issuingBankRsaN fCubeRootProduct 3
  in   (fProduct == fProductMatch)
    && (fvalues == sortedF)
    && (length challengeSequence == bankKDiv2)
    && (length challengeResponse == bankKDiv2)

unmaskDoubleSpend :: (Integer, Integer, Integer) -> (Integer, Integer, Integer) -> (Int, Int)
unmaskDoubleSpend (d, x, aXorUCatV) (a, c, y) = unpack64To32x2 $ fromIntegral $ aXorUCatV `xor` a
  
diagnoseDoubleDeposit :: RecipientCoin -> RecipientCoin -> String
diagnoseDoubleDeposit coina coinb =
  let allZipped = zip4 (rcChallengeSequence coina) (rcChallengeSequence coinb)
               (rcChallengeResponse coina) (rcChallengeResponse coinb)
      differing = filter (\(t1, t2, t3, t4) -> t1 /= t2) allZipped
      zeroOneResponses = fmap (\(t1, t2, t3, t4) -> if t1 then (t4, t3) else (t3, t4)) differing
      uAndVValues = fmap (uncurry unmaskDoubleSpend) zeroOneResponses
  in if null uAndVValues
    then "Identical challenge sequence. Likely same recipient member"
  else
    let strUandV = fmap (\(u,v) -> "u:" ++ (show u) ++ ",v:" ++ (show v)) uAndVValues
    in "different challenge sequence: " ++ intercalate ", " strUandV      

depositCoin :: Bank -> Int -> RecipientCoin -> Either String Bank
depositCoin bank1 memberId recipientCoin = case Map.lookup memberId (memberMap bank1) of
  Nothing -> Left "Can't find member by that id"
  Just bankMember ->
    if (rcIssuingBankRsaPublicKey recipientCoin) /= (bankRsaPublicKey bank1)
    then Left "Wrong bank"
    else if not $ verifyChallengeResponse (rcIssuingBankRsaPublicKey recipientCoin)
      (rcChallengeSequence recipientCoin) (rcChallengeResponse recipientCoin)
      (rcPaymentFCubeRootProduct recipientCoin)
    then Left "Failed verification test"
    else case Map.lookup (rcPaymentFCubeRootProduct recipientCoin) (depositLogByC bank1) of
      Just (conflictDepositMemberId, conflictDepositRecipientCoin) ->
        Left $ "conflict detected: " ++ (diagnoseDoubleDeposit recipientCoin conflictDepositRecipientCoin)
      Nothing ->
        Right bank1 { depositLogByC = Map.insert
                      (rcPaymentFCubeRootProduct recipientCoin) (memberId, recipientCoin)
                      $ depositLogByC bank1 }





{-# LANGUAGE ScopedTypeVariables #-}

module PeerPayer where

import Bank
import BankMember
import PeerRecipient

import Data.Bits (xor)
import System.Random

challengeResponseF :: PayerCoin -> Int -> Bool -> (Integer, Integer, Integer)
-- (di, xi, aXorUiVi)
challengeResponseF coin i False = (coinD coin !! i, coinX coin !! i,
                                  (coinA coin !! i) `xor` (fromIntegral $ coinUCatV coin !! i))
-- (ai, ci, yi)
challengeResponseF coin i True = (coinA coin !! i, coinC coin !! i, coinY coin !! i)

-- This code would run on the payer's device.
-- (recipient is currently not used)
payPeer :: RandomGen g => PayerCoin -> BankMember -> g -> (Either String RecipientCoin, g)
payPeer coin receivingMember gen1 =
  let (challenge, gen2) = initiatePeerPayment (coinBankRsaPublicKey coin) receivingMember (coinFCubeRootProduct coin) gen1
      -- Respond to challenge
      challengeResponse = fmap (uncurry $ challengeResponseF coin) $ zip [0..] (challengeSequence challenge)
      verificationResponse = paymentChallengeResponse challenge challengeResponse
  in (verificationResponse, gen2)


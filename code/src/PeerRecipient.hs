{-# LANGUAGE ScopedTypeVariables #-}

-- This code would run on the recipient's device.
module PeerRecipient
( PeerPaymentChallenge,
  challengeSequence,
  initiatePeerPayment,
  paymentChallengeResponse
) where

import Bank
import BankMember
import Modulo
import RandomUtilities
import SimpleRSA

import System.Random

-- In a proper secure implementation, the payment recipient device would be store this information privately
-- and reference it by a session token or cookie of some kind and prevent the payer from having direct access.
data PeerPaymentChallenge = PeerPaymentChallenge
  { issuingBankRsaPublicKey :: SimpleRsaPublicKey
  --, paymentFCubeRoot :: [Integer]
  , paymentFCubeRootProduct :: Integer
  , challengeSequence :: [Bool]
  } deriving (Show, Eq)

initiatePeerPayment :: RandomGen g => SimpleRsaPublicKey -> BankMember -> Integer -> g -> (PeerPaymentChallenge, g)
initiatePeerPayment issuingBankRsaPublicKey receivingMember fCubeRootProduct gen1 =
  let (challengeSequenceSuffix, gen2) = randomNextN bankKDiv4 gen1
      challengeSequence = (challengeSequencePrefix receivingMember) ++ challengeSequenceSuffix
      challenge = PeerPaymentChallenge
        { issuingBankRsaPublicKey = issuingBankRsaPublicKey
        , paymentFCubeRootProduct = fCubeRootProduct
        , challengeSequence = challengeSequence
        }
  in (challenge, gen2)

paymentChallengeResponse :: PeerPaymentChallenge -> [(Integer, Integer, Integer)] -> Either String RecipientCoin
paymentChallengeResponse challenge response = if verifyChallengeResponse (issuingBankRsaPublicKey challenge)
    (challengeSequence challenge) response (paymentFCubeRootProduct challenge) then
  Right RecipientCoin
  { rcIssuingBankRsaPublicKey = issuingBankRsaPublicKey challenge
  , rcPaymentFCubeRootProduct = paymentFCubeRootProduct challenge
  , rcChallengeSequence = challengeSequence challenge
  , rcChallengeResponse = response
  }
  else Left "payment challenge failed"
  

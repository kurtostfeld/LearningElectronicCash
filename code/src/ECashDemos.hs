{-# LANGUAGE ScopedTypeVariables #-}

module ECashDemos where

import Bank
import BankMember
import PeerPayer

import System.Random


-- Break this down into smaller transactions...

diagnosticWithdraw :: RandomGen g => Bank -> BankMember -> g -> IO (Either String ((Bank, BankMember, PayerCoin), g))
diagnosticWithdraw bank bankMember gen =
  let (withdrawResponse, gen') = withdrawCoin bank bankMember gen
  in case withdrawResponse of
    Right (bank', bankMember', payerCoin) -> do
      putStrLn $ "withdraw success: " ++ (show payerCoin)
      pure $ Right ((bank', bankMember', payerCoin), gen')
    Left withdrawError -> do
      let fullError = "withdraw error: " ++ withdrawError
      putStrLn fullError
      pure $ Left fullError

diagnosticPayment :: RandomGen g => PayerCoin -> BankMember -> g -> IO (Either String (RecipientCoin, g))
diagnosticPayment payerCoin bankMember gen =
  let (paymentResponse, gen') = payPeer payerCoin bankMember gen
  in case paymentResponse of
    Right recipientCoin -> do
      putStrLn $ "payment success: " ++ (show recipientCoin)
      pure $ Right (recipientCoin, gen')
    Left paymentError -> do
      let fullError = "payment error: " ++ paymentError
      putStrLn fullError
      pure $ Left fullError

diagnosticDeposit :: Bank -> BankMember -> RecipientCoin -> IO (Either String Bank)
diagnosticDeposit bank bankMember recipientCoin =
  case depositCoin bank (memberId bankMember) recipientCoin of
    Right bank' -> do
      putStrLn $ "deposit success. bank=" ++ (show bank)
      pure $ Right bank'
    Left depositError -> do
      let fullError = "deposit error: " ++ depositError
      putStrLn fullError
      pure $ Left fullError

-- This function is code messy.
-- This mixes three Monads: IO, Either, and Random and I'm not sure how to code that without a giant Haskell mess.
withdrawAndPayAndDeposit :: RandomGen g => Bank -> BankMember -> BankMember -> g -> IO (g)
withdrawAndPayAndDeposit bank1 fromMember toMember gen1 = do
  let (withdrawResponse, gen2) = withdrawCoin bank1 fromMember gen1
  case withdrawResponse of
    Left errorMessage -> do
      putStrLn $ "Failed to withdraw a coin: " ++ errorMessage
      pure gen2
    Right (bank2, fromMember2, coin) -> do
      let verification = verifyPayerCoin coin
      --putStrLn $ "got bank: " ++ (show bank2)
      putStrLn $ "got fromMember: " ++ (show fromMember2)      
      putStrLn $ "got coin: " ++ (show coin)
      putStrLn $ "coin verifcation: " ++ (show verification)
      let (paymentResponse, gen3) = payPeer coin toMember gen2
      case paymentResponse of
        Left paymentError -> do
          putStrLn $ "payment error: " ++ paymentError
          pure gen3
        Right recipientCoin -> do
          putStrLn $ "payment success: " ++ (show recipientCoin)
          case depositCoin bank2 (memberId toMember) recipientCoin of
            Right bank3 -> do
              putStrLn $ "deposit success. bank3=" ++ (show bank3)
              pure gen3
            Left depositError -> do
              putStrLn $ "deposit error: " ++ depositError
              pure gen3

-- User withdraws coins, pays vendor once, vendor tries to deposit twice.
withdrawAndPayAndDoubleDeposit :: RandomGen g => Bank -> BankMember -> BankMember -> g -> IO (g)
withdrawAndPayAndDoubleDeposit bank1 fromMember toMember gen1 = do
  let (withdrawResponse, gen2) = withdrawCoin bank1 fromMember gen1
  case withdrawResponse of
    Left errorMessage -> do
      putStrLn $ "Failed to withdraw a coin: " ++ errorMessage
      pure gen2
    Right (bank2, fromMember2, coin) -> do
      let verification = verifyPayerCoin coin
      --putStrLn $ "got bank: " ++ (show bank2)
      putStrLn $ "got fromMember: " ++ (show fromMember2)      
      putStrLn $ "got coin: " ++ (show coin)
      putStrLn $ "coin verifcation: " ++ (show verification)
      let (paymentResponse, gen3) = payPeer coin toMember gen2
      case paymentResponse of
        Left paymentError -> do
          putStrLn $ "payment error: " ++ paymentError
          pure gen3
        Right recipientCoin -> do
          putStrLn $ "payment success: " ++ (show recipientCoin)
          case depositCoin bank2 (memberId toMember) recipientCoin of
            Right bank3 -> do
              putStrLn $ "deposit success. bank3=" ++ (show bank3)
              -- Second erroneous deposit of same coin.
              putStrLn "trying second (erroneous deposit)"
              case depositCoin bank3 (memberId toMember) recipientCoin of
                Right bank4 -> do
                  putStrLn $ "second deposit success. This is not right. bank4=" ++ (show bank4)
                  pure gen3
                Left depositError -> do
                  putStrLn $ "deposit error: " ++ depositError
                  pure gen3
            Left depositError -> do
              putStrLn $ "deposit error: " ++ depositError
              pure gen3

-- User withdraws coin, pays two different users, who separately try to deposit.
withdrawAndDoublePayAndDeposit :: RandomGen g => Bank -> BankMember -> BankMember -> BankMember -> g -> IO (g)
withdrawAndDoublePayAndDeposit bank1 fromMember toMember1 toMember2 gen1 = do
  let (withdrawResponse, gen2) = withdrawCoin bank1 fromMember gen1
  case withdrawResponse of
    Left errorMessage -> do
      putStrLn $ "Failed to withdraw a coin: " ++ errorMessage
      pure gen2
    Right (bank2, fromMember2, coin) -> do
      let verification = verifyPayerCoin coin
      --putStrLn $ "got bank: " ++ (show bank2)
      putStrLn $ "got fromMember: " ++ (show fromMember2)      
      putStrLn $ "got coin: " ++ (show coin)
      putStrLn $ "coin verifcation: " ++ (show verification)
      let (paymentResponse, gen3) = payPeer coin toMember1 gen2
      case paymentResponse of
        Left paymentError -> do
          putStrLn $ "payment (1) error: " ++ paymentError
          pure gen3
        Right recipientCoin1 -> do
          putStrLn $ "payment (1) success: " ++ (show recipientCoin1)
          let (paymentResponse, gen4) = payPeer coin toMember2 gen3
          case paymentResponse of
            Left paymentError -> do
              putStrLn $ "payment (2) error: " ++ paymentError
              pure gen3
            Right recipientCoin2 -> do
              putStrLn $ "payment (2) success: " ++ (show recipientCoin2)
              -- Deposit 1
              case depositCoin bank2 (memberId toMember1) recipientCoin1 of
                Left depositError -> do
                  putStrLn $ "deposit (1) error: " ++ depositError
                  pure gen3
                Right bank3 -> do
                  putStrLn $ "deposit (1) success. bank3=" ++ (show bank3)
                  -- Second erroneous deposit of same coin.
                  putStrLn "trying second (erroneous deposit)"
                  case depositCoin bank3 (memberId toMember2) recipientCoin2 of
                    Left depositError -> do
                      putStrLn $ "deposit (2) error: " ++ depositError
                      pure gen3
                    Right bank4 -> do
                      putStrLn $ "second deposit success. This is not right. bank4=" ++ (show bank4)
                      pure gen3

       -- eCashTrialMain
eCashTrialSimpleSuccess :: IO ()
eCashTrialSimpleSuccess = do
  putStrLn "eCashTrialMain"
  gen1 <- getStdGen
  let (bank1, gen2) = createBank "great bank" gen1
  let ((bank2, alice), gen3) = createBankMember bank1 "alice" gen2
  let ((bank3, bob), gen4) = createBankMember bank2 "bob" gen3
  putStrLn $ "bob: " ++ (show bob)
--  withdrawResponse <- diagnosticWithdraw bank3 alice gen4
--  withdrawResponse >>= \(bank4, alice2, payerCoin) gen5 -> do
--    paymentResponse <- diagnosticPayment payerCoin bob gen5
--    pure ()
  gen5 <- withdrawAndPayAndDeposit bank3 alice bob gen4
  pure ()

eCashTrialDoubleDeposit :: IO ()
eCashTrialDoubleDeposit = do
  putStrLn "eCashTrialMain"
  gen1 <- getStdGen
  let (bank1, gen2) = createBank "great bank" gen1
  let ((bank2, alice), gen3) = createBankMember bank1 "alice" gen2
  putStrLn $ "alice: " ++ (show alice)
  let ((bank3, bob), gen4) = createBankMember bank2 "bob" gen3
  putStrLn $ "bob: " ++ (show bob)
  gen5 <- withdrawAndPayAndDoubleDeposit bank3 alice bob gen4
  pure ()

eCashTrialDoublePayment :: IO ()
eCashTrialDoublePayment = do
  putStrLn "eCashTrialMain"
  gen1 <- getStdGen
  let (bank1, gen2) = createBank "great bank" gen1
  let ((bank2, alice), gen3) = createBankMember bank1 "alice" gen2
  putStrLn $ "alice: " ++ (show alice)
  let ((bank3, bob), gen4) = createBankMember bank2 "bob" gen3
  putStrLn $ "bob: " ++ (show bob)
  let ((bank4, charlie), gen5) = createBankMember bank3 "charlie" gen4
  putStrLn $ "charlie: " ++ (show charlie)
  gen6 <- withdrawAndDoublePayAndDeposit bank4 alice bob charlie gen5
  pure ()

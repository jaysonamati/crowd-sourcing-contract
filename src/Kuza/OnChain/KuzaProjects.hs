{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fobject-code #-}

module Kuza.OnChain.KuzaProjects
    (-- * Utility Functions 
      saveVal
    , saveLucidCode
    ) where

import           Control.Lens                hiding (contains, to)
-- import qualified Data.ByteString.Lazy        as LB
-- import qualified Data.ByteString.Short       as SBS
-- import           Codec.Serialise             ( serialise )
import           Prelude                     (Show (..), IO)
import           Prelude                     hiding (Bool, (.), ($))


import           PlutusTx                  ( compile, applyCode, unstableMakeIsData, FromData(fromBuiltinData), makeLift, liftCode, CompiledCode )
import           PlutusTx.Prelude            hiding (Semigroup(..), unless)
import           PlutusTx.Prelude            (Bool, BuiltinData, (.), ($))
import           Plutus.V1.Ledger.Value       ( TokenName, geq, AssetClass)   
import qualified Plutus.V1.Ledger.Value    as Value
-- import qualified Plutus.V1.Ledger.Api      as Ledger
-- import           Plutus.V1.Ledger.Address     (scriptHashAddress)
import           Plutus.V2.Ledger.Contexts as V2
import           Plutus.V2.Ledger.Api         ( mkValidatorScript, Validator, MintingPolicyHash, Address, PubKeyHash, Validator, ScriptContext,
                                                POSIXTime, UnsafeFromData (unsafeFromBuiltinData), OutputDatum(OutputDatumHash, NoOutputDatum, OutputDatum),
                                                Datum(Datum))

import qualified Utilities.Serialise       as Serialise
import           Utilities.PlutusTx           (wrapValidator) 


---------------------------------------------------------------------------------------------------
----------------------------- ON-CHAIN: HELPER FUNCTIONS/TYPES ------------------------------------

{-# INLINABLE parseProjectDatum #-}
parseProjectDatum :: TxOut -> TxInfo -> Maybe PubKeyHash
parseProjectDatum o info = case txOutDatum o of 
    NoOutputDatum -> Nothing
    OutputDatum (Datum d) -> fromBuiltinData d
    OutputDatumHash dh -> do 
                        Datum d <- findDatum dh info
                        fromBuiltinData d

---------------------------------------------------------------------------------------------------
------------------------------------ ON-CHAIN: VALIDATOR ------------------------------------------

data ProjectType = Approved | PoolProject deriving Show
makeLift ''ProjectType
unstableMakeIsData ''ProjectType


data ProjectParams = ProjectParams
    { projectOwners :: [PubKeyHash]
    , projectOwnerTokenMintPolicy :: MintingPolicyHash
    , projectFundingTarget :: Integer
    , projectFundingDeadline :: POSIXTime
    , projectExpenditureTokenMintPolicy :: MintingPolicyHash
    , projectTypeFlag :: ProjectType
    } deriving Show
makeLift ''ProjectParams

newtype ProjectDatum = ProjectDatum PubKeyHash

unstableMakeIsData ''ProjectDatum


data ProjectAction = Vote (PubKeyHash)
                   | Fund (AssetClass, Integer, PubKeyHash)
                   | MoveFunds ()

unstableMakeIsData ''ProjectAction


{-# INLINABLE mkProjectsValidator #-}
mkProjectsValidator :: ProjectParams -> ProjectDatum -> ProjectAction -> ScriptContext -> Bool
mkProjectsValidator project _ red ctx = True
    where 
        info :: TxInfo
        info = scriptContextTxInfo ctx



---------------------------------------------------------------------------------------------------
------------------------------ COMPILE AND SERIALIZE VALIDATOR ------------------------------------

{-# INLINABLE  mkWrappedProjectsValidator #-}
mkWrappedProjectsValidator :: ProjectParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedProjectsValidator = wrapValidator . mkProjectsValidator

typedProjectsValidator :: ProjectParams -> Validator
typedProjectsValidator pp = mkValidatorScript 
    ($$(compile [|| mkWrappedProjectsValidator ||])
    `applyCode`
    liftCode pp)


{-# INLINABLE  mkWrappedProjectValidatorLucid #-}
-- | serializing the projects validator to be used by lucid off-chain
mkWrappedProjectValidatorLucid :: BuiltinData -- projectOwners
                               -> BuiltinData -- projectOwnerTokenMintPolicy
                               -> BuiltinData -- projectFundingTarget
                               -> BuiltinData -- projectFundingDeadline
                               -> BuiltinData -- projectExpenditureTokenMintPolicy
                               -> BuiltinData -- projectTypeFlag
                               -> BuiltinData -> BuiltinData -> BuiltinData -> () --  datum         redeemer        context
mkWrappedProjectValidatorLucid po pwtp pft pfd petmp ptf = wrapValidator $ mkProjectsValidator pp 
    where 
        pp = ProjectParams
            { projectOwners  = unsafeFromBuiltinData po
            , projectOwnerTokenMintPolicy  = unsafeFromBuiltinData pwtp
            , projectFundingTarget = unsafeFromBuiltinData pft
            , projectFundingDeadline = unsafeFromBuiltinData pfd
            , projectExpenditureTokenMintPolicy = unsafeFromBuiltinData petmp
            , projectTypeFlag = unsafeFromBuiltinData ptf
            }

projectsValidatorCodeLucid :: CompiledCode (BuiltinData 
                                        -> BuiltinData 
                                        -> BuiltinData 
                                        -> BuiltinData 
                                        -> BuiltinData 
                                        -> BuiltinData 
                                        -> BuiltinData -> BuiltinData -> BuiltinData -> ())
projectsValidatorCodeLucid = $$( compile [|| mkWrappedProjectValidatorLucid ||])                                        

------------------------------------------------------------------------------------------------------------------
------------------------------------------ HELPER FUNCTIONS ------------------------------------------------------

saveVal :: ProjectParams -> IO () 
saveVal = Serialise.writeValidatorToFile "./assets/kuzaProjects.plutus" . typedProjectsValidator

saveLucidCode :: IO ()
saveLucidCode = Serialise.writeCodeToFile "./assets/lucidProjectsValidator.plutus" projectsValidatorCodeLucid

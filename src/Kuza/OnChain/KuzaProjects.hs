{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}

{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fobject-code #-}

module Kuza.OnChain.KuzaProjects
    (-- * Utility Functions 
      saveVal
    , saveLucidCode
    -- * Data Types
    , ProjectParams
    ) where

import           Control.Lens                hiding (contains, to, from)
-- import qualified Data.ByteString.Lazy        as LB
-- import qualified Data.ByteString.Short       as SBS
-- import           Codec.Serialise             ( serialise )
import           Prelude                     (Show (..), IO)
import           Prelude                     hiding (Bool, (.), ($), (&&), foldl, map, mconcat, any, elem, (>=), (<=), not)


import           PlutusTx                  ( compile, applyCode, unstableMakeIsData, FromData(fromBuiltinData), makeLift, liftCode, CompiledCode )
import           PlutusTx.Prelude            hiding (Semigroup(..), unless)
import           PlutusTx.Prelude            (Bool, BuiltinData, (.), ($), (&&))
import           Plutus.V1.Ledger.Value       ( TokenName, geq, AssetClass, gt, singleton, adaToken, adaSymbol, isZero)
import qualified Plutus.V1.Ledger.Value    as Value
import           Plutus.V1.Ledger.Interval (contains)
-- import qualified Plutus.V1.Ledger.Api      as Ledger
-- import           Plutus.V1.Ledger.Address     (scriptHashAddress)
import           Plutus.V2.Ledger.Contexts as V2
import           Plutus.V2.Ledger.Api         ( mkValidatorScript, to, Validator, PubKeyHash, Validator, Value,
                                                POSIXTime, UnsafeFromData (unsafeFromBuiltinData), OutputDatum(OutputDatumHash, NoOutputDatum, OutputDatum),
                                                Datum(Datum), CurrencySymbol)

import qualified Utilities.Serialise       as Serialise
import           Utilities.PlutusTx           (wrapValidator)


---------------------------------------------------------------------------------------------------
------------------------------------ ON-CHAIN: VALIDATOR ------------------------------------------

data ProjectType = Approved | PoolProject deriving Show
makeLift ''ProjectType
unstableMakeIsData ''ProjectType

data Goal = SDG1 | SDG2 | SDG3 | SDG4 | SDG5 | SDG6 | SDG7 | SDG8 | SDG9 | SDG10 | SDG11 | SDG12 | SDG13 | SDG14 | SDG15 | SDG16 deriving Show 

unstableMakeIsData ''Goal

type Impact = [Goal]

-- | This is the milestone data object
-- It accompanies every expenditure proposal
data Milestone = Milestone 
    { title          :: BuiltinByteString 
    , amountRequired :: Integer
    , timeRange      :: Integer
    , proposedImpact :: [Impact]
    , dependencies   :: [Milestone]
    } deriving Show

unstableMakeIsData ''Milestone

-- | This is the tentative schedule of the project, it is input during project creation
-- There is debate whether it should be in the on-chain code (to avoid bloat data)
-- What would be it's using in moving funds from the project script address??
type Schedule = [Milestone]


data ProjectParams = ProjectParams
    { projectFundingTarget :: Integer
    -- The funding target of the project
    , projectFundingDeadline :: POSIXTime
    -- The funding deadline of the project
    , projectTypeFlag :: ProjectType
    } deriving Show
makeLift ''ProjectParams

data ProjectDatum = ProjectDatum
        { spendingMintingPolicyId     :: CurrencySymbol
        -- This is the policy that mints tokens which allow the project owners to move funds from the project address
        , fundingTokMintingPolicyId   :: CurrencySymbol
        -- This is the token a contributor gets after funding into a project
        , votingTokMintingPolicyId    :: CurrencySymbol
        -- This allows the project funders to vote for expenditure proposals
        , projectOwnerTokMintPolicyId :: CurrencySymbol
        -- This allows for the minting of project ownership tokens
        , projectFunders              :: [PubKeyHash]
        -- There is a privacy argument to be made that this should not be here
        , projectOwners               :: [PubKeyHash]
        -- The project owners, this is input once the project is initialized
        , fundingAmount               :: Integer
        -- This is the amount that is currently funded into the project script address, there is a argument to be made that it should be of type Value.
        } deriving Show

unstableMakeIsData ''ProjectDatum


{- [ProjectDatum] 
We want this to be able to control when funds are moved out of the script address so as to actualize an expenditure, so the information contained 
in the datum should enable that control. For instance the minting policies will check if the pkh spending any funds has the correct token (AssetClass)
before getting to spend. 

Another function of the datum would be to allow for state management of a project; For instance since the project can be in the Funding stage, the 
Implementation stage (which contains the expenditure proposals and the reporting) and finally the 'Actualization' stage (this is more like realizing 
the impact the project has, this will probably have features such as carbon credits etc). We'll start with the implementation of the two stages for the 
hackathon demo. Each of the stages will have information that is relevant to them. 
-}

data ProjectAction = Fund (AssetClass, Integer, PubKeyHash)
                   | MoveFundsProposal (PubKeyHash, Impact, BuiltinByteString )
                   | MoveFunds (AssetClass, Integer, PubKeyHash)

unstableMakeIsData ''ProjectAction


{-# INLINABLE mkProjectsValidator #-}
mkProjectsValidator :: ProjectParams -> ProjectDatum -> ProjectAction -> ScriptContext -> Bool
mkProjectsValidator project _ red ctx =
    case red of
        Fund (_, _, pkh)                ->   traceIfFalse "Amount is not greater than one" checkContributionAmount                   &&
                                             traceIfFalse "Signed By contributor" (signedByContributor pkh)                          &&
                                             traceIfFalse "Datum has not updated" (checkDatumUpdate pkh)                             &&
                                             traceIfFalse "Funding token not minted and transferred" (checkFundingTokenTransfer pkh) &&
                                             traceIfFalse "Deadline has passed" checkDeadlinePassed                                  &&
                                             traceIfFalse "Target has reached" checkFundingTargetReached
        MoveFundsProposal (_, _, _)     -> False
        MoveFunds (_, _ ,_)             -> False


    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        checkContributionAmount :: Bool
        checkContributionAmount = case getContinuingOutputs ctx of
          [] -> traceError "No outputs paying to the script"
          os -> let payingOutVal = mconcat $ map txOutValue os
                in payingOutVal `gt` singleton adaSymbol adaToken 1_000_000

        signedByContributor :: PubKeyHash -> Bool
        signedByContributor contributorPkh = txSignedBy info contributorPkh

        checkDatumUpdate :: PubKeyHash -> Bool
        checkDatumUpdate contPkh = case txInfoOutputs info of
          [] -> traceError "No outputs in the transaction"
          tos -> any hasFunderPkh tos && any hasIncreasedAmount tos
            where
                hasFunderPkh, hasIncreasedAmount:: TxOut -> Bool
                hasFunderPkh o = case parseProjectDatum o info of
                                        Nothing -> False
                                        Just pd -> elem contPkh $ projectFunders pd
                hasIncreasedAmount o = case parseProjectDatum o info of
                                        Nothing -> False
                                        Just pd -> valueProduced info `geq` singleton adaSymbol adaToken (fundingAmount pd)

        checkFundingTokenTransfer :: PubKeyHash -> Bool
        checkFundingTokenTransfer contrPkh = not (isZero $ valuePaidTo info contrPkh)  -- This is a simple check a more comprehensive check would reference the Currency symbol of the funding mintingPolicy `geq` valueProduced info

        checkDeadlinePassed :: Bool
        checkDeadlinePassed = contains (to $ projectFundingDeadline project) $ txInfoValidRange info

        checkFundingTargetReached :: Bool
        checkFundingTargetReached = case txInfoOutputs info of 
                                            [] -> traceError "No transaction outputs found"
                                            tout : _ -> case parseProjectDatum tout info of
                                                                Nothing -> traceError "datum not found"
                                                                Just pd -> fundingAmount pd <= projectFundingTarget project


---------------------------------------------------------------------------------------------------
----------------------------- ON-CHAIN: HELPER FUNCTIONS/TYPES ------------------------------------

{-# INLINABLE parseProjectDatum #-}
parseProjectDatum :: TxOut -> TxInfo -> Maybe ProjectDatum
parseProjectDatum o info = case txOutDatum o of
    NoOutputDatum -> Nothing
    OutputDatum (Datum d) -> fromBuiltinData d
    OutputDatumHash dh -> do
                        Datum d <- findDatum dh info
                        fromBuiltinData d


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
mkWrappedProjectValidatorLucid :: BuiltinData -- projectFundingTarget
                               -> BuiltinData -- projectFundingDeadline
                               -> BuiltinData -- projectTypeFlag
                               -> BuiltinData -> BuiltinData -> BuiltinData -> () --  datum         redeemer        context
mkWrappedProjectValidatorLucid pft pfd ptf = wrapValidator $ mkProjectsValidator pp
    where
        pp = ProjectParams
            {projectFundingTarget = unsafeFromBuiltinData pft
            , projectFundingDeadline = unsafeFromBuiltinData pfd
            , projectTypeFlag = unsafeFromBuiltinData ptf
            }

projectsValidatorCodeLucid :: CompiledCode (BuiltinData
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

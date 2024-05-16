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
    -- * On-Chain Helper Functions
    , parseProjectDatum
    -- * Data Types
    , ProjectParams
    , ProjectDatum(..)
    ) where

-- import           Control.Lens                hiding (contains, to, from)
-- import qualified Data.ByteString.Lazy        as LB
-- import qualified Data.ByteString.Short       as SBS
-- import           Codec.Serialise             ( serialise )
-- import           Prelude                     (Show (..), IO)
import           Prelude                     hiding (Bool, (.), ($), (&&), foldl, map, mconcat, any, elem, (>=), (<=), not)


import           PlutusTx                  ( compile, applyCode, unstableMakeIsData, FromData(fromBuiltinData), makeLift, liftCode, CompiledCode )
import           PlutusTx.Prelude            hiding (Semigroup(..), unless)
-- import           PlutusTx.Prelude            (Bool, BuiltinData, (.), ($), (&&), take, filter)
import           Plutus.V1.Ledger.Value       ( geq, AssetClass (AssetClass), gt, singleton, adaToken, adaSymbol, symbols)
-- import qualified Plutus.V1.Ledger.Value    as Value
import           Plutus.V1.Ledger.Interval (contains)
-- import qualified Plutus.V1.Ledger.Api      as Ledger
-- import           Plutus.V1.Ledger.Address     (scriptHashAddress)
import           Plutus.V2.Ledger.Contexts as V2
import           Plutus.V2.Ledger.Api         ( mkValidatorScript, to, Validator, PubKeyHash, Validator,
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
    , projectCreator  :: PubKeyHash
    -- The project original creator
    , creatorUniqueToken :: AssetClass
    } deriving Show
makeLift ''ProjectParams

data ProjectDatum = ProjectDatum
        { spendingMintingPolicyId      :: CurrencySymbol
        -- This is the policy that mints tokens which allow the project owners to move funds from the project address
        , fundingAckTokMintingPolicyId :: CurrencySymbol
        -- This is the token a contributor gets after funding into a project.
        , proposalTokMintingPolicyId   :: CurrencySymbol
        -- This allows the project funders to vote for expenditure proposals
        , projectOwnerTokMintPolicyId  :: CurrencySymbol
        -- This allows for the minting of project ownership tokens
        , projectFunders               :: [PubKeyHash]
        -- There is a privacy argument to be made that this should not be here
        , projectOwners                :: [PubKeyHash]
        -- The project owners, this is input once the project is initialized
        , fundingAmount                :: Integer
        -- This is the amount that is currently funded into the project script address, there is a argument to be made that it should be of type Value.
        , fundingAckAmount             :: Integer
        -- The amount of funding acknowledgement tokens in the project's script address.
        , currentProposalAmount        :: Integer
        -- The token that represents the current proposal to move funds from the script address
        -- , approvedImplementors         :: [PubKeyHash]
        -- The list of people the project creator (owners) is allowed to move funds to, ideally these should be given by the creator during the project 
        -- deployment and creation stage, they pick a list from known vendors (to become a 'vendor' there is a approval/voting stage).

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

To put the original datum in script address, when the project Creator is minting the initial funding acknowledgement tokens, they deposit those token in 
the script address with the datum included.(Maybe there is another way to do this)
-}

type ProposalTitle = BuiltinByteString

type ReportDocument = BuiltinByteString

data ProjectAction = Fund PubKeyHash
                   | MoveFundsProposal PubKeyHash
                   -- This proposal would ideally include proposal parameters as part of the input...
                   | WithdrawFunds (Integer, PubKeyHash)
                   | SubmitReport ReportDocument

unstableMakeIsData ''ProjectAction


{-# INLINABLE mkProjectsValidator #-}
mkProjectsValidator :: ProjectParams -> ProjectDatum -> ProjectAction -> ScriptContext -> Bool
mkProjectsValidator project dat red ctx =
    case red of
        Fund pkh                        ->   traceIfFalse "Amount is not greater than five" checkContributionAmount                                   &&
                                             traceIfFalse "Signed By contributor" (signedByContributor pkh)                                           &&
                                             traceIfFalse "Datum has not updated" (checkDatumUpdate pkh)                                              &&
                                             traceIfFalse "Funding token not minted and transferred" (checkFundingTokenTransfer pkh)                  &&
                                             traceIfFalse "Deadline has passed" checkDeadlinePassed                                                   &&
                                             traceIfFalse "Target has reached" checkFundingTargetReached
        
        MoveFundsProposal pkh           ->   traceIfFalse "Not signed by project funder" (signedByProjectFunder pkh)                                  &&
                                             traceIfFalse "Signed by project creator" (not (signedByProjectCreator (projectCreator project)))         &&
                                             traceIfFalse "Proposal Token not minted and transferred" checkProposalTokenTransfer
        
        WithdrawFunds (_, _)            ->   traceIfFalse "Expenditure token not in input" (checkTokenInInputs $ spendingMintingPolicyId dat)         &&
                                             traceIfFalse "Proposal token not in input" (checkTokenInInputs $ proposalTokMintingPolicyId dat)         &&
                                             traceIfFalse "Datum has not updated" (checkDatumUpdate (projectCreator project))                         &&
                                             traceIfFalse "Funds exceeding proposal amount" checkExpenditureFundsAmount                               &&
                                             traceIfFalse "Transaction not signed by authorized entity" (signedByProjectCreator (projectCreator project))
        
        SubmitReport _                  ->   False


    where
        --------- UTILITY FUNCTIONS ------------

        info :: TxInfo
        info = scriptContextTxInfo ctx

        datumInOutput :: Maybe ProjectDatum
        datumInOutput = case txInfoOutputs info of
            []   -> Nothing
            outs -> Just =<<
              parseProjectDatum (txOutDatum (PlutusTx.Prelude.head $ PlutusTx.Prelude.take 1 outs)) info

        --------- FUNDING-RELATED FUNCTIONS ------------

        checkContributionAmount :: Bool
        checkContributionAmount = case getContinuingOutputs ctx of
          [] -> traceError "No outputs paying to the script"
          os -> let payingOutVal = mconcat $ PlutusTx.Prelude.filter (\outV -> adaSymbol `elem` symbols outV) $ map txOutValue os
                in payingOutVal `gt` singleton adaSymbol adaToken 5_000_000

        signedByContributor :: PubKeyHash -> Bool
        signedByContributor contributorPkh = txSignedBy info contributorPkh

        checkDatumUpdate :: PubKeyHash -> Bool
        checkDatumUpdate contPkh = case txInfoOutputs info of
          [] -> traceError "No outputs in the transaction"
          tos -> any hasFunderPkh tos && any hasIncreasedAmount tos
            where
                hasFunderPkh, hasIncreasedAmount:: TxOut -> Bool
                hasFunderPkh o = case parseProjectDatum (txOutDatum o) info of
                                        Nothing -> False
                                        Just pd -> elem contPkh $ projectFunders pd
                hasIncreasedAmount o = case parseProjectDatum (txOutDatum o) info of
                                        Nothing -> False
                                        Just pd -> valueProduced info `geq` singleton adaSymbol adaToken (fundingAmount pd)

        checkFundingTokenTransfer :: PubKeyHash -> Bool
        checkFundingTokenTransfer contrPkh = fundingAckTokMintingPolicyId dat `elem` symbols (valuePaidTo info contrPkh)  -- This is a simple check a more comprehensive check would reference the Currency symbol of the funding mintingPolicy `geq` valueProduced info

        checkDeadlinePassed :: Bool
        checkDeadlinePassed = not (to (projectFundingDeadline project) `contains` txInfoValidRange info)

        checkFundingTargetReached :: Bool
        checkFundingTargetReached = case txInfoOutputs info of
                                            [] -> traceError "No transaction outputs found"
                                            tout : _ -> case parseProjectDatum (txOutDatum tout) info of
                                                                Nothing -> traceError "datum not found"
                                                                Just pd -> fundingAmount pd <= projectFundingTarget project


        --------- PROPOSAL-RELATED FUNCTIONS ------------
        checkProposalTokenTransfer :: Bool
        checkProposalTokenTransfer = any (\funder -> proposalTokMintingPolicyId dat `elem` symbols (valuePaidTo info $ projectCreator dat)) $ projectFunders dat

        signedByProjectFunder :: PubKeyHash -> Bool
        signedByProjectFunder projFunder = txSignedBy info projFunder 
        -- && (case datumInOutput of
                                                                                -- Nothing -> False
                                                                                -- Just pd -> projFunder `elem` projectFunders pd) 

        --------- EXPENDITURE-RELATED FUNCTIONS ------------

        signedByProjectCreator :: PubKeyHash -> Bool
        signedByProjectCreator creatorPkh = txSignedBy info creatorPkh

        checkExpenditureFundsAmount :: Bool
        checkExpenditureFundsAmount = case txInfoOutputs info of
                                                [] -> traceError "Transaction doesn't have any outputs"
                                                tos -> let adaValues = PlutusTx.Prelude.filter (\outV -> adaSymbol `elem` symbols outV) $ map txOutValue tos 
                                                       in not (mconcat adaValues `geq` singleton adaSymbol adaToken (currentProposalAmount dat))


        checkTokenInInputs :: CurrencySymbol -> Bool
        checkTokenInInputs cs = case getContinuingOutputs ctx of
                                            [] -> traceError "No inputs in transaction"
                                            txIns -> any (\txIn -> cs `elem` symbols (txOutValue txIn)) txIns


        --------- REPORTING-RELATED FUNCTIONS ------------


---------------------------------------------------------------------------------------------------
----------------------------- ON-CHAIN: HELPER FUNCTIONS/TYPES ------------------------------------

{-# INLINABLE parseProjectDatum #-}
parseProjectDatum :: OutputDatum-> TxInfo -> Maybe ProjectDatum
parseProjectDatum o info = case o of
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
                               -> BuiltinData -- projectCreator
                               -> BuiltinData -- creatorUniqueToken CS
                               -> BuiltinData -- creatorUniqueToken TN
                               -> BuiltinData -> BuiltinData -> BuiltinData -> () --  datum         redeemer        context
mkWrappedProjectValidatorLucid pft pfd pc ctokcs ctoktn = wrapValidator $ mkProjectsValidator pp
    where
        pp = ProjectParams
            {projectFundingTarget = unsafeFromBuiltinData pft
            , projectFundingDeadline = unsafeFromBuiltinData pfd
            , projectCreator = unsafeFromBuiltinData pc
            , creatorUniqueToken = AssetClass (unsafeFromBuiltinData ctokcs, unsafeFromBuiltinData ctoktn)
            }

projectsValidatorCodeLucid :: CompiledCode (BuiltinData
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

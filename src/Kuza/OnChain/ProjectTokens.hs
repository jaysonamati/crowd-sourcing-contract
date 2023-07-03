{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Kuza.OnChain.ProjectTokens
    ( saveProjectOwnerTokenCode
    , saveProjectOwnerTokenPolicy
    , projectOwnerTokenCurrencySymbol
    -- * Expenditure Proposal
    , saveExpenditurePropCode
    -- * Funding Acknowledge
    , saveFundingAcknowledgePolicyCode
    -- * Expenditure Spending
    , saveExpenditureSpendingPolicyCode
    ) where


import qualified Data.ByteString.Char8      as BS8
import           Plutus.V1.Ledger.Value     (flattenValue, assetClassValueOf, AssetClass (AssetClass))
import           Plutus.V2.Ledger.Contexts  (txSignedBy, ownCurrencySymbol, scriptOutputsAt)
import           Plutus.V2.Ledger.Api      (BuiltinData, CurrencySymbol,  OutputDatum(..), Value,
                                            MintingPolicy, PubKeyHash,
                                            ScriptContext (scriptContextTxInfo),
                                            mkMintingPolicyScript, TxOutRef, TokenName, TxInfo (txInfoInputs, txInfoMint), TxInInfo (txInInfoOutRef),
                                            TxOutRef (TxOutRef, txOutRefId, txOutRefIdx),TokenName (unTokenName),
                                             TxId (TxId, getTxId), ValidatorHash, UnsafeFromData (unsafeFromBuiltinData),)
import qualified PlutusTx
import           PlutusTx.Builtins.Internal (BuiltinByteString (BuiltinByteString))
import           PlutusTx.Prelude           (Bool (False, True), Integer, Eq ((==)), any,  Maybe(..),
                                             traceIfFalse, ($), (&&), (.), (>), (<), (>=), traceError, take, (!!), head)
import           Prelude                   (IO, Show (show), String)
import           Text.Printf               (printf)

import qualified Utilities.Serialise       as Serialise
import           Utilities.PlutusTx        (wrapPolicy)
import           Utilities.Conversions     (currencySymbol, bytesToHex)

import           Kuza.OnChain.KuzaProjects


---------------------------------------------------------------------------------------------------
------------------------------------- PROJECT OWNER MINT --------------------------------------------


{-# INLINABLE mkProjectOwnerTokPolicy #-}
mkProjectOwnerTokPolicy :: TxOutRef -> TokenName -> () -> ScriptContext -> Bool
mkProjectOwnerTokPolicy oref tn () ctx = traceIfFalse "UTxO no consumed"    hasUTxO      &&
                                         traceIfFalse "Wrong amount minted" checkMintedAmount
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        hasUTxO :: Bool
        hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

        checkMintedAmount :: Bool
        checkMintedAmount = case flattenValue (txInfoMint info) of
                                [(_, tn'', amt)] -> tn'' == tn && amt == 1
                                _                -> False


{-# INLINABLE mkWrappedProjectOwnerTokPolicy #-}
mkWrappedProjectOwnerTokPolicy :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedProjectOwnerTokPolicy tid ix tn' = wrapPolicy (mkProjectOwnerTokPolicy oref tn)
    where
        oref :: TxOutRef
        oref = TxOutRef
            (TxId $ PlutusTx.unsafeFromBuiltinData tid)
            (PlutusTx.unsafeFromBuiltinData ix)

        tn :: TokenName
        tn = PlutusTx.unsafeFromBuiltinData tn'

-- | This is for use in the lucid off-chain part
projectOwnerTokenCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
projectOwnerTokenCode = $$(PlutusTx.compile [|| mkWrappedProjectOwnerTokPolicy ||])


projectOwnerTokenPolicy :: TxOutRef -> TokenName -> MintingPolicy
projectOwnerTokenPolicy oref tn = mkMintingPolicyScript $
    projectOwnerTokenCode
        `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData $ getTxId $ txOutRefId oref)
        `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData $ txOutRefIdx oref)
        `PlutusTx.applyCode` PlutusTx.liftCode (PlutusTx.toBuiltinData tn)


---------------------------------------------------------------------------------------------------
-------------------------------------FUNDING ACKNOWLEDGE TOKEN --------------------------------------------


-- The Project validator hash is passed in as a parameter
data FundingAckParams = FundingAckParams
        { projectValidatorHash    :: ValidatorHash
        , initialFundingAckSupply :: Integer
        , fundingAckTokenName     :: TokenName
        } deriving Show
PlutusTx.makeLift ''FundingAckParams

data FundingAckMintRedeemer = InitialMint | Mint | Burn
PlutusTx.unstableMakeIsData ''FundingAckMintRedeemer

{-# INLINABLE mkFundingAcknowledgePolicy #-}
mkFundingAcknowledgePolicy :: FundingAckParams -> FundingAckMintRedeemer -> ScriptContext -> Bool
mkFundingAcknowledgePolicy fundAckParams red ctx = case red of
    InitialMint -> traceIfFalse "minted amount must be positive" checkMintPositive && 
                   traceIfFalse "invalid datum at project output" checkDatum       &&
                   traceIfFalse "Invalid mint amount" checkIniitalMintValue
    Mint   -> traceIfFalse "minted amount must be positive" checkMintPositive  
            --   traceIfFalse "invalid datum at project output" checkDatum
    Burn   -> traceIfFalse "Minting instead of burning!" checkBurnNegative

    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        --------- MINTING-RELATED FUNCTIONS ------------

        -- Get amount of funding acknowledge tokens minted in this transaction 
        mintedAmount :: Integer
        mintedAmount = assetClassValueOf (txInfoMint info) (AssetClass (ownCurrencySymbol ctx, fundingAckTokenName fundAckParams))

        -- Check that the amount of stablecoins minted is positive
        checkMintPositive :: Bool
        checkMintPositive = mintedAmount > 0

        -- Check that the amount of ack tokens burned is negative
        checkBurnNegative :: Bool
        checkBurnNegative = mintedAmount < 0

        checkIniitalMintValue :: Bool 
        checkIniitalMintValue = mintedAmount == initialFundingAckSupply fundAckParams

        --------- PROJECT-RELATED FUNCTIONS ------------

        -- Get a project's output datum and value
        projectOutput :: (OutputDatum , Value)
        projectOutput = case scriptOutputsAt (projectValidatorHash fundAckParams) info of
                        outs -> head outs
                        []   -> traceError "expected a project output"


        -- Get the collateral's output datum
        projectOutputDatum :: Maybe ProjectDatum
        projectOutputDatum = parseProjectDatum d info
            where
                (d,_) = projectOutput

        -- Check that the collateral's output datum has the correct values
        checkDatum :: Bool
        checkDatum = case projectOutputDatum of
            Nothing -> False
            Just d  -> --fundingAckTokMintingPolicyId d  == ownCurrencySymbol ctx &&
                       fundingAckAmount d >= mintedAmount &&
                       any (txSignedBy info) (projectFunders d)



{-# INLINABLE  mkWrappedFundingAcknowledgePolicy #-}
mkWrappedFundingAcknowledgePolicy :: FundingAckParams -> BuiltinData -> BuiltinData -> ()
mkWrappedFundingAcknowledgePolicy = wrapPolicy . mkFundingAcknowledgePolicy

fundingAcknowledgePolicy :: FundingAckParams -> MintingPolicy
fundingAcknowledgePolicy fAckParam = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkWrappedFundingAcknowledgePolicy ||])
        `PlutusTx.applyCode`
        PlutusTx.liftCode fAckParam

{-# INLINABLE  mkWrappedFundingAcknowledgePolicyLucid #-}
--                                      project ValHash   initialSupply   tokenName      redeemer       context
mkWrappedFundingAcknowledgePolicyLucid :: BuiltinData ->  BuiltinData ->  BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedFundingAcknowledgePolicyLucid pValHash initSup tn = wrapPolicy $ mkFundingAcknowledgePolicy fAckParam
    where
        fAckParam = FundingAckParams
            { projectValidatorHash    = unsafeFromBuiltinData pValHash
            , initialFundingAckSupply = unsafeFromBuiltinData initSup
            , fundingAckTokenName     = unsafeFromBuiltinData tn
            }

fundingAcknowledgePolicyCodeLucid :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
fundingAcknowledgePolicyCodeLucid = $$( PlutusTx.compile [|| mkWrappedFundingAcknowledgePolicyLucid ||])



------------------------------------------------------------------------------------------------------------
------------------------------------- EXPENDITURE PROPOSAL MINT --------------------------------------------


type Proposal = BuiltinByteString 

data ExpenditureProposalParams = ExpenditureProposalParams
        { projectValidator             :: ValidatorHash
        -- , expenditureProposal          :: Proposal
        , expenditureProposalTokenName :: TokenName  
        } deriving Show
PlutusTx.makeLift ''ExpenditureProposalParams        

-- | Ideally this should be a unique token, there is a case to be made that this should be parametrized by the project params
{-# INLINABLE mkExpenditurePropPolicy #-}
mkExpenditurePropPolicy :: ExpenditureProposalParams -> () -> ScriptContext -> Bool
mkExpenditurePropPolicy expPropParams () ctx = traceIfFalse "Project has no datum"    hasDatum      &&
                                               traceIfFalse "Amount not positive" checkMintPositive
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        -- Get amount of funding acknowledge tokens minted in this transaction 
        mintedAmount :: Integer
        mintedAmount = assetClassValueOf (txInfoMint info) (AssetClass (ownCurrencySymbol ctx, expenditureProposalTokenName expPropParams))

        -- Check that the amount of stablecoins minted is positive
        checkMintPositive :: Bool
        checkMintPositive = mintedAmount > 0

        -- Get a project's output datum and value
        projectOutput :: (OutputDatum , Value)
        projectOutput = case scriptOutputsAt (projectValidator expPropParams) info of
                        []   -> traceError "expected a project output"
                        outs -> head outs


        -- Get the collateral's output datum
        projectOutputDatum :: Maybe ProjectDatum
        projectOutputDatum = parseProjectDatum d info
            where
                (d,_) = projectOutput

        hasDatum :: Bool
        hasDatum = case projectOutputDatum of            
                            Nothing -> False
                            Just pd -> True

        checkMintedAmount :: Bool
        checkMintedAmount = case flattenValue (txInfoMint info) of
                                [(_, tn'', amt)] -> tn'' == expenditureProposalTokenName expPropParams && amt == 1
                                _                -> False


{-# INLINABLE mkWrappedExpenditurePropPolicy #-}
mkWrappedExpenditurePropPolicy :: ExpenditureProposalParams -> BuiltinData -> BuiltinData -> ()
mkWrappedExpenditurePropPolicy = wrapPolicy . mkExpenditurePropPolicy

expenditureProposalTokenPolicy :: ExpenditureProposalParams -> MintingPolicy
expenditureProposalTokenPolicy expPropParams = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkWrappedExpenditurePropPolicy ||])
        `PlutusTx.applyCode`
        PlutusTx.liftCode expPropParams

{-# INLINABLE  mkWrappedExpenditureProposalPolicyLucid #-}
--                                      project ValHash        tokenName      redeemer       context
mkWrappedExpenditureProposalPolicyLucid :: BuiltinData  ->  BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedExpenditureProposalPolicyLucid pValHash tn = wrapPolicy $ mkExpenditurePropPolicy expPropParams
    where
        expPropParams = ExpenditureProposalParams
            { projectValidator             = unsafeFromBuiltinData pValHash
            -- , expenditureProposal          = unsafeFromBuiltinData proposal
            , expenditureProposalTokenName = unsafeFromBuiltinData tn
            }

-- | This is for use in the lucid off-chain part
expenditurePropCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
expenditurePropCode = $$(PlutusTx.compile [|| mkWrappedExpenditureProposalPolicyLucid ||])


------------------------------------------------------------------------------------------------------------
------------------------------------- EXPENDITURE (SPENDING) MINT --------------------------------------------


data ExpenditureSpendingParams = ExpenditureSpendingParams 
        { projectValidatorHashExp     :: ValidatorHash
        , expenditureProposalPolicyId :: CurrencySymbol
        , expenditureAmount           :: Integer
        , expenditureSpendTokenName   :: TokenName
        } deriving Show
PlutusTx.makeLift ''ExpenditureSpendingParams

{-# INLINABLE mkExpenditureSpendPolicy #-}
mkExpenditureSpendPolicy :: ExpenditureSpendingParams -> () -> ScriptContext -> Bool
mkExpenditureSpendPolicy expSpendParams () ctx = traceIfFalse "Project has no datum" hasDatum          &&
                                                 traceIfFalse "Amount not positive"  checkMintPositive 
                                                --  traceIfFalse "Not minted by project funder" checkMintingEntity 
    where 
        info :: TxInfo
        info = scriptContextTxInfo ctx

        -- Get amount of funding acknowledge tokens minted in this transaction 
        mintedAmount :: Integer
        mintedAmount = assetClassValueOf (txInfoMint info) (AssetClass (ownCurrencySymbol ctx, expenditureSpendTokenName expSpendParams))

        -- Check that the amount of tokens minted is positive
        checkMintPositive :: Bool
        checkMintPositive = mintedAmount > 0

        -- Get a project's output datum and value
        projectOutput :: (OutputDatum , Value)
        projectOutput = case scriptOutputsAt (projectValidatorHashExp expSpendParams) info of
                        []   -> traceError "expected a project output"
                        outs -> head outs


        -- Get the collateral's output datum
        projectOutputDatum :: Maybe ProjectDatum
        projectOutputDatum = parseProjectDatum d info
            where
                (d,_) = projectOutput

        hasDatum :: Bool
        hasDatum = case projectOutputDatum of            
                            Nothing -> False
                            Just _  -> True



{-# INLINABLE mkWrappedExpenditureSpendPolicy #-}
mkWrappedExpenditureSpendPolicy :: ExpenditureSpendingParams -> BuiltinData -> BuiltinData -> ()
mkWrappedExpenditureSpendPolicy = wrapPolicy . mkExpenditureSpendPolicy

expenditureSpendingTokenPolicy :: ExpenditureSpendingParams -> MintingPolicy
expenditureSpendingTokenPolicy expSpendParams = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| mkWrappedExpenditureSpendPolicy ||])
        `PlutusTx.applyCode`
        PlutusTx.liftCode expSpendParams

{-# INLINABLE  mkWrappedExpenditureSpendingPolicyLucid #-}
--                                      project ValHash     propPolicy      expAmount       tokenName      redeemer       context
mkWrappedExpenditureSpendingPolicyLucid :: BuiltinData ->  BuiltinData -> BuiltinData ->  BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedExpenditureSpendingPolicyLucid pValHash propPolicy spendAmt tn = wrapPolicy $ mkExpenditureSpendPolicy expSpendParams
    where
        expSpendParams = ExpenditureSpendingParams
            { projectValidatorHashExp      = unsafeFromBuiltinData pValHash
            , expenditureProposalPolicyId  = unsafeFromBuiltinData propPolicy
            , expenditureAmount            = unsafeFromBuiltinData spendAmt
            , expenditureSpendTokenName    = unsafeFromBuiltinData tn
            }



-- | This is for use in the lucid off-chain part
expenditureSpendCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ())
expenditureSpendCode = $$(PlutusTx.compile [|| mkWrappedExpenditureSpendingPolicyLucid ||])

---------------------------------------------------------------------------------------------------
------------------------------------- HELPER FUNCTIONS --------------------------------------------

saveProjectOwnerTokenCode :: IO ()
saveProjectOwnerTokenCode = Serialise.writeCodeToFile "assets/projectOwnerTok.plutus" projectOwnerTokenCode

saveProjectOwnerTokenPolicy :: TxOutRef -> TokenName -> IO ()
saveProjectOwnerTokenPolicy oref tn = Serialise.writePolicyToFile
    (printf "assets/projectOwnerToken-%s#%d-%s.plutus"
        (show $ txOutRefId oref)
        (txOutRefIdx oref) $
        tn') $
    projectOwnerTokenPolicy oref tn
  where
    tn' :: String
    tn' = case unTokenName tn of
        (BuiltinByteString bs) -> BS8.unpack $ bytesToHex bs

projectOwnerTokenCurrencySymbol :: TxOutRef -> TokenName -> CurrencySymbol
projectOwnerTokenCurrencySymbol oref tn = currencySymbol $ projectOwnerTokenPolicy oref tn

------------------------------------------------------------------------------------------------------
-- Helper functions for the funding acknowledge token ------------------

saveFundingAcknowledgePolicyCode :: Prelude.IO ()
saveFundingAcknowledgePolicyCode = Serialise.writeCodeToFile "assets/fundingAcknowledgeToken.plutus" fundingAcknowledgePolicyCodeLucid

---------------------------------------------------------------------------------------------------
-- Helper functions for the expenditure proposal token 

saveExpenditurePropCode :: Prelude.IO ()
saveExpenditurePropCode = Serialise.writeCodeToFile "assets/expenditureProposalToken.plutus" expenditurePropCode

expenditurePropCurrencySymbol :: ExpenditureProposalParams -> CurrencySymbol
expenditurePropCurrencySymbol expPropParams = currencySymbol $ expenditureProposalTokenPolicy expPropParams


------------------------------------------------------------------------------------------------------
-- Helper functions for the expenditure spending token ------------------

saveExpenditureSpendingPolicyCode :: Prelude.IO ()
saveExpenditureSpendingPolicyCode = Serialise.writeCodeToFile "assets/expenditureSpendingToken.plutus" expenditureSpendCode
{
    // ===== Contract Description ===== //
    // Name: Token Lock
    // Description: This contract manages the funds deposited by the benefactor.
    // Version: 1.0.0
    // Author: Luca D'Angelo (ldgaetano@protonmail.com)
    // Reviewer: mgpai22@github.com

    // ===== Box Contents ===== //
    
    // Tokens
    // 1. (TokenLockId, 1L)
    
    // Registers
    // R4: GroupElement                         BenefactorGE
    // R5: (Coll[Byte], Long)                   (KeyId, KeyAmount) // Empty Coll[Byte]() and 0L initially.
    // R6: (Int, Long)                          (Deadline, OracleValue) // Deadline must be greater than the creation-height of the token lock box. OracleValue can be set to any valid Long value, if oracle option is not used this will be determined by OracleNFT.
    // R7: Coll[GroupElement]                   Designates // Empty Coll[GroupElement]() initially.
    // R8: (Coll[Byte], Boolean)                (OracleNFT, isGreaterThan) // The tuple can contain an empty Coll[Coll[Byte]]() as the first argument.
    // R9: (Coll[Coll[Byte]], Long)             (Coll(ContractNameBytes, SigmanautsFeeAddressBytesHash), SigmanautsFee)

    // ===== Transactions ===== //
    
    // 1. Create Token Lock Keys
    // Description: The benefactor will create keys that can be used to redeem funds from the token lock box.
    //              An arbitrary amount of keys can be minted and each will have the same token id. 
    //              Redeeming is an all-or-nothing action.
    // Data Inputs: None
    // Inputs: TokenLock, Benefactor
    // Outputs: TokenLock, Benefactor, SigmanautsFee
    // Context Variables: Action

    // 2. Fund Token Lock
    // Description: The benefactor can fund the token lock box with erg and/or arbitrary tokens.
    // Inputs: TokenLock, Benefactor
    // Data Inputs: None
    // Outputs: TokenLock, SigmanautsFee
    // Context Variables: Action
    
    // 3. Redeem Token Lock: Designate Redeem
    // Description: When the keys are create, special addresses (designates) can be assigned with the keys.
    //              Any designate address that has the key can bypass all restrictions and redeem funds
    //              from the token lock box at any time.
    // DataInputs: None
    // Inputs: TokenLock, Designate
    // Outputs: Designate, SigmanautsFee
    // Context Variables: Action

    // 4. Redeem Token Lock: Deadline Reached
    // Description: A key holder who is not a designate can only redeem the funds from the token lock
    //              if the deadline height is reached.
    // DataInputs: None
    // Inputs: TokenLock, KeyHolder
    // Outputs: KeyHolder, SigmanautsFee
    // Context Variables: Action

    // 5. Redeem Token Lock: Price Greater Than Set Threshold
    // Description: When creating the token lock, it is possible to set the condition for key holder redemption
    //              to depend on some threshold value determined by an oracle datapoint. The contract is
    //              designed to be agnostic to the oracle used, with the only condition being that the datapoint
    //              must be a Long type. The condition that the deadline height is reached must also apply.
    // DataInputs: OracleDatapoint
    // Inputs: TokenLock, KeyHolder
    // Outputs: KeyHolder, SigmanautsFee
    // Context Variables: Action 

    // ===== Compile Time Constants ($) ===== //
    // None
    
    // ===== Context Variables (_) ===== //
    // _action: Int

    // ===== Functions ===== //
    // def isSigmaPropEqualToBoxProp: (SigmaProp, Box) => Boolean
    // def validSigmanautsFee: Box => Boolean
    // def validKeyHolderInput: Box => Boolean
    // def validKeyHolderRecreation: (Box, Box) => Boolean
    // def validTokenLockBurn: Coll[Byte] => Boolean

    def isSigmaPropEqualToBoxProp(propAndBox: (SigmaProp, Box)): Boolean = {

        val prop: SigmaProp = propAndBox._1
        val box: Box = propAndBox._2

        val propBytes: Coll[Byte] = prop.propBytes
        val treeBytes: Coll[Byte] = box.propositionBytes

        (propBytes.slice(1, propBytes.size) == treeBytes.slice(1, treeBytes.size))

    }

    def validSigmanautsFee(feeBox: Box): Boolean = {

        val contractInfo: (Coll[Coll[Byte]], Long)      = SELF.R9[(Coll[Coll[Byte]], Long)].get
        val sigmanautsFeeAddressBytesHash: Coll[Byte]   = contractInfo._1(1)
        val sigmanautsFee: Long                         = contractInfo._2
    
        (feeBox.value >= sigmanautsFee) &&
        (blake2b256(feeBox.propositionBytes) == sigmanautsFeeAddressBytesHash)

    }

    def validKeyHolderInput(holderInput: Box): Boolean = {

        val keyInfo: (Coll[Byte], Long) = SELF.R5[(Coll[Byte], Long)].get
        val keyId: Coll[Byte] = keyInfo._1

        holderInput.tokens.exists({ (token: (Coll[Byte], Long)) => {

            (token._1 == keyId)

        }})

    }

    def validKeyHolderRecreation(holder: (Box, Box)): Boolean = {

        val holderIn: Box = holder._1
        val holderOut: Box = holder._2
        
        val validErg: Boolean = (holderOut.value == SELF.value)

        val validProps: Boolean = (holderOut.propositionBytes == holderIn.propositionBytes)

        // We want to ignore the token lock id NFT.
        val validTokens: Boolean = if (SELF.tokens.size > 1) (holderOut.tokens == SELF.tokens.slice(1, SELF.tokens.size)) else true

        validErg &&
        validProps &&
        validTokens

    }    

    def validTokenLockBurn(tokenLockId: Coll[Byte]): Boolean = {

        OUTPUTS.forall({ (output: Box) => {

            val validTokenLockIdBurn: Boolean = {

                output.tokens.forall({ (token: (Coll[Byte], Long)) => { 
                    
                    (token._1 != tokenLockId) 
                
                }})                  

            }

            val validTokenLockDestruction: Boolean = {

                (output.propositionBytes != SELF.propositionBytes)

            }

            validTokenLockIdBurn &&
            validTokenLockDestruction

        }})

    }

    // ===== Variables ===== //
    val tokenLockId: Coll[Byte]                     = SELF.tokens(0)._1
    
    val benefactorGE: GroupElement                  = SELF.R4[GroupElement].get
    val benefactorSigmaProp: SigmaProp              = proveDlog(benefactorGE)

    val keyInfo: (Coll[Byte], Long)                 = SELF.R5[(Coll[Byte], Long)].get
    val keyId: Coll[Byte]                           = keyInfo._1
    val keyAmount: Long                             = keyInfo._2

    val protocolValues: (Int, Long)                 = SELF.R6[(Int, Long)].get
    val deadline: Int                               = protocolValues._1
    val oracleValue: Long                           = protocolValues._2

    val designates: Coll[GroupElement]              = SELF.R7[Coll[GroupElement]].get

    val oracleInfo: (Coll[Byte], Coll[Byte])        = SELF.R8[(Coll[Byte], Boolean)].get
    val oracleNFT: Coll[Byte]                       = oracleInfo._1
    val isGreaterThan: Boolean                      = oracleInfo._2

    val contractInfo: (Coll[Coll[Byte]], Long)      = SELF.R9[(Coll[Coll[Byte]], Long)].get
    val contractNameBytes: Coll[Byte]               = contractInfo._1(0)
    val sigmanautsFeeAddressBytesHash: Coll[Byte]   = contractInfo._1(1)
    val sigmanautsFee: Long                         = contractInfo._2

    val _action: Int                                = getVar[Int](0).get

    val isKeysCreated: Boolean      = (keyAmount > 0L)
    val isDeadlineReached: Boolean  = (HEIGHT > deadline)
    val isOracleRedeem: Boolean     = (oracleNFT.size > 0) && (CONTEXT.dataInputs.size >= 1)

    if (_action == 1) {

        val validCreateTokenLockKeysTx: Boolean = {

            if (!isKeysCreated) {

                // Outputs
                val tokenLockOut: Box       = OUTPUTS(0)
                val issuanceOut: Box        = OUTPUTS(1)
                val sigmanautsFeeOut: Box   = OUTPUTS(2)

                val validSelfRecreation: Boolean = {

                    allOf(Coll(
                        (tokenLockOut.value == SELF.value),
                        (tokenLockOut.tokens(0)._1 == tokenLockId),
                        (tokenLockOut.R4[GroupElement].get == benefactorGE),
                        (tokenLockOut.R6[(Int, Long)].get == protocolValues),
                        (tokenLockOut.R8[(Coll[Byte], Boolean)].get == oracleInfo),
                        (tokenLockOut.R9[(Coll[Coll[Byte]], Long)].get == contractInfo)
                    ))

                }

                val validKeyMint: Boolean = {

                    val outKeyAmount: Long = issuanceOut.tokens(0)._2

                    // This check ensures only one box contains the minted tokens.
                    val validIssuanceUniqueness: Boolean = {

                        OUTPUTS.filter({ (output: Box) => 
                            output.tokens.exists({ (token: (Coll[Byte], Long)) => 
                                (token._1 == SELF.id) 
                            }) 
                        }).size == 1

                    }

                    val validIssuanceMint: Boolean = {

                        // We follow EIP-4 asset standard using the benefator's box.
                        allOf(Coll(
                            (issuanceOut.tokens(0)._1 == SELF.id),
                            (outKeyAmount > 0L)
                        ))

                    }

                    val validKeyInfoUpdate: Boolean = (tokenLockOut.R5[(Coll[Byte], Long)].get == (SELF.id, outKeyAmount))

                    val propAndBox: (SigmaProp, Box) = (benefactorSigmaProp, issuanceOut)

                    isSigmaPropEqualToBoxProp(propAndBox) &&
                    validIssuanceUniqueness &&
                    validIssuanceMint && 
                    validKeyInfoUpdate

                }

                val validDesignates: Boolean = (tokenLockOut.R7[Coll[GroupElement]].get.size >= designates.size)

                validSelfRecreation &&
                validKeyMint &&
                validDesignates &&
                validSigmanautsFee(sigmanautsFeeOut)

            } else {
                false
            }

        }

        sigmaProp(validCreateTokenLockKeysTx) && benefactorSigmaProp

    } else if (_action == 2) {

        val validFundTokenLockTx: Boolean = {

            // Outputs
            val tokenLockOut: Box = OUTPUTS(0)
            val sigmanautsFeeOut: Box = OUTPUTS(1)

            val validErgDelta: Long = (tokenLockOut.value >= SELF.value)

            val validTokenDelta: Boolean = tokenLockOut.tokens.forall({ (token: (Coll[Byte], Long)) =>
                
                val tokenId: Coll[Byte] = token._1
                
                val inputAmount: Long = SELF.tokens.fold(0L, { (sum: Long, t: (Coll[Byte], Long)) => 
                    if (t._1 == tokenId) sum + t._2 else sum 
                })
                
                val outputAmount: Long = tokenLockOut.tokens.fold(0L, { (sum: Long, t: (Coll[Byte], Long)) => 
                    if (t._1 == tokenId) sum + t._2 else sum 
                })
                
                // This works even if new tokens are added in the output that are not present in the input since inputAmount will therefore be 0.
                // To make this function efficient, it is important that tokens of the same token id are kept in the same location, otherwise
                // these sums will be computed multiple times for the same token id that is in different locations within the token array.

                (outputAmount >= inputAmount) // Should only return false if tokens are removed.
            
            })            

            val validSelfRecreation: Boolean = {

                allOf(Coll(
                    (tokenLockOut.tokens(0)._1 == tokenLockId),
                    (tokenLockOut.R4[GroupElement].get == benefactorGE),
                    (tokenLockOut.R5[(Coll[Byte], Long)].get == keyInfo),
                    (tokenLockOut.R6[(Int, Long)].get == protocolValues),
                    (tokenLockOut.R7[Coll[GroupElement]].get == designates),
                    (tokenLockOut.R8[(Coll[Byte], Boolean)].get == oracleInfo),
                    (tokenLockOut.R9[(Coll[Coll[Byte]], Long)].get == contractInfo)
                ))

            }

            // If ergs are (not removed || added) and tokens are (not removed || added), we want this to pass.
            // If ergs are removed but tokens are (not removed || added), we want this to fail.
            // If ergs are (not removed || added) but tokens are removed, we want this to fail.
            // If ergs are removed and tokens are removed, we want this to fail. 
            val validFund: Boolean = (validErgDelta && validTokenDelta)

            validSelfRecreation &&
            validFund &&
            validSigmanautsFee(sigmanautsFeeOut)         

        }

        sigmaProp(validFundTokenLockTx) && benefactorSigmaProp

    } else if (_action == 3) {

        val validRedeemTokenLockTx: Boolean = {
                
            if (isKeysCreated) {

                // Inputs
                val keyHolderIn: Box = INPUTS(1)

                // Outputs
                val keyHolderOut: Box = OUTPUTS(0)
                val sigmanautsFeeOut: Box = OUTPUTS(1)

                val validKeyHolder: Boolean = {
                    
                    val holder: (Box, Box) = (keyHolderIn, keyHolderOut)

                    validKeyHolderInput(keyHolderIn) &&
                    validKeyHolderRecreation(holder)

                }

                val validDesignateRedeem: Boolean = {

                        designates.exists({ (designate: GroupElement) => 

                            val designateProp: SigmaProp = proveDlog(designate)
                            val propAndBox: (SigmaProp, Box) = (designateProp, keyHolderIn)

                            isSigmaPropEqualToBoxProp(propAndBox)

                        })

                }

               val validOracleRedeem: Boolean = {

                    if (isOracleRedeem) {

                        val oracle: Box = CONTEXT.dataInputs(0)
                        val datapoint: Long = oracle.R4[Long].get // We assume oracle datapoint is only Long type for now.

                        val validOracle: Boolean = (oracle.tokens(0)._1 == oracleNFT)
                        
                        val validThresholdReached: Boolean = {

                            if (isGreaterThan) {
                                
                                (datapoint >= oracleValue)
                            
                            } else {
                            
                                (datapoint <= oracleValue)
                            
                            }
                            
                        }

                        validOracle &&
                        validThresholdReached

                    } else {
                        false
                    }

                }                

                val validRedeemOption: Boolean = {
  
                    (isDeadlineReached || validOracleRedeem) ||
                    validDesignateRedeem

                }

                validRedeemOption &&
                validKeyHolder &&
                validTokenLockBurn(tokenLockId) &&
                validSigmanautsFee(sigmanautsFeeOut)

            } else {
                false
            }
        
        }

        sigmaProp(validRedeemTokenLockTx)

    } else {
        sigmaProp(false)
    }   

}


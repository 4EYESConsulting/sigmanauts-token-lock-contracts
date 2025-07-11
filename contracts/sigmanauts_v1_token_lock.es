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
    // R4: GroupElement         BenefactorGE
    // R5: (Long, Boolean)      (KeyAmount, IsKeysCreated)
    // R6: Coll[Byte]           KeyTokenId
    // R7: Boolean              IsBenefactorRedeem
    // R8: Coll[Byte]           ContractNameBytes
    // R9: (Coll[Byte], Long)   (SigmanautsFeeAddressBytesHash, SigmanautsFee)

    // ===== Transactions ===== //
    
    // 1. Create Token Lock Keys
    // Description: The benefactor will create keys that can be used to redeem funds from the token lock box.
    //              An arbitrary amount of keys can be minted, each will have the same token id. Redeeming is an all-or-nothing action.
    // Inputs: TokenLock, Benefactor
    // Data Inputs: None
    // Outputs: TokenLock, Benefactor, SigmanautsFee
    // Context Variables: Action

    // 2. Fund Token Lock
    // Description: The benefactor can fund the token lock box with erg and/or arbitrary tokens.
    // Inputs: TokenLock, Benefactor
    // Data Inputs: None
    // Outputs: TokenLock, SigmanautsFee
    // Context Variables: Action
    
    // 3. Benefactor Redeem
    // Description: If enabled, the benefactor can redeem all the funds from the box, this terminates the token lock box.
    // Inputs: TokenLock, Benefactor
    // Data Inputs: None
    // Outputs: Benefactor, SigmanautsFee
    // Context Variables: Action
    
    // 4. Beneficiary Redeem
    // Description: Whoever has a token lock key, after the deadline is reached, can redeem all the funds from the box, this terminates the token lock box.
    // Inputs: TokenLock, Beneficiary
    // Data Inputs: None
    // Outputs: Beneficiary, SigmanautsFee
    // Context Variables: Action

    // ===== Compile Time Constants ($) ===== //
    // None
    
    // ===== Context Variables (_) ===== //
    // _action: Int

    // ===== Functions ===== //
    // def validSigmanautsFee: Box => Boolean
    // def isSigmaPropEqualToBoxProp: (SigmaProp, Box) => Boolean
    // def validTokenLockBurn: Coll[Byte] => Boolean

    def validSigmanautsFee(fee: Box): Boolean = {

        val sigmanautsInfo: (Coll[Byte], Long)          = SELF.R9[(Coll[Byte], Long)].get
        val sigmanautsFeeAddressBytesHash: Coll[Byte]   = sigmanautsInfo._1
        val sigmanautsFee: Long                         = sigmanautsInfo._2
        
        allOf(Coll(
            (fee.value >= sigmanautsFee),
            (blake2b256(fee.propositionBytes) == sigmanautsFeeAddressBytesHash)
        ))

    }

    def isSigmaPropEqualToBoxProp(propAndBox: (SigmaProp, Box)): Boolean = {

        val prop: SigmaProp = propAndBox._1
        val box: Box = propAndBox._2

        val propBytes: Coll[Byte] = prop.propBytes
        val treeBytes: Coll[Byte] = box.propositionBytes

        if (treeBytes(0) == 0) {

            (treeBytes == propBytes)

        } else {

            // offset = 1 + <number of VLQ encoded bytes to store propositionBytes.size>
            val offset = if (treeBytes.size > 127) 3 else 2
            (propBytes.slice(1, propBytes.size) == treeBytes.slice(offset, treeBytes.size))

        }

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

            allOf(Coll(
                validTokenLockIdBurn,
                validTokenLockDestruction
            ))

        }})

    }

    // ===== Variables ===== //
    val tokenLockId: Coll[Byte]                     = SELF.tokens(0)._1
    val benefactorGE: GroupElement                  = SELF.R4[GroupElement].get
    val benefactorSigmaProp: SigmaProp              = proveDlog(benefactorGE)
    val keyInfo: (Long, Boolean)                    = SELF.R5[(Long, Boolean)].get
    val keyAmount: Long                             = keyInfo._1 // 0 Initially.
    val isKeysCreated: Boolean                      = keyInfo._2 // False initially, this must happen however.
    val keyTokenInfo: Coll[Byte]                    = SELF.R6[(Coll[Byte], Coll[Byte])].get // Empty tuple of Coll[Byte]() initially.
    val options: Boolean                            = SELF.R7[(Boolean, Boolean)].get // Can be true or false.
    val isKeyAddressRedeem: Boolean                 = options._1 // If this address is provided along with a key, they can redeem at any time.
    val isPriceRedeem: Boolean                      = options._2 // If the price of ERG in USD is above the threshold and the time required time has elapsed, then redemption is possible.
    val contractNameBytes: Coll[Byte]               = SELF.R8[Coll[Byte]].get
    val sigmanautsInfo: (Coll[Byte], Long)          = SELF.R9[(Coll[Byte], Long)].get
    val sigmanautsFeeAddressBytesHash: Coll[Byte]   = sigmanautsInfo._1
    val sigmanautsFee: Long                         = sigmanautsInfo._2
    val _action: Int                                = getVar[Int](0).get

    if (_action == 1) {

        // One time mint of keys.
        val validCreateTokenLockKeysTx: Boolean = {

            // Outputs
            val tokenLockOut: Box       = OUTPUTS(0)
            val issuanceOut: Box        = OUTPUTS(1)
            val sigmanautsFeeOut: Box   = OUTPUTS(2)

            val validSelfRecreation: Boolean = {

                allOf(Coll(
                    (tokenLockOut.value == SELF.value),
                    (tokenLockOut.tokens(0) == (tokenLockId, 1L)),
                    (tokenLockOut.R4[GroupElement].get == benefactorGE),
                    (tokenLockOut.R7[Boolean].get == isBenefactorRedeem),
                    (tokenLockOut.R8[Coll[Byte]].get == contractNameBytes),
                    (tokenLockOut.R9[(Coll[Byte], Long)].get == sigmanautsInfo)
                ))

            }

            val validKeyInfoUpdate: Boolean = {

                allOf(Coll(
                    (tokenLockOut.R5[(Long, Boolean)].get == (keyAmount, true)),
                    (tokenLockOut.R6[Coll[Byte]].get == SELF.id)
                ))

            }

            // Validates token is minted to benefactor.
            val validKeyMint: Boolean = {

                // This check ensures only one box contains the minted tokens.
                val validIssuance: Boolean = {

                    OUTPUTS.filter({ (output: Box) => 
                        output.tokens.exists({ (token: (Coll[Byte], Long)) => 
                            (token._1 == SELF.id) 
                        }) 
                    }).size == 1

                }

                val propAndBox: (SigmaProp, Box) = (benefactorSigmaProp, issuanceOut)

                allOf(Coll(
                    (issuanceOut.tokens(0) == (SELF.id, keyAmount)),
                    isSigmaPropEqualToBoxProp(propAndBox), // We follow EIP-4 asset standard using the benefator's box.
                    validIssuance
                ))

            }

            allOf(Coll(
                validSelfRecreation,
                validKeyInfoUpdate,
                validKeyMint,
                !isKeysCreated,
                validSigmanautsFee(sigmanautsFeeOut)
            ))

        }

        sigmaProp(validCreateTokenLockKeysTx) && benefactorSigmaProp

    } else if (_action == 2) {

        // This action allow only the benefactor to add more ERG or arbitrary tokens to the contract.
        val validFundTokenLockTx: Boolean = {

            // Outputs
            val tokenLockOut: Box = OUTPUTS(0)
            val sigmanautsFeeOut: Box = OUTPUTS(1)

            // Variables
            val ergDelta: Long = (tokenLockOut.value - SELF.value)
            val tokenIn: SELF.tokens.fold(0L, { (acc: Long, curr: (Coll[Byte], Long)) => acc + curr._2 })
            val tokenOut: tokenLockOut.tokens.fold(0L, { (acc: Long, curr: (Coll[Byte], Long)) => acc + curr._2 })
            val tokenDelta: (tokenOut - tokenIn)

            val validSelfRecreation: Boolean = {

                allOf(Coll(
                    (tokenLockOut.tokens(0) == (tokenLockId, 1L)),
                    (tokenLockOut.R4[GroupElement].get == benefactorGE),
                    (tokenLockOut.R5[(Long, Boolean)].get == keyInfo),
                    (tokenLockOut.R6[Coll[Byte]].get == keyTokenId),
                    (tokenLockOut.R7[Boolean].get == isBenefactorRedeem),
                    (tokenLockOut.R8[Coll[Byte]].get == contractNameBytes),
                    (tokenLockOut.R9[(Coll[Byte], Long)].get == sigmanautsInfo)
                ))

            }

            val validFund: Boolean = (ergDelta > 0) || (tokenDelta > 0)

            allOf(Coll(
                validSelfRecreation,
                validFund,
                validSigmanautsFee(sigmanautsFeeOut)
            ))               

        }

        sigmaProp(validFundTokenLockTx) && benefactorSigmaProp

    } else if (_action == 3) {

        // This action allows the benefactor to redeem all the funds from the contract.
        val validBenefactorRedeemTx: Boolean = {

            // Outputs
            val benefactorOut: Box = OUTPUTS(0)
            val sigmanautsFeeOut: Box = OUTPUTS(1)

            val validBenefactorOut: Boolean = {

                val propAndBox: (SigmaProp, Box) = (benefactorSigmaProp, benefactorOut)

                allOf(Coll(
                    (benefactorOut.value == SELF.value - sigmanautsFee),
                    isSigmaPropEqualToBoxProp(propAndBox)
                ))

            }

            allOf(Coll(
                validBenefactorOut,
                validTokenLockBurn(tokenLockId),
                validSigmanautsFee(sigmanautsFeeOut),
                isBenefactorRedeem
            ))
            

        }

        sigmaProp(validBenefactorRedeemTx) && benefactorSigmaProp

    } else if (_action == 4) {

        // This action is the same as action 3 but the beneficiary/redeemer is the key holder
        val validBeneficiaryRedeemTx: Boolean = {

            // Inputs
            val beneficiaryIn: Box = INPUTS.filter({ (input: Box) => 
                input.tokens.exists({ (t: (Coll[Byte], Long)) =>
                    (t._1 == keyTokenId)
                })
            })(0) // There should only be one valid input.

            // Outputs
            val beneficiaryOut: Box = OUTPUTS(0)
            val sigmanautsFeeOut: Box = OUTPUTS(1)

            val validBeneficiaryOut: Boolean = {

                allOf(Coll(
                    (beneficiaryOut.value == SELF.value - sigmanautsFee),
                    (beneficiaryOut.propositionBytes == beneficiaryIn.propositionBytes)
                ))             

            }

            allOf(Coll(
                validBeneficiaryOut,
                validTokenLockBurn(tokenLockId),
                validSigmanautsFee(sigmanautsFeeOut),
                isKeysCreated
            ))           

        }

        sigmaProp(validBeneficiaryRedeemTx)

    } else {
        sigmaProp(false)
    }   
    
}
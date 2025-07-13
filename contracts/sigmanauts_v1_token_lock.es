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
    //              An arbitrary amount of keys can be minted and each will have the same token id. 
    //              Redeeming is an all-or-nothing action.
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
    //              must be a valid ErgoScript Numeric type. The condition that the deadline height is reached must also apply.
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
    // def validTokenLockBurn: Coll[Byte] => Boolean

    // ===== Protocol Options ===== //
    // isDesignateRedeem: Boolean
    // isOracleRedeem: Boolean

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
    
    def validSigmanautsFee(fee: Box): Boolean = {

        val sigmanautsInfo: (Coll[Byte], Long)          = SELF.R9[(Coll[Byte], Long)].get
        val sigmanautsFeeAddressBytesHash: Coll[Byte]   = sigmanautsInfo._1
        val sigmanautsFee: Long                         = sigmanautsInfo._2
        
        allOf(Coll(
            (fee.value >= sigmanautsFee),
            (blake2b256(fee.propositionBytes) == sigmanautsFeeAddressBytesHash)
        ))

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

    val keyInfo: (Coll[Byte], Long)                 = SELF.R5[(Coll[Byte], Long)].get
    val keyId: Coll[Byte]                           = keyInfo._1 // Empty Coll[Byte]() initially.
    val KeyAmount: Coll[Byte]                       = keyInfo._2 // 0L initially.

    val deadline: Numeric                              = SELF.R6[Numeric].get // 0L initially.

    val designateInfo: Coll[Byte]                   = SELF.R7[Coll[Coll[Byte]]].get // Empty Coll[Coll[Byte]]() initially.

    val oracleInfo: (Coll[Byte], Coll[Byte])        = SELF.R8[(Coll[Byte], Coll[Byte])].get
    val oracleNFT: Coll[Byte]                       = oracleInfo._1
    //val oracleSerializedType: Coll[Byte]            = oracleInfo._2(0)
    val oracleSerializedValue: Coll[Byte]           = oracleInfo._2

    val contractInfo: (Coll[Coll[Byte]], Long)      = SELF.R9[(Coll[Coll[Byte]], Long)].get
    val contractNameBytes: Coll[Byte]               = contractInfo._1(0)
    val sigmanautsFeeAddressBytesHash: Coll[Byte]   = contractInfo._1(1)
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
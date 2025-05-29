{
    // ===== Contract Description ===== //
    // Name: Token Lock
    // Description: This contract manages the funds deposited by the benefactor.
    // Version: 1.0.0
    // Author: Luca D'Angelo (ldgaetano@protonmail.com)
    // Reviewed by: mgpai22@github.com

    // ===== Box Contents ===== //
    // Tokens
    // 1. (TokenLockId, 1L)
    // Registers
    // R4: GroupElement     BenefactorGE
    // R5: (Long, Boolean)  KeyInfo
    // R6: Coll[Byte]       KeyTokenId
    // R7: Boolean          IsBenefactorRedeem

    // ===== Transactions ===== //
    // 1. Create Token Lock Keys
    // Inputs: TokenLock, Benefactor
    // Data Inputs: None
    // Outputs: TokenLock, Benefactor, SigmanautsFee
    // Context Variables: Action
    // 2. Fund Token Lock
    // Inputs: TokenLock, Benefactor
    // Data Inputs: None
    // Outputs: TokenLock, SigmanautsFee
    // Context Variables: Action
    // 3. Benefactor Redeem
    // Inputs: TokenLock, Benefactor
    // Data Inputs: None
    // Outputs: Benefactor, SigmanautsFee
    // Context Variables: Action
    // 4. Beneficiary Redeem
    // Inputs: TokenLock, Beneficiary
    // Data Inputs: None
    // Outputs: Beneficiary, SigmanautsFee
    // Context Variables: Action

    // ===== Compile Time Constants ($) ===== //
    // $sigmanautsFeeAddressBytesHash: Coll[Byte]
    // $sigmanautsFee: Long

    // ===== Context Variables (_) ===== //
    // _action: Int

    // ===== Functions ===== //
    // def validSigmanautsFee: Box => Boolean

    def validSigmanautsFee(fee: Box): Boolean = {

        allOf(Coll(
            (fee.value >= $sigmanautsFee),
            (blake2b256(fee.propositionBytes) == $sigmanautsFeeAddressBytesHash)
        ))

    }

    // ===== Global Variables ===== //
    val tokenLockId: Coll[Byte]         = SELF.tokens(0)._1
    val benefactorGE: GroupElement      = SELF.R4[GroupElement].get
    val benefactorSigmaProp: SigmaProp  = proveDlog(benefactorGE)
    val keyInfo: (Long, Boolean)        = SELF.R5[(Long, Boolean)].get
    val keyAmount: Long                 = keyInfo._1
    val isKeysCreated: Boolean          = keyInfo._2
    val keyTokenId: Coll[Byte]          = SELF.R6[Coll[Byte]].get
    val isBenefactorRedeem: Boolean     = SELF.R7[Boolean].get
    val _action: Int                    = getVar[Int](0).get

    if (_action == 1) {

        // one time mint of keys

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
                    (tokenLockOut.R7[Boolean].get == isBenefactorRedeem)
                ))

            }

            val validKeyInfoUpdate: Boolean = {

                allOf(Coll(
                    (tokenLockOut.R5[(Long, Boolean)].get == (keyAmount, true)),
                    (tokenLockOut.R6[Coll[Byte]].get == SELF.id)
                ))

            }

            // validates token is minted to benefactor
            val validKeyMint: Boolean = {

                val validIssuance: Boolean = {

                    OUTPUTS.filter({ (output: Box) => 
                        output.tokens.exists({ (token: (Coll[Byte], Long)) => 
                            (token._1 == SELF.id) 
                        }) 
                    }).size == 1

                }

                allOf(Coll(
                    (issuanceOut.tokens(0) == (SELF.id, keyAmount)),
                    (issuanceOut.propositionBytes == benefactorSigmaProp.propBytes),
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

        // this action allow only the benefactor to add more ERG to the contract

        val validFundTokenLockTx: Boolean = {

            // Outputs
            val tokenLockOut: Box = OUTPUTS(0)
            val sigmanautsFeeOut: Box = OUTPUTS(1)

            // Variables
            val delta: Long = (tokenLockOut.value - SELF.value)

            val validSelfRecreation: Boolean = {

                allOf(Coll(
                    (tokenLockOut.tokens(0) == (tokenLockId, 1L)),
                    (tokenLockOut.R4[GroupElement].get == benefactorGE),
                    (tokenLockOut.R5[(Long, Boolean)].get == keyInfo),
                    (tokenLockOut.R6[Coll[Byte]].get == keyTokenId),
                    (tokenLockOut.R7[Boolean].get == isBenefactorRedeem)
                ))

            }

            val validFund: Boolean = (delta > 0)

            allOf(Coll(
                validSelfRecreation,
                validFund,
                validSigmanautsFee(sigmanautsFeeOut)
            ))               

        }

        sigmaProp(validFundTokenLockTx) && benefactorSigmaProp

    } else if (_action == 3) {

        // this action allows the benefactor to redeem ERG in contract

        val validBenefactorRedeemTx: Boolean = {

            // Outputs
            val benefactorOut: Box = OUTPUTS(0)
            val sigmanautsFeeOut: Box = OUTPUTS(1)

            val validBenefactorOut: Boolean = {

                allOf(Coll(
                    (benefactorOut.value == SELF.value - $sigmanautsFee),
                    (benefactorOut.propositionBytes == benefactorSigmaProp.propBytes)
                ))

            }

            val validTokenLockBurn: Boolean = {

                OUTPUTS.forall({ (output: Box) =>
                    output.tokens.forall({ (t: (Coll[Byte], Long)) =>
                        (t._1 != tokenLockId)
                    })
                })

            }

            allOf(Coll(
                validBenefactorOut,
                validTokenLockBurn,
                validSigmanautsFee(sigmanautsFeeOut),
                isBenefactorRedeem
            ))
            

        }

        sigmaProp(validBenefactorRedeemTx) && benefactorSigmaProp

    } else if (_action == 4) {

        // this action is the same as action 3 but the beneficiary/redeemer is the key holder

        val validBeneficiaryRedeemTx: Boolean = {

            // Inputs
            val beneficiaryIn: Box = INPUTS.filter({ (input: Box) => 
                input.tokens.exists({ (t: (Coll[Byte], Long)) =>
                    (t._1 == keyTokenId)
                })
            })(0)

            // Outputs
            val beneficiaryOut: Box = OUTPUTS(0)
            val sigmanautsFeeOut: Box = OUTPUTS(1)

            val validBeneficiaryOut: Boolean = {

                allOf(Coll(
                    (beneficiaryOut.value == SELF.value - $sigmanautsFee),
                    (beneficiaryOut.propositionBytes == beneficiaryIn.propositionBytes)
                ))             

            }

            val validTokenLockBurn: Boolean = {

                OUTPUTS.forall({ (output: Box) =>
                    output.tokens.forall({ (t: (Coll[Byte], Long)) =>
                        (t._1 != tokenLockId)
                    })
                })

            }

            allOf(Coll(
                validBeneficiaryOut,
                validTokenLockBurn,
                validSigmanautsFee(sigmanautsFeeOut),
                isKeysCreated
            ))           

        }

        sigmaProp(validBeneficiaryRedeemTx)

    } else {
        sigmaProp(false)
    }   
    
}
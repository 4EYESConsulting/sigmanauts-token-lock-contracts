{
    // ===== Contract Description ===== //
    // Name: Token Lock
    // Description: This contract manages the funds deposited by the benefactor.
    // Version: 1.0.0
    // Author: Luca D'Angelo (ldgaetano@protonmail.com)

    // ===== Box Contents ===== //
    // Tokens
    // 1. (TokenLockId, 1L)
    // Registers
    // R4: GroupElement     BenefactorGE
    // R5: (Long, Boolean)  KeyInfo
    // R6: Coll[Byte]       KeyTokenId

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
    val keyAmount: Long                 = keyData.get._1
    val isKeysCreated: Boolean          = keyData.get._2
    val keyTokenId: Coll[Byte]          = SELF.R6[Coll[Byte]].get
    val _action: Int                    = getVar[Int](0).get

    if (_action == 1) {

        val validCreateTokenLockKeysTx: Boolean = {

            // Outputs
            val tokenLockOut: Box       = OUTPUTS(0)
            val issuanceOut: Box        = OUTPUTS(1)
            val sigmanautsFeeOut: Box   = OUTPUTS(2)

            val validSelfRecreation: Boolean = {

                allOf(Coll(
                    (tokenLockOut.value == SELF.value),
                    (tokenLockOut.tokens(0) == (tokenLockId, 1L)),
                    (tokenLockOut.R4[GroupElement].get == benefactorGE)
                ))

            }

            val validKeyInfoUpdate: Boolean = {

                allOf(Coll(
                    (tokenLockOut.R5[(Long, Boolean)].get == (keyAmount, true)),
                    (tokenLockOut.R6[Coll[Byte]].get == SELF.id)
                ))

            }

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

        val validFundTokenLockTx: Boolean = {

            // Outputs
            val tokenLockOut: Box = OUTPUTS(0)
            val sigmanautsFeeOut: Box = OUTPUTS(1)

            // Variables
            val delta: Long = (tokenLockOut.value - SELF.value)

            val validSelfRecreation: Boolean = {

                allOf(Coll(
                    (tokenLockOut.value == SELF.value),
                    (tokenLockOut.tokens(0) == (tokenLockId, 1L)),
                    (tokenLockOut.R4[GroupElement].get == benefactorGE),
                    (tokenLockOut.R5[(Int, Boolean)].get == keyInfo),
                    (tokenLockOut.R6[Coll[Byte]].get == keyTokenId)
                ))

            }

            val validBenefactorIn: Boolean = {

                INPUTS.filter({ (input: Box) => 
                    (input.R6[Coll[Byte]].get == keyTokenId) && 
                    (input.propositionBytes == benefactorSigmaProp.propBytes)
                }).size == 1

            }

            val validBenefactorOut: Boolean = {

                OUTPUTS.filter({ (output: Box) => 
                    (output.R6[Coll[Byte]].get == keyTokenId) && 
                    (output.propositionBytes == benefactorSigmaProp.propBytes)
                }).size == 1                

            }

            val validFund: Boolean = (delta > 0)

            allOf(Coll(
                validSelfRecreation,
                validBenefactorIn,
                validBenefactorOut
                validFund,
                isKeysCreated
            ))               

        }

        sigmaProp(validFundTokenLockTx) && benefactorSigmaProp

    } else {
        sigmaProp(false)
    }   
    
}
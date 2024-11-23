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
    // R5: (Long, Boolean)   KeyInfo

    // ===== Transactions ===== //
    // 1. Create Token Lock Keys
    // Inputs: TokenLock, Benefactor
    // Data Inputs: None
    // Outputs: TokenLock, Benefactor, SigmanautsFee
    // Context Variables: Action, KeyAmount

    // ===== Compile Time Constants ($) ===== //
    // $sigmanautsFeeAddressBytesHash: Coll[Byte]

    // ===== Context Variables (_) ===== //
    // _action: Int

    // ===== Functions ===== //
    // None

    // ===== Global Variables ===== //
    val tokenLockId: Coll[Byte] = SELF.tokens(0)._1
    val benefactorGE: GroupElement = SELF.R4[GroupElement].get
    val benefactorSigmaProp: SigmaProp = proveDlog(benefactorGE)
    val keyAmount: Long = SELF.R5[(Long, Boolean)].get._1
    val isKeysCreated: Boolean = SELF.R5[(Int, Boolean)].get._2
    val _action: Int = getVar[Int](0).get

    if (_action == 1) {

        val validCreateTokenLockKeysTx: Boolean = {

            // Outputs
            val tokenLockOut: Box = OUTPUTS(0)
            val issuanceOut: Box = OUTPUTS(1)

            val validSelfRecreation: Boolean = {

                allOf(Coll(
                    (tokenLockOut.value == SELF.value),
                    (tokenLockOut.tokens(0) == (tokenLockId, 1L)),
                    (tokenLockOut.R4[GroupElement].get == benefactorGE)
                ))

            }

            val validKeyInfoUpdate: Boolean = {

                (tokenLockOut.R5[(Long, Boolean)].get == (keyAmount, true))

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
                    (issuanceOut.propositionBytes == benefactorSigmaProp.propositionBytes),
                    validIssuance
                ))

            }

            allOf(Coll(
                validSelfRecreation,
                validKeyInfoUpdate,
                validKeyMint,
                !isKeysCreated
            ))

        }

        sigmaProp(validCreateTokenLockKeysTx)

    } else {
        sigmaProp(false)
    }   
    
}
#include CryptoApi.h

FUNCTION CalculateSHA1Hash(tcInput)
    LOCAL hProv, hHash
    LOCAL lcUtf8, lcHashBin, lnHashLen
    LOCAL lcBase64, lnBase64Len

    hProv = 0
    hHash = 0

    * 1?? Converte string para UTF-8 (IGUAL ao C#)
    lcUtf8 = STRCONV(tcInput, 9)

    * 2?? Adquire contexto criptográfico
    IF CryptAcquireContext(@hProv, NULL, NULL, PROV_RSA_AES, CRYPT_VERIFYCONTEXT) = 0
        ERROR "Erro ao adquirir contexto criptográfico"
    ENDIF

    * 3?? Cria hash SHA1
    IF CryptCreateHash(hProv, CALG_SHA1, 0, 0, @hHash) = 0
        CryptReleaseContext(hProv, 0)
        ERROR "Erro ao criar hash SHA1"
    ENDIF

    * 4?? Aplica os dados UTF-8 ao hash
    IF CryptHashData(hHash, lcUtf8, LEN(lcUtf8), 0) = 0
        CryptDestroyHash(hHash)
        CryptReleaseContext(hProv, 0)
        ERROR "Erro ao aplicar dados ao hash"
    ENDIF

    * 5?? Descobre tamanho do hash
    lnHashLen = 0
    CryptGetHashParam(hHash, HP_HASHVAL, NULL, @lnHashLen, 0)

    * 6?? Obtém o hash binário
    lcHashBin = REPLICATE(CHR(0), lnHashLen)
    CryptGetHashParam(hHash, HP_HASHVAL, @lcHashBin, @lnHashLen, 0)

    * 7?? Libera recursos
    CryptDestroyHash(hHash)
    CryptReleaseContext(hProv, 0)

    * 8?? Converte binário ? Base64
    lnBase64Len = 0
    CryptBinaryToString(lcHashBin, lnHashLen, CRYPT_STRING_BASE64, NULL, @lnBase64Len)

    lcBase64 = SPACE(lnBase64Len)
    CryptBinaryToString(lcHashBin, lnHashLen, CRYPT_STRING_BASE64, @lcBase64, @lnBase64Len)

    RETURN ALLTRIM(lcBase64)
ENDFUNC
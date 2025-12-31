* ==============================
* CryptoAPI - DeclARAÇÕES
* ==============================

#include CryptoApi.h

DECLARE INTEGER CryptAcquireContext IN advapi32 ;
    INTEGER @phProv, ;
    STRING pszContainer, ;
    STRING pszProvider, ;
    INTEGER dwProvType, ;
    INTEGER dwFlags

DECLARE INTEGER CryptCreateHash IN advapi32 ;
    INTEGER hProv, ;
    INTEGER Algid, ;
    INTEGER hKey, ;
    INTEGER dwFlags, ;
    INTEGER @phHash

DECLARE INTEGER CryptHashData IN advapi32 ;
    INTEGER hHash, ;
    STRING pbData, ;
    INTEGER dwDataLen, ;
    INTEGER dwFlags

DECLARE INTEGER CryptGetHashParam IN advapi32 ;
    INTEGER hHash, ;
    INTEGER dwParam, ;
    STRING @pbData, ;
    INTEGER @pdwDataLen, ;
    INTEGER dwFlags

DECLARE INTEGER CryptDestroyHash IN advapi32 ;
    INTEGER hHash

DECLARE INTEGER CryptReleaseContext IN advapi32 ;
    INTEGER hProv, ;
    INTEGER dwFlags

DECLARE INTEGER CryptBinaryToString IN crypt32 ;
    STRING pbBinary, ;
    INTEGER cbBinary, ;
    INTEGER dwFlags, ;
    STRING @pszString, ;
    INTEGER @pcchString

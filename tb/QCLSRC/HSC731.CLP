     /*%CSTD===========================================================*      */
     /** Application. : SAM ARCAD Sample application                   *      */
     /** Composant. . : HSC731                        Type: CLP        *      */
     /**===============================================================*      */
     /** Sous-système : TRD Trade                                      *      */
     /** Fonction . . : PRD Products                                   *      */
     /** Sous-fonction:                                                *      */
     /**%S=============================================================*      */
     /** Description des fonctionnalités:                              *      */
     /** Descriptif fonctionnel du composant HSC731.ligne 1            *      */
     /** Descriptif fonctionnel du composant HSC731.ligne 2            *      */
     /**%E=============================================================*      */
     /** AUTEUR:                          00:00                        *      */
     /** MODIFS: 01 SGOMES     11/05/2007 11:41  V 1.00.X FM N°07/0037 *      */
     /**           2007/00018 fportier 15/01/2007                      *      */
     /*%ECSTD==========================================================*      */
/********************************************************************/
/* Program     : HSC731                                             */
/* Purpose     : Create OPNQRYF for Voucher Control file HSLVCTLA   */
/* System      : HSV - Voucher Tracking and Control                 */
/* Date        : January 1999                                       */
/********************************************************************/
 
             PGM        PARM(&XPROD &XPROD1 &XPGMID &XUSRID &XJOBNO)
             DCL        VAR(&XPROD) TYPE(*CHAR) LEN(13)
             DCL        VAR(&XPROD1) TYPE(*CHAR) LEN(13)
             DCL        VAR(&XPGMID) TYPE(*CHAR) LEN(10)
             DCL        VAR(&XUSRID) TYPE(*CHAR) LEN(10)
             DCL        VAR(&XJOBNO) TYPE(*CHAR) LEN(10)
             OVRDBF     FILE(HSLVCTLA) SHARE(*YES)
             OPNQRYF    FILE((HSLVCTLA)) QRYSLT('(''BPROD *GE ''" +
                          *CAT ''&XPROD1'' *CAT "'' '')') +
                          KEYFLD((BPROD)) SEQONLY(*NO) +
                          OPTIMIZE(*FIRSTIO 12)
   /*        OPNQRYF    FILE((HSLVCTLA)) QRYSLT('BPROD = %RANGE(' || + */
 /*              "'&£PROD'" |> "'&£PROD1'" || ')') KEYFLD((BPROD)) +   */
 /*                       SEQONLY(*NO) OPTIMIZE(*FIRSTIO 12)           */
             CPYFRMQRYF FROMOPNID(HSLVCTLA) TOFILE(*PRINT)
             CALL       PGM(HSR735) PARM(&XPROD &XPROD1 &XPGMID +
                          &XUSRID &XJOBNO)
             DLTOVR     FILE(HSLVCTLA)
             CLOF       OPNID(HSLVCTLA)
                          ENDPGM

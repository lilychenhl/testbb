      *%CSTD===========================================================*
      ** Application. : STS Arcad Sample application                   *
      ** Composant. . : HSR200ECS                     Type: RPGLE      *
      **===============================================================*
      ** Sous-système : TRD Trade                                      *
      ** Fonction . . :                                                *
      ** Sous-fonction:                                                *
      **%S=============================================================*
      ** Description des fonctionnalités:                              *
      **                                                               *
      **                                                               *
      **                                                               *
      **%E=============================================================*
      ** AUTEUR:                          00:00                        *
      ** MODIFS: 01 MICHEL     28/11/2012 15:28  V 4.00.E FM N°12/0045 *
      **           Adapt HSR200 to BAtch Process (remove screen)       *
      *%ECSTD==========================================================*
      */TITLE Sales Order Entry / Maintenance / Inquiry
      *
      * System        : HSV Control & Tracking
      * Author        : Bob Lee (Intec Systems)
      * Date          : January 1999
      *
      *================================================================
      * Indicator usage:
      *  20 - Order Valid for Despatch
      *  30 - 45 Input fields- DSPATR.
      *  79 - MSGSFL (SFLCLR ETC...).
      *  99 - General CHAIN/SETLL result.
      *
      *================================================================
     H DATEDIT(*YMD)
     FHSLCUSTB  IF   E           K DISK
     F                                     RENAME(HSFCUST:HSFCUSTB)
      * Customer Master by Post Code.
      *
     FHSLCUSTA  IF   E           K DISK
      * Customer Master by Customer Account.
      *
     F*LTCUSTAIF  E           K        DISK
     F*           HSFCUST                           KRENAMEALTCUSTF
      * Customer Master by Customer Account.
      *
     FHSLORDPA  IF   E           K DISK
      * Sales Order Parameters.
      *
     FHSLEXPAA  UF A E           K DISK    USROPN
      * Unbanded Order line file by Product/Series/Serial No.
      *
     FHSLVCTLA  UF   E           K DISK
      * Voucher Control.
      *
     FHSLVXRFB  IF   E           K DISK
      * Agency Product Cross-Reference.
      *
     FHSLORDHA  UF A E           K DISK
      * Sales Order Header.
      *
     FHSLORDDB  UF A E           K DISK
      * Sales Order Detail.
      *
     FHSLODELA  UF A E           K DISK
      * Sales Order Delivery Detail.
      *
     FHSLSHPOA  IF   E           K DISK
      * Ship Only.
      *
     FHSLINVMA  UF   E           K DISK
      * Inventory Master.
      *
     FHSLWHSEA  IF   E           K DISK
      * Warehouse Master.
      *
     FHSLCOMPA  IF   E           K DISK
      * Company Master.
      *
     FHSLPRODA  IF   E           K DISK
      * Product Master.
      *
     FHSLDISCA  IF   E           K DISK
      * Customer Discount File
      *
BL   FHSPINVT   UF A E           K DISK
      * Inventory Transactions.
      *
     F***200    CF   E             WORKSTN INFDS(INFDS)
     F***                                  SFILE(HSS200C:RRNX)
      * Screen file.
      *================================================================
      * Arrays.
      *
      * Days in Month for date validation.
     D DIM             S              2  0 DIM(12) CTDATA PERRCD(12)
      *================================================================
      * Data Structures.
      *
      * Screen Information DS.
     D INFDS           DS
     D  KEY                  369    369
     D  CSRLOC               370    371B 0
      *
     D XXXSDS         SDS           429
     D  XPGMID           *PROC
     D  XJOBNO               244    253
     D  XUSRID               254    263
      *
      * General DDMMCCYY date format.
     D                 DS
     D  DD                     1      2  0 INZ
     D  MM                     3      4  0 INZ
     D  CC                     5      6  0 INZ
     D  YY                     7      8  0 INZ
     D  DMCY                   1      8  0
      *
      * General CCYYMMDD date format.
     D                 DS
     D  CCC                    1      2  0 INZ
     D  YYY                    3      4  0 INZ
     D  MMM                    5      6  0 INZ
     D  DDD                    7      8  0 INZ
     D  CYMD                   1      8  0
      *
      * Input order date format.
     D                 DS
     D  JDD                    1      2  0 INZ
     D  JMM                    3      4  0 INZ
     D  JCC                    5      6  0 INZ
     D  JYY                    7      8  0 INZ
     D  XJORDD                 1      8  0
      *
      * Output order date format.
     D                 DS
     D  WKC                    1      2  0 INZ
     D  WKY                    3      4  0 INZ
     D  WKM                    5      6  0 INZ
     D  WKD                    7      8  0 INZ
     D  WKORDD                 1      8  0
      *
      *----------------------------------------------------------------
      * Constants.
      *
      * Named hexidecimal constants for function keys.
      *
      * Zeros to "SETOF" error indicators.
     D XZEROS          C                   CONST('0000000000000000000')
      *
      *================================================================
      * M A I N L I N E
      *================================================================
      *
      * Program mode
     C                   EXSR      YCLRSC
XXX  C                   MOVEL     YCUST         XJCUST
XXX  C                   MOVEL     YTYPE         XJTYPE            3
XXX  C                   MOVE      *OFF          *IN47
      *
ZZZ  C     XJTYPE        CHAIN     HSLORDPA                           99
ZZZ  C     MSALE         IFEQ      'C'
ZZZ  C                   MOVE      *ON           *IN30
ZZZ  C                   MOVEL     'HSV2027'     P0MSID
ZZZ  C                   EXSR      YSNDMG
     C                   MOVE      *ON           *INLR                          If Type not good out
ZZZ  C                   ENDIF
      *
      *
      * Validate header screen fields.
     C                   EXSR      VALIDH
      *
      * Validate ship only requirements.
     C                   EXSR      SHPONL
      *
      * Clear messages.
     C                   EXSR      YCLRMG
      *
      *-------------------------------------------------------
      * Program mode - Entry
      * Retrieve order number.
     C     *DTAARA       DEFINE    HSAORDERNO    ORDERX            8 0
     C     *LOCK         IN        ORDERX
     C                   ADD       1             ORDERX
     C                   OUT       ORDERX
     C                   Z-ADD     ORDERX        XJSORD
      *
     C     DET           TAG
     C                   MOVEL     *ON           *IN73
     C                   ADD       XKVALU        XKTVAL            9 2
     C                   TIME                    XXTME             6 0
     C                   MOVEA     '011'         *IN(70)
     C                   MOVEL     *OFF          *IN73
     C                   MOVEL     *OFF          *IN86
      *
      * Validate order detail screen fields.
     C                   EXSR      VALIDD
      *
      * Discount retrieval.
     C                   EXSR      DISCNT
      *
      * Check for minimum order value.
      *
XXX   * Credit limit check.... Warning only.
XXX  C     XKTVAL        ADD       XJTYSL        TOT              20 5
XXX  C     TOT           IFGT      XJCLIM
XXX  C                   MOVE      *ON           *IN46
XXX  C                   MOVEL     'HSV2019'     P0MSID
XXX  C                   EXSR      YSNDMG
XXX  C                   ENDIF
      *
      *
      *
      *
      *
      * Update all files, if no errors exist.
     C     XERROR        IFEQ      XNO
     C                   Z-ADD     1             RRNX
     C                   SETOFF                                       98
     C     XSELC         IFNE      *BLANK
     C     XKPROD        ANDNE     *BLANK
     C     *IN98         ANDEQ     *OFF
     C     XSELC         ORNE      *BLANK
     C     XNDESC        ANDNE     *BLANK
     C     *IN98         ANDEQ     *OFF
      *
      *
OWE   * Update Voucher Control if in entry mode....
BL   C     MSALE         IFEQ      'S'
OWE  C                   EXSR      UPPVCT
OWE  C                   ENDIF
      *
BL    * Create/Update order details if in Entry,Maint,or Dispatch mode.
     C                   EXSR      UPORDD
      *
BL    * Update voucher tracking if in Entry or Dispatch mode.
BL    * Stock lines only.
      *
BL    * Allocate inventory if in Entry mode.
BL    * Stock lines only.
BL   C     XSELC         IFEQ      'S'
      * Update inventory allocations.
     C                   EXSR      UPINVA
     C                   ENDIF
      *
BL    * Sell inventory if in Dispatch mode.
BL    * Stock lines only.
     C                   ENDIF                                                   ****?***
      *
BL    * Create/Update order header if in Entry,Maint,or Dispatch mode.
     C                   EXSR      UPORDH
      *
      * Create/update delivery record if in Entry,Maint or Dispat.mode.
     C                   EXSR      UPODEL
      *
BL    * Ship to all delivery points if in Entry mode.
BL   C     MALLP         IFEQ      'Y'
     C                   EXSR      SHPALL
BL   C                   ENDIF
      *
BL    * Print delivery note if in Entry mode, and charges only not set.
BL   C     MINVC         IFNE      'Y'
     C                   EXSR      PRTDEL
BL   C                   ENDIF
      *
BL    * Print Invoice if in Entry mode, and Immediate print required.
BL   C     MINVP         IFEQ      'I'
     C                   EXSR      PRTINV
BL   C                   ENDIF
      *
     C                   ENDIF
      *
     C                   SETON                                        LR
      *
      *****************************************************************
      *----------------------------------------------------------------
      * Ã CLRSC - Clear screen fields.
      *----------------------------------------------------------------
      *
     C     YCLRSC        BEGSR
      *
BL    * Clear header fields.
     C                   CLEAR                   XJCOMP            6
     C                   CLEAR                   XJWHSE            3
     C                   CLEAR                   XJPCDE           10
     C                   CLEAR                   XJCUST            8
     C                   CLEAR                   XJDISP            5 2
     C                   CLEAR                   XJORDD            8 0
     C                   CLEAR                   XJORDR           15
      *
BL    * Clear delivery fields.
BL   C                   CLEAR                   XVNAM1           30
BL   C                   CLEAR                   XVADR1           30
BL   C                   CLEAR                   XVADR2           30
BL   C                   CLEAR                   XVADR3           30
BL   C                   CLEAR                   XVPCDE           10
BL   C                   CLEAR                   XVDIN1           30
BL   C                   CLEAR                   XVDIN2           30
BL   C                   CLEAR                   XVDIN3           30
     C                   ENDSR
      *
      *----------------------------------------------------------------
      * SHPONL - Validate ship only requirements.
      *----------------------------------------------------------------
     C     SHPONL        BEGSR
      *
      * Check if a ship only record exists for this customer
     C     XJCUST        CHAIN     HSLSHPOA                           99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     'Y'           SHONLY            1
BL   C                   ELSE
BL   C                   MOVEL     ' '           SHONLY
     C                   ENDIF
     C                   ENDSR
      *
      *----------------------------------------------------------------
      * AGXREF - Check for agency cross-references.
      *----------------------------------------------------------------
     C     AGXREF        BEGSR
     C     KEYXRF        CHAIN     HSLVXRFB                           99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     HPROD         KPROD
     C                   MOVEL     HPROD         XKPROD
     C                   ENDIF
     C                   ENDSR
      *
      *----------------------------------------------------------------
      * VALIDD - Validate order detail
      *----------------------------------------------------------------
     C     VALIDD        BEGSR
      *
      * Order Detail validation:
      *
OWE   *
     C                   MOVEL     'N'           XSELC             1
     C                   MOVEL     YPROD         XKPROD
     C                   MOVEL     YTEXT         XNDESC
     C                   Z-ADD     YPRIC         XKPRIC
     C                   Z-ADD     YQTYN         XKQTYN
     C                   Z-ADD     0             XKVALU
     C***                MOVEL     *BLANK        XKSERC
     C***                Z-ADD     0             XKSERF
     C***                Z-ADD     0             XKSERT
      *
     C
     C     XSELC         IFNE      *BLANK                                       ....If3
     C     XKPROD        ORNE      *BLANK                                       ....If3
     C     XNDESC        ORNE      *BLANK                                       ....If3
     C     XKPRIC        ORNE      *ZEROS                                       ....If3
     C     XKQTYN        ORNE      *ZEROS                                       ....If3
     C     XKVALU        ORNE      *ZEROS                                       ....If3
     C     XKVATA        ORNE      *ZEROS                                       ....If3
     C***  XKSERC        ORNE      *BLANKS                                      ....If3
     C***  XKSERF        ORNE      *ZEROS                                       ....If3
     C***  XKSERT        ORNE      *ZEROS                                       ....If3
      *
      * Validate Selection code.
     C     XSELC         IFNE      'S'
     C     XSELC         ANDNE     'N'
     C     XSELC         ANDNE     'T'
      *
      * Validate Selection code / Product code / Text combination
     C     XSELC         OREQ      *BLANK
     C     XKPROD        ANDNE     *BLANKS
     C     XSELC         OREQ      *BLANK
     C     XNDESC        ANDNE     *BLANKS
     C     XSELC         OREQ      *BLANK
     C     XKPRIC        ANDNE     0
     C     XSELC         OREQ      *BLANK
     C     XKQTYN        ANDNE     0
     C     XSELC         OREQ      'T'
     C     XKPROD        ANDNE     *BLANKS
     C     XSELC         OREQ      'T'
     C     XKPRIC        ANDNE     0
     C     XSELC         OREQ      'T'
     C     XKQTYN        ANDNE     0
     C*    XSELC         OREQ      'T'
     C*    XKSERC        ANDNE     *BLANKS
     C*    XSELC         OREQ      'T'
     C*    XKSERF        ANDNE     0
     C*    XSELC         OREQ      'T'
     C*    XKSERT        ANDNE     0
     C     XSELC         OREQ      'S'
     C     XKQTYN        ANDEQ     0
     C     XSELC         OREQ      'N'
     C     XKQTYN        ANDEQ     0
     C     XSELC         OREQ      'N'
     C     XKPRIC        ANDEQ     0
     C                   MOVE      *ON           *IN36
     C                   MOVE      XYES          XERROR
     C                   MOVEL     'HSV2006'     P0MSID
     C                   EXSR      YSNDMG
     C                   ENDIF
      *
      * Validate Selection code for Invoice Charge Only order type.
     C     XSELC         IFEQ      'S'
     C     MINVC         ANDEQ     'Y'
     C                   MOVE      *ON           *IN36
     C                   MOVE      XYES          XERROR
     C                   MOVEL     'HSV2014'     P0MSID
     C                   EXSR      YSNDMG
     C                   ENDIF
      *
      * Validate Product.
     C     XSELC         IFEQ      'S'
     C     XKPROD        IFNE      *BLANKS
     C     XKPROD        CHAIN     HSLPRODA                           99
     C     *IN99         IFEQ      *ON
     C                   MOVE      *ON           *IN37
     C                   MOVE      XYES          XERROR
     C                   MOVEL     'HSV2007'     P0MSID
     C                   EXSR      YSNDMG
     C                   ELSE
     C                   MOVEL     NDESC         XNDESC
     C                   Z-ADD     NPRIC         XKPRIC
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
      * Reverse price for Credit
     C     MSALE         IFEQ      'C'
     C                   Z-SUB     XKPRIC        XKPRIC
     C                   ENDIF
      *
      * Extended value
     C     XSELC         IFEQ      'S'
     C     XSELC         OREQ      'N'
     C     XKPRIC        MULT      XKQTYN        XKVALU
     C                   ENDIF
      *
OWE   * Save Price, Value and Description field contents....
OWE  C                   MOVE(P)   XKPRIC        TKPRIC            9 2
OWE  C                   MOVE(P)   XKVALU        TKVALU
OWE  C                   MOVE(P)   XNDESC        TNDESC           19
      *
      * Check voucher control if not yet allocated
      *
OWE  C                   MOVE      TKPRIC        XKPRIC
OWE  C                   MOVE      TKVALU        XKVALU
OWE  C                   MOVE      TNDESC        XNDESC
      *
     C                   ENDIF                                                  ....EndIf3
      *
      * If no errors found...
     C                   ENDSR
      *
      *----------------------------------------------------------------
      * DISCNT - Discount retrieval.
      *----------------------------------------------------------------
      *
     C     DISCNT        BEGSR
      *
      * If customer discount is not entered either
      * caluclate from HSPDISC file keyed on Order Value or default
      * according to Customer Type.
      *
     C     XJDISP        IFEQ      0
      *
      * Setup key list for HSPDISC
      *
     C                   Z-ADD     XKTVAL        PFAMT
      *
      * SETLL on HSLDISCA
      *
     C     DSCKEY        SETLL     HSLDISCA                               99
      *
      * If record not found READP
      *
     C     *IN99         IFEQ      *OFF
     C                   READP     HSLDISCA                               99
      *
      * If not BOF
      *
     C     *IN99         IFEQ      *OFF
BL   C     PCTYP         ANDEQ     ECTYP
     C                   Z-ADD     PDISC         KDISP
      *
      * Else calculate discount according to Customer Type
      *
     C                   ELSE
      *
     C                   SELECT
      *
      * If Customer Type is '001' - Z-ADD 10 to discount
      *
     C     ECTYP         WHENEQ    '001'
     C                   Z-ADD     10            KDISP
      *
      * If Customer Type is '002' - Z-ADD 20 to discount
      *
     C     ECTYP         WHENEQ    '002'
     C                   Z-ADD     20            KDISP
      *
     C                   ENDSL
      *
     C                   ENDIF
      *
      * Record found on SETLL
      *
     C                   ELSE
      *
     C                   Z-ADD     PDISC         KDISP
      *
     C                   ENDIF
      *
      * Â£JDISP not equal to 0
      *
     C                   ELSE
     C                   Z-ADD     XJDISP        KDISP
      *
     C                   ENDIF
      *
     C                   ENDSR
      *
      *
      *----------------------------------------------------------------
      * Ship to all delivery points.
      *----------------------------------------------------------------
     C     SHPALL        BEGSR
     C*
     C     XJCUST        SETLL     HSLCUSTA
     C     XJCUST        READE     HSLCUSTA                               99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C* If ECGRP not equal to blanks
     C*
     C     ECGRP         IFNE      *BLANKS
     C*
     C* Write Order Header Record
     C*
     C                   EXSR      UPORDH
     C*
     C* Set up Delivery Name and Address
     C*
     C                   MOVEL     ENAM1         XVNAM1
     C                   MOVEL     EADR1         XVADR1
     C                   MOVEL     EADR2         XVADR2
     C                   MOVEL     EADR3         XVADR3
     C                   MOVEL     EPCDE         XVPCDE
     C*
     C* Write Order Delivery Record
     C*
     C                   EXSR      UPODEL
     C*
     C* Write Order Detail Record
     C*
     C                   EXSR      UPORDD
     C*
     C                   ENDIF
     C*
     C* Increment order number
     C*
     C     *LOCK         IN        ORDERX
     C                   ADD       1             ORDERX
     C                   OUT       ORDERX
     C                   Z-ADD     ORDERX        XJSORD
     C*
     C* READE on HSLCUSTA with Customer Number
     C*
     C     XJCUST        READE     HSLCUSTA                               99
     C*
     C                   ENDDO
     C*
     C* EOF on HSLCUSTA
     C*
     C                   ENDSR
      *
      *
      *----------------------------------------------------------------
      * Delete contents of HSLEXPAA
      *----------------------------------------------------------------
      *
     C     YDELT         BEGSR
     C                   CALL      'HSC200A'
     C                   ENDSR
      *
      *----------------------------------------------------------------
      * Update inventory allocations.
      *----------------------------------------------------------------
     C     UPINVA        BEGSR
      *
      * Update stock quantities.
     C     IVMKEY        CHAIN     HSLINVMA                           99
     C     *IN99         IFEQ      *OFF
      *
      * Increase allocated quantity and reduce available quantity.
     C                   ADD       XKQTYN        DQTYR
     C                   SUB       XKQTYN        DQTYF
     C                   UPDATE    HSFINVM
     C                   ENDIF
     C                   ENDSR
      *
      *----------------------------------------------------------------
      * Print delivery note.
      *----------------------------------------------------------------
     C     PRTDEL        BEGSR
BL   C                   MOVEL     XJSORD        ORDPRM
BL   C                   CALL      'HSR230'
BL   C                   PARM                    ORDPRM
     C                   ENDSR
      *
      *----------------------------------------------------------------
      * Print invoice.
      *----------------------------------------------------------------
     C     PRTINV        BEGSR
BL   C                   MOVEL     XJSORD        ORDPRM
BL   C                   CALL      'HSR220'
BL   C                   PARM                    ORDPRM
     C                   ENDSR
      *
      *----------------------------------------------------------------
      * Create order details.
      *----------------------------------------------------------------
     C     UPORDD        BEGSR
      *
      * Order line already exists
     C                   MOVEL     ' '           UPDAT
     C     XOLIN         IFGT      0
     C     KEYOLN        CHAIN     HSLORDDB                           98
     C                   Z-ADD     XOLIN         KLINE
     C                   ENDIF
      *
      * Write fields for order detail record.
     C                   MOVEL     XSELC         KLTYP
     C                   MOVEL     XJCOMP        KCOMP
     C                   MOVEL     XJTYPE        KTYPE
     C                   Z-ADD     XJSORD        KSORD
     C                   Z-ADD     RRNX          KLINE
     C                   MOVEL     XJCUST        KCUST
     C                   MOVEL     XKPROD        KPROD
     C*                  MOVEL     XKSERC        KSERC
     C*                  Z-ADD     XKSERF        KSERNF
     C                   Z-ADD     0             KELEMF
     C*                  Z-ADD     XKSERT        KSERNT
     C                   Z-ADD     0             KELEMT
     C                   Z-ADD     XKQTYN        KQTYN
     C                   Z-ADD     NCOST         KCOST
     C                   Z-ADD     XKPRIC        KPRIC
     C                   Z-ADD     XKVALU        KVALU
      *
BL    * Obtain VAT% from Order Type record.
     C                   Z-ADD     CYMD          KCRTD
     C                   Z-ADD     XXTME         KCRTT
     C                   MOVEL     XNDESC        KDESC
     C                   MOVEL     XJOBNO        KCRTS
     C                   MOVEL     XUSRID        KCRTU
     C                   MOVEL     XPGMID        KCRTP
     C                   MOVEL     *BLANK        KDELT
      *
      * Update order detail record.
     C     XOLIN         IFGT      0
     C     *IN98         IFEQ      *OFF
     C                   MOVEL     'Y'           UPDAT
     C                   UPDATE    HSFORDD
     C                   ENDIF
     C                   ELSE
      *
      * Write order detail record.
     C                   WRITE     HSFORDD
     C                   ENDIF
     C                   ENDSR
      *
      *----------------------------------------------------------------
      * Create order header.
      *----------------------------------------------------------------
     C     UPORDH        BEGSR
      *
      *
      * Write fields for order header record.
     C                   MOVEL     XJCOMP        JCOMP
     C                   MOVEL     XJCUST        JCUST
     C                   MOVEL     *BLANKS       JCUSB
     C                   MOVEL     XJORDR        JORDR
     C                   MOVEL     XJTYPE        JTYPE
     C                   Z-ADD     XJSORD        JSORD
     C                   Z-ADD     WKORDD        JORDD
     C                   MOVEL     *BLANKS       JINVN
     C                   Z-ADD     0             JINVD
     C                   MOVEL     *BLANKS       JDELN
     C                   Z-ADD     0             JDELD
     C                   Z-ADD     CYMD          JCRTD
     C                   Z-ADD     XXTME         JCRTT
     C                   MOVEL     XJOBNO        JCRTS
     C                   MOVEL     XUSRID        JCRTU
     C                   MOVEL     XPGMID        JCRTP
     C                   MOVEL     *BLANK        JDELT
      *
      *
      * Write order header record.
BL   C                   MOVEL     'A'           JSTAT
     C                   WRITE     HSFORDH
      *
     C                   ENDSR
      *
      *----------------------------------------------------------------
      * Create order delivery details.
     C     UPODEL        BEGSR
      *
      * Write fields for order delivery detail record.
     C                   MOVEL     XJCOMP        VCOMP
     C                   MOVEL     XJTYPE        VTYPE
     C                   Z-ADD     XJSORD        VSORD
     C                   MOVEL     XVNAM1        VNAM1
     C                   MOVEL     XVADR1        VADR1
     C                   MOVEL     XVADR2        VADR2
     C                   MOVEL     XVADR3        VADR3
     C                   MOVEL     XVPCDE        VPCDE
     C                   MOVEL     XVDIN1        VDIN1
     C                   MOVEL     XVDIN2        VDIN2
     C                   MOVEL     XVDIN3        VDIN3
     C                   MOVEL     *BLANKS       VDELT
      *
      * Write order delivery details record.
     C                   WRITE     HSFODEL
      *
     C                   ENDSR
      *
      *----------------------------------------------------------------
      * VALIDH - Validate header screen.
      *----------------------------------------------------------------
      *
     C     VALIDH        BEGSR
      *
      * Screen Header validation:
      *
      * Reset work values.
     C                   RESET                   XERROR
      *
      * Reset error indicators.
     C                   MOVEA     XZEROS        *IN(30)
      *
      * Validate that Order Type is a valid Type.
     C     YTYPE         CHAIN     HSLORDPA                           99
     C     *IN99         IFEQ      *ON
     C                   MOVE      *ON           *IN30
     C                   MOVE      XYES          XERROR
     C                   MOVEL     'HSV2001'     P0MSID
     C                   EXSR      YSNDMG
     C                   ENDIF
      *
      * Validate that Company is valid.
     C     XJCOMP        IFNE      *BLANKS
     C     XJCOMP        CHAIN     HSLCOMPA                           99
     C     *IN99         IFEQ      *ON
     C                   MOVE      *ON           *IN31
     C                   MOVE      XYES          XERROR
     C                   MOVEL     'HSV2002'     P0MSID
     C                   EXSR      YSNDMG
     C                   ENDIF
     C                   ENDIF
      *
      * Validate that Warehouse is valid.
     C     XJWHSE        IFNE      *BLANKS
     C     XJWHSE        CHAIN     HSLWHSEA                           99
     C     *IN99         IFEQ      *ON
     C                   MOVE      *ON           *IN32
     C                   MOVE      XYES          XERROR
     C                   MOVEL     'HSV0006'     P0MSID
     C                   EXSR      YSNDMG
     C                   ENDIF
     C                   ENDIF
      *
      * Validate Post Code
XXX  C     XJCUST        IFEQ      *BLANKS
     C     XJPCDE        IFNE      *BLANKS
     C     XJPCDE        CHAIN     HSLCUSTB                           99
     C     *IN99         IFEQ      *ON
     C                   MOVE      *ON           *IN33
     C                   MOVE      XYES          XERROR
     C                   MOVEL     'HSV2003'     P0MSID
     C                   EXSR      YSNDMG
     C                   ENDIF
      *
      * If Customer is found
     C     *IN99         IFEQ      *OFF
      * ...load Customer record into screen fields.
XXXM C                   MOVEL     ENAM1         XJNAM1           30            Name 1
XXXM C                   MOVEL     ENAM2         XJNAM2           30            Name 2
XXXM C                   MOVEL     EADR1         XJADR1           30            Address 1
XXXM C                   MOVEL     EADR2         XJADR2           30            Address 2
XXXM C                   MOVE      ECUST         XJCUST                         Cust No.
XXX  C                   MOVE      ECLIM         XJCLIM           15 0          Credit Limit
XXX  C                   MOVE      ETYSL         XJTYSL           20 5          Last Year Sales
XXX  C                   ENDIF
      *
     C                   ENDIF
     C                   ENDIF
      *
      * Validate Customer Number
XXXD C*          Â£JCUST    IFNE *BLANKS
     C     XJCUST        CHAIN     HSLCUSTA                           99
     C     *IN99         IFEQ      *ON
     C                   MOVE      *ON           *IN34
     C                   MOVE      XYES          XERROR
     C                   MOVEL     'HSV2004'     P0MSID
     C                   EXSR      YSNDMG
     C                   ENDIF
      *
      * If Customer is found
     C     *IN99         IFEQ      *OFF
      * ...load Customer record into screen fields.
XXXM C                   MOVEL     ENAM1         XJNAM1                         Name 1
XXXM C                   MOVEL     ENAM2         XJNAM2                         Name 2
XXXM C                   MOVEL     EADR1         XJADR1                         Address 1
XXXM C                   MOVEL     EADR2         XJADR2                         Address 2
XXXM C                   MOVE      EPCDE         XJPCDE                         Post Code
XXX  C                   MOVE      ECLIM         XJCLIM           15 0          Credit Limit
XXX  C                   MOVE      ETYSL         XJTYSL           20 5          Last Year Sales
XXX   *
XXX   * Check customer's credit rating allows order entry......
XXX  C     ECRAT         IFEQ      '01 '
XXX  C                   MOVE      *ON           *IN34
XXX  C                   MOVE      XYES          XERROR
XXX  C                   MOVEL     'HSV2018'     P0MSID
XXX  C                   EXSR      YSNDMG
XXX  C                   ENDIF
      *
     C                   ENDIF
XXXD C*                    ENDIF
      *
      * Maximum discount is 40%
     C     XJDISP        IFGT      40
     C                   MOVE      *ON           *IN36
     C                   MOVE      XYES          XERROR
     C                   MOVEL     'HSV0142'     P0MSID
     C                   EXSR      YSNDMG
     C                   ENDIF
      *
      * Check order date
     C     XJORDD        IFEQ      0
     C                   EXSR      YDATEC
     C                   ELSE
      *
      * a) Check for leap year and adjust number of days in February
      *    in Days in Month array.
     C     JYY           DIV       4             REM               4 0
     C     REM           IFEQ      0
     C                   MOVEL     'Y'           LEAP              1
     C                   ELSE
     C                   MOVEL     ' '           LEAP
     C                   ENDIF
     C                   SELECT
     C     LEAP          WHENEQ    ' '
     C                   MOVEA     28            DIM(2)
     C     LEAP          WHENEQ    'Y'
     C                   MOVEA     29            DIM(2)
     C                   ENDSL
      * Day
     C     JDD           IFGT      31
     C     JDD           ORLT      01
      * Month
     C     JMM           ORGT      12
     C     JMM           ORLT      01
      *
      * If date error found...
     C                   MOVE      *ON           *IN35
     C                   MOVE      XYES          XERROR
     C                   MOVEL     'HSV2005'     P0MSID
     C                   EXSR      YSNDMG
     C                   ENDIF
      *
      * Validate Day within month.
     C                   Z-ADD     JMM           M                 2 0
     C     JDD           IFLT      1
     C     JMM           ORGE      1
     C     JMM           ANDLE     12
     C     JDD           ANDGT     DIM(M)
      *
      * If date error found...
     C                   MOVE      *ON           *IN35
     C                   MOVE      XYES          XERROR
     C                   MOVEL     'HSV2005'     P0MSID
     C                   EXSR      YSNDMG
     C                   ENDIF
     C                   ENDIF
      *
      *
     C                   ENDSR
      *
      *----------------------------------------------------------------
      * Ã DATEC - Date Check Routine
      *----------------------------------------------------------------
      *
     C     YDATEC        BEGSR
      *
      * Convert six-digit system date to eight digits, with century.
     C                   MOVEL     UDAY          DD
     C                   MOVEL     UMONTH        MM
     C                   MOVEL     UYEAR         YY
     C     YY            IFGT      80
     C                   Z-ADD     19            CC
     C                   ELSE
     C                   Z-ADD     20            CC
     C                   ENDIF
     C                   MOVEL     CC            CCYY              4 0
     C                   MOVE      YY            CCYY
      *
      * Set default date for order entry.
     C     XJORDD        IFEQ      0
     C                   Z-ADD     DMCY          XJORDD
     C                   ENDIF
      *
      * Set date for transaction audit field.
     C                   Z-ADD     DD            DDD
     C                   Z-ADD     MM            MMM
     C                   Z-ADD     CC            CCC
     C                   Z-ADD     YY            YYY
     C                   ENDSR
      *
      *----------------------------------------------------------------
      * Ã SNDMG - Send message to this program's MSGQ.
      *----------------------------------------------------------------
      *
     C     YSNDMG        BEGSR
      *
     C                   CALL      'SNDMSGC'
     C                   PARM                    P0PGMQ                         Program queue
     C                   PARM                    P0PGRL                         Rel queue
     C                   PARM                    P0MSID                         Message ID
     C                   PARM                    P0MSGF                         Message file
     C                   PARM                    P0MSDA                         Message data
     C                   PARM                    P0MSTP                         Message type
      *
     C                   ENDSR
      *
      *----------------------------------------------------------------
      * Ã CLRMG - Clear message(s) from this program's MSGQ.
      *----------------------------------------------------------------
      *
     C     YCLRMG        BEGSR
      *
     C                   CALL      'RMVMSGC'
      *
     C                   ENDSR
      *
      *----------------------------------------------------------------
      * *UPPVCT- Update the voucher control
      *----------------------------------------------------------------
      *
     C     UPPVCT        BEGSR
     C*    XSELC         IFNE      *BLANK
     C*    XKPROD        ORNE      *BLANK
     C*    XNDESC        ORNE      *BLANK
     C*    XKPRIC        ORNE      *ZEROS
     C*    XKQTYN        ORNE      *ZEROS
     C*    XKVALU        ORNE      *ZEROS
     C*    XKVATA        ORNE      *ZEROS
     C*    XKSERC        ORNE      *BLANKS
     C*    XKSERF        ORNE      *ZEROS
     C*    XKSERT        ORNE      *ZEROS
     C*                  MOVE      XKSERC        WKSERC
     C*                  MOVE      XKSERT        TEMP              9 0
     C*                  ADD       1             TEMP
     C*    VCHKEY        SETLL     HSLVCTLA
     C*    XKPROD        READE     HSLVCTLA                               92
     C*                  MOVE      TEMP          BSERNN
     C*                  UPDATE    HSFVCTL
     C*                  ENDIF
      *
     C                   ENDSR
      *
      *----------------------------------------------------------------
      * *INZSR - Initialization.
      *----------------------------------------------------------------
      *
     C     *INZSR        BEGSR
BL    *
BL    * Dummy read to open file.
BL   C                   READ      HSPINVT                                99
BL   C                   UPDATE    HSFINVT
     C                   OPEN      HSLEXPAA
BL   C                   READ      HSLEXPAA                               99
     C                   CLOSE     HSLEXPAA
      *
      * Key lists.
      * Key List for Voucher Control.
     C*    VCHKEY        KLIST
     C*                  KFLD                    XKPROD
     C*                  KFLD                    WKSERC
      *
      * Key List for Voucher Cross-Reference.
     C     KEYXRF        KLIST
     C                   KFLD                    CCYY
     C                   KFLD                    XJCUST
     C                   KFLD                    XKPROD
      *
      * Key List for Sales Order.
     C     KEYOLN        KLIST
     C                   KFLD                    XJSORD
     C                   KFLD                    XOLIN             4 0
      *
      * Key List for Inventory Masterfile.
     C     IVMKEY        KLIST
     C                   KFLD                    XKPROD
     C                   KFLD                    WKSUBC
     C                   MOVE      *BLANKS       WKSUBC
      *
      *
      * Key List for Customer Discount File.
     C     DSCKEY        KLIST
     C                   KFLD                    ECTYP
     C                   KFLD                    PFAMT
      *
      * Parameter Lists.
      * Entry parameter list.
     C     *ENTRY        PLIST
     C                   PARM                    YTYPE             3            Order Type
     C                   PARM                    YCUST             8            Customer No.
     C                   PARM                    YPROD            13            Product Name
     C                   PARM                    YTEXT            40            Product description
     C                   PARM                    YPRIC            20 5          Price
     C                   PARM                    YQTYN            15 0          Quantity
     C                   PARM                    XJSORD            8 0          Commande £
      *
      *
      * Field definitions
     C     *LIKE         DEFINE    KVALU         TKVALU
     C     *LIKE         DEFINE    KVALU         XKVALU
     C     *LIKE         DEFINE    KVALU         WKTOTV
     C**   *LIKE         DEFINE    XKSERC        WKSERC
     C     *LIKE         DEFINE    BSERNN        WKSERN
     C     *LIKE         DEFINE    YQTYN         WKQTYN
     C     *LIKE         DEFINE    YQTYN         XKQTYN
     C     *LIKE         DEFINE    DSUBC         WKSUBC
     C     *LIKE         DEFINE    XNO           WKRANG
     C     *LIKE         DEFINE    XNO           WKVCTL
     C     *LIKE         DEFINE    XNO           UPDAT
     C     *LIKE         DEFINE    KPRIC         XKPRIC
     C     *LIKE         DEFINE    KPROD         XKPROD
     C*
     C     *LIKE         DEFINE    KVATA         XKVATA
     C     *LIKE         DEFINE    NDESC         XNDESC
      *
      * Variable declarations.
     C                   MOVE      '0'           XNO               1
     C                   MOVE      '1'           XYES              1
     C                   MOVE      XNO           XERROR            1
     C                   MOVE      XNO           XUPDAT            1
     C                   MOVE      XNO           XDELET            1
BL   C                   MOVEL     *BLANKS       ORDPRM            8
     C                   Z-ADD     0             RRNX              5 0
     C                   Z-ADD     0             RRNVAL            5 0
     C                   Z-ADD     0             RRNLIN            5 0
     C                   Z-ADD     0             RRNQ              5 0
     C                   Z-ADD     0             RRNP              5 0
      *
      * Prepare message subfile.
     C                   MOVE      *ON           *IN79
     C                   MOVE      XPGMID        P0PGMQ           10            Program queu
     C                   MOVEL     '*PRV'        P0PGRL            5            Rel queue
     C                   MOVE      *BLANKS       P0MSID            7            Message ID
     C                   MOVEL     'HSM001'      P0MSGF           10            Message file
     C                   MOVE      *BLANKS       P0MSDA          132            Message data
     C                   MOVEL     '*INFO'       P0MSTP            7            Message type
      *
     C                   EXSR      YDATEC
      *
     C                   ENDSR
     C     DUMMY1        BEGSR
     C*                  WRITE     MSGCTL
     C*                  WRITE     HSS200A
     C*                  WRITE     HSS200B
     C*                  WRITE     HSS200E
XXXM C*                  WRITE     HSS200F
     C*                  EXFMT     HSS200D
OWE  C*                  READC     HSS200C                                99
     C                   ENDSR
** Days in Month array DIM.
312831303130313130313031

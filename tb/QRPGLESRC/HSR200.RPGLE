       // %CSTD==========================================================*
       //  Application. : SAM Arcad Sample application                   *
       //  Component. . : HSR200                        Type: RPG        *
       // ===============================================================*
       //  Sub-system . : TRD   Trade                                    *
       //  Function . . : SAL   Sales                                    *
       //  Sub-function :                                                *
       // %S=============================================================*
       //  Description of functionalities :                              *
       //                                                                *
       //                                                                *
       //                                                                *
       // %E=============================================================*
       //  AUTHOR:                           00:00                       *
       //  MODIFS: ** MARY      28/07/2000   12:09 V 1.00.B MR N[00/0506 *
       //            CR 00/0122 Sales Order Delivery Details             *
       // %ECSTD=========================================================*
       // %CSTD===========================================================*
       // * Application. : JAD        JAD                                 *
       // * Composant. . : HSR200                        Type: RPG        *
       // *===============================================================*
       // * Sous-syst}me :                                                *
       // * Fonction . . :                                                *
       // * Sous-fonction:                                                *
       // *%S=============================================================*
       // * Description des fonctionnalit{s:                              *
       // *                                                               *
       // *                                                               *
       // *                                                               *
       // *%E=============================================================*
       // * AUTEUR:                          00:00                        *
       // * MODIFS: ** JDESROCHES 06/08/2015   :    V 1.03.P    00/       *
       // *         01 JDESROCHES 06/07/2015 12:40  V 1.03.A    00/       *
       // %ECSTD==========================================================*
       // /TITLE Sales Order Entry / Maintenance / Inquiry
       //
       //  System        : HSV Control & Tracking
       //  Author        : Bob Lee (Intec Systems)
       //  Date          : January 1999
       //
       // ================================================================
       //  Indicator usage:
       //   20 - Order Valid for Despatch
       //   30 - 45 Input fields- DSPATR.
       //   62 - Control display/non-display of F10-Update.
       //   75 - 78 Program mode - Dispatch/Entry/Inquiry/Maintenance
       //   79 - MSGSFL (SFLCLR ETC...).
       //   99 - General CHAIN/SETLL result.
       //
       // ================================================================
       //  Maintenance   :
       //  Fix/Chg Ref. Date       Description.
       //  ------------ ---------- -----------------------------------
       // ================================================================
       Ctl-Opt DATEDIT(*YMD);
       Dcl-F HSLCUSTB   Keyed
                        RENAME(HSFCUST:HSFCUSTB);
       //  Customer Master by Post Code.
       //
       Dcl-F HSLCUSTA   Keyed;
       //  Customer Master by Customer Account.
       //
       // LTCUSTAIF  E           K        DISK
       //            HSFCUST                           KRENAMEALTCUSTF
       //  Customer Master by Customer Account.
       //
       Dcl-F HSLORDPA   Keyed;
       //  Sales Order Parameters.
       //
       Dcl-F HSLTRACB   Usage(*Update:*Delete) Keyed;
       //  Voucher Tracking by Product/Series/Serial No./Element No.
       //
       Dcl-F HSLEXPAA   Usage(*Update:*Delete:*Output) Keyed USROPN;
       //  Unbanded Order line file by Product/Series/Serial No.
       //
       Dcl-F HSLVCTLA   Usage(*Update:*Delete) Keyed;
       //  Voucher Control.
       //
       Dcl-F HSLVXRFB   Keyed;
       //  Agency Product Cross-Reference.
       //
       Dcl-F HSLORDHA   Usage(*Update:*Delete:*Output) Keyed;
       //  Sales Order Header.
       //
       Dcl-F HSLORDDB   Usage(*Update:*Delete:*Output) Keyed;
       //  Sales Order Detail.
       //
       Dcl-F HSLODELA   Usage(*Update:*Delete:*Output) Keyed;
       //  Sales Order Delivery Detail.
       //
       Dcl-F HSLSHPOA   Keyed;
       //  Ship Only.
       //
       Dcl-F HSLINVMA   Usage(*Update:*Delete) Keyed;
       //  Inventory Master.
       //
       Dcl-F HSLWHSEA   Keyed;
       //  Warehouse Master.
       //
       Dcl-F HSLCOMPA   Keyed;
       //  Company Master.
       //
       Dcl-F HSLPRODA   Keyed;
       //  Product Master.
       //
       Dcl-F HSLDISCA   Keyed;
       //  Customer Discount File
       //
       Dcl-F HSPINVT    Usage(*Update:*Delete:*Output) Keyed;
       //  Inventory Transactions.
       //
       Dcl-F ARTICLE    Keyed;
       //  FOR ARTCILE XREF
       //
       Dcl-F HSS200     WORKSTN INFDS(INFDS)
                                SFILE(HSS200C:RRNX);
       //  Screen file.
       // ================================================================
       //  Arrays.
       //
       //  Days in Month for date validation.
       Dcl-S DIM             Packed(2:0)     DIM(12) CTDATA PERRCD(12);
       // ================================================================
       //  Data Structures.
       //
       //  Screen Information DS.
       Dcl-Ds INFDS;
         KEY             Char(1)         Pos(369);
         CSRLOC          BinDec(4:0)     Pos(370);
       End-Ds;
       //
       Dcl-Ds XXXSDS PSDS Len(429);
         XPGMID          *PROC;
         XJOBNO          Char(10)        Pos(244);
         XUSRID          Char(10)        Pos(254);
       End-Ds;
       //
       //  General DDMMCCYY date format.
       Dcl-Ds *n;
         DD              Zoned(2:0)      Pos(1) INZ;
         MM              Zoned(2:0)      Pos(3) INZ;
         CC              Zoned(2:0)      Pos(5) INZ;
         YY              Zoned(2:0)      Pos(7) INZ;
         DMCY            Zoned(8:0)      Pos(1);
       End-Ds;
       //
       //  General CCYYMMDD date format.
       Dcl-Ds *n;
         CCC             Zoned(2:0)      Pos(1) INZ;
         YYY             Zoned(2:0)      Pos(3) INZ;
         MMM             Zoned(2:0)      Pos(5) INZ;
         DDD             Zoned(2:0)      Pos(7) INZ;
         CYMD            Zoned(8:0)      Pos(1);
       End-Ds;
       //
       //  Input order date format.
       Dcl-Ds *n;
         JDD             Zoned(2:0)      Pos(1) INZ;
         JMM             Zoned(2:0)      Pos(3) INZ;
         JCC             Zoned(2:0)      Pos(5) INZ;
         JYY             Zoned(2:0)      Pos(7) INZ;
         XJORDD          Zoned(8:0)      Pos(1);
       End-Ds;
       //
       //  Output order date format.
       Dcl-Ds *n;
         WKC             Zoned(2:0)      Pos(1) INZ;
         WKY             Zoned(2:0)      Pos(3) INZ;
         WKM             Zoned(2:0)      Pos(5) INZ;
         WKD             Zoned(2:0)      Pos(7) INZ;
         WKORDD          Zoned(8:0)      Pos(1);
       End-Ds;
       //
       // ----------------------------------------------------------------
       //  Constants.
       //
       //  Named hexidecimal constants for function keys.
       Dcl-C F03             CONST(X'33');
       Dcl-C F04             CONST(X'34');
       Dcl-C F06             CONST(X'36');
       Dcl-C F07             CONST(X'37');
       Dcl-C F10             CONST(X'3A');
       Dcl-C F11             CONST(X'3B');
       Dcl-C F12             CONST(X'3C');
       //
       //  Zeros to "SETOF" error indicators.
       Dcl-C XZEROS          CONST('0000000000000000000');
       Dcl-S ATag            Char(14);
       Dcl-S WKORDN          Packed(8:0);
       Dcl-S ORDERX          Packed(8:0)     DtaAra('HSAORDERNO');
       Dcl-S TOT             Packed(20:5);
       Dcl-S SHONLY          Char(1);
       Dcl-S RRNS            Packed(5:0);
       Dcl-S RRNT            Packed(5:0);
       Dcl-S pAToArrStr      Pointer;
       Dcl-S AToArrStr       Char(65535)     Based(pAToArrStr);
       Dcl-S TPROD1          Char(13);
       Dcl-S TQTYN1          Packed(15:0);
       Dcl-S TKPRIC          Packed(9:2);
       Dcl-S TKVALU          Packed(9:2);
       Dcl-S TNDESC          Char(19);
       Dcl-S TSELC           Char(1);
       Dcl-S TPROD           Char(13);
       Dcl-S TPRIC           Packed(9:2);
       Dcl-S TQTYN           Packed(9:0);
       Dcl-S TDESC           Char(19);
       Dcl-S XQUAN           Packed(9:0);
       Dcl-S XJUMP           Char(1);
       Dcl-S XFIRST          Char(1);
       Dcl-S RRNXX           Packed(5:0);
       Dcl-S OPROD           Char(13);
       Dcl-S OSERC           Char(3);
       Dcl-S OSERN           Packed(9:0);
       Dcl-S XXSERF          Packed(9:0);
       Dcl-S XXRES           Packed(9:0);
       Dcl-S XXSERC          Char(3);
       Dcl-S XXSERN          Packed(9:0);
       Dcl-S XJUMP1          Char(1);
       Dcl-S DIF             Packed(9:0);
       Dcl-S DSERN           Packed(9:0);
       Dcl-S RES             Packed(9:0);
       Dcl-S KKSERC          Char(3);
       Dcl-S KKSERT          Packed(9:0);
       Dcl-S XJCLIM          Packed(15:0);                                      // Credit Limit
       Dcl-S XJTYSL          Packed(20:5);                                      // Last Year Sales
       Dcl-S REM             Packed(4:0);
       Dcl-S LEAP            Char(1);
       Dcl-S M               Packed(2:0);
       Dcl-S CCYY            Packed(4:0);
       Dcl-S YRTNC           Char(1);
       Dcl-S YNUMBR          Char(15);
       Dcl-S FILEY           Char(10);
       Dcl-S NAMEY           Char(6);
       Dcl-S NUMBRY          Char(6);
       Dcl-S LINY            Packed(3:0);
       Dcl-S COLY            Packed(3:0);
       Dcl-S ROW             Packed(2:0);
       Dcl-S COL             Packed(2:0);
       Dcl-S TEMP            Packed(9:0);
       Dcl-S WKTOTV          Like(XKVALU);
       Dcl-S WKSERC          Like(XKSERC);
       Dcl-S WKSERN          Like(BSERNN);
       Dcl-S WKQTYN          Like(XKQTYN);
       Dcl-S WKSUBC          Like(DSUBC);
       Dcl-S WKRANG          Like(XNO);
       Dcl-S WKVCTL          Like(XNO);
       Dcl-S UPDAT           Like(XNO);
       Dcl-S XNO             Char(1);
       Dcl-S XYES            Char(1);
       Dcl-S XERROR          Char(1);
       Dcl-S XUPDAT          Char(1);
       Dcl-S XDELET          Char(1);
       Dcl-S ORDPRM          Char(8);
       Dcl-S RRNX            Packed(5:0);
       Dcl-S RRNVAL          Packed(5:0);
       Dcl-S RRNLIN          Packed(5:0);
       Dcl-S RRNQ            Packed(5:0);
       Dcl-S RRNP            Packed(5:0);
       Dcl-S P0PGMQ          Char(10);                                          // Program queu
       Dcl-S P0PGRL          Char(5);                                           // Rel queue
       Dcl-S P0MSID          Char(7);                                           // Message ID
       Dcl-S P0MSGF          Char(10);                                          // Message file
       Dcl-S P0MSDA          Char(132);                                         // Message data
       Dcl-S P0MSTP          Char(7);                                           // Message type
       // Prototype for HSC200A
       Dcl-Pr Pgm_HSC200A ExtPgm('HSC200A') End-Pr;
       // Prototype for HSR200
       Dcl-Pr Pgm_HSR200 ExtPgm('HSR200');
         YMODE           Char(1);                                               // Mode
         YORDNO          Char(8);                                               // Sales Order No.
         YTYPE           Char(3);                                               // Order Type
         YCUST           Char(8);                                               // Customer No.
       End-Pr;
       // Procedure interface for HSR200
       Dcl-Pi Pgm_HSR200;
         YMODE           Char(1);                                               // Mode
         YORDNO          Char(8);                                               // Sales Order No.
         YTYPE           Char(3);                                               // Order Type
         YCUST           Char(8);                                               // Customer No.
       End-Pi;
       // Prototype for HSR220
       Dcl-Pr Pgm_HSR220 ExtPgm('HSR220');
         ORDPRM          Char(8);
       End-Pr;
       // Prototype for HSR230
       Dcl-Pr Pgm_HSR230 ExtPgm('HSR230');
         ORDPRM          Char(8);
       End-Pr;
       // Prototype for HSR341
       Dcl-Pr Pgm_HSR341 ExtPgm('HSR341');
         YRTNC           Char(1);
         YNUMBR          Char(15);
         FILEY           Char(10);
         NAMEY           Char(6);
         NUMBRY          Char(6);
         LINY            Packed(3:0);
         COLY            Packed(3:0);
       End-Pr;
       // Prototype for HSR342
       Dcl-Pr Pgm_HSR342 ExtPgm('HSR342') End-Pr;
       // Prototype for RMVMSGC
       Dcl-Pr Pgm_RMVMSGC ExtPgm('RMVMSGC') End-Pr;
       // Prototype for SNDMSGC
       Dcl-Pr Pgm_SNDMSGC ExtPgm('SNDMSGC');
         P0PGMQ          Char(10);                                              // Program queue
         P0PGRL          Char(5);                                               // Rel queue
         P0MSID          Char(7);                                               // Message ID
         P0MSGF          Char(10);                                              // Message file
         P0MSDA          Char(132);                                             // Message data
         P0MSTP          Char(7);                                               // Message type
       End-Pr;
       //
       // ================================================================
       //  M A I N L I N E
       // ================================================================
       //
       //  Dispatch mode
       If YMODE = 'D';
         *IN75 = *ON;
       Else;
         *IN75 = *OFF;
       EndIf;
       Read ARTICLE;
       *IN99 = %Eof;
       //
       //  Entry mode
       If YMODE = 'E';
         *IN76 = *ON;
       Else;
         *IN76 = *OFF;
       EndIf;
       //
       //  Inquiry mode
       If YMODE = 'I';
         *IN77 = *ON;
       Else;
         *IN77 = *OFF;
       EndIf;
       //
       //  Maintenance mode
       If YMODE = 'M';
         *IN78 = *ON;
       Else;
         *IN78 = *OFF;
       EndIf;
       //
       //  Program mode
       DoU ATag <> 'START' and ATag <> 'HDR'
         and ATag <> 'DET';
         If ATag = 'START' or ATag = *Blanks;
           ATag = *Blanks;
           If YMODE = 'I'
             or YMODE = 'D'
             or YMODE = 'M';
             If YORDNO <> *BLANKS;
               XJSORD = %Dec(%XLate(' ':'0':
                     YORDNO):8:0);
               WKORDN = %Dec(%XLate(' ':'0':
                     YORDNO):8:0);
               Chain WKORDN HSLORDHA;
               *IN99 = not %Found;
             EndIf;
           EndIf;
           //
           If YMODE = 'D';
             XJTYPE = JTYPE;
             XJCUST = JCUST;
             WKORDD = JORDD;
             JCC = WKC;
             JYY = WKY;
             JMM = WKM;
             JDD = WKD;
             XJORDR = JORDR;
             XJSORD = JSORD;
             If JSTAT = 'A'
               or JSTAT = ' ';
               *IN20 = *ON;
             Else;
               *IN20 = *OFF;
               P0MSID = 'HSV2015';
               Exsr YSNDMG;
             EndIf;
             Chain JCUST HSLCUSTA;
             *IN13 = not %Found;
             XJPCDE = EPCDE;
             XENAM1 = ENAM1;
             Setll JSORD HSLORDDB;
             ReadE JSORD HSLORDDB;
             *IN99 = %Eof;
             DoW *IN99 = *OFF;
               XVQTY += KQTYN;
               ReadE JSORD HSLORDDB;
               *IN99 = %Eof;
             EndDo;
             XJDISP = KDISP;
           Else;
             Exsr YCLRSC;
           EndIf;
           //
           //  Dispatch mode.
           If YMODE = 'D';
             Chain JSORD HSLODELA;
             *IN99 = not %Found;
             If VNAM1 = *BLANKS;
               XXNAM1 = ENAM1;
               XXNAM2 = ENAM2;
               XXADR1 = EADR1;
               XXADR2 = EADR2;
             Else;
               XXNAM1 = VNAM1;
               XXNAM2 = *Blanks;
               XXADR1 = VADR1;
               XXADR2 = VADR2;
             EndIf;
           EndIf;
           //
           //  Dispatch, or Inquiry, or Maintenance mode.
           If YMODE = 'I'
             or YMODE = 'D'
             or YMODE = 'M';
             XJCUST = YCUST;
             XJTYPE = YTYPE;
           EndIf;
         EndIf;
         If ATag = 'HDR' or ATag = *Blanks;
           ATag = *Blanks;
           *IN47 = *OFF;
           //  Display screen to receive Customer No. until F03/F10
           DoU KEY = F03
             or KEY = F10
             and XERROR = XNO;
             //
             //  Exit if F03 pressed.
             If KEY = F03;
               *INLR = *ON;
               Leave;
             EndIf;
             //
             //  Display messages.
             Write MSGCTL;
             //
             XXTME = %Dec(%Time);
             Exfmt HSS200A;
             //
             Chain XJTYPE HSLORDPA;
             *IN99 = not %Found;
             If MSALE = 'C';
               *IN30 = *ON;
               XERROR = XYES;
               P0MSID = 'HSV2027';
               Exsr YSNDMG;
               Iter;
             EndIf;
             //
             Clear XVQTY;
             If *IN20 = *OFF
               and YMODE = 'D';
               Iter;
             EndIf;
             //
             RCDNBR = 1;
             //
             //  Clear messages.
             Exsr YCLRMG;
             //
             //  Process the various function keys available on the screen.
             //
             //  Process Despatch if F07 pressed.
             If KEY = F07;
               //
               //  Validate that Order Type is a valid Type.
               Chain XJTYPE HSLORDPA;
               *IN99 = not %Found;
               If *IN99 = *ON;
                 *IN30 = *ON;
                 XERROR = XYES;
                 P0MSID = 'HSV2001';
                 Exsr YSNDMG;
                 ATag = 'START';
                 Leave;
               EndIf;
             EndIf;
             //
             If YMODE = 'D'
               and WKORDN <> XJSORD;
               WKORDN = XJSORD;
               Chain XJSORD HSLORDHA;
               *IN99 = not %Found;
               If *IN99 = *ON;
                 *IN20 = *OFF;
                 P0MSID = 'HSV2017';
                 Exsr YSNDMG;
               Else;
                 XJTYPE = JTYPE;
                 XJCUST = JCUST;
                 WKORDD = JORDD;
                 JCC = WKC;
                 JYY = WKY;
                 JMM = WKM;
                 JDD = WKD;
                 XJORDR = JORDR;
                 XJSORD = JSORD;
                 If JSTAT = 'A'
                   or JSTAT = ' ';
                   *IN20 = *ON;
                 Else;
                   *IN20 = *OFF;
                   P0MSID = 'HSV2015';
                   Exsr YSNDMG;
                 EndIf;
               EndIf;
               Chain JCUST HSLCUSTA;
               *IN13 = not %Found;
               XJPCDE = EPCDE;
               XENAM1 = ENAM1;
               Setll JSORD HSLORDDB;
               ReadE JSORD HSLORDDB;
               *IN99 = %Eof;
               DoW *IN99 = *OFF;
                 XVQTY += KQTYN;
                 ReadE JSORD HSLORDDB;
                 *IN99 = %Eof;
               EndDo;
               XJDISP = KDISP;
               ATag = 'HDR';
               Leave;
             EndIf;
             //
             //  Program in inquiry, dispatch or maintenance mode
             If YMODE = 'I'
               or YMODE = 'M'
               or YMODE = 'D';
               //
               //  Initialise subfile
               *In70 = '1';
               *In71 = '0';
               *In72 = '0';
               *In73 = '1';
               Write HSS200B;
               If XJSORD <> 0;
                 RRNX = 0;
                 Setll XJSORD HSLORDDB;
                 DoU *IN97 = *ON;
                   ReadE XJSORD HSLORDDB;
                   *IN97 = %Eof;
                   //
                   //  Build subfile from existing order.
                   If *IN97 = *OFF;
                     RRNX += 1;
                     Chain RRNX HSS200C;
                     *IN99 = not %Found;
                     XOLIN = KLINE;
                     XSELC = KLTYP;
                     XKPROD = KPROD;
                     XNDESC = KDESC;
                     XKSERC = KSERC;
                     XKSERF = KSERNF;
                     XKSERT = KSERNT;
                     XKQTYN = KQTYN;
                     XKPRIC = KPRIC;
                     XKVALU = KVALU;
                     XXQTYN = XKQTYN;
                     //
                     //  Ship Only - set line value to zero.
                     If SHONLY = 'Y'
                       and XSELC = 'S';
                       XKVALU = 0;
                     EndIf;
                     //
                     //           #KVALU    MULT MVATP     #KVATA
                     //
                     //  Extended value
                     If XSELC = 'S'
                       or XSELC = 'N';
                       XKVALU = XKPRIC * XKQTYN;
                       //
                       //  Ship Only - set line value to zero.
                       If SHONLY = 'Y'
                         and XSELC = 'S';
                         XKVALU = 0;
                       EndIf;
                       //
                       //           #KVALU    MULT MVATP     #KVATA
                       //
                     EndIf;
                     //
                     //  Write subfile record.
                     If *IN99 = *OFF;
                       Update HSS200C;
                     Else;
                       Write HSS200C;
                     EndIf;
                   EndIf;
                 EndDo;
               EndIf;
               Leave;
             EndIf;
             //
             //  Program mode - Entry
             If YMODE = 'E';
               //
               //  Execute search if F04 pressed.
               If KEY = F04;
                 Exsr YSERCH;
                 Iter;
                 Exsr YCLRMG;
               EndIf;
               //
               //  Validate header screen fields.
               Exsr VALIDH;
               //
               //  Validate ship only requirements.
               Exsr SHPONL;
               //
             EndIf;
             //
             //  Process Despatch if F07 pressed.
             If KEY = F07;
               //
               Leave;
             EndIf;
             //
             //  End of DO loops for F03/F10 key checks.
           EndDo;
           If ATag = 'START' or ATag = 'HDR';
             Iter;
           EndIf;
           *IN61 = *OFF;
           //
           //  Clear messages.
           Exsr YCLRMG;
           //
           //  Exit if F03 pressed.
           If KEY = F03;
             //  Exit program.
             *INLR = *ON;
             Return;
           EndIf;
           //
           // -------------------------------------------------------
           //
           //  Program mode - Entry
           //  Retrieve order number.
           If YMODE = 'E';
             In *LOCK ORDERX;
             ORDERX += 1;
             Out ORDERX;
             XJSORD = ORDERX;
           EndIf;
           //
           //  Display screen to receive Order Details until F10/F12
         EndIf;
         // branch when ATag = 'DET'
         ATag = *Blanks;
         //
         //  Program in inquiry, or dispatch, or maintenance mode
         If YMODE = 'I'
           or YMODE = 'M'
           or YMODE = 'D'
           or KEY = F12;
           *IN73 = *OFF;
         Else;
           *IN73 = *ON;
         EndIf;
         //
         DoU KEY = F10
           and XERROR = XNO
           or KEY = F12;
           //
           //  Display messages.
           Write MSGCTL;
           //
           //  Read subfile to accumulate order value.
           If RRNX > 0;
             RRNVAL = 1;
             XKTVAL = 0;
             DoU *IN98 = *ON
               or RRNVAL = 100
               or XSELC = *BLANK;
               Chain RRNVAL HSS200C;
               *IN98 = not %Found;
               If XSELC <> *BLANK
                 and XKPROD <> *BLANK
                 and *IN98 = *OFF
                 or XSELC <> *BLANK
                 and XNDESC <> *BLANK
                 and *IN98 = *OFF;
                 XKTVAL += XKVALU;
               EndIf;
               RRNVAL += 1;
             EndDo;
           EndIf;
           //
           //  Dispatch mode.
           If YMODE = 'D';
             Chain XJCUST HSLCUSTA;
             *IN99 = not %Found;
             //  ...load Customer record into screen 2 fields.
             XJNAM1 = ENAM1;                                                    // Name 1
             XJNAM2 = ENAM2;                                                    // Name 2
             XJADR1 = EADR1;                                                    // Address 1
             XJADR2 = EADR2;                                                    // Address 2
           EndIf;
           //
           //
           //  Display footer.
           If KEY <> F07;
             Write HSS200E;
           EndIf;
           //
           XXTME = %Dec(%Time);
           *In70 = '0';
           *In71 = '1';
           *In72 = '1';
           //
           If KEY <> F07;
             Exfmt HSS200B;
             XJUMP1 = XNO;
             //
             If KEY <> F10;
               Exsr YDELT;
             EndIf;
             //
           EndIf;
           //
           *IN73 = *OFF;
           *IN86 = *OFF;
           //
           //  Clear messages (Only if an error has not been repeated)
           //
           If XERROR = XNO;
             //  Clear messages.
             Exsr YCLRMG;
           EndIf;
           //
           //  Process the various function keys available on the screen.
           //
           //  Exit if F12 pressed.
           If KEY = F12;
             ATag = 'HDR';
             Leave;
           EndIf;
           //
           //  Execute search if F04 pressed.
           If KEY = F04;
             Exsr YSERSF;
             Iter;
           EndIf;
           //
           //  Check for agency cross-references.
           If MXREF = 'Y';
             Exsr AGXREF;
           EndIf;
           //
           //  Validate order detail screen fields (only re-validate data
           //  if it has been changed. Otherwise re-display error message).
           If XERROR = XYES;
             Chain RRNS HSS200C;
             *IN98 = not %Found;
             If XSELC <> TSELC
               or XKPROD <> TPROD
               or XKPRIC <> TPRIC
               or XKQTYN <> TQTYN
               or XNDESC <> TDESC;
               *IN93 = *ON;
             EndIf;
           EndIf;
           //
           If *IN93 = *ON
             or XERROR = XNO;
             //
             //  Validate order detail screen fields.
             Exsr VALIDD;
           EndIf;
           //
           //  Discount retrieval.
           Exsr DISCNT;
           //
           //  Stock allocation preview
           //                  - with banded allocation and manual allocation.
           //           MSALE     IFEQ 'S'
           //                     EXSR ALLOCS
           //                     ENDIF
           //
           //  Process Dispatch if F07 pressed.
           If KEY = F07;
             //
             Leave;
           EndIf;
           //
           //  End of DO loops for F10/F12 key checks.
         EndDo;
         If ATag = 'HDR';
           Iter;
         EndIf;
         //
         // -------------------------------------------------------
         //  Confirm order - prompt for delivery details.
         //                - file updates when F10 pressed.
         DoU KEY = F10
           and XERROR = XNO
           or KEY = F12;
           //
           //  Clear messages.
           Exsr YCLRMG;
           //
           //  Process the various function keys available on the screen.
           //
           //  Exit if F12 pressed.
           //           KEY       IFEQ F12
           //                     GOTO DET
           //                     ENDIF
           //
           //  Check for minimum order value.
           If KEY <> F07;
             If MORDV > 0;
               Exsr MINVAL;
             EndIf;
             //
             //  Credit limit check.... Warning only.
             TOT = XKTVAL + XJTYSL;
             If TOT > XJCLIM;
               *IN46 = *ON;
               P0MSID = 'HSV2019';
               Exsr YSNDMG;
             EndIf;
           EndIf;
           //
           //  Display messages.
           Write MSGCTL;
           //
           //  Display key text footer.
           //                     WRITEHSS200E
           If KEY <> F07;
             Write HSS200F;
           EndIf;
           //
           //  Delivery address prompt screen.
           XXTME = %Dec(%Time);
           //
           //  Program in inquiry, dispatch or maintenance mode
           If YMODE = 'I'
             or YMODE = 'M'
             or YMODE = 'D';
             Chain WKORDN HSLODELA;
             *IN99 = not %Found;
             //
             //  Write fields for order delivery detail record.
             If *IN99 = *OFF;
               XVNAM1 = VNAM1;
               XVADR1 = VADR1;
               XVADR2 = VADR2;
               XVADR3 = VADR3;
               XVPCDE = VPCDE;
               XVDIN1 = VDIN1;
               XVDIN2 = VDIN2;
               XVDIN3 = VDIN3;
             EndIf;
           EndIf;
           //
           //  Write order delivery details record.
           If KEY <> F07;
             Exfmt HSS200D;
           EndIf;
           //
           //  Exit if F12 pressed.
           If KEY = F12;
             Exsr YDELT;
             ATag = 'DET';
             Leave;
           EndIf;
           //
           //  Clear messages.
           If *IN47 = *OFF;
             Exsr YCLRMG;
           EndIf;
           //
           //  Update all files, if no errors exist.
           If KEY = F10
             and XERROR = XNO
             or KEY = F07;
             RRNX = 1;
             DoU *IN98 = *ON
               or RRNX = 100
               or XSELC = *BLANK;
               Chain RRNX HSS200C;
               *IN98 = not %Found;
               If XSELC <> *BLANK
                 and XKPROD <> *BLANK
                 and *IN98 = *OFF
                 or XSELC <> *BLANK
                 and XNDESC <> *BLANK
                 and *IN98 = *OFF;
                 //
                 //  Save RRN for current order line.
                 RRNLIN = RRNX;
                 //
                 //  Update Voucher Control if in entry mode....
                 If YMODE = 'E'
                   and MSALE = 'S';
                   Exsr UPPVCT;
                 EndIf;
                 //
                 //  Re-position to required subfile line
                 RRNX = RRNLIN;
                 Chain RRNX HSS200C;
                 *IN98 = not %Found;
                 //
                 //  Create/Update order details if in Entry,Maint,or Dispatch mode.
                 If YMODE = 'E'
                   or YMODE = 'D'
                   or YMODE = 'M';
                   Exsr UPORDD;
                 EndIf;
                 //
                 //  Update voucher tracking if in Entry or Dispatch mode.
                 //  Stock lines only.
                 If YMODE = 'E'
                   and XSELC = 'S'
                   or YMODE = 'D'
                   and XSELC = 'S';
                   Exsr UPTRAC;
                 EndIf;
                 //
                 //  Allocate inventory if in Entry mode.
                 //  Stock lines only.
                 If YMODE = 'E'
                   and XSELC = 'S';
                   //  Update inventory allocations.
                   Exsr UPINVA;
                 EndIf;
                 //
                 //  Sell inventory if in Dispatch mode.
                 //  Stock lines only.
                 If YMODE = 'D'
                   and XSELC = 'S';
                   //  Update inventory sales.
                   Exsr UPINVS;
                   //
                   //  Create inventory sales transaction.
                   Exsr UPINVT;
                 EndIf;
               EndIf;
               RRNX += 1;
             EndDo;
             //
             //  Create/Update order header if in Entry,Maint,or Dispatch mode.
             If YMODE = 'E'
               or YMODE = 'D'
               or YMODE = 'M';
               Exsr UPORDH;
               //
               //  Create/update delivery record if in Entry,Maint or Dispat.mode.
               Exsr UPODEL;
             EndIf;
             //
             //  Ship to all delivery points if in Entry mode.
             If YMODE = 'E'
               and MALLP = 'Y';
               Exsr SHPALL;
             EndIf;
             //
             //  Print delivery note if in Entry mode, and charges only not set.
             If YMODE = 'E'
               and MINVC <> 'Y';
               Exsr PRTDEL;
             EndIf;
             //
             //  Print Invoice if in Entry mode, and Immediate print required.
             If YMODE = 'E'
               and MINVP = 'I';
               Exsr PRTINV;
             EndIf;
             //
           EndIf;
           //  Process Despatch if F07 pressed.
           If KEY = F07;
             Exsr YCLRSC;
             //
             Leave;
           EndIf;
           //
           //  End of Confirmation.
         EndDo;
         If ATag = 'DET';
           Iter;
         EndIf;
         //
         //  Return to order details if F12 pressed.
         If KEY = F12;
           ATag = 'DET';
           Iter;
         EndIf;
         //
         //  Clear Subfile
         *In70 = '1';
         *In71 = '0';
         *In72 = '0';
         *In73 = '0';
         Write HSS200B;
         //
         //  Clear contents of HSPEXPA.
         Exsr YDELT;
         //
         //  Return to order header.
         ATag = 'HDR';
         Iter;
       EndDo;
       //
       // ****************************************************************
       // ----------------------------------------------------------------
       //  @CLRSC - Clear screen fields.
       // ----------------------------------------------------------------
       //
       BegSr YCLRSC;
         //
         //  Clear header screen fields.
         If YMODE <> 'E';
           Clear XJTYPE;
         EndIf;
         If YMODE = 'D';
           Clear XJSORD;
           Clear XENAM1;
           Clear XVQTY;
         EndIf;
         Clear XJCOMP;
         Clear XJWHSE;
         Clear XJPCDE;
         Clear XJCUST;
         Clear XJDISP;
         Clear XJORDD;
         Clear XJORDR;
         //
         //  Clear delivery screen fields.
         Clear XVNAM1;
         Clear XVADR1;
         Clear XVADR2;
         Clear XVADR3;
         Clear XVPCDE;
         Clear XVDIN1;
         Clear XVDIN2;
         Clear XVDIN3;
       EndSr;
       //
       // ----------------------------------------------------------------
       //  SHPONL - Validate ship only requirements.
       // ----------------------------------------------------------------
       BegSr SHPONL;
         //
         //  Check if a ship only record exists for this customer
         Chain XJCUST HSLSHPOA;
         *IN99 = not %Found;
         If *IN99 = *OFF;
           SHONLY = 'Y';
         Else;
           SHONLY = ' ';
         EndIf;
       EndSr;
       //
       // ----------------------------------------------------------------
       //  AGXREF - Check for agency cross-references.
       // ----------------------------------------------------------------
       BegSr AGXREF;
         Chain (CCYY : XJCUST : XKPROD) HSLVXRFB;
         *IN99 = not %Found;
         If *IN99 = *OFF;
           KPROD = HPROD;
           XKPROD = HPROD;
         EndIf;
       EndSr;
       //
       // ----------------------------------------------------------------
       //  VALIDD - Validate order detail screen fields.
       // ----------------------------------------------------------------
       BegSr VALIDD;
         //
         //  Order Detail validation:
         //
         //  Reset work values.
         //                     Z-ADD0         RRN#
         RRNS = 0;
         RRNT = 1;
         Reset XERROR;
         *IN61 = *OFF;                                                          // Hide F10/F11
         //
         //  Reset error indicators.
         pAToArrStr = %Addr(*IN(30));
         %Subst(AToArrStr:1:19) = XZEROS;
         //
         //  Read all Records from subfile and validate contents.
         DoU *IN99 = *ON;                                                       // ..Do1
           //
           //  Process the subfile in one of two modes depending on whether
           //  a previous change to the subfile has been made....
           //
           If *IN82 = *OFF;
             ReadC HSS200C;
             *IN99 = %Eof;
             RRNT = RRNX;
             RRNT += 1;
           Else;
             If RRNT = 101;
               Leave;
             EndIf;
             Chain RRNT HSS200C;
             *IN99 = not %Found;
             RRNT += 1;
           EndIf;
           //
           //                     READCHSS200C                  99
           //  Retrieve subfile record.
           If *IN99 = *OFF;                                                     // ...If2
             TPROD1 = XKPROD;
             TQTYN1 = %Dec(%Subst(%EditC(TQTYN1:'X'):1:6)
                    + %EditC(XKQTYN:'X')
                       :15:0);
             *IN82 = *ON;
             //
             //  Reset subfile error indicators.
             *In36 = '0';
             *In37 = '0';
             *In38 = '0';
             *In39 = '0';
             *In40 = '0';
             *In41 = '0';
             *In42 = '0';
             *In43 = '0';
             //
             If XSELC <> *BLANK                                                 // ....If3
               or XKPROD <> *BLANK                                              // ....If3
               or XNDESC <> *BLANK                                              // ....If3
               or XKPRIC <> *ZEROS                                              // ....If3
               or XKQTYN <> *ZEROS                                              // ....If3
               or XKVALU <> *ZEROS                                              // ....If3
               or XKVATA <> *ZEROS                                              // ....If3
               or XKSERC <> *BLANKS                                             // ....If3
               or XKSERF <> *ZEROS                                              // ....If3
               or XKSERT <> *ZEROS;                                             // ....If3
               //
               //  Count active subfile records.
               //                     ADD  1         RRN#
               RRNS += 1;
               //
               //  Validate Selection code.
               If XSELC <> 'S'
                 and XSELC <> 'N'
                 and XSELC <> 'T'
                 //
                 //  Validate Selection code / Product code / Text combination
                 or XSELC = *BLANK
                 and XKPROD <> *BLANKS
                 or XSELC = *BLANK
                 and XNDESC <> *BLANKS
                 or XSELC = *BLANK
                 and XKPRIC <> 0
                 or XSELC = *BLANK
                 and XKQTYN <> 0
                 or XSELC = 'T'
                 and XKPROD <> *BLANKS
                 or XSELC = 'T'
                 and XKPRIC <> 0
                 or XSELC = 'T'
                 and XKQTYN <> 0
                 or XSELC = 'T'
                 and XKSERC <> *BLANKS
                 or XSELC = 'T'
                 and XKSERF <> 0
                 or XSELC = 'T'
                 and XKSERT <> 0
                 or XSELC = 'S'
                 and XKQTYN = 0
                 //           #SELC     OREQ 'S'
                 //           #KPRIC    ANDEQ0
                 or XSELC = 'N'
                 and XKQTYN = 0
                 or XSELC = 'N'
                 and XKPRIC = 0;
                 *IN36 = *ON;
                 XERROR = XYES;
                 P0MSID = 'HSV2006';
                 Exsr YSNDMG;
               EndIf;
               //
               //  Validate Selection code for Invoice Charge Only order type.
               If XSELC = 'S'
                 and MINVC = 'Y';
                 *IN36 = *ON;
                 XERROR = XYES;
                 P0MSID = 'HSV2014';
                 Exsr YSNDMG;
               EndIf;
               //
               If YMODE = 'D'
                 and XKQTYN > XXQTYN;
                 XERROR = XYES;
                 P0MSID = 'HSV2020';
                 Exsr YSNDMG;
                 Leave;
               Else;
                 Exsr YCLRMG;
               EndIf;
               //
               //  Validate Product.
               If XSELC = 'S';
                 If XKPROD <> *BLANKS;
                   Chain XKPROD HSLPRODA;
                   *IN99 = not %Found;
                   If *IN99 = *ON;
                     *IN37 = *ON;
                     XERROR = XYES;
                     P0MSID = 'HSV2007';
                     Exsr YSNDMG;
                   Else;
                     XNDESC = NDESC;
                     XKPRIC = NPRIC;
                   EndIf;
                 EndIf;
               EndIf;
               //
               //  Reverse price for Credit
               If MSALE = 'C';
                 XKPRIC = 0 - XKPRIC;
               EndIf;
               //
               //  Extended value
               If XSELC = 'S'
                 or XSELC = 'N';
                 XKVALU = XKPRIC * XKQTYN;
                 //
                 //           #KVALU    MULT MVATP     #KVATA
               EndIf;
               //
               //  Save Price, Value and Description field contents....
               TKPRIC = XKPRIC;
               TKVALU = XKVALU;
               TNDESC = XNDESC;
               //
               //  Check quantity against allocated serial numbers.
               //           #KQTYN    IFNE 0
               //           #KSERF    ANDNE0
               //           #KSERT    ANDNE0
               //           #KSERT    SUB  #KSERF    WKQTYN
               //                     ADD  1         WKQTYN
               //           #KQTYN    IFNE WKQTYN
               //                     MOVEA'1011'    *IN,40
               //                     MOVE #YES      #ERROR
               //                     MOVEL'HSV2013' P0MSID
               //                     EXSR @SNDMG
               //                     ENDIF
               //                     ENDIF
               //
               //  Check voucher control if not yet allocated
               If XSELC = 'S'
                 and XKQTYN > 0;
                 //           #KSERC    IFEQ *BLANKS
                 //           #KSERF    ANDEQ0
                 //           #KSERT    ANDEQ0
                 WKVCTL = 'U';
                 Exsr UPVCTL;
               EndIf;
               //                     ENDIF
               //
               //  Update subfile record with DSPATR settings and set SFLRCDNBR.
               RCDNBR = RRNX;
               XKPRIC = TKPRIC;
               XKVALU = TKVALU;
               XNDESC = TNDESC;
               Update HSS200C;                                                  // SFL
               *In36 = '0';
               *In37 = '0';
               *In38 = '0';
               *In39 = '0';
               *In40 = '0';
               *In41 = '0';
               *In42 = '0';
               *In43 = '0';
               //
               //  If errors encountered in subfile then exit subfile validation.
               If XERROR = XYES;
                 TSELC = XSELC;
                 TPROD = XKPROD;
                 TPRIC = XKPRIC;
                 TQTYN = XKQTYN;
                 TDESC = XNDESC;
                 //                     Z-ADDRRN#      RRNS    50
                 Leave;
               EndIf;
               //
               //  End of non-blank subfile check.
             EndIf;                                                             // ....EndIf3
             //
             //  End of Subfile CHAIN check.
           EndIf;                                                               // ...EndIf2
           //
           //  End of loop for next subfile record.
         EndDo;                                                                 // ..EndDo1
         //
         //  Save last RRN number used.
         //                     Z-ADDRRN#      SAVRRN
         //
         //  If no errors found...
         If XERROR = XNO;
           //  ...display "Data valid.Press F10 to update" message...
           P0MSID = 'HSV9006';
           Exsr YSNDMG;
           //  ...activate F10 key (*IN61=*ON).
           *IN61 = *ON;
         EndIf;
         If XJUMP1 = XYES
           and *IN52 = *ON;
           Pgm_HSR342();
         EndIf;
         *IN52 = *OFF;
       EndSr;
       //
       // ----------------------------------------------------------------
       //  DISCNT - Discount retrieval.
       // ----------------------------------------------------------------
       //
       BegSr DISCNT;
         //
         //  If customer discount is not entered on header screen either
         //  caluclate from HSPDISC file keyed on Order Value or default
         //  according to Customer Type.
         //
         If XJDISP = 0;
           //
           //  Setup key list for HSPDISC
           //
           PFAMT = XKTVAL;
           //
           //  SETLL on HSLDISCA
           //
           Setll (ECTYP : PFAMT) HSLDISCA;
           *IN99 = %Equal;
           //
           //  If record not found READP
           //
           If *IN99 = *OFF;
             ReadP HSLDISCA;
             *IN99 = %Eof;
             //
             //  If not BOF
             //
             If *IN99 = *OFF
               and PCTYP = ECTYP;
               KDISP = PDISC;
               //
               //  Else calculate discount according to Customer Type
               //
             Else;
               //
               Select;
                   //
                   //  If Customer Type is '001' - Z-ADD 10 to discount
                   //
                 When ECTYP = '001';
                   KDISP = 10;
                   //
                   //  If Customer Type is '002' - Z-ADD 20 to discount
                   //
                 When ECTYP = '002';
                   KDISP = 20;
                   //
               EndSl;
               //
             EndIf;
             //
             //  Record found on SETLL
             //
           Else;
             //
             KDISP = PDISC;
             //
           EndIf;
           //
           //  #JDISP not equal to 0
           //
         Else;
           KDISP = XJDISP;
           //
         EndIf;
         //
       EndSr;
       //
       // ----------------------------------------------------------------
       //  Check for minimum order value.
       // ----------------------------------------------------------------
       BegSr MINVAL;
         WKTOTV = 0;
         RRNP = 1;
         //
         //  Accumulate order total value
         DoU *IN99 = *ON
           or RRNP = 100
           or XSELC = *BLANK;
           Chain RRNP HSS200C;
           *IN99 = not %Found;
           If *IN99 = *OFF;
             WKTOTV += XKVALU;
           EndIf;
           RRNP += 1;
         EndDo;
         //
         //  Compare order total value and minimum order value
         If WKTOTV < MORDV;
           XERROR = XYES;
           P0MSID = 'HSV2011';
           Exsr YSNDMG;
         EndIf;
         RRNP = 1;
         RRNX = 1;
       EndSr;
       //
       // ----------------------------------------------------------------
       //  Ship to all delivery points.
       // ----------------------------------------------------------------
       BegSr SHPALL;
         //
         //  SETLL on HSLCUSTA with Customer Number
         //
         Setll XJCUST HSLCUSTA;
         //
         //  READE on HSLCUSTA with Customer Number
         //
         ReadE XJCUST HSLCUSTA;
         *IN99 = %Eof;
         //
         //  If not EOF
         //
         DoW *IN99 = *OFF;
           //
           //  If ECGRP not equal to blanks
           //
           If ECGRP <> *BLANKS;
             //
             //  CHAIN to Customer File for Delivery Name and Address
             //
             //           ECGRP     CHAINALTCUSTF             13
             //
             //           *IN13     IFEQ *OFF
             //
             //  Write Order Header Record
             //
             Exsr UPORDH;
             //
             //  Set up Delivery Name and Address
             //
             XVNAM1 = ENAM1;
             XVADR1 = EADR1;
             XVADR2 = EADR2;
             XVADR3 = EADR3;
             XVPCDE = EPCDE;
             //
             //  Write Order Delivery Record
             //
             Exsr UPODEL;
             //
             //  Write Order Detail Record
             //
             Exsr UPORDD;
             //
             //  ECGRP equal to blanks
             //
             //                     ENDIF
             //
             //  Customer Record not found
             //
           EndIf;
           //
           //  Increment order number
           //
           In *LOCK ORDERX;
           ORDERX += 1;
           Out ORDERX;
           XJSORD = ORDERX;
           //
           //  READE on HSLCUSTA with Customer Number
           //
           ReadE XJCUST HSLCUSTA;
           *IN99 = %Eof;
           //
         EndDo;
         //
         //  EOF on HSLCUSTA
         //
       EndSr;
       //
       // ----------------------------------------------------------------
       //  Update voucher control.
       // ----------------------------------------------------------------
       BegSr UPVCTL;
         //
         *IN85 = *ON;
         //  Save order line values
         //
         XQUAN = 0;
         XJUMP = XNO;
         //  Clear flag for successful range allocation.
         WKRANG = *Blanks;
         //
         //  Clear flag for first tracking record found.
         XFIRST = XNO;
         //
         //  Get Serial number range.
         WKSERC = *Blanks;
         DoU *IN92 = *ON
           or WKRANG = 'Y';
           //
           //  Work back through the subfile checking for duplicate
           //  product codes.... If found validate data under DOUB subroutine.
           //
           RRNXX = RRNX;
           DoU RRNX = 0;
             RRNX -= 1;
             If RRNX = 0;
               Leave;
             EndIf;
             Chain RRNX HSS200C;
             *IN90 = not %Found;
             If XKPROD = TPROD1;
               RRNX = RRNXX;
               Exsr DOUB;
               *IN81 = *ON;
               Leave;
             EndIf;
           EndDo;
           //
           //  Reset subfile position.....
           RRNX = RRNXX;
           Chain RRNX HSS200C;
           *IN90 = not %Found;
           //
           //  Exit from subroutine if DOUB sr has been processed....
           //
           If *IN81 = *ON;
             XKSERF = XKSERF;
             XKSERC = XKSERC;
             XKSERT = XKSERT;
             *IN81 = *OFF;
             Leave;
           EndIf;
           //
           Setll (XKPROD : WKSERC) HSLVCTLA;
           ReadE XKPROD HSLVCTLA;
           *IN92 = %Eof;
           //
           //  Move pointer file values into temporary fields....
           //
           OPROD = BPROD;
           OSERC = BSERCC;
           OSERN = BSERNN;
           WKSERN = BSERNN;
           WKSERC = BSERCC;
           XXSERF = BSERNN;
           // *******             SUB  1         ##SERF
           //
           //
           If *IN92 = *OFF;
             //
             //  Scan voucher tracking file for first available voucher....
             //
             XKSERC = BSERCC;
             XKSERF = OSERN;
             //
             DoU *IN94 = *ON
               or XQUAN = XKQTYN;
               //
               If *IN85 = *ON;
                 Setll (XKPROD : XKSERC : XKSERF) HSLTRACB;
                 *IN85 = *OFF;
               EndIf;
               //
               ReadE XKPROD HSLTRACB;
               *IN94 = %Eof;
               //
               If *IN94 = *ON;
                 Leave;
               EndIf;
               //
               If ASERC <> WKSERC
                 or ASERN <> WKSERN
                 or AALLD <> *ZEROS;
                 DSERN += 1;
                 //
                 If *IN87 = *OFF;
                   XXRES = XXSERN + 1;
                   //           XXRES     IFNE ASERN
                   //                     MOVE ASERN     ##SERF
                   //                     ELSE
                   XXSERF = ASERN;
                   XXSERF += 1;
                   WKSERN += 1;
                   *IN85 = *ON;
                   XKSERF += 1;
                   //                     ENDIF
                 Else;
                   //
                   //  No previous error so add to HSPEXPA.....
                   XXSERC = WKSERC;
                   XXSERN = WKSERN;
                   WKSERN -= 1;
                   XJUMP = XYES;
                   XJUMP1 = XYES;
                   //
                   // ***       *IN87     IFEQ *ON
                   USERNS = XXSERF;
                   USERNE = WKSERN;
                   USERCC = WKSERC;
                   UPROD = XKPROD;
                   Open HSLEXPAA;
                   Write HSFEXPA;
                   Close HSLEXPAA;
                   // ****                ENDIF
                   //
                   XXSERF = ASERN;
                   DSERN += 1;
                   If DSERN <> ASERN;
                     DIF = ASERN - DSERN;
                     XXSERF += DIF;
                   EndIf;
                   //
                   XKSERF += 1;
                   WKSERN += 2;
                   *IN85 = *ON;
                   *IN87 = *OFF;
                   //
                   If ASERC <> WKSERC;
                     WKSERC = ASERC;
                     WKSERN = ASERN;
                     XKSERC = WKSERC;
                     XKSERF = WKSERN;
                     XXSERF = WKSERN;
                     *IN85 = *ON;
                   EndIf;
                   //
                   Iter;
                 EndIf;
                 //
                 If ASERC <> WKSERC;
                   WKSERC = ASERC;
                   WKSERN = ASERN;
                   XKSERC = WKSERC;
                   XKSERF = WKSERN;
                   XXSERF = WKSERN;
                   *IN85 = *ON;
                 EndIf;
                 //
               EndIf;
               //
               //
               If AALLD = *ZEROS;
                 // **        *IN87     IFEQ *OFF
                 // **                  ADD  1         ##SERF
                 // **                  ENDIF
                 *IN87 = *ON;
                 XQUAN += 1;
                 XFIRST = XYES;
                 WKSERC = ASERC;
                 DSERN = ASERN;
                 WKSERN += 1;
                 XKSERF += 1;
                 *IN85 = *ON;
                 Iter;
               Else;
                 Iter;
               EndIf;
               //
             EndDo;
             //
             If XFIRST = XNO;
               XERROR = XYES;
               P0MSID = 'HSV2024';
               Exsr YSNDMG;
               Clear XKSERC;
               Clear XKSERT;
               Clear XKSERF;
               Leave;
             EndIf;
             //
             If XQUAN <> XKQTYN;
               XERROR = XYES;
               P0MSID = 'HSV2025';
               Exsr YSNDMG;
               Clear XKSERC;
               Clear XKSERT;
               Clear XKSERF;
               // ****                MOVE *ON       *IN52
               Leave;
             EndIf;
             //
             *IN52 = *ON;
             //
             If XJUMP = XYES;
               // **                  MOVE #YES      #ERROR
               P0MSID = 'HSV2026';
               Exsr YSNDMG;
               //
               WKSERN -= 1;
               WKRANG = 'Y';
               //
               // ****                MOVE XXSERC    #KSERC
               // ****                MOVE XXSERN    #KSERF
               // ****      TRCKEY    CHAINHSLTRACB             99
               // ****      *IN99     IFEQ *OFF
               // ****                ADD  1         ##SERF
               // ****                ENDIF
               //
               USERNS = XXSERF;
               USERNE = WKSERN;
               USERCC = WKSERC;
               Open HSLEXPAA;
               Write HSFEXPA;
               Close HSLEXPAA;
               //
               Clear XKSERC;
               Clear XKSERT;
               Clear XKSERF;
               Exsr YJUMP;
               *IN86 = *ON;
               *IN61 = *ON;
               Leave;
             EndIf;
             //
             //  Series code found.
             XKSERC = BSERCC;
             //
             XKSERF = OSERN;
             WKSERN -= 1;
             XKSERT = WKSERN;
             //
             //  Flag successful range allocation.
             WKRANG = 'Y';
             //
             //  Update if required.
           EndIf;
         EndDo;
         //
         //  Unsuccessful range allocation.
         If WKRANG <> 'Y';
           *In41 = '1';
           *In42 = '1';
           *In43 = '1';
           // ***                 MOVE #YES      #ERROR
           P0MSID = 'HSV2012';
           Exsr YSNDMG;
         EndIf;
         //
       EndSr;
       //
       // ----------------------------------------------------------------
       //  Display contents of HSLEXPAA
       // ----------------------------------------------------------------
       //
       BegSr YJUMP;
       EndSr;
       //
       //
       // ----------------------------------------------------------------
       //  Delete contents of HSLEXPAA
       // ----------------------------------------------------------------
       //
       BegSr YDELT;
         Pgm_HSC200A();
       EndSr;
       //
       // ----------------------------------------------------------------
       //  Repeat of Product Code in a single order
       // ----------------------------------------------------------------
       //
       BegSr DOUB;
         //
         //  Get Serial number range.
         WKSERC = *Blanks;
         DoU *IN92 = *ON
           or WKRANG = 'Y';
           //
           Setll (XKPROD : WKSERC) HSLVCTLA;
           ReadE XKPROD HSLVCTLA;
           *IN92 = %Eof;
           If *IN92 = *OFF;
             //
             //  Get available series code.
             XXSERN = BSERNN + TQTYN1;
             //           ##SERN    IFGT BSERNE
             //                     MOVELBSERCN    WKSERC
             Iter;
           Else;
             //
             //  Work back through the subfile searching for duplicate
             //  product code/series code combination records......
             //
             RRNXX = RRNX;
             DoU RRNX = 0;
               RRNX -= 1;
               If RRNX = 0;
                 *IN83 = *ON;
                 Leave;
               EndIf;
               Chain RRNX HSS200C;
               *IN90 = not %Found;
               If XKSERC = BSERCC;
                 RES = XKSERT + TQTYN1;
                 //           RES       IFLE BSERNE
                 KKSERC = XKSERC;
                 KKSERT = XKSERT;
                 Leave;
               Else;
                 *IN84 = *ON;
                 Leave;
               EndIf;
               //                     ENDIF
             EndDo;
             //
             //  Reset the subfile pointer position.....
             RRNX = RRNXX;
             Chain RRNX HSS200C;
             *IN90 = not %Found;
             //
             If *IN84 = *OFF
               and *IN83 = *OFF;
               //  Series code found.
               XKSERC = KKSERC;
               //
               //  Set To serial number for the order.
               XKSERT = RES;
               //
               //  Set From Serial Number for the order.
               XKSERF = KKSERT + 1;
               //
               //  Set Next Serial Number for the voucher control.
               BSERNN += TQTYN1;
               //
               //  Flag successful range allocation.
               WKRANG = 'Y';
               Iter;
             EndIf;
             //
             If *IN83 = *ON
               and *IN84 = *OFF;
               *IN83 = *OFF;
               //
               //  Series code found.
               XKSERC = BSERCC;
               //
               //  Set To serial number for the order.
               XKSERT = BSERNN + TQTYN1;
               XKSERT -= 1;
               //
               //  Set From Serial Number for the order.
               XKSERF = BSERNN;
               //
               //  Set Next Serial Number for the voucher control.
               BSERNN += TQTYN1;
               //
               //  Flag successful range allocation.
               WKRANG = 'Y';
               Iter;
             EndIf;
             *IN84 = *OFF;
             //                     MOVELBSERCN    WKSERC
             //
             //                     ENDIF
           EndIf;
         EndDo;
       EndSr;
       // ----------------------------------------------------------------
       //  Update voucher tracking.
       // ----------------------------------------------------------------
       BegSr UPTRAC;
         //
         //  Reverse order date before update.
         If YMODE = 'E';
           WKD = JDD;
           WKM = JMM;
           WKC = JCC;
           WKY = JYY;
           ADFIS = WKORDD;
         EndIf;
         //
         //  Locate voucher record.
         Setll (XKPROD : XKSERC : XKSERF) HSLTRACB;
         DoU XKPROD <> APROD
           or *IN99 = *ON;
           Read HSLTRACB;
           *IN99 = %Eof;
           If *IN99 = *OFF;
             //
             //  Exit if product code or series code does not match.
             If XKPROD <> APROD
               or XKSERC <> ASERC;
               Leave;
             EndIf;
             //
             //  Update vouchers in the serial number range.
             If XKSERF <= ASERN
               and XKSERT >= ASERN;
               If YMODE = 'D';
                 ACUST = XJCUST;
                 ADSPB = 0;
               Else;
                 //
                 ASORD = XJSORD;
                 ADFIS = WKORDD;
                 Update HSFTRAC;
               EndIf;
             EndIf;
           EndIf;
         EndDo;
       EndSr;
       //
       // ----------------------------------------------------------------
       //  Update inventory allocations.
       // ----------------------------------------------------------------
       BegSr UPINVA;
         //
         //  Update stock quantities.
         Chain (XKPROD : WKSUBC) HSLINVMA;
         *IN99 = not %Found;
         If *IN99 = *OFF;
           //
           //  Increase allocated quantity and reduce available quantity.
           DQTYR += XKQTYN;
           DQTYF -= XKQTYN;
           Update HSFINVM;
         EndIf;
       EndSr;
       //
       // ----------------------------------------------------------------
       //  Update inventory sales.
       // ----------------------------------------------------------------
       BegSr UPINVS;
         //
         //  Update stock quantities.
         Chain (XKPROD : WKSUBC) HSLINVMA;
         *IN99 = not %Found;
         If *IN99 = *OFF;
           //
           //  Reduce allocated quantity and reduce on-hand quantity.
           DQTYR -= XKQTYN;
           DQTYO -= XKQTYN;
           Update HSFINVM;
         EndIf;
       EndSr;
       //
       // ----------------------------------------------------------------
       //  Print delivery note.
       // ----------------------------------------------------------------
       BegSr PRTDEL;
         ORDPRM = %EditC(XJSORD:'X');
         Pgm_HSR230(ORDPRM);
       EndSr;
       //
       // ----------------------------------------------------------------
       //  Print invoice.
       // ----------------------------------------------------------------
       BegSr PRTINV;
         ORDPRM = %EditC(XJSORD:'X');
         Pgm_HSR220(ORDPRM);
       EndSr;
       //
       // ----------------------------------------------------------------
       //  Create order details.
       // ----------------------------------------------------------------
       BegSr UPORDD;
         //
         //  Order line already exists
         UPDAT = ' ';
         If XOLIN > 0;
           Chain (XJSORD : XOLIN) HSLORDDB;
           *IN98 = not %Found;
           KLINE = XOLIN;
         EndIf;
         //
         //  Write fields for order detail record.
         KLTYP = XSELC;
         KCOMP = XJCOMP;
         KTYPE = XJTYPE;
         KSORD = XJSORD;
         KLINE = RRNX;
         KCUST = XJCUST;
         KPROD = XKPROD;
         KSERC = XKSERC;
         KSERNF = XKSERF;
         KELEMF = 0;
         KSERNT = XKSERT;
         KELEMT = 0;
         KQTYN = XKQTYN;
         KCOST = NCOST;
         KPRIC = XKPRIC;
         KVALU = XKVALU;
         //
         //  Obtain VAT% from Order Type record.
         //           #KVALU    MULT .175      KVATA
         //           #KVALU    MULT MVATP     KVATA
         //           KVATA     DIV  100       KVATA     H
         KCRTD = CYMD;
         KCRTT = XXTME;
         %Subst(KDESC:1:19) = XNDESC;
         KCRTS = XJOBNO;
         KCRTU = XUSRID;
         KCRTP = XPGMID;
         KDELT = *Blanks;
         //
         //  Update order detail record.
         If XOLIN > 0;
           If *IN98 = *OFF;
             UPDAT = 'Y';
             Update HSFORDD;
           EndIf;
         Else;
           //
           //  Write order detail record.
           Write HSFORDD;
         EndIf;
       EndSr;
       //
       // ----------------------------------------------------------------
       //  Create order header.
       // ----------------------------------------------------------------
       BegSr UPORDH;
         //
         //  Program mode - Maintenance, or Dispatch
         If YMODE = 'M'
           or YMODE = 'D';
           //
           //  Access order header record.
           Chain XJSORD HSLORDHA;
           *IN99 = not %Found;
         EndIf;
         //
         //  Write fields for order header record.
         JCOMP = XJCOMP;
         JCUST = XJCUST;
         JCUSB = *Blanks;
         JORDR = XJORDR;
         JTYPE = XJTYPE;
         JSORD = XJSORD;
         JORDD = WKORDD;
         JINVN = *Blanks;
         JINVD = 0;
         JDELN = *Blanks;
         JDELD = 0;
         JCRTD = CYMD;
         JCRTT = XXTME;
         JCRTS = XJOBNO;
         JCRTU = XUSRID;
         JCRTP = XPGMID;
         JDELT = *Blanks;
         //
         //  Program mode - Entry
         If YMODE = 'E';
           //
           //  Write order header record.
           JSTAT = 'A';
           Write HSFORDH;
         EndIf;
         //
         //  Program mode - Maintenance, or Dispatch
         If YMODE = 'M'
           or YMODE = 'D';
           If YMODE = 'D';
             JDELD = *DATE;
             JSTAT = 'D';
           EndIf;
           //
           //  Update order header record.
           If *IN99 = *OFF;
             Update HSFORDH;
           EndIf;
         EndIf;
       EndSr;
       //
       // ----------------------------------------------------------------
       //  Create order delivery details.
       BegSr UPODEL;
         //
         //  Program mode - Maintenance, or Dispatch
         If YMODE = 'M'
           or YMODE = 'D';
           //
           Chain XJSORD HSFODEL;
           *IN99 = not %Found;
         EndIf;
         //
         //  Write fields for order delivery detail record.
         VCOMP = XJCOMP;
         VTYPE = XJTYPE;
         VSORD = XJSORD;
         VNAM1 = XVNAM1;
         VADR1 = XVADR1;
         VADR2 = XVADR2;
         VADR3 = XVADR3;
         VPCDE = XVPCDE;
         VDIN1 = XVDIN1;
         VDIN2 = XVDIN2;
         VDIN3 = XVDIN3;
         VDELT = *Blanks;
         //
         //  Program mode - Entry
         If YMODE = 'E';
           //
           //  Write order delivery details record.
           Write HSFODEL;
         EndIf;
         //
         //  Program mode - Maintenance, or Dispatch
         If YMODE = 'M'
           or YMODE = 'D';
           //
           //  Update order delivery details record.
           If *IN99 = *OFF;
             Update HSFODEL;
           EndIf;
         EndIf;
         //
       EndSr;
       // ----------------------------------------------------------------
       //  Create inventory sales transaction.
       // ----------------------------------------------------------------
       BegSr UPINVT;
         //
         //  Write fields for inventory sales transaction record.
         CCOMP = XJCOMP;
         CWHSE = XJWHSE;
         CPROD = XKPROD;
         CSUBCF = *Blanks;
         CSUBCT = *Blanks;
         CVTYP = *Blanks;
         CSERC = XKSERC;
         CSERNF = XKSERF;
         CELEMF = 0;
         CSERNT = XKSERT;
         CELEMT = 0;
         CQTYN = XKQTYN;
         CBATC = 0;
         CDELN = %EditC(XJSORD:'X');
         CREAS = *Blanks;
         CTDAT = CYMD;
         CTYPE = MTTYP;
         CSUPP = *Blanks;
         CCUST = XJCUST;
         CAREA = *Blanks;
         CAISL = *Blanks;
         CBAYR = *Blanks;
         CLEVL = *Blanks;
         CINVN = *Blanks;
         CCRTD = CYMD;
         CCRTT = XXTME;
         CCRTS = XJOBNO;
         CCRTU = XUSRID;
         CCRTP = XPGMID;
         CDELT = *Blanks;
         //
         //  Write inventory sales transaction record.
         Write HSFINVT;
       EndSr;
       //
       // ----------------------------------------------------------------
       //  VALIDH - Validate header screen.
       // ----------------------------------------------------------------
       //
       BegSr VALIDH;
         //
         //  Screen Header validation:
         //
         //  Reset work values.
         Reset XERROR;
         *IN61 = *OFF;                                                          // Hide F10/F11
         //
         //  Reset error indicators.
         pAToArrStr = %Addr(*IN(30));
         %Subst(AToArrStr:1:19) = XZEROS;
         //
         //  Validate that Order Type is a valid Type.
         Chain XJTYPE HSLORDPA;
         *IN99 = not %Found;
         If *IN99 = *ON;
           *IN30 = *ON;
           XERROR = XYES;
           P0MSID = 'HSV2001';
           Exsr YSNDMG;
         EndIf;
         //
         //  Validate that Company is valid.
         If XJCOMP <> *BLANKS;
           Chain XJCOMP HSLCOMPA;
           *IN99 = not %Found;
           If *IN99 = *ON;
             *IN31 = *ON;
             XERROR = XYES;
             P0MSID = 'HSV2002';
             Exsr YSNDMG;
           EndIf;
         EndIf;
         //
         //  Validate that Warehouse is valid.
         If XJWHSE <> *BLANKS;
           Chain XJWHSE HSLWHSEA;
           *IN99 = not %Found;
           If *IN99 = *ON;
             *IN32 = *ON;
             XERROR = XYES;
             P0MSID = 'HSV0006';
             Exsr YSNDMG;
           EndIf;
         EndIf;
         //
         //  Validate Post Code
         If XJCUST = *BLANKS;
           If XJPCDE <> *BLANKS;
             Chain XJPCDE HSLCUSTB;
             *IN99 = not %Found;
             If *IN99 = *ON;
               *IN33 = *ON;
               XERROR = XYES;
               P0MSID = 'HSV2003';
               Exsr YSNDMG;
             EndIf;
             //
             //  If Customer is found
             If *IN99 = *OFF;
               //  ...load Customer record into screen fields.
               XJNAM1 = ENAM1;                                                  // Name 1
               XJNAM2 = ENAM2;                                                  // Name 2
               XJADR1 = EADR1;                                                  // Address 1
               XJADR2 = EADR2;                                                  // Address 2
               XJCUST = ECUST;                                                  // Cust No.
               XJCLIM = ECLIM;                                                  // Credit Limit
               XJTYSL = ETYSL;                                                  // Last Year Sales
             EndIf;
             //
           EndIf;
         EndIf;
         //
         //  Validate Customer Number
         //           #JCUST    IFNE *BLANKS
         Chain XJCUST HSLCUSTA;
         *IN99 = not %Found;
         If *IN99 = *ON;
           *IN34 = *ON;
           XERROR = XYES;
           P0MSID = 'HSV2004';
           Exsr YSNDMG;
         EndIf;
         //
         //  If Customer is found
         If *IN99 = *OFF;
           //  ...load Customer record into screen fields.
           XJNAM1 = ENAM1;                                                      // Name 1
           XJNAM2 = ENAM2;                                                      // Name 2
           XJADR1 = EADR1;                                                      // Address 1
           XJADR2 = EADR2;                                                      // Address 2
           XJPCDE = EPCDE;                                                      // Post Code
           XJCLIM = ECLIM;                                                      // Credit Limit
           XJTYSL = ETYSL;                                                      // Last Year Sales
           //
           //  Check customer's credit rating allows order entry......
           If ECRAT = '01 ';
             *IN34 = *ON;
             XERROR = XYES;
             P0MSID = 'HSV2018';
             Exsr YSNDMG;
           EndIf;
           //
         EndIf;
         //                     ENDIF
         //
         //  Maximum discount is 40%
         If XJDISP > 40;
           *IN36 = *ON;
           XERROR = XYES;
           P0MSID = 'HSV0142';
           Exsr YSNDMG;
         EndIf;
         //
         //  Check order date
         If XJORDD = 0;
           Exsr YDATEC;
         Else;
           //
           //  a) Check for leap year and adjust number of days in February
           //     in Days in Month array.
           REM = JYY / 4;
           If REM = 0;
             LEAP = 'Y';
           Else;
             LEAP = ' ';
           EndIf;
           Select;
             When LEAP = ' ';
               DIM(2) = 28;
             When LEAP = 'Y';
               DIM(2) = 29;
           EndSl;
           //  Day
           If JDD > 31
             or JDD < 01
             //  Month
             or JMM > 12
             or JMM < 01;
             //
             //  If date error found...
             *IN35 = *ON;
             XERROR = XYES;
             P0MSID = 'HSV2005';
             Exsr YSNDMG;
           EndIf;
           //
           //  Validate Day within month.
           M = JMM;
           If JDD < 1
             or JMM >= 1
             and JMM <= 12
             and JDD > DIM(M);
             //
             //  If date error found...
             *IN35 = *ON;
             XERROR = XYES;
             P0MSID = 'HSV2005';
             Exsr YSNDMG;
           EndIf;
         EndIf;
         //
         //  If no errors found...
         If XERROR = XNO;
           //  ...display "Data valid.Press F10 to update" message...
           P0MSID = 'HSV9006';
           Exsr YSNDMG;
           //  ...activate F10 key (*IN61=*ON).
           *IN61 = *ON;
         EndIf;
         //
       EndSr;
       //
       // ----------------------------------------------------------------
       //  @DATEC - Date Check Routine
       // ----------------------------------------------------------------
       //
       BegSr YDATEC;
         //
         //  Convert six-digit system date to eight digits, with century.
         DD = UDAY;
         MM = UMONTH;
         YY = UYEAR;
         If YY > 80;
           CC = 19;
         Else;
           CC = 20;
         EndIf;
         CCYY = %Dec(%EditC(CC:'X')
                 + %Subst(%EditC(CCYY:'X'):3:2)
                 :4:0);
         CCYY = %Dec(%Subst(%EditC(CCYY:'X'):1:2)
                + %EditC(YY:'X')
                   :4:0);
         //
         //  Set default date for order entry.
         If XJORDD = 0;
           XJORDD = DMCY;
         EndIf;
         //
         //  Set date for transaction audit field.
         DDD = DD;
         MMM = MM;
         CCC = CC;
         YYY = YY;
       EndSr;
       //
       // ----------------------------------------------------------------
       //  @SERCH - F4 prompt control.
       // ----------------------------------------------------------------
       //
       BegSr YSERCH;
         //
         //  Check that cursor is in a valid field for prompting.
         If CSRRCD = 'HSS200A'
           and CSRFLD = 'XJCUST'
           or CSRRCD = 'HSS200A'
           and CSRFLD = 'XJTYPE'
           or CSRRCD = 'HSS200A'
           and CSRFLD = 'XJCOMP'
           or CSRRCD = 'HSS200A'
           and CSRFLD = 'XJWHSE'
           or CSRRCD = 'HSS200A'
           and CSRFLD = 'XJPCDE'
           or CSRRCD = 'HSS200C'
           and CSRFLD = 'XKPROD';
           //
           //  Move appropriate data to parameter fields depending upon search
           //  field chosen.
           Select;
               //
               //  Customer No.
             When CSRFLD = 'XJCUST';
               FILEY = 'HSLCUSTA';
               NAMEY = 'ENAM1';
               NUMBRY = 'ECUST';
               //
               //  Post Code
             When CSRFLD = 'XJPCDE';
               FILEY = 'HSLCUSTB';
               NAMEY = 'ENAM1';
               NUMBRY = 'EPCDE';
               //
               //  Order Type.
             When CSRFLD = 'XJTYPE';
               FILEY = 'HSLORDPA';
               NAMEY = 'MODES';
               NUMBRY = 'MOTYP';
               //
               //  Company Code
             When CSRFLD = 'XJCOMP';
               FILEY = 'HSLCOMPA';
               NAMEY = 'WODES';
               NUMBRY = 'WCOMP';
               //
               //  Warehouse Code
             When CSRFLD = 'XJWHSE';
               FILEY = 'HSLWHSEA';
               NAMEY = 'PODES';
               NUMBRY = 'PWHSE';
               //
               //  Product Code
             When CSRFLD = 'XKPROD';
               FILEY = 'HSLPRODA';
               NAMEY = 'NDESC';
               NUMBRY = 'NPROD';
               //
           EndSl;
           //
           //  Call system window program to retrieve data.  If Return Code is
           //  *OFF then load retrieved data into screen fields.
           LINY = 10;
           COLY = 8;
           Pgm_HSR341(YRTNC : YNUMBR : FILEY : NAMEY : NUMBRY : LINY :
                 COLY);
           //
           //  If Return Code is *OFF then load retrieved data into
           //  appropriate screen fields.
           If YRTNC = *OFF;                                                     // ..If2
             //
             Select;
                 //
                 //  Customer No.
               When CSRFLD = 'XJCUST';
                 XJCUST = YNUMBR;
                 //
                 //  Post Code
               When CSRFLD = 'XJPCDE';
                 XJPCDE = YNUMBR;
                 //
                 //  Customer Type.
               When CSRFLD = 'XJTYPE';
                 XJTYPE = YNUMBR;
                 //
                 //  Company Code
               When CSRFLD = 'XJCOMP';
                 XJCOMP = YNUMBR;
                 //
                 //  Warehouse Code
               When CSRFLD = 'XJWHSE';
                 XJWHSE = YNUMBR;
                 //
                 //  Product Code
               When CSRFLD = 'XKPROD';
                 XKPROD = YNUMBR;
                 //
             EndSl;
           EndIf;                                                               // ..EndIf2
           //
           //  otherwise cursor is in wrong format/field so display message.
         Else;                                                                  // .ElseIf1
           P0MSID = 'HSV9009';
           Exsr YSNDMG;
         EndIf;                                                                 // .EndIf1
         //
       EndSr;
       //
       // ----------------------------------------------------------------
       //  @SERSF - F4 prompt control for order detail subfile.
       // ----------------------------------------------------------------
       //
       BegSr YSERSF;
         //  Clear row / column control.
         CSRROW = 0;
         CSRCOL = 0;
         //
         //  Retrieve the current cursor position.
         ROW = CSRLOC / 256;
         COL = %Rem(CSRLOC : 256);
         CSRROW = ROW;
         CSRCOL = COL;
         //
         //  Execute search.
         Exsr YSERCH;
         //
         //  Flag subfile record to return value.
         Chain CSRRRN HSS200C;
         *IN99 = not %Found;
         If *IN99 = *OFF;
           *IN37 = *ON;
           If YNUMBR <> *BLANKS;
             XKPROD = YNUMBR;
           EndIf;
           Update HSS200C;
           *IN37 = *OFF;
         EndIf;
       EndSr;
       //
       // ----------------------------------------------------------------
       //  @SNDMG - Send message to this program's MSGQ.
       // ----------------------------------------------------------------
       //
       BegSr YSNDMG;
         //
         Pgm_SNDMSGC(P0PGMQ : P0PGRL : P0MSID : P0MSGF : P0MSDA :
               P0MSTP);
         //
       EndSr;
       //
       // ----------------------------------------------------------------
       //  @CLRMG - Clear message(s) from this program's MSGQ.
       // ----------------------------------------------------------------
       //
       BegSr YCLRMG;
         //
         Pgm_RMVMSGC();
         //
       EndSr;
       //
       // ----------------------------------------------------------------
       //  *UPPVCT- Update the voucher control
       // ----------------------------------------------------------------
       //
       BegSr UPPVCT;
         RRNQ = 1;
         Chain RRNQ HSS200C;
         *IN97 = not %Found;
         DoW *IN97 = *OFF;
           //
           RRNQ += 1;
           If XSELC <> *BLANK
             or XKPROD <> *BLANK
             or XNDESC <> *BLANK
             or XKPRIC <> *ZEROS
             or XKQTYN <> *ZEROS
             or XKVALU <> *ZEROS
             or XKVATA <> *ZEROS
             or XKSERC <> *BLANKS
             or XKSERF <> *ZEROS
             or XKSERT <> *ZEROS;
             WKSERC = XKSERC;
             TEMP = XKSERT;
             TEMP += 1;
             Setll (XKPROD : WKSERC) HSLVCTLA;
             ReadE XKPROD HSLVCTLA;
             *IN92 = %Eof;
             BSERNN = TEMP;
             Update HSFVCTL;
           EndIf;
           //
           If RRNQ = 100;
             Leave;
           EndIf;
           Chain RRNQ HSS200C;
           *IN97 = not %Found;
         EndDo;
       EndSr;
       //
       // ----------------------------------------------------------------
       //  *INZSR - Initialization.
       // ----------------------------------------------------------------
       //
       BegSr *INZSR;
         //
         //  Dummy read to open file.
         Read HSPINVT;
         *IN99 = %Eof;
         Update HSFINVT;
         Open HSLEXPAA;
         Read HSLEXPAA;
         *IN99 = %Eof;
         Close HSLEXPAA;
         //
         //  Key lists.
         //  Key List for Voucher Control.
         //  Key List for Voucher Cross-Reference.
         //  Key List for Sales Order.
         //  Key List for Inventory Masterfile.
         WKSUBC = *Blanks;
         //
         //  Key List for Voucher Tracking.
         //  Key List for Customer Discount File.
         //  Parameter Lists.
         //  Entry parameter list.
         //
         //  List for Call to HSR341 - Search program.
         //  Field definitions
         //  Variable declarations.
         XNO = '0';
         XYES = '1';
         XERROR = XNO;
         XUPDAT = XNO;
         XDELET = XNO;
         ORDPRM = *Blanks;
         RCDNBR = 1;
         RRNX = 0;
         RRNVAL = 0;
         RRNLIN = 0;
         RRNQ = 0;
         RRNP = 0;
         //
         //  Prepare message subfile.
         *IN79 = *ON;
         P0PGMQ = XPGMID;                                                       // Program queu
         %Subst(P0PGRL:1:4) = '*PRV';                                           // Rel queue
         P0MSID = *Blanks;                                                      // Message ID
         %Subst(P0MSGF:1:6) = 'HSM001';                                         // Message file
         P0MSDA = *Blanks;                                                      // Message data
         %Subst(P0MSTP:1:5) = '*INFO';                                          // Message type
         //
         Exsr YDATEC;
         //
       EndSr;
** Days in Month array DIM.
312831303130313130313031

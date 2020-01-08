      SUBROUTINE ST_ETOA  ( barray, nchar, outstr, iret )
C************************************************************************
C* ST_ETOA								*
C*									*
C* This subroutine converts a character string in EBCDIC to a string	*
C* in ASCII.								*
C*									*
C* ST_ETOA  ( BARRAY, NCHAR, OUTSTR, IRET )				*
C*									*
C* Input parameters:							*
C*	BARRAY (NCHAR)	CHAR*		EBCDIC characters		*
C*	NCHAR		INTEGER		Number of characters		*
C*									*
C* Output parameters:							*
C*	OUTSTR		CHAR*		ASCII string			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* J. Fulson-Woytek/932	 4/88						*
C* M. desJardins/GSFC	12/90						*
C* D. Kidwell/NCEP	07/96	Converted to Cray                       *
C************************************************************************
	CHARACTER*(*)	outstr
	CHARACTER*(*)	barray (*)
C*
	CHARACTER * 1   chr
	CHARACTER * 8   chr8
C*
	EQUIVALENCE 	( chr, chr8 ( 8:8 ) ) 
	EQUIVALENCE	( chr8, ichr )
	INTEGER		itable (256)
C*
	DATA   ITABLE  /
     1	  0,001,002,003,  0,009,  0,127,  0,  0,  0,011,012,013,014,015,
     2	016,017,018,019,  0,  0,008,  0,024,025,  0,  0,028,029,030,031,
     3	  0,  0,028,  0,  0,010,023,027,  0,  0,  0,  0,  0,005,006,007,
     4	  0,  0,022,  0,  0,030,  0,004,  0,  0,  0,019,020,021,  0,026,
     5	032,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,046,060,040,043,124,
     6	038,  0,  0,  0,  0,  0,  0,  0,  0,  0,033,036,042,041,059,094,
     7	045,047,  0,  0,  0,  0,  0,  0,  0,  0,  0,044,037,095,062,063,
     8	  0,  0,  0,  0,  0,  0,  0,  0,  0,096,058,035,064,039,061,034,
     9	  0,097,098,099,100,101,102,103,104,105,  0,  0,  0,  0,  0,  0,
     A	  0,106,107,108,109,110,111,112,113,114,  0,  0,  0,  0,  0,  0,
     B	  0,126,115,116,117,118,119,120,121,122,  0,  0,  0,091,  0,  0,
     C	  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,093,  0,  0,
     D	123,065,066,067,068,069,070,071,072,073,  0,  0,  0,  0,  0,  0,
     E	125,074,075,076,077,078,079,080,081,082,  0,  0,  0,  0,  0,  0,
     F	092,  0,083,084,085,086,087,088,089,090,  0,  0,  0,  0,  0,  0,
     G	048,049,050,051,052,053,054,055,056,057,124,  0,  0,  0,  0,  0/
C------------------------------------------------------------------------
	iret = 0
C
C*	Convert one character at a time.
C
	DO  i = 1, nchar
	    ichr = 0
	    chr = barray ( i )
	    n = ichr
	    IF  ( n .lt. 0 )  n = n + 256
	    m = itable (n + 1)
	    outstr (i:i) = CHAR ( m )
	END DO
C*
	RETURN
	END

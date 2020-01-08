	FUNCTION PR_VWND  ( sped, drct )
C************************************************************************
C* PR_VWND 								*
C*									*
C* This function computes VWND from SPED and DRCT or VKNT from SKNT	*
C* and DRCT.  The following equation is used:				*
C*									*
C*                VWND = -COS ( DRCT ) * SPED				*
C*									*
C* PR_VWND  ( SPED, DRCT )						*
C*									*
C* Input parameters:							*
C*	SPED		REAL		Wind speed			*
C*	DRCT		REAL		Wind direction in degrees	*
C*									*
C* Output parameters:							*
C*	PR_VWND		REAL		V component			*
C**									*
C* Log:									*
C* P. Kocin/914		1980	Original source code			*
C* I. Graffman/RDS	12/87	GEMPAK4					*
C* G. Huffman/GSC	 8/88	Modified documentation			*
C* D. Kidwell/NCEP      10/96   Ported to Cray                          *
C************************************************************************
C* GEMPRM.PRM
C*
C* This include file contains parameter definitions for the GEMPAK
C* software routines in the ST_ and PR_ libraries.
C*
C* CRAY version
C**
C* Log:  
C*	Kidwell/NCEP	07/96	Adapted a subset of gemprm.prm for Cray
C************************************************************************
C!
C!	Missing data definitions
C!
 	PARAMETER	( RMISSD = -9999.0 )
C!						Missing data value
	PARAMETER	( RDIFFD =  0.1    )
C!						Missing value fuzziness
	PARAMETER	( IMISSD = -9999   )
C!						Missing integer value
	LOGICAL		  ERMISS
C!						Declare for stmt func
C!
C! 	Physical and mathematical constants
C!
	PARAMETER       ( PI = 3.14159265  )
C!                                              PI
	PARAMETER       ( DTR = PI / 180.  )
	PARAMETER       ( RTD = 180. / PI  )
C!                                              Degrees <--> Radians
	PARAMETER	( GRAVTY = 9.80616  )
C!						Acceleration of gravity
	PARAMETER	( RDGAS  = 287.04   )
	PARAMETER	( RKAP   = RDGAS / GRAVTY )
C!						Gas constant of dry air
	PARAMETER	( RKAPPA = 2. / 7. )
C!						Poisson constant
	PARAMETER	( GAMUSD = 6.5 )
C!						US std atmos lapse rate
	PARAMETER	( TMCK   = 273.15 )
C!						Centigrade -> Kelvin
C!
C!	ASCII character constants 
C!
C!	Since the Cray does not allow the use of a function (e.g.,
C!	CHAR) to define a parameter, nor does it allow a character
C!	to be defined directly as a hex (X) value, the convolutions
C!      below are necessary to define the character values for Cray.
C!
C
	CHARACTER * 1 chnull, chtab, chspac, chtlda
C
	CHARACTER * 8 c8null, c8tab, c8spac, c8tlda
C
	INTEGER       iigemc ( 4 )
C
	EQUIVALENCE ( chnull, c8null (8:8) ), ( chtab , c8tab  (8:8) ),
     +              ( chspac, c8spac (8:8) ), ( chtlda, c8tlda (8:8) ) 
C
	EQUIVALENCE ( iigemc ( 1), c8null ), ( iigemc ( 2), c8tab  ),
     +              ( iigemc ( 3), c8spac ), ( iigemc ( 4), c8tlda )
C
	DATA iigemc / X'00',    X'09',    X'20',    X'7E' /
C                     Null      Tab       Space     Tilda
C!
C
       ERMISS (xxxx) = ( ABS ( xxxx - RMISSD ) .lt. RDIFFD )
C------------------------------------------------------------------------
	IF  ( ERMISS ( sped ) .or. ERMISS ( drct ) )  THEN
	    PR_VWND = RMISSD
	  ELSE
	    PR_VWND = -COS ( drct * DTR ) * sped
	END IF
C*
	RETURN
	END

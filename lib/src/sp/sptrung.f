C-----------------------------------------------------------------------
      SUBROUTINE SPTRUNG(IROMB,MAXWV,IDRTI,IMAXI,JMAXI,KMAX,NMAX,
     &                   IPRIME,ISKIPI,JSKIPI,KSKIPI,KGSKIP,
     &                   NRSKIP,NGSKIP,JCPU,RLAT,RLON,GRIDI,GP)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  SPTRUNG    SPECTRALLY INTERPOLATE SCALARS TO STATIONS
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-02-29
C
C ABSTRACT: THIS SUBPROGRAM SPECTRALLY TRUNCATES SCALAR FIELDS
C           ON A GLOBAL CYLINDRICAL GRID, RETURNING THE FIELDS
C           TO SPECIFIED SETS OF STATION POINTS ON THE GLOBE.
C           THE WAVE-SPACE CAN BE EITHER TRIANGULAR OR RHOMBOIDAL.
C           THE GRID-SPACE CAN BE EITHER AN EQUALLY-SPACED GRID
C           (WITH OR WITHOUT POLE POINTS) OR A GAUSSIAN GRID.
C           THE GRID AND POINT FIELDS MAY HAVE GENERAL INDEXING.
C           THE TRANSFORMS ARE ALL MULTIPROCESSED.
C           TRANSFORM SEVERAL FIELDS AT A TIME TO IMPROVE VECTORIZATION.
C           SUBPROGRAM CAN BE CALLED FROM A MULTIPROCESSING ENVIRONMENT.
C
C PROGRAM HISTORY LOG:
C   96-02-29  IREDELL
C
C USAGE:    CALL SPTRUNG(IROMB,MAXWV,IDRTI,IMAXI,JMAXI,KMAX,NMAX,
C    &                   IPRIME,ISKIPI,JSKIPI,KSKIPI,KGSKIP,
C    &                   NRSKIP,NGSKIP,JCPU,RLAT,RLON,GRIDI,GP)
C   INPUT ARGUMENTS:
C     IROMB    - INTEGER SPECTRAL DOMAIN SHAPE
C                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C     MAXWV    - INTEGER SPECTRAL TRUNCATION
C     IDRTI    - INTEGER INPUT GRID IDENTIFIER
C                (IDRTI=4 FOR GAUSSIAN GRID,
C                 IDRTI=0 FOR EQUALLY-SPACED GRID INCLUDING POLES,
C                 IDRTI=256 FOR EQUALLY-SPACED GRID EXCLUDING POLES)
C     IMAXI    - INTEGER EVEN NUMBER OF INPUT LONGITUDES.
C     JMAXI    - INTEGER NUMBER OF INPUT LATITUDES.
C     KMAX     - INTEGER NUMBER OF FIELDS TO TRANSFORM.
C     NMAX     - INTEGER NUMBER OF STATION POINTS TO RETURN
C     IPRIME   - INTEGER INPUT LONGITUDE INDEX FOR THE PRIME MERIDIAN.
C                (DEFAULTS TO 1 IF IPRIME=0)
C                (OUTPUT LONGITUDE INDEX FOR PRIME MERIDIAN ASSUMED 1.)
C     ISKIPI   - INTEGER SKIP NUMBER BETWEEN INPUT LONGITUDES
C                (DEFAULTS TO 1 IF ISKIPI=0)
C     JSKIPI   - INTEGER SKIP NUMBER BETWEEN INPUT LATITUDES FROM SOUTH
C                (DEFAULTS TO -IMAXI IF JSKIPI=0)
C     KSKIPI   - INTEGER SKIP NUMBER BETWEEN INPUT GRID FIELDS
C                (DEFAULTS TO IMAXI*JMAXI IF KSKIPI=0)
C     KGSKIP   - INTEGER SKIP NUMBER BETWEEN STATION POINT SETS
C                (DEFAULTS TO NMAX IF KGSKIP=0)
C     NRSKIP   - INTEGER SKIP NUMBER BETWEEN STATION LATS AND LONS
C                (DEFAULTS TO 1 IF NRSKIP=0)
C     NGSKIP   - INTEGER SKIP NUMBER BETWEEN STATION POINTS
C                (DEFAULTS TO 1 IF NGSKIP=0)
C     RLAT     - REAL (*) STATION LATITUDES IN DEGREES
C     RLON     - REAL (*) STATION LONGITUDES IN DEGREES
C     JCPU     - INTEGER NUMBER OF CPUS OVER WHICH TO MULTIPROCESS
C                (DEFAULTS TO ENVIRONMENT NCPUS IF JCPU=0)
C     GRIDI    - REAL (*) INPUT GRID FIELDS
C   OUTPUT ARGUMENTS:
C     GP       - REAL (*) STATION POINT SETS
C
C SUBPROGRAMS CALLED:
C   SPTRAN       PERFORM A SCALAR SPHERICAL TRANSFORM
C   SPTGPT       TRANSFORM SPECTRAL SCALAR TO STATION POINTS
C   NCPUS        GETS ENVIRONMENT NUMBER OF CPUS
C
C REMARKS: MINIMUM GRID DIMENSIONS FOR UNALIASED TRANSFORMS TO SPECTRAL:
C   DIMENSION                    LINEAR              QUADRATIC
C   -----------------------      ---------           -------------
C   IMAX                         2*MAXWV+2           3*MAXWV/2*2+2
C   JMAX (IDRT=4,IROMB=0)        1*MAXWV+1           3*MAXWV/2+1
C   JMAX (IDRT=4,IROMB=1)        2*MAXWV+1           5*MAXWV/2+1
C   JMAX (IDRT=0,IROMB=0)        2*MAXWV+3           3*MAXWV/2*2+3
C   JMAX (IDRT=0,IROMB=1)        4*MAXWV+3           5*MAXWV/2*2+3
C   JMAX (IDRT=256,IROMB=0)      2*MAXWV+1           3*MAXWV/2*2+1
C   JMAX (IDRT=256,IROMB=1)      4*MAXWV+1           5*MAXWV/2*2+1
C   -----------------------      ---------           -------------
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C
C$$$
      REAL RLAT(*),RLON(*),GRIDI(*),GP(*)
      REAL W((MAXWV+1)*((IROMB+1)*MAXWV+2)/2*2+1,KMAX)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  TRANSFORM INPUT GRID TO WAVE
      JC=JCPU
      IF(JC.EQ.0) JC=NCPUS()
      MX=(MAXWV+1)*((IROMB+1)*MAXWV+2)/2
      MDIM=2*MX+1
      JN=-JSKIPI
      IF(JN.EQ.0) JN=IMAXI
      JS=-JN
      INP=(JMAXI-1)*MAX(0,-JN)+1
      ISP=(JMAXI-1)*MAX(0,-JS)+1
      CALL SPTRAN(IROMB,MAXWV,IDRTI,IMAXI,JMAXI,KMAX,
     &            IPRIME,ISKIPI,JN,JS,MDIM,KSKIPI,0,0,JC,
     &            W,GRIDI(INP),GRIDI(ISP),-1)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  TRANSFORM WAVE TO OUTPUT
      CALL SPTGPT(IROMB,MAXWV,KMAX,NMAX,MDIM,KGSKIP,NRSKIP,NGSKIP,
     &            RLAT,RLON,W,GP)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END

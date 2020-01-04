data2gfs
*********
Program for creating the initial value file (sigma restart file) of NCEP-GFS

Overview
========
Directory structure
-------------------
 ./exec executable file 
 ./src program source 
 ./exp executable shell script

Effective shell script
---------------------
 ===========  ==============  ======= ========
 name         vertical level  format  user id
 ===========  ==============  ======= ========
 YOTC         hybrid level    grib1   userid=1
 GANAL        pressure level  grads   userid=2
 JRA25        pressure level  grib1   userid=3
 ERA-interim  hybrid level    grib1   userid=4
 ERA-interim  pressure level  grib1   userid=5
 ALERA2       sigma level     grads   userid=6
 ===========  ==============  ======= ========

Supported formats
-----------------
 grib1, grib2, grads format

Compile
===========
NCEP libraries (w3lib, bacio, g2lib, iplib, splib) are required.

- http://www.nco.ncep.noaa.gov/pmb/docs/libs/
- http://www.nco.ncep.noaa.gov/pmb/codes/GRIB2/


Change log
==========
 2013.08.07 initial version 

Author Information
===================
tmiyachi 
manmeet3591 - translated to english

- https://bitbucket.org/tmiyachi
- https://github.com/tmiyachi


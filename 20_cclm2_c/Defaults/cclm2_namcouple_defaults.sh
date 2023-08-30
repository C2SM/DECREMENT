#!/bin/bash

export COUPLING_TIMESTEP=1800 # should be the same as CLM timestep

# EXPORTED is default for sending/receiving
# set to EXPOUT to produce 2 netcdf files for the coupling field for values sent and received after interpolation

# ---------------------------------------------------------------
#                            namcouple
# ---------------------------------------------------------------
cclm2_c_namcouple(){
  cat > namcouple << EONL
# This is a typical input file for OASIS 3.0, using netCDF format
# for restart input files.  Oasis reads in
# this file at run-time. Don't hesitate to ask precisions or make
# suggestions (oasishelp@cerfacs.fr). This file can be used AS IT IS
# to run the CLIM toy model (toyclim).
#
# Any line beginning with # is ignored. Blank lines are not allowed.
#
#########################################################################
 \$NFIELDS
# This is the total number of fields being exchanged. 
# For the definition of the fields, see under \$STRINGS keyword
#
       17
 \$END
##########################################################################
 \$NBMODEL
# This gives you the number of models running in this experiment +
# their names (6 characters) + , in option, the maximum Fortran unit
# number used by each model; 1024 will be used if none are given.
#
  2  cosmoc   clmxxx   99  99
 \$END
###########################################################################
 \$RUNTIME
# This gives you the total simulated time for this run in seconds (here 6 days)
#
  ${LM_NL_RUNTIME_S}
 \$END
###########################################################################
 \$NLOGPRT
# Index of printing level in output file cplout: 0 = no printing
#  1 = main routines and field names when treated, 2 = complete output
  ${OASIS_PRINTING}
 \$END
###########################################################################
 \$STRINGS
#
# The above variables are the general parameters for the experiment.
# Everything below has to do with the fields being exchanged.
#
############################################################################
#                      ATMOSPHERE  --->>>  SOIL
#                      --------------------
############################################################################
#
# --- start Field 1 : Surface temperature
# 
COSTEMPE  CLMTEMPE 1  ${COUPLING_TIMESTEP}  2  cosmo.nc  ${OASIS_OUT}
 ${LM_NL_IELM_C} ${LM_NL_JELM_C} ${CLM_nx} ${CLM_ny} coap  clme  LAG=+0 SEQ=1
R  0  R  0
#
LOCTRANS   SCRIPR 
#
 AVERAGE
 BILINEAR LR SCALAR LATLON 1
# DISTWGT LR SCALAR LATLON 10 4
# 
# --- end field 1 ---
############################################################################
#
# --- start Field 2 : eastward_wind
# 
COSUWIND CLMUWIND 182  ${COUPLING_TIMESTEP}  2  cosmo.nc  EXPORTED
 ${LM_NL_IELM_C} ${LM_NL_JELM_C} ${CLM_nx} ${CLM_ny} coap  clme  LAG=+0 SEQ=1
R  0  R  0
#
LOCTRANS   SCRIPR 
#
 AVERAGE
 BILINEAR LR SCALAR LATLON 1
# DISTWGT LR SCALAR LATLON 10 4
# --- end field 2 ---
############################################################################
#
# --- start Field 3 : northward_wind
# 
COSVWIND CLMVWIND 262  ${COUPLING_TIMESTEP}  2  cosmo.nc  EXPORTED
 ${LM_NL_IELM_C} ${LM_NL_JELM_C} ${CLM_nx} ${CLM_ny} coap  clme  LAG=+0 SEQ=1
R  0  R  0
#
LOCTRANS   SCRIPR 
#
 AVERAGE
 BILINEAR LR SCALAR LATLON 1
# DISTWGT LR SCALAR LATLON 10 4
# 
# --- end field 3 ---
############################################################################
#
# --- start Field 4 : specific water vapor content
# 
COSSPWAT CLMSPWAT 456  ${COUPLING_TIMESTEP}  2  cosmo.nc  EXPORTED
 ${LM_NL_IELM_C} ${LM_NL_JELM_C} ${CLM_nx} ${CLM_ny} coap  clme  LAG=+0 SEQ=1
R  0  R  0
#
LOCTRANS   SCRIPR 
#
 AVERAGE
 BILINEAR LR SCALAR LATLON 1
# DISTWGT LR SCALAR LATLON 10 4
# 
# --- end field 4 ---
############################################################################
#
# --- start Field 5 : thickness of lowest level (m)
# 
COSTHICK CLMTHICK 116  ${COUPLING_TIMESTEP}  2  cosmo.nc  EXPORTED
 ${LM_NL_IELM_C} ${LM_NL_JELM_C} ${CLM_nx} ${CLM_ny} coap  clme  LAG=+0 SEQ=1
R  0  R  0
#
LOCTRANS   SCRIPR 
#
 AVERAGE
 BILINEAR LR SCALAR LATLON 1
# DISTWGT LR SCALAR LATLON 10 4
# 
# --- end field 5 ---
############################################################################
#
# --- start Field 6 : surface pressure (Pa)
# 
COSPRESS CLMPRESS 348  ${COUPLING_TIMESTEP}  2  cosmo.nc  EXPORTED
 ${LM_NL_IELM_C} ${LM_NL_JELM_C} ${CLM_nx} ${CLM_ny} coap  clme  LAG=+0 SEQ=1
R  0  R  0
#
LOCTRANS   SCRIPR 
#
 AVERAGE
 BILINEAR LR SCALAR LATLON 1
# DISTWGT LR SCALAR LATLON 10 4
# 
# --- end field 6 ---
############################################################################
#
# --- start Field 7 : direct shortwave downward radiation (W/m2)
# 
COSDIRSW CLMDIRSW  11  ${COUPLING_TIMESTEP}  2  cosmo.nc  EXPORTED
 ${LM_NL_IELM_C} ${LM_NL_JELM_C} ${CLM_nx} ${CLM_ny} coap  clme  LAG=+0 SEQ=1
R  0  R  0
#
LOCTRANS   SCRIPR 
#
 AVERAGE
 BILINEAR LR SCALAR LATLON 1
# DISTWGT LR SCALAR LATLON 10 4
# 
# --- end field 7 ---
############################################################################
#
# --- start Field 8 :  diffuse shortwave downward radiation (W/m2)
# 
COSDIFSW CLMDIFSW 12  ${COUPLING_TIMESTEP}  2  cosmo.nc  EXPORTED
 ${LM_NL_IELM_C} ${LM_NL_JELM_C} ${CLM_nx} ${CLM_ny} coap  clme  LAG=+0 SEQ=1
R  0  R  0
#
LOCTRANS   SCRIPR 
#
 AVERAGE
 BILINEAR LR SCALAR LATLON 1
# DISTWGT LR SCALAR LATLON 10 4
# 
# --- end field 8 ---
############################################################################
#
# --- start Field 9 : longwave downward radiation (W/m2)
# 
COSLONGW CLMLONGW 10  ${COUPLING_TIMESTEP}  2  cosmo.nc  ${OASIS_OUT}
 ${LM_NL_IELM_C} ${LM_NL_JELM_C} ${CLM_nx} ${CLM_ny} coap  clme  LAG=+0 SEQ=1
R  0  R  0
#
LOCTRANS   SCRIPR 
#
 AVERAGE
 BILINEAR LR SCALAR LATLON 1
# DISTWGT LR SCALAR LATLON 10 4
# 
# --- end field 9 ---
############################################################################
#
# --- start Field 10 :  total convective precipitation      (kg/m2*s)
# 
COSCVPRE CLMCVPRE 152  ${COUPLING_TIMESTEP}  2  cosmo.nc  ${OASIS_OUT}
 ${LM_NL_IELM_C} ${LM_NL_JELM_C} ${CLM_nx} ${CLM_ny} coap  clme  LAG=+0 SEQ=1
R  0  R  0
#
LOCTRANS   SCRIPR 
#
 AVERAGE
 BILINEAR LR SCALAR LATLON 1
# DISTWGT LR SCALAR LATLON 10 4
# 
# --- end field 10 ---
############################################################################
#
# --- start Field 11 : total large scale precipitation      (kg/m2*s)
# 
COSGSPRE CLMGSPRE  210  ${COUPLING_TIMESTEP}  2  cosmo.nc  ${OASIS_OUT}
 ${LM_NL_IELM_C} ${LM_NL_JELM_C} ${CLM_nx} ${CLM_ny} coap  clme  LAG=+0 SEQ=1
R  0  R  0
#
LOCTRANS   SCRIPR 
#
 AVERAGE
 BILINEAR LR SCALAR LATLON 1
# DISTWGT LR SCALAR LATLON 10 4
# 
# --- end field 11 ---
############################################################################
#                      SOIL  --->>>  ATMOSPHERE
#                      -------------------------
############################################################################
#
# --- start Field 12 :   zonal wind stress
# 
CLM_TAUX COS_TAUX 353 ${COUPLING_TIMESTEP}  2  clm.nc EXPORTED
 ${CLM_nx} ${CLM_ny} ${LM_NL_IELM_C} ${LM_NL_JELM_C} clme   coap   LAG=+0 SEQ=2
R  0  R  0
#
LOCTRANS   SCRIPR 
#
 AVERAGE
 DISTWGT U SCALAR LATLON 10 4
#
# --- end field 12 ---
#########################################################################
#
# --- start Field 13 :   meridional wind stress
# 
CLM_TAUY COS_TAUY 356 ${COUPLING_TIMESTEP}  2  clm.nc EXPORTED
 ${CLM_nx} ${CLM_ny} ${LM_NL_IELM_C} ${LM_NL_JELM_C} clme   coap   LAG=+0 SEQ=2
R  0  R  0
#
LOCTRANS   SCRIPR 
#
 AVERAGE
 DISTWGT U SCALAR LATLON 10 4
#
# --- end field 13 ---
#########################################################################
#
# --- start Field 14 :  total latent heat flux (W/m**2)
# 
CLMLATEN COSLATEN 8 ${COUPLING_TIMESTEP}  2  clm.nc EXPORTED
 ${CLM_nx} ${CLM_ny} ${LM_NL_IELM_C} ${LM_NL_JELM_C} clme   coap   LAG=+0 SEQ=2
R  0  R  0
#
LOCTRANS   SCRIPR 
#
 AVERAGE
 DISTWGT U SCALAR LATLON 10 4
#
# --- end field 14 ---
#########################################################################
#
# --- start Field 15 :   total sensible heat flux (W/m**2)
# 
CLMSENSI COSSENSI 9 ${COUPLING_TIMESTEP}  2  clm.nc EXPORTED
 ${CLM_nx} ${CLM_ny} ${LM_NL_IELM_C} ${LM_NL_JELM_C} clme   coap   LAG=+0 SEQ=2
R  0  R  0
#
LOCTRANS   SCRIPR 
#
 AVERAGE
 DISTWGT U SCALAR LATLON 10 4
#
# --- end field 15 ---
#########################################################################
#
# --- start Field 16 :   emitted infrared (longwave) radiation (W/m**2)
# 
CLMINFRA COSINFRA 5 ${COUPLING_TIMESTEP}  2  clm.nc EXPORTED
 ${CLM_nx} ${CLM_ny} ${LM_NL_IELM_C} ${LM_NL_JELM_C} clme   coap   LAG=+0 SEQ=2
R  0  R  0
#
LOCTRANS   SCRIPR 
#
 AVERAGE
 DISTWGT U SCALAR LATLON 10 4
#
# --- end field 16 ---
#########################################################################
#
# --- start Field 17 :  albedo
# 
CLMALBED COSALBED 17 ${COUPLING_TIMESTEP}  2  clm.nc EXPORTED
 ${CLM_nx} ${CLM_ny} ${LM_NL_IELM_C} ${LM_NL_JELM_C} clme   coap   LAG=+0 SEQ=2
R  0  R  0
#
LOCTRANS   SCRIPR 
#
 AVERAGE
 DISTWGT U SCALAR LATLON 10 4
#
# --- end field 17 ---
 \$END
EONL
}
export -f cclm2_c_namcouple

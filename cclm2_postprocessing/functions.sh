#!/bin/bash

#==========================================
# Case settings
#==========================================

# HISTORICAL
#export year_ini=2004                                 # initial date of the simulation, incl spin-up
#export year_start=2006                               # start of the analysis period, excl spin-up or min 10 years
#export year_end=2015                                 # end of the analysis period

# FUTURE
export year_ini=2034                                 # initial date of the simulation, incl spin-up
export year_start=2041                               # start of the analysis period, excl spin-up or min 10 years
export year_end=2050                                 # end of the analysis period

export case_source=$(basename "$(dirname "${PWD}")") # extract case name
export case_dest=$case_source                        # case name on PROJECT

export scriptdir=$PWD
export casedir=$SCRATCH/${case_source}

export years="$(seq $year_start $year_end)"

#==========================================
# Functions
#==========================================

# Basic post-processing of CLM output
postproc_clm(){
    # Modify global file attribute
    ncatted -O -h -a source,global,m,c,"CLM5.0 Community Land Model" $1 tmp1.nc
    
    # Rearrange longitude variable from 0..360 deg to -180..180 deg
    # Use ncap2 arithmetics with rounding to preserve 2 digit precision
    ncap2 -O -s 'where(lon>180) lon=round((lon-360)*100)/100' tmp1.nc tmp2.nc
    
    # Cut away sponge zone
    # EUR11 lonlat bounds excl. sponge zone for clipping
    lonmin=-44.55
    lonmax=64.95
    latmin=21.95
    latmax=72.65 # +0.1 for NCO to clip at 72.55
    ncks -O -d lon,${lonmin},${lonmax} -d lat,${latmin},${latmax} tmp2.nc tmp3.nc

    mv tmp3.nc $1
    rm tmp*.nc
}

# Grid description and weights for bilinear re-gridding to the CLM regular lonlat grid
scriptdir=$PWD
clmfile=$(ls ../20_cclm2_c/clm5.0_eur0.1.clm2.h0.*-01-01-00000.nc | head -n 1)
cosmofile=$(ls ../20_cclm2_c/cosmo_output/daily_2D/lffd*.nc | head -n 1)

export gridfile=${scriptdir}/grid_EUR11_lonlat_griddes.txt
if [ ! -f "$gridfile" ]; then
    cdo griddes $clmfile > $gridfile
fi

export weightsfile=${scriptdir}/grid_EUR11_lonlat_genbil.nc
if [ ! -f "$weightsfile" ]; then
    cdo -selname,T_2M_AV $cosmofile tmp1_cosmo.nc
    ncks -O -d rlon,14,-13 -d rlat,14,-13 tmp1_cosmo.nc tmp2_cosmo.nc
    cdo genbil,$gridfile tmp2_cosmo.nc $weightsfile
    rm tmp*_cosmo.nc
fi

# Basic post-processing of COSMO output
postproc_cosmo(){
    # Cut away sponge zone (13 cells per side)
    ncks -O -d rlon,14,-13 -d rlat,14,-13 $1 tmp_cut.nc
    
    # Remap to EUR11 0.1° lonlat grid (15 GB)
    cdo -remap,$gridfile,$weightsfile tmp_cut.nc $2
    
    # Compress (6 GB)
    #ncks -4 -L 1 tmp_remap.nc $2
    
    rm tmp_cut.nc
}
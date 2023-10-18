#!/bin/bash
# Copy executables and input data to decrement

# ---------------------------------------------------------------------------
# Copy executables to decrement/bin
# ---------------------------------------------------------------------------

# CESM
CASENAME=clm5.0.gnu-oasis.I2000Clm50SpGs.CLM_USRDAT.eur_0.5.cclm2 # compiled case
CASEDIR=$SCRATCH/CCLM2_cases/$CASENAME # case directory on scratch
rsync -av $CASEDIR/bld/cesm.exe $PROJECT/decrement/bin/

# COSMO
COSMO_PATH=$(spack location -i cosmo%nvhpc) # $PROJECT/spack-install/cosmo-c2sm-features/nvhpc-21.3/ozd2dbszafn3mpcxd2ywzcnf75swdpir
rsync -av $COSMO_PATH/bin/cosmo_gpu $PROJECT/decrement/bin/

# INT2LM
INT2LM_PATH=$(spack location -i int2lm%nvhpc) # $PROJECT/spack-install/int2lm-c2sm-features/nvhpc-21.3/eyg6ffi7rb5r3bnbp6ukque5ejet7z5u
rsync -av $INT2LM_PATH/bin $PROJECT/decrement/bin/int2lm@c2sm-features


# ---------------------------------------------------------------------------
# Specify the spack environment for COSMO and INT2LM in bin
# ---------------------------------------------------------------------------
spack load --sh cosmo@c2sm-features%nvhpc +oasis ^mpich%nvhpc ^oasis+fix_mct_conflict > $PROJECT/decrement/bin/spack_env_cosmo_gpu.sh
spack load --sh int2lm@c2sm-features%nvhpc > $PROJECT/decrement/bin/spack_env_int2lm.sh

# Linked from bin to the directories in which the executables are run in the CCLM2 config


# ---------------------------------------------------------------------------
# Copy CESM input data from SCRATCH (or sm61)
# ---------------------------------------------------------------------------

# DATM inputs
# For filenames, check $CASEDIR/Buildconf/datm.input_data_list (only those to keep, renamed)
faerosoldep="/scratch/snx3000/psieber/CCLM2_inputdata/cesm_inputdata/atm/cam/chem/trop_mozart_aero/aero/aerosoldep_WACCM.ensmean_monthly_hist_1849-2015_0.9x1.25_CMIP6_c180926.nc" # different aerosol deposition"
ftopo="/scratch/snx3000/psieber/CCLM2_inputdata/cesm_inputdata/atm/datm7/topo_forcing/topodata_0.9x1.25_USGS_070110_stream_c151201.nc"
fco2="/scratch/snx3000/psieber/CCLM2_inputdata/cesm_inputdata/atm/datm7/CO2/fco2_datm_global_simyr_1750-2014_CMIP6_c180929.nc" # newer fco2

# CLM inputs
# For filenames, check $CASEDIR/Buildconf/clm.input_data_list (renamed fatmlndfrc to domainfile)
megan_factors_file="/scratch/snx3000/psieber/CCLM2_inputdata/cesm_inputdata/atm/cam/chem/trop_mozart/emis/megan21_emis_factors_78pft_c20161108.nc"
paramfile_1="/scratch/snx3000/psieber/CCLM2_inputdata/cesm_inputdata/lnd/clm2/paramdata/clm5_params.c171117.nc" # Older paramfile is default, also clm50_params.c211112.nc (from CTSMdev?)
paramfile_2="/scratch/snx3000/psieber/CCLM2_inputdata/cesm_inputdata/lnd/clm2/paramdata/clm50_params.c211112.nc"
fsnowaging="/scratch/snx3000/psieber/CCLM2_inputdata/cesm_inputdata/lnd/clm2/snicardata/snicar_drdt_bst_fit_60_c070416.nc"
fsnowoptics="/scratch/snx3000/psieber/CCLM2_inputdata/cesm_inputdata/lnd/clm2/snicardata/snicar_optics_5bnd_c090915.nc"
stream_fldfilename_urbantv="/scratch/snx3000/psieber/CCLM2_inputdata/cesm_inputdata/lnd/clm2/urbandata/CLM50_tbuildmax_Oleson_2016_0.9x1.25_simyr1849-2106_c160923.nc"

# MOSART should not be needed because ROF is turned off
# stream_fldfilename_lai = 'CESM_input/MODISPFTLAI_0.5x0.5_c140711.nc' only created if use_lai_streams = .true.
stream_fldfilename_lai="/scratch/snx3000/psieber/CCLM2_inputdata/cesm_inputdata/lnd/clm2/lai_streams/MODISPFTLAI_0.5x0.5_c140711.nc"
    
# Resolution-specific
domainfile_05="/scratch/snx3000/psieber/CCLM2_inputdata/CCLM2_EUR_inputdata/domain/domain_EU-CORDEX_0.5_lon360.nc"
domainfile_01="/scratch/snx3000/psieber/CCLM2_inputdata/CCLM2_EUR_inputdata/domain/domain_EU-CORDEX_0.1_lon360.nc"

oasisdummy_05="/scratch/snx3000/psieber/CCLM2_inputdata/CCLM2_EUR_inputdata/OASIS_dummy_for_datm/OASIS_dummy_0.5_lon360.nc"
oasisdummy_01="/scratch/snx3000/psieber/CCLM2_inputdata/CCLM2_EUR_inputdata/OASIS_dummy_for_datm/OASIS_dummy_0.1_lon360.nc"

fsurdat_05="/scratch/snx3000/psieber/CCLM2_inputdata/CCLM2_EUR_inputdata/surfdata/surfdata_0.5x0.5_hist_16pfts_Irrig_CMIP6_simyr2000_c190418.nc"
fsurdat_01="/scratch/snx3000/psieber/CCLM2_inputdata/CCLM2_EUR_inputdata/surfdata/surfdata_0.1x0.1_EUR_hist_16pfts_Irrig_CMIP6_simyr2005_c230523.nc" # produced with hires_pft option

finidat="/scratch/snx3000/psieber/CCLM2_inputdata/cesm_inputdata/lnd/clm2/initdata_map/clmi.I2000Clm50BgcCrop.2011-01-01.1.9x2.5_gx1v7_gl4_simyr2000_c190312.nc" # generic, will be interpolated
# later use restart file from spin up, e.g. IHistClm50Bgc.hcru_hcru.clm5.0_SeSCs_spinup_181223.clm2.r.1979-01-01-00000.nc / COSMO_CLM2_EUR-11_test_210407.clm2.r.1995-01-06-00000.nc

# Collect and loop over files to copy
flist=($faerosoldep $ftopo $fco2 \
$megan_factors_file $paramfile_1 $paramfile_2 $fsnowaging $fsnowoptics $stream_fldfilename_urbantv $stream_fldfilename_lai \
$domainfile_05 $domainfile_01 $oasisdummy_05 $oasisdummy_01 $fsurdat_05 $fsurdat_01 $finidat)

for f in ${flist[@]}; do
    rsync -av ${f} $PROJECT/cclm2_input_decrement/cesm_input/
done

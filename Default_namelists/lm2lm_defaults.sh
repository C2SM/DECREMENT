#!/bin/bash

# ---------------------------------------------------------------
#                            INPUT
# ---------------------------------------------------------------
lm2lm_INPUT(){
    cat > INPUT << EONL
 &CONTRL
  hstart = $LM_NL_HSTART
  hstop = $LM_NL_HSTOP
  hincbound = 1.0
  nprocx = $NQS_NXLM2LM
  nprocy = $NQS_NYLM2LM
  nprocio = $NQS_NIOLM2LM
  ydate_ini = '${LM_YYYY_INI}${LM_MM_INI}${LM_DD_INI}${LM_ZZ_INI}0000'
  ydate_bd = '${LM_YYYY_INI}${LM_MM_INI}${LM_DD_INI}${LM_ZZ_INI}0000'
  lmixcld = .FALSE.
  yinput_model = 'COSMO'
  linitial = .TRUE.
  idbg_level = 3
  luse_t_skin = .TRUE.
  nincwait = 30
  nmaxwait = 120
  lboundaries = .TRUE.
  lprog_qi = $LM_NL_LANA_QI_F
  lprog_qr_qs = .TRUE.
  lprog_rho_snow = $LM_NL_LAN_RHO_SNOW_F
  lmulti_layer_in = .TRUE.
  lmulti_layer_lm = .TRUE.
  itype_w_so_rel = 1
  lforest = .TRUE.
  lsso = .FALSE.
  lvertwind_ini = .TRUE.
  lvertwind_bd = .FALSE.
  llbc_smooth = .TRUE.
  lfilter_pp = .TRUE.
  lfilter_oro = .FALSE.
  ilow_pass_oro = 4
  numfilt_oro = 1
  ilow_pass_xso = 5
  lxso_first = .FALSE.
  numfilt_xso = 1
  rxso_mask = 750.0
  eps_filter = 0.1
  norder_filter = 5
  l_topo_z = .FALSE.
  l_s_oro = .TRUE.
  rfill_valley = 0.0
  ifill_valley = 7
  ltime_mean = .TRUE.
  lreorder = .FALSE.
  lasync_io = .FALSE.
  lbdclim = .TRUE.
  itype_aerosol = ${LM_NL_ITYPE_AEROSOL_F}
  itype_albedo = ${LM_NL_ITYPE_ALBEDO_F}
  lradtopo = $LM_NL_LRADTOPO_F
EONL
    [[ -n ${LM_NL_NHORI_F} ]] && echo "  nhori = $LM_NL_NHORI_F" >> INPUT_PHY
    cat >> INPUT << EONL
 /END

 &GRID_IN
  pcontrol_fi = 30000.
  ie_in_tot = $LM_NL_IELM_C
  je_in_tot = $LM_NL_JELM_C
  ke_in_tot = $LM_NL_KELM_C
  startlat_in_tot = $LM_NL_STARTLAT_TOT_C
  startlon_in_tot = $LM_NL_STARTLON_TOT_C
  pollat_in = $LM_NL_POLLATLM_C
  pollon_in = $LM_NL_POLLONLM_C
  dlat_in = $LM_NL_DLATLM_C
  dlon_in = $LM_NL_DLONLM_C
  endlon_in_tot = 13.86
  endlat_in_tot = 15.51
  ke_soil_in = 9
  czml_soil_in = 0.005, 0.025, 0.07, 0.16, 0.34, 0.70, 1.47, 2.86, 5.74, 11.50
 /END

 &LMGRID
  irefatm = 2
  ielm_tot = $LM_NL_IELM_F
  jelm_tot = $LM_NL_JELM_F
  kelm_tot = $LM_NL_KELM_F
  ivctype = 2
  vcflat = $LM_VCFLAT
  vcoord_d = $LM_VCOORD_D
  pollat = $LM_NL_POLLATLM_F
  pollon = $LM_NL_POLLONLM_F
  dlon = $LM_NL_DLONLM_F
  dlat = $LM_NL_DLATLM_F
  startlat_tot = $LM_NL_STARTLAT_TOT_F
  startlon_tot = $LM_NL_STARTLON_TOT_F
  ke_soil_lm = 9
  czml_soil_lm = 0.005, 0.025, 0.07, 0.16, 0.34, 0.70, 1.47, 2.86, 5.74, 11.50
 /END

 &DATABASE
 /END

 &DATA
  ylmext_form_read = 'ncdf'
  yinext_form_read = 'ncdf'
  yin_form_read = 'ncdf'
  ylm_form_write = 'ncdf'
  ie_ext = $LM_NL_EXT_IE_F
  je_ext = $LM_NL_EXT_JE_F
  ylmext_lfn = '../bin/$LM_NL_EXTPAR_F'
  ylmext_cat = './'
  yinext_lfn = 'lffd${LM_YYYY_INI}${LM_MM_INI}${LM_DD_INI}${LM_ZZ_INI}0000c.nc'
  yinext_cat = 'input/bc'
  yin_cat = 'input/bc'
  ylm_cat = 'output'
  nprocess_ini = 131
  nprocess_bd = 132
  yinput_type = 'forecast'
  ytunit_in = 'd'
  ytunit_out = 'd'
 /END

 &PRICTR
  igp_tot = 36, 40, 48, 44, 48, 85, 77
  jgp_tot = 30, 94, 38, 26, 26, 96, 12
  lchkin = .TRUE.
  lchkout = .TRUE.
  lprgp = .FALSE.
 /END
EONL
}
export -f lm2lm_INPUT

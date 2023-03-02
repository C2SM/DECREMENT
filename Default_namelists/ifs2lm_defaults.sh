#!/bin/bash

# ---------------------------------------------------------------
#                            INPUT
# ---------------------------------------------------------------
ifs2lm_INPUT(){
    cat > INPUT << EONL
 &CONTRL
  yinput_model = 'CM'
  hstart = $LM_NL_HSTART
  hstop = $LM_NL_HSTOP
  hincbound = $LM_NL_HINCBOUND_C
  ydate_ini = '${LM_YYYY_INI}${LM_MM_INI}${LM_DD_INI}${LM_ZZ_INI}0000'
  linitial = .TRUE.
  lboundaries = .TRUE.
  ltime_mean = .TRUE.
  luvcor = .TRUE.
  idbg_level = 3
  luse_t_skin = $LM_NL_LUSE_T_SKIN
  nprocx = ${NQS_NXIFS2LM}
  nprocy = ${NQS_NYIFS2LM}
  nprocio = ${NQS_NIOIFS2LM}
  lfilter_pp = .TRUE.
  llbc_smooth = .TRUE.
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
  lasync_io = .FALSE.
  lprog_qi = $LM_NL_LANA_QI_C
  nincwait = 30
  nmaxwait = 300
  lmulti_layer_lm = .TRUE.
  lmulti_layer_in = .TRUE.
  itype_w_so_rel = 2
  itype_t_cl = 1
  itype_rootdp = 4
  lforest = .TRUE.
  lsso = .TRUE.
  lbdclim = .TRUE.
  lprog_rho_snow = $LM_NL_LAN_RHO_SNOW_C
  lvertwind_ini = .TRUE.
  lvertwind_bd = .TRUE.
  itype_aerosol = $LM_NL_ITYPE_AEROSOL_C
  itype_albedo = $LM_NL_ITYPE_ALBEDO_C
 /END

 &GRID_IN
  pcontrol_fi = 30000.
  ie_in_tot = $LM_NL_IEIFS
  je_in_tot = $LM_NL_JEIFS
  ke_in_tot = $LM_NL_KEIFS
  startlat_in_tot = $LM_NL_PHILUIFS
  startlon_in_tot = $LM_NL_LAMLUIFS
  endlat_in_tot = $LM_NL_PHIROIFS
  endlon_in_tot = $LM_NL_LAMROIFS
  pollat_in = $LM_NL_POLPHIIFS
  pollon_in = $LM_NL_POLLAMIFS
  dlat_in = $LM_NL_DPHIIFS
  dlon_in = $LM_NL_DLAMIFS
  ke_soil_in = 3
  czml_soil_in = $LM_NL_CZML_SOIL_IN
 /END

 &LMGRID
  ielm_tot = $LM_NL_IELM_C
  jelm_tot = $LM_NL_JELM_C
  kelm_tot = $LM_NL_KELM_C
  ivctype = 2
  vcflat = $LM_VCFLAT
  ke_soil_lm = 9
  irefatm = 2
  czml_soil_lm = 0.005, 0.025, 0.07, 0.16, 0.34, 0.70, 1.47, 2.86, 5.74, 11.50
  vcoord_d = $LM_VCOORD_D
  pollat = $LM_NL_POLLATLM_C
  pollon =$LM_NL_POLLONLM_C
  dlon = $LM_NL_DLONLM_C
  dlat = $LM_NL_DLATLM_C
  startlat_tot = $LM_NL_STARTLAT_TOT_C
  startlon_tot = $LM_NL_STARTLON_TOT_C
 /END

 &DATA
  ylmext_form_read = 'ncdf'
  yinext_form_read = 'ncdf'
  yin_form_read = 'ncdf'
  ylm_form_write = 'ncdf'
  ie_ext = $LM_NL_EXT_IE_C
  je_ext = $LM_NL_EXT_JE_C
  ylmext_lfn = '../bin/${LM_NL_EXTPAR_C}'
  yinext_lfn = 'cas${LM_YYYY_INI}${LM_MM_INI}${LM_DD_INI}${LM_ZZ_INI}0000.nc'
  yinext_cat = './input/'
  yin_cat = './input/'
  ylm_cat =  './output/'
  nprocess_ini = 131, nprocess_bd = 132
  yinput_type = 'analysis'
  ytunit_out = 'd'
 /END

 &PRICTR
 /END

 &DATABASE
 /END
EONL
}
export -f ifs2lm_INPUT

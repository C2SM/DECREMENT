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
  idbg_level = ${LM_NL_IDBG_LEVEL_C}
  luse_t_skin = $LM_NL_LUSE_T_SKIN
  nprocx = ${NQS_NXIFS2LM}
  nprocy = ${NQS_NYIFS2LM}
  nprocio = ${NQS_NIOIFS2LM}
  nincwait = 30
  nmaxwait = 300
  lmulti_layer_lm = .TRUE.
  lmulti_layer_in = .TRUE.
  itype_w_so_rel = 1
  itype_t_cl = 1
  itype_rootdp = 4
  lforest = .TRUE.
  llake = $LM_NL_LLAKE_C
  llake_coldstart = $LM_NL_LLAKE_C
  lsso = .TRUE.
  lskinc = .TRUE.
  lbdclim = .TRUE.
  lprog_qi = $LM_NL_LANA_QI_C
  lprog_qr_qs = $LM_NL_LANA_QR_QS_C
  lprog_rho_snow = $LM_NL_LAN_RHO_SNOW_C
  itype_aerosol = $LM_NL_ITYPE_AEROSOL_C
  itype_albedo = $LM_NL_ITYPE_ALBEDO_C
  lradtopo = $LM_NL_LRADTOPO_C
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
  ke_soil_lm = 9
  irefatm = 2
  czml_soil_lm = $LM_NL_CZML_SOIL
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
  yncglob_institution = "ETH Zurich, Switzerland"
  yncglob_title = "INT2LM"
  yncglob_source = "int2lm@c2sm-features"
  yncglob_contact = $LM_NL_YNCGLOB_CONTACT
 /END

 &PRICTR
 /END

 &DATABASE
 /END
EONL
}
export -f ifs2lm_INPUT

#!/bin/bash

# Recommended parameters for COSMO-6 EURO-CORDEX COPAT2
# itype_albedo = 2 # needed for CCLM2 coupling (also in COPAT2)
# deviations from COPAT2 on GPU: do NOT set nproma (optimised for cpu/gpu), lgsp_first = .TRUE., y_scalar_advect='BOTT2_STRANG', L_3D_div_damping=.FALSE.
# To default: hincmxu, ldiagnos, qi0, iadv_order, itype_outflow_qrsg, itype_spubc
# To default: itype_hydmod, lsoil_init_fill, lan_rho_snow, nincwait, nmaxwait
# New value: ncomm_type, soilhyd, tkhmin, khmmin, rat_sea, c_soil, a_hshr, entr_sc
# New value: y_scalar_advect, rdheight, hd_corr_*, l_3D_div_damping, limpltkediff 
# New value: itype_conv, llake, czml_soil, czbot_w_so, itype_canopy, cskinc, lconf_avg, hincrad
# New value: nincrad, itype_gscp, ico2_rad, itype_albedo, icapdcycl, lgpshort, nincgp, nincmeanval

# ---------------------------------------------------------------
#                            INPUT_ORG
# ---------------------------------------------------------------

lm_c_INPUT_ORG(){
    cat > INPUT_ORG << EONL
 &LMGRID
  pollon = $LM_NL_POLLONLM_C
  pollat = $LM_NL_POLLATLM_C
  dlon = $LM_NL_DLONLM_C
  dlat = $LM_NL_DLATLM_C
  startlon_tot = $LM_NL_STARTLON_TOT_C
  startlat_tot = $LM_NL_STARTLAT_TOT_C
  ie_tot = $LM_NL_IELM_C
  je_tot = $LM_NL_JELM_C
  ke_tot = $LM_NL_KELM_C
 /END

 &RUNCTL
  hstart = $LM_NL_HSTART
  hstop = $LM_NL_HSTOP
  dt = $LM_NL_DT_C 
  ydate_ini = '${LM_YYYY_INI}${LM_MM_INI}${LM_DD_INI}${LM_ZZ_INI}0000'
  hincmxt = 24.0
  itype_pert = ${LM_NL_ITYPE_PERT_C}
  rperturb = ${LM_NL_RPERTURB_C}
  ldump_ascii = .FALSE.
  nprocx = $NQS_NXLM_C
  nprocy = $NQS_NYLM_C
  nprocio = 0
  num_asynio_comm = $NQS_NIOLM_C
  num_iope_percomm = $LM_NL_NUM_IOPE_PERCOMM_C
  asynio_block_size = $LM_NL_ASYNIO_BLOCK_SIZE_C
  nboundlines = 3
  ncomm_type = 3
  idbg_level = ${LM_NL_IDBG_LEVEL_C}
 /END

 &TUNING
  soilhyd = 1.6200
  tkhmin = $LM_NL_TKHMIN_C
  tkmmin = $LM_NL_TKMMIN_C
  rlam_heat = $LM_NL_RLAM_HEAT_C
  rat_sea = 9.0
  c_soil = 1.25
  pat_len = 500.0
  tur_len = $LM_NL_TUR_LEN_C
  a_hshr = 0.2
  uc1 = 0.0626
  q_crit = $LM_NL_Q_CRIT_C
  qc0 = $LM_NL_QC0_C
  mu_rain = 0.5
  v0snow = $LM_NL_V0SNOW_C
  entr_sc = 0.0002
 /END
EONL
}
export -f lm_c_INPUT_ORG


# ---------------------------------------------------------------
#                            INPUT_DYN
# ---------------------------------------------------------------

lm_c_INPUT_DYN(){
    cat > INPUT_DYN << EONL
 &DYNCTL
  lcpp_dycore = ${LM_NL_LCPP_DYCORE}
  y_scalar_advect = ${LM_NL_Y_SCALAR_ADVECT}
  rdheight = $LM_NL_RDHEIGHT_C
  rlwidth = ${LM_NL_RLWIDTH_C}
  nrdtau = ${LM_NL_NRDTAU_C}
  l_diff_smag = ${LM_NL_L_DIFF_SMAG_C}
  l_diff_cold_pools = ${LM_NL_LDIFF_COLD_POOLS_C}
  hd_corr_u_bd = $LM_NL_HD_CORR_U_BD_C
  hd_corr_t_bd = $LM_NL_HD_CORR_T_BD_C
  hd_corr_trcr_bd = $LM_NL_HD_CORR_TRCR_BD_C
  hd_corr_p_bd = $LM_NL_HD_CORR_P_BD_C
  hd_corr_u_in = $LM_NL_HD_CORR_U_IN_C
  hd_corr_t_in = $LM_NL_HD_CORR_T_IN_C
  hd_corr_trcr_in = $LM_NL_HD_CORR_TRCR_IN_C
  hd_corr_p_in = $LM_NL_HD_CORR_P_IN_C
  l_3D_div_damping = ${LM_NL_L_3D_DIV_DAMPING}
  isc_sn = 6
  jsc_sn = 6
 /END
EONL
}
export -f lm_c_INPUT_DYN


# ---------------------------------------------------------------
#                            INPUT_PHY
# ---------------------------------------------------------------

lm_c_INPUT_PHY(){
    cat > INPUT_PHY << EONL
 &PHYCTL
  lgsp_first = $LM_NL_LGSP_FIRST
  itype_aerosol = $LM_NL_ITYPE_AEROSOL_C
  loldtur = .TRUE.
  limpltkediff = .FALSE.
  lconv = $LM_NL_LCONV_C
  itype_conv = 2
  lseaice = .FALSE.
  llake = $LM_NL_LLAKE_C
  lsso = $LM_NL_LSSO_C
  ke_soil = 9
  czml_soil = $LM_NL_CZML_SOIL
  czbot_w_so = 4.0
  itype_heatcond = 2
  lsoil_init_fill = .FALSE.
  itype_canopy = 2
  cskinc = -1.0
  itype_root = 2
  ltkesso = .FALSE.
  ltkeshs = .FALSE.
  lconf_avg = .FALSE.
  hincrad = $LM_NL_HINCRAD_C
  nincrad = 10
  nincconv = 2
  itype_evsl = 4
  itype_gscp = $LM_NL_ITYPE_GSCP_C
  itype_vdif = -1
  imode_tran = 1
  ico2_rad = $LM_NL_ICO2_RAD_C
  icldm_tran = 0
  lradtopo = $LM_NL_LRADTOPO_C
  itype_albedo = $LM_NL_ITYPE_ALBEDO_C
  icapdcycl = 2
 /END
EONL
}
export -f lm_c_INPUT_PHY


# ---------------------------------------------------------------
#                           INPUT_IO
# ---------------------------------------------------------------

lm_c_INPUT_IO(){
    cat >INPUT_IO << EONL
 &IOCTL
  ngribout = 3
  nhour_restart = 0, $LM_NL_HSTOP, $LM_NL_HSTOP
  ytunit_restart = 'd'
  ydir_restart_in = 'output/restart'
  ydir_restart_out = 'output/restart'
  itype_gather = 2
  ymode_read = 'r  '
  ymode_write = 'w  '
  yform_read = 'ncdf'
  yform_restart = '${LM_NL_RESTART_FMT_C}'
  yncglob_institution = "ETH Zurich, Switzerland"
  yncglob_title = "COSMO-CLM2 (CCLM6-CLM5)"
  yncglob_source = $LM_NL_YNCGLOB_SOURCE
  yncglob_project_id = $LM_NL_YNCGLOB_PROJECT_ID
  yncglob_experiment_id = $LM_NL_YNCGLOB_EXPERIMENT_ID
  yncglob_contact = $LM_NL_YNCGLOB_CONTACT
  yncglob_references = "http://www.clm-community.eu/"
  lasync_io = $LM_NL_LASYNC_IO_C
  lprefetch_io = ${LM_NL_LPREFETCH_C}
  ldwd_grib_use = .FALSE.
  lbdclim = .TRUE.
 /END

 &DATABASE
 /END

 &GRIBIN
  ydirini = 'input'
  lchkini = .TRUE.
  lana_qi = $LM_NL_LANA_QI_C
  llb_qi = $LM_NL_LANA_QI_C
  lana_qr_qs = $LM_NL_LANA_QR_QS_C
  llb_qr_qs  = $LM_NL_LANA_QR_QS_C
  lana_rho_snow = $LM_NL_LAN_RHO_SNOW_C
  lan_t_so0 = .TRUE.
  lan_t_snow = .TRUE.
  lan_t_cl = .TRUE.
  lan_w_snow = .TRUE.
  lan_w_i = .TRUE.
  lan_w_cl = .TRUE.
  lan_vio3 = .TRUE.
  lan_hmo3 = .TRUE.
  lan_plcov = .TRUE.
  lan_lai = .TRUE.
  lan_rootdp = .TRUE.
  ydirbd = 'input'
  ytunitbd = 'd'
  hincbound = $LM_NL_HINCBOUND_C
  lchkbd =.TRUE.
 /END

 &GRIBOUT
  yform_write = '${LM_NL_OUTPUT_FMT_C}'
  hcomb = 0.0, ${LM_NL_HSTOP}, 3
  yvarml = 'T_2M','T_G','QV_2M','RELHUM_2M','U_10M','V_10M',
     'ASWDIR_S','ASWDIFD_S','ASWDIFU_S','ATHD_S','ATHU_S','ALHFL_S','ASHFL_S',
     'TQV','TQI','TQC','CLCT','PS','PMSL','HPBL'
  yvarpl = ' '
  yvarzl = ' '
  lcheck = .TRUE.
  luvmasspoint = .FALSE.
  lwrite_const = .FALSE.
  ydir = 'output/3h_2D'
  ytunit = 'd'
 /END
 
  &GRIBOUT
  yform_write = '${LM_NL_OUTPUT_FMT_C}'
  hcomb = 0.0, ${LM_NL_HSTOP}, 24
  yvarml = 'ASWDIR_S','ASWDIFD_S','ASWDIFU_S','ATHD_S','ATHU_S','ALHFL_S','ASHFL_S','AEVAP_S',
     'TOT_PREC','RAIN_CON','SNOW_CON','TMIN_2M','TMAX_2M','T_2M_AV',
     'VMAX_10M','VABS_10M','U_10M_AV','V_10M_AV'
  yvarpl = ' '
  yvarzl = ' '
  ireset_sums = 2
  ireset_winds = 2
  ireset_temps = 2
  lcheck = .TRUE.
  luvmasspoint = .FALSE.
  lwrite_const = .FALSE.
  ydir = 'output/daily_2D'
  ytunit = 'd'
 /END

 &GRIBOUT
  yform_write = '${LM_NL_OUTPUT_FMT_C}'
  hcomb = 0.0, ${LM_NL_HSTOP}, 3
  yvarml = ' '
  yvarpl = 'T','QV','FI'
  yvarzl = ' '
  plev = 200, 500, 600, 700, 800, 850, 950, 1000
  lcheck = .FALSE.
  luvmasspoint = .TRUE.
  lwrite_const = .FALSE.
  ydir = 'output/3h_plev'
  ytunit = 'd'
 /END
EONL
}
export -f lm_c_INPUT_IO


# ---------------------------------------------------------------
#                            INPUT_DIA
# ---------------------------------------------------------------

lm_c_INPUT_DIA(){
    cat > INPUT_DIA << EONL
 &DIACTL
  lgpshort = .TRUE.
  nincgp = 240
  nincmeanval = 24
 /END
EONL
}
export -f lm_c_INPUT_DIA


# ---------------------------------------------------------------
#                            INPUT_INI
# ---------------------------------------------------------------

lm_c_INPUT_INI(){
    cat > INPUT_INI << EONL
 &INICTL
 /END
EONL
}
export -f lm_c_INPUT_INI


# ---------------------------------------------------------------
#                            INPUT_SAT
# ---------------------------------------------------------------

lm_c_INPUT_SAT(){
    cat > INPUT_SAT << EONL
 &SATCTL
 /END
EONL
}
export -f lm_c_INPUT_SAT


# ---------------------------------------------------------------
#                            INPUT_ASS
# ---------------------------------------------------------------

lm_c_INPUT_ASS(){
    cat > INPUT_ASS << EONL
 &NUDGING
 /END
EONL
}
export -f lm_c_INPUT_ASS

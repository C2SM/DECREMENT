#!/bin/bash

# ---------------------------------------------------------------
#                       Fine grained Farameters
# ---------------------------------------------------------------

# DEBUG LEVEL
export LM_NL_IDBG_LEVEL_C=4

#I/O
export LM_NL_ASYNIO_BLOCK_SIZE_C=10
export LM_NL_LPREFETCH_C=.FALSE.
export LM_NL_RESTART_FMT_C='bina'

# radiation scheme
export LM_NL_LRADTOPO_C=.FALSE.

# Ensemble runs
export LM_NL_ENS_NUMBER_C=1
export LM_NL_ITYPE_PERT_C=0
export LM_NL_RPERTURB_C=0.00001

# update status of running job in status.log
export LM_NL_STATUS_C="true"

# ---------------------------------------------------------------
#                            INPUT_ORG
# ---------------------------------------------------------------

lm_c_INPUT_ORG(){
    cat > INPUT_ORG << EONL
 &LMGRID
  startlat_tot = $LM_NL_STARTLAT_TOT_C
  startlon_tot = $LM_NL_STARTLON_TOT_C
  pollat = $LM_NL_POLLATLM_C
  pollon = $LM_NL_POLLONLM_C
  dlon = $LM_NL_DLONLM_C
  dlat = $LM_NL_DLATLM_C
  ie_tot = $LM_NL_IELM_C
  je_tot = $LM_NL_JELM_C
  ke_tot = $LM_NL_KELM_C
 /END

 &RUNCTL
  dt = $LM_NL_DT_C
  hstart = $LM_NL_HSTART
  hstop = $LM_NL_HSTOP
  ydate_ini = '${LM_YYYY_INI}${LM_MM_INI}${LM_DD_INI}${LM_ZZ_INI}0000'
  hincmxt = 24.0
  hincmxu = 24.0
  itype_timing = 4
  lreproduce = .TRUE.
  luseobs =.FALSE.
  lphys =.TRUE.
  ldiagnos = .FALSE.
  luse_rttov = .FALSE.
  ldfi = .FALSE.
  nprocx = $NQS_NXLM_C
  nprocy = $NQS_NYLM_C
  nprocio = 0
  num_asynio_comm = $NQS_NIOLM_C
  num_iope_percomm = $LM_NL_NUM_IOPE_PERCOMM_C
  asynio_block_size = $LM_NL_ASYNIO_BLOCK_SIZE_C
  nboundlines = 3
  ldump_ascii = .FALSE.
  itype_pert = ${LM_NL_ITYPE_PERT_C}
  rperturb = ${LM_NL_RPERTURB_C}
  itype_calendar = 0
  idbg_level = ${LM_NL_IDBG_LEVEL_C}
 /END

 &TUNING
  tkhmin = $LM_NL_TKHMIN_C
  tkmmin = $LM_NL_TKMMIN_C
  mu_rain = 0.5
  v0snow = $LM_NL_V0SNOW_C
  rlam_mom = 0.0
  rlam_heat = $LM_NL_RLAM_HEAT_C
  rat_sea = 20.0
  rat_lam = 1.0
  rat_can = 1.0
  c_lnd = 2.0
  c_sea = 1.5
  c_soil = 1.0
  pat_len = 500.0
  z0m_dia = 0.2
  crsmin = 150.0
  clc_diag = 0.5
  q_crit = $LM_NL_Q_CRIT_C
  qc0 = $LM_NL_QC0_C
  qi0 = 5E-6
  uc1 = 0.0626
  tur_len = $LM_NL_TUR_LEN_C
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
  lspubc = .TRUE.
  itype_spubc = 3
  rdheight = $LM_NL_RDHEIGHT_C
  nrdtau = ${LM_NL_NRDTAU_C}
  rlwidth = ${LM_NL_RLWIDTH_C}
  ldyn_bbc = .FALSE.
  itype_bbc_w = 114
  betasw = 0.4
  xkd = 0.1
  epsass = 0.15
  lcond = .TRUE.
  l_diff_smag = ${LM_NL_L_DIFF_SMAG_C}
  l_diff_cold_pools = ${LM_NL_LDIFF_COLD_POOLS_C}
  lhordiff = .TRUE.
  itype_hdiff = 2
  hd_corr_u_bd = $LM_NL_HD_CORR_U_BD_C
  hd_corr_t_bd = $LM_NL_HD_CORR_T_BD_C
  hd_corr_trcr_bd = $LM_NL_HD_CORR_TRCR_BD_C
  hd_corr_p_bd = $LM_NL_HD_CORR_P_BD_C
  hd_corr_u_in = $LM_NL_HD_CORR_U_IN_C
  hd_corr_t_in = $LM_NL_HD_CORR_T_IN_C
  hd_corr_trcr_in = $LM_NL_HD_CORR_TRCR_IN_C
  hd_corr_p_in = $LM_NL_HD_CORR_P_IN_C
  hd_dhmax = 250.
  l2tls = .TRUE.
  irunge_kutta = 1
  irk_order = 3
  iadv_order = 5
  itheta_adv = 0
  ltadv_limiter = .FALSE.
  itype_fast_waves = 2
  y_scalar_advect = 'BOTT2_STRANG'
  y_vert_adv_dyn = 'impl2'
  ieva_order = 3
  itype_outflow_qrsg = 2
  itype_lbc_qrsg = 1
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
  lseaice =.FALSE.
  llake = .FALSE.
  lgsp = .TRUE.
  lgsp_first = .TRUE.
  itype_gscp = $LM_NL_ITYPE_GSCP_C
  ldiniprec = .FALSE.
  lrad = .TRUE.
  hincrad = $LM_NL_HINCRAD_C
  lradtopo = $LM_NL_LRADTOPO_C
EONL
    [[ -n ${LM_NL_NHORI_C} ]] && echo "  nhori = $LM_NL_NHORI_C" >> INPUT_PHY
    cat >> INPUT_PHY << EONL
  ico2_rad = $LM_NL_ICO2_RAD_C
  lforest = .TRUE.
  ltur = .TRUE.
  loldtur = .TRUE.
  ninctura = 1
  itype_vdif = -1
  lexpcor = .FALSE.
  ltmpcor = .FALSE.
  lprfcor = .FALSE.
  lnonloc = .FALSE.
  lcpfluc = .FALSE.
  itype_turb = 3
  imode_turb = 1
  itype_tran = 2
  imode_tran = 1
  limpltkediff = .TRUE.
  itype_wcld = 2
  icldm_rad = 4
  icldm_turb = 2
  icldm_tran = 0
  itype_synd = 2
  lsoil = .TRUE.
  itype_hydmod = 1
  lsoil_init_fill = .TRUE.
  lmelt = .TRUE.
  lmelt_var = .TRUE.
  itype_trvg = 2
  ke_soil = 9
  czml_soil = 0.005, 0.025, 0.07, 0.16, 0.34, 0.70, 1.47, 2.86, 5.74, 11.50
  czbot_w_so = 8.0
  itype_root = 2
  itype_heatcond = 2
  itype_evsl = 4
  lconv = $LM_NL_LCONV_C
  nincconv = 2
  lsso = $LM_NL_LSSO_C
  ltkesso = .False.
  ltkeshs = .False.
  itype_albedo = $LM_NL_ITYPE_ALBEDO_C
  itype_aerosol = $LM_NL_ITYPE_AEROSOL_C
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
  ldwd_grib_use = .FALSE.
  yform_read = 'ncdf'
  l_ke_in_gds = .TRUE.
  lasync_io = $LM_NL_LASYNC_IO_C
  lprefetch_io = ${LM_NL_LPREFETCH_C}
  ymode_read = 'r  '
  ymode_write = 'w  '
  nincwait = 10
  nmaxwait = 200
  nhour_restart = 0, $LM_NL_HSTOP, $LM_NL_HSTOP
  ngribout = 4
  itype_gather = 2
  yform_restart = '${LM_NL_RESTART_FMT_C}'
  ydir_restart_in = 'output/restart'
  ydir_restart_out = 'output/restart'
  ytunit_restart = 'd'
  lbdclim = .TRUE.
  yncglob_title = "COSMO driven by ERA5"
  yncglob_source = "COSMO_5.08"
 /END

 &DATABASE
 /END

 &GRIBIN
  lbdana = .FALSE.
  ydirini = 'input'
  lchkini = .TRUE.
  hincbound = $LM_NL_HINCBOUND_C
  ydirbd = 'input'
  lchkbd =.TRUE.
  lana_qi = $LM_NL_LANA_QI_C
  llb_qi = $LM_NL_LANA_QI_C
  lana_qg = .FALSE.
  llb_qg = .FALSE.
  lana_qr_qs = .FALSE.
  llb_qr_qs  = .FALSE.
  lana_rho_snow = $LM_NL_LAN_RHO_SNOW_C
  lan_lai = .TRUE.
  lan_rootdp = .TRUE.
  lan_vio3 = .TRUE.
  lan_plcov = .TRUE.
  lan_t_cl = .TRUE.
  lan_w_cl = .TRUE.
  lan_hmo3 = .TRUE.
  lan_t_so0 = .TRUE.
  lan_t_snow = .TRUE.
  lan_w_snow = .TRUE.
  lan_w_i = .TRUE.
  lan_rho_snow = .TRUE.
  lan_t_s = .FALSE.
  ytunitbd = 'd'
 /END

 &GRIBOUT
  yform_write = 'nc-4'
  hcomb = 0.0, ${LM_NL_HSTOP}, 1
  yvarml = 'U','V','W','T','PP',
     'QV','QC', 'QS','QR','QI','QG',
     'VIO3','HMO3',
     'T_SNOW','QV_S','W_SNOW','W_I','T_S',
     'T_SO','W_SO','RHO_SNOW','FRESHSNW'
  yvarpl = ' '
  yvarzl = ' '
  lcheck = .FALSE.
  luvmasspoint = .FALSE.
  lwrite_const = .TRUE.
  ydir = 'output/bc'
  l_z_filter = .FALSE.
  l_p_filter = .FALSE.
  l_fi_pmsl_smooth = .FALSE.
  ytunit = 'd'
 /END

 &GRIBOUT
  yform_write = 'nc-4'
  hcomb = 0.0, ${LM_NL_HSTOP}, 1
  yvarml = 'U_10M', 'V_10M', 'T_2M', 'RELHUM_2M', 'PS', 'QV_2M',
           'ALHFL_S', 'ASHFL_S', 'AUMFL_S', 'AVMFL_S', 'CAPE_ML',
           'CIN_ML', 'CLCH', 'CLCM', 'CLCL', 'TOT_PREC',
           'TQC', 'TQI','TQR','TQV','TQG','TQS',
           'ASOB_T', 'ASOD_T',  'ATHB_T', 'ASOBC_T', 'ATHBC_T',
           'ASOB_S', 'ASWDIFD_S','ASWDIR_S', 'ASWDIFU_S', 'ATHB_S', 'ATHD_S',
           'ASOBC_S', 'ATHBC_S'
  yvarpl = ' '
  yvarzl = ' '
  ireset_sums = 2
  lcheck = .TRUE.
  luvmasspoint = .FALSE.
  lwrite_const = .FALSE.
  ydir = 'output/1h_2D'
  l_z_filter = .FALSE.
  l_p_filter = .FALSE.
  l_fi_pmsl_smooth = .FALSE.
  ytunit = 'd'
 /END

 &GRIBOUT
  yform_write = 'nc-4'
  hcomb = 0.0, ${LM_NL_HSTOP}, 3
  yvarml = ' '
  yvarpl = ' ',
  yvarzl = 'U', 'V', 'W', 'T', 'P', 'QV', 'QC', 'QI'
  zlev = 100, 200, 400, 600, 800, 1000, 1200, 1400, 1600, 1800, 2000,
         2300, 2600, 2900, 3200, 3500,
         4000, 4500, 5000, 6000, 7000, 8000, 9000, 10000
  lcheck = .FALSE.
  luvmasspoint = .FALSE.
  lwrite_const = .FALSE.
  ydir = 'output/3h_3D_zlev'
  l_z_filter = .FALSE.
  l_p_filter = .FALSE.
  l_fi_pmsl_smooth = .FALSE.
  ytunit = 'd'
 /END

 &GRIBOUT
  yform_write = 'nc-4'
  hcomb = 0.0, ${LM_NL_HSTOP}, 24
  yvarml = 'VMAX_10M', 'W_SO', 'TMIN_2M', 'TMAX_2M', 'RUNOFF_S', 'RUNOFF_G',
  yvarpl = ' '
  yvarzl = ' '
  ireset_winds = 2
  ireset_temps = 2
  lcheck = .FALSE.
  luvmasspoint = .FALSE.
  lwrite_const = .FALSE.
  ydir = 'output/24h'
  l_z_filter = .FALSE.
  l_p_filter = .FALSE.
  l_fi_pmsl_smooth = .FALSE.
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

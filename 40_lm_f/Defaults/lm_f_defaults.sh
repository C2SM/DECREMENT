#!/bin/bash

# ---------------------------------------------------------------
#                       Fine grained Farameters
# ---------------------------------------------------------------

# DEBUG LEVEL
export LM_NL_IDBG_LEVEL_F=4

#I/O
export LM_NL_ASYNIO_BLOCK_SIZE_F=10
export LM_NL_LPREFETCH_F='.FALSE.'
export LM_NL_RESTART_FMT_F='bina'

# radiation scheme
export LM_NL_LRADTOPO_F=.FALSE.


# ---------------------------------------------------------------
#                            INPUT_ORG
# ---------------------------------------------------------------

lm_f_INPUT_ORG(){
    cat > INPUT_ORG << EONL
 &LMGRID
  startlat_tot = $LM_NL_STARTLAT_TOT_F
  startlon_tot = $LM_NL_STARTLON_TOT_F
  pollat = $LM_NL_POLLATLM_F
  pollon = $LM_NL_POLLONLM_F
  dlon = $LM_NL_DLONLM_F
  dlat = $LM_NL_DLATLM_F
  ie_tot = $LM_NL_IELM_F
  je_tot = $LM_NL_JELM_F
  ke_tot = $LM_NL_KELM_F
 /END

 &RUNCTL
  dt = $LM_NL_DT_F
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
  nprocx = $NQS_NXLM_F
  nprocy = $NQS_NYLM_F
  nprocio = 0
  num_asynio_comm = $NQS_NIOLM_F
  num_iope_percomm = $LM_NL_NUM_IOPE_PERCOMM_F
  asynio_block_size = $LM_NL_ASYNIO_BLOCK_SIZE_F
  nboundlines = 3
  ldump_ascii = .FALSE.
  itype_calendar = 0
  idbg_level = ${LM_NL_IDBG_LEVEL_F}
 /END

 &TUNING
  tkhmin = $LM_NL_TKHMIN_F
  tkmmin = $LM_NL_TKMMIN_F
  mu_rain = 0.5
  v0snow = $LM_NL_V0SNOW_F
  rlam_mom = 0.0
  rlam_heat = $LM_NL_RLAM_HEAT_F
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
  q_crit = $LM_NL_Q_CRIT_F
  qc0 = $LM_NL_QC0_F
  qi0 = 5E-6
  uc1 = 0.0626
  tur_len = $LM_NL_TUR_LEN_F
 /END
EONL
}
export -f lm_f_INPUT_ORG


# ---------------------------------------------------------------
#                            INPUT_DYN
# ---------------------------------------------------------------

lm_f_INPUT_DYN(){
    cat > INPUT_DYN << EONL
 &DYNCTL
  lcpp_dycore = ${LM_NL_LCPP_DYCORE}
  lspubc = .TRUE.
  itype_spubc = 3
  rdheight = $LM_NL_RDHEIGHT_F
  nrdtau = $LM_NL_NRDTAU_F
  rlwidth = $LM_NL_RLWIDTH_F
  ldyn_bbc = .FALSE.
  itype_bbc_w = 114
  betasw = 0.4
  xkd = 0.1
  epsass = 0.15
  lcond = .TRUE.
  l_diff_smag = $LM_NL_L_DIFF_SMAG_F
  l_diff_cold_pools = $LM_NL_LDIFF_COLD_POOLS_F
  lhordiff = .TRUE.
  itype_hdiff = 2
  hd_corr_u_bd = $LM_NL_HD_CORR_U_BD_F
  hd_corr_t_bd = $LM_NL_HD_CORR_T_BD_F
  hd_corr_trcr_bd = $LM_NL_HD_CORR_TRCR_BD_F
  hd_corr_p_bd = $LM_NL_HD_CORR_P_BD_F
  hd_corr_u_in = $LM_NL_HD_CORR_U_IN_F
  hd_corr_t_in = $LM_NL_HD_CORR_T_IN_F
  hd_corr_trcr_in = $LM_NL_HD_CORR_TRCR_IN_F
  hd_corr_p_in = $LM_NL_HD_CORR_P_IN_F
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
export -f lm_f_INPUT_DYN


# ---------------------------------------------------------------
#                            INPUT_PHY
# ---------------------------------------------------------------

lm_f_INPUT_PHY(){
    cat > INPUT_PHY << EONL
 &PHYCTL
  lseaice = .FALSE.
  llake = .FALSE.
  lgsp = .TRUE.
  lgsp_first = .TRUE.
  itype_gscp = $LM_NL_ITYPE_GSCP_F
  ldiniprec = .FALSE.
  lrad = .TRUE.
  hincrad = $LM_NL_HINCRAD_F
  lradtopo = $LM_NL_LRADTOPO_F
EONL
    [[ -n ${LM_NL_NHORI_F} ]] && echo "  nhori = $LM_NL_NHORI_F" >> INPUT_PHY
    cat >> INPUT_PHY << EONL
  ico2_rad = $LM_NL_ICO2_RAD_F
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
  lconv = $LM_NL_LCONV_F
  itype_conv = 3
  nincconv = 5
  lcape = .FALSE.
  lconf_avg = .TRUE.
  lsso = $LM_NL_LSSO_F
  ltkesso = .False.
  ltkeshs = .False.
  itype_albedo = $LM_NL_ITYPE_ALBEDO_F
  itype_aerosol = $LM_NL_ITYPE_AEROSOL_F
 /END
EONL
}
export -f lm_f_INPUT_PHY


# ---------------------------------------------------------------
#                           INPUT_IO
# ---------------------------------------------------------------

lm_f_INPUT_IO(){
    cat > INPUT_IO << EONL
 &IOCTL
  ldwd_grib_use = .FALSE.
  yform_read = 'ncdf'
  l_ke_in_gds = .TRUE.
  lasync_io = $LM_NL_LASYNC_IO_F
  lprefetch_io = ${LM_NL_LPREFETCH_F}
  ymode_read = 'r  '
  ymode_write = 'w  '
  nincwait = 10
  nmaxwait = 200
  nhour_restart = 0, $LM_NL_HSTOP, $LM_NL_HSTOP
  ngribout = 3
  itype_gather = 2
  yform_restart = '${LM_NL_RESTART_FMT_F}'
  ydir_restart_in = 'output/restart'
  ydir_restart_out = 'output/restart'
  ytunit_restart = 'd'
  lbdclim = .TRUE.
  yncglob_title = "COSMO driven by 4 km COSMO"
  yncglob_source = "COSMO_5.08"
 /END

 &DATABASE
 /END

 &GRIBIN
  lbdana = .FALSE.
  ydirini = 'input'
  lchkini = .TRUE.
  hincbound = $LM_NL_HINCBOUND_F
  ydirbd = 'input'
  lchkbd = .FALSE.
  lana_qi = $LM_NL_LANA_QI_F
  llb_qi = $LM_NL_LANA_QI_F
  lana_qg = .FALSE.
  llb_qg = .FALSE.
  lana_qr_qs = .FALSE.
  llb_qr_qs = .FALSE.
  lana_rho_snow = $LM_NL_LAN_RHO_SNOW_F
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
  yvarml = 'U_10M', 'V_10M', 'T_2M', 'RELHUM_2M', 'PS', 'QV_2M',
           'ALHFL_S', 'ASHFL_S', 'AUMFL_S', 'AVMFL_S',
           'CAPE_ML', 'CIN_ML', 'CLCH', 'CLCM', 'CLCL', 'TOT_PREC',
           'TQV', 'TQC', 'TQI','TQR','TQG','TQS',
           'ASOB_T', 'ASOD_T',  'ATHB_T', 'ASOBC_T', 'ATHBC_T',
           'ASOB_S', 'ASWDIFD_S','ASWDIR_S', 'ASWDIFU_S', 'ATHB_S', 'ATHD_S',
           'ASOBC_S', 'ATHBC_S'
  yvarpl = ' '
  yvarzl = ' '
  ireset_sums = 2
  lcheck = .TRUE.
  luvmasspoint = .FALSE.
  lwrite_const = .TRUE.
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
  yvarpl = ' '
  yvarzl = 'U', 'V', 'W', 'T', 'P', 'QV', 'QC', 'QI'
  zlev = 100, 200, 400, 600, 800, 1000, 1200, 1400, 1600, 1800, 2000,
         2300, 2600, 2900, 3200, 3500, 4000, 4500,
         5000, 6000, 7000, 8000, 9000, 10000
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
  yvarml = 'VMAX_10M', 'W_SO', 'TMIN_2M', 'TMAX_2M', 'RUNOFF_S', 'RUNOFF_G'
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
export -f lm_f_INPUT_IO


# ---------------------------------------------------------------
#                            INPUT_DIA
# ---------------------------------------------------------------

lm_f_INPUT_DIA(){
    cat > INPUT_DIA << EONL
 &DIACTL
  n0meanval = 0
  nincmeanval = 1
 /END
EONL
}
export -f lm_f_INPUT_DIA


# ---------------------------------------------------------------
#                            INPUT_INI
# ---------------------------------------------------------------

lm_f_INPUT_INI(){
    cat > INPUT_INI << EONL
 &INICTL
  ndfi = 2
  tspan = 1840.0
  taus = 1840.0
  dtbak = 20.0
  dtfwd = 20.0
 /END
EONL
}
export -f lm_f_INPUT_INI


# ---------------------------------------------------------------
#                            INPUT_SAT
# ---------------------------------------------------------------

lm_f_INPUT_SAT(){
    cat > INPUT_SAT << EONL
 &SATCTL
  num_sensors = 1
  sat_input_01 = 'MSG', 1, 'SEVIRI', 8, .TRUE., .TRUE., .TRUE., .TRUE.
  nchan_input_01 = 1, 2, 3, 4, 5, 6, 7, 8
  lcon_clw = .FALSE.
 /END
EONL
}
export -f lm_f_INPUT_SAT


# ---------------------------------------------------------------
#                            INPUT_ASS
# ---------------------------------------------------------------

lm_f_INPUT_ASS(){
    cat > INPUT_ASS << EONL
 &NUDGING
 /END
EONL
}
export -f lm_f_INPUT_ASS

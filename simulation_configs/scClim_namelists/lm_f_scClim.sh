#!/bin/bash

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
  a_hshr = 0.2
  entr_sc = 0.0002
  soilhyd = 1.62
 /END
EONL
}
export -f lm_f_INPUT_ORG


# ---------------------------------------------------------------
#                            INPUT_IO
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
  nhour_restart = 0, $LM_NL_HSTOP, 24
  ngribout = 5
  itype_gather = 2
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
  yform_write = '${LM_NL_OUTPUT_FMT_F}'
  hcomb = 0.0, ${LM_NL_HSTOP}, 1.0
  yvarml = 'U_10M', 'V_10M', 'VMAX_10M',
           'T_2M', 'TD_2M', 'RELHUM_2M', 'QV_2M','RUNOFF_S', 'RUNOFF_G',
           'ALHFL_S', 'ASHFL_S', 'AUMFL_S', 'AVMFL_S', 'PS', 'PMSL',
           'CAPE_ML', 'CAPE_MU', 'CIN_ML', 'CIN_MU', 'CLCH', 'CLCM', 'CLCL', 'CLCT', 'LFC_ML','LCL_ML',
           'TQV', 'TQC', 'TQI','TQR','TQG','TQS',
           'ASOB_T' , 'ASOD_T'   , 'ATHB_T'  ,
           'ASOB_S' , 'ASWDIFD_S', 'ASWDIR_S', 'ASWDIFU_S', 'ATHB_S', 'ATHD_S',
           'ATHU_S' , 'ALB_RAD', 'HPBL', 'HZEROCL'
  yvarpl = ' '
  yvarzl = ' '
  ireset_sums = 2
  ireset_winds= 2
  lcheck = .TRUE.
  luvmasspoint = .TRUE.
  lwrite_const = .TRUE.
  ydir = 'output/1h_2D'
  l_z_filter = .FALSE.
  l_p_filter = .FALSE.
  l_fi_pmsl_smooth = .FALSE.
  ytunit = 'd'
 /END

 &GRIBOUT
  yform_write = '${LM_NL_OUTPUT_FMT_F}'
  hcomb = 0.0, ${LM_NL_HSTOP}, 1.0
  yvarml = 'LPI_MAX', 'W_CTMAX'
  yvarpl = ' '
  yvarzl = ' '
  ireset_cells = 2
  lcheck = .TRUE.
  luvmasspoint = .TRUE.
  lwrite_const = .FALSE.
  ydir = 'output/1h_2D_LPI'
  l_z_filter = .FALSE.
  l_p_filter = .FALSE.
  l_fi_pmsl_smooth = .FALSE.
  ytunit = 'd'
 /END

 &GRIBOUT
  yform_write = '${LM_NL_OUTPUT_FMT_F}'
  hcomb = 0.0, ${LM_NL_HSTOP}, 24.0
  yvarml = 'T_SO', 'W_SO', 'W_SNOW', 'H_SNOW', 'TMIN_2M', 'TMAX_2M', 'VABSMX_10M'
  yvarpl = ' '
  yvarzl = ' '
  ireset_winds = 2
  ireset_temps = 2
  lcheck = .TRUE.
  luvmasspoint = .TRUE.
  lwrite_const = .FALSE.
  ydir = 'output/24h'
  l_z_filter = .FALSE.
  l_p_filter = .FALSE.
  l_fi_pmsl_smooth = .FALSE.
  ytunit = 'd'
 /END

 &GRIBOUT
  yform_write = '${LM_NL_OUTPUT_FMT_F}'
  hcomb = 0.0, ${LM_NL_HSTOP}, 1.0
  yvarml = ' '
  yvarpl = 'P', 'T', 'FI', 'RELHUM', 'U', 'V', 'W'
  yvarzl = ' '
  plev = 200, 300, 400, 500, 600, 700, 850, 925
  lcheck = .TRUE.
  luvmasspoint = .TRUE.
  lwrite_const = .FALSE.
  ydir = 'output/1h_3D_plev'
  l_z_filter = .FALSE.
  l_p_filter = .FALSE.
  l_fi_pmsl_smooth = .FALSE.
  ytunit = 'd'
 /END

 &GRIBOUT
  yform_write = '${LM_NL_OUTPUT_FMT_F}'
  ncomb = ${LM_NL_NSTART_F}, ${LM_NL_NSTOP_F}, 15
  yvarml = 'TOT_PREC','TOT_SNOW', 'DHAIL_MX'
  yvarpl = ' '
  yvarzl = ' '
  ireset_sums = 2
  lcheck = .TRUE.
  luvmasspoint = .TRUE.
  lwrite_const = .FALSE.
  ydir = 'output/5min_2D'
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
  itype_diag_t2m=1,
  itype_diag_gusts=1,  
  lgplong =.TRUE.,   n0gp=0,      hincgp=1.,
  stationlist_tot= 0, 0, 46.817,  6.935, 'Payerne',
                   0, 0, 47.483,  8.533, 'Zurich-Kloten',
  lhailcast=.TRUE.,
  ninchail = 15,
  wmax_lpi_filter=2.0,
 /END
EONL
}
export -f lm_f_INPUT_DIA

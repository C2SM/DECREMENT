#!/bin/bash

# ---------------------------------------------------------------
#                            drv_in
# ---------------------------------------------------------------
clm_c_drv_in(){
  # CLM start type
  if [[ ${LM_BEGIN_DATE} == ${LM_INI_DATE} ]]; then
    CLM_start_type='"startup"'
  else
    CLM_start_type='"continue"'
  fi

  # Write namelist
  cat > drv_in << EONL
&cime_driver_inst
  ninst_driver = 1
/
&cime_pes
  atm_layout = "concurrent"
  atm_ntasks = ${CLM_C_NTASKS}
  atm_nthreads = 1
  atm_pestride = 1
  atm_rootpe = 0
  cpl_ntasks = ${CLM_C_NTASKS}
  cpl_nthreads = 1
  cpl_pestride = 1
  cpl_rootpe = 0
  esp_layout = "concurrent"
  esp_ntasks = ${CLM_C_NTASKS}
  esp_nthreads = 1
  esp_pestride = 1
  esp_rootpe = 0
  glc_layout = "concurrent"
  glc_ntasks = ${CLM_C_NTASKS}
  glc_nthreads = 1
  glc_pestride = 1
  glc_rootpe = 0
  ice_layout = "concurrent"
  ice_ntasks = ${CLM_C_NTASKS}
  ice_nthreads = 1
  ice_pestride = 1
  ice_rootpe = 0
  lnd_layout = "concurrent"
  lnd_ntasks = ${CLM_C_NTASKS}
  lnd_nthreads = 1
  lnd_pestride = 1
  lnd_rootpe = 0
  ocn_layout = "concurrent"
  ocn_ntasks = ${CLM_C_NTASKS}
  ocn_nthreads = 1
  ocn_pestride = 1
  ocn_rootpe = 0
  rof_layout = "concurrent"
  rof_ntasks = ${CLM_C_NTASKS}
  rof_nthreads = 1
  rof_pestride = 1
  rof_rootpe = 0
  wav_layout = "concurrent"
  wav_ntasks = ${CLM_C_NTASKS}
  wav_nthreads = 1
  wav_pestride = 1
  wav_rootpe = 0
/
&esmf_inparm
  esmf_logfile_kind = "ESMF_LOGKIND_NONE"
/
&papi_inparm
  papi_ctr1_str = "PAPI_FP_OPS"
  papi_ctr2_str = "PAPI_NO_CTR"
  papi_ctr3_str = "PAPI_NO_CTR"
  papi_ctr4_str = "PAPI_NO_CTR"
/
&pio_default_inparm
  pio_async_interface = .false.
  pio_blocksize = -1
  pio_buffer_size_limit = -1
  pio_debug_level = 0
  pio_rearr_comm_enable_hs_comp2io = .true.
  pio_rearr_comm_enable_hs_io2comp = .false.
  pio_rearr_comm_enable_isend_comp2io = .false.
  pio_rearr_comm_enable_isend_io2comp = .true.
  pio_rearr_comm_fcd = "2denable"
  pio_rearr_comm_max_pend_req_comp2io = 0
  pio_rearr_comm_max_pend_req_io2comp = 64
  pio_rearr_comm_type = "p2p"
/
&prof_inparm
  profile_add_detail = .false.
  profile_barrier = .false.
  profile_depth_limit = 4
  profile_detail_limit = 2
  profile_disable = .false.
  profile_global_stats = .true.
  profile_outpe_num = 1
  profile_outpe_stride = 0
  profile_ovhd_measurement = .false.
  profile_papi_enable = .false.
  profile_single_file = .false.
  profile_timer = 4
/
&seq_cplflds_inparm
  flds_bgc_oi = .false.
  flds_co2_dmsa = .false.
  flds_co2a = .false.
  flds_co2b = .false.
  flds_co2c = .false.
  flds_wiso = .false.
  glc_nec = 10
  ice_ncat = 1
  nan_check_component_fields = .true.
  seq_flds_i2o_per_cat = .false.
/
&seq_cplflds_userspec
  cplflds_custom = ""
/
&seq_flux_mct_inparm
  seq_flux_atmocn_minwind = 0.5
  seq_flux_mct_albdif = 0.06
  seq_flux_mct_albdir = 0.07
/
&seq_infodata_inparm
  aoflux_grid = "ocn"
  aqua_planet = .false.
  aqua_planet_sst = 1
  atm_gnam = "360x720cru"
  bfbflag = .false.
  brnch_retain_casename = .false.
  budget_ann = 1
  budget_daily = 0
  budget_inst = 0
  budget_ltann = 1
  budget_ltend = 0
  budget_month = 1
  case_desc = "UNSET"
  case_name = "CCLM2_EU-CORDEX_50km_test"
  cime_model = "cesm"
  coldair_outbreak_mod = .true.
  cpl_decomp = 0
  cpl_seq_option = "CESM1_MOD"
  do_budgets = .false.
  do_histinit = .false.
  drv_threading = .false.
  eps_aarea = 9e-07
  eps_agrid = 1e-12
  eps_amask = 1e-13
  eps_frac = 1.0e-02
  eps_oarea = 0.1
  eps_ogrid = 0.01
  eps_omask = 1e-06
  flux_albav = .false.
  flux_convergence = 0.01
  flux_diurnal = .false.
  flux_epbal = "off"
  flux_max_iteration = 5
  force_stop_at = "month"
  glc_gnam = "null"
  glc_renormalize_smb = "on_if_glc_coupled_fluxes"
  gust_fac = 0.0D0
  histaux_a2x = .false.
  histaux_a2x1hr = .false.
  histaux_a2x1hri = .false.
  histaux_a2x24hr = .false.
  histaux_a2x3hr = .false.
  histaux_a2x3hrp = .false.
  histaux_double_precision = .false.
  histaux_l2x = .false.
  histaux_l2x1yrg = .false.
  histaux_r2x = .false.
  histavg_atm = .true.
  histavg_glc = .true.
  histavg_ice = .true.
  histavg_lnd = .true.
  histavg_ocn = .true.
  histavg_rof = .true.
  histavg_wav = .true.
  histavg_xao = .true.
  hostname = "pizdaint"
  ice_gnam = "null"
  info_debug = 3
  lnd_gnam = "360x720cru"
  logfilepostfix = ".log"
  max_cplstep_time = 0.0
  mct_usealltoall = .false.
  mct_usevector = .false.
  model_doi_url = "https://doi.org/10.5065/D67H1H0V"
  model_version = "release-clm5.0.35-44-g2d0b43b93"
  ocn_gnam = "null"
  orb_eccen = 1.e36
  orb_iyear = 2000
  orb_iyear_align = 2000
  orb_mode = "fixed_year"
  orb_mvelp = 1.e36
  orb_obliq = 1.e36
  outpathroot = "./"
  reprosum_diffmax = -1.0e-8
  reprosum_recompute = .false.
  reprosum_use_ddpdd = .false.
  restart_file = "str_undefined"
  rof_gnam = "r05"
  run_barriers = .false.
  scmlat = -999.
  scmlon = -999.
  shr_map_dopole = .true.
  single_column = .false.
  start_type = ${CLM_start_type}
  tchkpt_dir = "./timing/checkpoints"
  tfreeze_option = "mushy"
  timing_dir = "./timing"
  username = ${CLM_username}
  vect_map = "cart3d"
  wall_time_limit = -1.0
  wav_gnam = "null"
  wv_sat_scheme = "GoffGratch"
  wv_sat_table_spacing = 1.0D0
  wv_sat_transition_start = 20.0D0
  wv_sat_use_tables = .false.
/
&seq_timemgr_inparm
  atm_cpl_dt = 1800
  atm_cpl_offset = 0
  barrier_n = 1
  barrier_option = "ndays"
  barrier_ymd = -999
  calendar = "NO_LEAP"
  data_assimilation_atm = .false.
  data_assimilation_cpl = .false.
  data_assimilation_glc = .false.
  data_assimilation_ice = .false.
  data_assimilation_lnd = .false.
  data_assimilation_ocn = .false.
  data_assimilation_rof = .false.
  data_assimilation_wav = .false.
  end_restart = .false.
  esp_cpl_offset = 0
  esp_run_on_pause = .true.
  glc_avg_period = "yearly"
  glc_cpl_dt = 1800
  glc_cpl_offset = 0
  histavg_n = -999
  histavg_option = "never"
  histavg_ymd = -999
  history_n = -999
  history_option = "never"
  history_ymd = -999
  ice_cpl_dt = 1800
  ice_cpl_offset = 0
  lnd_cpl_dt = 1800
  lnd_cpl_offset = 0
  ocn_cpl_dt = 1800
  ocn_cpl_offset = 0
  pause_active_atm = .false.
  pause_active_cpl = .false.
  pause_active_glc = .false.
  pause_active_ice = .false.
  pause_active_lnd = .false.
  pause_active_ocn = .false.
  pause_active_rof = .false.
  pause_active_wav = .false.
  pause_n = 0
  pause_option = "never"
  restart_n = 86400
  restart_option = "nseconds"
  restart_ymd = -999
  rof_cpl_dt = 10800
  start_tod = 0
  start_ymd = ${LM_YYYY_BEGIN}${LM_MM_BEGIN}${LM_DD_BEGIN}
  stop_n = $(((LM_NL_HSTOP-LM_NL_HSTART) * 3600))
  stop_option = "nseconds"
  stop_ymd = -999
  tprof_n = -999
  tprof_option = "never"
  tprof_ymd = -999
  wav_cpl_dt = 1800
  wav_cpl_offset = 0
/
EONL
}
export -f clm_c_drv_in

# ---------------------------------------------------------------
#                            datm_in
# ---------------------------------------------------------------
clm_c_datm_in(){
  cat > datm_in << EONL
&datm_nml
  decomp = "1d"
  factorfn = "null"
  force_prognostic_true = .false.
  iradsw = 1
  presaero = .true.
  restfilm = "undefined"
  restfils = "undefined"
  wiso_datm = .false.
/
&shr_strdata_nml
  datamode = "CLMNCEP"
  domainfile = "./cesm_input/domain_EU-CORDEX_0.5_lon360.nc"
  dtlimit = 1.5, 1.5, 1.5, 1.5, 1.5
  fillalgo = "nn", "nn", "nn", "nn", "nn"
  fillmask = "nomask", "nomask", "nomask", "nomask", "nomask"
  fillread = "NOT_SET", "NOT_SET", "NOT_SET", "NOT_SET", "NOT_SET"
  fillwrite = "NOT_SET", "NOT_SET", "NOT_SET", "NOT_SET", "NOT_SET"
  mapalgo = "bilinear", "bilinear", "bilinear", "bilinear", "bilinear"
  mapmask = "nomask", "nomask", "nomask", "nomask", "nomask"
  mapread = "NOT_SET", "NOT_SET", "NOT_SET", "NOT_SET", "NOT_SET"
  mapwrite = "NOT_SET", "NOT_SET", "NOT_SET", "NOT_SET", "NOT_SET"
  readmode = "single", "single", "single", "single", "single"
  streams = "datm.streams.txt.CLMGSWP3v1.Solar 2011 2011 2011",
      "datm.streams.txt.CLMGSWP3v1.Precip 2011 2011 2011",
      "datm.streams.txt.CLMGSWP3v1.TPQW 2011 2011 2011",
      "datm.streams.txt.presaero.clim_2000 1 2000 2000",
      "datm.streams.txt.topo.observed 1 1 1"
  taxmode = "cycle", "cycle", "cycle", "cycle", "cycle"
  tintalgo = "coszen", "nearest", "linear", "linear", "lower"
  vectors = "null"
/
EONL
}
export -f clm_c_datm_in

# ---------------------------------------------------------------
#                            drv_flds_in
# ---------------------------------------------------------------
clm_c_drv_flds_in(){
  cat > drv_flds_in << EONL
&megan_emis_nl
  megan_factors_file = './cesm_input/megan21_emis_factors_78pft_c20161108.nc'
  megan_specifier = 'ISOP = isoprene',
      'C10H16 = pinene_a + carene_3 + thujene_a', 'CH3OH = methanol',
      'C2H5OH = ethanol', 'CH2O = formaldehyde', 'CH3CHO = acetaldehyde',
      'CH3COOH = acetic_acid', 'CH3COCH3 = acetone'
/
EONL
}
export -f clm_c_drv_flds_in

# ---------------------------------------------------------------
#                            lnd_in
# ---------------------------------------------------------------
clm_c_lnd_in(){
  cat > lnd_in << EONL
&clm_inparm
 albice = 0.50,0.30
 co2_ppmv = 367.0
 co2_type = 'constant'
 create_crop_landunit = .true.
 dtime = 1800
 fatmlndfrc = './cesm_input/domain_EU-CORDEX_0.5_lon360.nc'
 finidat = './cesm_input/clmi.I2000Clm50BgcCrop.2011-01-01.1.9x2.5_gx1v7_gl4_simyr2000_c190312.nc'
 fsnowaging = './cesm_input/snicar_drdt_bst_fit_60_c070416.nc'
 fsnowoptics = './cesm_input/snicar_optics_5bnd_c090915.nc'
 fsurdat = "./cesm_input/surfdata_0.5x0.5_hist_16pfts_Irrig_CMIP6_simyr2000_c190418.nc"
 glc_do_dynglacier = .false.
 glc_snow_persistence_max_days = 0
 h2osno_max = 10000.0
 int_snow_max = 2000.
 irrigate = .true.
 maxpatch_glcmec = 10
 maxpatch_pft = 17
 n_melt_glcmec = 10.0d00
 nlevsno = 12
 nsegspc = 35
 paramfile = "./cesm_input/clm5_params.cpbiomass.c190103.nc"
 run_zero_weight_urban = .false.
 soil_layerstruct = '20SL_8.5m'
 use_bedrock = .true.
 use_century_decomp = .false.
 use_cn = .false.
 use_crop = .false.
 use_dynroot = .false.
 use_fates = .false.
 use_fertilizer = .false.
 use_fun = .false.
 use_grainproduct = .false.
 use_hydrstress = .true.
 use_init_interp = .true.
 use_lai_streams = .false.
 use_lch4 = .false.
 use_luna = .true.
 use_nitrif_denitrif = .false.
 use_soil_moisture_streams = .false.
 use_vertsoilc = .false.
/
&ndepdyn_nml
/
&popd_streams
/
&urbantv_streams
 stream_fldfilename_urbantv = './cesm_input/CLM50_tbuildmax_Oleson_2016_0.9x1.25_simyr1849-2106_c160923.nc'
 stream_year_first_urbantv = 2000
 stream_year_last_urbantv = 2000
 urbantvmapalgo = 'nn'
/
&light_streams
/
&soil_moisture_streams
/
&lai_streams
/
&atm2lnd_inparm
 glcmec_downscale_longwave = .true.
 lapse_rate = 0.006
 lapse_rate_longwave = 0.032
 longwave_downscaling_limit = 0.5
 precip_repartition_glc_all_rain_t = 0.
 precip_repartition_glc_all_snow_t = -2.
 precip_repartition_nonglc_all_rain_t = 2.
 precip_repartition_nonglc_all_snow_t = 0.
 repartition_rain_snow = .true.
/
&lnd2atm_inparm
 melt_non_icesheet_ice_runoff = .true.
/
&clm_canopyhydrology_inparm
 interception_fraction = 1.0
 maximum_leaf_wetted_fraction = 0.05
 snowveg_flag = 'ON_RAD'
 use_clm5_fpi = .true.
/
&cnphenology
/
&clm_soilhydrology_inparm
/
&dynamic_subgrid
/
&cnvegcarbonstate
/
&finidat_consistency_checks
/
&dynpft_consistency_checks
/
&clm_initinterp_inparm
 init_interp_method = 'general'
/
&century_soilbgcdecompcascade
/
&soilhydrology_inparm
 baseflow_scalar = 0.001d00
/
&luna
 jmaxb1 = 0.093563
/
&friction_velocity
 zetamaxstable = 0.5d00
/
&mineral_nitrogen_dynamics
/
&soilwater_movement_inparm
 dtmin = 60.
 expensive = 42
 flux_calculation = 1
 inexpensive = 1
 lower_boundary_condition = 2
 soilwater_movement_method = 1
 upper_boundary_condition = 1
 verysmall = 1.e-8
 xtolerlower = 1.e-2
 xtolerupper = 1.e-1
/
&rooting_profile_inparm
 rooting_profile_method_carbon = 1
 rooting_profile_method_water = 1
/
&soil_resis_inparm
 soil_resis_method = 1
/
&bgc_shared
/
&canopyfluxes_inparm
 use_undercanopy_stability = .false.
/
&aerosol
 fresh_snw_rds_max = 204.526d00
/
&clmu_inparm
 building_temp_method = 1
 urban_hac = 'ON_WASTEHEAT'
 urban_traffic = .false.
/
&clm_soilstate_inparm
 organic_frac_squared = .false.
/
&clm_nitrogen
 lnc_opt = .false.
/
&clm_snowhydrology_inparm
 lotmp_snowdensity_method = 'Slater2017'
 reset_snow = .false.
 reset_snow_glc = .false.
 reset_snow_glc_ela = 1.e9
 snow_overburden_compaction_method = 'Vionnet2012'
 upplim_destruct_metamorph = 175.d00
 wind_dependent_snow_density = .true.
/
&cnprecision_inparm
/
&clm_glacier_behavior
 glacier_region_behavior = 'single_at_atm_topo','virtual','virtual','multiple'
 glacier_region_ice_runoff_behavior = 'melted','melted','remains_ice','remains_ice'
 glacier_region_melt_behavior = 'remains_in_place','replaced_by_ice','replaced_by_ice','replaced_by_ice'
 glacier_region_rain_to_snow_behavior = 'converted_to_snow','converted_to_snow','converted_to_snow','converted_to_snow'
/
&crop
/
&irrigation_inparm
 irrig_depth = 0.6
 irrig_length = 14400
 irrig_min_lai = 0.0
 irrig_start_time = 21600
 irrig_target_smp = -3400.
 irrig_threshold_fraction = 1.0
 limit_irrigation_if_rof_enabled = .false.
/
&clm_humanindex_inparm
 calc_human_stress_indices = 'FAST'
/
&cnmresp_inparm
/
&photosyns_inparm
 leafresp_method = 0
 light_inhibit = .true.
 modifyphoto_and_lmr_forcrop = .true.
 rootstem_acc = .false.
 stomatalcond_method = 'Medlyn2011'
/
&cnfire_inparm
/
&cn_general
/
&nitrif_inparm
/
&lifire_inparm
/
&ch4finundated
/
&clm_canopy_inparm
 leaf_mr_vcm = 0.015d00
/
EONL
}
export -f clm_c_lnd_in

# ---------------------------------------------------------------
#                            mosart_in
# ---------------------------------------------------------------
clm_c_mosart_in(){
  cat > mosart_in << EONL
&mosart_inparm
  bypass_routing_option = "direct_in_place"
  coupling_period = 10800
  decomp_option = "roundrobin"
  delt_mosart = 3600
  do_rtm = .false.
  do_rtmflood = .false.
  finidat_rtm = " "
  frivinp_rtm = "./cesm_input/MOSART_routing_Global_0.5x0.5_c170601.nc"
  ice_runoff = .true.
  qgwl_runoff_option = "threshold"
  rtmhist_fexcl1 = ""
  rtmhist_fexcl2 = ""
  rtmhist_fexcl3 = ""
  rtmhist_fincl1 = ""
  rtmhist_fincl2 = ""
  rtmhist_fincl3 = ""
  rtmhist_mfilt = 1
  rtmhist_ndens = 1
  rtmhist_nhtfrq = 0
  smat_option = "Xonly"
/
EONL
}
export -f clm_c_mosart_in

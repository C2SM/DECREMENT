#!/bin/bash

# ---------------------------------------------------------------
#                            atm_modelio.nml
# ---------------------------------------------------------------
clm_c_atm_modelio(){
  cat > atm_modelio.nml << EONL
&modelio
  diri = "./cesm_input"
  diro = "./cesm_output"
  logfile = "atm.log"
/
&pio_inparm
  pio_netcdf_format = "64bit_offset"
  pio_numiotasks = -99
  pio_rearranger = 1
  pio_root = 1
  pio_stride = 12
  pio_typename = ${CLM_pio_typename}
/
EONL
}
export -f clm_c_atm_modelio

# ---------------------------------------------------------------
#                            cpl_modelio.nml
# ---------------------------------------------------------------
clm_c_cpl_modelio(){
  cat > cpl_modelio.nml << EONL
&modelio
  diri = "./cesm_input"
  diro = "./cesm_output"
  logfile = "cpl.log"
/
&pio_inparm
  pio_netcdf_format = "64bit_offset"
  pio_numiotasks = -99
  pio_rearranger = 1
  pio_root = 1
  pio_stride = 12
  pio_typename = ${CLM_pio_typename}
/
EONL
}
export -f clm_c_cpl_modelio

# ---------------------------------------------------------------
#                            esp_modelio.nml
# ---------------------------------------------------------------
clm_c_esp_modelio(){
  cat > esp_modelio.nml << EONL
&modelio
  diri = "./cesm_input"
  diro = "./cesm_output"
  logfile = "esp.log"
/
&pio_inparm
  pio_netcdf_format = ""
  pio_numiotasks = -99
  pio_rearranger = -99
  pio_root = -99
  pio_stride = -99
  pio_typename = "nothing"
/
EONL
}
export -f clm_c_esp_modelio

# ---------------------------------------------------------------
#                            glc_modelio.nml
# ---------------------------------------------------------------
clm_c_glc_modelio(){
  cat > glc_modelio.nml << EONL
&modelio
  diri = "./cesm_input"
  diro = "./cesm_output"
  logfile = "glc.log"
/
&pio_inparm
  pio_netcdf_format = "64bit_offset"
  pio_numiotasks = -99
  pio_rearranger = 1
  pio_root = 1
  pio_stride = 12
  pio_typename = ${CLM_pio_typename}
/
EONL
}
export -f clm_c_glc_modelio

# ---------------------------------------------------------------
#                            ice_modelio.nml
# ---------------------------------------------------------------
clm_c_ice_modelio(){
  cat > ice_modelio.nml << EONL
&modelio
  diri = "./cesm_input"
  diro = "./cesm_output"
  logfile = "ice.log"
/
&pio_inparm
  pio_netcdf_format = "64bit_offset"
  pio_numiotasks = -99
  pio_rearranger = 1
  pio_root = 1
  pio_stride = 12
  pio_typename = ${CLM_pio_typename}
/
EONL
}
export -f clm_c_ice_modelio

# ---------------------------------------------------------------
#                            lnd_modelio.nml
# ---------------------------------------------------------------
clm_c_lnd_modelio(){
  cat > lnd_modelio.nml << EONL
&modelio
  diri = "./cesm_input"
  diro = "./cesm_output"
  logfile = "lnd.log"
/
&pio_inparm
  pio_netcdf_format = "64bit_offset"
  pio_numiotasks = -99
  pio_rearranger = 1
  pio_root = 1
  pio_stride = 12
  pio_typename = ${CLM_pio_typename}
/
EONL
}
export -f clm_c_lnd_modelio

# ---------------------------------------------------------------
#                            ocn_modelio.nml
# ---------------------------------------------------------------
clm_c_ocn_modelio(){
  cat > ocn_modelio.nml << EONL
&modelio
  diri = "./cesm_input"
  diro = "./cesm_output"
  logfile = "ocn.log"
/
&pio_inparm
  pio_netcdf_format = "64bit_offset"
  pio_numiotasks = -99
  pio_rearranger = 1
  pio_root = 1
  pio_stride = 12
  pio_typename = ${CLM_pio_typename}
/
EONL
}
export -f clm_c_ocn_modelio

# ---------------------------------------------------------------
#                            rof_modelio.nml
# ---------------------------------------------------------------
clm_c_rof_modelio(){
  cat > rof_modelio.nml << EONL
&modelio
  diri = "./cesm_input"
  diro = "./cesm_output"
  logfile = "rof.log"
/
&pio_inparm
  pio_netcdf_format = "64bit_offset"
  pio_numiotasks = -99
  pio_rearranger = 1
  pio_root = 1
  pio_stride = 12
  pio_typename = ${CLM_pio_typename}
/
EONL
}
export -f clm_c_rof_modelio

# ---------------------------------------------------------------
#                            wav_modelio.nml
# ---------------------------------------------------------------
clm_c_wav_modelio(){
  cat > wav_modelio.nml << EONL
&modelio
  diri = "./cesm_input"
  diro = "./cesm_output"
  logfile = "wav.log"
/
&pio_inparm
  pio_netcdf_format = "64bit_offset"
  pio_numiotasks = -99
  pio_rearranger = 1
  pio_root = 1
  pio_stride = 12
  pio_typename = ${CLM_pio_typename}
/
EONL
}
export -f clm_c_wav_modelio

#!/bin/bash

# Generate datm streams files
# OASIS for variables sent from COSMO (replacing Solar, Precip and TPQW)
# presaero, topo and co2tseries as usual

# ---------------------------------------------------------------
#                            OASIS.stream
# ---------------------------------------------------------------
clm_c_OASIS_stream(){
  cat > OASIS.stream.txt << EOF
<streamstemplate>
      <general_comment>
         streams template for datm in CCSM4
      </general_comment>
<stream>
      <comment>
         Stream description file for OASIS fake
      </comment>
      <dataSource>
         CLMNCEP
      </dataSource>
      <domainInfo>
         <variableNames>
            time    time
            xc      lon
            yc      lat
            area    area
            mask    mask
         </variableNames>
         <filePath>
            ./cesm_input
         </filePath>
         <fileNames>
            ${CLM_oasisdummy}
         </fileNames>
      </domainInfo>
      <fieldInfo>
         <variableNames>
            BCDEPWET   swdn
            BCDEPWET   precn
            BCDEPWET   tbot
            BCDEPWET   wind
            BCDEPWET   shum
            BCDEPWET   pbot
         </variableNames>
         <filePath>
             ./cesm_input
         </filePath>
         <offset>
            0
         </offset>
         <fileNames>
            ${CLM_oasisdummy}
         </fileNames>
      </fieldInfo>
      <!-- Information on the program that created this file -->
      <build_streams_documentation>
         This CCSM stream text file was created using /project/s193/emaison/cesm1_0_3/scripts/ccsm_utils/Tools/build_streams
      </build_streams_documentation>
</stream>
</streamstemplate>
EOF
}
export -f clm_c_OASIS_stream

# ---------------------------------------------------------------
#                            datm.stream.presaero
# ---------------------------------------------------------------
clm_c_datm_stream_presaero(){
  cat > datm.streams.txt.presaero.clim_2000 << EONL
<?xml version="1.0"?>
<file id="stream" version="1.0">
  <dataSource>
    GENERIC
  </dataSource>
  <domainInfo>
    <variableNames>
      time    time
      lon     lon
      lat     lat
      area    area
      mask    mask
    </variableNames>
    <filePath>
      ./cesm_input
    </filePath>
    <fileNames>
      aerosoldep_WACCM.ensmean_monthly_hist_1849-2015_0.9x1.25_CMIP6_c180926.nc
    </fileNames>
  </domainInfo>
  <fieldInfo>
    <variableNames>
      BCDEPWET   bcphiwet
      BCPHODRY   bcphodry
      BCPHIDRY   bcphidry
      OCDEPWET   ocphiwet
      OCPHIDRY   ocphidry
      OCPHODRY   ocphodry
      DSTX01WD   dstwet1
      DSTX01DD   dstdry1
      DSTX02WD   dstwet2
      DSTX02DD   dstdry2
      DSTX03WD   dstwet3
      DSTX03DD   dstdry3
      DSTX04WD   dstwet4
      DSTX04DD   dstdry4
    </variableNames>
    <filePath>
      ./cesm_input
    </filePath>
    <fileNames>
      aerosoldep_WACCM.ensmean_monthly_hist_1849-2015_0.9x1.25_CMIP6_c180926.nc
    </fileNames>
    <offset>
      0
    </offset>
  </fieldInfo>
</file>
EONL
}
export -f clm_c_datm_stream_presaero

# ---------------------------------------------------------------
#                            datm.stream.topo
# ---------------------------------------------------------------
clm_c_datm_stream_topo(){
  cat > datm.streams.txt.topo.observed << EONL
<?xml version="1.0"?>
<file id="stream" version="1.0">
  <dataSource>
    GENERIC
  </dataSource>
  <domainInfo>
    <variableNames>
      time   time
      LONGXY lon
      LATIXY lat
      area   area
      mask   mask
    </variableNames>
    <filePath>
      ./cesm_input
    </filePath>
    <fileNames>
      topodata_0.9x1.25_USGS_070110_stream_c151201.nc
    </fileNames>
  </domainInfo>
  <fieldInfo>
    <variableNames>
      TOPO topo
    </variableNames>
    <filePath>
      ./cesm_input
    </filePath>
    <fileNames>
      topodata_0.9x1.25_USGS_070110_stream_c151201.nc
    </fileNames>
    <offset>
      0
    </offset>
  </fieldInfo>
</file>
EONL
}
export -f clm_c_datm_stream_topo

# ---------------------------------------------------------------
#                            datm.stream.co2tseries
# ---------------------------------------------------------------
clm_c_datm_stream_co2tseries(){
  cat > datm.streams.txt.co2tseries.20tr << EONL
<?xml version="1.0"?>
<file id="stream" version="1.0">
<dataSource>
   GENERIC
</dataSource>
<domainInfo>
  <variableNames>
     time    time
        lonc    lon
        latc    lat
        area    area
        mask    mask
  </variableNames>
  <filePath>
     ./cesm_input
  </filePath>
  <fileNames>
     fco2_datm_global_simyr_1750-2014_CMIP6_c180929.nc
  </fileNames>
</domainInfo>
<fieldInfo>
   <variableNames>
     CO2        co2diag
   </variableNames>
   <filePath>
     ./cesm_input
   </filePath>
   <fileNames>
    fco2_datm_global_simyr_1750-2014_CMIP6_c180929.nc
   </fileNames>
   <offset>
      0
   </offset>
</fieldInfo>
</file>
EONL
}
export -f clm_c_datm_stream_co2tseries
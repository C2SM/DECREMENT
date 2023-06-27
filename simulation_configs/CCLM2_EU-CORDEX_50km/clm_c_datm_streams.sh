#!/bin/bash

# ---------------------------------------------------------------
#                            datm.stream.Precip
# ---------------------------------------------------------------
clm_c_datm_stream_Precip(){
  cat > datm.streams.txt.CLMGSWP3v1.Precip << EONL
<?xml version="1.0"?>
<file id="stream" version="1.0">
  <dataSource>
    GENERIC
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
      domain.lnd.360x720_gswp3.0v1.c170606.nc
    </fileNames>
  </domainInfo>
  <fieldInfo>
    <variableNames>
      PRECTmms precn
    </variableNames>
    <filePath>
      ./cesm_input
    </filePath>
    <fileNames>
      clmforc.GSWP3.c2011.0.5x0.5.Prec.2011-01.nc
      clmforc.GSWP3.c2011.0.5x0.5.Prec.2011-02.nc
      clmforc.GSWP3.c2011.0.5x0.5.Prec.2011-03.nc
      clmforc.GSWP3.c2011.0.5x0.5.Prec.2011-04.nc
      clmforc.GSWP3.c2011.0.5x0.5.Prec.2011-05.nc
      clmforc.GSWP3.c2011.0.5x0.5.Prec.2011-06.nc
      clmforc.GSWP3.c2011.0.5x0.5.Prec.2011-07.nc
      clmforc.GSWP3.c2011.0.5x0.5.Prec.2011-08.nc
      clmforc.GSWP3.c2011.0.5x0.5.Prec.2011-09.nc
      clmforc.GSWP3.c2011.0.5x0.5.Prec.2011-10.nc
      clmforc.GSWP3.c2011.0.5x0.5.Prec.2011-11.nc
      clmforc.GSWP3.c2011.0.5x0.5.Prec.2011-12.nc
    </fileNames>
    <offset>
      0
    </offset>
  </fieldInfo>
</file>
EONL
}
export -f clm_c_datm_stream_Precip

# ---------------------------------------------------------------
#                            datm.stream.Solar
# ---------------------------------------------------------------
clm_c_datm_stream_Solar(){
  cat > datm.streams.txt.CLMGSWP3v1.Solar << EONL
<?xml version="1.0"?>
<file id="stream" version="1.0">
  <dataSource>
    GENERIC
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
      domain.lnd.360x720_gswp3.0v1.c170606.nc
    </fileNames>
  </domainInfo>
  <fieldInfo>
    <variableNames>
      FSDS swdn
    </variableNames>
    <filePath>
      ./cesm_input
    </filePath>
    <fileNames>
      clmforc.GSWP3.c2011.0.5x0.5.Solr.2011-01.nc
      clmforc.GSWP3.c2011.0.5x0.5.Solr.2011-02.nc
      clmforc.GSWP3.c2011.0.5x0.5.Solr.2011-03.nc
      clmforc.GSWP3.c2011.0.5x0.5.Solr.2011-04.nc
      clmforc.GSWP3.c2011.0.5x0.5.Solr.2011-05.nc
      clmforc.GSWP3.c2011.0.5x0.5.Solr.2011-06.nc
      clmforc.GSWP3.c2011.0.5x0.5.Solr.2011-07.nc
      clmforc.GSWP3.c2011.0.5x0.5.Solr.2011-08.nc
      clmforc.GSWP3.c2011.0.5x0.5.Solr.2011-09.nc
      clmforc.GSWP3.c2011.0.5x0.5.Solr.2011-10.nc
      clmforc.GSWP3.c2011.0.5x0.5.Solr.2011-11.nc
      clmforc.GSWP3.c2011.0.5x0.5.Solr.2011-12.nc
    </fileNames>
    <offset>
      0
    </offset>
  </fieldInfo>
</file>
EONL
}
export -f clm_c_datm_stream_Solar

# ---------------------------------------------------------------
#                            datm.stream.TPQW
# ---------------------------------------------------------------
clm_c_datm_stream_TPQW(){
  cat > datm.streams.txt.CLMGSWP3v1.TPQW << EONL
<?xml version="1.0"?>
<file id="stream" version="1.0">
  <dataSource>
    GENERIC
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
      domain.lnd.360x720_gswp3.0v1.c170606.nc
    </fileNames>
  </domainInfo>
  <fieldInfo>
    <variableNames>
      TBOT     tbot
      WIND     wind
      QBOT     shum
      PSRF     pbot
      FLDS     lwdn
    </variableNames>
    <filePath>
      ./cesm_input
    </filePath>
    <fileNames>
      clmforc.GSWP3.c2011.0.5x0.5.TPQWL.2011-01.nc
      clmforc.GSWP3.c2011.0.5x0.5.TPQWL.2011-02.nc
      clmforc.GSWP3.c2011.0.5x0.5.TPQWL.2011-03.nc
      clmforc.GSWP3.c2011.0.5x0.5.TPQWL.2011-04.nc
      clmforc.GSWP3.c2011.0.5x0.5.TPQWL.2011-05.nc
      clmforc.GSWP3.c2011.0.5x0.5.TPQWL.2011-06.nc
      clmforc.GSWP3.c2011.0.5x0.5.TPQWL.2011-07.nc
      clmforc.GSWP3.c2011.0.5x0.5.TPQWL.2011-08.nc
      clmforc.GSWP3.c2011.0.5x0.5.TPQWL.2011-09.nc
      clmforc.GSWP3.c2011.0.5x0.5.TPQWL.2011-10.nc
      clmforc.GSWP3.c2011.0.5x0.5.TPQWL.2011-11.nc
      clmforc.GSWP3.c2011.0.5x0.5.TPQWL.2011-12.nc
    </fileNames>
    <offset>
      0
    </offset>
  </fieldInfo>
</file>
EONL
}
export -f clm_c_datm_stream_TPQW

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

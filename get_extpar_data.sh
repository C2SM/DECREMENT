#!/bin/bash

echo "Copying extpar files form /project. This may take a while."

#For SIMULATION_EU_CORDEX_50km 
rsync -av /project/pr133/davidle/extpar/extpar_50km_EU_CORDEX_159x153.nc bin/

#For SIMULATION_W_ATL
rsync -av /project/pr133/davidle/extpar/extpar_SA_4km.nc bin/
rsync -av /project/pr133/davidle/extpar/lhentge/extpar_atlantic_2km.nc bin/

#For SIMULATION_S_ATL
rsync -av /project/pr133/jcanton/extpar_files/test1_4km/extpar_test1_4km.nc bin/
rsync -av /project/pr133/jcanton/extpar_files/test1_2km/extpar_test1_2km.nc bin/

#For simulation SIMULAITON_NC_ATL
rsync -av /project/pr133/davidle/extpar/extpar_1km_NCA.nc bin/

# For SIMULATION_S_ATL_3km
rsync -av /project/pr133/leclairm/cosmo_sandbox_input/S_ATL_3km_2km/extpar/extpar_SA_3km_new_albedoCON_soil_landuse.nc bin/
rsync -av /project/pr133/leclairm/cosmo_sandbox_input/S_ATL_3km_2km/extpar/extpar_SA_2km_orosmooth_n20_s10.nc bin/

# For SIMULATION_EU_12km_2km
rsync -av /project/pr133/leclairm/cosmo_sandbox_input/EU_12km_2km/extpar/extpar_12km_europe_771x771.nc bin/
rsync -av /project/pr133/leclairm/cosmo_sandbox_input/EU_12km_2km/extpar/extpar_2km_europe_2313x2313.nc bin/

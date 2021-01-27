#!/bin/bash

echo "Copying extpar files form /project. This may take a while."

#For SIMULATION_EU_CORDEX_50km 
cp /project/pr94/davidle/extpar/extpar_50km_EU_CORDEX_159x153.nc bin/

#For SIMULATION_W_ATL
cp /project/pr94/davidle/extpar/extpar_SA_4km.nc bin/
cp /project/pr94/davidle/extpar/lhentge/extpar_atlantic_2km.nc bin/

#For SIMULATION_S_ATL
cp /project/pr94/jcanton/extpar_files/test1_4km/extpar_test1_4km.nc bin/
cp /project/pr94/jcanton/extpar_files/test1_2km/extpar_test1_2km.nc bin/

#For simulation SIMULAITON_NC_ATL
cp /project/pr94/davidle/extpar/extpar_1km_NCA.nc

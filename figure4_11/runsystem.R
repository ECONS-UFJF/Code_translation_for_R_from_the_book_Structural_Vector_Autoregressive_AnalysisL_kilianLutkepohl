# translate to R by André Suriane ECONS ECONOMIA UFJF 
#
# script: runsystem.m
# Generate the system over all necessary sample periods and generate
# bootstrapped confidence bands

# Logan Lewis
# 21-Sept-2009

#-------------------------------------------------------------------------------
rm(list = ls())
#-------------------------------------------------------------------------------
system.time({
# Run the system over the full sample period for monetary shock
# monetarysystem('monthlydata.mat','monthlysystem-1967.05-2008.06.mat',true,1967.05,2008.06)

# BGW sample period (roughtly), using CFNAI
# monetarysystem('monthlydata.mat','monthlysystem-1967.05-1995.12-CFNAI.mat',true,1967.05,1995.12)

# BGW sample period, using BGW GDP
# monetarysystem('monthlydata.mat','monthlysystem-1967.05-1995.12-GDP.mat',false,1967.05,1995.12)

# First period, using BGW GDP
# monetarysystem('monthlydata.mat','monthlysystem-1967.05-1987.07-GDP.mat',false,1967.05,1987.07)

# First period
# monetarysystem('monthlydata.mat','monthlysystem-1967.05-1987.07.mat',true,1967.05,1987.07)
fileloc= 'monthlydata.mat'; saveloc = 'monthlysystem-1967.05-1987.07.RData'; cfnaibool <- TRUE; startdate= 1967.05; enddate = 1987.07;

# Second period
# monetarysystem('monthlydata.mat','monthlysystem-1987.08-2008.06.mat',true,1987.08,2008.06)

# Pre-Volcker
# monetarysystem('monthlydata.mat','monthlysystem-1967.05-1979.07.mat',true,1967.05,1979.07)

# Volcker including 12 months pre-Volcker history
# monetarysystem('monthlydata.mat','monthlysystem-1978.07-1987.07.mat',true,1978.07,1987.07)
source("monetarysystem.R")
})

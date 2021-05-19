# finding files to copy ------
cd '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/data'
for i in renewable_refinery_capacity.xlsx \
oil_price_projections.xlsx \
well_type_df.csv \
ca_crude_prod_a.csv \
Total_Energy_Nominal_Prices_Brent.csv \
  find . -name $i -exec cp {} '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/deliverables/data-repository/energy' \;


# finding file to remove -----
#cd '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/deliverables/data-repository/energy/hist_well'
#for i in CaliforniaOilAndGasFieldMonthlyDisposition.csv \
#CaliforniaOilAndGasFieldQuarterlyOtherWaterAllocation.csv \
#CaliforniaOilAndGasWellQuarterlyInjection.csv \
#CaliforniaOilAndGasWellQuarterlyProduction.csv
#  find . -name $i -exec rm -rf {} \;

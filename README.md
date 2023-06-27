# ca-transport-supply-decarb

# Equitable low-carbon transition pathways for California’s oil extraction

**Authors:** Ranjit Deshmukh, Paige Weber, Olivier Deschenes, Danae Hernandez-Cortes, Tia Kordell, Ruiwen Lee, Christopher Malloy, Tracey Mangin, Measrainsey Meng, Sandy Sum, Vincent Thivierge, Anagha Uppal, David W. Lea, and Kyle Meng

**Corresponding authors:** Ranjit Deshmukh (rdeshmukh@ucsb.edu); Paige Weber, (paigeweber@unc.edu); Kyle Meng (kmeng@bren.ucsb.edu) 

**GitHub repository manager:** Tracey Mangin (tmangin@ucsb.edu)
**Zenodo repository manager:** Tracey Mangin (tmangin@ucsb.edu)

The purpose of this file is to document the data and scripts used in the study “Equitable low-carbon transition pathways for California’s oil extraction.” The first section includes information about data inputs. The second section includes descriptions of the scripts developed to conduct the study (all code related to this study is available in the GitHub repository ca-transport-supply-decarb). The third section includes information about the files included in the study’s [Zenodo repository](https://zenodo.org/record/7742803#.ZGPBrezML0o). 

Due to data confidentiality, the user can only run a subset of the scripts. Thus, we provide all of the intermediate outputs needed to run the following scripts:
* `ca-transport-supply-decarb/energy/extraction-segment/model/full-run-revised/00_extraction_steps.R` - this script runs the energy model that results in oil extraction outputs. To successfully run the energy model, the user should make the following changes in the script:
  - Set `zenodo_repo <- TRUE` 
  - Define `zenodo_user_path`, which should be the user-specific path that leads up to the `ca-transport-supply-decarb-files` folder downloaded from Zenodo
  - Define a `save_path`, which represent the path to where the user would like the outputs to be saved
  - Define a `run_name`, which is used with the current to generate a folder to save outputs (this folder will be created in the `save_path` location).
* `ca-transport-supply-decarb/energy/extraction-segment/figs-and-results/fig_outputs.R` - this script takes the energy, health, and labor outputs and computes values for the manuscript (i.e., outputs needed to make figures and results presented in the paper). To successfully run the code, the user should make the following changes to the script:
  - Set `zenodo_repo <- TRUE` 
  - Define `zenodo_user_path`, which should be the user-specific path that leads up to the `ca-transport-supply-decarb-files` folder downloaded from Zenodo
  - Define `save_info_path`, which is a path specifying where the user wants the outputs to be saved
  - Zenodo users do not need to specify `energy_result_date` or `comp_result_date`. 
* The scripts contained in the main `ca-transport-supply-decarb/energy/extraction-segment/figs-and-results/` (these scripts create figures and values presented in the manuscript. The files `figure2.R`, `figure3.R`, and `figure6.R` are set up so that the user can set `zenodo_repo <- TRUE`  and specify a `zenodo_user_path and zenodo_save_path`. For all other scripts in this folder, the user will need to update file paths. 

## External data
The following section documents all inputs needed to conduct the study. The “Required” column indicates if the file is needed to conduct the analysis, and the “Zenodo” column indicates if the input is included in the [Zenodo repository](https://zenodo.org/record/7742803#.ZGPBrezML0o). Note that a asterisk* indicates files that are not publicly available.

### Extraction inputs
| File | Description | Source | Required | Zenodo |
| ----- | ----- | ----- | ----- | ----- |
| All_wells_20200417.xlsx | Geographical and other information for California wells | California Department of Conservation (DOC) | Yes | Yes |
| AllWells_20210427.csv | Geographical and other information for California wells | DOC | Yes | Yes |
| CaliforniaOilAndGasWellMonthlyProduction.csv | Well-level oil and gas monthly production data for 1977-2017 (file names are the same, folders indicate the years included in the data file) | DOC | Yes | Yes |
| CaliforniaOilAndGasWellMonthlyInjection.csv | Well-level oil and gas monthly injection data for 1977-2017 (file names are the same, folders indicate the years included in the data file) | DOC | Yes | Yes |
| CaliforniaOilAndGasWells.csv | Well information (12 files, same name, separate folders) | California Department of Conservation | Yes | Yes |
| county_codes.csv | County names and codes | Created using DOC online resources | Yes | Yes |
| CA_Counties_TIGER2016_noislands.shp | Spatial file: CA counties, no islands | https://data.ca.gov/dataset/ca-geographic-boundaries | Yes | Yes |
| DOGGR_Admin_Boundaries_Master.shp | Spatial file: field boundaries | DOC | Yes | Yes |
| oil_price_projections_revised.xlsx | Oil price projections | Energy Information Administration (EIA), International Energy Agency (IEA) | Yes | Yes |
| well_type_df.csv | Well types and abbreviations | Created using DOC online resources | Yes | Yes |
| Asset_opex_capex_govtt.csv* | Annual CapEx, OpEx, and government take (historic and future) | Rystad | Yes | No |
| resources_prod_myprod.csv* | Resource, production, and production under maximum allowed oil price ($120/bbl) by asset by year | Rystad | Yes | No |
| capex_per_recoverable_bbl.csv* | Annual CapEx value per recoverable barrel by asset | Rystad | Yes | No |
| asset-wells.csv* | Well (API) production by asset | Rystad | Yes | No |
| capex_per_bbl_nom.csv* | Annual CapEx per barrel (nominal) by asset | Rystad | Yes | No |
| opex_per_bbl_nom.csv* | Annual OpEx per barrel (nominal) by asset | Rystad | Yes | No |
| well_cost_per_eur.csv* | Cost of building each well | Rystad | Yes | No |
| ca_production.csv* | Annual California oil and gas production by asset, data type, and data source | Rystad | Yes | No |
| asset_latlon.csv* | Asset latitude and longitude | Rystad | Yes | No |
| wti_brent.csv* | Annual WTI and Brent price per barrel | Rystad | Yes | No |
| rystad_asset_rename.csv* | Rystad asset names and adjusted names to remove periods | Created using Rystad data | Yes | No |
| CA_Counties_TIGER2016.shp | California counties (shape file) | https://data.ca.gov/dataset/ca-geographic-boundaries | Yes | Yes |
| 2000_2019_ghg_inventory_trends_figures.xlsx | GHG emissions by sector | California Air Resources Board (CARB) | Yes | Yes |
| asset_econ_categories.csv* | Annual CapEx, OpEx, government take, exploration CapEx, and free cash flow by asset | Rystad | No | No |
| field_to_asset.csv* | Field and asset production days | Rystad | No | No |
| N9010CA2a.xls | California Natural Gas Gross Withdrawals | EIA | No | No |

### Health inputs
| File | Description | Source | Required | Zenodo |
| ----- | ----- | ----- | ----- | ----- |
| ces3results.xlsx | CalEnviroScreen 3.0 results | Office of Environmental Health Hazard Assessment (OEHHA) | Yes | Yes |
| nhgis0001_ts_geog2010_tract.csv | 2010 census tract population by age-group | National Historical Geographic Information System (NHGIS) | Yes | Yes |
| CDOF_p2_Age_1yr_Nosup.csv | County-level population projections by age-group | California Department of Finance (CDOF) | Yes | Yes |
| County_def.shp | County shape file | Downloaded from BenMAP-CE | Yes | Yes |
| age_group_desc.csv | Age groups for 2010 Census | Created from Codebook from NHGIS data file 'nhgis0001_ts_geog2010_tract' | Yes | Yes |
| Mortality Incidence (2015).csv | County by age-group mortality rates | Downloaded from BenMAP-CE | Yes | Yes |
| growth_rates.csv | Real GDP growth rates | Created from https://www.cbo.gov/publication/56442 | Yes | Yes |
| ces3_data.csv | Select CalEnviroScreen 3.0 results | Created from “ces3results.xlsx” | Yes | Yes |
| tl_2019_06_tract.shp | Spatial file: census tracts | Census | Yes | Yes |
| n/a (InMap data can be obtained from InMap open source repository) | InMap data and open source model | https://github.com/spatialmodel/inmap | Yes | No |
| n/a (BenMAP data can be obtained by downloading the software from the US EPA) | BenMAP Community Edition (BenMAP-CE) underlying data | https://www.epa.gov/benmap | Yes | No |

### Labor inputs
| File | Description | Source | Required | Zenodo |
| ----- | ----- | ----- | ----- | ----- |
| n/a (proprietary data can be obtained through a license with IMPLAN)* | County-specific employment and compensation multipliers for the oil extraction and refining industries | IMPLAN, 2018 edition (app.implan.com) | Yes | No |
| n/a (proprietary data can be obtained through a license with IMPLAN)* | County and industry-specific Multi-Region Input/Output analysis results. Includes direct, indirect, and induced impacts. | IMPLAN, 2018 edition (app.implan.com) | Yes | No |
| fte-convert.xlsx | Industry-specific ratios of the number of job-years per FTE worker | IMPLAN (https://implanhelp.zendesk.com/hc/en-us/articles/115002782053-IMPLAN-to-FTE-Conversions)  | Yes | Yes |

### Scenario inputs
| File | Description and source | Zenodo |
| ----- | ----- | ----- |
| innovation_scenarios.csv | Innovation scenario inputs developed for study | Yes |
| carbon_prices_revised.csv | Carbon price scenario inputs developed for study using California Air Resources Board: 2020 Annual Auction Reserve Price Notice (2019), [United States Government Interagency Working Group on Social Cost of Greenhouse Gases: Technical Update of the Social Cost of Carbon for Regulatory Impact Analysis Under Executive Order 12866. Technical report (August 2016)](https://www.google.com/url?q=https://www.epa.gov/sites/production/files/2016-12/documents/sc_co2_tsd_august_2016.pdf&sa=D&source=docs&ust=1679596494813551&usg=AOvVaw2B1wJUGw_53KMcaeR0uEYJ), and California Air Resources Board: Final Regulation Order: [California Cap on Greenhouse Gas Emissions and Market-based Compliance Mechanisms (2018)](https://ww2.arb.ca.gov/sites/default/files/barcu/regact/2018/capandtrade18/ct18fro.pdf?_ga=2.258682314.1729598153.1606172336-1333792675.1605911480). See Supplementary Information for more details. | Yes |
| ccs_extraction_scenarios.csv | CCS scenario inputs developed for study based on Stanford 2020 and Jing et al. 2020. Only scenarios without CCS (‘no CCS’) were included in our study. | Yes |
| CCS_LCFS_45Q.xlsx | CCS scenario inputs developed for study (https://sgp.fas.org/crs/misc/IF11455.pdf) | Yes |
| prod_quota_scenarios.csv | Production quota scenarios developed for this study. Our study ultimately did not review scenarios with a production quota.) | Yes |

`*` Indicates data files that are not public.


## Code scripts
This section describes the code scripts developed to conduct this study. All code related to this study is available in the GitHub repository `ca-transport-supply-decarb`. The headings below represent folder names and paths.

### health/
`scripts/health_data.R`
Calculate population increases to 2045.
- Inputs: 
  * nhgis0001_ts_geog2010_tract.csv
  * age_group_desc.csv
  * CDOF_p2_Age_1yr_Nosup.csv
  * County_def.shp
  * Mortality Incidence (2015).csv
- Outputs: 
  * ct_incidence_ca.csv
  * ct_inc_45.csv
  * ct_pop_45.csv

`scripts/source_receptor_matrix.R`
This script creates the Total PM2.5 (weighted and unweighted).
- Inputs: 
  * tl_2019_06_tract.shp
  * CA_Counties_TIGER2016_noislands.shp (from *External data*)
- Outputs: 
  * data/health/source_receptor_matrix/inmap_processed_srm/srm_XX_fieldYY.csv (where XX denotes the different pollutants –NH3, PM2.5, SOx, NOx, VOC–) and YY is the different clusters –1-26–). 

`scripts/obtain_field_cluster_xwalk.do`
Uses raw data from GIS to obtain the crosswalk between fields and clusters.
- Inputs:
  * extraction_fields_clusters_10km.csv (dataset obtained from creating 10 km buffers surrounding fields from ArcGIS)
  * extraction_fields_xwalk_id.csv (dataset obtained from crosswalks between the 10km buffers and corresponding fields ids from ArcGIS)
- Outputs:
  * extraction_fields_xwalk
  * extraction_field_clusters_xwalk

`scripts/srm_extraction_population.R`
Creates inputs for figures.
- Inputs:
  * ces3results_part.csv (dataset obtained by modifying names of the ces3results.xlsx dataset to be read in Rstudio)
  * srm_XX_fieldYY.csv, (where XX represents nox, pm25, sox, voc; and YY represents numbers 1-26. The numbers represent the 26 oil extraction field clusters) (from InMap, *External data*)
  * extraction_fields_clusters_10km.csv (created in ArcGIS)
- Outputs:
  * extraction_cluster_affectedpop.csv
  * extraction_xwalk.csv

### labor/processing/
`ica_multiplier_process.R`
This file creates ICA multipliers for the labor analysis for both the extraction and refining segments.
- Inputs (all inputs from *External data*. Files not listed must be obtained from IMPLAN): 
  * fte-convert.xlsx 
  * ica-emp-ext-kern.csv
  * ica-va-ext-kern.csv
  * ica-emp-drill-kern.csv
  * ica-va-drill-kern.csv
  * ica-emp-ref-kern.csv
  * ica-va-ref-kern.csv
  * ica-emp-ext-la.csv 
  * ica-va-ext-la.csv
  * ica-emp-drill-la.csv
  * ica-va-drill-la.csv
  * ica-emp-ref-la.csv
  * ica-va-ref-la.csv
  * ica-emp-ext-sb.csv
  * ica-va-ext-sb.csv
  * ica-emp-drill-sb.csv
  * ica-va-drill-sb.csv
  * ica-emp-ref-sb.csv
  * ica-va-ref-sb.csv
  * ica-emp-ext-monterey.csv
  * ica-va-ext-monterey.csv
  * ica-emp-drill-monterey.csv
  * ica-va-drill-monterey.csv
  * ica-emp-ext-ventura.csv
  * ica-va-ext-ventura.csv
  * ica-emp-drill-ventura.csv
  * ica-va-drill-ventura.csv
  * ica-emp-ext-orange.csv
  * ica-emp-ext-orange.csv
  * ica-emp-ext-orange.csv
  * ica-va-ext-orange.csv
  * ica-emp-drill-orange.csv
  * ica-va-drill-orange.csv
  * ica-emp-ext-fresno.csv
  * ica-va-ext-fresno.csv
  * ica-emp-drill-fresno.csv
  * ica-va-drill-fresno.csv
  * ica-emp-ext-cc.csv
  * ica-va-ext-cc.csv
  * ica-emp-ref-cc.csv
  * ica-va-ref-cc.csv
  * ica-emp-ref-solano.csv
  * ica-va-ref-solano.csv
  * ica-emp-ext-slo.csv
  * ica-va-ext-slo.csv
  * ica-emp-ref-slo.csv
  * ica-va-ref-slo.csv
  * ica-emp-ext-sanbenito.csv
  * ica-va-ext-sanbenito.csv
  * ica-emp-ext-sanbernardino.csv
  * ica-va-ext-sanbernardino.csv
  * ica-emp-ext-tulare.csv
  * ica-va-ext-tulare.csv
  * ica-emp-ext-sanmateo.csv
  * ica-va-ext-sanmateo.csv
  * ica-emp-ext-kings.csv
  * ica-va-ext-kings.csv
  * ica-emp-ext-alameda.csv
  * ica-va-ext-alameda.csv
  * ica-emp-ext-riverside.csv
  * ica-va-ext-riverside.csv
  * ica-emp-ext-santaclara.csv
  * ica-va-ext-santaclara.csv
  * ica-emp-ext-statewide.csv
  * ica-va-ext-statewide.csv
  * ica-emp-drill-statewide.csv
  * ica-va-drill-statewide.csv
  * ica-emp-ref-statewide.csv
  * ica-va-ref-statewide.csv
- Outputs: 
  * ica_multipliers_v2.xlsx
  * ica_multipliers_by_industry_long.csv

### energy/
#### data-processing-prep/

`stocks_flows.R`
This script re-organizes and cleans data relevant to the energy portion of our study. 
- Inputs:
  * Imports_of_Heavy_Sour_to_Los_Angeles_CA.csv (from *External data*)
  * PET_PRI_SPT_S1_M.xls (from *External data*)
  * PET_PRI_DFP2_K_M.xls (from *External data*)
  * MCRFPCA1m.xls (from *External data*)
  * EmissionsByFacility.csv (from *External data*)
  * oilgascounty.xls (from *External data*)
  * MCRFPCA1a.xls (from *External data*)
  * ghg_inventory_scopingplan_sum_2000-17.pdf (from *External data*)
  * Ca_oil_refinery_loc_cap.xlsx (from *External data*)
  * WeeklyFuelsWatch_Summary_2014-2020_North_South.xlsx (from *External data*)
  * WeeklyFuelsWatch_Summary_2014-2020_North_South.xlsx (from *External data*)
  * oil_supply_sources_a.csv (from *External data*)
  * PET_PRI_GND_DCUS_SCA_W.xls (from *External data*)
  * PET_PRI_GND_DCUS_NUS_W.xls (from *External data*)
  * ghg_sector_data_og.csv (from *External data*)
  * ghg_sector_data_refining2.csv (from *External data*)
  * county_codes.csv (from *External data*)
  * 2018CaliforniaOilAndGasWellMonthlyProduction.csv (from *External data*)
  * 2019CaliforniaOilAndGasWellMonthlyProduction.csv (from *External data*)
  * 2018CaliforniaOilAndGasWellMonthlyInjection.csv (from *External data*)
  * 2019CaliforniaOilAndGasWellMonthlyInjection.csv (from *External data*)
  * 2008-ghg-emissions-facility-2012-03-12.xlsx (from *External data*)
  * 2009-ghg-emissions-facility-2012-03-12.xlsx (from *External data*)
  * 2010-ghg-emissions-2015-06-15.xlsx (from *External data*)
  * 2011-ghg-emissions-2018-11-05.xlsx (from *External data*)
  * 2012-ghg-emissions-2019-11-04.xlsx (from *External data*)
  * 2013-ghg-emissions-2019-11-04.xlsx (from *External data*)
  * 2014-ghg-emissions-2019-11-04.xlsx (from *External data*)
  * 2015-ghg-emissions-2019-11-04.xlsx (from *External data*)
  * 2016-ghg-emissions-2019-11-04.xlsx (from *External data*)
  * 2017-ghg-emissions-2019-11-04.xlsx (from *External data*)
  * 2018-ghg-emissions-2019-11-04.xlsx (from *External data*)
  * ghg_sector_data_refining2.csv (from *External data*)
  * epa_emissions.csv (from *External data*)
  * Historic Permit CalWIMS.xlsx (from *External data*)
  * Crude Oil Receipts 1981-2020.xlsx (from *External data*)
  * PET_CRD_CRPDN_ADC_MBBL_A.xls (from *External data*)
  * Imports_of_Light_Sweet_to_California.csv (from *External data*)
  * California Transportion Fuel Consumption - Summary 2020-06-01 GDS.xlsx (from *External data*)
  * 050515staffreport_opgee.pdf (from *External data*)
  * ci_2012.csv (from *External data*)
  * ci_2013.csv (from *External data*)
  * ci_2014.csv (from *External data*)
  * ci_2015.csv (from *External data*)
  * ci_2016.csv (from *External data*)
  * ci_2017.csv (from *External data*)
  * ci_2018.csv (from *External data*)
  * wells_19.csv (from `clean_doc_prod.R`)
  * Total_Energy_Nominal_Prices_Brent.csv (from *External data*)
  * 2000_2019_ghg_inventory_trends_figures.xlsx
- Outputs:
  * refinery_capacity.csv (refining segment output)
  * fuel_watch_data.csv (refining segment output)
  * refining_emissions.csv (refining segment output)
  * ghg_mrr_2011-2018.csv (refining segment output)
  * ghg_mrr_2009-2010.csv (refining segment output)
  * ghg_mrr_2008.csv (refining segment output)
  * ghg_inventory_tool.csv (refining segment output)
  * reg_refin_crude_receipts.csv (refining segment output)
  * imports_to_refineries.csv (refining segment output)
  * ca_fuel_consumption_cec.csv (refining segment output)
  * crude_imports_port.csv (not required for analysis)
  * spot_price_wti_m.csv (not required for analysis)
  * domestic_crude_first_p_price_streams.csv (not required for analysis)
  * ca_crude_prod_m.csv (not required for analysis)
  * emissions_by_facility.csv (not required for analysis)
  * oil_gas_county.csv (not required for analysis)
  * ca_crude_prod_a.csv (not required for analysis)
  * summary_emissions.csv (not required for analysis)
  * domestic_import_crude.csv (not required for analysis)
  * ca_gas_d_prices.csv (not required for analysis)
  * usa_retail_p2.csv (not required for analysis)
  * oil_gas_emissions.csv (not required for analysis)
  * all_prod_test.csv (not required for analysis)
  * all_inject_test.csv (not required for analysis)
  * ghg_emissions_epa.csv (not required for analysis)
  * historic_permit_calwims.csv (not required for analysis)
  * eia_ca_crude_prod.csv (not required for analysis)
  * usa_crude_prod.csv (not required for analysis)
  * ci_streams_all_yrs.csv (not required for analysis)
  * brent_oil_price_projections.csv (not required for analysis)
  * field_codes.csv (not required for analysis)
  * indust_emissions_2000-2019.csv (extraction segment output)

`create_ccs_scenarios.R`
This script creates CCS scenarios for extraction and refining segments.
- Inputs:
  * ccs_extraction_scenarios.csv (from *External data*)
  * ccs_refining_scenarios.csv (from *External data*)
- Outputs:
  * ccs_extraction_scenarios_revised.csv
  * ccs_refining_scenarios_revised.csv

`social_cost_carbon.R`
This script creates a file with social cost of carbon information obtained from [Technical Support Document: Social Cost of Carbon and Nitrous Oxide Interim Estimates under Executive Order 13990](https://www.whitehouse.gov/wp-content/uploads/2021/02/TechnicalSupportDocument_SocialCostofCarbonMethaneNitrousOxide.pdf).
- Outputs:
  * social_cost_carbon.csv

`extraction/clean_doc_prod.R`
This script re-organizes WellSTAR well information, production, and injection data. 
- Inputs:
  * All_wells_20200417.xlsx (from *External data*)
  * CSV_1977_1985/CaliforniaOilAndGasWellMonthlyProduction.csv (from *External data*)
  * CSV_1977_1985/CaliforniaOilAndGasWellMonthlyInjection.csv (from *External data*)
  * CSV_1977_1985/CaliforniaOilAndGasWells.csv (from *External data*)
  * CSV_1986_1989/CaliforniaOilAndGasWellMonthlyProduction.csv (from *External data*)
  * CSV_1986_1989/CaliforniaOilAndGasWellMonthlyInjection.csv (from *External data*)
  * CSV_1986_1989/CaliforniaOilAndGasWells.csv (from *External data*)
  * CSV_1990_1994/CaliforniaOilAndGasWellMonthlyProduction.csv (from *External data*)
  * CSV_1990_1994/CaliforniaOilAndGasWellMonthlyInjection.csv (from *External data*)
  * CSV_1990_1994/CaliforniaOilAndGasWells.csv (from *External data*)
  * CSV_1995_1999/CaliforniaOilAndGasWellMonthlyProduction.csv (from *External data*)
  * CSV_1995_1999/CaliforniaOilAndGasWellMonthlyInjection.csv (from *External data*)
  * CSV_1995_1999/CaliforniaOilAndGasWells.csv (from *External data*)
  * CSV_2000_2004/CaliforniaOilAndGasWellMonthlyProduction.csv (from *External data*)
  * CSV_2000_2004/CaliforniaOilAndGasWellMonthlyInjection.csv (from *External data*)
  * CSV_2000_2004/CaliforniaOilAndGasWells.csv (from *External data*)
  * CSV_2005_2009/CaliforniaOilAndGasWellMonthlyProduction.csv (from *External data*)
  * CSV_2005_2009/CaliforniaOilAndGasWellMonthlyInjection.csv (from *External data*)
  * CSV_2005_2009/CaliforniaOilAndGasWells.csv (from *External data*)
  * CSV_2010_2014/CaliforniaOilAndGasWellMonthlyProduction.csv (from *External data*)
  * CSV_2010_2014/CaliforniaOilAndGasWellMonthlyInjection.csv (from *External data*)
  * CSV_2010_2014/CaliforniaOilAndGasWells.csv (from *External data*)
  * CSV_2015/CaliforniaOilAndGasWellMonthlyProduction.csv (from *External data*)
  * CSV_2015/CaliforniaOilAndGasWellMonthlyInjection.csv (from *External data*)
  * CSV_2015/CaliforniaOilAndGasWells.csv (from *External data*)
  * CSV_2016/CaliforniaOilAndGasWellMonthlyProduction.csv (from *External data*)
  * CSV_2016/CaliforniaOilAndGasWellMonthlyInjection.csv (from *External data*)
  * CSV_2016/CaliforniaOilAndGasWells.csv (from *External data*)
  * CSV_2017/CaliforniaOilAndGasWellMonthlyProduction.csv (from *External data*)
  * CSV_2017/CaliforniaOilAndGasWellMonthlyInjection.csv (from *External data*)
  * CSV_2017/CaliforniaOilAndGasWells.csv (from E*External data*)
  * CSV_2018/CaliforniaOilAndGasWellMonthlyProduction.csv (from *External data*)
  * CSV_2018/CaliforniaOilAndGasWells.csv (from *External data*)
  * CSV_2018/CaliforniaOilAndGasWellMonthlyInjection.csv (from *External data*)
  * CSV_2019/CaliforniaOilAndGasWellMonthlyProduction.csv (from *External data*)
  * CSV_2019/CaliforniaOilAndGasWellMonthlyInjection.csv (from *External data*)
  * CSV_2019/CaliforniaOilAndGasWells.csv (from *External data*)
  * county_codes.csv (from *External data*)
- Outputs:
  * well_prod_m.rds
  * well_inject_m.rds
  * wells_19.csv

`extraction/process-monthly-prod.R`
This script processes the monthly prod file to filter out gas fields, add needed columns, and rename columns.
- Inputs:
  * well_prod_m.rds (from `clean_doc_prod.R`)
  * wells_19.csv (from `clean_doc_prod.R`)
- Outputs:
  * data/stocks-flows/processed/well_prod_m_processed.csv
  * data/stocks-flows/processed/field_info.csv

`extraction/process-monthly-inj.R`
This script processes the monthly injection file to filter out gas fields, add needed columns, and rename columns.
- Inputs:
  * well_inject_m.rds (from `clean_doc_prod.R`)
  * wells_19.csv (from `clean_doc_prod.R`)
- Outputs:
  * data/stocks-flows/processed/well_inj_m_processed.csv

`extraction/opgee-carb-results.R`
This script takes raw outputs from the OPGEE model and organizes them into a long format.
- Inputs:
  * OPGEE_v2.0_with-CARB-inputs.xlsm (from *External data*)
  * opgee_field_names.csv (created externally)
- Outputs:
  * field-level-emissions-results_processed_revised.csv

`extraction/rystad_processing.R`
This script cleans data from Rystad.
- Inputs:
  * Asset_opex_capex_govtt.csv (from *External data*)
  * ca_production.csv (from *External data*)
  * asset_econ_categories.csv (from *External data*)
  * resources_prod_myprod.csv (from *External data*)
  * capex_per_recoverable_bbl.csv (from *External data*)
  * asset-wells.csv (from *External data*)
  * capex_per_bbl_nom.csv (from *External data*)
  * opex_per_bbl_nom.csv (from *External data*)
  * rystad_asset_rename.csv (from *External data*)
  * well_cost_per_eur.csv (from *External data*)
  * field_to_asset.csv (from *External data*)
  * well_prod_m_processed.csv (from `process-monthly-prod.R`)
- Outputs:
  * oil_asset_opex_capex_govtt_clean.csv
  * ca_oil_production.csv 
  * economically_recoverable_resources_scenarios.csv
  * economically_recoverable_resources_scenarios_wide.csv
  * capex_bbl_reserves.csv 
  * rystad_asset_apis.csv
  * field_rystad_match_apis_revised.csv
  * rystad_capex_bbl_nom_clean.csv 
  * rystad_opex_bbl_nom_clean.csv
  * well_cost_per_eur_clean.csv (not required for analysis)
  * rystad_field_asset.csv (not required for analysis)
  * asset_economics_cats.csv (not required for analysis)
  * ca_asset_opex_capex_govtt_clean.csv (not required for analysis)

`extraction/zero_prod.R`
This script creates a file used in the well exit analysis.
- Inputs:
  * well_prod_m_processed.csv (from `clean_doc_prod.R`)
  * AllWells_20210427.csv (from *External data*)
- Outputs:
  * no_prod_wells_out.csv

`extraction/income_data.R`
This script collects Census data on household income for California using the censusapi R package.
- Outputs:
  * ca-median-house-income.csv
  * ca-median-house-income-county.csv
  * scenario-prep

`ccs_parameterization.R`
This script contains the functions that calculate the cost of CCS for each field or refinery, depending on the amount of GHG emitted by the respective field or refinery.
- Inputs:
  * crude_prod_x_field_revised.csv (from `crude_prod_x_field.R`)
  * ghg_emissions_x_field_2018-2045.csv (from `forecast_ghg_emission_factors.R`)
  * refinery_ghg_emissions.csv (from `ghg_emissions.R`)
- Outputs: None

#### extraction-segment/model-prep 

`well_setback_sp_prep.R`
This file reads in FrackTracker’s sensitive receptor data and creates shapefiles representing the three different buffer areas.
- Inputs:
  * FracTrackerSetbackdata.gdb (layers SetbackOutlines_SR_Dwellings_082220, PlaygroundsinCities, DayCareCenters, reselderlyCare, CHHS_adultdayhealthcare_csv_Events, CHHS_altbirthing_csv_Events, CHHS_Dialysis_csv_Events, CHHS_healthcare_facility_locations_csv_Events, CHHS_intermedcarefac_csv_Events, CHHS_PrimaryCareClinic_csv_Events, CHHS_psychclinics_csv_Events,
CHHS_rehabclinic_csv_Events, CHHS_skillednursingfacs_csv_Events, CHHS_surgicalclinic_csv_Events, CHHS_acutecarehospital_csv_Events_1, CAAcuteCAreHostpitalslatlon_1, PrivSchoolsCA_1, SchoolPropCA_1, SchoolsCA_Sabins_1, from *External data*)
- Outputs:
  * buffer_1000ft.shp
  * buffer_2500ft.shp
  * buffer_5280.shp

`gen_well_setback_status.R`
This file creates the information needed for setback analysis (i.e., whether or not each well falls within buffer and the percentage of the field that gets covered by a buffer)
- Inputs:
  * buffer_1000ft.shp (from `well_setback_sp_prep.R`)
  * buffer_2500ft.shp (from `well_setback_sp_prep.R`)
  * buffer_5280.shp (from `well_setback_sp_prep.R`)
  * allwells_gis/Wells_All.shp (from *External data*)
  * DOGGR_Admin_Boundaries_Master.shp (from *External data*)
  * well_prod_m_processed.csv (from `process-monthly-prod.R`)
- Outputs:
  * wells_in_setbacks_revised.csv
  * setback_coverage_R.csv
  * coverage_map.html

`economically_recoverable_resources.R`
This script analyzes production and economically recoverable resources at the asset level.
- Inputs:
  * economically_recoverable_resources_scenarios_wide.csv (from `rystad_processing.R`)
- Outputs:
  * asset_sum_my-production.csv
  * asset_max_resources.csv
  * asset-year_cumulative_sum_production.csv
  * asset-year_production_my-production_resources.csv

`create_entry_econ_variables.R`
This script creates economic variables.
- Inputs:
  * oil_asset_opex_capex_govtt_clean.csv (from `rystad_processing.R`)
  * ca_oil_production.csv (from `rystad_processing.R`)
  * capex_bbl_reserves.csv  (from `rystad_processing.R`)
  * rystad_opex_bbl_nom_clean.csv (from `rystad_processing.R`)
  * rystad_capex_bbl_nom_clean.csv (from `rystad_processing.R`)
  * asset_sum_my-production.csv (from `economically_recoverable_resources.R`)
  * asset_max_resources.csv (from `economically_recoverable_resources.R`)
  * asset-year_cumulative_sum_production.csv (from `economically_recoverable_resources.R`)
  * asset-year_production_my-production_resources.csv (from `economically_recoverable_resources.R`)
- Outputs:
  * rystad_entry_variables.csv

`impute_costs.do`
This script (a) imputes the asset-level costs (capex and opex) for years in the historic period when the cost data is missing, and (b) extrapolates the asset-level costs into the future period.
- Inputs:
  * rystad_entry_variables.csv (from `create_entry_econ_variables.R`) 
- Outputs:
  * Rystad_cost_imputed_all_assets.csv

`init_yr_prod.R`
This script summarizes production by well and initial production year.
- Inputs:
  * well_prod_m_processed.csv (from `clean_doc_prod.R`)
- Outputs:
  * well_start_prod_api10_revised.csv

`match_fields_assets.R`
This script creates an output used in another script to make well entry inputs. This script matches fields to Rystad assets. This is the final version, which matches 1) first on well-level APIs, 2) the closest either a) field with economic data or b) asset.
- Inputs:
  * field_rystad_match_apis_revised.csv (from `rystad_processing.R`)
  * well_prod_m_processed.csv (from `process-monthly-prod.R`)
  * DOGGR_Admin_Boundaries_Master.shp (from *External data*)
  * asset_latlon.csv (from *External data*)
  * ca_oil_production.csv  (from `rystad_processing.R`)
  * Rystad_cost_imputed_all_assets.csv (from `impute_costs.do`)
- Outputs:
  * outputs/stocks-flows/entry-model-input/well_doc_asset_match_revised.csv
  * data/Rystad/data/processed/asset_latlon_adj.csv
  * data/Rystad/data/processed/fieldsAssets_adj_revised.csv
  * outputs/stocks-flows/entry-model-input/field_x_field_match_revised.csv
  * outputs/stocks-flows/entry-model-input/final/field_asset_matches_revised.csv

`create_entry_input.R`
This script creates an output used in another script to make well entry inputs. This script matches fields to Rystad assets. This is the final version, which matches 1) first on well-level APIs, 2) the closest either a) field with economic data or b) asset.
- Inputs:
  * field_asset_matches_revised.csv (from `match_fields_assets.R`)
  * oil_asset_opex_capex_govtt_clean.csv (from `rystad_processing.R`)
  * rystad_entry_variables.csv (from `create_entry_econ_variables.R`)
  * Rystad_cost_imputed_all_assets.csv (from `impute_costs.do`)
  * well_prod_m_processed.csv (from `process-monthly-prod.R`)
  * well_start_prod_api10_revised.csv (from `init_yr_prod.R`)
  * wti_brent.csv (from *External data*)
- Outputs:
  * outputs/stocks-flows/entry-input-df/final/field_capex_opex_forecast_revised.csv
  * outputs/stocks-flows/entry-input-df/final/docfield_asset_crosswalk_entrydf_revised.csv
  * outputs/stocks-flows/entry-input-df/final/entry_df_final_revised.csv

`entry.do`
This script prepares the data for the entry model (which predicts the number of new wells in each field in each year in the future period in `predict.do`).
- Inputs:
  * entry_df_final_revised.csv (from `create_entry_input.R`)
- Outputs:
  * entry_revised.dta
  * entry_revised_real.dta

`depl.do`
This script estimates the total oil resource of each field.
- Inputs:
  * entry_revised.dta (from `entry.do`) 
- Outputs:
  * field_resource_revised.csv 

`predict.do`
This script predicts the number of new wells in each field in each year in the future period.
- Inputs:
  * entry_revised.dta (from `entry.do`) 
- Outputs:
  * new_wells_pred_revised.csv 
  * poisson_regression_coefficients_revised.csv 

`tab_entryexit.do`
This script produces the regression outputs for Supplementary Table 1.
- Inputs:
  * entry_revised_real.dta (from `entry.do`)
  * exit_fields.dta (from `exit.do`)
- Outputs: None

`crude_prod_x_field.R`
This script summarizes annual production by field.
- Inputs:
  * well_prod_m_processed.csv (from `clean_doc_prod.R`)
- Outputs:
  * crude_prod_x_field_revised.csv
  
`field_county_production.R`
This script summarizes production by year, county, and field. Production by field can be attributed to multiple counties.
- Inputs:
  * well_prod_m_processed.csv (from `clean_doc_prod.R`)
- Outputs:
  * annual_field_county_production_proportion_revised.csv
  * annual_final_year_field_county_production_proportion_revised.csv

`field_emission_factors_2015.R`
This script creates field-level GHG emission factors for oil extraction. GHG factors are based on CARB’s LCFS OPGEE emission factors. 
- Inputs:
  * injection-by-well-type-per-field-per-year_1977-2018_revised.csv (from `injection-type-by-field.R`) 
  * field-level-emissions-results_processed_revised.csv (from `opgee-carb-results.R`)
  * entry_df_final_revised.csv (from `create_entry_input.R`) 
  * crude_prod_x_field_revised.csv (from `crude_prod_x_field.R`) 
- Outputs:
  * opgee_emission_factors_x_field_2015_revised.csv
  * ghg_emissions_x_field_2015_revised.csv

`county-setback.R`
This script creates a file used to make county-level figures. 
- Inputs:
  * CA_Counties_TIGER2016.shp (from *External data*) 
  * DOGGR_Admin_Boundaries_Master.shp (from *External data*) 
  * buffer_1000ft.shp (from `well_setback_sp_prep.R`)
  * buffer_2500ft.shp (from `well_setback_sp_prep.R`)
  * buffer_5280.shp (from `well_setback_sp_prep.R`)
  * well_prod_m_processed.csv (from `process-monthly-prod.R`)
  * crude_prod_x_field_revised.csv (from `crude_prod_x_field.R`) 
- Outputs:
  * county_level_setback_coverage.csv

`well_exits.R`
This script summarizes the production by plugged wells in the year leading up to their exit by field. The outputs of this script are not needed for the analysis, but are used for comparison in another script.
- Inputs:
  * well_prod_m_processed.csv (from `clean_doc_prod.R`)
  * AllWells_20210427.csv (from *External data*)
- Outputs:
  * well_exit_volume_x_field_v1_revised.csv

`prep_data_field_year.R`
This script preps the production data to be at the field-start year level.
- Inputs:
  * well_prod_m_processed.csv (from `process-monthly-prod.R`)
  * well_start_prod_api10_revised.csv (from `init_yr_prod.R`)
  * entry_df_final_revised.csv (from `create_entry_input.R`)
- Outputs:
  * field-year_peak-production_yearly.csv
  * production_field-year_yearly_entry.csv
  * production_api10_yearly_start_year.csv
  * adj_val_field-year_pred_prod.csv

`field-vintage-exit-rule.R`
This script creates an output based on an alternative exit rule (based on a production threshold). This rule is not ultimately used in the analysis.
- Inputs:
  * production_field-year_yearly_entry.csv (from `prep_data_field_year`)
  * well_exit_volume_x_field_v1_revised.csv (from `well_exits.R`)
- Outputs:
  * well_exits_under_rule.csv

`field-vintage-exit.R`
This script creates an output used in the well exit analysis.
- Inputs:
  * well_prod_m_processed.csv (from `clean_doc_prod.R`)
  * AllWells_20210427.csv (from *External data*)
  * no_prod_wells_out.csv (from `zero_prod.R`)
- Outputs:
  * well_exits.csv

`exit.do`
This script has the exit model. The outputs are used in the final extraction model.
- Inputs:
  * well_exits_under_rule.csv (from `field-vintage-exit-rule.R`)
  * well_exits.csv (from `field-vintage-exit.R`)
- Outputs:
  * well_exits_pred.csv
  * exit_regression_coefficients.csv 

`historic-extraction-emissions.R`
This script compiles historic oil and gas greenhouse gas emissions.
- Inputs:
  * well_prod_m.rds (from `clean_doc_prod.R`)
  * wells_19.csv (from `clean_doc_prod.R`)
  * N9010CA2a.xls (from *External data*)
- Outputs:
  * ghg_sector_data_og_updated.csv
  * historic_ghg_emissions_og.csv
  * historic_ghg_emissions_og_ng_adjusted.csv

`prep_data_field_vintage.R`
This script organizes oil production data to the field-vintage level.
- Inputs:
  * well_prod_m_processed.csv (from `clean_doc_prod.R`)
well_start_prod_api10_revised.csv (from `init_yr_prod.R`) 
entry_df_final_revised.csv (from `create_entry_input.R`)
- Outputs:
  * field-vintage_peak-production_yearly_revised.csv 
  * production_field-vintange_yearly_entry_revised.csv
  * production_api10_monthly_revised.csv 

`decline_parameters_field_start_year.R`
This script parameterizes decline at field-start year level.
- Inputs:
  * production_field-year_yearly_entry.csv (from `prep_data_field_year.R`)
  * field-year_peak-production_yearly.csv (from `prep_data_field_year.R`)
  * entry_df_final_revised.csv (from `create_entry_input.R`)
- Outputs:
  * fitted-parameters_field-start-year_yearly_entry.csv

`predict_existing_production.R`
This script predicts production from existing wells and saves the outputs to be used in the extraction module.
- Inputs:
  * production_api10_yearly_start_year.csv (from `prep_data_field_year.R`)
  * fitted-parameters_field-start-year_yearly_entry.csv (from `decline_parameters_field_start_year.R`)
  * field-year_peak-production_yearly.csv (from `prep_data_field_year.R`)
  * adj_val_field-year_pred_prod.csv (from `prep_data_field_year.R`)
  * wells_in_setbacks_revised.csv (from `gen_well_setback_status.R`)
- Outputs:
  * pred_prod_no_exit_2020-2045_field_start_year_revised.csv

`analyze-parameters.R`
This script conducts analysis and creates plots related to decline curve parameters. The output is used in v1 of the model, but not the final model. 
- Inputs:
  * crude_prod_x_field_revised.csv (from `crude_prod_x_field.R`)
  * fitted-parameters_field-start-year_yearly_entry.csv (from `decline_parameters_field_start_year.R`)
  * entry_df_final_revised.csv (from `create_entry_input.R`)
- Outputs:
  * forecasted_decline_parameters_2020_2045.csv

`extraction_fields.R`
This script creates a list of fields included in the projection analysis.
- Inputs:
  * entry_df_final_revised.csv (from `create_entry_input.R`)
  * pred_prod_no_exit_2020-2045_field_start_year_revised.csv (from `predict_existing_production.R`)
  * DOGGR_Admin_Boundaries_Master.shp (from *External data*)
- Outputs:
  * extraction_fields.shp

`extra/injection-type-by-field.R`
This script summarizes the types of injection by each field and by each field-year.
- Inputs:
  * well_inj_m_processed.csv (from `clean_doc_prod.R`)
  * well_type_df.csv (from *External data*)
Outputs:
  * injection-by-well-type-per-field_1977-2018_revised.csv
  * injection-by-well-type-per-field-per-year_1977-2018_revised.csv

`forecast_ghg_emission_factors.R`
This script takes the updated OPGEE emission factors (modeled at various historical years), linearly regresses the emission factors, and projects emission factors for all fields until 2045.
- Inputs:
  * Various saved OPGEE results files (copy and pasted from the OPGEE spreadsheet)
  * crude_prod_x_field_revised.csv (from `crude_prod_x_field.R`)
  * entry_df_final_revised.csv (from `create_entry_input.R`)
  * injection-by-well-type-per-field-per-year_1977-2018_revised.csv (from `injection-type-by-field.R`) 
- Outputs:
  * ghg_emissions_x_field_2018-2045.csv
  * ghg_emissions_x_field_historic.csv

#### extraction-segment/model/full-run-revised

`input_prep/emissions-target-90.R`
This script calculates a 90% reduction in GHG emissions for scenario development.
- Inputs:
  * indust_emissions_2000-2019.csv (from `stocks_flows.R`)
- Outputs:
  * emission_reduction_90.csv

`input_prep/prep-excise-non-target.R`
This script creates scenario input information for excise tax scenarios.
- Inputs: None
- Outputs:
  * excise_tax_non_target_scens.csv
  
`target-functions/func_calc_2045_ghg.R`
This script contains a function used in the final model to determine a 2045 GHG emissions target. 

`target-functions/func_calc_carbonpx.R`
This script contains a function that calculates a stream of carbon prices given a start value.

`target-functions/func_calc_inputs_and_ghg.R`
This script contains a function that prepares input values for the final model.

`target-functions/optim_functions.R`
This script contains functions that find the excise/carbon tax values that result in a target 2045 GHG emission value.

`load_input_info.R`
This script loads input info for the final model. The user will need to update the file paths This script is sourced in `00_extraction_steps.R`.
- Inputs:
  * entry_df_final_revised.csv (from `create_entry_input.R`)
  * poisson_regression_coefficients_revised.csv (from `predict.do`)
  * forecasted_decline_parameters_2020_2045.csv (from `analyze-parameters.R`)
  * field-year_peak-production_yearly.csv (from `prep_data_field_year.R`)
  * pred_prod_no_exit_2020-2045_field_start_year_revised.csv (from `predict_existing_production.R`)
  * crude_prod_x_field_revised.csv (from `crude_prod_x_field.R`)
  * exit_regression_coefficients.csv (from `exit.do`)
  * field_capex_opex_forecast_revised.csv (from `create_entry_input.R`)
  * field_resource_revised.csv (from `depl.do`)
  * oil_price_projections_revised.xlsx (from *External data*)
  * innovation_scenarios.csv (from *Scenario inputs*)
  * carbon_prices_revised.csv (from *Scenario inputs*)
  * ccs_extraction_scenarios_revised.csv (from *Scenario inputs*)
  * ghg_emissions_x_field_2018-2045.csv (from `forecast_ghg_emission_factors.R`)
  * setback_coverage_R.csv (from `gen_well_setback_status.R`)
  * prod_quota_scenarios.csv (from *Scenario inputs*)
  * excise_tax_non_target_scens.csv (from `prep-excise-non-target.R`)
  * CCS_LCFS_45Q.xlsx (from *Scenario inputs*)
  * n_wells_area.csv (from `predict_existing_production.R`)
  * emission_reduction_90.csv (from `emissions-target-90.R`)
	
`scenario-list-targets.R`
This script creates a list of scenarios that is used in `00_extraction_steps.R`.
- Inputs:
  * oil_price_projections_revised.xlsx (from *External data*)
  * innovation_scenarios.csv (from *Scenario inputs*)
  * carbon_prices_revised.csv (from *Scenario inputs*)
  * ccs_extraction_scenarios_revised.csv (from *Scenario inputs*)
  * setback_coverage_R.csv (from `gen_well_setback_status.R`)
  * prod_quota_scenarios_with_sb.csv (from `setback_quota_scenarios.R`)
  * prod_quota_scenarios.csv (from *Scenario inputs*)
  * excise_tax_non_target_scens.csv (from `prep-excise-non-target.R`)
  * CCS_LCFS_45Q.xlsx (from *Scenario inputs*)
- Output:
  * scenario_id_list_targets.csv

`00_extraction_steps.R`
This script runs the final extraction model.
- Inputs:
  * scenario_id_list_targets.csv (from `scenario-list-targets.R`)
- Outputs:
  * None

`fun_extraction_model_targets.R`
This script runs the final model. This script is sourced in `00_extraction_steps.R`.
- Outputs:
  * run_info.csv
  * XX_vintage.csv (where XX represents a scenario id)
  * XX_field.rds
  * XX_state.rds
  * XX_density.csv
  * XX_exit.csv
  * XX_depletion.csv

#### extraction-segment/output-review

`review_target_out.R`
This script reviews the outputs of each scenario (particularly 2045 GHG emissions values compared to target values).
- Inputs:
  * All state-level outputs from `fun_extraction_model_targets.R`
- Outputs: None
  * compile-outputs

`compile_extraction_outputs_full.R`
This file compiles the energy, labor, and health outputs for the scenarios.
- Input files:
  * well_prod_m_processed.csv (from `process-monthly-prod.R`)
  * oil_price_projections_revised.xlsx (from *External data*)
  * ghg_emissions_x_field_2018-2045.csv (from `forecast_ghg_emission_factors.R`)
  * indust_emissions_2000-2019.csv (from `stocks_flows.R`)
  * ces3results.xlsx (from *External data*)
  * ca-median-house-income.csv (from `income_data.R`)
  * ca-median-house-income-county.csv (from `income_data.R`)
  * ica_multipliers_v2.xlsx (from `ica_multiplier_process.R`)
  * XX/srm_nh3_fieldYY.csv (where XX represents folders nox, pm25, sox, voc; and YY represents numbers 1-26. The numbers represent the 26 oil extraction field clusters) (from InMap, *External data*)
  * extraction_fields_clusters_10km.csv (created in ArcGIS)
  * extraction_fields_xwalk_id.dbf (created in ArcGIS)
  * ces3_data.csv (from *External data*)
  * ct_inc_45.csv (from `health_data.R`)
  * growth_rates.csv (from *External data*)
- Output files:
  * XX_state_results.rds (where XX represents a scenario id)
  * XX_ct_results.rds (where XX represents a scenario id)
  * XX_county_results.rds (where XX represents a scenario id)

`compile_subset_csvs.R`
This file creates summary csvs of the extraction segment outputs.
- Inputs:
  * scenario_id_list_targets.csv
  * XX_ct_results.rds (where XX represents a scenario id)
  * XX_county_results.rds (where XX represents a scenario id)
  * XX_state_results.rds (where XX represents a scenario id)
- Outputs:
  * subset_census_tract_results.csv
  * subset_county_results.csv
  * subset_state_results.csv

#### figs-and-results/

`fig_outputs.R`
This script creates outputs needed to make the figures for the manuscript.
- Inputs:
  * indust_emissions_2000-2019.csv (from `stocks_flows.R`)
  * social_cost_carbon.csv (from `social_cost_carbon.R`)
  * carbon_price_scenarios_revised.xlsx (from *Scenario inputs*)
  * growth_rates.csv (from *External data*)
  * ct_inc_45.csv (from `health_data.R`)
  * subset_state_results.csv (from `compile_subset_csvs.R`)
  * subset_census_tract_results.csv (from `compile_subset_csvs.R`)
  * extraction_field_cluster_xwalk.csv (from `source_receptor_matrix.do`)
- Outputs:
  * subset_state_results.csv
  * state_levels_all_oil.csv
  * npv_x_metric_all_oil.csv
  * dac_health_labor_all_oil.csv
  * dac_bau_health_labor_all_oil
  * state_dac_ratios.csv

`field_characteristics.R`
This script creates an output used in figures.
- Inputs:
  * reference case_no_setback_no quota_price floor_no ccs_low innovation_no tax_field_results.rds (from `compile_extraction_outputs_full.R`)
  * reference case_no_setback_no quota_price floor_no ccs_low innovation_no tax_county_results.rds (from `compile_extraction_outputs_full.R`)
  * field_capex_opex_forecast_revised.csv (from `create_entry_input.R`)
  * ghg_emissions_x_field_2018-2045.csv (from `forecast_ghg_emission_factors.R`)
  * setback_coverage_R.csv (from `gen_well_setback_status.R`)
  * well_prod_m_processed.csv (from `process-monthly-prod.R`)
  * ces3results.xlsx (from *External data*)
  * ct_inc_45.csv (from `health_data.R`)
- Outputs:
  * field_characteristics.csv
  * county_characteristics.csv

`figure_themes.R`
This file has figures themes and is sourced in the scripts that create figures for the manuscript.
- Inputs: None
- Outputs: None

`figure1.R`
This script creates the components of figure 1.
- Inputs:
  * well_prod_m_processed.csv (from `process-monthly-prod.R`)
  * reference case_no_setback_no quota_price floor_no ccs_low innovation_no tax_ct_results.rds (from `compile_extraction_outputs_full.R`)
  * reference case_no_setback_no quota_price floor_no ccs_low innovation_no tax_county_results.rds (from `compile_extraction_outputs_full.R`)
  * extraction_fields.shp (from `extraction_fields.R`)
  * DOGGR_Admin_Boundaries_Master.shp (from *External data*)
  * CA_Counties_TIGER2016.shp (from *External data*)
  * CA_Counties_TIGER2016_noislands.shp (from *External data*)
  * tl_2019_06_tract.shp (from *External data*)
  * ces3results.xlsx (from *External data*) 
  * new_wells_pred_revised.csv (from `predict.do`) 
  * extraction_fields_clusters_10km.csv (created in ArcGIS)
  * extraction_fields_xwalk_id.dbf  (created in ArcGIS)
- Outputs:
  * Components of figure 1 (arranged together in Adobe Illustrator)

`figure2.R`
This script creates figure 2 in the main text and figures in the Supplementary Information.
- Inputs:
  * state_levels_all_oil.csv (from `fig_outputs.R`)
- Outputs:
  * figure2-ref-case.pdf/csv
  * figure2-low.pdf/csv
  * figure2-high.pdf/csv

`figure3.R`
This script creates figures 2 and 3 in the main text and additional figures in the Supplementary Information.
- Inputs:
  * npv_x_metric_all_oil.csv (from `fig_outputs.R`)
  * dac_bau_health_labor_all_oil.csv (from `fig_outputs.R`)
- Outputs:
  * figure3-ref-case.pdf/png
  * figure3-low.pdf/png
  * figure3-high.pdf/png
  * figure5-refcase-relBAU.pdf/png

`figure6.R`
This script creates figure 6 in the main text and additional figures in the Supplementary Information.
- Inputs:
  * state_levels_all_oil.csv (from `fig_outputs.R`)
  * npv_x_metric_all_oil.csv.csv (from `fig_outputs.R`)
  * dac_bau_health_labor_all_oil.csv (from `fig_outputs.R`)
- Outputs:
  * figure6-ref-case.pdf
  * figure6-ref-case.png

`calc_values.R`
This script calculates values used in the main text and Supplementary Information
- Inputs:
  * state_levels_all_oil.csv (from `fig_outputs.R`)
- Outputs:
  * none

`prep_files_for_figs.R`
This script preps oil price data for figures
- Inputs:
  * oil_price_projections_revised.xlsx.csv (from *External data*)
  * wti_brent.csv (from *External data*)
`Outputs:`
  * historial_brent.csv

`si/capex-opex-figs.R`
This file makes figures of opex and capex (historic and future projections).
- Inputs:
  * field_capex_opex_forecast_revised.csv (from `create_entry_input.R`)
  * entry_df_final_revised.csv (from `create_entry_input.R`)
  * well_prod_m_processed.csv (from `process-monthly-prod.R`)
- Outputs:
  * projected-capex-opex-si-fig.png
  * historical-capex-opex-si-fig.png

`si/county-level-figs.R`
This script makes county-level figures.
- Inputs:
  * ghg_emissions_x_field_2018-2045.csv (from `forecast_ghg_emission_factors.R`)
  * well_prod_m_processed.csv (from `process-monthly-prod.R`)
  * field_capex_opex_forecast_revised.csv (from `create_entry_input.R`)
  * setback_coverage_R.csv (from `gen_well_setback_status.R`)
  * subset_census_tract_results.csv (from `compile_subset_csvs.R`)
  * county_level_setback_coverage.csv (from `county-setback.R`)
  * extraction_field_cluster_xwalk.csv  (from `source_receptor_matrix.do`)
  * oil_price_projections_revised.xlsx (from *External data*)
  * well_inj_m_processed.csv (from `clean_doc_prod.R`)
  * ces3results.xlsx (from *External data*)
  * ica_multipliers_v2.xlsx (from `ica_multiplier_process.R`)
  * CA_Counties_TIGER2016.shp (from *External data*) 
- Outputs:

`si/entry-exit-figs.R`
This script makes well entry and exit figures.
- Inputs:
  * well_prod_m_processed.csv (from `process-monthly-prod.R`)
  * new_wells_pred_revised.csv (from `predict.do`)
  * well_exits_pred.csv (from `exit.do`)
- Outputs:
  * pred_fullsample_topfield.png
  * pred_fullsample_state.png
  * pred_exit_topfield.png
  * pred_exit_state.png

`si/si-results.R`
This script makes figures for the supplementary document.
- Inputs:
  * state_levels_all_oil.csv (from `fig_outputs.R`)
  * oil_price_projections_revised.xlsx (from *External data*)
  * carbon_prices_revised.csv (from *External data*)
  * well_prod_m_processed.csv (from `process-monthly-prod.R`)
- Outputs:
  * si-oil-prod-oilpx.png
  * si-ghg-oilpx.png
  * si-prod-setback.png
  * si-ghg-setback.png
  * si-excise-tax-fig.png
  * si-carbon-tax-fig.png
  * hist-future-prod.png

`si/si-macro-econ-figs.R`
This script makes figures for the supplementary document.
- Inputs:
  * oil_price_projections_revised.xlsx (from *External data*)
  * carbon_prices_revised.csv (from *External data*)
- Outputs:
  * Figures

`si/ghg_x_cost.R`
This script makes figures showing GHG intensity by cost of production.
- Inputs:
  * ghg_emissions_x_field_2018-2045.csv (from `forecast_ghg_emission_factors.R`)
  * well_prod_m_processed.csv (from `process-monthly-prod.R`)
  * field_capex_opex_forecast_revised.csv (from `create_entry_input.R`)
- Outputs:
  * ghg_x_capex_plus_opex.pdf
  * ghg_x_production_cost.pdf

`si/clusters.R`
This script makes figures showing GHG intensity by cost of production.
- Inputs:
  * CA_Counties_TIGER2016.shp (from *External data*)
  * CA_Counties_TIGER2016_noislands.shp (from *External data*)
  * extraction_fields_clusters_10km.shp (dataset obtained from creating 10 km buffers surrounding fields from ArcGIS)
- Outputs:
  * cluster_si_fig.png
  * cluster_si_fig.pdf


#### mechanism

`scripts/mechanisms.do`
This STATA script generates the cross-sectional panels in Fig. 4 of the main text and Fig. S33 in the SI. It requires changing the local file path at the top of the script.
- Inputs:
  * county_characteristics.csv (from `field_characteristics.R`)
  * extraction_cluster_affectedpop.csv (from `scripts/srm_extraction_population.R`)
  * extraction_field_cluster_xwalk.csv (from `scripts/obtain_field_cluster_xwalk.do`)
  * field_characteristics.csv (from `field_characteristics.R`)
- Outputs:
  * Health_emp_mechanism_setback_2500.jpg (Fig. 4 in main text)
  * cluster_ghg_intensity_cost.jpg (Fig. S33 in SI)

## Zenodo repository
This section lists the files that are included in the Zenodo repository. It includes all publically available input files, intermediate files needed to run the extraction model, model outputs (including extraction, health, and labor outputs), and files needed to create figures and results in the manuscript. **Please note that the user will need to change the file paths and change how some objects are defined before running the codes.** A full description of the scripts can be found in the readme for the manuscript’s GitHub repository ca-transport-supply-decarb. Due to data confidentiality, the user can only run a subset of the scripts. Thus, we provide all of the intermediate outputs needed to run the following scripts:
- `ca-transport-supply-decarb/energy/extraction-segment/model/full-run-revised/00_extraction_steps.R` - this script runs the energy model that results in oil extraction outputs. To successfully run the energy model, the user should make the following changes in the script:
  * `Set zenodo_repo <- TRUE` 
  * Define `zenodo_user_path`, which should be the user-specific path that leads up to the `ca-transport-supply-decarb-files` folder downloaded from Zenodo
  * Define a `save_path`, which represent the path to where the user would like the outputs to be saved
  * Define a `run_name`, which is used with the current to generate a folder to save outputs (this folder will be created in the save_path location)
- `ca-transport-supply-decarb/energy/extraction-segment/figs-and-results/fig_outputs.R` - this script takes the energy, health, and labor outputs and computes values for the manuscript (i.e., outputs needed to make figures and results presented in the paper). To successfully run the code, the user should make the following changes to the script:
  *  `Set zenodo_repo <- TRUE` 
  *  Define `zenodo_user_path`, which should be the user-specific path that leads up to the `ca-transport-supply-decarb-files` folder downloaded from Zenodo
  * Define `save_info_path`, which is a path specifying where the user wants the outputs to be saved
  * Zenodo users do not need to specify `energy_result_date` or `comp_result_date`. 
 - The scripts contained in the `ca-transport-supply-decarb/energy/extraction-segment/figs-and-results/` folder (these scripts create figures and values presented in the manuscript). The files `figure2.R`, `figure3.R`, and `figure6.R` are set up so that the user can set `zenodo_repo <- TRUE` and specify a `zenodo_user_path` and `zenodo_save_path`. For all other scripts in this folder, the user will need to update file paths.  

### List of files contained in [Zenodo repository](https://zenodo.org/record/7742803#.ZGPBrezML0o)
**ca-transport-supply-decarb-files/**
- inputs/
  * gis/
    - CA_counties_noislands/
      * CA_Counties_TIGER2016_noislands.shp (from *External data*)
    - field-boundaries/
      * DOGGR_Admin_Boundaries_Master.shp (from *External data*)
    - CA_Counties/
      * CA_Counties_TIGER2016.shp (from *External data*) 
    - census-tract/
      * tl_2019_06_tract.shp (from *External data*) 
  * extraction/
    - monthly-prod-inj-wells/ (from *External data*) 
      * CSV_1977_1985/
        - CaliforniaOilAndGasWellMonthlyProduction.csv 
        - CaliforniaOilAndGasWellMonthlyInjection.csv
        - CaliforniaOilAndGasWells.csv
      * CSV_1986_1989/
        - CaliforniaOilAndGasWellMonthlyProduction.csv
        - CaliforniaOilAndGasWellMonthlyInjection.csv
        - CaliforniaOilAndGasWells.csv
      * CSV_1990_1994/
        - CaliforniaOilAndGasWellMonthlyProduction.csv
        - CaliforniaOilAndGasWellMonthlyInjection.csv
        - CaliforniaOilAndGasWells.csv
      * CSV_1995_1999/
        - CaliforniaOilAndGasWellMonthlyProduction.csv
        - CaliforniaOilAndGasWellMonthlyInjection.csv
        - CaliforniaOilAndGasWells.csv
      * CSV_2000_2004/
        - CaliforniaOilAndGasWellMonthlyProduction.csv
        - CaliforniaOilAndGasWellMonthlyInjection.csv
        - CaliforniaOilAndGasWells.csv
      * CSV_2005_2009/
        - CaliforniaOilAndGasWellMonthlyProduction.csv
        - CaliforniaOilAndGasWellMonthlyInjection.csv
        - CaliforniaOilAndGasWells.csv
      * CSV_2010_2014/
        - CaliforniaOilAndGasWellMonthlyProduction.csv
        - CaliforniaOilAndGasWellMonthlyInjection.csv
        - CaliforniaOilAndGasWells.csv
      * CSV_2015/
        - CaliforniaOilAndGasWellMonthlyProduction.csv
        - CaliforniaOilAndGasWellMonthlyInjection.csv
        - CaliforniaOilAndGasWells.csv
      * CSV_2016/
        - CaliforniaOilAndGasWellMonthlyProduction.csv
        - CaliforniaOilAndGasWellMonthlyInjection.csv
        - CaliforniaOilAndGasWells.csv
      * CSV_2017/
        - CaliforniaOilAndGasWellMonthlyProduction.csv
        - CaliforniaOilAndGasWellMonthlyInjection.csv
        - CaliforniaOilAndGasWells.csv
      * CSV_2018/
        - CaliforniaOilAndGasWellMonthlyProduction.csv
        - CaliforniaOilAndGasWellMonthlyInjection.csv
        - CaliforniaOilAndGasWells.csv
      * CSV_2019/
        - CaliforniaOilAndGasWellMonthlyProduction.csv
        - CaliforniaOilAndGasWellMonthlyInjection.csv
        - CaliforniaOilAndGasWells.csv
    - All_wells_20200417.xlsx (from *External data*) 
    - AllWells_20210427.csv (from *External data*) 
    - county_codes.csv (from *External data*) 
    - oil_price_projections_revised.xlsx (from *External data*) 
    - well_type_df.csv (from *External data*) 
    - 2000_2019_ghg_inventory_trends_figures.xlsx (from *External data*) 
  * health/
    - ces3results.xlsx (from *External data*) 
    - nhgis0001_ts_geog2010_tract.csv (from E*External data*) 
    - CDOF_p2_Age_1yr_Nosup.csv (from *External data*) 
    - County_def.shp (from *External data*) 
    - age_group_desc.csv (from *External data*) 
    - Mortality Incidence (2015).csv (from *External data*) 
    - growth_rates.csv (from *External data*) 
    - ces3_data.csv (from *External data*) 
  * labor/   
    - fte-convert.xlsx (from *External data*) 
  * scenarios/
    - innovation_scenarios.csv (from *External data*a) 
    - carbon_prices_revised.csv (from *External data*) 
    - ccs_extraction_scenarios.csv (from *External data*) 
    - ccs_extraction_scenarios_revised.csv  (from *Scenario inputs*)
    - CCS_LCFS_45Q.xlsx (from *External data*) 
    - prod_quota_scenarios.csv (from *External data*) 
- intermediate/
  * extraction-model/
    - refinery_ghg_emissions.csv
    - entry_df_final_revised.csv (from `create_entry_input.R`)
    - poisson_regression_coefficients_revised.csv (from `predict.do`)
    - forecasted_decline_parameters_2020_2045.csv (from `analyze-parameters.R`)
    - field-year_peak-production_yearly.csv (from `prep_data_field_year.R`)
    - pred_prod_no_exit_2020-2045_field_start_year_revised.csv (from `predict_existing_production.R`)
    - crude_prod_x_field_revised.csv (from `crude_prod_x_field.R`)
    - exit_regression_coefficients.csv (from `exit.do`)
    - field_capex_opex_forecast_revised.csv (from `create_entry_input.R`)
    - field_resource_revised.csv (from `depl.do`)
    - ghg_emissions_x_field_2018-2045.csv (from `forecast_ghg_emission_factors.R`)
    - setback_coverage_R.csv (from `gen_well_setback_status.R`)
    - excise_tax_non_target_scens.csv (from `prep-excise-non-target.R`)
    - n_wells_area.csv (from `predict_existing_production.R`)
    - emission_reduction_90.csv (from `emissions-target-90.R`)
    - prod_quota_scenarios_with_sb.csv (from `setback_quota_scenarios.R`) 
    - scenario_id_list_targets.csv (from `scenario-list-targets.R`)
  * inmap-processed-srm-extraction/
    - nh3/srm_nh3_field1.csv (26 files, 1-26) (from InMap, *External data*)
    - nox/srm_nox_field1.csv (26 files, 1-26) (from InMap, *External data*)
    - pm25/srm_pm25_field1.csv (26 files, 1-26) (from InMap, *External data*)
    - sox/srm_sox_field1.csv (26 files, 1-26) (from InMap, *External data*)
    - voc/srm_voc_field1.csv (26 files, 1-26) (from InMap, *External data*)
- outputs/
  * model-out/
    - subset_census_tract_results.csv (from `compile_subset_csvs.R`)
    - subset_county_results_adj.csv (from `compile_subset_csvs.R`)
    - subset_state_results.csv (from `compile_subset_csvs.R`)
  * fig-and-results-out/
    - state_levels_all_oil.csv (from `fig_outputs.R`)
    - npv_x_metric_all_oil.csv (from `fig_outputs.R`)
    - dac_bau_health_labor_all_oil.csv (from `fig_outputs.R`)
    - field_characteristics.csv (from `field_characteristics.R`)
    - county_characteristics.csv (from `field_characteristics.R`)
    - extraction_cluster_affectedpop.csv (from `field_characteristics.R`)
    - extraction_field_cluster_xwalk.csv (from `field_characteristics.R`)
    - well_prod_m_processed.csv (from `process-monthly-prod.R`)
    - extraction_fields.shp (from `extraction_fields.R`)
    - new_wells_pred_revised.csv (from `predict.do`)
    - reference case-no_setback-no quota-price floor-no ccs-low innovation-no tax-0_ct_results.rds (from `fun_extraction_model_targets.R`)
    - county_level_out_adjusted.csv (use instead of reference case-no_setback-no quota-price floor-no ccs-low innovation-no tax-0_county_results.rds) (from `fun_extraction_model_targets.R`)
    - extraction_fields_clusters_10km.csv (dataset obtained from creating 10 km buffers surrounding fields from ArcGIS)
    - extraction_fields_xwalk_id.dbf (created in ArcGIS)
    - social_cost_carbon.csv (from `social_cost_carbon.R`)
    - ct_inc_45.csv (from `health_data.R`)
    - growth_rates.csv (from *External data*) 
    - indust_emissions_2000-2019.csv (from `stocks_flows.R`)

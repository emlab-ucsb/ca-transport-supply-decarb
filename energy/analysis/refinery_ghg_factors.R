# calculate refining ghg emission factors (cluster-level and refinery-level)
# created: october 12, 2020
# author: meas meng

# inputs --------

  proj_path     = '/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/calepa-cn'
  fw_file       = 'fuel_watch_data.csv'
  ghg_file      = 'refinery_ghg_emissions.csv'
  cap_file      = 'refinery_loc_cap.csv'

# outputs --------
  
  save_path     = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs/stocks-flows'
  
# load packages ------
  
  library(data.table)

# import data -------
  
  # import crude consumption data
    cons_dt = fread(file.path(proj_path, 'data/stocks-flows/processed', fw_file), header = T)
  
  # import ghg emissions data
    ghg_dt = fread(file.path(proj_path, 'outputs/stocks-flows', ghg_file), header = T)
    
  # import refinery capacity data
    cap_dt = fread(file.path(proj_path, 'data/stocks-flows/processed', cap_file), header = T)
    setnames(cap_dt, 'cluster', 'region')
    
# get crude oil consumption and aggregate to the region level ------
    
  cons_region = cons_dt[stock_flow == 'Refinery Input' & category == 'Crude Oil']
  cons_region = cons_region[, .(thous_barrels = sum(thous_barrels, na.rm = T)), by = .(year, region)]
  cons_region[, region_barrels := thous_barrels*1e3]
  
# match refinery emissions to region and aggregate by region ------  
  
  ghg_region = cap_dt[ghg_dt, on = 'site_id', nomatch = 0]
  ghg_region = ghg_region[, .(region_co2e_tonnes = sum(adj_total_co2e, na.rm = T),
                              region_co2e_megatonnes = sum(adj_total_Mt_co2e, na.rm = T)),
                          by = .(year, region)]
  ghg_region[, region_co2e_kg := region_co2e_tonnes*1e3]
  
# combine region-level emissions with region-level production to get region-level emissions factors ------
  
  emfac_region = cons_region[ghg_region, on = c('year', 'region'), nomatch = 0]
  emfac_region[, region_kgco2e_bbl := region_co2e_kg/region_barrels]
  emfac_region = emfac_region[, .(year, region, region_barrels, region_co2e_kg, region_kgco2e_bbl)]
  setorder(emfac_region, year, region)
  
# calculate proportion of each refinery's capacity to the cluster -----
  
  cap_prop = cap_dt[, .(site_id, refinery_name, region, barrels_per_day)]
  setnames(cap_prop, 'barrels_per_day', 'refinery_capacity_bpd')
  cap_prop[, region_capacity_bpd := sum(refinery_capacity_bpd), by = .(region)]
  cap_prop[, proportion := refinery_capacity_bpd/region_capacity_bpd]
  
# combine capacity proportions with consumption -----
  
  cons_ref = cap_prop[cons_region, on = 'region', allow.cartesian = T]
  setnames(cons_ref, 'thous_barrels', 'region_thous_barrels')

# use proportion to estimate refinery-level crude consumption -----
  
  cons_ref[, refinery_barrels := region_barrels*proportion]
  cons_ref = cons_ref[, .(year, site_id, refinery_name, region, region_barrels, proportion, refinery_barrels, refinery_capacity_bpd, region_capacity_bpd)]
  
# combine refinery-level crude consumption with refinery-level emissions to get refinery-level ghg factors ------
  
  emfac_ref = cons_ref[ghg_dt, on = .(site_id, year), nomatch = 0]
  emfac_ref = emfac_ref[, .(year, site_id, refinery_name, region, region_barrels, proportion, refinery_barrels, 
                            adj_total_co2e, adj_total_Mt_co2e, 
                            refinery_capacity_bpd, region_capacity_bpd)]
  setnames(emfac_ref, 'adj_total_co2e', 'refinery_co2e_tonnes')
  setnames(emfac_ref, 'adj_total_Mt_co2e', 'refinery_co2e_megatonnes')
  emfac_ref[, refinery_co2e_kg := refinery_co2e_tonnes*1e3]
  emfac_ref[, refinery_kgco2e_bbl := refinery_co2e_kg/refinery_barrels]
  emfac_ref = emfac_ref[, .(year, site_id, refinery_name, region, refinery_barrels, refinery_co2e_kg, refinery_kgco2e_bbl, proportion, 
                            refinery_capacity_bpd, region_capacity_bpd, region_barrels)]
  
# merge with region level emission factor -----
  
  emfac_ref = merge(emfac_ref,
                    emfac_region[, .(year, region, region_co2e_kg, region_kgco2e_bbl)],
                    by = c('year', 'region'))
  
  setorder(emfac_ref, year, region, -proportion)
  
# export to csv ------
  
  fwrite(emfac_region, file.path(save_path, 'refinery_ghg_factor_x_cluster.csv'), row.names = F)
  fwrite(emfac_ref, file.path(save_path, 'refinery_ghg_factor_x_indiv_refinery.csv'), row.names = F)
  
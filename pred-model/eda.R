str(well_prod_m_processed)

num_wells <- length(unique(well_prod_m_processed$api_ten_digit))
paste0("Numer of unique wells: ", num_wells)

num_counties <- length(unique(well_prod_m_processed$county))
paste0("Numer of counties: ", num_counties)

num_fieldcodes <- length(unique(well_prod_m_processed$doc_field_code))
paste0("Numer of unique field codes: ", num_fieldcodes)

well_productions <- unique(well_prod_m_processed$well_type_name)
paste0(well_productions)

producing_wells <- well_prod_m_processed %>% 
  filter(BTUofGasProduced > 0)

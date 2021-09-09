## Directory

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #In scripts folder
setwd('../../..') #Goes back to home project directory
getwd()


## Read census tract shp file

census_tract <- read_sf("./data/inmap/census-tract/tl_2019_06_tract.shp")%>%
  select(-STATEFP:-TRACTCE,-NAME:-INTPTLON)%>%
  st_transform(crs=3310)

## Load shape files names

#Select sector
sector <- "extraction/"
#sector <- "refining/"

inmap_files_raw <- list.files(paste0("./data/inmap/output/inmap_output_srm/",sector,sep = ""))

inmap_files <- ifelse(stringr::str_sub(inmap_files_raw,-3,-1)=="shp",inmap_files_raw, 0)
inmap_files <- subset(inmap_files, inmap_files != 0);inmap_fileslapply(unique(inmap_files), function(x)
  read_sf(paste0("./data/inmap/output/inmap_output_srm/",sector,x, sep=""))%>%
    st_transform(crs=3310)%>%
    select(-BasePM25:-SOx,-TotalPop, -WindSpeed)%>%
    st_intersection(census_tract)%>%
    mutate(area = as.numeric(st_area(.)))%>%
    group_by(GEOID)%>%
    mutate(weight = area/sum(area))%>%
    summarize(totalpm25 = mean(TotalPM25, na.rm = T),
              totalpm25_aw = sum(weight * TotalPM25, na.rm = T))%>%
    data.frame()%>%
    select(-geometry)%>%
    write.csv(paste0("./data/inmap/processed/",sector, substr(x,1,nchar(x)-4),".csv", sep=""), row.names = FALSE)
)


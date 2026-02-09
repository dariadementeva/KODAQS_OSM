library(sf)

base <- "C:/Users/daria/Desktop/OSM_data_2025"

files <- c(
  arnsberg_regbez_poi = "arnsberg-regbez-latest-free.shp",
  brandenburg_with_berlin_regbez_poi = "brandenburg_with_berlin-latest-free.shp",
  detmold_regbez_poi = "detmold-regbez-latest-free.shp",
  duesseldorf_regbez_poi = "duesseldorf-regbez-latest-free.shp",
  freiburg_regbez_poi = "freiburg-regbez-latest-free.shp",
  hamburg_regbez_poi = "hamburg-latest-free.shp",
  hessen_regbez_poi = "hessen-latest-free.shp",
  karlsruhe_regbez_poi = "karlsruhe-regbez-latest-free.shp",
  koeln_regbez_poi = "koeln-regbez-latest-free.shp",
  mecklenburg_vorpommern_regbez_poi = "mecklenburg-vorpommern-latest-free.shp",
  mittelfranken_regbez_poi = "mittelfranken-latest-free.shp",
  muenster_regbez_poi = "muenster-regbez-latest-free.shp",
  niederbayern_regbez_poi = "niederbayern-latest-free.shp",
  niedersachsen_with_bremen_regbez_poi = "niedersachsen_with_bremen-latest-free.shp",
  oberbayern_regbez_poi = "oberbayern-latest-free.shp",
  oberfranken_regbez_poi = "oberfranken-latest-free.shp",
  oberpfalz_regbez_poi = "oberpfalz-latest-free.shp",
  rheinland_pfalz_regbez_poi = "rheinland-pfalz-latest-free.shp",
  sachsen_anhalt_regbez_poi = "sachsen-anhalt-latest-free.shp",
  saarland_regbez_poi = "saarland-latest-free.shp",
  sachsen_regbez_poi = "sachsen-latest-free.shp",
  schleswig_holstein_regbez_poi = "schleswig-holstein-latest-free.shp",
  schwaben_regbez_poi = "schwaben-latest-free.shp",
  stuttgart_regbez_poi = "stuttgart-regbez-latest-free.shp",
  thueringen_regbez_poi = "thueringen-latest-free.shp",
  tuebingen_regbez_poi = "tuebingen-regbez-latest-free.shp",
  unterfranken_regbez_poi = "unterfranken-latest-free.shp"
)

for (nm in names(files)) {
  assign(
    nm,
    st_read(
      dsn = file.path(base, files[[nm]]),
      layer = "gis_osm_pois_free_1",
      quiet = TRUE
    )
  )
}


for (nm in names(files)) {
  assign(
    paste0(nm, "_poly"),
    st_read(
      dsn = file.path(base, files[[nm]]),
      layer = "gis_osm_pois_a_free_1",
      quiet = TRUE
    )
  )
}

save.image("OSM_raw_POI_data.RData")

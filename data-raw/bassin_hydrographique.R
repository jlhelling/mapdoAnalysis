## code to prepare `bassin_hydrographique` dataset goes here

query <- "SELECT * FROM bassin_hydrographique"
bassin_hydrographique <- sf::st_read(dsn = db_con(), query = query) %>%
  dplyr::filter(cdbh == "06")

# usethis::use_data(bassin_hydrographique, overwrite = TRUE)
# checkhelper::use_data_doc(name = "bassin_hydrographique")
# attachment::att_amend_desc()

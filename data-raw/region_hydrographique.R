## code to prepare `region_hydrographique` dataset goes here

query <- sprintf("SELECT * FROM region_hydrographique
                   WHERE gid = '11'")
region_hydrographique <- sf::st_read(dsn = db_con(), query = query)

# usethis::use_data(region_hydrographique, overwrite = TRUE)
# checkhelper::use_data_doc(name = "region_hydrographique")
# attachment::att_amend_desc()

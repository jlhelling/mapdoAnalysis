## code to prepare `network_axis` dataset goes here

query <-"SELECT
          network_axis.fid, axis, toponyme, gid_region, network_axis.geom
          FROM network_axis
          WHERE gid_region = 11"

network_axis <- sf::st_read(dsn = db_con(), query = query)

# usethis::use_data(network_axis, overwrite = TRUE)
# checkhelper::use_data_doc(name = "network_axis")
# attachment::att_amend_desc()


addReferentialNames = function(base_url, session_id, data, id_column_in_data,
                               coded_model, named_model, key_in_coded_model, key_in_named_model, name_column) {
  coded_model_df = getCloudReferentialSection(base_url, session_id, coded_model)
  named_model_df = getCloudReferentialSection(base_url, session_id, named_model)

  referential = merge(coded_model_df,
                      named_model_df,
                      by.x = key_in_coded_model,
                      by.y = key_in_named_model) %>%
    select(starts_with(key_in_named_model), matches(name_column))
  names(referential) = c("id", "name")

  data = merge(x = data,
               y = referential,
               by.x = id_column_in_data,
               by.y = "id")
  return(data)
}


addStationCoordinates = function(base_url, session_id, data, stop_id_column_in_data) {
  stop_df = getCloudReferentialSection(base_url, session_id, "stops") %>% select(id, station_id)
  station_df = getCloudReferentialSection(base_url, session_id, "stations")

  referential = merge(stop_df,
                      station_df,
                      by.x = "station_id",
                      by.y = "id") %>%
    select("id", "name", "lat", "lon")

  data = merge(x = data,
               y = referential,
               by.x = stop_id_column_in_data,
               by.y = "id")
  return(data)
}


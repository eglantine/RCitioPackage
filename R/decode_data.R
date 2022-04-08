
addReferentialNames = function(base_url, session_id, data, id_column_in_data,
                               coded_model, named_model, key_in_coded_model, key_in_named_model, name_column) {
  coded_model_df = getReferentialSection(base_url,session_id,coded_model)
  named_model_df = getReferentialSection(base_url,session_id,named_model)

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


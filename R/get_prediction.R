
getPredictedCoursesData = function(base_url, weekday = 0, session_id){
  
  predicted_courses_route = paste0(base_url,"/rest/predicted_courses?weekday=",weekday)
  
  response = getResponseFromRoute(courses_route, session_id)
  response = fromJSON(response)
  
  predicted_courses_data = do.call(rbind,response$data$stoptimes)
  
}

getPredictedOccupancyData = function(base_url,session_id, service_date, granularity = "15_minutes"){
  predicted_occupancy_route = paste0(base_url,
                                     "/rest/predicted_occupancy",
                                     "?service_date=",
                                     service_date,
                                     "&granularity=",
                                     granularity)
  
  response = getResponseFromRoute(predicted_occupancy_route, session_id)
  
  predicted_occupancy_data = data.table::rbindlist(response$data, fill = TRUE)
  
  return(predicted_occupancy_data)
}

getAllPredictedOccupanciesData = function(base_url,session_id, service_date, granularity = "15_minutes"){
  predicted_occupancy_route = paste0(base_url,
                                     "/rest/predicted_occupancy",
                                     "?service_date=",
                                     service_date,
                                     "&granularity=",
                                     granularity,
                                     "&show_all_occupancy=True"
  )
  
  response = getResponseFromRoute(predicted_occupancy_route, session_id)
  
  predicted_occupancy_data = data.table::rbindlist(response$data, fill = TRUE)
  
  return(predicted_occupancy_data)
}

getPredictedOccupancyData2 = function(base_url, session_id, service_date, granularity = "15_minutes", 
                                      show_all_occupancy = FALSE, version = "v1"){
  query_parameters = paste(paste0("service_date=",service_date),
                           paste0("granularity=", granularity),
                           sep="&")
  
  if(show_all_occupancy == TRUE){
    query_parameters = paste(query_parameters,"show_all_occupancy=True", sep = "&")
  }
  
  if(version == "v2"){
    query_parameters = paste(query_parameters,
                             paste0("version=", version),
                             sep = "&")
  }
  
  predicted_occupancy_route = paste0(base_url,
                                     paste("/rest/predicted_occupancy", query_parameters, sep = "?")
  )
  
  response = getResponseFromRoute(predicted_occupancy_route, session_id)
  
  predicted_occupancy_data = data.table::rbindlist(response$data, fill = TRUE)
  
  return(predicted_occupancy_data)
}

getPredictedStoptimesData = function(base_url,session_id, service_date){
  predicted_occupancy_route = paste0(base_url,
                                     "/rest/predicted_stoptimes",
                                     "?service_date=",
                                     service_date
                                     # ,if(station_id>0){
                                     # paste0("&station_id=", station_id)
                                     #   }
  )
  
  response = getResponseFromRoute(predicted_occupancy_route, session_id)
  
  predicted_stoptimes_data = data.table::rbindlist(response, fill = TRUE)
  
  return(predicted_stoptimes_data)
}



getServiceDates =function (base_url,session_id){
  referential_route = paste0(base_url,"/rest/service_date")
  response = getResponseFromRoute(referential_route, session_id)
  service_dates = data.table::rbindlist(response, fill = TRUE)
  
  return(service_dates)
}

getMaxServiceDate = function(base_url,session_id){
  service_dates = getServiceDates(base_url, session_id)
  max_service_date = max(service_dates[service_dates$num_courses>0,]$service_date)
  
  return (max_service_date)
}

getMaxValidationDate = function(base_url,session_id){
  service_dates = getServiceDates(base_url, session_id)
  max_validation_date = max(service_dates[service_dates$num_validations>0,]$service_date)
  
  return (max_validation_date)
}

getMaxCountingCellDate = function(base_url,session_id){
  service_dates = getServiceDates(base_url, session_id)
  max_validation_date = max(service_dates[service_dates$num_courses_with_counting_cells>0,]$service_date)
  
  return (max_validation_date)
}

getMaxControlsDate = function(base_url,session_id){
  service_dates = getServiceDates(base_url, session_id)
  max_controls_date = max(service_dates[service_dates$num_controls>0,]$service_date)
  
  return (max_controls_date)
}

getMaxScheduledCoursesDate = function(base_url,session_id){
  service_dates = getServiceDates(base_url, session_id)
  max_scheduled_courses_date = max(service_dates[service_dates$num_scheduled_courses>0,]$service_date)
  
  return (max_scheduled_courses_date)
}

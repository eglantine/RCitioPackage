#library(httr)
#library(jsonlite)

# source("R/credentials.conf")

####################################

buildBaseUrl = function(group, env) {
  base_url = paste0(
    "https://",
    group,
    ".api.",
    ifelse(env=="staging","staging.",""),
    "cit.io"
  )

  return(base_url)
}

getResponseFromRoute = function(route_url, session_id){
  response = GET(route_url,set_cookies(sessionid = session_id))
  return(
    content(response)
  )
}

getAgencyId = function(base_url, session_id){
  agency_route = paste0(base_url, "/rest/agency")
  response = getResponseFromRoute(agency_route,session_id)

  if(is.null(response$id)){
    stop("Agence non disponible")
  } else {
    return(response$id)}
}

getReferentialSection = function(base_url,session_id, referential_section){
  referential_route = paste0(base_url,"/rest/", referential_section)
  response = getResponseFromRoute(referential_route, session_id)
  #referential_table = do.call(rbind.data.frame, c(response, stringsAsFactors = F))
  referential_table = data.table::rbindlist(response, fill = TRUE)

  return(referential_table)
}

getKPIdata = function(base_url, kpi, agency_id, spatial_aggregation_level = "line", aggregated_by_time = FALSE,aggregated_by_day = FALSE, start_date = Sys.Date() - 7, end_date = Sys.Date(), days_of_the_week =  1111111, session_id){

  kpi_base_url = paste(base_url,
                       "kpis",
                       kpi,
                       "agency",
                       agency_id,
                       spatial_aggregation_level,
                       sep = "/")

  query_parameters = paste(paste0("aggregated_by_time=", tolower(aggregated_by_time)),
                           paste0("aggregated_by_day=", tolower(aggregated_by_day)),
                           paste0("included_date_perimeters=",
                                  paste(start_date,
                                        end_date,
                                        days_of_the_week,
                                        sep = "_")
                           ),
                           sep="&")

  kpi_route = paste(kpi_base_url,query_parameters, sep = "?")

  response = getResponseFromRoute(kpi_route, session_id)

  kpi_data_table = do.call(rbind.data.frame, c(response$data, stringsAsFactors = F))

  return(kpi_data_table)

}

getCoursesData = function(base_url, service_date = Sys.Date()-7, session_id){

  courses_route = paste0(base_url,"/rest/courses?service_date=",service_date)

  response = getResponseFromRoute(courses_route, session_id)

  courses_data = data.table::rbindlist(response$data, fill = TRUE)

}

getPredictedCoursesData = function(base_url, weekday = 0, session_id){

  predicted_courses_route = paste0(base_url,"/rest/predicted_courses?weekday=",weekday)

  response = getResponseFromRoute(courses_route, session_id)
  response = fromJSON(response)

  predicted_courses_data = do.call(rbind,response$data$stoptimes)

}

getPredictedOccupancyData = function(base_url,session_id, service_date){
  predicted_occupancy_route = paste0(base_url,
                                     "/rest/predicted_occupancy",
                                     "?service_date=",
                                     service_date)

  response = getResponseFromRoute(predicted_occupancy_route, session_id)

  predicted_occupancy_data = data.table::rbindlist(response$data, fill = TRUE)

  return(predicted_occupancy_data)
}

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

######################

# Samples
#
# session_id = getSessionId(login, password, group = "lorient", env = "staging")
#
# base_url = buildBaseUrl(group = "lorient", env = "staging")
#
# agency_id = getAgencyId(base_url, session_id)
#
# lines_referential = getReferentialSection(base_url,session_id,"lines")
#
# num_courses = getKPIdata(base_url,"num_courses",agency_id, session_id = session_id)

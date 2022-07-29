#library(httr)
#library(jsonlite)

# source("C:/Users/eglan/Git/RCitio/credentials.conf")

###################################

buildBaseUrl = function(agency, project, env) {
  base_url = paste0(
    "https://",
    agency,
    ".",
    project,
    ".",
    ifelse(env=="staging","staging.",""),
    "cit.io"
  )

  return(base_url)
}

getResponseFromRoute = function(route_url, session_id){

  if(is.null(session_id)){
    print("No session id")
  } else {

    response = GET(route_url,set_cookies(sessionid = session_id))

    if(response$status_code == "401"){
      print("Invalid credentials")
    }
    return(
      content(response, show_col_types = FALSE, encoding = "utf8")
    )
  }
}

getAgencyId = function(base_url, session_id){
  agency_route = paste0(base_url, "/rest/agency")
  response = getResponseFromRoute(agency_route,session_id)

  if(!is.null(response$detail)){
    stop(response$detail)
  } else {
    return(response$id)}
}

getReferentialSection = function(base_url,session_id, referential_section){
  referential_route = paste0(base_url,"/rest/", referential_section)
  response = getResponseFromRoute(referential_route, session_id)
  #referential_table = do.call(rbind.data.frame, c(response, stringsAsFactors = F))
  clean_referential = function(x) {x[c(-7,-8)]}
  referential_table = lapply(response,clean_referential)
  #  referential_table = data.table::rbindlist(response, fill = TRUE)
  referential_table = data.table::rbindlist(referential_table, fill = TRUE)

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

#  kpi_data_table = do.call(rbind.data.frame, c(response$data, stringsAsFactors = F, fill = NA))
  kpi_data_table = data.table::rbindlist(response$data, fill = TRUE)

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


getAgencyConfiguration =function (gateway_base_url,session_id){
  agency_configuration_route = paste0(gateway_base_url,"/agency.json")
  response = getResponseFromRoute(agency_configuration_route, session_id)
}

getActiveAgencies = function(){
  response = content(GET("http://django.gateway.cit.io/agencies/"))
  agency_list =  sapply(response, function(x) x$name)
  state =  sapply(response, function(x) x$state)
  active_agency_list = agency_list[state == "live"]
}

######################

# Samples
#
# session_id = getSessionId(login, password, agency = "lorient", env = "staging")
#
# base_url = buildBaseUrl(agency = "lorient","api", env = "staging")
#
# agency_id = getAgencyId(base_url, session_id)
#
# lines_referential = getReferentialSection(base_url,session_id,"lines")
#
# num_courses = getKPIdata(base_url,"num_courses",agency_id, session_id = session_id)

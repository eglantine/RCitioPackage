#library(httr)
#library(jsonlite)

# source("C:/Users/eglan/Git/RCitio/credentials.conf")

###################################

buildBaseUrl = function(agency, project, env) {
  
  if(env == "localhost"){
    base_url = paste0("http://localhost:8000","/",agency)
  }
  else {
    
  base_url = paste0(
    "https://",
    agency,
    ".",
    project,
    ".",
    ifelse(env=="staging","staging.",""),
    "cit.io"
  )
  }
  return(base_url)
}

getResponseFromRoute = function(route_url, session_id){
  
  if(is.null(session_id)){
    print("No session id")
  } else {
    
    print(paste("Querying URL:", route_url))
    
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
  
  kpi_data_table = suppressWarnings(data.table::rbindlist(response$data, fill = TRUE))
  
  if(kpi == "course_trips"){
    kpi_data_table = kpi_data_table %>%
      mutate(ticket_types = NULL) %>%
      unique()
  }
  
  return(kpi_data_table)
  
}

getKPIdataForLineByDirection = function(base_url, kpi, line_id, direction = "1", spatial_aggregation_level = "line", 
                                        aggregated_by_time = FALSE,aggregated_by_day = FALSE, start_date = Sys.Date() - 7, 
                                        end_date = Sys.Date(), days_of_the_week =  1111111, session_id){
  kpi_base_url = paste(base_url,
                       "kpis",
                       kpi,
                       "line",
                       line_id,
                       "station",
                       sep = "/")
  query_parameters = paste(paste0("aggregated_by_time=", tolower(aggregated_by_time)),
                           paste0("aggregated_by_day=", tolower(aggregated_by_day)),
                           paste0("included_date_perimeters=",
                                  paste(start_date, end_date, days_of_the_week, sep = "_")),
                           "direction=", direction,
                           sep="&")
  
  kpi_route = paste(kpi_base_url,query_parameters, sep = "?")
  
  response = getResponseFromRoute(kpi_route, session_id)
  
  kpi_data_table = suppressWarnings(data.table::rbindlist(response$data, fill = TRUE))
  
  return(kpi_data_table)
}


getCoursesData = function(base_url, service_date = Sys.Date()-7, session_id){
  
  courses_route = paste0(base_url,"/rest/courses?service_date=",service_date)
  
  response = getResponseFromRoute(courses_route, session_id)
  
  courses_data = data.table::rbindlist(response$data, fill = TRUE)
  
}

getCourses2 = function(base_url, product, service_date = Sys.Date()-7, session_id){
  course_endpoint = paste("/courses", product, sep = "/")
  
  courses_route = paste0(base_url,course_endpoint,"?service_date=",service_date)
  
  response = getResponseFromRoute(courses_route, session_id)
  
  courses_data = data.table::rbindlist(response$data, fill = TRUE)
  
}

# DraftRegularCourses = function(base_url, product, kpi, occupancy_type = NULL, 
#                                service_date = Sys.Date()-7, session_id){
#   
#   if(product == "transport" | kpi != "occupancy"){
#     course_endpoint = paste("/courses", product, kpi, sep = "/")
#   } else if(product == "rail" & kpi == "occupancy"){
#     course_endpoint = switch(occupancy_type,
#                              "denoised" = paste0("/courses/rail/", kpi),
#                              "extrapolated" = paste0("/courses/rail_scheduled/", kpi),
#                              "manual" = paste0("/courses/rail_manual/", kpi))
#   } 
#   
#   courses_route = paste0(base_url,course_endpoint,"?service_date=",service_date)
#   
#   response = getResponseFromRoute(courses_route, session_id)
#   
#   courses_data = data.table::rbindlist(response$data, fill = TRUE)
#   
# }

getManualCoursesData = function(base_url, service_date = Sys.Date()-7, session_id){
  
  courses_route = paste0(base_url,"/rest/manual_courses?service_date=",service_date)
  
  response = getResponseFromRoute(courses_route, session_id)
  
  courses_data = data.table::rbindlist(response$data, fill = TRUE)
  
}

getAgencyConfiguration =function (gateway_base_url,session_id){
  agency_configuration_route = paste0(gateway_base_url,"/agency.json")
  response = getResponseFromRoute(agency_configuration_route, session_id)
}

getActiveAgencies = function(){
  response = content(GET("http://gateway.cit.io/agencies/"))
  agency_list =  sapply(response, function(x) x$name)
  state =  sapply(response, function(x) x$state)
  active_agency_list = agency_list[state == "live"]
  
}

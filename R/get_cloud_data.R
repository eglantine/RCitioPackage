getFolderList = function(base_url, session_id, path, depth){
  gcloud_base_url =  paste0(base_url, "/rest/gcloud/folders")
  
  query_parameters = paste(paste0("path=",path),
                           paste0("depth=",depth),
                           sep="&")
  full_query = paste(gcloud_base_url,query_parameters, sep = "?")
  response = getResponseFromRoute(full_query, session_id)
  
  folder_list = unlist(response$data)
}

getFileList = function(base_url, session_id, path){
  gcloud_base_url =  paste0(base_url, "/rest/gcloud/files?path=")
  
  full_query = paste0(gcloud_base_url,path)
  response = getResponseFromRoute(full_query, session_id)
  
  file_list = unlist(response$data)
}

getFileData = function(base_url, session_id, path){
  gcloud_base_url =  paste0(base_url, "/rest/gcloud/read_blob?path=")
  
  full_query = paste0(gcloud_base_url,path)
  df = getResponseFromRoute(full_query, session_id)
}

getFileListData = function(base_url, session_id, path){
  file_list = getFileList(base_url, session_id, path)
  
  i = 1
  data = data.frame()
  
  for(i in 1:length(file_list)){
    data_item = getFileData(base_url, session_id, file_list[i])
    data = rbind(data, data_item)
    i = i+1
  }
  return(data)
  
}

getCloudReferentialSection = function(base_url,session_id, model){
  model_path = paste0("clean-source/referential/", model, ".csv")
  
  referential_table = getFileData(base_url, session_id, model_path)
  return(referential_table)
}

getCleanData = function(agency, env, session_id, year_month, model){
  
  api_base_url = buildBaseUrl(agency, "api", env)
  
  path =  paste0("clean-source/", model, "/", year_month)
  
  if(!is.null(getFileList(api_base_url,session_id, path))){
    
    data = getFileListData(api_base_url,session_id,path)
    
    return(data)
  }
}
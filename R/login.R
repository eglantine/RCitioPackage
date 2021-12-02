setCredentials = function(login, password){
  credentials = paste("Basic", jsonlite::base64_enc(paste0(login,":",password)))
}

getSessionId = function(login = "eglantine@cit.io", password, agency = "casablanca", env = "staging"){

    credentials = setCredentials(login, password)

    login_route = paste0("https://",
                         agency,
                         ".gateway.",
                         ifelse(env=="staging","staging.",""),
                         "cit.io/api/login")

    response = GET(login_route, add_headers(Authorization = credentials))

    if(response$status_code == "401"){
      print("Invalid login or password")
    }

    if(response$status_code == "200"){
    return(
      response$cookies$value[response$cookies$name=="sessionid"])
      }
}

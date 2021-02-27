setCredentials = function(login, password){
  credentials = paste("Basic", jsonlite::base64_enc(paste0(login,":",password)))
}

getSessionId = function(login = "eglantine@cit.io", password, group = "casablanca", env = "staging"){
  if(is.null(login)||is.null(password)){
    stop("Please provide login and password")
  } else {

    credentials = setCredentials(login, password)

    login_route = paste0(
      "https://",
      group,
      ".gateway.",
      ifelse(env=="staging","staging.",""),
      "cit.io/api/login"
    )

    response = GET(login_route, add_headers(Authorization = credentials))

    return(
      response$cookies$value[response$cookies$name=="sessionid"]
    )
  }
}

################ Null to NA ###########

listNulltoNA <- function(x){
  if(length(x) == 0){
    return(NA)
  } else {
    x[sapply(x, is.null)] <- NA
    return(x)
  }
}


############# create url ############

create_url <- function(text = text, mindate = mindate, maxdate = maxdate, bbox = bbox, has_geo = has_geo, i = i){
  
  paste("https://api.flickr.com/services/rest/?method=flickr.photos.search&api_key=",api_key,
        "&text=", text,
        "&min_taken_date=", as.character(mindate),
        "&max_taken_date=", as.character(maxdate),
        ifelse(!(is.null(bbox)), paste("&bbox=", bbox), ''),
        ifelse(has_geo, paste("&has_geo=", has_geo), ''),
        "&extras=", "date_taken,geo,tags,license,url_sq,url_t,url_s,url_q,url_m,url_n,url_z,url_c,url_l,url_o",
        "&page=", i,
        "&format=", "rest",
        sep = "")
}

###

################## get data ########
search_url <- function(base_url = base_url){
  
  # get total number of results
  r <- httr::GET(paste(base_url))
  
  #put first error catch here
  count_stat <- 0
  
  while(r$status_code != 200 & count_stat < 3){
    Sys.sleep(0.5)
    r <- httr::GET(paste(base_url))
    count_stat <-  count_stat + 1
  }
  
  if(r$status_code != 200){
    warning('Status code:', r$status, ' for ', mindate, ' to ', maxdate, ' for area ', bbox, ' - message: ', content(r, 'text'))
  }
  
  error <- tryCatch({
    getPhotos_data <- XML::xmlRoot(XML::xmlTreeParse(content(r, 'text'), useInternalNodes = TRUE))
    error <- 'success'
  }, error = function(err){
    warning('Dates between ', mindate, ' and ', maxdate, ' for area ', bbox, ' skipped beacuse: ', err)
    error <- 'error'
    getPhotos_data = NULL
  })    
  
  return(getPhotos_data)
  
}





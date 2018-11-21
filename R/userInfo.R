user_info <- function(api_key, user_id){
  
  id_tmp <- NULL
  id_info <-NULL
 
  if(length(user_id) > 0){
  
    for(i in user_id){
      
      get_info <- paste("https://api.flickr.com/services/rest/?method=flickr.profile.getProfile&api_key=",api_key,"&user_id=",i,sep="")
      
      r <- GET(get_info)
      
      count_stat <- 0
      
      while(r$status_code != 200 & count_stat < 3){
        Sys.sleep(0.5)
        r <- GET(get_info)
        count_stat <-  count_stat + 1
      }
      
      if(r$status_code != 200){
        warning('Status code:', r$status, ' for id ', i, ' - message: ', content(r, 'text'))
      }
      
      error <- tryCatch({
        get_id_data <- xmlRoot(xmlTreeParse(content(r, 'text'), useInternalNodes = TRUE))
        error <- 'sucess'
      }, error = function(err){
        warning('ID ', i, ' skipped beacuse: ', err)
        error <- 'error'
      })
      
      if(error != 'error'){
        get_id_data <<- get_id_data
        user_id <- listNulltoNA(xpathSApply(get_id_data, "//profile", xmlGetAttr, "id"))
        country <- listNulltoNA(xpathSApply(get_id_data, "//profile", xmlGetAttr, "country"))
        first_name <- listNulltoNA(xpathSApply(get_id_data, "//profile", xmlGetAttr, "first_name"))
        
        if(!all(is.na(c(user_id, country, first_name)))){
          
          tmp_df <- data.frame(user_id, country, first_name,
                               stringsAsFactors = FALSE)
          
          id_tmp <- rbind(id_tmp, tmp_df)
          rm(list = 'tmp_df')
          
        }
      }
    }
    
    id_info <- rbind(id_info, id_tmp)
    
  }
}
  

  
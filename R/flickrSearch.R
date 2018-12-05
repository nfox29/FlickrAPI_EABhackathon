flickr_search <-
  function(min_taken = "2018-09-10",
           max_taken = "2018-10-10",
           text = "tree",          
           bbox = NULL,
           woe_id = 23424975,        
           has_geo = TRUE){
    

    if( !(is.null(bbox) | is.null(woe_id))==TRUE) {
      stop('can not provide bbox and woe_id')
    }
    
    text <- gsub(' ', '+', trimws(text))  
    api_key <- auth$key
    perpage <- "250"
    format <- "rest"
    extras <- "date_taken,geo,tags,license,url_sq,url_t,url_s,url_q,url_m,url_n,url_z,url_c,url_l,url_o"
    baseURL <- paste("https://api.flickr.com/services/rest/?method=flickr.photos.search&api_key=",api_key,sep="")   #set base URL
    pics<-NULL
    mindate <- min_taken
    maxdate <- max_taken
    
    year_dates <- NULL
    month_dates <- NULL
    week_dates <- NULL
    day_dates <- NULL
    skipped_dates <- NULL
    
    ########### test individual days#############
    
    test_date <- as.Date(mindate) + 1
    
    if(!is.null(bbox)){
      getPhotos <- paste(baseURL,
                         "&text=", text,
                         "&min_taken_date=", as.character(mindate),
                         "&max_taken_date=", as.character(test_date),
                         "&bbox=", paste0(bbox[1],",",bbox[2],",",bbox[3],",",bbox[4]),
                         "&extras=", extras,
                         "&per_page=", perpage,
                         "&format=", format,
                         sep = "")
    } else if(!is.null(woe_id)){
      getPhotos <- paste(baseURL,
                         "&text=", text,
                         "&min_taken_date=", as.character(mindate),
                         "&max_taken_date=", as.character(test_date),
                         "&woe_id=", woe_id,
                         "&extras=", extras,
                         "&per_page=", perpage,
                         "&format=", format,
                         sep = "")     
    } else {
      getPhotos <- paste(baseURL,
                         "&text=", text,
                         "&min_taken_date=", as.character(mindate),
                         "&max_taken_date=", as.character(test_date),
                         ifelse(has_geo, paste0("&has_geo=", has_geo), ''),
                         "&extras=", extras,
                         "&per_page=", perpage,
                         "&format=", format,
                         sep = "")
    }
    
    r <- GET(getPhotos)
    
    count_stat <- 0
    
    while(r$status_code != 200 & count_stat < 3){
      Sys.sleep(0.5)
      r <- GET(getPhotos)
      count_stat <-  count_stat + 1
    }
    
    if(r$status_code != 200){
      warning('Status code:', r$status, ' for between ', mindate, ' and ', maxdate, ' - message: ', content(r, 'text'))
    }
    
    error <- tryCatch({
      getPhotos_data <- xmlRoot(xmlTreeParse(content(r, 'text')))
      error <- 'success'
    }, error = function(err){
      warning('Between ', mindate, ' and ', maxdate, ' skipped beacuse: ', err)
      error <- 'error'
    })    
    
    if(error != 'error'){
      
      #results are returned in different pages so it is necessary to loop through pages to collect all the data
      #parse the total number of pages
      pages_data <- data.frame(xmlAttrs(getPhotos_data[["photos"]]))
      pages_data[] <- lapply(pages_data, FUN = function(x) as.integer(as.character(x)))
      total_pages <- pages_data["pages",]
      total <- pages_data["pages",]
    }
    
    if (total > 15){
      
      stop("API calls for individual days in your search paramaters will return more than the API allows: refine your query")
      
    }
    
    ########### end days test ##############

    if(!is.null(bbox)){
      getPhotos <- paste(baseURL,
                         "&text=", text,
                         "&min_taken_date=", as.character(mindate),
                         "&max_taken_date=", as.character(maxdate),
                         "&bbox=", paste0(bbox[1],",",bbox[2],",",bbox[3],",",bbox[4]),
                         "&extras=", extras,
                         "&per_page=", perpage,
                         "&format=", format,
                         sep = "")
    } else if(!is.null(woe_id)){
      getPhotos <- paste(baseURL,
                         "&text=", text,
                         "&min_taken_date=", as.character(mindate),
                         "&max_taken_date=", as.character(maxdate),
                         "&woe_id=", woe_id,
                         "&extras=", extras,
                         "&per_page=", perpage,
                         "&format=", format,
                         sep = "")     
    } else {
      getPhotos <- paste(baseURL,
                         "&text=", text,
                         "&min_taken_date=", as.character(mindate),
                         "&max_taken_date=", as.character(maxdate),
                         ifelse(has_geo, paste0("&has_geo=", has_geo), ''),
                         "&extras=", extras,
                         "&per_page=", perpage,
                         "&format=", format,
                         sep = "")
    }
    
    r <- GET(getPhotos)
    
    count_stat <- 0
    
    while(r$status_code != 200 & count_stat < 3){
      Sys.sleep(0.5)
      r <- GET(getPhotos)
      count_stat <-  count_stat + 1
    }
    
    if(r$status_code != 200){
      warning('Status code:', r$status, ' for between ', mindate, ' and ', maxdate, ' - message: ', content(r, 'text'))
    }
    
    error <- tryCatch({
      getPhotos_data <- xmlRoot(xmlTreeParse(content(r, 'text')))
      error <- 'success'
    }, error = function(err){
      warning('Between ', mindate, ' and ', maxdate, ' skipped beacuse: ', err)
      error <- 'error'
    })    
    
    if(error != 'error'){
      
      #get the number of pages >15 are duplicates
      pages_data <- data.frame(xmlAttrs(getPhotos_data[["photos"]]))
      pages_data[] <- lapply(pages_data, FUN = function(x) as.integer(as.character(x)))
      total_pages <- pages_data["pages",]
      total <- pages_data["pages",]
      
      #if all results are unique get results
      if (total <= 15) {
        
        pics_tmp <- NULL
        
        # loop thru pages of photos and save the list in a DF
        for(i in c(1:total_pages)){
          
          if(!is.null(bbox)){ 
            
            getPhotos <- paste(baseURL
                               ,"&text=",text,"&min_taken_date=",mindate,
                               "&max_taken_date=",maxdate,
                               "&bbox=", paste0(bbox[1],",",bbox[2],",",bbox[3],",",bbox[4]),
                               "&extras=",extras,
                               "&per_page=",perpage,"&format=",format,"&page="
                               ,i,sep="")
            
          } else if(!is.null(woe_id)){
            
            getPhotos <- paste(baseURL
                               ,"&text=",text,"&min_taken_date=",mindate,
                               "&max_taken_date=",maxdate,
                               "&woe_id=",woe_id,
                               "&extras=",extras,
                               "&per_page=",perpage,"&format=",format,"&page="
                               ,i,sep="")
            
          } else {
            
            getPhotos <- paste(baseURL
                               ,"&text=",text,"&min_taken_date=",mindate,
                               "&max_taken_date=",maxdate,
                               ifelse(has_geo, paste0("&has_geo=", has_geo), ''),
                               "&extras=",extras,
                               "&per_page=",perpage,"&format=",format,"&page="
                               ,i,sep="")        
            
          }
          
          r <- GET(getPhotos)
          
          count_stat <- 0
          
          while(r$status_code != 200 & count_stat < 3){
            Sys.sleep(0.5)
            r <- GET(getPhotos)
            count_stat <-  count_stat + 1
          }
          
          if(r$status_code != 200){
            warning('Status code:', r$status, ' for between ', mindate, ' and ', maxdate, ' page ', i, ' - message: ', content(r, 'text'))
          }
          
          error <- tryCatch({
            getPhotos_data <- xmlRoot(xmlTreeParse(content(r, 'text'), useInternalNodes = TRUE))
            error <- 'success'
          }, error = function(err){
            warning('Between ', mindate, ' and ', maxdate, ' page ', i,' skipped beacuse: ', err)
            error <- 'error'
          })
          
          if(error != 'error'){
            getPhotos_data <<- getPhotos_data
            id <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "id"))                 #extract photo id
            owner <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "owner"))           #extract user id
            datetaken <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "datetaken"))   #extract date picture was taken
            tags <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "tags"))             #extract tags
            latitude <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "latitude"))     #extract latitude
            longitude <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "longitude"))   #extract longitude
            license <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "license"))       #extract license
            url_s <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "url_s"))           #extract url_s
            url_m <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "url_m"))           #extract url_m
            url_l <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "url_l"))           #extract url_l
            url_o <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "url_o"))           #extract url_o
            
            if(!all(is.na(c(id,owner,datetaken,tags,latitude,longitude,license,url_s,url_m,url_l,url_o)))){
              
              tmp_df <- data.frame(id, owner, datetaken, tags,
                                   as.numeric(latitude),
                                   as.numeric(longitude), license,
                                   url_s = unlist(url_s), url_m = unlist(url_m),
                                   url_l = unlist(url_l), url_o = unlist(url_o),
                                   stringsAsFactors = FALSE)
              
              tmp_df$page <- i
              pics_tmp <- rbind(pics_tmp, tmp_df)
              rm(list = 'tmp_df')
              
            }
          }
        }
        
        pics <- rbind(pics, pics_tmp)
        
      } 
      
      #if results will contain duplicates, split dates in to years
      else if (total > 15){
       
        year_dates <- data.frame(min_taken, max_taken, stringsAsFactors = FALSE)
        
      }
      
    }
    
    #run searches on yearly intervals
    if (!is.null(year_dates)){
     
      month_tmp <- NULL
      
      for (x in 1:(nrow(year_dates))){
        
        mindate <- year_dates[x, 1]
        maxdate <- year_dates[x, 2]
        
        year_range <- seq(as.Date(mindate), as.Date(maxdate), by="+1 year")
        
        if (year_range[length(year_range)] != maxdate){
          year_range <- append(year_range, as.Date(maxdate))
        } 
        
        for(y in 1:(length(year_range) - 1)){ 
          
          mindate <- year_range[y]
          maxdate <- year_range[y + 1]
          
          if(!is.null(bbox)){
            getPhotos <- paste(baseURL,
                               "&text=", text,
                               "&min_taken_date=", as.character(mindate),
                               "&max_taken_date=", as.character(maxdate),
                               "&bbox=", paste0(bbox[1],",",bbox[2],",",bbox[3],",",bbox[4]),
                               "&extras=", extras,
                               "&per_page=", perpage,
                               "&format=", format,
                               sep = "")
          } else if(!is.null(woe_id)){
            getPhotos <- paste(baseURL,
                               "&text=", text,
                               "&min_taken_date=", as.character(mindate),
                               "&max_taken_date=", as.character(maxdate),
                               "&woe_id=", woe_id,
                               "&extras=", extras,
                               "&per_page=", perpage,
                               "&format=", format,
                               sep = "")     
          } else {
            getPhotos <- paste(baseURL,
                               "&text=", text,
                               "&min_taken_date=", as.character(mindate),
                               "&max_taken_date=", as.character(maxdate),
                               ifelse(has_geo, paste0("&has_geo=", has_geo), ''),
                               "&extras=", extras,
                               "&per_page=", perpage,
                               "&format=", format,
                               sep = "")
          }
          
          r <- GET(getPhotos)
          
          count_stat <- 0
          
          while(r$status_code != 200 & count_stat < 3){
            Sys.sleep(0.5)
            r <- GET(getPhotos)
            count_stat <-  count_stat + 1
          }
          
          if(r$status_code != 200){
            warning('Status code:', r$status, ' for between ', mindate, ' and ', maxdate, ' - message: ', content(r, 'text'))
          }
          
          error <- tryCatch({
            getPhotos_data <- xmlRoot(xmlTreeParse(content(r, 'text')))
            error <- 'success'
          }, error = function(err){
            warning('Between ', mindate, ' and ', maxdate, ' skipped beacuse: ', err)
            error <- 'error'
          })    
          
          if(error != 'error'){
            
            #results are returned in different pages so it is necessary to loop through pages to collect all the data
            #parse the total number of pages
            pages_data <- data.frame(xmlAttrs(getPhotos_data[["photos"]]))
            pages_data[] <- lapply(pages_data, FUN = function(x) as.integer(as.character(x)))
            total_pages <- pages_data["pages",]
            total <- pages_data["pages",]
            
            #if all results are unique get results
            if(total <= 15){
              
              pics_tmp <- NULL
              
              # loop thru pages of photos and save the list in a DF
              for(i in c(1:total_pages)){
                
                if(!is.null(bbox)){ 
                  
                  getPhotos <- paste(baseURL
                                     ,"&text=",text,"&min_taken_date=",mindate,
                                     "&max_taken_date=",maxdate,
                                     "&bbox=", paste0(bbox[1],",",bbox[2],",",bbox[3],",",bbox[4]),
                                     "&extras=",extras,
                                     "&per_page=",perpage,"&format=",format,"&page="
                                     ,i,sep="")
                  
                } else if(!is.null(woe_id)){
                  
                  getPhotos <- paste(baseURL
                                     ,"&text=",text,"&min_taken_date=",mindate,
                                     "&max_taken_date=",maxdate,
                                     "&woe_id=",woe_id,
                                     "&extras=",extras,
                                     "&per_page=",perpage,"&format=",format,"&page="
                                     ,i,sep="")
                  
                } else {
                  
                  getPhotos <- paste(baseURL
                                     ,"&text=",text,"&min_taken_date=",mindate,
                                     "&max_taken_date=",maxdate,
                                     ifelse(has_geo, paste0("&has_geo=", has_geo), ''),
                                     "&extras=",extras,
                                     "&per_page=",perpage,"&format=",format,"&page="
                                     ,i,sep="")        
                  
                }
                
                r <- GET(getPhotos)
                
                count_stat <- 0
                
                while(r$status_code != 200 & count_stat < 3){
                  Sys.sleep(0.5)
                  r <- GET(getPhotos)
                  count_stat <-  count_stat + 1
                }
                
                if(r$status_code != 200){
                  warning('Status code:', r$status, ' for between ', mindate, ' and ', maxdate, ' page ', i, ' - message: ', content(r, 'text'))
                }
                
                error <- tryCatch({
                  getPhotos_data <- xmlRoot(xmlTreeParse(content(r, 'text'), useInternalNodes = TRUE))
                  error <- 'success'
                }, error = function(err){
                  warning('Between ', mindate, ' and ', maxdate, ' page ', i,' skipped beacuse: ', err)
                  error <- 'error'
                })
                
                if(error != 'error'){
                  getPhotos_data <<- getPhotos_data
                  id <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "id"))                 #extract photo id
                  owner <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "owner"))           #extract user id
                  datetaken <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "datetaken"))   #extract date picture was taken
                  tags <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "tags"))             #extract tags
                  latitude <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "latitude"))     #extract latitude
                  longitude <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "longitude"))   #extract longitude
                  license <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "license"))       #extract license
                  url_s <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "url_s"))           #extract url_s
                  url_m <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "url_m"))           #extract url_m
                  url_l <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "url_l"))           #extract url_l
                  url_o <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "url_o"))           #extract url_o
                  
                  if(!all(is.na(c(id,owner,datetaken,tags,latitude,longitude,license,url_s,url_m,url_l,url_o)))){
                    
                    tmp_df <- data.frame(id, owner, datetaken, tags,
                                         as.numeric(latitude),
                                         as.numeric(longitude), license,
                                         url_s = unlist(url_s), url_m = unlist(url_m),
                                         url_l = unlist(url_l), url_o = unlist(url_o),
                                         stringsAsFactors = FALSE)
                    
                    tmp_df$page <- i
                    pics_tmp <- rbind(pics_tmp, tmp_df)
                    rm(list = 'tmp_df')
                    
                  }
                }
              }
              
              pics <- rbind(pics, pics_tmp)
              
            } 
            
            
            else if (total > 15){
              
              #else pass date to new dataframe
              tmp_df <- data.frame(mindate, maxdate, stringsAsFactors = FALSE)
              month_tmp <- rbind(month_tmp, tmp_df)
              rm(list = "tmp_df")
              
            }
            
            month_dates <- rbind(month_dates, month_tmp)
            month_tmp <- NULL
            
          }
          
            }
      
      }
      
    }
    
    #run searchs on monthly intervals
    if (!is.null(month_dates)){
      
      week_tmp <- NULL
      
      for (x in 1:(nrow(month_dates))){
        
        mindate <- month_dates[x, 1]
        maxdate <- month_dates[x, 2]
        
        month_range <- seq(as.Date(mindate), as.Date(maxdate), by="+1 month")
        
        if (month_range[length(month_range)] != maxdate){
          month_range <- append(month_range, as.Date(maxdate))
        } 
        
        for(m in 1:(length(month_range) - 1)){ 
          
          mindate <- month_range[m]
          maxdate <- month_range[m + 1]
          
          #here if < 4,000 run code
          if(!is.null(bbox)){
            getPhotos <- paste(baseURL,
                               "&text=", text,
                               "&min_taken_date=", as.character(mindate),
                               "&max_taken_date=", as.character(maxdate),
                               "&bbox=", paste0(bbox[1],",",bbox[2],",",bbox[3],",",bbox[4]),
                               "&extras=", extras,
                               "&per_page=", perpage,
                               "&format=", format,
                               sep = "")
          } else if(!is.null(woe_id)){
            getPhotos <- paste(baseURL,
                               "&text=", text,
                               "&min_taken_date=", as.character(mindate),
                               "&max_taken_date=", as.character(maxdate),
                               "&woe_id=", woe_id,
                               "&extras=", extras,
                               "&per_page=", perpage,
                               "&format=", format,
                               sep = "")     
          } else {
            getPhotos <- paste(baseURL,
                               "&text=", text,
                               "&min_taken_date=", as.character(mindate),
                               "&max_taken_date=", as.character(maxdate),
                               ifelse(has_geo, paste0("&has_geo=", has_geo), ''),
                               "&extras=", extras,
                               "&per_page=", perpage,
                               "&format=", format,
                               sep = "")
          }
          
          r <- GET(getPhotos)
          
          count_stat <- 0
          
          while(r$status_code != 200 & count_stat < 3){
            Sys.sleep(0.5)
            r <- GET(getPhotos)
            count_stat <-  count_stat + 1
          }
          
          if(r$status_code != 200){
            warning('Status code:', r$status, ' for between ', mindate, ' and ', maxdate, ' - message: ', content(r, 'text'))
          }
          
          error <- tryCatch({
            getPhotos_data <- xmlRoot(xmlTreeParse(content(r, 'text')))
            error <- 'success'
          }, error = function(err){
            warning('Between ', mindate, ' and ', maxdate, ' skipped beacuse: ', err)
            error <- 'error'
          })    
          
          if(error != 'error'){
            
            #results are returned in different pages so it is necessary to loop through pages to collect all the data
            #parse the total number of pages
            pages_data <- data.frame(xmlAttrs(getPhotos_data[["photos"]]))
            pages_data[] <- lapply(pages_data, FUN = function(x) as.integer(as.character(x)))
            total_pages <- pages_data["pages",]
            total <- pages_data["pages",]
            
            #if all results are unique get results
            if(total <= 15){
              
              pics_tmp <- NULL
              
              # loop thru pages of photos and save the list in a DF
              for(i in c(1:total_pages)){
                
                if(!is.null(bbox)){ 
                  
                  getPhotos <- paste(baseURL
                                     ,"&text=",text,"&min_taken_date=",mindate,
                                     "&max_taken_date=",maxdate,
                                     "&bbox=", paste0(bbox[1],",",bbox[2],",",bbox[3],",",bbox[4]),
                                     "&extras=",extras,
                                     "&per_page=",perpage,"&format=",format,"&page="
                                     ,i,sep="")
                  
                } else if(!is.null(woe_id)){
                  
                  getPhotos <- paste(baseURL
                                     ,"&text=",text,"&min_taken_date=",mindate,
                                     "&max_taken_date=",maxdate,
                                     "&woe_id=",woe_id,
                                     "&extras=",extras,
                                     "&per_page=",perpage,"&format=",format,"&page="
                                     ,i,sep="")
                  
                } else {
                  
                  getPhotos <- paste(baseURL
                                     ,"&text=",text,"&min_taken_date=",mindate,
                                     "&max_taken_date=",maxdate,
                                     ifelse(has_geo, paste0("&has_geo=", has_geo), ''),
                                     "&extras=",extras,
                                     "&per_page=",perpage,"&format=",format,"&page="
                                     ,i,sep="")        
                  
                }
                
                r <- GET(getPhotos)
                
                count_stat <- 0
                
                while(r$status_code != 200 & count_stat < 3){
                  Sys.sleep(0.5)
                  r <- GET(getPhotos)
                  count_stat <-  count_stat + 1
                }
                
                if(r$status_code != 200){
                  warning('Status code:', r$status, ' for between ', mindate, ' and ', maxdate, ' page ', i, ' - message: ', content(r, 'text'))
                }
                
                error <- tryCatch({
                  getPhotos_data <- xmlRoot(xmlTreeParse(content(r, 'text'), useInternalNodes = TRUE))
                  error <- 'success'
                }, error = function(err){
                  warning('Between ', mindate, ' and ', maxdate, ' page ', i,' skipped beacuse: ', err)
                  error <- 'error'
                })
                
                if(error != 'error'){
                  getPhotos_data <<- getPhotos_data
                  id <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "id"))                 #extract photo id
                  owner <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "owner"))           #extract user id
                  datetaken <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "datetaken"))   #extract date picture was taken
                  tags <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "tags"))             #extract tags
                  latitude <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "latitude"))     #extract latitude
                  longitude <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "longitude"))   #extract longitude
                  license <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "license"))       #extract license
                  url_s <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "url_s"))           #extract url_s
                  url_m <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "url_m"))           #extract url_m
                  url_l <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "url_l"))           #extract url_l
                  url_o <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "url_o"))           #extract url_o
                  
                  if(!all(is.na(c(id,owner,datetaken,tags,latitude,longitude,license,url_s,url_m,url_l,url_o)))){
                    
                    tmp_df <- data.frame(id, owner, datetaken, tags,
                                         as.numeric(latitude),
                                         as.numeric(longitude), license,
                                         url_s = unlist(url_s), url_m = unlist(url_m),
                                         url_l = unlist(url_l), url_o = unlist(url_o),
                                         stringsAsFactors = FALSE)
                    
                    tmp_df$page <- i
                    pics_tmp <- rbind(pics_tmp, tmp_df)
                    rm(list = 'tmp_df')
                    
                  }
                }
              }
              
              pics <- rbind(pics, pics_tmp)
              
            } 
            
            else if (total > 15){
              
              #else pass date to new dataframe
              tmp_df <- data.frame(mindate, maxdate, stringsAsFactors = FALSE)
              week_tmp <- rbind(week_tmp, tmp_df)
              rm(list = "tmp_df")
            }
            
            week_dates <- rbind(week_dates, week_tmp)
            week_tmp <- NULL
            
          }
          
        }
        
      }
      
    }
    
    #run searches on weekly intervals
    if (!is.null(week_dates)){
      
      day_tmp <- NULL
      
      for (x in 1:(nrow(week_dates))){
        
        mindate <- week_dates[x, 1]
        maxdate <- week_dates[x, 2]
        
        week_range <- seq(as.Date(mindate), as.Date(maxdate), by="+1 week")
        
        if (week_range[length(week_range)] != maxdate){
          week_range <- append(week_range, as.Date(maxdate))
        } 
        
        for(w in 1:(length(week_range) - 1)){ 
          
          mindate <- week_range[w]
          maxdate <- week_range[w + 1]
          
          #here if < 4,000 run code
          if(!is.null(bbox)){
            getPhotos <- paste(baseURL,
                               "&text=", text,
                               "&min_taken_date=", as.character(mindate),
                               "&max_taken_date=", as.character(maxdate),
                               "&bbox=", paste0(bbox[1],",",bbox[2],",",bbox[3],",",bbox[4]),
                               "&extras=", extras,
                               "&per_page=", perpage,
                               "&format=", format,
                               sep = "")
          } else if(!is.null(woe_id)){
            getPhotos <- paste(baseURL,
                               "&text=", text,
                               "&min_taken_date=", as.character(mindate),
                               "&max_taken_date=", as.character(maxdate),
                               "&woe_id=", woe_id,
                               "&extras=", extras,
                               "&per_page=", perpage,
                               "&format=", format,
                               sep = "")     
          } else {
            getPhotos <- paste(baseURL,
                               "&text=", text,
                               "&min_taken_date=", as.character(mindate),
                               "&max_taken_date=", as.character(maxdate),
                               ifelse(has_geo, paste0("&has_geo=", has_geo), ''),
                               "&extras=", extras,
                               "&per_page=", perpage,
                               "&format=", format,
                               sep = "")
          }
          
          r <- GET(getPhotos)
          
          count_stat <- 0
          
          while(r$status_code != 200 & count_stat < 3){
            Sys.sleep(0.5)
            r <- GET(getPhotos)
            count_stat <-  count_stat + 1
          }
          
          if(r$status_code != 200){
            warning('Status code:', r$status, ' for between ', mindate, ' and ', maxdate, ' - message: ', content(r, 'text'))
          }
          
          error <- tryCatch({
            getPhotos_data <- xmlRoot(xmlTreeParse(content(r, 'text')))
            error <- 'success'
          }, error = function(err){
            warning('Between ', mindate, ' and ', maxdate, ' skipped beacuse: ', err)
            error <- 'error'
          })    
          
          if(error != 'error'){
            
            #results are returned in different pages so it is necessary to loop through pages to collect all the data
            #parse the total number of pages
            pages_data <- data.frame(xmlAttrs(getPhotos_data[["photos"]]))
            pages_data[] <- lapply(pages_data, FUN = function(x) as.integer(as.character(x)))
            total_pages <- pages_data["pages",]
            total <- pages_data["pages",]
            
            #if all results are unique get results
            if(total <= 15) {
              
              pics_tmp <- NULL
              
              # loop thru pages of photos and save the list in a DF
              for(i in c(1:total_pages)){
                
                if(!is.null(bbox)){ 
                  
                  getPhotos <- paste(baseURL
                                     ,"&text=",text,"&min_taken_date=",mindate,
                                     "&max_taken_date=",maxdate,
                                     "&bbox=", paste0(bbox[1],",",bbox[2],",",bbox[3],",",bbox[4]),
                                     "&extras=",extras,
                                     "&per_page=",perpage,"&format=",format,"&page="
                                     ,i,sep="")
                  
                } else if(!is.null(woe_id)){
                  
                  getPhotos <- paste(baseURL
                                     ,"&text=",text,"&min_taken_date=",mindate,
                                     "&max_taken_date=",maxdate,
                                     "&woe_id=",woe_id,
                                     "&extras=",extras,
                                     "&per_page=",perpage,"&format=",format,"&page="
                                     ,i,sep="")
                  
                } else {
                  
                  getPhotos <- paste(baseURL
                                     ,"&text=",text,"&min_taken_date=",mindate,
                                     "&max_taken_date=",maxdate,
                                     ifelse(has_geo, paste0("&has_geo=", has_geo), ''),
                                     "&extras=",extras,
                                     "&per_page=",perpage,"&format=",format,"&page="
                                     ,i,sep="")        
                  
                }
                
                r <- GET(getPhotos)
                
                count_stat <- 0
                
                while(r$status_code != 200 & count_stat < 3){
                  Sys.sleep(0.5)
                  r <- GET(getPhotos)
                  count_stat <-  count_stat + 1
                }
                
                if(r$status_code != 200){
                  warning('Status code:', r$status, ' for between ', mindate, ' and ', maxdate, ' page ', i, ' - message: ', content(r, 'text'))
                }
                
                error <- tryCatch({
                  getPhotos_data <- xmlRoot(xmlTreeParse(content(r, 'text'), useInternalNodes = TRUE))
                  error <- 'success'
                }, error = function(err){
                  warning('Between ', mindate, ' and ', maxdate, ' page ', i,' skipped beacuse: ', err)
                  error <- 'error'
                })
                
                if(error != 'error'){
                  getPhotos_data <<- getPhotos_data
                  id <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "id"))                 #extract photo id
                  owner <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "owner"))           #extract user id
                  datetaken <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "datetaken"))   #extract date picture was taken
                  tags <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "tags"))             #extract tags
                  latitude <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "latitude"))     #extract latitude
                  longitude <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "longitude"))   #extract longitude
                  license <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "license"))       #extract license
                  url_s <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "url_s"))           #extract url_s
                  url_m <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "url_m"))           #extract url_m
                  url_l <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "url_l"))           #extract url_l
                  url_o <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "url_o"))           #extract url_o
                  
                  if(!all(is.na(c(id,owner,datetaken,tags,latitude,longitude,license,url_s,url_m,url_l,url_o)))){
                    
                    tmp_df <- data.frame(id, owner, datetaken, tags,
                                         as.numeric(latitude),
                                         as.numeric(longitude), license,
                                         url_s = unlist(url_s), url_m = unlist(url_m),
                                         url_l = unlist(url_l), url_o = unlist(url_o),
                                         stringsAsFactors = FALSE)
                    
                    tmp_df$page <- i
                    pics_tmp <- rbind(pics_tmp, tmp_df)
                    rm(list = 'tmp_df')
                    
                  }
                }
              }
              
              pics <- rbind(pics, pics_tmp)
              
            } 
            
            else if (total > 15){
              
              #else pass date to new dataframe
              tmp_df <- data.frame(mindate, maxdate, stringsAsFactors = FALSE)
              day_tmp <- rbind(day_tmp, tmp_df)
              rm(list = "tmp_df")
            }
            
            day_dates <- rbind(day_dates, day_tmp)
            day_tmp <- NULL
            
          }
          
        }
        
      }
    }
    
    #run searches on daily intervals
    if (!is.null(day_dates)){
     
      skipped_tmp <- NULL
      
      for (x in 1:(nrow(day_dates))){
        
        mindate <- day_dates[x, 1]
        maxdate <- day_dates[x, 2]
        
        day_range <- seq(as.Date(mindate), as.Date(maxdate), by="+1 day")
        
        if (day_range[length(day_range)] != maxdate){
          day_range <- append(day_range, as.Date(maxdate))
        } 
        
        for(d in 1:(length(day_range) - 1)){ 
          
          mindate <- day_range[d]
          maxdate <- day_range[d + 1]
          
          
          if(!is.null(bbox)){
            getPhotos <- paste(baseURL,
                               "&text=", text,
                               "&min_taken_date=", as.character(mindate),
                               "&max_taken_date=", as.character(maxdate),
                               "&bbox=", paste0(bbox[1],",",bbox[2],",",bbox[3],",",bbox[4]),
                               "&extras=", extras,
                               "&per_page=", perpage,
                               "&format=", format,
                               sep = "")
          } else if(!is.null(woe_id)){
            getPhotos <- paste(baseURL,
                               "&text=", text,
                               "&min_taken_date=", as.character(mindate),
                               "&max_taken_date=", as.character(maxdate),
                               "&woe_id=", woe_id,
                               "&extras=", extras,
                               "&per_page=", perpage,
                               "&format=", format,
                               sep = "")     
          } else {
            getPhotos <- paste(baseURL,
                               "&text=", text,
                               "&min_taken_date=", as.character(mindate),
                               "&max_taken_date=", as.character(maxdate),
                               ifelse(has_geo, paste0("&has_geo=", has_geo), ''),
                               "&extras=", extras,
                               "&per_page=", perpage,
                               "&format=", format,
                               sep = "")
          }
          
          r <- GET(getPhotos)
          
          count_stat <- 0
          
          while(r$status_code != 200 & count_stat < 3){
            Sys.sleep(0.5)
            r <- GET(getPhotos)
            count_stat <-  count_stat + 1
          }
          
          if(r$status_code != 200){
            warning('Status code:', r$status, ' for between ', mindate, ' and ', maxdate, ' - message: ', content(r, 'text'))
          }
          
          error <- tryCatch({
            getPhotos_data <- xmlRoot(xmlTreeParse(content(r, 'text')))
            error <- 'success'
          }, error = function(err){
            warning('Between ', mindate, ' and ', maxdate, ' skipped beacuse: ', err)
            error <- 'error'
          })    
          
          if(error != 'error'){
            
            #results are returned in different pages so it is necessary to loop through pages to collect all the data
            #parse the total number of pages
            pages_data <- data.frame(xmlAttrs(getPhotos_data[["photos"]]))
            pages_data[] <- lapply(pages_data, FUN = function(x) as.integer(as.character(x)))
            total_pages <- pages_data["pages",]
            total <- pages_data["pages",]
            
            #if all results are unique get results
            if (total <= 15){
              
              pics_tmp <- NULL
              
              # loop thru pages of photos and save the list in a DF
              for(i in c(1:total_pages)){
                
                if(!is.null(bbox)){ 
                  
                  getPhotos <- paste(baseURL
                                     ,"&text=",text,"&min_taken_date=",mindate,
                                     "&max_taken_date=",maxdate,
                                     "&bbox=", paste0(bbox[1],",",bbox[2],",",bbox[3],",",bbox[4]),
                                     "&extras=",extras,
                                     "&per_page=",perpage,"&format=",format,"&page="
                                     ,i,sep="")
                  
                } else if(!is.null(woe_id)){
                  
                  getPhotos <- paste(baseURL
                                     ,"&text=",text,"&min_taken_date=",mindate,
                                     "&max_taken_date=",maxdate,
                                     "&woe_id=",woe_id,
                                     "&extras=",extras,
                                     "&per_page=",perpage,"&format=",format,"&page="
                                     ,i,sep="")
                  
                } else {
                  
                  getPhotos <- paste(baseURL
                                     ,"&text=",text,"&min_taken_date=",mindate,
                                     "&max_taken_date=",maxdate,
                                     ifelse(has_geo, paste0("&has_geo=", has_geo), ''),
                                     "&extras=",extras,
                                     "&per_page=",perpage,"&format=",format,"&page="
                                     ,i,sep="")        
                  
                }
                
                r <- GET(getPhotos)
                
                count_stat <- 0
                
                while(r$status_code != 200 & count_stat < 3){
                  Sys.sleep(0.5)
                  r <- GET(getPhotos)
                  count_stat <-  count_stat + 1
                }
                
                if(r$status_code != 200){
                  warning('Status code:', r$status, ' for between ', mindate, ' and ', maxdate, ' page ', i, ' - message: ', content(r, 'text'))
                }
                
                error <- tryCatch({
                  getPhotos_data <- xmlRoot(xmlTreeParse(content(r, 'text'), useInternalNodes = TRUE))
                  error <- 'success'
                }, error = function(err){
                  warning('Between ', mindate, ' and ', maxdate, ' page ', i,' skipped beacuse: ', err)
                  error <- 'error'
                })
                
                if(error != 'error'){
                  getPhotos_data <<- getPhotos_data
                  id <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "id"))                 #extract photo id
                  owner <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "owner"))           #extract user id
                  datetaken <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "datetaken"))   #extract date picture was taken
                  tags <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "tags"))             #extract tags
                  latitude <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "latitude"))     #extract latitude
                  longitude <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "longitude"))   #extract longitude
                  license <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "license"))       #extract license
                  url_s <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "url_s"))           #extract url_s
                  url_m <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "url_m"))           #extract url_m
                  url_l <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "url_l"))           #extract url_l
                  url_o <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "url_o"))           #extract url_o
                  
                  if(!all(is.na(c(id,owner,datetaken,tags,latitude,longitude,license,url_s,url_m,url_l,url_o)))){
                    
                    tmp_df <- data.frame(id, owner, datetaken, tags,
                                         as.numeric(latitude),
                                         as.numeric(longitude), license,
                                         url_s = unlist(url_s), url_m = unlist(url_m),
                                         url_l = unlist(url_l), url_o = unlist(url_o),
                                         stringsAsFactors = FALSE)
                    
                    tmp_df$page <- i
                    pics_tmp <- rbind(pics_tmp, tmp_df)
                    rm(list = 'tmp_df')
                    
                  }
                }
              }
              
              pics <- rbind(pics, pics_tmp)
              
            } 
            
            else if (total > 15){
              
              #else pass date to new dataframe
              tmp_df <- data.frame(mindate, maxdate, stringsAsFactors = FALSE)
              skipped_tmp <- rbind(skipped_tmp, tmp_df)
              rm(list = "tmp_df")
            }
            
            skipped_dates <- rbind(skipped_dates, skipped_tmp)
            skipped_tmp <- NULL
            
          }
          
        }
      }
    }

    #warn of skipped dates
    if (!is.null(skipped_dates)){
      
      warning(nrow(skipped_dates), " days skiped as they return too many results")
      warning("Days skipped: ", skipped_dates$mindate)
      
    }
     
    #remove duplicates and return outputs
    pics <- pics[!duplicated(pics$id), ]
    return(pics)
    
}


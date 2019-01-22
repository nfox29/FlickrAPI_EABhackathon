photo_search <-
  function(min_taken = "2015-01-01",
           max_taken = "2019-01-01",
           text = "ring-necked parakeet",          
           bbox = NULL,
           woe_id = 23424975,     
           has_geo = TRUE){
    
    if( !(is.null(bbox) | is.null(woe_id))==TRUE) {
      stop('can not provide bbox and woe_id')
    }
    
    text <- gsub(' ', '+', trimws(text))  
    api_key <- auth$key
    mindate <- min_taken
    maxdate <- max_taken
    pics <- NULL
    
    # set the base search url
    base_url <- if(!is.null(bbox)){
      search_url <- paste("https://api.flickr.com/services/rest/?method=flickr.photos.search&api_key=",api_key,
                          "&text=", text,
                          "&bbox=", paste0(bbox[1],",",bbox[2],",",bbox[3],",",bbox[4]),
                          "&extras=", "date_taken,geo,tags,license,url_sq,url_t,url_s,url_q,url_m,url_n,url_z,url_c,url_l,url_o",
                          "&per_page=", "250",
                          "&format=", "rest",
                          sep = "")
    } else if(!is.null(woe_id)){
      search_url <- paste("https://api.flickr.com/services/rest/?method=flickr.photos.search&api_key=",api_key,
                          "&text=", text,
                          "&woe_id=", woe_id,
                          "&extras=", "date_taken,geo,tags,license,url_sq,url_t,url_s,url_q,url_m,url_n,url_z,url_c,url_l,url_o",
                          "&per_page=", "250",
                          "&format=", "rest",
                          sep = "")     
    } else {
      search_url <- paste("https://api.flickr.com/services/rest/?method=flickr.photos.search&api_key=",api_key,
                          "&text=", text,
                          ifelse(has_geo, paste0("&has_geo=", has_geo), ''),
                          "&extras=", "date_taken,geo,tags,license,url_sq,url_t,url_s,url_q,url_m,url_n,url_z,url_c,url_l,url_o",
                          "&per_page=", "250",
                          "&format=", "rest",
                          sep = "")
    
    }
    
    # create dfs so large searches can be subset dynamically 
    date_df <- data.frame(mindate = mindate, maxdate = maxdate)
    spatial_df <-  data.frame("mindate" = character(), "maxdate" = character()) #add , "bbox" = character()
    
    # start while loop - until all dates are looped through
    while(nrow(date_df) > 0) {
      
      # set search dates
      mindate = date_df[1, "mindate"]
      maxdate = date_df[1, "maxdate"]
      
      # get total number of results
      r <- GET(paste(base_url,
                     "&min_taken_date=", as.character(mindate),
                     "&max_taken_date=", as.character(maxdate), sep=""))
      
      #put first error catch here
      count_stat <- 0
      
      while(r$status_code != 200 & count_stat < 3){
        Sys.sleep(0.5)
        r <- GET(paste(base_url,
                       "&min_taken_date=", as.character(mindate),
                       "&max_taken_date=", as.character(maxdate), sep=""))
        count_stat <-  count_stat + 1
      }
      
      if(r$status_code != 200){
        warning('Status code:', r$status, ' for ', mindate, ' to ', maxdate, ' - message: ', content(r, 'text'))
      }
      
      error <- tryCatch({
        getPhotos_data <- xmlRoot(xmlTreeParse(content(r, 'text')))
        error <- 'sucess'
      }, error = function(err){
        warning('Dates between ', mindate, ' and ', maxdate, ' skipped beacuse: ', err)
        error <- 'error'
      })    
      
      if(error != 'error'){
      
      getPhotos_data <- xmlRoot(xmlTreeParse(content(r, 'text')))
      pages_data <- data.frame(xmlAttrs(getPhotos_data[["photos"]]))
      pages_data[] <- lapply(pages_data, FUN = function(x) as.integer(as.character(x)))
      total_pages <- pages_data["pages",]
      total <- pages_data["total",]
      
      #if > 4000 and not single days, split
      if(total > 4000 && (as.Date(mindate) != (as.Date(maxdate) - 1))) {
        
        x <- ceiling(total/4000)
        y <- length(seq(as.Date(mindate), as.Date(maxdate), by="+1 day"))
        
        #if x + 1 is larger than number of days, split in to single days
        if (x + 1 > y){
          
          dates <- seq(as.Date(mindate), as.Date(maxdate), by="+1 day")
          
        }
        
        #else split accoring to x
        else {
          
          dates <- seq(as.Date(mindate), as.Date(maxdate), length.out = x + 1)
          
        }
        
        # create dataframe with minmaxdates
        date_df <- rbind(date_df[-1,], data.frame(mindate = dates[1:(length(dates) - 1)], maxdate = dates[2:length(dates)]))
      
        }
      
      #if > 4000 and single days, pass days to be split by area
      else if (total > 4000 && (as.Date(mindate) == (as.Date(maxdate) - 1))){
        
        spatial_df <- rbind(spatial_df, data.frame(mindate = mindate, maxdate = maxdate))
        
        date_df <- date_df[-1,]
        
      }
      
      #if all conditions are satisfied get data
      else if (total <= 4000 && total_pages > 0){ 
        
        #get data second error catch here
        pics_tmp <- NULL
        
        # loop thru pages of photos and save the list in a DF
        for(i in c(1:total_pages)){
          
          
          r <- GET(paste(base_url,
                         "&min_taken_date=", as.character(mindate),
                         "&max_taken_date=", as.character(maxdate), "&page="
                         ,i, sep=""))
          
          count_stat <- 0
          
          while(r$status_code != 200 & count_stat < 3){
            Sys.sleep(0.5)
            r <- GET(paste(base_url,
                           "&min_taken_date=", as.character(mindate),
                           "&max_taken_date=", as.character(maxdate), "&page="
                           ,i, sep=""))
            count_stat <-  count_stat + 1
          }
          
          if(r$status_code != 200){
            warning('Status code:', r$status, ' for dates between ', mindate, ' and ', maxdate, ' page ', i, ' - message: ', content(r, 'text'))
          }
          
          error <- tryCatch({
            getPhotos_data <- xmlRoot(xmlTreeParse(content(r, 'text'), useInternalNodes = TRUE))
            error <- 'sucess'
          }, error = function(err){
            warning('Dates between ', mindate, ' and ', maxdate, ' page ', i,' skipped beacuse: ', err)
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
        
        date_df <- date_df[-1,]
        
        #end second error catch here
        
        }

      #else search is empty, skip
      else {
        
        date_df <- date_df[-1,]
        
      } 
      
      #end first error catch here
      }
      
    }
    
  
    return(pics)
    
    }
    



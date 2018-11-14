photosSearches <-
  function(min_taken = NULL,
           max_taken = NULL,
           text,          
           bbox = NULL,
           woe_id =NULL,        
           has_geo = TRUE){
    
    if( !(is.null(bbox) | is.null(woe_id))==TRUE) {
      stop('can not provide bbox and woe_id')
    }
    
    text <- gsub(' ', '+', trimws(text))  
    api_key <- auth$key
    perpage <- "500"
    format <- "rest"
    extras <- "date_taken,geo,tags,license,url_sq,url_t,url_s,url_q,url_m,url_n,url_z,url_c,url_l,url_o"
    baseURL <- paste("https://api.flickr.com/services/rest/?method=flickr.photos.search&api_key=",api_key,sep="")   #set base URL
    pics<-NULL
    
    monthrange <- seq(as.Date(min_taken), as.Date(max_taken), by="+1 month")
    
    if (monthrange[length(monthrange)] != max_taken){
      monthrange <- append(monthrange, as.Date(max_taken))
    } 
    
    pb <- txtProgressBar(min = 0, max = length(monthrange), style = 3)
    
    for(m in 1:(length(monthrange) - 1)){ 
      
      mindate <- monthrange[m]
      maxdate <- monthrange[m + 1]
      
      
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
        warning('Status code:', r$status, ' for month between ', mindate, ' and ', maxdate, ' - message: ', content(r, 'text'))
      }
      
      error <- tryCatch({
        getPhotos_data <- xmlRoot(xmlTreeParse(content(r, 'text')))
        error <- 'success'
      }, error = function(err){
        warning('Month between ', mindate, ' and ', maxdate, ' skipped beacuse: ', err)
        error <- 'error'
      })    
      
      if(error != 'error'){
        
        #results are returned in different pages so it is necessary to loop through pages to collect all the data
        #parse the total number of pages
        pages_data <- data.frame(xmlAttrs(getPhotos_data[["photos"]]))
        pages_data[] <- lapply(pages_data, FUN = function(x) as.integer(as.character(x)))
        total_pages <- pages_data["pages",]
        total <- pages_data["pages",]
        
        if(total > 0){
          
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
              warning('Status code:', r$status, ' for month between ', mindate, ' and ', maxdate, ' page ', i, ' - message: ', content(r, 'text'))
            }
            
            error <- tryCatch({
              getPhotos_data <- xmlRoot(xmlTreeParse(content(r, 'text'), useInternalNodes = TRUE))
              error <- 'success'
            }, error = function(err){
              warning('Month between ', mindate, ' and ', maxdate, ' page ', i,' skipped beacuse: ', err)
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
      }
      
      setTxtProgressBar(pb, + m + 1)
      
    }
    
    close(pb)
    return(pics)
    
  }
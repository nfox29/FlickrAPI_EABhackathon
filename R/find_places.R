find_places <- function(place){

newURSltester <- paste("https://api.flickr.com/services/rest/?method=flickr.places.find&api_key=", api_key, sep = "") 
query <- place

getPhotos <- paste0(newURSltester
                    ,"&query=",query)

getPhotos_data <- XML::xmlRoot(XML::xmlTreeParse(RCurl::getURL
                                       (getPhotos,ssl.verifypeer=FALSE, useragent = "flickr")
                                       ,useInternalNodes = TRUE ))

place_id<-XML::xpathSApply(getPhotos_data,"//place",XML::xmlGetAttr,"place_id")                 
woe_id<-XML::xpathSApply(getPhotos_data,"//place",XML::xmlGetAttr,"woeid")                 
latitude<-XML::xpathSApply(getPhotos_data,"//place",XML::xmlGetAttr,"latitude")                 
longitude<-XML::xpathSApply(getPhotos_data,"//place",XML::xmlGetAttr,"longitude")                 
place_url<-XML::xpathSApply(getPhotos_data,"//place",XML::xmlGetAttr,"place_url")                 
woe_name<-XML::xpathSApply(getPhotos_data,"//place",XML::xmlGetAttr,"woe_name")                 

places<-data.frame(cbind(woe_name,place_url,place_id,woe_id,latitude,longitude))

return(places)
}

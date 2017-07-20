### freq.hashtag()
# input: dataframe of Twitter metadata OR vector of hashtags
# output: sorted dataframe of counts of distinct hashtags

freq.hashtag <- function(indata, #dataframe or vector of hashtags
                         list.split = "\t", #hashtag separator
                         hashtags = NULL #column for hashtags
                         ) {

  #check for and load dplyr
  if (!require(dplyr)) {
    install.packages(dplyr)
    library(dplyr)
  } else {
    library(dplyr)
  }

  #pull column from df, if given
  if (!is.null(hashtags)) {
    indata <- indata[[hashtags]]
  }
  
  #create df of all hashtags
  df.hashtag <- indata %>%
    unlist() %>% #makes tibble and list data readable
    strsplit(split = list.split) %>% #some have multiple hashtags, split by \t by default
    unlist() %>% #strsplit() outputs a list
    strsplit(split = " ") %>% #some tags split by spaces
    unlist() %>% # unlist again
    tolower() %>% #make all lowercase
    as.data.frame()
  
  #column name needed
  names(df.hashtag) <- "hashtag"
  
  #sort, count, and order by count
  df.hashtag <- df.hashtag %>%
    dplyr::group_by(hashtag) %>% #gather by hashtag
    dplyr::mutate(hashtag_count = n()) %>% #create new column of counts
    dplyr::distinct(.keep_all = TRUE) %>% #discard duplicates
    dplyr::arrange(desc(hashtag_count)) #order by hashtag_count, descending
    
}
  
#end function freq.hashtag()
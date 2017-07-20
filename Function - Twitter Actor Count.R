### freq.actors()
# input: dataframe of Twitter metadata
# output: sorted dataframe of counts of distinct Twitter IDs, labeled
#         by their corresponding ID Name

freq.actors <- function(indata, #input dataset
                        actorid = "actor_id", #define column for who tweeted (ID number)
                        actornames = "actor_displayName") { #define column for who tweeted (ID Name)
  #check for, install, and load dplyr
  if (!require(dplyr)) {
    install.packages(dplyr)
    library(dplyr)
  } else {
    library(dplyr)
  }

  #create DF of distinct IDs  
  counts <- indata %>%
    dplyr::count_(actorid, sort = TRUE) %>%
    as.data.frame()
  
  #create actor name key DF
  keylist <- indata %>%
               dplyr::select_(actorid, actornames) %>%
                as.data.frame()
  
  #join count and keylist DFs by ID, filter distinct
  inner_join(counts, keylist, by = actorid) %>%
    as.data.frame() %>%
    dplyr::distinct_()
}
#end freq.actors()
###############################################################################
##### CODE TO FORMAT BIBTEX ENTRIES TO RMARKDOWN TO CREATE A BIBLIOGRAPHY #####
###############################################################################

### HOW TO USE THE CODES:
# 1. CREATE A BIBTEX FILE USING, FOR INSTANCE, MENDELEY
# 2. YEAR SHOULD NOT BE SPECIFIED IN CASE OF IN PRESS PAPERS
# 3. IF AVAILABLE, THREE URLS NEED TO BE PROVIDED. THE ORDER OF THESE URLS ARE
#     (1) LINK TO WEBSITE OF JOURNAL, (2) LINK TO PDF OF PAPER, AND (3) LINK 
#     TO OSF. IF THESE URLS ARE NOT AVAILBLE, THEY DO NOT NEED TO BE PROVIDED. 
#     HOWEVER, THE ORDER IS IMPORTANT, SO NAs NEED TO BE ADDED IF, FOR EXAMPLE,
#     A LINK TO THE WEBSITE OF THE JOURNAL IS NOT AVAILABLE, BUT OSF IS AVAILABLE.

################################################################################

#################
### FUNCTIONS ###
#################

### Function for checking whether a RMarkdown link is NA
na_link <- function(x)
{
  substr(x, start = nchar(x)-1, stop = nchar(x)) == "()"
}

################################################################################

### Load overview file
# df <- read.csv("presentation_files/overview_presentations.csv")
df <- read.csv2("static/presentations_files/overview_presentations.csv")

entries <- matrix(NA, nrow = nrow(df), ncol = 2, dimnames = list(1:nrow(df), 
                                                                 c("year", "entry")))

for (i in 1:nrow(df))
{
  
  dat <- df[i, ]
  
  entries[i,"year"] <- dat$year
  
  #############
  ### TITLE ###
  #############
  
  title <- dat$ï..title
  
  #############
  ### EVENT ###
  #############
  
  event <- dat$event
  
  ################
  ### LOCATION ###
  ################
  
  location <- dat$location
  
  ######################
  ### LINK TO SLIDES ###
  ######################
  
  slides <- paste0("[Slides](", dat$slides, ")")
  
  #####################
  ### LINK TO VIDEO ###
  #####################
  
  video <- paste0("[Video](", dat$video, ")")
  
  ################################################################################
  
  if (na_link(slides) & na_link(video))
  {
    entries[i,"entry"] <- paste0("* ", title, ". ", event, ". ", location, ". ")  
  } else if (na_link(video))
  {
    entries[i,"entry"] <- paste0("* ", title, ". ", event, ". ", location, ". ", 
                                 slides)  
  } else
  {
    entries[i,"entry"] <- paste0("* ", title, ". ", event, ". ", location, ". ", 
                                 slides, " ", video)  
  }
  
}
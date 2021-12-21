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

rm(list = ls())

### Function for checking whether a RMarkdown link is NA
na_link <- function(x)
{
  substr(x, start = nchar(x)-2, stop = nchar(x)-1) == "NA"
}

### Function to get authors in the correct format
get_authors <- function(names)
{
  
  for (m in 1:nrow(names))
  {
    
    tmp <- names[m, ]
    
    ### Get the right order with last name before initials
    if (is.na(tmp$middle_name))
    {
      initials <- tmp$first_name
    } else
    {
      initials <- paste(tmp$first_name, tmp$middle_name)  
    }
    
    ### Add period after each initial
    initials <- gsub('([A-Z])', '\\1.', initials)
    
    ### Add comma after initials
    if (m == 1)
    {
      authors <- paste0(tmp$last_name, ", ", initials)
    } else
    {
      authors <- c(authors, paste0(tmp$last_name, ", ", initials))
    }
  }
  
  ### Number of authors
  nr <- nrow(names)
  
  ### Add comma after name of all authors
  for (m in 1:(nr-1))
  {
    if (nr > 2)
    {
      authors[m] <- paste0(authors[m], sep = ", ")  
    } else
    {
      authors[m] <- paste0(authors[m], " ")  
    }
  }
  
  ### If I am the single author, do not add & symbol
  if (nr > 1)
  {
    authors[nr] <- paste0("& ", authors[nr])
  }
  
  ### Make my own name bold
  authors <- ifelse(authors == "van Aert, R. C. M., ", "**van Aert, R. C. M.**, ", authors)
  authors <- ifelse(authors == "van Aert, R. C. M. ", "**van Aert, R. C. M.** ", authors)
  authors <- ifelse(authors == "& van Aert, R. C. M.", "& **van Aert, R. C. M.**", authors)
  
  ### Restrict the number of authors that are displayed
  if (length(authors) > 6)
  {
    authors <- c(authors[1:5], " ..., ", authors[length(authors)])  
  }
  
  ### If my name does not get displayed, add it
  if (("**van Aert, R. C. M.**, " %in% authors | 
       "& **van Aert, R. C. M.**" %in% authors | 
       "**van Aert, R. C. M.** " %in% authors) == FALSE)
  {
    tmp <- authors[6:7]
    authors[6] <- "..., **van Aert, R. C. M.**, "
    authors[7:8] <- tmp
  }
  
  ### Collapse all elements in the vector to a single one
  authors <- paste(authors, collapse = "")
  
  ### Use Open Science Collaboration as author name for RPP
  authors <- ifelse(names[1,"full_name"] == "Open Science Collaboration", 
                    "Open Science Collaboration", authors)
  
  return(authors)
}

################################################################################

### Load bibtex file
df <- bib2df::bib2df("C:/users/s787802/desktop/publications.bib", 
                     separate_names = TRUE)
df <- as.data.frame(df)

### Code currently works for paper with Wolfgang
i <- 21
df[i,]

entries <- matrix(NA, nrow = nrow(df), ncol = 3, dimnames = list(1:nrow(df), 
                                                                 c("year", "entry", 
                                                                   "submitted")))

for (i in 1:nrow(df))
{
  
  dat <- df[i, ]
  
  entries[i,"year"] <- dat$YEAR
  
  if (dat$CATEGORY == "ARTICLE")
  {
    
    ###############
    ### AUTHORS ###
    ###############
    
    authors <- get_authors(names = dat$AUTHOR[[1]])
    
    ############
    ### YEAR ###
    ############
    
    year <- dat$YEAR
    
    ### Return "in press" if your is unknown
    year <- ifelse(is.na(year), "in press", year)
    
    #############
    ### TITLE ###
    #############
    
    title <- dat$TITLE
    
    ###############
    ### JOURNAL ###
    ###############
    
    journal <- paste0("*", dat$JOURNAL, "*")
    
    ##############
    ### VOLUME ###
    ##############
    
    volume <- paste0("*", dat$VOLUME, "*")
    
    #############
    ### ISSUE ###
    #############
    
    issue <- paste0("(", dat$ISSUE, ")")
    
    #############
    ### PAGES ###
    #############
    
    pages <- gsub("--", "-", dat$PAGES)
    
    ###########
    ### DOI ###
    ###########
    
    ### If you add to the "URL" field in Mendeley the link to the pdf and OSF, 
    # you first need to split the URL field to get extract the correct URL
    split <- strsplit(dat$URL, split = " ")
    
    doi <- paste0("[", dat$DOI, "](", split[[1]][1], ")")
    
    ###################
    ### LINK TO PDF ###
    ###################
    
    pdf <- paste0("[PDF](", split[[1]][2], ")")
    
    ###################
    ### LINK TO OSF ###
    ###################
    
    osf <- paste0("[OSF](", split[[1]][3], ")")
    
    ################################################################################
    
    if (is.na(dat$JOURNAL) == FALSE)
    { # If journal not NA, create entry
      
      if (is.na(pages) & na_link(osf) & na_link(doi) & volume == "*NA*" & 
          issue == "(NA)")
      { # If page numbers, OSF, doi, volume, and issue are missing
        entries[i,"entry"] <- paste0("* ", authors, " (", year, "). ", title, ". ", journal, 
                                     ". ", pdf) 
      } else if (is.na(pages) & issue == "(NA)" & volume == "*NA*")
      {
        entries[i,"entry"] <- paste0("* ", authors, " (", year, "). ", title, ". ", journal, 
                                     ". doi: ", doi, " ", pdf, " ", osf) 
      } else if (is.na(pages) & issue == "(NA)")
      { # If page numbers and isue are missing, exclude these from citation
        entries[i,"entry"] <- paste0("* ", authors, " (", year, "). ", title, ". ", journal, 
                                     ", ", volume, ". doi: ", doi, " ", 
                                     pdf) 
      } else if (is.na(pages) & na_link(osf) & na_link(pdf))
      { # Page numbers and OSF and PDF links are missing
        entries[i,"entry"] <- paste0("* ", authors, " (", year, "). ", title, ". ", journal, 
                                     ", ", volume, issue, ". doi: ", doi)
      } else if (is.na(pages) & na_link(osf))
      { # If page numbers and OSF are missing, exclude these from citation
        entries[i,"entry"] <- paste0("* ", authors, " (", year, "). ", title, ". ", journal, 
                                     ", ", volume, issue, ". doi: ", doi, " ", 
                                     pdf) 
      } else if (issue == "(NA)" & na_link(osf))
      { # If issue and OSF are missing
        entries[i,"entry"] <- paste0("* ", authors, " (", year, "). ", title, ". ", journal, 
                                     ", ", volume, ", ", pages, ". doi: ", doi, " ", 
                                     pdf) 
      } else if (na_link(osf) & na_link(pdf))
      { # OSF and PDF links are missing, so do not include link to OSF and PDF in citation
        entries[i,"entry"] <- paste0("* ", authors, " (", year, "). ", title, ". ", journal, 
                                     ", ", volume, issue, ", ", pages, ". doi: ", doi)
      } else if (na_link(osf))
      { # OSF link is missing, so do not include link to OSF in citation
        entries[i,"entry"] <- paste0("* ", authors, " (", year, "). ", title, ". ", journal, 
                                     ", ", volume, issue, ", ", pages, ". doi: ", doi, " ", 
                                     pdf)
      } else
      {
        entries[i,"entry"] <- paste0("* ", authors, " (", year, "). ", title, ". ", journal, 
                                     ", ", volume, issue, ", ", pages, ". doi: ", doi, " ", 
                                     pdf, " ", osf)
      }
      
      entries[i,"submitted"] <- 0
    } else
    { # If journal is NA, manuscript is submitted
      if (is.na(pages) & na_link(osf) & na_link(doi) & volume == "*NA*" & 
          issue == "(NA)" & na_link(pdf))
      { # If page numbers, OSF, PDF, doi, volume, and issue are missing
        entries[i,"entry"] <- paste0("* ", authors, " (", year, "). ", title, ". ")
      } else 
      {
        entries[i,"entry"] <- paste0("* ", authors, " (", year, "). ", title, 
                                     ". doi: ", doi, " ", pdf, " ", osf)
      }
      
      entries[i,"submitted"] <- 1
    }
    
  } else if (dat$CATEGORY == "BOOK")
  {
    ###############
    ### AUTHORS ###
    ###############
    
    authors <- get_authors(dat$AUTHOR[[1]])
    
    ############
    ### YEAR ###
    ############
    
    year <- dat$YEAR
    
    #############
    ### TITLE ###
    #############
    
    title <- dat$TITLE
    
    #################
    ### PUBLISHER ###
    #################
    
    publisher <- dat$PUBLISHER
    
    ###########
    ### DOI ###
    ###########
    
    ### If you add to the "URL" field in Mendeley the link to the pdf and OSF, 
    # you first need to split the URL field to get extract the correct URL
    split <- strsplit(dat$URL, split = " ")
    
    doi <- paste0("[", dat$DOI, "](", split[[1]][1], ")")
    
    ###################
    ### LINK TO PDF ###
    ###################
    
    pdf <- paste0("[PDF](", split[[1]][2], ")")
    
    ############################################################################
    
    entries[i,"entry"] <- paste0("* ", authors, " (", year, "). ", title, ". ",
                                 publisher, ". doi: ", doi, " ", pdf)
    
  } else if (dat$CATEGORY == "BOOK_SECTION")
  {
    ###############
    ### AUTHORS ###
    ###############
    
    authors <- get_authors(names = dat$AUTHOR[[1]])
    
    ############
    ### YEAR ###
    ############
    
    year <- dat$YEAR
    
    ### Return "in press" if your is unknown
    year <- ifelse(is.na(year), "in press", year)
    
    #############
    ### TITLE ###
    #############
    
    title <- dat$TITLE
    
    #################
    ### PUBLISHER ###
    #################
    
    publisher <- dat$PUBLISHER
    
    ###############
    ### EDITORS ###
    ###############
    
    editors <- get_authors(names = dat$EDITOR[[1]])
    
    ### Quick-and-dirty way to remove my own name
    editors <- strsplit(editors, split = "NANA")[[1]][1]
    
    ##################
    ### BOOK TITLE ###
    ##################
    
    book_title <- paste0("*", dat$JOURNAL, "*")
    
    ###########
    ### DOI ###
    ###########
    
    ### If you add to the "URL" field in Mendeley the link to the pdf and OSF, 
    # you first need to split the URL field to get extract the correct URL
    split <- strsplit(dat$URL, split = " ")
    
    doi <- paste0("[", dat$DOI, "](", split[[1]][1], ")")
    
    ###################
    ### LINK TO PDF ###
    ###################
    
    pdf <- paste0("[PDF](", split[[1]][2], ")")
    
    ###################
    ### LINK TO OSF ###
    ###################
    
    osf <- paste0("[OSF](", split[[1]][3], ")")
    
    ############################################################################
    
    entries[i,"entry"] <- paste0("* ", authors, " (", year, "). ", title, ". In ",
                                 editors, " ", book_title, ". ", publisher, 
                                 ". doi: ", doi, " ", pdf, " ", osf)
  }
  
}

dump("entries", file = "static/publications_files/entries.txt")

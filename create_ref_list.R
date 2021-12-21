rm(list = ls())

### Function for checking whether a RMarkdown link is NA
na_link <- function(x)
{
  substr(x, start = nchar(x)-2, stop = nchar(x)-1) == "NA"
}

### Load bibtex file
df <- bib2df::bib2df("C:/users/s787802/desktop/publications.bib")
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
    
    ### Number of authors
    nr <- length(dat$AUTHOR[[1]])
    
    ### Add periods after initials
    authors <- gsub('([A-Z])\\s', '\\1. ', dat$AUTHOR[[1]])
    authors <- paste0(authors, ".")
    
    ### Add comma after name of all authors
    for (m in 1:(nr-1))
    {
      authors[m] <- paste0(authors[m], sep = ", ")  
    }
    
    authors[nr] <- paste0("& ", authors[nr])
    
    ### Make my own name bold
    authors <- ifelse(authors == "van Aert, R. C. M., ", "**van Aert, R. C. M.**, ", authors)
    authors <- ifelse(authors == "& van Aert, R. C. M.", "& **van Aert, R. C. M.**", authors)
    
    ### Restrict the number of authors that are displayed
    if (length(authors) > 6)
    {
      authors <- c(authors[1:5], " ..., ", authors[length(authors)])  
    }
    
    ### If my name does not get displayed, add it
    if (("**van Aert, R. C. M.**, " %in% authors | 
         "& **van Aert, R. C. M.**" %in% authors) == FALSE)
    {
      tmp <- authors[6:7]
      authors[6] <- "..., **van Aert, R. C. M.**, "
      authors[7:8] <- tmp
    }
    
    ### Collapse all elements in the vector to a single one
    authors <- paste(authors, collapse = "")
    
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
    
    ##############
    ### NUMBER ###
    ##############
    
    number <- paste0("(", dat$NUMBER, ")")
    
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
          number == "(NA)")
      { # If page numbers, OSF, doi, volume, and number are missing
        entries[i,"entry"] <- paste0("* ", authors, " (", year, "). ", title, ". ", journal, 
                                     ". ", pdf) 
      } else if (is.na(pages) & number == "(NA)" & volume == "*NA*")
      {
        entries[i,"entry"] <- paste0("* ", authors, " (", year, "). ", title, ". ", journal, 
                                     ". doi: ", doi, " ", pdf, " ", osf) 
      } else if (is.na(pages) & na_link(osf))
      { # If page numbers and OSF are missing, exclude these from citation
        entries[i,"entry"] <- paste0("* ", authors, " (", year, "). ", title, ". ", journal, 
                                     ", ", volume, number, ". doi: ", doi, " ", 
                                     pdf) 
      } else if (number == "(NA)" & na_link(osf))
      { # If number and OSF are missing
        entries[i,"entry"] <- paste0("* ", authors, " (", year, "). ", title, ". ", journal, 
                                     ", ", volume, ", ", pages, ". doi: ", doi, " ", 
                                     pdf) 
      } else if (na_link(osf))
      { # OSF link is missing, so do not include link to OSF in citation
        entries[i,"entry"] <- paste0("* ", authors, " (", year, "). ", title, ". ", journal, 
                                     ", ", volume, number, ", ", pages, ". doi: ", doi, " ", 
                                     pdf) 
      } else
      {
        entries[i,"entry"] <- paste0("* ", authors, " (", year, "). ", title, ". ", journal, 
                                     ", ", volume, number, ", ", pages, ". doi: ", doi, " ", 
                                     pdf, " ", osf)
      }
      
      entries[i,"submitted"] <- 0
    } else
    { # If journal is NA, manuscript is submitted
      entries[i,"entry"] <- paste0("* ", authors, " (", year, "). ", title, 
                                   ". doi: ", doi, " ", pdf, " ", osf)
      entries[i,"submitted"] <- 1
    }
    
  }
  
}

dump("entries", file = "static/publications_files/entries.txt")
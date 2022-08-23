## ---------------------------
##
## Script name: regulations_gov_functions
##
## Purpose of script: functions for scraping comment data from regulations.gov
##
## Author: Bennett McIntosh
##
## Date Created: 2022-08-16
##
## Email: bamcintosh@wisc.edu
##
## ---------------------------
##
## Notes: 
## ---------------------------


# Necessary libraries

library(jsonlite)
library(httr)
library(tidyverse)
library(pdftools)
library(dplyr)
library(stringr)
library(xlsx)

# Wrapper function to wait out connection issues and the GSA's rate limits
get_or_wait <- function(url) {
  # A wrapper for API requests from GSA; pauses for 1m on connection error
  # and 1hr 5m if we're nearing the hourly rate limit
  errorSleep <- 60
  rateLimSleep <- 3900
  r <- NA
  
  # GET() within a trycatch in case GET() returns an error
  while (is.na(r[1])) {
    r <- tryCatch(
      { r <- GET(url) }, 
      error=function(cond) {
        message(paste("error in fetching url: ", url, " sleeping for ", 
                      errorSleep, "\n", cond))
        Sys.sleep(errorSleep)
        return(NA)
      }
    )
  }
  
  #TODO: add a way of catching when we've actually hit the rate limit
  
  # Your remaining requests before the rate limit are stored in r$headers
  remaining <- as.integer(r$headers$`x-ratelimit-remaining`)
  
  # Catch an error I have not yet been able to reproduce, so just in case...
  if (length(remaining < 5) == 0) {
    message("got error where 'remaining < 5' is of length 0 happened again,",
            " saving raw data")
    saveRDS(r, "badRawComment.RData")
  }
  
  # If remaining is low, sleep 1hr then resume execution, reporting with message
  while (remaining < 5) {
    pauseTime <- lubridate::now()
    message("APPROACHING RATE LIMIT: pausing until 1hr 5min from ", pauseTime, 
            "\n", "requests remaining currently: ", remaining, "\n")
    Sys.sleep(rateLimSleep)
    restartTime <- lubridate::now()
    r <- GET(url)
    remaining <- as.integer(r$headers$`x-ratelimit-remaining`)
    message("Restarting at ", restartTime, "; requests remaining currently: ", 
            remaining, "\n")
  }
  return(r)
}

# Various useful helper functions
nullToNA <- function(x) {
  # replace NULL elements of the list x with NA
  x[sapply(x, is.null)] <- NA
  return(x)
}

slug <- function(string, sep = "-") {
  # return the characters after the last instance of sep in string 
  numSegments <- str_count(string, sep) + 1
  return(word(string, numSegments, sep = sep))
}

fixTimeString <- function(yyyymmddThhmmssZ) {
  # reformats time strings for use in requests to the GSA's API
  time_UTC <- as.POSIXct(sub("T", " ", yyyymmddThhmmssZ), tz = "UTC")
  time_EST <- sub(" E*T", "", lubridate::with_tz(time_UTC, "America/New_York"))
  return(time_EST)
}

# Function for grabbing a page of comments
getAPageOfComments <- function(n, objectID, last_mod = "", pSize = 250) {
  # gets the nth page of comments on objectID modified after last_mod 
  if (pSize < 5) {
    pSize <- 5 
    warning("\npage size too small; now set to 5\n")
  }
  if (pSize > 250) {
    pSize <- 250
    warning("\npage size too large; now set to 250\n")
  }
  
  if (last_mod == "") {
    url <- paste0("https://api.regulations.gov/v4/comments?filter[commentOnId]",
                  "=", objectID, "&page[size]=", pSize, "&page[number]=", n,
                  "&sort=lastModifiedDate,documentId&api_key=", api_key)
  } else {
    # Replace the space in the time string with "%20" for url uses
    last_mod <- sub(" ", "%20", last_mod)
    url <- paste0("https://api.regulations.gov/v4/comments?filter[commentOnId]",
                  "=", objectID, "&filter[lastModifiedDate][ge]=", last_mod, 
                  "&page[size]=", pSize, "&page[number]=", n, 
                  "&sort=lastModifiedDate,documentId&api_key=", api_key)
  }
  
  raw_comments <- get_or_wait(url)  
  comments <- fromJSON(rawToChar(raw_comments$content)) 
  return(comments)
}

# Gets the total number of comments on a given objectID
getCommentCount <- function(objectID){
  url <- paste0("https://api.regulations.gov/v4/comments?filter[commentOnId]=",
                objectID, "&api_key=", api_key)
  
  raw_comments <- get_or_wait(url)
  comments <- fromJSON(rawToChar(raw_comments$content))
  return(comments$meta$totalElements)
}

# Gets the comment at the url 'link'
getCommentByUrl <- function(link){
  raw_comment <- get_or_wait(paste0(link, "?include=attachments&api_key=", api_key))
  comment <- fromJSON(rawToChar(raw_comment$content))
}

# Gets the comment with the given id (NOT the hexadecimal object ID) 
getCommentById <- function(id) {
  url <- paste0("https://api.regulations.gov/v4/comments/", id)
  return(getCommentByUrl(url))
} # Ready

# Attempts to pull the author name from the comment title; returns title on fail
getAuthorName <- function(comment) {
  title <- comment$data$attributes$title
  if (startsWith(title, "Anonymous")) { return("Anonymous") }
  if (startsWith(title, "Mass Comment Campaign Sponsored")) {
    return(paste(substr(title, 36, 1000), "(mass mail)"))
  }
  if (startsWith(title, "Mass Comment Campaign sponsored")) {
    return(paste(substr(title, 36, 1000), "(mass mail)"))
  }
  if (startsWith(title, "Mass Comment Campaign sponsoring organization unknown")) {
    return("unknown organization (mass mail)")
  }
  if (startsWith(title, "Comment submitted by")) {
    return(substr(title, 22, 1000))
  }
  if (startsWith(title, "Comment by")) {
    return(substr(title, 11, 1000))
  }
  
  # if we haven't met the previous patterns, give up
  return(title)
} # Ready 

# Creates the name of a title
# - prefixed with a##_, where ## is its index in the attachment list
# - then using the title itself
# - truncated if the resulting filename would be longer than maxLen
filenameFromTitle <- function(j, title, format = "pdf", maxLen = 64) {
  # to avoid path errors, we replace "/" with "-" 
  filename <- paste0(sprintf("a%02d_", j), gsub("/", "-",title))
  if (nchar(title) > maxLen) {title <- substr(title, 1, maxLen)}
  return(paste0(filename, ".", format))
} 

# Gets a list of attachment filenames for a comment, using filenameFromTitle() 
filenamesFromComment <- function(comment, maxLen = 64) {
  
  N <- length(comment$included$id)
  filenames = rep(NA, N)
  for (i in 1:N) {
    availableFormats <- comment[["included"]][["attributes"]][["fileFormats"]][[i]][[2]]
    if ("pdf" %in% availableFormats) {
      itemFormat <- "pdf"
      fi <- match("pdf", availableFormats)
    } else {
      itemFormat <- availableFormats[1]
      fi <- 1
    }
    filenames[i] <- filenameFromTitle(comment$included$attributes$docOrder[i], 
                                      comment$included$attributes$title[i],
                                      format = itemFormat, maxLen = maxLen)
  }
  return(filenames)
} 

# Download wrapper that waits a minute between tries if service interrupted
resilientDownload <- function(link, destfile, suppress = TRUE) {
  
  statusCode <- 1
  errorSleep <- 60 
  
  if (file.exists(destfile)) { file.remove(destfile) }  
  
  while (!file.exists(destfile)) {
    tryCatch( { 
      download.file(link[[1]], destfile = destfile, mode = "wb", 
                      quiet = suppress) 
      }, error=function(cond) {
        message(paste("error in fetching file at: ", link, " sleeping for ", 
                      errorSleep, "\n", cond))
        Sys.sleep(errorSleep)
      }
    )
  }
  return(0)
}

# for all files in path, returns the percent of tokens which are English words
percentEnglish <- function(pathname, words = NA) {
  
  # if no wordlist is specified, use the one included in repo 
  if (is.na(words)) {
    words <- lapply(read_lines("corpora/wordlist.txt", skip = 44), tolower)
  }
  rt_csv <- readtext(paste0(pathname, "/*"))
  corpus_csv <- corpus(rt_csv)
  tokens_csv <- tokens(corpus_csv, remove_punct = TRUE, split_hyphens = TRUE, 
                       remove_url = TRUE, remove_numbers = TRUE)
  summary(tokens_csv)
  nDocs = length(tokens_csv)
  pcts <- rep(0, length = nDocs)
  names(pcts) <- names(tokens_csv)
  message("processing", nDocs, "documents")
  for (i in 1:nDocs) {
    if (!(i %% 100)) { message(i, "of", nDocs, "\n") }
    if (length(tokens_csv[[i]]) == 0) {
      pcts[i] <- 0
    } else {
      pcts[i] <- sum(lapply(tokens_csv[[i]], tolower) %in% words) /
        length(tokens_csv[[i]])
    }
  }
  return(pcts)
}

# Downloads attachments from the web, and returns some details about them
downloadAttachmentsTo <- function(comment, pathPrefix = "test") {
  
  # create the directory to which we are downloading the documents
  if (!dir.exists(pathPrefix)) { dir.create(pathPrefix) }
  
  # edge case for if the attachments consist of a single, restricted file
  if (is.na(comment$included$attributes$fileFormats)[1]) { 
    message("\nsole attachment could not be downloaded from comment ",
            comment$data$id, ", perhaps because of restrictReasonType: ", 
            comment$included$attributes$restrictReasonType[1], "\n")
    return()
  }  
  
  # TODO: Not sure what happens with multiple attachments, all, restricted but 
  # either this condition or the one in the loop should catch that
  
  filenames <- filenamesFromComment(comment)
  N <- length(filenames)
  
  for (i in 1:N) {
    
    # tests if the file is restricted
    if (is.null(comment$included$attributes$fileFormats[[i]])) { 
      message("\nattachment ", i, " could not be downloaded from comment ",
              comment$data$id, ", perhaps because of restrictReasonType: ", 
              comment$included$attributes$restrictReasonType[i], "\n")
      next
    }
    
    itemFormat <- slug(filenames[i], sep = "\\.")
    
    # alert the user if the file is not marked as an attachment or is not a pdf
    # I've never seen the former come up but I'd sure want a warning if it did!
    if (comment$included$type[i] != "attachments") {
      warning("\nitem ", i, "'s type in comment ", comment$data$id,
              " is not 'attachments' but", comment$included$type, "\n") 
    }
    if (itemFormat != "pdf") {
      message("\nitem #", i, ": ", filenames[i], " is not pdf but ", 
              itemFormat, "\n")
    }
    
    destpath <- paste(pathPrefix, filenames[i], sep = "/")
    
    # download the file to the targeted path
    
    # first we need to make sure we're grabbing the URL with the right extension
    links <- as.list(comment$included$attributes$fileFormats[[i]]$fileUrl)
    pattern <- paste0("*\\.", itemFormat)
    link <- links[grepl(pattern, links)]
    
    resilientDownload(link, destpath)
  }
  # TODO: combine all downloaded files into one big file PDF
  
}

printGoodbye <- function() {
  message("goodbye")
  return()
}

# Returns a list of local attributes, and downloads the comments if "doDownload"
getAttachmentDetails <- function(comment, destdir = "attachments", 
                                 doDownload = TRUE) {
  
  # attachments will be stored in destdir/commentId
  pathPrefix <- paste(destdir, comment$data$id, sep = "/")
  
  # The below may go into the downloadAttachmentsTo function
  if (doDownload) {
    if (!dir.exists(destdir)) { dir.create(destdir) }
    if (!dir.exists(pathPrefix)) { dir.create(pathPrefix) }
  }
  
  localAttributes <- list(fullText = "", coverAndComment = "", 
                          attachmentCount = 0, nonPdfs = 0, restricteds = 0, 
                          authorName = getAuthorName(comment))
  
  textFName <- paste(pathPrefix, "fullText.txt", sep = "/")
  
  # fullText.txt header
  cat("Full text of comment", comment$data$id, 
      "\nAuthor:", localAttributes$authorName, "\n\n", 
      file = textFName, append = FALSE)
  
  # add comment to fullText.txt
  cat(comment$data$attributes$comment, file = textFName, append = TRUE)
  
  # if there aren't attachments, you have the data you need
  if (is.null(comment$included)) {
    text <- readChar(textFName, file.info(textFName)$size)
    localAttributes$fullText <- text
    localAttributes$coverAndComment <- text
    return(localAttributes)
  } 
  # if there are attachments, add to the directory and to the fullText string
  localAttributes$attachmentCount <- length(comment$included$id)
  
  # get the attachments if needed
  if (doDownload) { downloadAttachmentsTo(comment, pathPrefix = pathPrefix) }
  
  # get the text of Comment.pdf if it exists
  
  fileList <- list.files(path = pathPrefix)
  # message(paste(fileList, sep = " "))
  
  attachmentList <- fileList[!grepl("fullText\\.txt", fileList)]
  commentPdf <- attachmentList[grepl("a\\d\\d_Comment\\.pdf", attachmentList)]
  if (length(commentPdf)) {
    commentPdfText <- paste(pdf_text(paste(pathPrefix, commentPdf, sep = "/")), 
                            collapse = "", sep = "/n")
    cat("\n\n---\nAttached Comment:\n---\n\n", commentPdfText, 
        file = textFName, append = TRUE)    
  }
  localAttributes$coverAndComment <- readChar(textFName, 
                                              file.info(textFName)$size)
  # message("added Comment.pdf (if any)\n")
  # add a list of non-parsed attachments
  
  # TODO: update the parser to use readtext, which also handles json/docx/txt
  nonPdfs <- attachmentList[!grepl("*\\.pdf", attachmentList)]
  if (length(nonPdfs)) {
    cat("\n\n---\nCould not parse:", nonPdfs, sep = "\n", 
        file = textFName, append = TRUE)
    message(paste("found", length(nonPdfs), "unparseable files:\n", 
                  paste(nonPdfs, sep = "\n")))
  }
  localAttributes$nonPdfs <- length(nonPdfs)
  
  # add a list of restricted attachments
  restrictedStatus <- comment$included$attributes$restrictReasonType
  names(restrictedStatus) <- comment$included$attributes$title
  restricted <- restrictedStatus[!is.na(restrictedStatus)]
  if (length(restricted)) {
    cat("\n\n---\nRestricted files:\n", restricted, 
        file = textFName, append = TRUE)
    message("\nfound ", length(restricted), " restricted files\n")
    for (i in 1:length(restricted)) {
      cat(names(restricted)[i], ": ", restricted[i], "\n", 
          file = textFName, append = TRUE)
    }
  }
  localAttributes$restricteds <- length(restricted)

  # add the text of other attachments
  allPdfs <- attachmentList[grepl("*\\.pdf", attachmentList)]
  otherPdfs <- allPdfs[!grepl("a\\d\\d_Comment\\.pdf", allPdfs)]
  for (f in otherPdfs) {
    attachmentText <- paste(pdf_text(paste(pathPrefix, f, sep = "/")), 
                            collapse = "", sep = "/n")
    j <- substr(f, 2, 3)
    title <- substr(f, 5, nchar(f) - 4)
    cat("\n\n---\n", "Attachment", j, "title:\n", title, "\n---\n\n", 
        attachmentText, file = textFName, append = TRUE)
  }
  
  localAttributes$fullText <- readChar(textFName, file.info(textFName)$size)
  
  return(localAttributes)
} 

# gets the objectIDs associated with the documents of documentType in a docket
# for instance, documentType = "Proposed Rule" or "Notice" on a given docket
getOIDs <- function(docketId, documentType, ignoreExtensions = TRUE) {
  
  # make documentType URL compatible
  documentType = gsub(" ", "%20", documentType)
  
  url = paste0("https://api.regulations.gov/v4/documents?filter[docketId]=",
               docketId, "&filter[documentType]=", documentType, "&api_key=",
               api_key)
  
  rawDocket <- get_or_wait(url)
  
  docket <- fromJSON(rawToChar(rawDocket$content))
  
  docs <- docket$data$attributes
  
  # Extensions of Comment Period often have 0 comments; you may choose to ignore
  if (ignoreExtensions) {
    docs <- docs %>% filter(is.na(subtype) | 
                              subtype != "Extension of Comment Period")
  }
  
  return(docs$objectId)
  
}

# fetches all comments on the given objectID
getCommentList <- function(oID, pageLen = 250, pagesPerBatch = 20, 
                         dataDest = "data", attachmentsDest = "attachments", 
                        slug = "", downloadDocs = TRUE) {
  # oID: the object on which you want all comments
  # pageLen / pagesPerBatch: leave as default except for testing/debugging
  # dataDest / attachmentsDest: where data files and comment text will be saved
  # startCommentID / commentsAdded: use to restart interrupted runs
  # commentDF / commentList: if adding to previous data, paths to load data from
  # slug: will be a component of data file names
  # downloadDocs: downloads attachments if true, else tries using local files
  
  startTime <- lubridate::now()
  commentCount <- getCommentCount(oID)
  commentsAdded <- 0
  
  # initialize some tracking variables
  # if (lastCommentID != "") {
  #   lastComment <- getCommentById(lastCommentID)
  #   start_last_mod <- fixTimeString(lastComment$data$attributes$modifyDate)
  # } else { start_last_mod <- "" }
  
  start_last_mod <- ""
  
  batchN <- 1  
  stop = FALSE
  
  commentList <- rep(list(), commentCount)
  
  # iterate through batches
  while (!stop) {
    
    # iterate through each page in a batch
    for (pageN in 1:pagesPerBatch) {
      
      # don't bother adding more comments if we have all the comments we need
      # this will usually be the stop condition when collecting all comments
      if(commentsAdded == commentCount) { 
        stop = TRUE
        break 
      }
      
      # pull a page of comments
      message("\npulling page ", pageN, " (", pageLen, " items) of batch ",
              batchN, "\n")
      pageOfComments <- getAPageOfComments(pageN, oID, 
                                           last_mod = start_last_mod, 
                                           pSize = pageLen)
      thisPageLen <- pageOfComments$meta$numberOfElements
      message(" with ", thisPageLen, " elements\n")
      message(" and adding to the ", commentsAdded, " elements already added\n")
      message(" starting with last_mod = ", 
              fixTimeString(pageOfComments$data$attributes$lastModifiedDate[1]))
      
      # for the first page of a batch, start after the comment we left off on
      if ((pageN == 1) & (commentsAdded != 0)) { 
        start <- match(lastCommentID, pageOfComments$data$id) + 1
        message(paste0("starting batch ", batchN," at ", start, "th comment\n"))
      
        # otherwise, start with the first comment on the page
      } else { start <- 1 }
      
      # get every comment in the page (skipping any we already got)
      for (i in start:thisPageLen) {
        
        commentsAdded <- commentsAdded + 1
        
        # indicates mismatch if commentsAdded and last_mod were specified
        if (commentsAdded > commentCount) {
          warning(paste0("it seems there's more comments than commentCount (",
                         commentCount, ")! Terminating...\n"))
          stop = TRUE
          break
        }
        
        url <- paste0(pageOfComments$data$links$self[i])
        comment <- getCommentByUrl(url)
        
        # log that the comment has been pulled
        message(commentsAdded, ":-", slug(comment$data$id), "...", 
                appendLF = FALSE)
        
        # adjust the comments attributes list in the following ways:
        
        # ...remove columns that are all NA on public data...
        attsList <- comment$data$attributes[
          c('commentOn', 'commentOnDocumentId', 'duplicateComments', 
            'comment', 'docketId', 'documentType', 'objectId', 
            'modifyDate', 'pageCount', 'postedDate', 'postmarkDate', 
            'receiveDate', 'subtype', 'title', 'withdrawn')
        ] 
        
        # ...add derived attributes (and download attachments if user asks)...
        localAtts <- getAttachmentDetails(comment, destdir = attachmentsDest, 
                                          doDownload = downloadDocs)
        
        # ... remove NULL items to NA items that don't break later work...
        attsList <- nullToNA(c(list(ID = comment$data$id), localAtts, attsList))
        # browser()
        # ... and save the new attributes list in the comment itself
        comment$DFattributes <- attsList
        if (is.null(comment$included)) { comment$included <- NA }
        
        # Add comment to list & the attributes to data frame for later analysis
        commentList[[commentsAdded]] <- comment
        names(commentList)[commentsAdded] <- comment$data$id
        # if (commentsAdded == 1) {
        #   commentDF <- as.data.frame(attsList)
        # } else { 
        #   commentDF <- rbind.data.frame(commentDF, as.data.frame(attsList))
        # }
        # TODO: allow loaded data frames
      }
      
    }
    # get the last modified date of the last comment loaded (in correct format)
    start_last_mod <- fixTimeString(pageOfComments$data$
                                attributes$lastModifiedDate[pageLen])
    lastCommentID <- comment$data$id
    
    # use while loop because # of total batches isn't necessarily known
    batchN <- batchN + 1
    # only matters if many comments have identical last_mod, but why risk it?
  }
  

  # save(commentList, file = paste0(pathPrefix, slug, "_List.RData"))
  # 
  # wb <- createWorkbook()
  # s <- createSheet(wb, sheetName = "comments")
  # 
  # addDataFrame(commentDF, s, col.names = TRUE, row.names = FALSE, byrow = FALSE)
  # saveWorkbook(wb, paste0(pathPrefix, slug, "_Table.xlsx"))
  
  # log the total time and time per comment
  endTime <- lubridate::now()
  runTime <- endTime - startTime
  timePerComment <- runTime / commentCount
  message(paste("Run time of", runTime, "\nor", timePerComment, "per comment"))
  
  return(commentList)
  
}

# returns a comment's list of local attributes created by getCommentList
getLocalAtts <- function(comment) {
  return(comment$DFattributes)
}

# turns commentList into a data frame using the local attributes
getCommentDF <- function(commentList) {
  return(as.data.frame(do.call(rbind, lapply(commentList, getLocalAtts))))
}

# saves CommentDF as an RData file AND xlsx file
saveCommentDF <- function(commentDF, slug = "", dataDest = "data") {
  if (slug == "") { slug <- commentDF$docketId[[1]] }
  datePrefix <- format(Sys.Date(), format="%m_%d_")
  pathPrefix <- paste0(dataDest, "/", datePrefix)
  
  save(commentDF, file = paste0(pathPrefix, slug, "_DF.RData"))
  
  wb <- createWorkbook()
  s <- createSheet(wb, sheetName = "comments")
  addDataFrame(commentDF, s, col.names = TRUE, row.names = FALSE, byrow = FALSE)
  saveWorkbook(wb, paste0(pathPrefix, slug, "_Table.xlsx"))
}

# saves commentList as an RData file
saveCommentList <- function(commentList, slug = "", dataDest = "data") {
  datePrefix <- format(Sys.Date(), format="%m_%d_")
  pathPrefix <- paste0(dataDest, "/", datePrefix)
  
  # if slug is blank use the docket ID; usually slug should be more descriptive
  if (slug == "") { slug <- commentList[[1]]$data$attributes$docketId }
  
  save(commentList, file = paste0(pathPrefix, slug, "_List.RData"))
}

getRestrictedList <- function(commentList) {
  rList <- NULL
  for (comment in commentList) {
    if (comment$DFattributes$restricteds) {
      id <- comment$data$id
      message(paste("\nRestricted files in", id))
      message(paste(comment$DFattributes$authorName))
      restrictedStatus <- comment$included$attributes$restrictReasonType
      titles <- comment$included$attributes$title
      reasons <- comment$included$attributes$restrictReasonType
      names(reasons) <- titles
      count <- comment$DFattributes$restricteds
      message(paste("[", count, "total]:"))
      print(reasons)
      
      if (is.null(rList)) {
        rList <- list(c(author = comment$DFattributes$authorName, reasons))
      } else {
        rList <- append(rList, list(c(author = comment$DFattributes$authorName, 
                                      reasons)))
      }
      names(rList)[length(rList)] <- id
    }
  }
  return(rList)
}

getNonPdfsList <- function(commentList) {
  npList <- NULL
  for (comment in commentList) {
    if(comment$DFattributes$nonPdfs) {
      id <- comment$data$id
      message(paste("\nNon-PDF files in", id))
      message(paste(comment$DFattributes$authorName))
      message(paste("[", comment$DFattributes$nonPdfs, "total]:"))
      filenames <- filenamesFromComment(comment)
      filetypes <- slug(filenames, sep = "\\.")
      names(filetypes) <- filenames
      print(filetypes)
      
      if (is.null(npList)) {
        npList <- list(c(author = comment$DFattributes$authorName, filetypes))
      } else {
        npList <- append(npList, list(c(author = comment$DFattributes$authorName, 
                                        filetypes)))
      }
      names(npList)[length(npList)] <- id
    }
  }
  return(npList)
}

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
  if (remaining < 5) {
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
  last_mod_UTC <- as.POSIXct(sub("T", " ", yyyymmddThhmmssZ), tz = "UTC")
  last_mod <- sub(" E*T", "", lubridate::with_tz(last_mod_UTC,
                                                 "America/New_York"))
  return(last_mod)
}

# Function for grabbing a page of comments
getAPageOfComments <- function(n, objectID, last_mod = "", pSize = 250) {
  # gets the nth page of comments on objectID modified after last_mod 
  if (pSize < 5) {
    pSize <- 5 
    warning("page size too small; now set to 5")
  }
  if (pPsize > 250) {
    pSize <- 250
    warning("page size too large; now set to 250")
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
  if (startsWith(title, "Anonymous")) {return("Anonymous")}
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
    return (substr(title, 22, 1000))
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

# Downloads attachments from the web, and returns some details about them
downloadAttachmentsTo <- function(comment, pathPrefix = "test") {
  
  # create the directory to which we are downloading the documents
  if (!dir.exists(pathPrefix)) { dir.create(pathPrefix) }
  
  # edge case for if the attachments consist of a single, restricted file
  if (is.na(comment$included$attributes$fileFormats)[1]) { 
    warning("sole attachment could not be downloaded from comment ",
            comment$data$id, ", perhaps because of restrictReasonType: ", 
            comment$included$attributes$restrictReasonType[1])
    return()
  }  
  
  # TODO: Not sure what happens with multiple attachments, all, restricted but 
  # either this condition or the one in the loop should catch that
  
  filenames <- filenamesFromComment(comment)
  N <- length(filenames)
  
  for (i in 1:N) {
    
    # tests if the file is restricted
    if (is.null(comment$included$attributes$fileFormats[[i]])) { 
      warning("attachment ", i, " could not be downloaded from comment ",
              comment$data$id, ", perhaps because of restrictReasonType: ", 
              comment$included$attributes$restrictReasonType[i])
      next
    }
    
    itemFormat <- slug(filenames[i], sep = "\\.")
    
    # Warn the user if the file is not marked as an attachment or is not a pdf
    # I've never seen the former come up but I'd sure want a warning if it did!
    if (comment$included$type[i] != "attachments") {
      warning("item ", i, "'s type in comment ", comment$data$id,
              " is not 'attachments' but", comment$included$type) 
    }
    if (itemFormat != "pdf") {
      warning("item #", i, ": ", filenames[i], " is not pdf but ", itemFormat)
    }
    
    destpath <- paste(pathPrefix, filenames[i], sep = "/")
    
    # download the file to the targeted path
    
    # first we need to make sure we're grabbing the URL with the right extension
    links <- as.list(comment$included$attributes$fileFormats[[i]]$fileUrl)
    pattern <- paste0("*\\.", itemFormat)
    link <- links[grepl(pattern, links)]
    suppressMessages(download.file(link[[1]], destfile = destpath, mode = "wb", 
                                   quiet = TRUE))
    
  }
  # TODO: combine all downloaded files into one big file PDF
  
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
  
  nonPdfs <- attachmentList[!grepl("*\\.pdf", attachmentList)]
  if (length(nonPdfs)) {
    cat("\n\n---\nCould not parse:", nonPdfs, sep = "\n", 
        file = textFName, append = TRUE)
    warning(paste("found", length(nonPdfs), "unparseable files:\n", 
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
    message("WARNING: found ", length(restricted), " restricted files:")
    for (i in 1:length(restricted)) {
      cat(names(restricted)[i], ": ", restricted[i], "\n", 
          file = textFName, append = TRUE)
    }
  }
  localAttributes$restricteds <- length(restricted)
  # message(length(restricted), " restricted files\n")
  
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
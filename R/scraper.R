library(dplyr)
library(tidyRSS)
library(purrr)
library(rvest)

# download RSS
bisurl <- "https://www.bis.org/doclist/cbspeeches.rss?from=&till=&objid=cbspeeches&page=&paging_length=10&sort_list=date_desc&theme=cbspeeches&ml=false&mlurl=&emptylisttext="

# parse RSS


feed <- tidyfeed(bisurl)

# extract the speech ID

# get the url
speech_url <- feed$item_link[1]

get_speech <- function(speech_url) {
  speech_page <- read_html(speech_url)
  # === tags ===
  # included in the feed:
  #title <- "h1"
  #description <- "#extratitle-div p"
  author <- ".authorline .dashed" # get text and URL
  date <- ".date"
  content <- "#cmsContent p"
  type <- ".pdfdocinfo span"
  pdf_link <- ".pdftitle_link"

  # pull function
  pull_text <- function(x) {
    text <- speech_page %>% html_elements(x) %>% html_text() %>% paste(collapse = " ")
    return(text)
  }

  # pull
  author_text <- pull_text(author)
  date_text <- pull_text(date)
  content_text <- pull_text(content)
  type_text <- pull_text(type)
  pdf_text <- pull_text(pdf_link)

  # TODO get the links
  # author
  # pdf

  # save to a data frame
  df <- data.frame(
    author = author_text,
    date = date_text,
    speech = content_text,
    type = type_text,
    pdf = pdf_text
  )
  return(df)
}

latest_speeches <- map_dfr(feed$item_link, get_speech)


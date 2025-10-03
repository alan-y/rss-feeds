# Process the BBC RSSs feed by filtering out some content
# Credit: Dan Q's 'BBC News without the crap'
# https://danq.me/2024/03/09/bbc-news-without-the-crap

library(xml2)
library(stringr)
library(purrr)

# Function to process RSS feeds and save out
process_feed <- function(url, file_name) {
  stopifnot(tools::file_ext(file_name) == "xml")

  feed <- xml2::read_xml(url)
  items <- xml2::xml_find_all(feed, "//item")

  # Strip the anchors off the <guid>s: BBC News "republishes" stories by using
  # guids with #0, #1, #2 etc, which results in duplicates in feed readers
  for (i in seq_len(length(items))) {
    guid_node <- xml2::xml_find_first(items[i], "./guid")
    current_guid <- xml2::xml_text(guid_node)
    cleaned_guid <- stringr::str_remove_all(current_guid, "#.*$")
    xml2::xml_text(guid_node) <- cleaned_guid
  }

  # Regular expression describing the GUIDs to reject from the resulting RSS feed
  # We want to drop iPlayer/Sounds/Ideas links
  reject_patterns <- "iplayer|sounds|ideas|news/videos|programmes"

  guid_text <- purrr::map_chr(
    items,
    ~ xml2::xml_text(xml2::xml_find_first(.x, "./guid"))
  )

  to_remove <- unique(c(
    which(duplicated(guid_text)),
    stringr::str_which(guid_text, reject_patterns)
  ))

  xml2::xml_remove(items[to_remove])

  xml2::write_xml(feed, file = file_name)
}

url_list <- c(
  "http://feeds.bbci.co.uk/sport/0/football/rss.xml?edition=uk",
  "http://feeds.bbci.co.uk/sport/0/boxing/rss.xml",
  "http://feeds.bbci.co.uk/sport/0/tennis/rss.xml?edition=uk",
  "https://feeds.bbci.co.uk/news/technology/rss.xml?edition=uk",
  "http://feeds.bbci.co.uk/news/health/rss.xml",
  "http://feeds.bbci.co.uk/news/world/asia/china/rss.xml"
)

file_names <- c(
  "bbc-football-ay.xml",
  "bbc-boxing-ay.xml",
  "bbc-tennis-ay.xml",
  "bbc-technology-ay.xml",
  "bbc-health-ay.xml",
  "bbc-china-ay.xml"
)

walk2(url_list, file_names, ~ process_feed(.x, .y))

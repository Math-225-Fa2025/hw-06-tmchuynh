# load packages ----------------------------------------------------------------

library(tidyverse)
library(rvest)
library(here)
library(rlang)

# function: scrape_pac ---------------------------------------------------------

scrape_pac <- function(url) {
  # read the page
  page <- read_html(url)

  #  exract the table
  pac <- page %>%
    # select node .DataTable (identified using the SelectorGadget)
    html_node(".DataTable-Partial") %>%
    # parse table at node td into a data frame
    #   table has a head and empty cells should be filled with NAs
    html_table("td", header = TRUE, fill = TRUE) %>%
    # convert to a tibble
    tibble::as_tibble()

  # rename variables
  pac <- pac %>%
    # rename columns
    dplyr::rename(
      name = 1,
      country_parent = 2,
      total = 3,
      dems = 4,
      repubs = 5
    )

  # fix name
  pac <- pac %>%
    # remove extraneous whitespaces from the name column
    dplyr::mutate(name = stringr::str_squish(.data$name))

  # add year
  pac <- pac %>%
    # extract last 4 characters of the URL and save as year
    dplyr::mutate(year = stringr::str_sub(url, -4, -1))

  # return data frame
  pac
}

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

# test function ----------------------------------------------------------------
url_2022 <- "https://www.opensecrets.org/political-action-committees-pacs/foreign-connected-pacs/2022"
pac_2022 <- scrape_pac(url_2022)

url_2020 <- "https://www.opensecrets.org/political-action-committees-pacs/foreign-connected-pacs/2020"
pac_2020 <- scrape_pac(url_2020)

url_2000 <- "https://www.opensecrets.org/political-action-committees-pacs/foreign-connected-pacs/2000"
pac_2000 <- scrape_pac(url_2000)

# list of urls -----------------------------------------------------------------

# first part of url
root <- "https://www.opensecrets.org/political-action-committees-pacs/foreign-connected-pacs/"

# second part of url (election years as a sequence)
year <- seq(from = 2000, to = 2022, by = 2)

# construct urls by pasting first and second parts together
urls <- paste0(root, year)

# map the scrape_pac function over list of urls --------------------------------

pac_all <- purrr::map_dfr(urls, scrape_pac)

# write data -------------------------------------------------------------------

readr::write_csv(pac_all, file = here::here("data/pac-all.csv"))

# THIS VERSION OF THE SCRIPT IS CAPABLE OF EXTRACTING
# UNLIMITED AMOUNT OF FB PAGES, WHOSE IDs WE HAVE TO
# SPECIFY IN A "PARTIES" LIST

# PART 1: LOAD THE REQUIRED LIBRARIES FOR THIS SCRIPT

# Specify the package names we will be using.
packages <- c("dplyr", "tidyr", "remotes")

# Install packages not yet installed.
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading.
invisible(lapply(packages, library, character.only = TRUE))

# We have to install the Radlibrary package, which is available only on GitHub
# To install Radlibrary, we need to use install_github function from a lightweight
# remotes package. We also specify argument "upgrade" to never, so we do not get
# a dialog window asking us whether to update when the script runs automatically.

remotes::install_github("facebookresearch/Radlibrary", upgrade = "never", dependencies = TRUE)

library(Radlibrary)

# Disable scientific notation of numbers
options(scipen = 999)

# PART 2: DEFINE THE FUNCTION THAT WILL EXTRACT, MERGE AND SAVE FB ADS DATASETS

############################### FUNCTION BEGGINING #############################

get_all_tables_merge <- function(token, parties_ids, min_date, max_date, directory) {

  # A. SPECIFICATION PART OF THE FUNCTION
  # We need to specify the arguments we want to supply the Radlibrary functions
  fields_vector <- c("ad_data", "region_data", "demographic_data")
  table_type_vector <- c("ad", "region", "demographic")

  # We initialize empty datasets to which we add rows with each loop iteration
  dataset_ad <- tibble()
  dataset_demographic <- tibble()
  dataset_region <- tibble()

  # We have to create a desired directory, if one does not yet exist
  if (!dir.exists(directory)) {
    dir.create(directory)
  } else {
    print("Output directory already exists")
  }

  # B. EXTRACTION PART OF THE FUNCTION

  # We will be using 2 nested for loops for extraction
  # The outer for loop cycles over the list of parties
  # The inner for loop gets us the 3 distinct types of tables from the FB Ads.

  for (p in seq_len(length(parties_ids))) {
    print(paste("outer_loop", p))

    for (i in seq_len(length(fields_vector))) {
      print(paste("inner_loop", i))
      # Building the query
      query <- adlib_build_query(
        ad_reached_countries = "CZ",
        ad_active_status = "ALL",
        ad_delivery_date_max = max_date,
        ad_delivery_date_min = min_date,
        ad_type = "POLITICAL_AND_ISSUE_ADS",
        publisher_platform = c("FACEBOOK", "INSTAGRAM"),
        limit = 1000,
        search_page_ids = parties_ids[[p]],
        fields = fields_vector[i]
      ) 
      query$search_type <- NULL
      # The call is limited to last 1000 results, pagination overcomes it
      response <- adlib_get_paginated(query, token = token, max_gets = 100)

      assign(
        paste0(
          "dataset_",
          table_type_vector[i], "_", print(p)
        ),
        as_tibble(response,
          type = table_type_vector[i],
          censor_access_token = TRUE
        )
      )
    }
    # With each iteration of the outer for loop, we append the dataset
    new_rows <- get(paste0("dataset_ad_", print(p)))
    dataset_ad <- bind_rows(dataset_ad, new_rows)

    new_rows <- get(paste0("dataset_demographic_", print(p)))
    dataset_demographic <- bind_rows(dataset_demographic, new_rows)

    new_rows <- get(paste0("dataset_region_", print(p)))
    dataset_region <- bind_rows(dataset_region, new_rows)
  }

  # C. MERGE PART OF THE FUNCTION
  # After extraction of the three tables through the for loop, we transform
  # and merge into one. The demographic & region datasets are in the "long"
  # format and we need a transformation to a "wide" format of the ad dataset

  dataset_demographic_wide <- dataset_demographic %>%
    unnest(col = "demographic_distribution") %>% 
    mutate(across(percentage, round, 3)) %>%
    pivot_wider(
      id_cols = id,
      names_from = c("gender", "age"),
      values_from = percentage,
      names_sort = TRUE
    )

  dataset_region_wide <- dataset_region %>%
    unnest(col = "delivery_by_region") %>% 
    mutate(across(percentage, round, 3)) %>%
    pivot_wider(
      id_cols = id,
      names_from = region,
      values_from = percentage,
      names_sort = TRUE
    )

  # Performing the join on common columns across the 3 datasets
  merged_dataset <- dataset_ad %>%
    left_join(dataset_demographic_wide, by = "id") %>%
    left_join(dataset_region_wide, by = "id") %>%
    mutate(across(c(
      "bylines",
      "currency",
      "page_name",
      "page_id"
    ), factor)) %>%
    filter(ad_creation_time >= as.Date(min_date)) %>%
    arrange(desc(ad_creation_time))

  # We save each of the three tables in a memory to a dedicated csv and rds file
  # We save the merged dataset as well, both in the csv and rds formats
  # Rds enables faster reading when using the dataset in R for further analyses
  # We turn off compression for rds files (optional). Their size is larger, but
  # the advantage are a magnitude faster read/write times using R.

  fwrite(x = dataset_ad, file = paste0(directory, "/ad_data.csv"))
  fwrite(x = dataset_demographic_wide, file = paste0(directory, "/demographic_data.csv"))
  fwrite(x = dataset_region_wide, file = paste0(directory, "/region_data.csv"))
  fwrite(x = merged_dataset, file = paste0(directory, "/merged_data.csv"))
  saveRDS(object = merged_dataset, file = paste0(directory, "/merged_data.rds"))
  
  # Saving a "lean" version of the dataset, which contains only id and text
  # We will use this in a different repository (social media scrape through Hlidac Statu)
  merged_dataset_lean <- merged_dataset %>%
   select(page_name, page_id, ad_creative_bodies)

 saveRDS(object = merged_dataset_lean, file = paste0(directory, "/merged_data_lean.rds"))
}

############################### FUNCTION END ###################################


# PART 3: SPECIFY THE ARGUMENTS NEEDED TO RUN THE FUNCTION

# Current token expires within 60 days of issue. It need to be prolonged before then.
token <- Sys.getenv("FB_TOKEN")

min_date <- "2022-09-01"

# Get today's date in format FB wants for automation
max_date <- format((Sys.Date()), "%Y-%m-%d")

# In this script, we can specify potentially unlimited number of Page ids,
# however, each item of the list is limited by 10 ids maximum in a numeric form.
# The numeric ids are not easy to find for many pages and FB does
# not provide an easy way. However, we can find numeric ids from the already
# queried table if we search using the search_terms argument of the
# adlib_build_query function from Radlibrary.
# To make this script more legible, ids are specified in a separate script file
# named "monitored_pages_list.R" which also saves rds file that is loaded below.

parties_ids <- readRDS("data/saved_pages_list.rds")[1:2]

# Specify the desired output folder
directory <- "data"

# PART 4: RUNNING THE FUNCTION WITH APPROPRIATE ARGUMENTS

# The end result should be 4 tables saved in the data folder
get_all_tables_merge(
  token = token,
  parties_ids = parties_ids,
  min_date = min_date,
  max_date = max_date,
  directory = directory
)

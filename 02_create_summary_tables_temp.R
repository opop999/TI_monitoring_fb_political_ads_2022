# NOTE: THIS IS A TEMPORARY ALTERED VERSION OF THE ORIGINAL SCRIPT TO OVERCOME THE BUG IN THE RADLIBRARY DEPENDENCY,
# WHICH DOES RETURN ONLY THE "AD" TABLES, BUT NOT THE "DEMOGRAPHIC/REGION" TABLES. ONCE THE BUG IS RESOLVED
# WE WILL RETURN TO THE FULL VERSION OF THE SCRIPT

## 1. Load the required R libraries

# Package names
packages <- c("dplyr", "data.table", "tidyr")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

# Disable scientific notation of numbers
options(scipen = 999)

# Specify directory
directory <- "data/temp"

full_ads_table <- readRDS(paste0(directory, "/merged_data.rds"))

if (!dir.exists(paste0(directory, "/summary_tables"))) {
  dir.create(paste0(directory, "/summary_tables"))
} else {
  print("Output directory already exists")
}

## 2. Create summary tables and write them to a output file

# Creating a summary table focused on the detailed aspects of the advertising
# Percentage figures rounded to 3 decimal places
ad_summary <- full_ads_table %>%
  select(1:19) %>%
  group_by(page_name, page_id) %>%
  summarise(
    total_ads = n(),
    unique_ads = n_distinct(ad_creative_body),
    percent_unique = round(unique_ads / total_ads, digits = 3),
    # lower_spend = sum(spend_lower, na.rm = TRUE),
    # upper_spend = sum(spend_upper, na.rm = TRUE),
    avg_spend = round(((sum(spend_lower, na.rm = TRUE) + sum(spend_upper, na.rm = TRUE)) / 2), digits = 0),
    per_ad_avg_spend = round(avg_spend / total_ads, digits = 0),
    # total_lower_impressions = sum(impressions_lower, na.rm = TRUE),
    # total_upper_impressions = sum(impressions_upper, na.rm = TRUE),
    total_avg_impressions = round(((sum(impressions_lower, na.rm = TRUE) + sum(impressions_upper, na.rm = TRUE)) / 2), digits = 0),
    per_ad_avg_impression = round(total_avg_impressions / total_ads, digits = 0),
    total_min_reach = sum(potential_reach_lower, na.rm = TRUE),
    per_ad_min_reach = round(total_min_reach / total_ads, digits = 0),
    avg_ad_runtime = round(mean(ad_delivery_stop_time - ad_delivery_start_time, na.rm = TRUE), digits = 1)
  ) %>%
  arrange(desc(total_ads)) %>%
  ungroup()

# Writing the table to a csv file
fwrite(ad_summary, paste0(directory, "/summary_tables/ad_summary.csv"))

# # Creating a summary table focused on the demographic aspects
# # Percentage figures rounded to 3 decimal places
# demographic_summary <- full_ads_table %>%
#   transmute(page_name,
#     page_id,
#     female_13_17 = `female_13-17`,
#     female_18_24 = `female_18-24`,
#     female_25_34 = `female_25-34`,
#     female_35_44 = `female_35-44`,
#     female_45_54 = `female_45-54`,
#     female_55_64 = `female_55-64`,
#     female_65_plus = `female_65+`,
#     male_13_17 = `male_13-17`,
#     male_18_24 = `male_18-24`,
#     male_25_34 = `male_25-34`,
#     male_35_44 = `male_35-44`,
#     male_45_54 = `male_45-54`,
#     male_55_64 = `male_55-64`,
#     male_65_plus = `male_65+`
#   ) %>%
#   replace(is.na(.), 0) %>%
#   group_by(page_name, page_id) %>%
#   summarise(
#     total_ads = n(),
#     avg_female_13_17 = mean(female_13_17),
#     avg_female_18_24 = mean(female_18_24),
#     avg_female_25_34 = mean(female_25_34),
#     avg_female_35_44 = mean(female_35_44),
#     avg_female_45_54 = mean(female_45_54),
#     avg_female_55_64 = mean(female_55_64),
#     avg_female_65_plus = mean(female_65_plus),
#     avg_male_13_17 = mean(male_13_17),
#     avg_male_18_24 = mean(male_18_24),
#     avg_male_25_34 = mean(male_25_34),
#     avg_male_35_44 = mean(male_35_44),
#     avg_male_45_54 = mean(male_45_54),
#     avg_male_55_64 = mean(male_55_64),
#     avg_male_65_plus = mean(male_65_plus),
#     avg_female = (avg_female_13_17 + avg_female_18_24 + avg_female_25_34 + avg_female_35_44 + avg_female_45_54 + avg_female_55_64 + avg_female_65_plus),
#     avg_male = (avg_male_13_17 + avg_male_18_24 + avg_male_25_34 + avg_male_35_44 + avg_male_45_54 + avg_male_55_64 + avg_male_65_plus),
#     avg_13_17 = (avg_female_13_17 + avg_male_13_17),
#     avg_18_24 = (avg_female_18_24 + avg_male_18_24),
#     avg_25_34 = (avg_female_25_34 + avg_male_25_34),
#     avg_35_44 = (avg_female_35_44 + avg_male_35_44),
#     avg_45_54 = (avg_female_45_54 + avg_male_45_54),
#     avg_55_64 = (avg_female_55_64 + avg_male_55_64),
#     avg_65_plus = (avg_female_65_plus + avg_male_65_plus)
#   ) %>%
#   mutate(across(3:25, round, digits = 3)) %>%
#   arrange(desc(total_ads)) %>%
#   ungroup()
#
# # Writing the table to a csv file
# fwrite(demographic_summary, paste0(directory, "/summary_tables/demographic_summary.csv"))


# Creating a summary table focused on the regional aspects
# We rename the Czech regions using Czech Statistical Office abbreviations
# Percentage figures rounded to 3 decimal places
# region_summary <- full_ads_table %>%
#   transmute(page_name,
#     page_id,
#     pha = `Prague`,
#     stc = `Central Bohemian Region`,
#     jhc = `South Bohemian Region`,
#     plk = `Plzeň Region`,
#     kvk = `Karlovy Vary Region`,
#     ulk = `Ústí nad Labem Region`,
#     lbk = `Liberec Region`,
#     hkk = `Hradec Králové Region`,
#     pak = `Pardubice Region`,
#     vys = `Vysočina Region`,
#     jhm = `South Moravian Region`,
#     olk = `Olomouc Region`,
#     msk = `Moravian-Silesian Region`,
#     zlk = `Zlín Region`
#   ) %>%
#   replace(is.na(.), 0) %>%
#   group_by(page_name, page_id) %>%
#   summarise(
#     total_ads = n(),
#     avg_pha = mean(pha),
#     avg_stc = mean(stc),
#     avg_jhc = mean(jhc),
#     avg_plk = mean(plk),
#     avg_kvk = mean(kvk),
#     avg_ulk = mean(ulk),
#     avg_lbk = mean(lbk),
#     avg_hkk = mean(hkk),
#     avg_pak = mean(pak),
#     avg_vys = mean(vys),
#     avg_jhm = mean(jhm),
#     avg_olk = mean(olk),
#     avg_msk = mean(msk),
#     avg_zlk = mean(zlk)
#   ) %>%
#   mutate(across(3:16, round, digits = 3)) %>%
#   arrange(desc(total_ads)) %>%
#   ungroup()

# Writing the table to a csv
# fwrite(region_summary, paste0(directory, "/summary_tables/region_summary.csv"))

# Creating a summary table with cumulative spending per page throughout time
time_summary <- full_ads_table %>%
  arrange(ad_creation_time) %>%
  transmute(ad_creation_time,
            page_name,
            page_id,
            avg_spend = (spend_lower + spend_upper) / 2
            ) %>%
  group_by(page_name) %>%
  mutate(cumulative_spend = cumsum(avg_spend)) %>%
  ungroup()

fwrite(time_summary, paste0(directory, "/summary_tables/time_summary.csv"))
saveRDS(object = time_summary, file = paste0(directory, "/summary_tables/time_summary.rds"), compress = FALSE)


# Merge ad, demogtaphic and region table to one
merged_summary <- ad_summary
  # %>%
  # inner_join(demographic_summary, by = c("page_name", "page_id", "total_ads")) %>%
  # inner_join(region_summary, by = c("page_name", "page_id", "total_ads"))

fwrite(merged_summary, paste0(directory, "/summary_tables/merged_summary.csv"))
saveRDS(object = merged_summary, file = paste0(directory, "/summary_tables/merged_summary.rds"), compress = FALSE)




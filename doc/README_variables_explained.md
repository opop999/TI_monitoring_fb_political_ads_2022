## This document intends to explain the meaning and logic behind some of the variables included in the summary tables.

### The summary tables input the information of the raw ads contained in the data folder (one row equals one advertisement) and aggregates them on the level of each advertiser (one row equals one actor).

### Data transformation notes:
 -Only entites with at least one political ad since 1 January 2021 are included in the summary tables.
 
 -Only Czech regions are included in the region summary & merged tables. 
 
 -In the demographic & merged tables, we did not include the columns of *unknown gender*, because the vast majority of the values were equal to 0 or less than 0.1 %. 
 
 -Computed variables, such as *total_avg_impressions* are rounded to the whole number.
 
 -Percentage variables, such as *percent_unique* are rounded to 3 decimal places (1.000 = 100%).
 
 -Raw dataset refers to the one-row-per-ad datasets contained directly in the *data* folder

### Description of selected variables:
-*unique_ads* / *percent_unique_ads*: non-uniqueness means that the text (*ad_creative_body* from the raw dataset) of a particular ad (identified by the *adlib_id*) is exactly the same as some other ad with a different *adlib_id*. This means that a difference of even one character means that the ad would be identified as unique.

-*avg_spend*: FB does not seem to provide exact spend amounts for a given ad through its API. See the raw dataset for columns *spend_lower* / *spend_upper* as a boundary (such as 100 000 - 500 000 CZK). Therefore, *avg_spend* sums the total *spend_lower* and *spend_upper* for a given FB advertiser and averages them. While this is not an ideal situation, this should provide some approximation for the spending intensity of selected political subjects (especially when making relative comparisons). However, please be careful when citing this figure as a piece of precise information about a specific political actor's sum of spend money.

-*total_avg_impressions*: This is calculated using the same logic as *avg_spend*, as Facebook only provides boundary values for impressions.

-*total_min_audience*: Total audience (previously called *reach* by FB) per political actor is calculated as a sum only from the *potential_audience_lower* variable of the raw dataset. The reason for this is that FB again provides only boundary values for the audience of the political ads *, however,* in this case, about 50% of the *potential_audience_higher* values are empty. This means that this calculation will likely underestimate the audience of the political advertising (and the same applies to the *per_ad_min_audience* variable).

-*avg_ad_runtime*: average difference in days for a given political actor between the *ad_delivery_start_time* and the *ad_delivery_end_time* from the raw dataset.

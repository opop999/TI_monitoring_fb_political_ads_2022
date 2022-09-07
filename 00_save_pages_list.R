# To improve readability of the main script and its length, this script is made
# to be modified. We can add and remove the monitored FB pages and than we save
# the list to a rds file, which is then read to the main extraction script.

# The only limitation is that the maximum number of FB ids per item of the list
# is limited to 10. This limitation comes directly from the Facebook API.

# These ids were obtained by using the following tool: https://lookup-id.com/
# We needed to input the full url of each profile to get the numeric version
# See the readme.md for the currently monitored pages

parties_list <- list(
  c(
    "102389958091735",
    "937443906286455",
    "119624098506",
    "277957209202178",
    "258989695009",
    "106891623334554",
    "110687557739540",
    "115802229102602",
    "119687288907752",
    "424122987643945"
  ),
  c(
    "179497582061065",
    "39371299263",
    "132141523484024",
    "211401918930049",
    "219333261570307",
    "317802208282505",
    "326507470746765",
    "995938427265360",
    "1401321553476900",
    "100913275009934"
  ),
  c(
    "487445514669670",
    "251656685576",
    "106008800769282",
    "156945169098",
    "739115596482745",
    "112718776235",
    "992555574111774",
    "1433946376688930",
    "103430204491217"
  ),
  c(
    "397919187215",
    "278212515809",
    "384187235387895",
    "275042429350706",
    "214827221987263",
    "102844685102730",
    "108726857444837",
    "728495140691300",
    "111041662264882",
    "470593056405865"
  ),
  c(
    "298789466930469",
    "1477535869227480",
    "1736314393286600",
    "109323929038",
    "197010357446014",
    "109787574497667",
    "356451014434612",
    "100201752249169",
    "107747200956355"
  ),
  c(
    "356448681873897",
    "370583064327",
    "90002267161",
    "134443820058669",
    "30575632699",
    "110278350358403",
    "104517784280261",
    "2108659169383946",
    "471227586234132"
  ),
  c(
    "114474108677288",
    "108697510778834",
    "108697510778834",
    "466459903794526",
    "1434319750128022",
    "1795946254027412",
    "1414644678821208",
    "146713862010620",
    "104264008348176",
    "351488672592066"
  ),
  c(
    "1917661888252695",
    "495496033796328",
    "1681478228766601",
    "553773474671244",
    "301147597357008",
    "101894271393926",
    "176459739425212",
    "129667385500897",
    "1104866262972886",
    "100102138818077"
  )
)

# NOTE, the id "178362315106" does not work, it overwhelms the API when returning
# the region data. This is because the account is of the EU parliament and the region
# table includes all of the regions of the EU, instead of just Czech regions.
# Workaround - instead include Czech-specific accounts of EU Commission (397919187215)
# and EU Parliament (278212515809).

# We have to create a desired directory, if one does not yet exist
if (!dir.exists("data")) {
  dir.create("data")
} else {
  print("Output directory already exists")
}

saveRDS(parties_list, "data/saved_pages_list.rds", compress = FALSE)

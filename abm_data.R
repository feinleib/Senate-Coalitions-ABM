# R code for Senate Coalitions agent-based model
# Max Feinleib
# May 2024

############
# packages #
############
# install latest version of {filibustr}
devtools::install_github("feinleib/filibustr", ref = "local-arg-dev")

library(dplyr)
library(readr)

library(filibustr)

###############
# data import #
###############

# Senate data from HVW et al.
s_abm_data <- get_hvw_data(chamber = "s")

s_data_114 <- s_abm_data |>
  # 114th is most recent Congress in HVW data
  filter(congress == 114) |>
  # relevant columns
  select(
    ## member info
    last, first, state, icpsr,
    dwnom1, meddist, majparty_dist, votepct, up_for_reelection,
    party, dem, majority,
    seniority, maj_leader, min_leader, chair, subchr, power,
    ## bipartisanship/effectiveness measures
    les, benchmark,
    # Mean number of cosponsors on bills SPONSORED by member
    # (among bills with at least one cosponsor)
    mean_cospon_spon_SN, mean_cospon_spon_SN_nc,
    # PBCA (as %)
    # TODO: also want SS PBCA/PBCO?
    mean_pct_cospon_opp_spon_SN, mean_pct_cospon_opp_spon_SN_nc,
    # PBCO (as %)
    perc_co_bipart, perc_co_bipart_nc,
    ## chamber info
    congress, year, majsize, majmargin, demmd, repmd
  ) |>
  arrange(dwnom1)


########################
# writing data to CSVs #
########################
write_csv(s_data_114, "data/senators_data_114.csv", col_names = FALSE)

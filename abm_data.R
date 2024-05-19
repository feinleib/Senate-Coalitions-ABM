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
# import data #
###############

# Senate data from HVW et al.
s_bipart_data <- get_hvw_data(chamber = "s")
# Senate LES data
s_les <- get_les("s")

###############
# filter data #
###############
s_dwnom_data <- s_les |>
  select(icpsr, congress, dwnom1_career = dwnom1, dwnom2)

s_data_114 <- s_bipart_data |>
  # 114th is most recent Congress in HVW data
  filter(congress == 114) |>
  inner_join(s_dwnom_data, by = c("icpsr", "congress")) |>
  # relevant columns
  select(
    ## member info
    last, first, state, icpsr,
    dwnom1, dwnom2, meddist, majparty_dist, votepct, up_for_reelection,
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

# zero-indexed column numbers for NetLogo code
matrix(colnames(s_data_114),
       ncol = 1,
       dimnames = list(1:length(s_data_114) - 1, "column"))

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
# Voteview data
s_members_114 <- get_voteview_members(chamber = "s", congress = 114) |>
  filter(chamber == "Senate")

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

# estimate PBCA for independents using their PBCO
pbca_lm <- lm(mean_pct_cospon_opp_spon_SN ~ perc_co_bipart, data = s_data_114)
pbca_nc_lm <- lm(mean_pct_cospon_opp_spon_SN_nc ~ perc_co_bipart_nc, data = s_data_114)

# impute PBCA, which is NA for independents
s_data_114 <- s_data_114 |>
  mutate(mean_pct_cospon_opp_spon_SN = ifelse(is.na(mean_pct_cospon_opp_spon_SN),
                                              predict(pbca_lm, s_data_114),
                                              mean_pct_cospon_opp_spon_SN),
         mean_pct_cospon_opp_spon_SN_nc = ifelse(is.na(mean_pct_cospon_opp_spon_SN_nc),
                                                 predict(pbca_nc_lm, s_data_114),
                                                 mean_pct_cospon_opp_spon_SN_nc))


# compare Voteview and LES/HVW dwnom scores
s_joined <- s_data_114 |>
  left_join(s_members_114, by = "icpsr")

plot(s_joined$dwnom1, s_joined$nominate_dim1)
plot(s_joined$dwnom2, s_joined$nominate_dim2)


# majority leader nays
s_114_votes <- get_voteview_member_votes(chamber = "s", congress = 114)

maj_leaders <- s_data_114 |>
  filter(maj_leader == 1) |>
  select(last, first, icpsr)

s_114_votes |>
  left_join(get_voteview_cast_codes(), by = "cast_code") |>
  inner_join(maj_leaders, by = "icpsr") |>
  group_by(rollnumber) |>
  summarize(n_yeas = sum(vote_cast == "Yea")) |>
  pull(n_yeas) |>
  table()



########################
# writing data to CSVs #
########################
write_csv(s_data_114, "data/senators_data_114.csv", col_names = FALSE)

# zero-indexed column numbers for NetLogo code
matrix(colnames(s_data_114),
       ncol = 1,
       dimnames = list(1:length(s_data_114) - 1, "column"))

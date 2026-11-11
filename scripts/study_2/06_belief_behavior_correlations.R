# =============================================================================
# 06 · Pretreatment belief and time-1 behavior
# =============================================================================
# Pearson correlation between each pretreatment moral belief (0-100,
# agreement that the act is morally wrong) and the matching time-1 behavior.
# The control arm is the estimate: its behavior was not targeted by persuasion.
#
# Expected signs if the belief tracks the behavior:
#   meat     belief vs plant-based voucher      positive
#   charity  belief vs donation (cents)         positive
#   lying    belief vs reporting heads          negative
#   polviol  belief vs willingness to join      negative
#
# Output:
#   output/study_2/tables/belief-behavior-correlations.csv

library(dplyr)
library(here)

source(here("scripts", "functions", "helpers.R"))

d1 <- readRDS(here("data", "study_2", "study2_d1.rds")) |>
  mutate(
    voucher_is_veg = as.integer(
      coalesce(voucher_DV_beeffirst, voucher_DV_vegfirst) == 1
    ),
    coin_heads = as.integer(lying_DV == 1)
  )

defs <- tibble::tribble(
  ~domain,   ~belief,               ~behavior,
  "meat",    "belief_meat_pre",     "voucher_is_veg",
  "charity", "belief_charity_pre",  "donation_cents",
  "lying",   "belief_lying_pre",    "coin_heads",
  "polviol", "belief_polviol_pre",  "pv_participation"
)

cor_row <- function(dat, belief, behavior) {
  ok <- complete.cases(dat[[belief]], dat[[behavior]])
  ct <- cor.test(dat[[belief]][ok], dat[[behavior]][ok])
  tibble(
    n = sum(ok),
    r = unname(ct$estimate),
    ci_lower = ct$conf.int[1],
    ci_upper = ct$conf.int[2],
    p = ct$p.value
  )
}

out <- purrr::pmap_dfr(defs, function(domain, belief, behavior) {
  bind_cols(
    tibble(sample = "control", domain = domain, behavior = behavior),
    cor_row(filter(d1, condition == "control"), belief, behavior)
  )
})

print(as.data.frame(out), digits = 3)

out_dir <- here("output", "study_2", "tables")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
out_csv <- file.path(out_dir, "belief-behavior-correlations.csv")
write.csv(out, out_csv, row.names = FALSE)

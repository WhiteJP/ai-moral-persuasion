# =============================================================================
# Study 2 outcome dictionary (shared column names and confirmatory arms)
# =============================================================================
# One source of truth for pretreatment items, t1/t2 outcomes, short labels,
# outcome types, Cohen's d role, and the preregistered own-arm. Analysis
# scripts subset or rename these rows; they do not retype the column names.

study2_primary_treat <- c(
  meat    = "meat",
  charity = "charity",
  lying   = "lying",
  polviol = "polviol"
)

study2_treat_levels <- c("meat", "charity", "polviol", "lying")

study2_belief_outcomes <- tibble::tribble(
  ~key,      ~pre,                  ~change_t1,               ~change_t2,                 ~post_t1,               ~level_t2,          ~short,               ~statement,                                                          ~group,  ~flip,
  "meat",    "belief_meat_pre",     "belief_meat_change",     "belief_meat_change_t2",     "belief_meat_post",     "belief_meat_t2",   "Meat",               "Eating meat is morally wrong",                                      "pro",    1,
  "charity", "belief_charity_pre",  "belief_charity_change",  "belief_charity_change_t2",  "belief_charity_post",  "belief_charity_t2","Charity",            "Not giving to charity is morally wrong",                            "pro",    1,
  "lying",   "belief_lying_pre",    "belief_lying_change",    NA_character_,               "belief_lying_post",    NA_character_,      "Lying",              "Deceiving others for your own financial gain is morally wrong",     "anti",  -1,
  "polviol", "belief_polviol_pre",  "belief_polviol_change",  NA_character_,               "belief_polviol_post",  NA_character_,      "Political Violence", "In the US today, using violence to advance political goals is morally wrong", "anti", -1
)

# Behaviors have no pretreatment measure of the behavior itself. d_role is
# "control": Cohen's d divides the model coefficient by the control arm's SD
# of the time-1 outcome, including binary 0/1 outcomes. That time-1 SD is
# reused at follow-up.
study2_behavior_outcomes <- tibble::tribble(
  ~key,      ~outcome_t1,         ~outcome_t2,           ~pre,                  ~short,                ~type,     ~d_role,    ~x_lab,
  "meat",    "voucher_is_veg",    "voucher_is_veg_t2",   "belief_meat_pre",     "Dietary choice",      "binary",  "control", "% choosing veg voucher",
  "charity", "donation_cents",    "donation_cents_t2",   "belief_charity_pre",  "Charitable donation", "cents",   "control", "Donation to AMF (cents)",
  "lying",   "coin_heads",        NA_character_,         "belief_lying_pre",    "Coin flip (heads)",   "binary",  "control", "% reporting heads",
  "polviol", "pv_participation",  NA_character_,         "belief_polviol_pre",  "PV participation",    "points",  "control", "Would join crowd (0–100)"
)

#' Immediate belief-change defs: key, pre, change, short, statement.
study2_belief_defs_t1 <- function(keys = study2_belief_outcomes$key) {
  study2_belief_outcomes |>
    dplyr::filter(key %in% keys) |>
    dplyr::transmute(
      key, pre, change = change_t1, short, statement
    )
}

#' Follow-up belief-change defs (meat/charity): key, outcome, pre, short, type.
study2_belief_defs_t2 <- function(keys = c("meat", "charity")) {
  study2_belief_outcomes |>
    dplyr::filter(key %in% keys, !is.na(change_t2)) |>
    dplyr::transmute(
      key,
      outcome = change_t2,
      pre,
      short,
      type = "change",
      label = paste0(
        "Belief change (t2): ", short, " — \"", statement, "\""
      )
    )
}

#' Immediate behaviour defs: key, outcome, pre, short, type, optional x_lab.
study2_behavior_defs_t1 <- function(keys = study2_behavior_outcomes$key,
                                    with_x_lab = TRUE) {
  out <- study2_behavior_outcomes |>
    dplyr::filter(key %in% keys) |>
    dplyr::transmute(
      key, outcome = outcome_t1, pre, short, type, x_lab
    )
  if (!isTRUE(with_x_lab)) {
    out <- dplyr::select(out, -x_lab)
  }
  out
}

#' Follow-up behaviour defs (meat/charity): key, outcome, pre, short, type.
study2_behavior_defs_t2 <- function(keys = c("meat", "charity")) {
  study2_behavior_outcomes |>
    dplyr::filter(key %in% keys, !is.na(outcome_t2)) |>
    dplyr::transmute(
      key,
      outcome = outcome_t2,
      pre,
      short,
      type,
      label = paste0(
        "Behaviour (t2): ", short,
        dplyr::if_else(type == "cents", " (cents)", ""),
        "  [pre = ", pre, "]"
      )
    )
}

#' Pro/anti comparison defs used by 08_pro_v_anti.R.
study2_belief_defs_pro_anti <- function() {
  study2_belief_outcomes |>
    dplyr::transmute(
      domain = key,
      change = change_t1,
      pre,
      flip,
      group,
      label = dplyr::if_else(
        key == "polviol", "Political violence", short
      )
    )
}

#' Frozen Cohen's d denominators from study2_behavior_outcomes$d_role.
study2_behavior_d_sds <- function(data, defs = study2_behavior_outcomes) {
  stats::setNames(
    vapply(seq_len(nrow(defs)), function(i) {
      role <- defs$d_role[i]
      if (is.na(role)) return(NA_real_)
      study2_d_sd(data, defs$outcome_t1[i], role)
    }, numeric(1)),
    defs$key
  )
}

study2_belief_d_sds <- function(data, defs = study2_belief_outcomes) {
  stats::setNames(
    vapply(defs$pre, function(v) study2_d_sd(data, v, "pre"), numeric(1)),
    defs$key
  )
}

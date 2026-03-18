# =============================================================================
# 00 · Download data from OSF
# =============================================================================
# Downloads all data files from OSF node rwn89 into the data/ directory.
# Skips files that already exist locally.
#
# Expected files:
#   - study1_initial.csv              (Qualtrics export, initial study)
#   - study1_followup.csv             (Qualtrics export, follow-up study)
#   - classifications_4ns.csv         (LLM meat 4Ns reasoning classifications)
#   - classifications_other.csv       (LLM meat other reasoning classifications)
#   - classifications_reasoning.csv   (LLM charity reasoning classifications)
#   - classifications_persuasion_strategies.csv (LLM AI strategy ratings)

source(here::here("scripts", "functions", "download_data.R"))
download_osf_data("rwn89", osf_path = "data")

########################################################################
# BUILD CANDIDATE-LEVEL NSC ADMISSIONS DATASET
# One row per candidate | APS score | UWC / NWU eligibility flags
########################################################################
#
# INPUT FILES (edit paths as needed):
#   WC_Data_202511.xlsx      - subject-level NSC results, Western Cape
#   NW_Data_202511.xlsx      - subject-level NSC results, North West
#   Subject_List_NSC.xlsx    - Subject code -> Subject name lookup
#
# KNOWN DATA QUIRKS handled below:
#   - NW_Data_202511.xlsx uses lower-case column names (p, dob, g, a,
#     le, nr, r) while WC_Data_202511.xlsx uses Title case (P, DOB, G,
#     A, LE, NR, R). Both are standardised before combining.
#   - 'Admission' contains trailing tabs/whitespace (e.g. "Bachelors\t")
#     - this is trimmed.
#   - A handful of candidates have NR (number of subjects) > 7 - these
#     are kept; APS is still based on the candidate's best 6 subjects
#     excluding Life Orientation (see ASSUMPTIONS).
#
# ASSUMPTIONS (none of these were specified in Metadata_202511.docx -
# adjust the constants in the "ADMISSION CRITERIA" section below if
# your programme guide differs):
#
#   1. APS conversion uses the standard national NSC 7-point achievement
#      scale, summed over the candidate's BEST 6 subjects, EXCLUDING
#      Life Orientation (this is NWU's documented method; UWC's exact
#      weighting is not published in the supplied metadata, so the same
#      scale is applied for consistency - update aps_from_pct() and/or
#      the exclusion rule if UWC uses a different scale).
#
#        0-29%  -> 1 pt   50-59% -> 4 pts
#        30-39% -> 2 pts  60-69% -> 5 pts
#        40-49% -> 3 pts  70-79% -> 6 pts
#                          80-100%-> 7 pts
#
#   2. Programme used for both universities: BSc Mathematical &
#      Statistical Sciences (UWC) / BSc Mathematical Sciences -
#      Statistics & Mathematics (NWU). Requirements (subject codes from
#      Subject_List_NSC.xlsx):
#
#        UWC  - APS >= 33
#             - Mathematics (19331054)                       >= 60%
#             - English HL (13301084) or FAL (13311114)      >= 50%
#             - Any other official-language subject           >= 40%
#             - Physical Sciences (19351114) OR Life Sciences
#               (19351084) OR Information Technology (19351054) >= 50%
#
#        NWU  - APS >= 26
#             - Mathematics (19331054)                       >= 60%
#             - Physical Sciences (19351114)                 >= 50%
#
#   3. A "Bachelors" NSC endorsement (Admission == "Bachelors") is
#      required for degree-level eligibility at both institutions, in
#      addition to the APS/subject rules above.
#
#   4. Mathematical Literacy (19321024) is NOT equivalent to Mathematics
#      for these programmes - only the pure Mathematics code counts.
#
########################################################################

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)

## ------------------------------------------------------------------
## 1. READ RAW FILES
## ------------------------------------------------------------------

wc_raw <- read_excel("WC_Data_202511.xlsx", sheet = "WC_Data")
nw_raw <- read_excel("NW_Data_202511.xlsx", sheet = "Sheet1")
subj_raw <- read_excel("Subject_List_NSC.xlsx", sheet = "SubjectData")

## ------------------------------------------------------------------
## 2. STANDARDISE COLUMN NAMES & COMBINE THE TWO PROVINCIAL FILES
## ------------------------------------------------------------------

canonical_names <- c(
  "candidate_id", "centre_id", "centre_type", "p", "subject", "pct",
  "date", "dob", "g", "a", "le", "nr", "admission", "r"
)

# Both files have the SAME 14 columns in the SAME order - they only
# differ in letter case (WC uses Title case, NW uses lower case), so a
# positional rename is the most robust way to align them (safer than
# matching on name text, since a column literally named "_" doesn't
# survive name-cleaning utilities predictably).
standardise <- function(df) {
  stopifnot(ncol(df) == length(canonical_names))
  names(df) <- canonical_names
  df
}

wc <- standardise(wc_raw)
nw <- standardise(nw_raw)

nsc_long <- bind_rows(wc, nw) %>%
  mutate(
    subject    = as.character(subject),
    admission  = str_squish(as.character(admission)),   # strip \t / extra ws
    centre_type = str_squish(as.character(centre_type)),
    r          = str_squish(as.character(r)),
    candidate_id = as.character(candidate_id)
  )

## ------------------------------------------------------------------
## 3. SUBJECT LOOKUP (for reference / QA only - not required for the
##    eligibility logic, which matches on subject codes directly)
## ------------------------------------------------------------------

subj_lookup <- subj_raw %>%
  clean_names() %>%
  rename(subject = 1, description = 2) %>%
  mutate(
    subject     = str_trim(as.character(subject)),
    description = str_trim(as.character(description))
  ) %>%
  distinct(subject, .keep_all = TRUE)

## ------------------------------------------------------------------
## 4. ADMISSION CRITERIA - subject codes & thresholds
##    (edit here if the programme guide differs from the ASSUMPTIONS
##    noted above)
## ------------------------------------------------------------------

MATH_CODE      <- "19331054"   # Mathematics (NOT Mathematical Literacy)
MATH_LIT_CODE  <- "19321024"   # Mathematical Literacy (does not qualify)
PHYS_SCI_CODE  <- "19351114"   # Physical Sciences
LIFE_SCI_CODE  <- "19351084"   # Life Sciences
IT_CODE        <- "19351054"   # Information Technology
ENG_HL_CODE    <- "13301084"   # English Home Language
ENG_FAL_CODE   <- "13311114"   # English First Additional Language
LO_CODE        <- "16341024"   # Life Orientation (excluded from APS)

# Any language subject other than English HL/FAL, used for UWC's
# "any other language >= 40%" requirement. Built dynamically from the
# subject list so every official-language code is covered.
other_lang_codes <- subj_lookup %>%
  filter(str_detect(description, regex("Language", ignore_case = TRUE))) %>%
  filter(!subject %in% c(ENG_HL_CODE, ENG_FAL_CODE)) %>%
  pull(subject)

## ------------------------------------------------------------------
## 5. APS: convert each subject percentage to an NSC point (1-7) and
##    sum the candidate's best 6 subjects, excluding Life Orientation
## ------------------------------------------------------------------

aps_from_pct <- function(pct) {
  cut(
    pct,
    breaks = c(-Inf, 29, 39, 49, 59, 69, 79, 100),
    labels = 1:7,
    right  = TRUE
  ) %>% as.character() %>% as.integer()
}

nsc_long <- nsc_long %>%
  mutate(aps_points = aps_from_pct(pct))

aps_by_candidate <- nsc_long %>%
  filter(subject != LO_CODE) %>%
  group_by(candidate_id) %>%
  slice_max(order_by = aps_points, n = 6, with_ties = FALSE) %>%
  summarise(APS = sum(aps_points), .groups = "drop")

## ------------------------------------------------------------------
## 6. SUBJECT-SPECIFIC REQUIREMENT INDICATORS (1 = met, 0 = not met)
## ------------------------------------------------------------------

subject_flags <- nsc_long %>%
  group_by(candidate_id) %>%
  summarise(
    math_pct     = max(pct[subject == MATH_CODE], -Inf),
    phys_sci_pct = max(pct[subject == PHYS_SCI_CODE], -Inf),
    life_sci_pct = max(pct[subject == LIFE_SCI_CODE], -Inf),
    it_pct       = max(pct[subject == IT_CODE], -Inf),
    eng_pct      = max(pct[subject %in% c(ENG_HL_CODE, ENG_FAL_CODE)], -Inf),
    other_lang_pct = max(pct[subject %in% other_lang_codes], -Inf),
    .groups = "drop"
  ) %>%
  mutate(
    # UWC: Maths>=60, English HL/FAL>=50, another language>=40,
    #      (Phys Sci OR Life Sci OR IT) >= 50
    uwc_subject_met = as.integer(
      math_pct >= 60 &
        eng_pct  >= 50 &
        other_lang_pct >= 40 &
        (phys_sci_pct >= 50 | life_sci_pct >= 50 | it_pct >= 50)
    ),
    # NWU: Maths>=60, Physical Sciences>=50
    nwu_subject_met = as.integer(
      math_pct >= 60 &
        phys_sci_pct >= 50
    )
  ) %>%
  select(candidate_id, uwc_subject_met, nwu_subject_met)

## ------------------------------------------------------------------
## 7. CONSOLIDATE TO ONE ROW PER CANDIDATE
##    Demographic / centre fields are constant per candidate and taken
##    as-is; Subject and pct (the original "_" column) are collapsed
##    into semicolon-separated lists (one candidate can sit several
##    subjects, so a single scalar value isn't possible without losing
##    information).
## ------------------------------------------------------------------

candidate_core <- nsc_long %>%
  arrange(candidate_id, subject) %>%
  group_by(candidate_id) %>%
  summarise(
    Centre_id   = first(centre_id),
    Centre_type = first(centre_type),
    P           = first(p),
    Subject     = paste(subject, collapse = "; "),
    `_`         = paste(pct, collapse = "; "),
    Date        = first(date),
    DOB         = first(dob),
    G           = first(g),
    A           = first(a),
    LE          = first(le),
    NR          = first(nr),
    Admission   = first(admission),
    R           = first(r),
    .groups = "drop"
  )

## ------------------------------------------------------------------
## 8. FINAL DATASET: eligibility flags + APS
## ------------------------------------------------------------------

candidate_admissions <- candidate_core %>%
  left_join(aps_by_candidate, by = "candidate_id") %>%
  left_join(subject_flags,    by = "candidate_id") %>%
  mutate(
    bachelors_pass = as.integer(Admission == "Bachelors"),
    Eligible_UWC  = as.integer(bachelors_pass == 1 & APS >= 33 & uwc_subject_met == 1),
    Eligible_NWU  = as.integer(bachelors_pass == 1 & APS >= 26 & nwu_subject_met == 1),
    Eligible_Both = as.integer(Eligible_UWC == 1 & Eligible_NWU == 1)
  ) %>%
  rename(Candidate_id = candidate_id) %>%
  select(
    Candidate_id, Centre_id, Centre_type, P, Subject, `_`, Date, DOB,
    G, A, LE, NR, Admission, R,
    APS, Eligible_UWC, Eligible_NWU, Eligible_Both
  )

## ------------------------------------------------------------------
## 9. QUICK CHECKS / OUTPUT
## ------------------------------------------------------------------

glimpse(candidate_admissions)

candidate_admissions %>%
  summarise(
    n_candidates   = n(),
    n_eligible_uwc = sum(Eligible_UWC),
    n_eligible_nwu = sum(Eligible_NWU),
    n_eligible_both = sum(Eligible_Both)
  ) %>%
  print()

# write.csv(candidate_admissions, "candidate_admissions.csv", row.names = FALSE)

# 1. Install and load the package
install.packages("writexl")
library(writexl)

# 2. Save a single dataframe to an Excel file
write_xlsx(candidate_admissions, "C://Users//Administrator//Desktop//STA 761//ResearchProject//candidate_admissions.xlsx")
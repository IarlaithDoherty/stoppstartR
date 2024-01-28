library(tidyverse)
library(readxl)

fulldata0822 <- read_excel(
  paste0(
    "C:/Users/Administrator/OneDrive - Trinity College Dublin/IarlaithMasters",
    "/Data Collection Sheet Session 37 full data set (clean V9) regression.xlsx"),
  sheet = "Data")

# remove NA rows at the bottom
fulldata0822 <- fulldata0822[!is.na(fulldata0822$`Patient Identifier`), ]

# change names of columns 113 and 114
colnames(fulldata0822)[113] <- "any_stopp"
colnames(fulldata0822)[114] <- "any_start"

# fulldata0822$drug_count <- fulldata0822 |>
#   select(starts_with("Drug_")) |>
#   mutate(across(everything(), function(x) !is.na(x))) |>
#   summarise(rowSums(x))
#

fulldata0822$drug_count <- apply(
  fulldata0822[, startsWith(colnames(fulldata0822), "Drug_")],
  MARGIN = 1,
  function(x) sum(!is.na(x))) # count non-NA values

fulldata0822$comorb_count <- apply(
  fulldata0822[, startsWith(colnames(fulldata0822), "Comorbidity_")],
  MARGIN = 1,
  function(x) sum(!is.na(x))) # count non-NA values

fulldata0822[, "Female"] <- fulldata0822[, "Gender"] == "F"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# START ------------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

any_start_glm <- stats::glm(
  formula = any_start ~ Age + Female +
                        `HASBLED Total Score` + `CHA2DS-VASc Total Score` +
                        drug_count + comorb_count,
  data = fulldata0822,
  family = binomial(link = "logit") # specify logistic regression
  )
summary(any_start_glm)
exp(any_start_glm$coefficients) # Odds Ratios

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# START Tests for Independence -------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

t.test(formula = Age ~ any_start, data = fulldata0822)
t.test(formula = `HASBLED Total Score` ~ any_start, data = fulldata0822)
t.test(formula = `CHA2DS-VASc Total Score` ~ any_start, data = fulldata0822)
t.test(formula = drug_count ~ any_start, data = fulldata0822)
t.test(formula = comorb_count ~ any_start, data = fulldata0822)
chisq.test(x = fulldata0822$Gender, y = fulldata0822$any_start)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# START Plots ------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Use ggsave to export these plots.

start_box_age <- ggplot(fulldata0822) +
  geom_boxplot(aes(x = any_start, y = Age))
start_box_age

start_jit_hasbled <- ggplot(fulldata0822) +
  geom_jitter(aes(x = any_start, y = `HASBLED Total Score`))
start_jit_hasbled

start_jit_cha2ds <- ggplot(fulldata0822) +
  geom_jitter(aes(x = any_start, y = `CHA2DS-VASc Total Score`))
start_jit_cha2ds

start_box_comorb <- ggplot(fulldata0822) +
  geom_boxplot(aes(x = any_start, y = comorb_count))
start_box_comorb

start_box_drug <- ggplot(fulldata0822) +
  geom_boxplot(aes(x = any_start, y = drug_count))
start_box_drug

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# STOPP glm --------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

any_stopp_glm <- stats::glm(
  formula = any_stopp ~ Age + Female +
                        `HASBLED Total Score` + `CHA2DS-VASc Total Score` +
                        drug_count + comorb_count,
  data = fulldata0822,
  family = binomial(link = "logit") # specify logistic regression
)
summary(any_stopp_glm)
exp(any_stopp_glm$coefficients) # Odds Ratios

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# STOPP Tests for Independence -------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

t.test(formula = Age ~ any_stopp, data = fulldata0822)
t.test(formula = `HASBLED Total Score` ~ any_stopp, data = fulldata0822)
t.test(formula = `CHA2DS-VASc Total Score` ~ any_stopp, data = fulldata0822)
t.test(formula = drug_count ~ any_stopp, data = fulldata0822)
t.test(formula = comorb_count ~ any_stopp, data = fulldata0822)
chisq.test(x = fulldata0822$Gender, y = fulldata0822$any_stopp)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# STOPP Plots ------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Use ggsave to export these plots.

stopp_box_age <- ggplot(fulldata0822) +
  geom_boxplot(aes(x = any_stopp, y = Age))
stopp_box_age

stopp_jit_hasbled <- ggplot(fulldata0822) +
  geom_jitter(aes(x = any_stopp, y = `HASBLED Total Score`))
stopp_jit_hasbled

stopp_jit_cha2ds <- ggplot(fulldata0822) +
  geom_jitter(aes(x = any_stopp, y = `CHA2DS-VASc Total Score`))
stopp_jit_cha2ds

stopp_box_comorb <- ggplot(fulldata0822) +
  geom_boxplot(aes(x = any_stopp, y = comorb_count))
stopp_box_comorb

stopp_box_drug <- ggplot(fulldata0822) +
  geom_boxplot(aes(x = any_stopp, y = drug_count))
stopp_box_drug

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Other ------------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


plot(fulldata0822$drug_count, fulldata0822$comorb_count)
abline(lm(comorb_count ~ drug_count, data = fulldata0822))
cor(fulldata0822$drug_count, fulldata0822$comorb_count)

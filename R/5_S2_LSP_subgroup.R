

# set-up
library(tidyverse)
library(splines)
library(splitstackshape)
library(survival)
library(lmtest)
library(sandwich)


source("utilities.R")

# load dataset
## overall sample
df.lsp <- readRDS("df_lsp.rds") %>%
  mutate(SBP_BL = ifelse(is.na(SBP_BL), median(SBP_BL, na.rm = T), SBP_BL),
         DBP_BL = ifelse(is.na(DBP_BL), median(DBP_BL, na.rm = T), DBP_BL),
         BMI = ifelse(is.na(BMI), median(BMI, na.rm = T), BMI),
         CAN_score_BL = ifelse(is.na(CAN_score_BL), median(CAN_score_BL, na.rm = T), CAN_score_BL)) %>%
  mutate(exp = case_when(Arm == "LSP/CDS" ~ 1,
                         TRUE ~ 0)) %>%
  mutate(id = row_number()) %>%
  ## create cat for subgroup analyses
  mutate(agecat = factor(case_when(Age < 40 ~ "<40",
                                   Age >=40 & Age < 55 ~ "40-54",
                                   TRUE ~ "55+"),
                         levels = c("<40", "40-54", "55+"))) %>%
  mutate(sbpcat = factor(case_when(SBP_BL<130 ~ "<130",
                                   SBP_BL>= 130 & SBP_BL <140 ~ "130-139",
                                   TRUE ~ "140+"),
                         levels = c("<130", "130-139", "140+")))  

df.lsp.wi <- fun_IPTiW(df = df.lsp,
                       cov = cov_LSP_bl)

df.lsp.merge <- inner_join(df.lsp,
                           select(df.lsp.wi, id, IPTiW),
                           by = c("id"))


## w/o CV indications
df.s1.lsp <- readRDS(df_s1_lsp.rds) %>%
  ## create cat for subgroup analyses
  mutate(agecat = factor(case_when(Age < 40 ~ "<40",
                                   Age >=40 & Age < 55 ~ "40-54",
                                   TRUE ~ "55+"),
                         levels = c("<40", "40-54", "55+"))) %>%
  mutate(sbpcat = factor(case_when(SBP_BL<130 ~ "<130",
                                   SBP_BL>= 130 & SBP_BL <140 ~ "130-139",
                                   TRUE ~ "140+"),
                         levels = c("<130", "130-139", "140+")))

df.s1.lsp.wi <- fun_IPTiW(df = df.s1.lsp,
                          cov = cov_LSP_s1_bl)

df.s1.lsp.merge <- inner_join(df.s1.lsp,
                              select(df.s1.lsp.wi, id, IPTiW),
                              by = c("id"))

rm(df.lsp, df.lsp.wi)
rm(df.s1.lsp, df.s1.lsp.wi)


#################################################################################
# Overall, stratification by AGE, MACE
fun_lsp_subgroup(df.lsp.merge, agecat, "<40",
                 timevar = MACE_itt_time, eventvar = MACE_itt_event, coxHR = TRUE)
fun_lsp_subgroup(df.lsp.merge, agecat, "40-54",
                 timevar = MACE_itt_time, eventvar = MACE_itt_event, coxHR = TRUE)
fun_lsp_subgroup(df.lsp.merge, agecat, "55+",
                 timevar = MACE_itt_time, eventvar = MACE_itt_event, coxHR = TRUE)

# Overall, stratification by AGE, MI
fun_lsp_subgroup(df.lsp.merge, agecat, "<40",
                 timevar = MI_itt_time, eventvar = MI_itt_event, coxHR = TRUE)
fun_lsp_subgroup(df.lsp.merge, agecat, "40-54",
                 timevar = MI_itt_time, eventvar = MI_itt_event, coxHR = TRUE)
fun_lsp_subgroup(df.lsp.merge, agecat, "55+",
                 timevar = MI_itt_time, eventvar = MI_itt_event, coxHR = TRUE)


# Overall stratification by AGE, ischemic stroke
fun_lsp_subgroup(df.lsp.merge, agecat, "<40",
                 timevar = IschemicStroke_itt_time, eventvar = IschemicStroke_itt_event, coxHR = TRUE)
fun_lsp_subgroup(df.lsp.merge, agecat, "40-54",
                 timevar = IschemicStroke_itt_time, eventvar = IschemicStroke_itt_event, coxHR = TRUE)
fun_lsp_subgroup(df.lsp.merge, agecat, "55+",
                 timevar = IschemicStroke_itt_time, eventvar = IschemicStroke_itt_event, coxHR = TRUE)


# Overall stratification by BP, MACE
fun_lsp_subgroup(df.lsp.merge, sbpcat, "<130",
                 timevar = MACE_itt_time, eventvar = MACE_itt_event, coxHR = TRUE)
fun_lsp_subgroup(df.lsp.merge, sbpcat, "130-139",
                 timevar = MACE_itt_time, eventvar = MACE_itt_event, coxHR = TRUE)
fun_lsp_subgroup(df.lsp.merge, sbpcat, "140+",
                 timevar = MACE_itt_time, eventvar = MACE_itt_event, coxHR = TRUE)

# Overall stratification by BP, MI
fun_lsp_subgroup(df.lsp.merge, sbpcat, "<130",
                 timevar = MI_itt_time, eventvar = MI_itt_event, coxHR = TRUE)
fun_lsp_subgroup(df.lsp.merge, sbpcat, "130-139",
                 timevar = MI_itt_time, eventvar = MI_itt_event, coxHR = TRUE)
fun_lsp_subgroup(df.lsp.merge, sbpcat, "140+",
                 timevar = MI_itt_time, eventvar = MI_itt_event, coxHR = TRUE)

# Overall stratification by BP, ischemic stroke
fun_lsp_subgroup(df.lsp.merge, sbpcat, "<130",
                 timevar = IschemicStroke_itt_time, eventvar = IschemicStroke_itt_event, coxHR = TRUE)
fun_lsp_subgroup(df.lsp.merge, sbpcat, "130-139",
                 timevar = IschemicStroke_itt_time, eventvar = IschemicStroke_itt_event, coxHR = TRUE)
fun_lsp_subgroup(df.lsp.merge, sbpcat, "140+",
                 timevar = IschemicStroke_itt_time, eventvar = IschemicStroke_itt_event, coxHR = TRUE)


################################################################################################
# w/o CV indication, stratification by AGE, MACE
fun_lsp_subgroup(df.s1.lsp.merge, agecat, "<40",
                 timevar = MACE_itt_time, eventvar = MACE_itt_event, coxHR = TRUE)
fun_lsp_subgroup(df.s1.lsp.merge, agecat, "40-54",
                 timevar = MACE_itt_time, eventvar = MACE_itt_event, coxHR = TRUE)
fun_lsp_subgroup(df.s1.lsp.merge, agecat, "55+",
                 timevar = MACE_itt_time, eventvar = MACE_itt_event, coxHR = TRUE)

# w/o CV indication, stratification by AGE, MI
fun_lsp_subgroup(df.s1.lsp.merge, agecat, "<40",
                 timevar = MI_itt_time, eventvar = MI_itt_event, coxHR = TRUE)

fun_lsp_subgroup(df.s1.lsp.merge, agecat, "40-54",
                 timevar = MI_itt_time, eventvar = MI_itt_event, coxHR = TRUE)

fun_lsp_subgroup(df.s1.lsp.merge, agecat, "55+",
                 timevar = MI_itt_time, eventvar = MI_itt_event, coxHR = TRUE)

# w/o CV indication, stratification by AGE, ischemic stroke
fun_lsp_subgroup(df.s1.lsp.merge, agecat, "<40",
                 timevar = IschemicStroke_itt_time, eventvar = IschemicStroke_itt_event, coxHR = TRUE)
fun_lsp_subgroup(df.s1.lsp.merge, agecat, "40-54",
                 timevar = IschemicStroke_itt_time, eventvar = IschemicStroke_itt_event, coxHR = TRUE)
fun_lsp_subgroup(df.s1.lsp.merge, agecat, "55+",
                 timevar = IschemicStroke_itt_time, eventvar = IschemicStroke_itt_event, coxHR = TRUE)

# w/o CV indication, stratification by BP, MACE
fun_lsp_subgroup(df.s1.lsp.merge, sbpcat, "<130",
                 timevar = MACE_itt_time, eventvar = MACE_itt_event, coxHR = TRUE)
fun_lsp_subgroup(df.s1.lsp.merge, sbpcat, "130-139",
                 timevar = MACE_itt_time, eventvar = MACE_itt_event, coxHR = TRUE)
fun_lsp_subgroup(df.s1.lsp.merge, sbpcat, "140+",
                 timevar = MACE_itt_time, eventvar = MACE_itt_event, coxHR = TRUE)

# w/o CV indication, stratification by BP, MI
fun_lsp_subgroup(df.s1.lsp.merge, sbpcat, "<130",
                 timevar = MI_itt_time, eventvar = MI_itt_event, coxHR = TRUE)
fun_lsp_subgroup(df.s1.lsp.merge, sbpcat, "130-139",
                 timevar = MI_itt_time, eventvar = MI_itt_event, coxHR = TRUE)
fun_lsp_subgroup(df.s1.lsp.merge, sbpcat, "140+",
                 timevar = MI_itt_time, eventvar = MI_itt_event, coxHR = TRUE)

# w/o CV indication, stratification by BP, ischemic stroke
fun_lsp_subgroup(df.s1.lsp.merge, sbpcat, "<130",
                 timevar = IschemicStroke_itt_time, eventvar = IschemicStroke_itt_event, coxHR = TRUE)
fun_lsp_subgroup(df.s1.lsp.merge, sbpcat, "130-139",
                 timevar = IschemicStroke_itt_time, eventvar = IschemicStroke_itt_event, coxHR = TRUE)
fun_lsp_subgroup(df.s1.lsp.merge, sbpcat, "140+",
                 timevar = IschemicStroke_itt_time, eventvar = IschemicStroke_itt_event, coxHR = TRUE)




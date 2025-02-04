

# set-up
library(tidyverse)
library(splines)
library(splitstackshape)
library(survival)
library(lmtest)
library(sandwich)

source("utilities.R")

# dataset
df.s1.lsp <- readRDS("bl1.rds") %>%
  filter(Arm %in% c("LSP/CDS", "TPM")) %>%
  # filter(startdate >= as.Date("2018-06-01")) %>%        # expand dataset FY08 - FY24
  ## same exclusion criteria as of df.lsp
  filter(!(Arm == "TPM" & FirstMigDate > First_TPMDate)) %>%
  filter(!(Arm == "LSP/CDS" & FirstMigDate > First_LSPDate)) %>%
  filter(RX_everACEIsARBs_BL == "No") %>%
  filter(!(startdate > MACE_date & !is.na(MACE_date))) %>%
  filter(!(Arm == "TPM" & CORX_LSP_BL == "Yes")) %>%
  filter(!(Arm == "LSP/CDS" & CORX_TPM_BL == "Yes")) %>%
  filter(!(Arm == "TPM" & RX_any_ACEIs_ARBs_BL == "Yes")) %>%
  filter(!(Arm == "LSP/CDS" & RX_any_ACEIs_ARBs_BL == "Yes")) %>% 
  filter(AnyPCVisit_1ycat != "None") %>%
  ## CV indications for LSP/CAN
  filter(HTN_BL == "No") %>%
  filter(Hx_MI_BL == "No") %>%
  filter(HF_BL == "No") %>%
  filter(CKD_BL == "No") %>%
  ## update mo of baseline
  mutate(trial_num = (interval(as.Date('2007-10-01'), startdate) %/% months(1)) + 1) %>%
  mutate(MACE_itt_event = MACE_event,
         MACE_itt_date = case_when(MACE_itt_event == 1  ~ MACE_date,
                                   TRUE ~ pmin(as.Date('2024-09-30'), startdate %m+% months(60)-1)),
         MACE_itt_time = (interval(startdate, MACE_itt_date) %/% months(1)) + 1) %>%
  mutate(MI_itt_event = MI_event,
         MI_itt_date = case_when(MI_itt_event == 1 ~ MI_date,
                                 TRUE ~ pmin(as.Date('2024-09-30'), startdate %m+% months(60)-1, Death_date, na.rm = T)),
         MI_itt_time = (interval(startdate, MI_itt_date) %/% months(1)) + 1) %>%
  mutate(IschemicStroke_itt_event = IschemicStroke_event,
         IschemicStroke_itt_date = case_when(IschemicStroke_itt_event == 1 ~ IschemicStroke_date,
                                             TRUE ~ pmin(as.Date('2024-09-30'), startdate %m+% months(60)-1, Death_date, na.rm = T)),
         IschemicStroke_itt_time = (interval(startdate, IschemicStroke_itt_date) %/% months(1)) + 1) %>%
  mutate(ICH_SAH_itt_event = ICH_SAH_event,
         ICH_SAH_itt_date = case_when(ICH_SAH_itt_event == 1 ~ ICH_SAH_date,
                                      TRUE ~ pmin(as.Date('2024-09-30'), startdate %m+% months(60)-1, Death_date, na.rm = T)),
         ICH_SAH_itt_time = (interval(startdate, ICH_SAH_itt_date) %/% months(1)) + 1) %>%
  ## impute missing cont covs to median
  mutate(SBP_BL = ifelse(is.na(SBP_BL), median(SBP_BL, na.rm = T), SBP_BL),
         DBP_BL = ifelse(is.na(DBP_BL), median(DBP_BL, na.rm = T), DBP_BL),
         BMI = ifelse(is.na(BMI), median(BMI, na.rm = T), BMI),
         CAN_score_BL = ifelse(is.na(CAN_score_BL), median(CAN_score_BL, na.rm = T), CAN_score_BL)) %>%
  mutate(exp = case_when(Arm == "LSP/CDS" ~ 1,
                         TRUE ~ 0)) %>%
  mutate(id = row_number())


# drop aCGRP level
df.s1.lsp$Arm <- droplevels(df.s1.lsp$Arm)

# change ref
df.s1.lsp <- fun_ref(df.s1.lsp)


# saveRDS(df.s1.lsp, "df_s1_lsp.rds")

# update BL cov list
cov_LSP_s1_bl <- c("ns(trial_num, df = 5)",
                   "ns(Age, knots = c(35, 45, 60, 75))",
                   "Gender",
                   "Race3cat",
                   "Ethnicity",
                   "GISURH",
                   "SMK",
                   "ns(BMI, df = 4)",
                   "ns(SBP_BL, df = 3)",
                   "ns(DBP_BL, df = 5)",
                   "ns(CAN_score_BL, df = 3)",
                   "flushot_1y",
                   "AFibFlutter_BL",
                   "ARD_BL", 
                   "Cardiomyopathy_BL", 
                   # "CKD_BL",
                   "COPD_BL",
                   "DM_BL",
                   "DysLipid_BL",
                   # "HF_BL",
                   # "HTN_BL",
                   "MDD_BL",
                   "OSA_BL",
                   "PAD_BL",
                   "SRD_BL",
                   "TBI_BL",
                   "VHD_BL",
                   # "Hx_MI_BL", 
                   "Hx_IschemicStroke_BL",
                   "Hx_ICH_SAH_BL",
                   "RX_betaBlocker_BL",
                   "RX_CCB_BL",
                   "RX_Diuretic_BL",
                   "RX_aHTN_other_BL",
                   "RX_aCoag_BL",
                   "RX_aPLT_BL",
                   "RX_lipid_BL",
                   "RX_HRT_BL",
                   "RX_SGLT2_BL",
                   "RX_aPsyc_BL",
                   "ns(Migraine_duration, df = 4)",
                   "chronicMig_BL",
                   "HAEDVisit_1yBin",
                   "HANeuroVisit_1ycat",
                   "RX_any_aCGRP_BL",
                   "RX_Mgr_triptan_BL",
                   "RX_NSAID_BL",
                   "RX_Mgr_otherAnticonvulsant_BL",
                   "RX_Mgr_TCA_BL",
                   "RX_Mgr_neurotoxin_BL")

# overall HRs
fun_itt(df.s1.lsp, timevar = MACE_itt_time, eventvar = MACE_itt_event, cov = cov_LSP_s1_bl, coxHR = TRUE)
fun_itt(df.s1.lsp, timevar = MI_itt_time, eventvar = MI_itt_event, cov = cov_LSP_s1_bl, coxHR = TRUE)
fun_itt(df.s1.lsp, timevar = IschemicStroke_itt_time, eventvar = IschemicStroke_itt_event, cov = cov_LSP_s1_bl, coxHR = TRUE)
fun_itt(df.s1.lsp, timevar = ICH_SAH_itt_time, eventvar = ICH_SAH_itt_event, cov = cov_LSP_s1_bl, coxHR = TRUE)


# Cum inc, RD and RR
## MACE
est.LSP.s1.MACE <- fun_itt(df.s1.lsp, timevar = MACE_itt_time, eventvar = MACE_itt_event, cov = cov_LSP_s1_bl, coxHR = FALSE)

est.LSP.s1.MACE.ci <- fun_bootstrap(seed = 7, n = 500,
                                    df = df.s1.lsp, timevar = MACE_itt_time, eventvar = MACE_itt_event,
                                    cov = cov_LSP_s1_bl, coxHR = FALSE)

merge.LSP.s1.MACE <- inner_join(est.LSP.s1.MACE, est.LSP.s1.MACE.ci, by = c("time"))


## secondary outcomes
### MI
est.LSP.s1.MI <- fun_itt(df.s1.lsp, 
                         timevar = MI_itt_time, eventvar = MI_itt_event, 
                         cov = cov_LSP_s1_bl, coxHR = FALSE)

est.LSP.s1.MI.ci <- fun_bootstrap(seed = 7, n = 500,
                                  df = df.s1.lsp, timevar = MI_itt_time, eventvar = MI_itt_event,
                                  cov = cov_LSP_s1_bl, coxHR = FALSE)

merge.LSP.s1.MI <- inner_join(est.LSP.s1.MI, est.LSP.s1.MI.ci, by = c("time"))

### Ischemic Stroke
est.LSP.s1.IschemicStroke <- fun_itt(df.s1.lsp, 
                                     timevar = IschemicStroke_itt_time, eventvar = IschemicStroke_itt_event, 
                                     cov = cov_LSP_s1_bl, coxHR = FALSE)

est.LSP.s1.IschemicStroke.ci <- fun_bootstrap(seed = 7, n = 500,
                                              df = df.s1.lsp, timevar = IschemicStroke_itt_time, eventvar = IschemicStroke_itt_event,
                                              cov = cov_LSP_s1_bl, coxHR = FALSE)

merge.LSP.s1.IschemicStroke <- inner_join(est.LSP.s1.IschemicStroke, est.LSP.s1.IschemicStroke.ci, by = c("time"))

### ICH/SAH
est.LSP.s1.ICH_SAH <- fun_itt(df.s1.lsp, 
                              timevar = ICH_SAH_itt_time, eventvar = ICH_SAH_itt_event, 
                              cov = cov_LSP_s1_bl, coxHR = FALSE)

est.LSP.s1.ICH_SAH.ci <- fun_bootstrap(seed = 7, n = 500,
                                       df = df.s1.lsp, timevar = ICH_SAH_itt_time, eventvar = ICH_SAH_itt_event,
                                       cov = cov_LSP_s1_bl, coxHR = FALSE)

merge.LSP.s1.ICH_SAH <- inner_join(est.LSP.s1.ICH_SAH, est.LSP.s1.ICH_SAH.ci, by = c("time"))






# set-up
library(tidyverse)
library(labelled)
library(survey)
library(tableone)
library(splines)
library(lmtest)
library(sandwich)
library(splitstackshape)
library(sqldf)
library(survival)
library(survminer)
library(patchwork)

source("utilities.R")

#---------------------------------------------------------------------------------------#
#------------------------------ 1. LSP/CAN vs. TPM -------------------------------------#
#---------------------------------------------------------------------------------------#

# 1.load dataset-------------------------------------------------------------------

df.lsp <- readRDS("df_lsp.rds") %>%
  ## impute missing cont covs to median
  mutate(SBP_BL = ifelse(is.na(SBP_BL), median(SBP_BL, na.rm = T), SBP_BL),
         DBP_BL = ifelse(is.na(DBP_BL), median(DBP_BL, na.rm = T), DBP_BL),
         BMI = ifelse(is.na(BMI), median(BMI, na.rm = T), BMI),
         CAN_score_BL = ifelse(is.na(CAN_score_BL), median(CAN_score_BL, na.rm = T), CAN_score_BL)) %>%
  mutate(exp = case_when(Arm == "LSP/CDS" ~ 1,
                         TRUE ~ 0)) %>%
  mutate(id = row_number())


# set ref
df.lsp <- fun_ref(df.lsp)

# IPTiW
df.lsp.wi <- fun_IPTiW(df = df.lsp,
                       cov = cov_LSP_bl)

summary(df.lsp.wi$IPTiW)    ## check IPTiW distribution

# 1.2. chekc SMD-------------------------------------------------------------------
df.lsp.wi.merge <- inner_join(df.lsp,
                              select(df.lsp.wi, id, IPTiW),
                              by = c("id"))

var_label(df.lsp.wi.merge) <- varLabelList

SMD.vars <- c("Age", "Gender", "Race3cat", "Ethnicity", "GISURH", "ServiceConnectedFlag", "SMK",
              "BMI", "SBP_BL", "DBP_BL", "CAN_score_BL", "AnyPCVisit_1ycat", "flushot_1y",
              "AFibFlutter_BL", "ARD_BL", "Cardiomyopathy_BL", "CID_BL", "CKD_BL", "COPD_BL", "DM_BL", "DVT_PE_BL", 
              "DysLipid_BL", "HTN_BL", "HF_BL", "MDD_BL", "OSA_BL",  "PAD_BL", "SRD_BL", "TBI_BL", "VHD_BL",
              "Hx_MACE_BL", "Hx_MI_BL", "Hx_IschemicStroke_BL", "Hx_ICH_SAH_BL",
              # "RX_any_ACEIs_ARBs_BL", 
              "RX_betaBlocker_BL", "RX_CCB_BL", "RX_Diuretic_BL", "RX_aHTN_other_BL",
              "RX_aCoag_BL", "RX_aPLT_BL", "RX_lipid_BL", "RX_HRT_BL", "RX_GLP1_BL", "RX_SGLT2_BL", "RX_aPsyc_BL", 
              "Migraine_duration", "chronicMig_BL", "HAEDVisit_1yBin", "HANeuroVisit_1ycat", "RX_any_aCGRP_BL",
              "RX_Mgr_ergotamine_BL", "RX_Mgr_triptan_BL", "RX_NSAID_BL", "RX_Mgr_otherAnticonvulsant_BL", "RX_Mgr_SNRI_BL", 
              "RX_Mgr_TCA_BL", "RX_Mgr_neurotoxin_BL")


# no weight
noweightedtbl <- CreateTableOne(vars = SMD.vars, 
                                strata = "Arm",
                                data = df.lsp.wi.merge,
                                test = FALSE)

# add IPTiW
weighted <- svydesign(ids = ~ 1, data = df.lsp.wi.merge, weights = ~ IPTiW)
weightedtbl <- svyCreateTableOne(vars = SMD.vars,
                                 strata = "Arm",
                                 data = weighted,
                                 test = FALSE)

# 1.3.SMD plot-------------------------------------------------------------------
SMD.plot <- data.frame(variable = unlist(noweightedtbl$MetaData$varLabels),
                       Unadjusted = as.numeric(ExtractSmd(noweightedtbl)),
                       Weighted = as.numeric(ExtractSmd(weightedtbl)))


SMD.plot.melt <- reshape2::melt(data = SMD.plot,
                                id.vars = "variable",
                                variable.name = "Method",
                                value.name = "SMD") %>%
  ## order x-axis variable order 
  mutate(variable_new = fct_relevel(variable, 
                                    "Neurotoxin",
                                    "TCAs",
                                    "SNRI",
                                    "Other anticonvulsants",
                                    "NSAIDs",
                                    "Triptans",
                                    "Ergotamines",
                                    "Any aCGRP",
                                    "Headache-related Neurology visits",
                                    "Headache-related ED visits",
                                    "Chronic migraine",
                                    "Migraine duration, mths",
                                    "Anti-psychotics",
                                    "SGLT2i",
                                    "GLP-1 agonists",
                                    "HRT",
                                    "Lipid modifying agents",
                                    "Anti-platelets",
                                    "Anti-coagulants",
                                    "Other antihypertensives",
                                    "Diuretics",
                                    "Calcium channel blockers",
                                    "\u03b2-blockers",
                                    # "Any ACEIs/ARBs",
                                    "History of ICH/SAH",
                                    "History of ischemic stroke",
                                    "History of MI",
                                    "History of MACE",
                                    "Valvular heart disease",
                                    "Traumatic brain injury",
                                    "Substance-related disorder",
                                    "Peripheral artery disease",
                                    "Obstructive sleep apnea",
                                    "Depression",
                                    "Heart failure",
                                    "Hypertension",
                                    "Dyslipidemia",
                                    "DVT/PE",
                                    "Diabetes",
                                    "COPD",
                                    "Chronic kidney disease",
                                    "Chronic inflammatory conditions",
                                    "Cardiomyopathy",
                                    "Alcohol-related disorder",
                                    "Atrial fibrillation/flutter",
                                    "Influena vaccine",
                                    "No. primary care visits",
                                    "Care assessment need score",
                                    "Diastolic BP, mmHg",
                                    "Systolic BP, mmHg",
                                    "Body mass index, kg/m2",
                                    "Smoking status",
                                    "Service-connected disability",
                                    "Residence",
                                    "Ethnicity",
                                    "Race",
                                    "Gender",
                                    "Age, years"))  


SMDplot <- ggplot(SMD.plot.melt, aes(x = variable_new, y = SMD, group = Method, color = Method)) +
  geom_point(aes(shape = Method), size = 4) +
  geom_hline(aes(yintercept = 0.1), color = "black", linewidth = 1.2) +
  coord_flip() + 
  theme_minimal() +
  # scale_y_continuous(limits = c(0, .52), breaks = seq(0, .52, 0.1)) +
  xlab(" ") +
  ylab("Standardized mean difference") +
  scale_color_manual(values = c("#ED4B4B", "#5596E6")) +
  scale_shape_manual(values = c(17, 16)) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line.x = element_line(color = "black", linewidth = 1),
        axis.line.y = element_line(color = "black", linewidth = 1),
        axis.ticks = element_line(linewidth=0.5),
        panel.grid.major.x = element_line(color = "darkgrey", linetype = "dashed"),
        panel.grid.minor.x = element_line(color = "darkgrey", linetype = "dashed"),
        axis.text = element_text(color = "black", size = 9),
        text = element_text(color = "black", size = 9),
        legend.title = element_blank())


ggsave(SMDplot, filename = paste("SMD_LSP_", Sys.Date(), ".tiff", sep = "") ,
       width = 7, height = 9, dpi = 450, units = "in")


rm(df.lsp.wi.merge)
rm(noweightedtbl)
rm(weighted)
rm(weightedtbl)
rm(varLabelList)
rm(SMD.vars)
rm(SMD.plot)
rm(SMD.plot.melt)
rm(SMDplot)

# 1.4.Overall HRs-----------------------------------------------------------------------------------------------------
fun_itt(df.lsp, timevar = MACE_itt_time, eventvar = MACE_itt_event, cov = cov_LSP_bl, coxHR = TRUE)
fun_itt(df.lsp, timevar = MI_itt_time, eventvar = MI_itt_event, cov = cov_LSP_bl, coxHR = TRUE)
fun_itt(df.lsp, timevar = IschemicStroke_itt_time, eventvar = IschemicStroke_itt_event, cov = cov_LSP_bl, coxHR = TRUE)
fun_itt(df.lsp, timevar = ICH_SAH_itt_time, eventvar = ICH_SAH_itt_event, cov = cov_LSP_bl, coxHR = TRUE)

# 1.5.Cum inc, RD, RR-------------------------------------------------------------------------------------------------
# MACE
est.LSP.MACE <- fun_itt(df.lsp, timevar = MACE_itt_time, eventvar = MACE_itt_event, cov = cov_LSP_bl, coxHR = FALSE)

est.LSP.MACE.ci <- fun_bootstrap(seed = 7, n = 500,
                                 df = df.lsp, timevar = MACE_itt_time, eventvar = MACE_itt_event,
                                 cov = cov_LSP_bl, coxHR = FALSE)        ## ~ 14 hrs

merge.LSP.MACE <- inner_join(est.LSP.MACE, est.LSP.MACE.ci, by = c("time"))

rm(est.LSP.MACE, est.LSP.MACE.ci)

# secondary outcomes

## MI
est.LSP.MI <- fun_itt(df.lsp, 
                      timevar = MI_itt_time, eventvar = MI_itt_event, 
                      cov = cov_LSP_bl, coxHR = FALSE)

est.LSP.MI.ci <- fun_bootstrap(seed = 7, n = 500,
                               df = df.lsp, timevar = MI_itt_time, eventvar = MI_itt_event,
                               cov = cov_LSP_bl, coxHR = FALSE)

merge.LSP.MI <- inner_join(est.LSP.MI, est.LSP.MI.ci, by = c("time"))

## ischemic stroke
est.LSP.IschemicStroke <- fun_itt(df.lsp, 
                                  timevar = IschemicStroke_itt_time, eventvar = IschemicStroke_itt_event, 
                                  cov = cov_LSP_bl, coxHR = FALSE)

est.LSP.IschemicStroke.ci <- fun_bootstrap(seed = 7, n = 500,
                                           df = df.lsp, timevar = IschemicStroke_itt_time, eventvar = IschemicStroke_itt_event,
                                           cov = cov_LSP_bl, coxHR = FALSE)

merge.LSP.IschemicStroke <- inner_join(est.LSP.IschemicStroke, est.LSP.IschemicStroke.ci, by = c("time"))

## ICH/SAH
est.LSP.ICH_SAH <- fun_itt(df.lsp, 
                           timevar = ICH_SAH_itt_time, eventvar = ICH_SAH_itt_event, 
                           cov = cov_LSP_bl, coxHR = FALSE)

est.LSP.ICH_SAH.ci <- fun_bootstrap(seed = 7, n = 500,
                                    df = df.lsp, timevar = ICH_SAH_itt_time, eventvar = ICH_SAH_itt_event,
                                    cov = cov_LSP_bl, coxHR = FALSE)

merge.LSP.ICH_SAH <- inner_join(est.LSP.ICH_SAH, est.LSP.ICH_SAH.ci, by = c("time"))

rm(est.LSP.MI, est.LSP.MI.ci)
rm(est.LSP.IschemicStroke, est.LSP.IschemicStroke.ci)
rm(est.LSP.ICH_SAH, est.LSP.ICH_SAH.ci)

# 1.6.Cum inc plot-------------------------------------------------------------------------------------------------

# MACE
g1 <- fun_cumIncPlot(df = df.lsp, df_boot = merge.LSP.MACE,
                     expmed = "LSP/CAN", timevar = "MACE_itt_time", eventvar = "MACE_itt_event",
                     expmedcolor = "#E6A555", ymax = 8, ybreaks = 2)

# secondary
g2 <- fun_cumIncPlot(df = df.lsp, df_boot = merge.LSP.MI,
                     expmed = "LSP/CAN", timevar = "MI_itt_time", eventvar = "MI_itt_event",
                     expmedcolor = "#E6A555", ymax = 3, ybreaks = 0.5)

g3 <- fun_cumIncPlot(df = df.lsp, df_boot = merge.LSP.IschemicStroke,
                     expmed = "LSP/CAN", timevar = "IschemicStroke_itt_time", eventvar = "IschemicStroke_itt_event",
                     expmedcolor = "#E6A555", ymax = 2.2, ybreaks = 0.4)

g4 <- fun_cumIncPlot(df = df.lsp, df_boot = merge.LSP.ICH_SAH,
                     expmed = "LSP/CAN", timevar = "ICH_SAH_itt_time", eventvar = "ICH_SAH_itt_event",
                     expmedcolor = "#E6A555", ymax = 0.42, ybreaks = 0.1)

ggsave(g1, filename = "LSP_MACE_itt.tiff", width = 7.5, height = 7.5, bg = "transparent")
ggsave(g2, filename = "LSP_MI_itt.tiff", width = 7.5, height = 7.5, bg = "transparent")
ggsave(g3, filename = "LSP_IscStroke_itt.tiff", width = 7.5, height = 7.5, bg = "transparent")
ggsave(g4, filename = "LSP_ICH_SAH_itt.tiff", width = 7.5, height = 7.5, bg = "transparent")

rm(merge.LSP.MACE, merge.LSP.MI, merge.LSP.IschemicStroke, merge.LSP.ICH_SAH)
rm(g1, g2, g3, g4)
rm(df.lsp)
rm(df.lsp.wi)


#-------------------------------------------------------------------------------------#
#------------------------------ 2. aCGRP vs. TPM -------------------------------------#
#-------------------------------------------------------------------------------------#

# 2.1.load dataset-------------------------------------------------------------------
df.aCGRP <- readRDS("df_aCGRP_coRx_20241222.rds") %>%
  ## impute missing cont covs to median
  mutate(SBP_BL = ifelse(is.na(SBP_BL), median(SBP_BL, na.rm = T), SBP_BL),
         DBP_BL = ifelse(is.na(DBP_BL), median(DBP_BL, na.rm = T), DBP_BL),
         BMI = ifelse(is.na(BMI), median(BMI, na.rm = T), BMI),
         CAN_score_BL = ifelse(is.na(CAN_score_BL), median(CAN_score_BL, na.rm = T), CAN_score_BL)) %>%
  mutate(exp = case_when(Arm == "aCGRP" ~ 1,
                         TRUE ~ 0)) %>%
  mutate(id = row_number())

# set ref
df.aCGRP <- fun_ref(df.aCGRP)

# IPTiW
df.aCGRP.wi <- fun_IPTiW(df = df.aCGRP,
                         cov = cov_aCGRP_bl)

summary(df.aCGRP.wi$IPTiW)    ## check IPTiW distribution

# 2.2. chekc SMD-------------------------------------------------------------------
df.aCGRP.wi.merge <- inner_join(df.aCGRP,
                                select(df.aCGRP.wi, id, IPTiW),
                                by = c("id"))

var_label(df.aCGRP.wi.merge) <- varLabelList

SMD.vars <- c("Age", "Gender", "Race3cat", "Ethnicity", "GISURH", "ServiceConnectedFlag", "SMK",
              "BMI", "SBP_BL", "DBP_BL", "CAN_score_BL", "AnyPCVisit_1ycat", "flushot_1y",
              "AFibFlutter_BL", "ARD_BL", "Cardiomyopathy_BL", "CID_BL", "CKD_BL", "COPD_BL", "DM_BL", "DVT_PE_BL", 
              "DysLipid_BL", "HTN_BL", "HF_BL", "MDD_BL", "OSA_BL",  "PAD_BL", "SRD_BL", "TBI_BL", "VHD_BL",
              "Hx_MACE_BL", "Hx_MI_BL", "Hx_IschemicStroke_BL", "Hx_ICH_SAH_BL",
              "RX_any_ACEIs_ARBs_BL", "RX_betaBlocker_BL", "RX_CCB_BL", "RX_Diuretic_BL", "RX_aHTN_other_BL",
              "RX_aCoag_BL", "RX_aPLT_BL", "RX_lipid_BL", "RX_HRT_BL", "RX_GLP1_BL", "RX_SGLT2_BL", "RX_aPsyc_BL", 
              "Migraine_duration", "chronicMig_BL", "HAEDVisit_1yBin", "HANeuroVisit_1ycat", 
              # "RX_any_aCGRP_BL",
              "RX_Mgr_ergotamine_BL", "RX_Mgr_triptan_BL", "RX_NSAID_BL", "RX_Mgr_otherAnticonvulsant_BL", "RX_Mgr_SNRI_BL", 
              "RX_Mgr_TCA_BL", "RX_Mgr_neurotoxin_BL")


# no weight
noweightedtbl <- CreateTableOne(vars = SMD.vars, 
                                strata = "Arm",
                                data = df.aCGRP.wi.merge,
                                test = FALSE)

# add IPTiW
weighted <- svydesign(ids = ~ 1, data = df.aCGRP.wi.merge, weights = ~ IPTiW)

weightedtbl <- svyCreateTableOne(vars = SMD.vars,
                                 strata = "Arm",
                                 data = weighted,
                                 test = FALSE)

# print(noweightedtbl, smd = TRUE)
# print(weightedtbl, smd = TRUE)

# 2.3.SMD plot-------------------------------------------------------------------

SMD.plot <- data.frame(variable = unlist(noweightedtbl$MetaData$varLabels),
                       Unadjusted = as.numeric(ExtractSmd(noweightedtbl)),
                       Weighted = as.numeric(ExtractSmd(weightedtbl)))


SMD.plot.melt <- reshape2::melt(data = SMD.plot,
                                id.vars = "variable",
                                variable.name = "Method",
                                value.name = "SMD") %>%
  ## order x-axis variable order 
  mutate(variable_new = fct_relevel(variable, 
                                    "Neurotoxin",
                                    "TCAs",
                                    "SNRI",
                                    "Other anticonvulsants",
                                    "NSAIDs",
                                    "Triptans",
                                    "Ergotamines",
                                    # "Any aCGRP",
                                    "Headache-related Neurology visits",
                                    "Headache-related ED visits",
                                    "Chronic migraine",
                                    "Migraine duration, mths",
                                    "Anti-psychotics",
                                    "SGLT2i",
                                    "GLP-1 agonists",
                                    "HRT",
                                    "Lipid modifying agents",
                                    "Anti-platelets",
                                    "Anti-coagulants",
                                    "Other antihypertensives",
                                    "Diuretics",
                                    "Calcium channel blockers",
                                    "\u03b2-blockers",
                                    "Any ACEIs/ARBs",
                                    "History of ICH/SAH",
                                    "History of ischemic stroke",
                                    "History of MI",
                                    "History of MACE",
                                    "Valvular heart disease",
                                    "Traumatic brain injury",
                                    "Substance-related disorder",
                                    "Peripheral artery disease",
                                    "Obstructive sleep apnea",
                                    "Depression",
                                    "Heart failure",
                                    "Hypertension",
                                    "Dyslipidemia",
                                    "DVT/PE",
                                    "Diabetes",
                                    "COPD",
                                    "Chronic kidney disease",
                                    "Chronic inflammatory conditions",
                                    "Cardiomyopathy",
                                    "Alcohol-related disorder",
                                    "Atrial fibrillation/flutter",
                                    "Influena vaccine",
                                    "No. primary care visits",
                                    "Care assessment need score",
                                    "Diastolic BP, mmHg",
                                    "Systolic BP, mmHg",
                                    "Body mass index, kg/m2",
                                    "Smoking status",
                                    "Service-connected disability",
                                    "Residence",
                                    "Ethnicity",
                                    "Race",
                                    "Gender",
                                    "Age, years"))  


SMDplot <- ggplot(SMD.plot.melt, aes(x = variable_new, y = SMD, group = Method, color = Method)) +
  geom_point(aes(shape = Method), size = 4) +
  geom_hline(aes(yintercept = 0.1), color = "black", linewidth = 1.2) +
  coord_flip() + 
  theme_minimal() +
  # scale_y_continuous(limits = c(0, .52), breaks = seq(0, .52, 0.1)) +
  xlab(" ") +
  ylab("Standardized mean difference") +
  scale_color_manual(values = c("#ED4B4B", "#5596E6")) +
  scale_shape_manual(values = c(17, 16)) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line.x = element_line(color = "black", linewidth = 1),
        axis.line.y = element_line(color = "black", linewidth = 1),
        axis.ticks = element_line(linewidth=0.5),
        panel.grid.major.x = element_line(color = "darkgrey", linetype = "dashed"),
        panel.grid.minor.x = element_line(color = "darkgrey", linetype = "dashed"),
        axis.text = element_text(color = "black", size = 9),
        text = element_text(color = "black", size = 9),
        legend.title = element_blank())

ggsave(SMDplot, filename = paste("SMD_aCGRP_", Sys.Date(), ".tiff", sep = "") ,
       width = 7, height = 9, dpi = 450, units = "in")


rm(df.aCGRP.wi.merge)
rm(noweightedtbl)
rm(weighted)
rm(weightedtbl)
rm(varLabelList)
rm(SMD.vars)
rm(SMD.plot)
rm(SMD.plot.melt)
rm(SMDplot)


# 2.4.Overall HRs-----------------------------------------------------------------------------------------------------
fun_itt(df.aCGRP, timevar = MACE_itt_time, eventvar = MACE_itt_event, cov = cov_aCGRP_bl, coxHR = TRUE)
fun_itt(df.aCGRP, timevar = MI_itt_time, eventvar = MI_itt_event, cov = cov_aCGRP_bl, coxHR = TRUE)
fun_itt(df.aCGRP, timevar = IschemicStroke_itt_time, eventvar = IschemicStroke_itt_event, cov = cov_aCGRP_bl, coxHR = TRUE)
fun_itt(df.aCGRP, timevar = ICH_SAH_itt_time, eventvar = ICH_SAH_itt_event, cov = cov_aCGRP_bl, coxHR = TRUE)

# 2.5.Cum inc, RD, RR-------------------------------------------------------------------------------------------------
# MACE
est.aCGRP.MACE <- fun_itt(df.aCGRP, timevar = MACE_itt_time, eventvar = MACE_itt_event, cov = cov_aCGRP_bl, coxHR = FALSE)

est.aCGRP.MACE.ci <- fun_bootstrap(seed = 7, n = 500,
                                   df = df.aCGRP, timevar = MACE_itt_time, eventvar = MACE_itt_event,
                                   cov = cov_aCGRP_bl, coxHR = FALSE)

merge.aCGRP.MACE <- inner_join(est.aCGRP.MACE, est.aCGRP.MACE.ci, by = c("time"))


rm(est.aCGRP.MACE, est.aCGRP.MACE.ci)

# secondary outcomes
## MI
est.aCGRP.MI <- fun_itt(df.aCGRP, 
                        timevar = MI_itt_time, eventvar = MI_itt_event, 
                        cov = cov_aCGRP_bl, coxHR = FALSE)

est.aCGRP.MI.ci <- fun_bootstrap(seed = 7, n = 500,
                                 df = df.aCGRP, timevar = MI_itt_time, eventvar = MI_itt_event,
                                 cov = cov_aCGRP_bl, coxHR = FALSE)

merge.aCGRP.MI <- inner_join(est.aCGRP.MI, est.aCGRP.MI.ci, by = c("time"))


## ischemic stroke
est.aCGRP.IschemicStroke <- fun_itt(df.aCGRP, 
                                    timevar = IschemicStroke_itt_time, eventvar = IschemicStroke_itt_event, 
                                    cov = cov_aCGRP_bl, coxHR = FALSE)

est.aCGRP.IschemicStroke.ci <- fun_bootstrap(seed = 7, n = 500,
                                             df = df.aCGRP, timevar = IschemicStroke_itt_time, eventvar = IschemicStroke_itt_event,
                                             cov = cov_aCGRP_bl, coxHR = FALSE)     ## 17.6 hrs

merge.aCGRP.IschemicStroke <- inner_join(est.aCGRP.IschemicStroke, est.aCGRP.IschemicStroke.ci, by = c("time"))


## ICH/SAH
est.aCGRP.ICH_SAH <- fun_itt(df.aCGRP, 
                             timevar = ICH_SAH_itt_time, eventvar = ICH_SAH_itt_event, 
                             cov = cov_aCGRP_bl, coxHR = FALSE)

est.aCGRP.ICH_SAH.ci <- fun_bootstrap(seed = 7, n = 500,
                                      df = df.aCGRP, timevar = ICH_SAH_itt_time, eventvar = ICH_SAH_itt_event,
                                      cov = cov_aCGRP_bl, coxHR = FALSE)

merge.aCGRP.ICH_SAH <- inner_join(est.aCGRP.ICH_SAH, est.aCGRP.ICH_SAH.ci, by = c("time"))


rm(est.aCGRP.MI, est.aCGRP.MI.ci)
rm(est.aCGRP.IschemicStroke, est.aCGRP.IschemicStroke.ci)
rm(est.aCGRP.ICH_SAH, est.aCGRP.ICH_SAH.ci)


# 2.6.Cum inc plot-------------------------------------------------------------------------------------------------

# MACE
h1 <- fun_cumIncPlot(df = df.aCGRP, df_boot = merge.aCGRP.MACE,
                     expmed = "aCGRP", timevar = "MACE_itt_time", eventvar = "MACE_itt_event",
                     expmedcolor = "#ED4B4B", ymax = 5.5, ybreaks = 1)

# secondary
h2 <- fun_cumIncPlot(df = df.aCGRP, df_boot = merge.aCGRP.MI,
                     expmed = "aCGRP", timevar = "MI_itt_time", eventvar = "MI_itt_event",
                     expmedcolor = "#ED4B4B", ymax = 1.7, ybreaks = 0.4)

h3 <- fun_cumIncPlot(df = df.aCGRP, df_boot = merge.aCGRP.IschemicStroke,
                     expmed = "aCGRP", timevar = "IschemicStroke_itt_time", eventvar = "IschemicStroke_itt_event",
                     expmedcolor = "#ED4B4B", ymax = 2, ybreaks = 0.4)


h4 <- fun_cumIncPlot(df = df.aCGRP, df_boot = merge.aCGRP.ICH_SAH,
                     expmed = "aCGRP", timevar = "ICH_SAH_itt_time", eventvar = "ICH_SAH_itt_event",
                     expmedcolor = "#ED4B4B", ymax = 0.28, ybreaks = 0.05)

ggsave(h1, filename = "aCGRP_MACE_itt.tiff", width = 7.5, height = 7.5, bg = "transparent")
ggsave(h2, filename = "aCGRP_MI_itt.tiff", width = 7.5, height = 7.5, bg = "transparent")
ggsave(h3, filename = "aCGRP_IscStroke_itt.tiff", width = 7.5, height = 7.5, bg = "transparent")
ggsave(h4, filename = "aCGRP_ICH_SAH_itt.tiff", width = 7.5, height = 7.5, bg = "transparent")

rm(merge.aCGRP.MACE, merge.aCGRP.MI, merge.aCGRP.IschemicStroke, merge.aCGRP.ICH_SAH)
rm(h1, h2, h3, h4)
rm(df.aCGRP)
rm(df.aCGRP.wi)


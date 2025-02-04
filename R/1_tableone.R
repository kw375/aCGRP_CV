

# load libraries
library(tidyverse)
library(labelled)
library(tableone)

source("utilities.R")


# Table 1

## define variables
vars.tbl.bl <- c("Age", "Gender", "Race", "Ethnicity", "VISN", 
                 "GISURH", "ADI_NATRANK", "ServiceConnectedFlag", "SMK", 
                 "BMI", "BMICat", "SBP_BL", "DBP_BL", "CAN_score_BL", "AnyPCVisit_1y", "AnyPCVisit_1ycat", "flushot_1y", 
                 # comorbidity
                 "AFibFlutter_BL", "ARD_BL", "Cardiomyopathy_BL", "CID_BL", "CKD_BL", "COPD_BL", "DM_BL", "DVT_PE_BL", "DysLipid_BL",
                 "HF_BL", "HTN_BL", "MDD_BL", "OSA_BL", "PAD_BL", "SRD_BL", "TBI_BL", "VHD_BL",
                 "Hx_MACE_BL", "Hx_MI_BL", "Hx_IschemicStroke_BL", "Hx_ICH_SAH_BL", "Hx_HemoStroke_BL", "Hx_SAH_BL",
                 # rx
                 "RX_any_ACEIs_ARBs_BL", "RX_other_ACEIs_ARBs_BL", "RX_betaBlocker_BL", "RX_CCB_BL", 
                 "RX_Diuretic_BL", "RX_Diuretic_potassium_BL", "RX_aHTN_other_BL", 
                 "RX_aCoag_BL", "RX_aPLT_BL", "RX_lipid_BL", "RX_HRT_BL", "RX_GLP1_BL", "RX_SGLT2_BL", "RX_aPsyc_BL", 
                 # migraine
                 "Migraine_duration", "chronicMig_BL", "HAEDVisit_1ycat", "HANeuroVisit_1ycat", "RX_any_aCGRP_BL", "CORX_aCGRP_BL",
                 "RX_Mgr_aCGRP_acute_BL", "RX_Mgr_ergotamine_BL", "RX_Mgr_triptan_BL", "RX_NSAID_BL", 
                 "RX_Mgr_otherAnticonvulsant_BL", "RX_Mgr_SNRI_BL", "RX_Mgr_TCA_BL", "RX_Mgr_neurotoxin_BL",
                 "MPR", "daysdiff")

medianvars <- c("CAN_score_BL", "AnyPCVisit_1y", "Migraine_duration", "MPR", "daysdiff")

## LSP/CAN vs. TPM
tbl.bl <- CreateTableOne(vars = vars.tbl.bl,
                         data = df.lsp,
                         strata = "Arm",
                         includeNA = TRUE,
                         test = FALSE)

## aCGRP vs. TPM
tbl.bl <- CreateTableOne(vars = vars.tbl.bl,
                         data = df.aCGRP,
                         strata = "Arm",
                         includeNA = TRUE,
                         test = FALSE)

## Output table 1
tblPrint <- print(tbl.bl,
                  nonnormal = medianvars,
                  showAllLevels = TRUE,
                  contDigits = 2,
                  test = FALSE,
                  missing = TRUE,
                  quote = FALSE,
                  varLabels = TRUE,
                  dropEqual = TRUE,
                  noSpaces = TRUE,
                  smd = TRUE)

write.table(tblPrint,
            file = paste("table1_LSP_", Sys.Date(), ".txt", sep = ""),
            sep = "\t", quote = F, row.names = T, col.names = NA)




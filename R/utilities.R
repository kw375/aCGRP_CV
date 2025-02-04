
#####################
### binary factor ###
#####################
YNFun <- function(x) {
  factor(case_when(!is.na(x) ~ "Yes",
                   TRUE ~ "No"),
         levels = c("Yes", "No")) 
}

##############
### labels ###
##############

varLabelList <- list(Arm = "Treatment strategy",
                     MACE_event = "MACE",
                     MI_event = "MI",
                     IschemicStroke_event = "Ischemic stroke",
                     Death_event = "Death",
                     Gender = "Gender",
                     Race = "Race",
                     Ethnicity = "Ethnicity",
                     ServiceConnectedFlag = "Service-connected disability",
                     GISURH = "Residence",
                     ADI_NATRANK = "Area deprivation index (2015)",
                     Weight_BL = "Weight, lbs",
                     Weight_kg_BL = "Weight, kg",
                     SBP_BL = "Systolic BP, mmHg",
                     DBP_BL = "Diastolic BP, mmHg",
                     CAN_score_BL = "Care assessment need score",
                     VISN = "VISN",
                     MPR = "Medication possession raio",
                     Age = "Age, years",
                     Race3cat = "Race",
                     SMK = "Smoking status",
                     BMI = "Body mass index, kg/m2",
                     BMICat = "Body mass index, kg/m2",
                     AFibFlutter_BL = "Atrial fibrillation/flutter",
                     ARD_BL = "Alcohol-related disorder",
                     AS_BL = "Ankylosing spondylitis",
                     chronicMig_BL = "Chronic migraine",
                     Cardiomyopathy_BL = "Cardiomyopathy",
                     CKD_BL = "Chronic kidney disease",
                     COPD_BL = "COPD",
                     DM_BL = "Diabetes",
                     DVT_PE_BL = "DVT/PE",
                     DysLipid_BL = "Dyslipidemia",
                     HF_BL = "Heart failure",
                     HIV_BL = "HIV",
                     HTN_BL = "Hypertension",
                     IBD_BL = "Inflammatory bowel disease",
                     MDD_BL = "Depression",
                     OSA_BL = "Obstructive sleep apnea",
                     PAD_BL = "Peripheral artery disease",
                     PsA_BL = "Psoriatic arthritis",
                     RA_BL = "Rheumatoid arthritis",
                     SLE_BL = "SLE",
                     SRD_BL = "Substance-related disorder",
                     SSc_BL = "Systemic sclerosis",
                     TBI_BL = "Traumatic brain injury",
                     VHD_BL = "Valvular heart disease",
                     Hx_MI_BL = "History of MI",
                     Hx_IschemicStroke_BL = "History of ischemic stroke", 
                     Hx_ICH_SAH_BL = "History of ICH/SAH",
                     CID_BL = "Chronic inflammatory conditions",
                     Migraine_duration = "Migraine duration, mths",
                     RX_alpha_blocker_BL = "\u03b1-blockers",
                     RX_betaBlocker_BL = "\u03b2-blockers",
                     RX_CCB_BL = "Calcium channel blockers",
                     RX_Central_sympathetic_BL = "Centrally acting sympathetic agonists",
                     RX_Diuretic_potassium_BL = "MRAs",
                     RX_Diuretic_TZD_BL = "Diuretics, TZDs",
                     RX_Diuretics_loop_BL = "Diuretics, loop",
                     RX_Nitrates_BL = "Nitrates",
                     RX_other_ACEIs_ARBs_BL = "Other ACEIs/ARBs",
                     RX_any_ACEIs_ARBs_BL = "Any ACEIs/ARBs",
                     RX_Renin_blocker_BL = "Direct renin inhibitor",
                     RX_Vasodiator_BL = "Vasodilators",
                     RX_Anticoagulant_DOAC_BL = "Anticoagulant, DOACs",
                     RX_Anticoagulant_VKA_BL = "Anticoagulant, warfarin",
                     RX_HRT_BL = "HRT",
                     RX_Mgr_aCGRP_acute_BL = "aCGRP, abortive",
                     RX_Mgr_clonidine_BL = "Clonidine",
                     RX_Mgr_ergotamine_BL = "Ergotamines",
                     RX_Mgr_neurotoxin_BL = "Neurotoxin",
                     RX_Mgr_NMDA_BL = "Memantine",
                     RX_Mgr_otherAnticonvulsant_BL = "Other anticonvulsants",
                     RX_Mgr_SNRI_BL = "SNRI",
                     RX_Mgr_TCA_BL = "TCAs",
                     RX_Mgr_triptan_BL = "Triptans",
                     RX_NSAID_BL = "NSAIDs",
                     RX_aHTN_other_BL = "Other antihypertensives",
                     RX_Diuretic_BL = "Diuretics",
                     RX_aPLT_BL = "Anti-platelets",
                     RX_aCoag_BL = "Anti-coagulants",
                     RX_lipid_BL = "Lipid modifying agents",
                     RX_aPsyc_BL = "Anti-psychotics",
                     HemoStroke_event = "Intracerebral hemorrhage",
                     SAH_event = "Subarachnoid hemorrhage",
                     ICH_SAH_event = "ICH/SAH",
                     Flu_event = "Influenza",
                     Melanoma_event = "Melanoma",
                     AnyPCVisit_1y = "No. primary care visits",
                     AnyPCVisit_1ycat = "No. primary care visits",
                     flushot_1y = "Influena vaccine",
                     Hx_MACE_BL = "History of MACE",
                     RX_SGLT2_BL = "SGLT2i",
                     RX_GLP1_BL = "GLP-1 agonists",
                     HAEDVisit_1y = "No. headache-related ED visits, past 12 months",
                     HAEDVisit_1ycat = "No. headache-related ED visits, past 12 months",
                     HAEDVisit_1yBin = "Headache-related ED visits",
                     HANeuroVisit_1y = "No. headache-related Neurology visits, past 12 months",
                     HANeuroVisit_1ycat = "Headache-related Neurology visits",
                     CORX_aCGRP_BL = "aCGRP, preventive",
                     RX_any_aCGRP_BL = "Any aCGRP")

############################
### set reference levels ###
############################

fun_ref <- function(df) {
  
  df$Arm <- relevel(df$Arm, ref = "TPM")
  df$Gender <- relevel(df$Gender, ref = "Men")
  df$Race <- relevel(df$Race, ref = "White")
  df$Race3cat <- relevel(df$Race3cat, ref = "White")
  df$Ethnicity <- relevel(df$Ethnicity, ref = "Non-Hispanics")
  df$GISURH <- relevel(df$GISURH, ref = "Urban")
  df$ServiceConnectedFlag <- relevel(df$ServiceConnectedFlag, ref = "No")
  df$SMK <- relevel(df$SMK, ref = "Never")
  df$AnyPCVisit_1ycat <- relevel(df$AnyPCVisit_1ycat, ref = "None")
  df$flushot_1y <- relevel(df$flushot_1y, ref = "No")
  df$AFibFlutter_BL <- relevel(df$AFibFlutter_BL, ref = "No")
  df$ARD_BL <- relevel(df$ARD_BL, ref = "No")
  df$CID_BL <- relevel(df$CID_BL, ref = "No")
  df$Cardiomyopathy_BL <- relevel(df$CID_BL, ref = "No")
  df$CKD_BL <- relevel(df$CKD_BL, ref = "No")
  df$COPD_BL <- relevel(df$COPD_BL, ref = "No")
  df$DM_BL <- relevel(df$DM_BL, ref = "No")
  df$DVT_PE_BL <- relevel(df$DVT_PE_BL, ref = "No")
  df$DysLipid_BL <- relevel(df$DysLipid_BL, ref = "No")
  df$HF_BL <- relevel(df$HF_BL, ref = "No")
  df$HTN_BL <- relevel(df$HTN_BL, ref = "No")
  df$MDD_BL <- relevel(df$MDD_BL, ref = "No")
  df$OSA_BL <- relevel(df$OSA_BL, ref = "No")
  df$PAD_BL <- relevel(df$PAD_BL, ref = "No")
  df$SRD_BL <- relevel(df$SRD_BL, ref = "No")
  df$TBI_BL <- relevel(df$TBI_BL, ref = "No")
  df$VHD_BL <- relevel(df$VHD_BL, ref = "No")
  df$Hx_MACE_BL <- relevel(df$Hx_MACE_BL, ref = "No")
  df$Hx_MI_BL <- relevel(df$Hx_MI_BL, ref = "No")
  df$Hx_IschemicStroke_BL <- relevel(df$Hx_IschemicStroke_BL, ref = "No")
  df$Hx_HemoStroke_BL <- relevel(df$Hx_HemoStroke_BL, ref = "No")
  df$Hx_SAH_BL <- relevel(df$Hx_SAH_BL, ref = "No")
  df$RX_any_ACEIs_ARBs_BL <- relevel(df$RX_any_ACEIs_ARBs_BL, ref = "No")
  df$RX_other_ACEIs_ARBs_BL <- relevel(df$RX_other_ACEIs_ARBs_BL, ref = "No")
  df$RX_betaBlocker_BL <- relevel(df$RX_betaBlocker_BL, ref = "No")
  df$RX_CCB_BL <- relevel(df$RX_CCB_BL, ref = "No")
  df$RX_Diuretic_BL <- relevel(df$RX_Diuretic_BL, ref = "No")
  df$RX_Diuretic_potassium_BL <- relevel(df$RX_Diuretic_potassium_BL, ref = "No")
  df$RX_aHTN_other_BL <- relevel(df$RX_aHTN_other_BL, ref = "No")
  df$RX_aCoag_BL <- relevel(df$RX_aCoag_BL, ref = "No")
  df$RX_aPLT_BL <- relevel(df$RX_aPLT_BL, ref = "No")
  df$RX_lipid_BL <- relevel(df$RX_lipid_BL, ref = "No")
  df$RX_HRT_BL <- relevel(df$RX_HRT_BL, ref = "No")
  df$RX_GLP1_BL <- relevel(df$RX_GLP1_BL, ref = "No")
  df$RX_SGLT2_BL <- relevel(df$RX_SGLT2_BL, ref = "No")
  df$RX_aPsyc_BL <- relevel(df$RX_aPsyc_BL, ref = "No")
  df$chronicMig_BL <- relevel(df$chronicMig_BL, ref = "No")
  df$HAEDVisit_1ycat <- relevel(df$HAEDVisit_1ycat, ref = "None")
  df$HANeuroVisit_1ycat <- relevel(df$HANeuroVisit_1ycat, ref = "None")
  df$CORX_aCGRP_BL <- relevel(df$CORX_aCGRP_BL, ref = "No")
  df$RX_any_aCGRP_BL <- relevel(df$RX_any_aCGRP_BL, ref = "No")
  df$RX_Mgr_aCGRP_acute_BL <- relevel(df$RX_Mgr_aCGRP_acute_BL, ref = "No")
  df$RX_Mgr_ergotamine_BL <- relevel(df$RX_Mgr_ergotamine_BL, ref = "No")
  df$RX_Mgr_triptan_BL <- relevel(df$RX_Mgr_triptan_BL, ref = "No")
  df$RX_NSAID_BL <- relevel(df$RX_NSAID_BL, ref = "No")
  df$RX_Mgr_otherAnticonvulsant_BL <- relevel(df$RX_Mgr_otherAnticonvulsant_BL, ref = "No")
  df$RX_Mgr_SNRI_BL <- relevel(df$RX_Mgr_SNRI_BL, ref = "No")
  df$RX_Mgr_TCA_BL <- relevel(df$RX_Mgr_TCA_BL, ref = "No")
  df$RX_Mgr_neurotoxin_BL <- relevel(df$RX_Mgr_neurotoxin_BL, ref = "No")
  
  return(df)
}

#######################################
### baseline cov for LSP/CAN vs.TPM ###
#######################################

cov_LSP_bl <- c("ns(trial_num, df = 5)",
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
                "CKD_BL",
                "COPD_BL",
                "DM_BL",
                "DysLipid_BL",
                "HF_BL",
                "HTN_BL",
                "MDD_BL",
                "OSA_BL",
                "PAD_BL",
                "SRD_BL",
                "TBI_BL",
                "VHD_BL",
                "Hx_MI_BL",
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
                "RX_any_aCGRP_BL", ###
                "RX_Mgr_triptan_BL",
                "RX_NSAID_BL",
                "RX_Mgr_otherAnticonvulsant_BL",
                "RX_Mgr_TCA_BL",
                "RX_Mgr_neurotoxin_BL")

########################################################
### baseline and time-updated cov for LSP/CAN vs.TPM ###
########################################################

cov_LSP_tv <- c("ns(trial_num, df = 5)",
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
                "CKD_BL",
                "COPD_BL",
                "DM_BL",
                "DysLipid_BL",
                "HF_BL",
                "HTN_BL",
                "MDD_BL",
                "OSA_BL",
                "PAD_BL",
                "SRD_BL",
                "TBI_BL",
                "VHD_BL",
                "Hx_MI_BL",
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
                "RX_any_aCGRP_BL",   ###
                "RX_Mgr_triptan_BL",
                "RX_NSAID_BL",
                "RX_Mgr_otherAnticonvulsant_BL",
                "RX_Mgr_TCA_BL",
                "RX_Mgr_neurotoxin_BL",
                # time updated
                "ns(BMI_tv, df = 4)",
                "ns(SBP_tv, df = 3)",
                "ns(DBP_tv, df = 5)",
                "hosp_tv",
                "CKD_TV",
                "HF_TV",
                "HTN_TV",
                "RX_betaBlocker_TV",
                "RX_Diuretic_TV",
                "RX_aPLT_TV",
                "RX_lipid_TV",
                "ChronicMigraine_TV",
                "RX_Mgr_triptan_TV",
                "RX_NSAID_TV")

######################################
### baseline cov for aCGRP vs. TPM ###
######################################

cov_aCGRP_bl <- c("ns(trial_num, df = 5)",
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
                  "CKD_BL",
                  "COPD_BL",
                  "DM_BL",
                  "DysLipid_BL",
                  "HF_BL",
                  "HTN_BL",
                  "MDD_BL",
                  "OSA_BL",
                  "PAD_BL",
                  "SRD_BL",
                  "TBI_BL",
                  "VHD_BL",
                  "Hx_MI_BL", 
                  "Hx_IschemicStroke_BL", 
                  "Hx_ICH_SAH_BL", 
                  "RX_any_ACEIs_ARBs_BL", ###
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
                  "RX_Mgr_triptan_BL",
                  "RX_NSAID_BL",
                  "RX_Mgr_otherAnticonvulsant_BL",
                  "RX_Mgr_TCA_BL",
                  "RX_Mgr_neurotoxin_BL")

#######################################################
### baseline and time-updated cov for aCGRP vs. TPM ###
#######################################################

cov_aCGRP_tv <- c("ns(trial_num, df = 5)",
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
                  "CKD_BL",
                  "COPD_BL",
                  "DM_BL",
                  "DysLipid_BL",
                  "HF_BL",
                  "HTN_BL",
                  "MDD_BL",
                  "OSA_BL",
                  "PAD_BL",
                  "SRD_BL",
                  "TBI_BL",
                  "VHD_BL",
                  "Hx_MI_BL", 
                  "Hx_IschemicStroke_BL", 
                  "Hx_ICH_SAH_BL", 
                  "RX_any_ACEIs_ARBs_BL", ###
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
                  "RX_Mgr_triptan_BL",
                  "RX_NSAID_BL",
                  "RX_Mgr_otherAnticonvulsant_BL",
                  "RX_Mgr_TCA_BL",
                  "RX_Mgr_neurotoxin_BL",
                  # time updated
                  "ns(BMI_tv, df = 4)",
                  "ns(SBP_tv, df = 3)",
                  "ns(DBP_tv, df = 5)",
                  "hosp_tv",
                  "CKD_TV",
                  "HF_TV",
                  "HTN_TV",
                  "RX_ACEIS_ARBs_TV", 
                  "RX_betaBlocker_TV",
                  "RX_Diuretic_TV",
                  "RX_aPLT_TV",
                  "RX_lipid_TV",
                  "ChronicMigraine_TV",
                  "RX_Mgr_triptan_TV",
                  "RX_NSAID_TV")

#####################################################
### calculate treatment initiation weight (IPTiW) ###
#####################################################

fun_IPTiW <- function(df, cov) {
  
  # formula
  f_num <- paste("exp == 1  ~ 1", sep = "")
  f_denom <- paste("exp == 1 ~ ", paste(cov, collapse = " + "), sep = "")
  
  # IPTiW numerator
  p_num <- glm(formula = f_num,
               family = "binomial",
               data = df, x = FALSE, y = FALSE)
  
  # IPTiW denominator
  p_denom <- glm(formula = f_denom,
                 family = "binomial",
                 data = df, x = FALSE, y = FALSE)
  
  # print(summary(p_denom))
  
  df$pn <- predict(p_num, type = "response")
  df$pd <- predict(p_denom, type = "response")
  
  # calculate IPTiW
  df$IPTiW <- ifelse(df$exp == 1, df$pn/df$pd,
                     (1-df$pn)/(1-df$pd))
  
  output <- df %>%
    select(ScrSSN, Arm, exp, startdate, id, pn, pd, IPTiW)
  

  return(output)
  
}


###################################
### intention-to-treat analysis ###
###################################

fun_itt <- function(df = df.lsp,
                    timevar = MACE_itt_time,
                    eventvar = MACE_itt_event,
                    cov = cov_LSP,
                    coxHR = FALSE) {
  
  df <- df %>%
    mutate(id = row_number())
  
  ## get IPTiW
  df.weights <- fun_IPTiW(df = df,
                          cov = cov)
  
  ## transform into person-trial-months
  df.itt <- df %>%
    select(ScrSSN, Arm, exp, startdate, id, {{timevar}}, {{eventvar}}) %>%
    mutate(surv = {{timevar}}) %>%
    expandRows("surv", drop = F) %>%
    mutate(time = sequence(rle(id)$lengths) - 1) %>%
    mutate(event = ifelse((time + 1) == surv & {{eventvar}} == 1, 1, 0)) 

  model_data <- inner_join(df.itt,
                           select(df.weights, id, IPTiW),
                           by = c("id"))
  
  ## overall HRs
  if(coxHR) {
    fit.itt.hr <- glm(event == 1 ~ exp + ns(time, knots = c(6, 12, 24, 48)),
                      family = "quasibinomial", data = model_data,
                      weights = IPTiW)
    
    estimates <- coeftest(fit.itt.hr, vcov = sandwich)
    
    print(estimates)
    print(coefci(fit.itt.hr, vcov = sandwich))
    
    gc()
    
    # return(estimates)
    
  ## cumulative incidence, RD, RR
  }  else {
    
    events <- model_data %>%
      group_by(time, exp) %>%
      summarise(events = sum(event), .groups = "keep") %>%
      ungroup() %>%
      pivot_wider(names_from = exp, values_from = events) %>%
      rename(events_0 = "0",
             events_1 = "1") %>%
      mutate(events_0 = cumsum(events_0),
             events_1 = cumsum(events_1))
    
    fit.itt.risk <- glm(event == 1 ~ exp + ns(time, knots = c(6, 12, 24, 48)) + 
                          exp*ns(time, knots = c(6, 12, 24, 48)),
                        family = "quasibinomial", data = model_data, 
                        weights = IPTiW, x = FALSE, y = FALSE)  
    
    # create shells
    arm_0 <- data.frame(0, seq(0, 59))
    arm_1 <- data.frame(1, seq(0, 59))
    
    colnames(arm_0) <- c("exp", "time")
    colnames(arm_1) <- c("exp", "time")
    
    # 1 - prob
    arm_0$p_0 <- 1 - predict(fit.itt.risk, arm_0, type = "response")
    arm_1$p_1 <- 1 - predict(fit.itt.risk, arm_1, type = "response")
    
    # survival
    arm_0$s_0 <- cumprod(arm_0$p_0)
    arm_1$s_1 <- cumprod(arm_1$p_1)
    
    
    # merge and cum inc
    estimates <- merge(arm_0, arm_1, by = "time") %>%
      merge(events, by = "time") %>%
      mutate(cuminc_0 = 1 - s_0,
             cuminc_1 = 1 - s_1,
             rd = cuminc_1 - cuminc_0,
             logrr = log(cuminc_1/cuminc_0)) %>%
      arrange(time) %>%
      select(time, events_0, events_1, cuminc_0, cuminc_1, rd, logrr)
    
    rm(model_data, events, fit.itt.risk, arm_0, arm_1)
    gc()
    
    return(estimates)
    
  }
}


#############################
### bootstrapping for CIs ###
#############################

# bootstrapping
fun_resample <- function(dat) {dat[sample(1:nrow(dat), nrow(dat), replace = TRUE), ]}

fun_bootstrap <- function(seed, n, df, timevar, eventvar, cov, coxHR) {
  
  set.seed(seed)
  
  replicate(n, fun_resample(df), FALSE) %>%
    map(~ fun_itt(df = .,
                  timevar = {{timevar}},
                  eventvar = {{eventvar}},
                  cov = {{cov}},
                  coxHR = {{coxHR}})) %>%
    bind_rows() %>%
    group_by(time) %>%
    summarise(cuminc_0_lcl = quantile(cuminc_0, probs = 0.025, na.rm = T),
              cuminc_0_ucl = quantile(cuminc_0, probs = 0.975, na.rm = T),
              cuminc_1_lcl = quantile(cuminc_1, probs = 0.025, na.rm = T),
              cuminc_1_ucl = quantile(cuminc_1, probs = 0.975, na.rm = T),
              rd_lcl = quantile(rd, probs = 0.025, na.rm = T),
              rd_ucl = quantile(rd, probs = 0.975, na.rm = T),
              logrr_lcl = quantile(logrr, probs = 0.025, na.rm = T),
              logrr_ucl = quantile(logrr, probs = 0.975, na.rm = T),
              .groups = "keep")
}

#################################
### cumulative incidence plot ###
#################################

fun_cumIncPlot <- function(df, 
                           df_boot,
                           expmed = "LSP/CAN",
                           timevar = flu_itt_time, eventvar = flu_itt_event,
                           expmedcolor = "#E6A555",
                           ymax = 8, ybreaks = 1) {
  
  d0 <- df_boot
  
  data.km <- sqldf("SELECT 0 as exp, time, 100*cuminc_0 as cuminc, 100*cuminc_0_lcl as lcl, 100*cuminc_0_ucl as ucl
                      FROM d0
                 UNION
                 SELECT 1 as exp, time, 100*cuminc_1 as cuminc, 100*cuminc_1_lcl as lcl, 100*cuminc_1_ucl as ucl
                      FROM d0") %>%
    mutate(Arm = factor(case_when(exp == 0 ~ "TPM",
                                  TRUE ~ expmed),
                        levels = c(expmed, "TPM")))
  
  ## borrow at-risk table from ggsurvplot
  surv.formula <- as.formula(paste("Surv(", {{timevar}}, ", ", {{eventvar}}, ") ~ Arm", sep = ""))
  
  survfit <- survfit(surv.formula, data = df)
  survfit$call$formula <- surv.formula
  survfit$call$data <- df
  
  
  survplot <- ggsurvplot(survfit, fun = "event", censor = T,  risk.table = T, risk.table.col = "strata",
               xlim = c(0, 60), break.x.by = 12)

  
  table.atrisk <- survplot$table$data %>%
    mutate(Arm = factor(case_when(Arm == "TPM" ~ "TPM",
                                  TRUE ~ expmed), 
                        levels = c(expmed, "TPM")))
  
  ## ggplot at-risk table
  p.atrisk <- ggplot(table.atrisk, aes(x = time, y = Arm, label = n.risk, colour = Arm)) +
    geom_text(size = 4.5) +
    scale_y_discrete(limits = c("TPM", expmed), labels = c("TPM", expmed)) +
    ylab(" ") +
    xlab(" ") +
    scale_color_manual(values = c(expmedcolor, "#5596E6")) +
    theme_minimal() +
    ggtitle("Num at risk") +
    coord_cartesian(xlim = c(0, 60),
                    clip = "on") +
    theme(panel.grid = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(color = c("#5596E6", expmedcolor), size = 13, face = "bold"),
          legend.position = "none",
          plot.margin = unit(c(-0.5, 1, 0, 0.5), "lines"),
          plot.title = element_text(face = "bold", size = 13, hjust = 0))
  
  ## ggplot cumulative incidence curve
  p.curv <- ggplot(data.km, aes(x = time, y = cuminc, group = Arm)) +
    geom_line(aes(color = Arm), lwd = 1.2) +
    geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = Arm), alpha = 0.3) +
    theme_minimal() +
    scale_color_manual(values = c(expmedcolor, "#5596E6")) +
    scale_fill_manual(values = c(expmedcolor, "#5596E6")) +
    scale_x_continuous(limits = c(0, 60), breaks = seq(0, 60, 12)) +
    scale_y_continuous(limits = c(0, ymax), breaks = seq(0, ymax, ybreaks)) +
    ylab("Adjusted Cumulative Incidence (%)") +
    xlab("Months") +
    coord_cartesian(xlim = c(0, 60),
                    clip = "on") +
    theme(legend.title = element_blank(),
          legend.key.size = unit(1, "cm"),
          legend.position = "none",
          legend.text = element_text(size = 13, face = "bold"),
          axis.text = element_text(size = 13),
          axis.title = element_text(size = 13, face = "bold"))

  # align x-axes with patchwork package
  g <- (p.curv/p.atrisk) + 
    plot_layout(heights = c(1, 0.25)) &
    theme(plot.background = element_rect(fill = "transparent", colour = NA),
          legend.background = element_rect(fill = "transparent"),
          panel.background = element_rect(fill = "transparent", colour = NA))
  
  return(g)
  
}

#####################################
### per-protocol, LSP/CAN vs. TPM ###
#####################################

fun_lsp_perprotocol <- function(datevar = MACE_date,
                                cov_bl = cov_LSP_bl,
                                cov_tv = cov_LSP_tv) {
  
  lsp.pp0 <- left_join(df.lsp, censor.dates, 
                       by = c("ScrSSN", "Arm", "startdate")) %>%
    # event, 60mo, end of fu, LSP/CAN -> TPM/other ACEIs/ARBs, or TPM -> any ACEIs/ARBs
    mutate(pp_date = pmin(enddate,
                          startdate %m+% months(60)-1,
                          {{datevar}}, 
                          Death_date,                 # placeholder for secondary outcomes
                          as.Date("2024-09-30"), 
                          RX_CEN_LSP_DT, 
                          RX_CEN_other_ACEIs_ARBs_DT,
                          RX_CEN_TPM_DT,
                          na.rm = T), 
           pp_event = case_when({{datevar}} == pp_date ~ 1,
                                TRUE ~ 0),
           # adherence defined as event occurance, end of fu by 60 mo or by 9/30/24
           adhere = case_when(pp_event == 1 ~ 1,
                              pp_date == as.Date("2024-09-30") ~ 1,
                              pp_date == startdate %m+% months(60)-1 ~ 1,
                              TRUE ~ 0),
           pp_time = (interval(startdate, pp_date) %/% months(1)) + 1)
  
  # median f/u length
  print(summary(lsp.pp0$pp_time))
  
  # reshape to person-trial-month
  lsp.pp1 <- lsp.pp0 %>%
    expandRows("pp_time", drop = F) %>%
    mutate(time = sequence(rle(id)$lengths) - 1) %>%
    mutate(event = ifelse((time + 1) == pp_time & pp_event == 1, 1, 0),
           adhere = ifelse((time + 1) == pp_time & adhere == 0, 0, 1)) 
  
  # merge with time-updated covs
  df.lsp.pp <- inner_join(lsp.pp1,
                          pp1,
                          by = c("ScrSSN", "Arm", "startdate", "time")) %>%
    mutate(BMI_tv = 703 * Weight_tv / (HEIGHT_BL*HEIGHT_BL)) %>%
    mutate(BMI = case_when(BMI <= quantile(BMI, probs = 0.001, na.rm = TRUE) ~ quantile(BMI, probs = 0.001, na.rm = TRUE),
                           BMI >= quantile(BMI, probs = 0.999, na.rm = TRUE) ~ quantile(BMI, probs = 0.999, na.rm = TRUE),
                           is.na(BMI) ~ median(BMI, na.rm = T),
                           TRUE ~ BMI)) %>%
    mutate(BMI_tv = case_when(BMI_tv <= quantile(BMI_tv, probs = 0.001, na.rm = TRUE) ~ quantile(BMI_tv, probs = 0.001, na.rm = TRUE),
                              BMI_tv >= quantile(BMI_tv, probs = 0.999, na.rm = TRUE) ~ quantile(BMI_tv, probs = 0.999, na.rm = TRUE),
                              is.na(BMI_tv) ~ median(BMI_tv, na.rm = T),
                              TRUE ~ BMI_tv))
  
  # IPTaW models
  f_IPTaW_num <- paste("adhere == 1 ~ exp +", paste({{cov_bl}}, collapse = " + "), sep = "")
  f_IPTaW_denom <- paste("adhere == 1 ~ exp +", paste({{cov_tv}}, collapse = " + "), sep = "")
  
  p_IPTaW_num <- glm(f_IPTaW_num, 
                     data = df.lsp.pp,  family = "binomial", 
                     x = FALSE, y = FALSE)
  
  p_IPTaW_denon <- glm(f_IPTaW_denom, 
                       data = df.lsp.pp,  family = "binomial", 
                       x = FALSE, y = FALSE)
  
  # IPTaW, w/o or w/ trucation
  df.IPTaW <- df.lsp.pp %>%
    mutate(num = predict(p_IPTaW_num, df.lsp.pp, type = "response"),
           denom = predict(p_IPTaW_denon, df.lsp.pp, type = "response")) %>%
    select(ScrSSN, Arm, startdate, id, time, exp, event, adhere, num, denom) %>%
    mutate(numcont = adhere*num + (1 - adhere)*(1 - num),
           numcont = ifelse(time == 0, 1, numcont),
           denomcont = adhere*denom + (1 - adhere)*(1 - denom),
           denomcont = ifelse(time == 0, 1, denomcont)) %>%
    group_by(id) %>%
    arrange(time) %>%
    mutate(k1_0 = cumprod(numcont),
           k1_w = cumprod(denomcont)) %>%
    ungroup() %>%
    mutate(IPTaW = k1_0/k1_w) %>%
    mutate(IPTaW_t = ifelse(IPTaW >= quantile(IPTaW, 0.999), quantile(IPTaW, 0.999),
                            ifelse(IPTaW <= quantile(IPTaW, 0.001), quantile(IPTaW, 0,001), IPTaW))) %>%
    arrange(id, time) %>%
    select(id, time, exp, event, IPTaW, IPTaW_t)
  
  # if large weight, use trucated IPTaW
  print(quantile(df.IPTaW$IPTaW, probs = c(0, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 1)), digits = 6)
  
  
  # get IPTiW 
  df.IPTiW <- fun_IPTiW(df = df.lsp,
                        cov = {{cov_bl}})
  
  # final weight
  df.ipw <- left_join(df.IPTaW,
                      select(df.IPTiW, id, IPTiW),
                      by = "id") %>%
    mutate(ipw = IPTiW * IPTaW)
  
  # overall HRs
  fit.pp.hr <- glm(event == 1 ~ exp + ns(time, knots = c(6, 12, 24, 48)),
                   family = "quasibinomial", data = df.ipw,
                   weights = ipw)
  
  print(coeftest(fit.pp.hr, vcov = sandwich))
  print(coefci(fit.pp.hr, vcov = sandwich))
  
}

###################################
### per-protocol, aCGRP vs. TPM ###
###################################

fun_aCGRP_perprotocol <- function(datevar = MACE_date,
                                  cov_bl = cov_aCGRP_bl,
                                  cov_tv = cov_aCGRP_tv) {
  
  aCGRP.pp0 <- left_join(df.aCGRP, censor.dates, 
                         by = c("ScrSSN", "Arm", "startdate")) %>%
    # event, 60mo, end of fu, aCGRP -> TPM, or TPM -> aCGRP acute/prev
    mutate(pp_date = pmin(enddate,
                          startdate %m+% months(60)-1,
                          {{datevar}}, 
                          Death_date,             # placeholder for secondary outcomes
                          as.Date("2024-09-30"), 
                          RX_CEN_aCGRP_acute_DT, 
                          RX_CEN_aCGRP_Prev_DT,
                          RX_CEN_TPM_DT,
                          na.rm = T), 
           pp_event = case_when({{datevar}} == pp_date ~ 1,
                                TRUE ~ 0),
           # adherence defined as event occurance, end of fu by 60 mo or by 9/30/24
           adhere = case_when(pp_event == 1 ~ 1,
                              pp_date == as.Date("2024-09-30") ~ 1,
                              pp_date == startdate %m+% months(60)-1 ~ 1,
                              TRUE ~ 0),
           pp_time = (interval(startdate, pp_date) %/% months(1)) + 1)
  
  # median f/u length
  print(summary(aCGRP.pp0$pp_time))
  
  # reshape to person-trial-month
  aCGRP.pp1 <- aCGRP.pp0 %>%
    expandRows("pp_time", drop = F) %>%
    mutate(time = sequence(rle(id)$lengths) - 1) %>%
    mutate(event = ifelse((time + 1) == pp_time & pp_event == 1, 1, 0),
           adhere = ifelse((time + 1) == pp_time & adhere == 0, 0, 1)) 

  # merge with time-updated covs
  df.aCGRP.pp <- inner_join(aCGRP.pp1,
                            pp1,
                            by = c("ScrSSN", "Arm", "startdate", "time")) %>%
    mutate(BMI_tv = 703 * Weight_tv / (HEIGHT_BL*HEIGHT_BL)) %>%
    mutate(BMI = case_when(BMI <= quantile(BMI, probs = 0.001, na.rm = TRUE) ~ quantile(BMI, probs = 0.001, na.rm = TRUE),
                           BMI >= quantile(BMI, probs = 0.999, na.rm = TRUE) ~ quantile(BMI, probs = 0.999, na.rm = TRUE),
                           is.na(BMI) ~ median(BMI, na.rm = T),
                           TRUE ~ BMI)) %>%
    mutate(BMI_tv = case_when(BMI_tv <= quantile(BMI_tv, probs = 0.001, na.rm = TRUE) ~ quantile(BMI_tv, probs = 0.001, na.rm = TRUE),
                              BMI_tv >= quantile(BMI_tv, probs = 0.999, na.rm = TRUE) ~ quantile(BMI_tv, probs = 0.999, na.rm = TRUE),
                              is.na(BMI_tv) ~ median(BMI_tv, na.rm = T),
                              TRUE ~ BMI_tv))
  
  # IPTaW models
  f_IPTaW_num <- paste("adhere == 1 ~ exp +", paste({{cov_bl}}, collapse = " + "), sep = "")
  f_IPTaW_denom <- paste("adhere == 1 ~ exp +", paste({{cov_tv}}, collapse = " + "), sep = "")
  
  p_IPTaW_num <- glm(f_IPTaW_num, 
                     data = df.aCGRP.pp,  family = "binomial", 
                     x = FALSE, y = FALSE)
  
  p_IPTaW_denon <- glm(f_IPTaW_denom, 
                       data = df.aCGRP.pp,  family = "binomial", 
                       x = FALSE, y = FALSE)
  
  # IPTaW, w/o or w/ trucation
  df.IPTaW <- df.aCGRP.pp %>%
    mutate(num = predict(p_IPTaW_num, df.aCGRP.pp, type = "response"),
           denom = predict(p_IPTaW_denon, df.aCGRP.pp, type = "response")) %>%
    select(ScrSSN, Arm, startdate, id, time, exp, event, adhere, num, denom) %>%
    mutate(numcont = adhere*num + (1 - adhere)*(1 - num),
           numcont = ifelse(time == 0, 1, numcont),
           denomcont = adhere*denom + (1 - adhere)*(1 - denom),
           denomcont = ifelse(time == 0, 1, denomcont)) %>%
    group_by(id) %>%
    arrange(time) %>%
    mutate(k1_0 = cumprod(numcont),
           k1_w = cumprod(denomcont)) %>%
    ungroup() %>%
    mutate(IPTaW = k1_0/k1_w) %>%
    mutate(IPTaW_t = ifelse(IPTaW >= quantile(IPTaW, 0.999), quantile(IPTaW, 0.999),
                            ifelse(IPTaW <= quantile(IPTaW, 0.001), quantile(IPTaW, 0,001), IPTaW))) %>%
    arrange(id, time) %>%
    select(id, time, exp, event, IPTaW, IPTaW_t)

  # if large weight, use trucated IPTaW
  print(quantile(df.IPTaW$IPTaW, probs = c(0, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 1)), digits = 6)
  
  # get IPTiW 
  df.IPTiW <- fun_IPTiW(df = df.aCGRP,
                        cov = {{cov_bl}})
  
  # final weight
  df.ipw <- left_join(df.IPTaW,
                      select(df.IPTiW, id, IPTiW),
                      by = "id") %>%
    mutate(ipw = IPTiW * IPTaW)
  
  # overall HRs
  fit.pp.hr <- glm(event == 1 ~ exp + ns(time, knots = c(6, 12, 24, 48)),
                   family = "quasibinomial", data = df.ipw,
                   weights = ipw)
  
  print(coeftest(fit.pp.hr, vcov = sandwich))
  print(coefci(fit.pp.hr, vcov = sandwich))
  
}



######################
### competing risk ###
######################

fun_competing <- function(data = df.lsp,
                          event_name = MI_itt_event,
                          event_date = MI_itt_date,
                          event_time = MI_itt_time,
                          cov = cov_LSP_bl) {
  
  # define reference levels
  df <- fun_ref(data)
  
  # create indicate for competing events and max fu time had obs not died
  df <- df.lsp %>%
    mutate(cr_event = case_when({{event_name}} == 1 ~ 1,
                                {{event_name}} == 0 & {{event_date}} == Death_date ~ 2,
                                TRUE ~ 0)) %>%
    mutate(maxfu_date = case_when(cr_event == 1 ~ {{event_date}},
                                  TRUE ~ pmin(as.Date('2024-09-30'), startdate %m+% months(60)-1, na.rm = T)),
           maxfu_time = (interval(startdate, maxfu_date) %/% months(1)) + 1) %>%
    select(ScrSSN, Arm, id, startdate, cr_event, MI_itt_event, MI_itt_date, MI_itt_time, Death_date, maxfu_time, everything())
  
  df.long <- df %>%
    expandRows("maxfu_time", drop = F) %>%
    mutate(time = sequence(rle(id)$lengths) - 1) %>%
    mutate(event = case_when(cr_event == 1 & time + 1 == {{event_time}} ~ 1,
                             time + 1 > {{event_time}} ~ NA,
                             TRUE ~ 0),
           event_death = case_when(cr_event == 2 & time + 1 == {{event_time}} ~ 1,
                                   time + 1 > {{event_time}} ~ NA,
                                   TRUE ~ 0)) %>%
    mutate(fgstatus = if_else(!is.na(event), event, 0))    # after death, event set as 0
  
  # outcome model, death
  fit.death <- glm(event_death == 1 ~ exp + ns(time, knots = c(6, 12, 24, 48)) +
                     ns(trial_num, df = 3) + ns(Age, df = 3) + Gender + Race3cat + Ethnicity + 
                     SMK + ns(BMI, df = 4) + ns(CAN_score_BL, df = 3) + flushot_1y + 
                     CKD_BL + COPD_BL + DM_BL + HF_BL + HTN_BL + MDD_BL + 
                     OSA_BL + PAD_BL + SRD_BL + TBI_BL + VHD_BL + Hx_MACE_BL + RX_betaBlocker_BL +
                     RX_Diuretic_BL + RX_aCoag_BL + RX_aPLT_BL + RX_lipid_BL + RX_SGLT2_BL + RX_NSAID_BL,
                   data = df.long, family = "binomial")
  
  # outcome model, event of interest (death treated as censor)
  f_event <- paste("event == 1 ~ ", paste(c("exp", "ns(time, knots = c(6, 12, 24, 48))", cov), collapse = " + "), sep = "")
  
  fit.event <- glm(formula = f_event,
                   family = "binomial",
                   data = df.long, x = FALSE, y = FALSE)
  
  df.long <- df.long %>%
    mutate(pdeath = predict(fit.death, newdata = df.long, type = "response"),
           pevent = predict(fit.event, newdata = df.long, type = "response"),
           ps = case_when(!is.na(event_MI) ~ 1,
                          TRUE ~ (1-pdeath)*(1-pevent))) %>%
    arrange(id, time) %>%
    group_by(id) %>%
    mutate(wt = cumprod(ps)) %>%
    ungroup()
  
  # competing risk model with wt
  f_cr <- paste("fgstatus == 1 ~ ", paste(c("exp", "ns(time, knots = c(6, 12, 24, 48))", cov), collapse = " + "), sep = "")
  
  fit.cr <- glm(formula = f_cr,
                family = "binomial",
                data = df.long, weights = wt)
  
  coef <- summary(fit.cr)$coefficient
  ci <- coefci(fit.cr, vcov = sandwich)
  
  
  OR <- data.frame(var = rownames(coef),
                   OR = round(exp(coef[, 1]), 2),
                   lcl = round(exp(ci[, 1]), 2),
                   ucl = round(exp(ci[, 2]), 2))
  
  rownames(OR) <- NULL 
  
  return(OR)
  
}


##################################################################
### Subgroup analyses, LSP/CAN, overall and w/o CV indications ###
##################################################################

fun_lsp_subgroup <- function(df,
                             subvar,
                             cat,
                             timevar,
                             eventvar,
                             coxHR = FALSE) {
  
  df.itt <- df %>%
    filter({{subvar}} == cat) %>%
    select(ScrSSN, Arm, exp, startdate, id, IPTiW, {{timevar}}, {{eventvar}}) %>%
    mutate(surv = {{timevar}}) %>%
    expandRows("surv", drop = F) %>%
    mutate(time = sequence(rle(id)$lengths) - 1) %>%
    mutate(event = ifelse((time + 1) == surv & {{eventvar}} == 1, 1, 0)) 
  
  if(coxHR) {
    fit.itt.hr <- glm(event == 1 ~ exp + ns(time, knots = c(6, 12, 24, 48)),
                      family = "quasibinomial", data = df.itt,
                      weights = IPTiW)
    
    estimates <- coeftest(fit.itt.hr, vcov = sandwich)
    
    print(estimates)
    print(coefci(fit.itt.hr, vcov = sandwich))
    
    gc()
    
    return(estimates)
    
    
  }  else {
    
    events <- df.itt %>%
      group_by(time, exp) %>%
      summarise(events = sum(event), .groups = "keep") %>%
      ungroup() %>%
      pivot_wider(names_from = exp, values_from = events) %>%
      rename(events_0 = "0",
             events_1 = "1") %>%
      mutate(events_0 = cumsum(events_0),
             events_1 = cumsum(events_1))
    
    fit.itt.risk <- glm(event == 1 ~ exp + ns(time, knots = c(6, 12, 24, 48)) + 
                          exp*ns(time, knots = c(6, 12, 24, 48)),
                        family = "quasibinomial", data = df.itt, 
                        weights = IPTiW, x = FALSE, y = FALSE)  
    
    # create shells
    arm_0 <- data.frame(0, seq(0, 59))
    arm_1 <- data.frame(1, seq(0, 59))
    
    colnames(arm_0) <- c("exp", "time")
    colnames(arm_1) <- c("exp", "time")
    
    # 1 - prob
    arm_0$p_0 <- 1 - predict(fit.itt.risk, arm_0, type = "response")
    arm_1$p_1 <- 1 - predict(fit.itt.risk, arm_1, type = "response")
    
    # survival
    arm_0$s_0 <- cumprod(arm_0$p_0)
    arm_1$s_1 <- cumprod(arm_1$p_1)
    
    
    # merge and cum inc
    estimates <- merge(arm_0, arm_1, by = "time") %>%
      merge(events, by = "time") %>%
      mutate(cuminc_0 = 1 - s_0,
             cuminc_1 = 1 - s_1,
             rd = cuminc_1 - cuminc_0,
             logrr = log(cuminc_1/cuminc_0)) %>%
      arrange(time) %>%
      select(time, events_0, events_1, cuminc_0, cuminc_1, rd, logrr)
    
    rm(df.itt, events, fit.itt.risk, arm_0, arm_1)
    gc()
    
    return(estimates)
    
  }
}





# set-up
library(tidyverse)
library(splines)
library(splitstackshape)
library(survival)
library(sandwich)
library(lme4)
library(lmerTest)
library(emmeans)

source("utilities.R")


# 1.load dataset-------------------------------------------------------------------

# LSP/CAN
df.lsp <- readRDS("df_lsp.rds") %>%
  mutate(SBP_BL = ifelse(is.na(SBP_BL), median(SBP_BL, na.rm = T), SBP_BL),
         DBP_BL = ifelse(is.na(DBP_BL), median(DBP_BL, na.rm = T), DBP_BL),
         BMI = ifelse(is.na(BMI), median(BMI, na.rm = T), BMI),
         CAN_score_BL = ifelse(is.na(CAN_score_BL), median(CAN_score_BL, na.rm = T), CAN_score_BL)) %>%
  mutate(exp = case_when(Arm == "LSP/CDS" ~ 1,
                         TRUE ~ 0)) %>%
  mutate(id = row_number())
df.lsp <- fun_ref(df.lsp)

# aCGRP
df.aCGRP <- readRDS("df_aCGRP.rds") %>%
  mutate(SBP_BL = ifelse(is.na(SBP_BL), median(SBP_BL, na.rm = T), SBP_BL),
         DBP_BL = ifelse(is.na(DBP_BL), median(DBP_BL, na.rm = T), DBP_BL),
         BMI = ifelse(is.na(BMI), median(BMI, na.rm = T), BMI),
         CAN_score_BL = ifelse(is.na(CAN_score_BL), median(CAN_score_BL, na.rm = T), CAN_score_BL)) %>%
  mutate(exp = case_when(Arm == "aCGRP" ~ 1,
                         TRUE ~ 0)) %>%
  mutate(id = row_number())
df.aCGRP <- fun_ref(df.aCGRP)


# 2.1.Negative outcomes, overall HRs--------------------------------------------------

fun_itt(df.lsp, timevar = flu_itt_time, eventvar = flu_itt_event, cov = cov_LSP_bl, coxHR = TRUE)
fun_itt(df.lsp, timevar = Fracture_itt_time, eventvar = Fracture_itt_event, cov = cov_LSP_bl, coxHR = TRUE)


fun_itt(df.aCGRP, timevar = flu_itt_time, eventvar = flu_itt_event, cov = cov_aCGRP_bl, coxHR = TRUE)
fun_itt(df.aCGRP, timevar = Fracture_itt_time, eventvar = Fracture_itt_event, cov = cov_aCGRP_bl, coxHR = TRUE)

# 2.2.Negative outcomes, RD/RR---------------------------------------------------------

# LSP/CAN. flu
est.LSP.flu <- fun_itt(df.lsp, timevar = flu_itt_time, eventvar = flu_itt_event, cov = cov_LSP_bl, coxHR = FALSE)
est.LSP.flu.ci <- fun_bootstrap(seed = 7, n = 500,
                                df = df.lsp, timevar = flu_itt_time, eventvar = flu_itt_event,
                                cov = cov_LSP_bl, coxHR = FALSE)

merge.LSP.flu <- inner_join(est.LSP.flu, est.LSP.flu.ci, by = c("time"))

# LSP/CAN, fracutre
est.LSP.Fracture <- fun_itt(df.lsp, timevar = Fracture_itt_time, eventvar = Fracture_itt_event, cov = cov_LSP_bl, coxHR = FALSE)
est.LSP.Fracture.ci <- fun_bootstrap(seed = 7, n = 500,
                                     df = df.lsp, timevar = Fracture_itt_time, eventvar = Fracture_itt_event,
                                     cov = cov_LSP_bl, coxHR = FALSE)

merge.LSP.Fracture <- inner_join(est.LSP.Fracture, est.LSP.Fracture.ci, by = c("time"))

# aCGRP, flu
est.aCGRP.flu <- fun_itt(df.aCGRP, timevar = flu_itt_time, eventvar = flu_itt_event, cov = cov_aCGRP_bl, coxHR = FALSE)
est.aCGRP.flu.ci <- fun_bootstrap(seed = 7, n = 500,
                                  df = df.aCGRP, timevar = flu_itt_time, eventvar = flu_itt_event,
                                  cov = cov_aCGRP_bl, coxHR = FALSE)

merge.aCGRP.flu <- inner_join(est.aCGRP.flu, est.aCGRP.flu.ci, by = c("time"))

# aCGRP, fracture
est.aCGRP.fracture <- fun_itt(df.aCGRP, timevar = Fracture_itt_time, eventvar = Fracture_itt_event, cov = cov_aCGRP_bl, coxHR = FALSE)
est.aCGRP.fracture.ci <- fun_bootstrap(seed = 7, n = 500,
                                       df = df.aCGRP, timevar = Fracture_itt_time, eventvar = Fracture_itt_event,
                                       cov = cov_aCGRP_bl, coxHR = FALSE)

merge.aCGRP.fracture <- inner_join(est.aCGRP.fracture, est.aCGRP.fracture.ci, by = c("time"))


rm(est.LSP.flu, est.LSP.flu.ci)
rm(est.LSP.Fracture, est.LSP.Fracture.ci)
rm(est.aCGRP.flu, est.aCGRP.flu.ci)
rm(est.aCGRP.Fracture, est.aCGRP.fracture.ci)
rm(est.aCGRP.fracture, est.aCGRP.fracture.ci)


# 3.Positive outcomes, Malignancy---------------------------------------------------------

df.lsp.ca <- df.lsp %>%
  filter(startdate < Malignancy_itt_date)

df.aCGRP.ca <- df.aCGRP %>%
  filter(startdate < Malignancy_itt_date)

# overall HR
fun_itt(df.lsp.ca, timevar = Malignancy_itt_time, eventvar = Malignancy_itt_event, cov = cov_LSP_bl, coxHR = TRUE)
fun_itt(df.aCGRP.ca, timevar = Malignancy_itt_time, eventvar = Malignancy_itt_event, cov = cov_aCGRP_bl, coxHR = TRUE)


# RD/RR, LSP
est.LSP.malignancy <- fun_itt(df.lsp.ca, timevar = Malignancy_itt_time, eventvar = Malignancy_itt_event, 
                              cov = cov_LSP_bl, coxHR = FALSE)
est.LSP.malignancy.ci <- fun_bootstrap(seed = 7, n = 500,
                                       df = df.lsp.ca, timevar = Malignancy_itt_time, eventvar = Malignancy_itt_event,
                                       cov = cov_LSP_bl, coxHR = FALSE)

merge.LSP.malignancy <- inner_join(est.LSP.malignancy, est.LSP.malignancy.ci, by = c("time"))


# RD/RR, aCGRP
est.aCGRP.malignancy <- fun_itt(df.aCGRP.ca, timevar = Malignancy_itt_time, eventvar = Malignancy_itt_event, 
                                cov = cov_aCGRP_bl, coxHR = FALSE)
est.aCGRP.malignancy.ci <- fun_bootstrap(seed = 7, n = 500,
                                         df = df.aCGRP.ca, timevar = Malignancy_itt_time, eventvar = Malignancy_itt_event,
                                         cov = cov_aCGRP_bl, coxHR = FALSE)

merge.aCGRP.malignancy <- inner_join(est.aCGRP.malignancy, est.aCGRP.malignancy.ci, by = c("time"))


rm(est.LSP.malignancy, est.LSP.malignancy.ci)
rm(est.aCGRP.malignancy, est.aCGRP.malignancy.ci)


# 4.Positive outcomes, Weight change ---------------------------------------------------------
# load all weight measures
w0 <- readRDS("raw_weight.rds") %>%
  filter(!is.na(Weight_BL)) %>% 
  mutate(weight_time = as.numeric(WeightLossDate - startdate)) %>%
  mutate(diff_kg = diff/2.205)

w.pt <- w0 %>%
  distinct(., ScrSSN, startdate, .keep_all = FALSE)


###### LSP/CAN ######
df.lsp.w <- inner_join(df.lsp,
             w.pt,
             by = c("ScrSSN", "startdate"))  

# IPTiW
ipw.lsp <- fun_IPTiW(df.lsp, cov_LSP_bl)

# merge weight in kg and IPTiW
df.weight.LSP <- inner_join(select(w0, ScrSSN, Arm, startdate, Weight_BL, Weight, WeightLossDate, diff_kg, weight_time),
                            df.lsp.w,
                            by = c("ScrSSN", "Arm", "startdate")) %>%
  arrange(ScrSSN, startdate, WeightLossDate) %>%
  inner_join(.,
             select(ipw.lsp, id, IPTiW),
             by = "id")

fit.lsp <- lmer(formula = diff_kg ~ Arm + ns(weight_time, 3) + Arm*ns(weight_time, 3) + Weight_BL.x + (1|ScrSSN),
                data = df.weight.LSP, weights = IPTiW)

# summary(fit.lsp)

# estimate weight change at 1y
rg.LSP <- ref_grid(fit.lsp, 
                   at = list(trt_group = c("LSP/CAN", "TPM"),
                             weight_time = c(365)))

emmeans(rg.LSP, ~ Arm*weight_time)
confint(pairs(emmeans(rg.LSP, ~ Arm*weight_time)))


###### aCGRP ######
df.aCGRP.w <- inner_join(df.aCGRP,
                         w.pt,
                         by = c("ScrSSN", "startdate"))  

# IPTiW
ipw.aCGRP <- fun_IPTiW(df.aCGRP, cov_LSP_bl)

# merge weight in kg and IPTiW
df.weight.aCGRP <- inner_join(select(w0, ScrSSN, Arm, startdate, Weight_BL, Weight, WeightLossDate, diff_kg, weight_time),
                              df.aCGRP,
                              by = c("ScrSSN", "Arm", "startdate")) %>%
  arrange(ScrSSN, startdate, WeightLossDate) %>%
  inner_join(.,
             select(ipw.aCGRP, id, IPTiW),
             by = "id")

fit.aCGRP <- lmer(formula = diff_kg ~ Arm + ns(weight_time, 3) + Arm*ns(weight_time, 3) + Weight_BL.x + (1|ScrSSN),
                  data = df.weight.aCGRP, weights = IPTiW)

summary(fit.aCGRP)

emm_options(opt.digits= F, 
            lmerTest.limit = 486447,
            pbkrtest.limit = 486447)

rg.aCGRP <- ref_grid(fit.aCGRP,
                     at = list(trt_group = c("aCGRP", "TPM"),
                               weight_time = c(365)))

emmeans(rg.aCGRP, ~ Arm*weight_time)
confint(pairs(emmeans(rg.aCGRP, ~ Arm*weight_time)))



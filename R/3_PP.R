
# set-up

library(tidyverse)
library(splitstackshape)
library(splines)
library(lmtest)
library(sandwich)

source("utilities.R")

# person-trial-month dataset for time-updated covs
pp1 <- readRDS("pp.rds")

# censor when deviate from treatment
censor.dates <- readRDS("RX_CensorDT.rds")

# person-trial dataset
df.lsp <- readRDS("df_lsp.rds") %>%
  mutate(SBP_BL = ifelse(is.na(SBP_BL), median(SBP_BL, na.rm = T), SBP_BL),
         DBP_BL = ifelse(is.na(DBP_BL), median(DBP_BL, na.rm = T), DBP_BL),
         BMI = ifelse(is.na(BMI), median(BMI, na.rm = T), BMI),
         CAN_score_BL = ifelse(is.na(CAN_score_BL), median(CAN_score_BL, na.rm = T), CAN_score_BL)) %>%
  mutate(exp = case_when(Arm == "LSP/CDS" ~ 1,
                         TRUE ~ 0)) %>%
  mutate(id = row_number())

df.aCGRP <- readRDS("df_aCGRP.rds") %>%
  mutate(SBP_BL = ifelse(is.na(SBP_BL), median(SBP_BL, na.rm = T), SBP_BL),
         DBP_BL = ifelse(is.na(DBP_BL), median(DBP_BL, na.rm = T), DBP_BL),
         BMI = ifelse(is.na(BMI), median(BMI, na.rm = T), BMI),
         CAN_score_BL = ifelse(is.na(CAN_score_BL), median(CAN_score_BL, na.rm = T), CAN_score_BL)) %>%
  mutate(exp = case_when(Arm == "aCGRP" ~ 1,
                         TRUE ~ 0)) %>%
  mutate(id = row_number())


# per-protocol, LSP/CAN vs. TPM
fun_lsp_perprotocol(MACE_date, cov_LSP_bl, cov_LSP_tv)      ## 9 mins
fun_lsp_perprotocol(MI_date, cov_LSP_bl, cov_LSP_tv)
fun_lsp_perprotocol(IschemicStroke_date, cov_LSP_bl, cov_LSP_tv)
fun_lsp_perprotocol(ICH_SAH_date, cov_LSP_bl, cov_LSP_tv)


# per-protocol, aCGRP vs. TPM
fun_aCGRP_perprotocol(MACE_date, cov_aCGRP_bl, cov_aCGRP_tv)  ## 14.4 mins, 5d for 500 boots
fun_aCGRP_perprotocol(MI_date, cov_aCGRP_bl, cov_aCGRP_tv)
fun_aCGRP_perprotocol(IschemicStroke_date, cov_aCGRP_bl, cov_aCGRP_tv)
fun_aCGRP_perprotocol(ICH_SAH_date, cov_aCGRP_bl, cov_aCGRP_tv)



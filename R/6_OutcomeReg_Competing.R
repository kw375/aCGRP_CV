

# set-up
library(tidyverse)
library(splines)
library(splitstackshape)
library(survival)
library(lmtest)
library(sandwich)

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


# 2.1. Outcome Regression, LSP/CAN, MACE--------------------------------------------
df.lsp.long <- df.lsp %>%
  expandRows("MACE_itt_time", drop = F) %>%
  mutate(time = sequence(rle(id)$lengths) - 1) %>%
  mutate(event = ifelse((time + 1) == MACE_itt_time & MACE_itt_event == 1, 1, 0)) 

f_event <- paste("event == 1 ~ ", paste(c("exp", "ns(time, knots = c(6, 12, 24, 48))", cov_lsp_bl), collapse = " + "), sep = "")

fit.lsp <- glm(formula = f_event, 
               family = "binomial", data = df.lsp.long, x = FALSE, y = FALSE)


coef <- summary(fit.lsp)$coefficient
ci <- coefci(fit.lsp, vcov = sandwich)


OR <- data.frame(var = rownames(coef),
                 OR = round(exp(coef[, 1]), 2),
                 lcl = round(exp(ci[, 1]), 2),
                 ucl = round(exp(ci[, 2]), 2))

rownames(OR) <- NULL 
view(OR)

rm(df.lsp.long, f_event, fit.lsp, coef, ci, OR)

# 2.2. Outcome Regression, aCGRP, MACE--------------------------------------------
df.aCGRP.long <- df.aCGRP %>%
  expandRows("MACE_itt_time", drop = F) %>%
  mutate(time = sequence(rle(id)$lengths) - 1) %>%
  mutate(event = ifelse((time + 1) == MACE_itt_time & MACE_itt_event == 1, 1, 0)) 

f_event <- paste("event == 1 ~ ", paste(c("exp", "ns(time, knots = c(6, 12, 24, 48))", cov_aCGRP_bl), collapse = " + "), sep = "")


fit.aCGRP <- glm(formula = f_event, 
                 family = "binomial", data = df.long, x = FALSE, y = FALSE)


coef <- summary(fit.aCGRP)$coefficient
ci <- coefci(fit.aCGRP, vcov = sandwich)


OR <- data.frame(var = rownames(coef),
                 OR = round(exp(coef[, 1]), 2),
                 lcl = round(exp(ci[, 1]), 2),
                 ucl = round(exp(ci[, 2]), 2))

rownames(OR) <- NULL 
view(OR)

rm(df.aCGRP.long, f_event, fit.aCGRP, coef, ci, OR)


# 3.1. Secondary outcomes, competing risk, LSP/CAN--------------------------------------------

OR.LSP.MI <- fun_competing(data = df.lsp, event_name = MI_itt_event, 
                           event_date = MI_itt_date, event_time = MI_itt_time,
                           cov = cov_LSP_bl)

OR.LSP.IschemicStroke <- fun_competing(data = df.lsp, event_name = IschemicStroke_itt_event, 
                                       event_date = IschemicStroke_itt_date,  event_time = IschemicStroke_itt_time,
                                       cov = cov_LSP_bl)

write.table(OR.LSP.MI,
            file = paste("Competing_LSP_MI_", Sys.Date(), ".txt", sep = ""),
            sep = "\t", quote = F, row.names = T, col.names = NA)
write.table(OR.LSP.IschemicStroke,
            file = paste("Competing_LSP_IschemicStroke_", Sys.Date(), ".txt", sep = ""),
            sep = "\t", quote = F, row.names = T, col.names = NA)


# 3.2. Secondary outcomes, competing risk, aCGRP--------------------------------------------
OR.aCGRP.MI <- fun_competing(data = df.aCGRP, event_name = MI_itt_event, 
                             event_date = MI_itt_date, event_time = MI_itt_time,
                             cov = cov_aCGRP_bl)

OR.aCGRP.IschemicStroke <- fun_competing(data = df.aCGRP, event_name = IschemicStroke_itt_event, 
                                         event_date = IschemicStroke_itt_date,  event_time = IschemicStroke_itt_time,
                                         cov = cov_aCGRP_bl)


write.table(OR.aCGRP.MI,
            file = paste("Competing_aCGRP_MI_", Sys.Date(), ".txt", sep = ""),
            sep = "\t", quote = F, row.names = T, col.names = NA)
write.table(OR.aCGRP.IschemicStroke,
            file = paste("Competing_aCGRP_IschemicStroke_", Sys.Date(), ".txt", sep = ""),
            sep = "\t", quote = F, row.names = T, col.names = NA)


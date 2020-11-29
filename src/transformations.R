library(knitr)
library(xlsx)
library(lubridate)
library(dplyr)
library(tidyr)
library(arules)
library(DT)
library(kableExtra)
library(plotly)
library(ggplot2)
library(corrplot)
library(codebook)

df <- df_raw %>% 
  filter(!is.na(RE_DATE)) %>%
  fill(PATIENT_ID)

df <- df %>%
  mutate(gender = factor(gender, labels = c('male', 'female')),
         outcome = factor(outcome, labels = c('survived', 'deceased')))

df <- df %>%
  rename("hs.cTnI" = "Hypersensitive.cardiac.troponinI") %>%
  rename("PT" = "Prothrombin.time") %>%
  rename("IL.2.receptor" = "Interleukin.2.receptor") %>%
  rename("ALP" = "Alkaline.phosphatase") %>%
  rename("IL.10" = "Interleukin.10") %>%
  rename("AT" = "antithrombin") %>%
  rename("IL.8" = "Interleukin.8") %>%
  rename("RDW" = "Red.blood.cell.distribution.width.") %>%
  rename("q.anti.TP" = "Quantification.of.Treponema.pallidum.antibodies") %>%
  rename("MCV" = "mean.corpuscular.volume") %>%
  rename("WBC.count" = "White.blood.cell.count") %>%
  rename("TNF.alpha" = "Tumor.necrosis.factorα") %>%
  rename("MCHC" = "mean.corpuscular.hemoglobin.concentration") %>%
  rename("IL.1.beta" = "Interleukin.1β") %>%
  rename("RBC.count" = "Red.blood.cell.count") %>%
  rename("MPV" = "Mean.platelet.volume") %>%
  rename("RBC.DW.SD" = "RBC.distribution.width.SD") %>%
  rename("TT" = "Thrombin.time") %>%
  rename("q.anti.HCV" = "HCV.antibody.quantification") %>%
  rename("D.Dimer" = "D.D.dimer") %>%
  rename("AST" = "aspartate.aminotransferase") %>%
  rename("NT.proBNP" = "Amino.terminal.brain.natriuretic.peptide.precursor.NT.proBNP.") %>%
  rename("LDH" = "Lactate.dehydrogenase") %>%
  rename("P.LCR" = "platelet.large.cell.ratio.") %>%
  rename("IL.6" = "Interleukin.6") %>%
  rename("FDP" = "Fibrin.degradation.products") %>%
  rename("PDW" = "PLT.distribution.width") %>%
  rename("GGT" = "γ.glutamyl.transpeptidase") %>%
  rename("INR" = "International.standard.ratio") %>%
  rename("X2019.nCoV.NAT" = "X2019.nCoV.nucleic.acid.detection") %>%
  rename("MCH" = "mean.corpuscular.hemoglobin.") %>%
  rename("aPTT" = "Activation.of.partial.thromboplastin.time") %>%
  rename("hs.CRP" = "High.sensitivity.C.reactive.protein") %>%
  rename("q.anti.HIV" = "HIV.antibody.quantification") %>%
  rename("ALAT" = "glutamic.pyruvic.transaminase") %>%
  rename_with(~gsub('_', '.', .)) %>%
  rename_with(~gsub('\\.{3}$', '.percent', .)) %>%
  rename_with(~gsub('^X\\.{3}|\\.$', '', .)) %>%
  rename_with(tolower, matches("^[[:upper:]]{1}[[:lower:]]+|^[[:upper:]]{2,}\\.{1}[[:upper:]]{2,}$", ignore.case = FALSE))

df <- df %>% select(-c(colnames(df)[apply(df, 2, function(x) any(x == -1, na.rm = TRUE))]))

df_clean <- df %>%
  mutate(re.date = floor_date(re.date, unit = "day")) %>%
  group_by(patient.id, re.date) %>%
  summarize(across(age:outcome, last), across(hs.cTnI:creatinine, ~.[last(which(!is.na(.)))])) %>%
  ungroup()

ids <- df_clean %>%
  group_by(patient.id) %>%
  summarize(across(hs.cTnI:creatinine, ~any(!is.na(.)))) %>%
  summarize(patient.id = patient.id, n = rowSums(select(., c(hs.cTnI:creatinine)))) %>%
  ungroup() %>%
  filter(n > ncol(select(df_clean, c(hs.cTnI:creatinine))) * 0.5)

df_clean <- df_clean %>%
  filter(patient.id %in% ids$patient.id)

rm("ids")
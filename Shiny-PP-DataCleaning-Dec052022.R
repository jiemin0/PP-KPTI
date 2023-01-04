#Date: Nov-16-2022
# contents: Prepare data for Patient's Profile:
#  Exposure, AE
# plotting population: safety popualtion

# 0 preparing load packages and df---------------------------------------------
# 
# library(tidyverse)
# library(clipr)
# library(haven)
# library(magrittr)
# library(lubridate)
# library(Hmisc)
# path <-  "C:/Users/Jiemin.yang/OneDrive - Karyopharm Therapeutics/Documents/Work/Project/035/11-01-2022"
# df_lb <- read_sas(paste0(path,'/SDTM/lb.sas7bdat'))
# 
# setwd(path)
# df_ex <- read_sas(paste0(path,'/SDTM/ex.sas7bdat'))
# df_dm <- read_sas(paste0(path,'/SDTM/dm.sas7bdat'))
# df_ae <- read_sas(paste0(path,'/SDTM/ae.sas7bdat'))
# df_ds <- read_sas(paste0(path,'/SDTM/ds.sas7bdat'))
# df_fa <- read_sas(paste0(path,'/SDTM/fa.sas7bdat'))
# df_dd <- read_sas(paste0(path,'/SDTM/dd.sas7bdat'))
# df_suppfa <- read_sas(paste0(path,'SDTM/suppfa.sas7bdat'))

# compute analysis population

compute_safety_pp <- function(df_dm, df_ex){
  df_dm %>%
    filter(! is.na(ACTARM)) %>%
    distinct(USUBJID) %>%
    inner_join(df_ex, by = "USUBJID")  %>% 
    filter(EXDOSE != 0) %>%
    group_by(USUBJID) %>%
    summarise(
      TRTSTDY = min(EXSTDY, na.rm = TRUE), 
      TRTENDY = max(EXSTDY, na.rm = TRUE), 
      TRTSTDC = min(as.Date(EXSTDTC), na.rm = TRUE),
      TRTENDC = max(as.Date(EXENDTC), na.rm = TRUE)
    )-> safety_pp
  
  return(safety_pp)
}

#safety_pp <- compute_safety_pp(df_dm, df_ex)

#1 Exposure----------------------------------------------

# process Exposure of Selinexor --------------------------------
# EX compute algorithms:
#' the dosing start date is defined as the first no-zero dose. 
#' Missing value
#'  if ex start date missing, exclude
#'  for PC's arm, if start date or end date missing, exclude. 
#' Ex regimen was assumed to last for 7 days for each dose.


process_ex <- function(df_ex){
  df_ex %>% 
    # filter pt with missing start date
    filter(EXSTDTC != "" ) %>%
    filter(EXTRT == "SELINEXOR"  ) %>% 
    mutate(
      EXSTDTC = as.Date(EXSTDTC),
      EXENDTC = as.Date(EXENDTC),
      #
      EXRGM = gsub('[Mm][Gg].*', '', EXDOSRGM),
      EXRGM = factor(as.numeric(EXRGM)),
      EXDOSE = factor(EXDOSE)
    ) -> ex

  
  #Regimen duration, if EXADJ != ""., then dose adjusted, start with an new regimen
  ex %<>% 
    group_by(USUBJID) %>% 
    arrange(EXSTDTC) %>%  
    mutate(EXMODSEQ = cumsum(EXADJ != "")) %>% 
    ungroup 
  
  ex_rgm =  ex %>% 
    group_by(USUBJID, EXMODSEQ) %>% 
    summarise(
      RGMSTDY = min(EXSTDY), 
      # max only count for starting date, +7 to make up the gap
      RGMENDY = max(EXENDY) +7, 
      EXRGM = EXRGM[1]
    ) %>%
    mutate(
      # compute label location for regimen
      label_x = (RGMSTDY - RGMENDY)/2 + RGMENDY
    )
  exdfs <- list(ex, ex_rgm)
  return(exdfs)
  
}

make_ex_listing <- function(df_ex, df_dm, pid){
  df_dm %>%
    filter(ACTARM != "") %>%
    select(USUBJID, ACTARM) -> df_arm
  
  df_ex %>%
    left_join(df_arm, by = "USUBJID") %>%
    filter(USUBJID == pid) %>%
    select(
      USUBJID,
      ACTARM,
      EXCAT,
      EXDOSE,
      EXDOSU,
      EXSTDTC,
      EXSTDY,
      EXENDTC,
      EXENDY,
      EXDOSFRM,
      EXROUTE,
      EXADJ
    ) %>%
    mutate(
      EXSTDTC = as.Date(EXSTDTC),
      EXENDTC = as.Date(EXENDTC)
    ) %>%
    rename(
      `Patient Identifier` =  USUBJID,
      Arm = ACTARM,
      Treatment = EXCAT,
      Dose = EXDOSE,
      `Dose Unit` = EXDOSU,
      `Treatment Start Date` = EXSTDTC,
      `Start Day` = EXSTDY,
      `Treatment End Date` = EXENDTC,
      `End Day` = EXENDY,
      `Dose Form` = EXDOSFRM,
      `Dose Route` = EXROUTE,
      `Reason for Action Taken` =  EXADJ
    ) -> ex_listing
  return(ex_listing)
  
}

#t <- make_ex_listing(df_ex, df_dm, "XPORT-MF-035-1200-001" )



# AE------------------------------------------------------------------------------
# AE Alg:
#   Pt's population: safety population 
#   missing value
#     exclude pt missing AESTDATE, 
#     if AEENDTC is missing, enddate = TRT end date + 30,  
#     if ex end date is also missing, end date = data cut date

#? update AESTDY2 and AEENDY2
process_ae <- function(df_ae, pt_info, cut_off_d){
  
  df_ae %>%
    # exclude ae happended before study date
    filter(AESTDY > 0) %>%
    inner_join(pt_info, by = "USUBJID") %>% 
    rowwise() %>%
    mutate(
      AESTDTC = as.Date(AESTDTC),
      cutoffdc = as.Date(cut_off_d),
      # if AE end date missing and AESTDTC on or before trt end date + 30, fill in 
      #with the earlist of data cutoff date, last dosing date + 30, death date
      AEENDTC2 = if_else(
        is.na(AEENDY) & AESTDTC <= TRTENDC + 30,
        min(DDDTC, TRTENDC + 30, cutoffdc, na.rm = TRUE),
        # if AE end date missing and AESTDTC after trt end date + 30, fill in with t
        #he earlist of data cutoff date, death date
        if_else(
          is.na(AEENDY) & AESTDTC > TRTENDC + 30,
          min(DDDTC, cutoffdc, na.rm = TRUE),
          as.Date(AEENDTC)
        )
      )) %>%
    ungroup() %>%
    mutate(
      AEENDY2 = as.numeric(as.Date(AEENDTC2) - as.Date(TRTSTDC) + 1),
      AETOXGR = as.factor(paste0("G", AETOXGR)),
      # x axis for text of dose modification
      AEACN2 = ifelse(AEACN %in% c("DOSE NOT CHANGED", "NOT APPLICABLE"), NA, AEACN),
      text_x = AESTDY + (AEENDY2 - AESTDY)/2,
      # if related, thick bar, if not related, thin bar
      bar_size = as.factor(ifelse(AEREL == "RELATED", 3, 2)),
      AEDECOD = ifelse(AEREL == "RELATED", toupper(AEDECOD), AEDECOD),
      AEDECOD = factor(AEDECOD, levels = sort(unique(AEDECOD), decreasing = T))
      )  -> ae
  ae %>% filter(AESTDTC == AEENDTC) -> ae_oneday
  
  aedfs <- list(ae, ae_oneday)
  
  return(aedfs)
}

# AE Listing----------------------------------------------------------------

# ARM
make_ae_listing <- function(ae,pid){

  ae %>% 
    filter(USUBJID == pid) %>%
    select(
        USUBJID,
        ACTARM,
        AESOC,
        AEDECOD,
        AETERM,
        AESTDTC, 
        AEENDTC,
        AESTDY,
        AEENDY,
        AEENDY2,
        AETOXGR,
        AESER, 
        AESDTH, 
        AEOUT, 
        AEREL,
        AEACN
    ) %>%
    rename(
      `Patient ID` = USUBJID,
      Arm = ACTARM,
      `System Organ class` = AESOC,
      `Preferred Term` = AEDECOD,
      `AE Reported Term` = AETERM,
      `Start Date` = AESTDTC,
      `End Date` = AEENDTC,
      `Start Study Day` = AESTDY,
      `End Study Day` = AEENDY,
      `End Study Day (computed)` = AEENDY2,
      Grade = AETOXGR,
      `SAE?` = AESER,
      Fatal = AESDTH,
      Outcome = AEOUT, 
      Causality = AEREL,
      `Action Taken` = AEACN
    ) -> ae_listing
  return(ae_listing)
}
# pid =  "XPORT-MF-035-1200-001"
#  ae <- process_ae(df_ae, pt_info, '2022-11-01' )[[1]]
# make_ae_listing(ae, df_dm, "XPORT-MF-035-1200-001")

#LB-----------------------------------------------------------------------------
#Including: Hemoglobin, platelets, whilte blood cell; WBC HGB, PLAT
#'Assume the LBDY is correct, dose not use VISIT information as it's not consist
#'ent across different trial. 029: Combination therapy Cycle 1 Day 1. upper/lowe
#'r,  puncuation
#'Safety Population
# Hemoglobin alg:
# missingness: 
#' assessments with missing assessment date were excluded. 
#' Empty assessment points were excluded.
#'BL assessment: last measurement on or before first dose date 
 

process_lb <- function(df_lb, safety_pp){
  
  df_lb %>%
    filter(USUBJID %in% unique(safety_pp$USUBJID))%>%
    filter(! is.na(LBDY)) %>%
    filter(LBTESTCD %in% c("HGB", "WBC", "PLAT")) %>%
    filter(!is.na(LBSTRESN)) %>% 
    arrange(USUBJID,ymd(LBDTC)) -> df_lb_noNA
  # baseline assessment last measurement on or before first dose date.
  
  df_lb_noNA %>% 
    filter(LBDY <= 1) %>%
    group_by(USUBJID, LBTESTCD) %>%
    summarise(LBDY = max(LBDY)) %>% 
    left_join(df_lb_noNA, by = c("USUBJID", "LBTESTCD","LBDY")) %>%
    # change all baseline measurement to day 1
    mutate(LBDY = 1) -> lb_bl
  
  df_lb_noNA %>%
    filter(LBDY > 1) %>% 
    bind_rows(lb_bl)%>%
    arrange(USUBJID, LBTESTCD, LBDY)-> lb
  
    # calculate percentage change
  lb %>%
    group_by(USUBJID, LBTESTCD) %>%
    #absolute change from basline
    mutate(
      PCH = (LBSTRESN - first(LBSTRESN))/first(LBSTRESN),
      # for plotting
      PCH2 = case_when(
        LBTESTCD == "HGB" ~ PCH,
        LBTESTCD == 'PLAT' ~ PCH + 0.15,
        LBTESTCD == 'WBC' ~ PCH + 0.30
        )
      ) -> lb
  return(lb)
}
  
#process_lb(df_lb, safety_pp)

# Compute pt's info ----------------------------------------------------------------

# can seperate dm and fa, if fa is not consistent across different trials.
# exclude disease histoty as it's not consistent across different trials.


compute_ptinfo <- function(df_ds,df_dm,df_dd, safety_pp){
    # EOT, Reason
  df_ds %>%
    filter(DSSCAT == "END OF RANDOMIZATION TREATMENT") %>%
    select(
      USUBJID, DSSCAT, DSDECOD, DSSTDTC, DSSTDY
    ) %>%
    mutate(
      DSSCAT = "EOT",
      DSSTDTC  = as.Date(DSSTDTC)
    ) -> df_eot
  
  #EOS, Reason
  df_ds %>%
    filter(DSSCAT == "END OF STUDY") %>%
    select(
      USUBJID, DSSCAT, DSDECOD, DSSTDTC, DSSTDY
    ) %>%
    mutate(
      DSSCAT = "EOS",
      DSSTDTC  = as.Date(DSSTDTC)
    ) -> df_eos
  
  # Death, Reason
  df_dd %>%
    select(
      USUBJID,DDSTRESC,DDDTC,DDDY
    ) %>%
    mutate(
      DDDTC  = as.Date(DDDTC)
    ) -> df_dth
  
  # Actual arm
  df_dm %>%
    filter(ACTARMCD != "") %>%
    select(USUBJID, ACTARM, ACTARMCD) -> df_arm
  
  # demographic
  df_dm %>%
    select(USUBJID,AGE, SEX, RACE, ETHNIC) %>%
    mutate(
      across(AGE:ETHNIC, 
             ~ ifelse(. == "", NA, .)),
      SEX = case_when(
        SEX == "M" ~ "MALE",
        SEX == 'F' ~ "FEMALE"
      )
    ) -> df_dm1
  
  # # disease history
  # df_fa %>%
  #   select(USUBJID, FATEST, FASTRESC, FADY) %>%
  #   pivot_wider(
  #     id_cols = USUBJID,
  #     names_from = FATEST,
  #     values_from = c(FASTRESC, FADY)
  #   ) -> df_diseaseH
  # 
  # # duration from inital diagonsis to seli trt
  # df_suppfa %>%
  #   filter(IDVARVAL == 1) %>%
  #   select(USUBJID, QVAL) -> df_diseaseH_supp
  
  #combine
  df_eos %>%
    full_join(df_eot, by = 'USUBJID', suffix = c('.eos', '.eot')) %>%
    full_join(df_dth, by = 'USUBJID', suffix = c("", ".dth")) %>%
    right_join(safety_pp, by = "USUBJID") %>%
    inner_join(df_arm, by = "USUBJID")  %>%
    left_join(df_dm1, by = "USUBJID") -> df_ptinfo
    # left_join(df_diseaseH, by = "USUBJID") %>%
    # left_join(df_diseaseH_supp, by = "USUBJID") 
  # 
  # df_ptinfo %<>%
  #   mutate(
  #     Duration_diag2trt = 
  #       round(
  #         as.numeric(as.Date(TRTSTDC) - as.Date(QVAL))/30.4, 
  #         0
  #       )
  #   )
  # 
  
  
  df_ptinfo %<>%
    mutate(
      title = paste0(
        USUBJID,
        "\n", "Date of First Dose:", TRTSTDC,
        "\n", ACTARM,
        "\n", "Demographics: ", AGE, " || ", SEX, " || ", RACE, " || ", ETHNIC,
        '\nEoT:',  DSSTDTC.eot, ' [Day ', DSSTDY.eot, ']',
        ' Reason:', DSDECOD.eot,
        '\nEoS:', DSSTDTC.eos, ' [Day ', DSSTDY.eos, ']',
        ' Reason:', DSDECOD.eos,
        '\nDeath:', DDDTC, ' [Day ', DDDY, ']',
        ' Reason:', DDSTRESC
        # "\n", "Disease History: ", `FASTRESC_Myelofibrosis type`, " MF", " || ",
        # `FASTRESC_Driver Mutation`, " || ",
        # 'DIPSS-', `FASTRESC_DIPSS (at Screening)`, " || ",
        # 'TRANS-',`FASTRESC_Transfusion status (at Screening)`,
        # "\n", "Duration from Active Diagnosis to Treatment:", Duration_diag2trt, ' mth'
      )
    )# df_fa <- read_sas(paste0(path,'/SDTM/fa.sas7bdat'))

  return(df_ptinfo)
}





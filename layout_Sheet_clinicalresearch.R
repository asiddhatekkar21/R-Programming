setwd("D:/Training/Academy/R Course (C-DAC)/Clinical_Datasets/raw")
library(lubridate)
library(dplyr)
library(tidyr)

add <- function(a, b, c){
  a+b+c
}

add(3,5,6)

orig_vs <- read.csv("vs_raw.csv")

summr_temp <- orig_vs %>% 
                summarise(avg_meas =  mean(IT.TEMP, na.rm=T),
                      sd_meas = sd(IT.TEMP, na.rm=T),
                      med_meas = median(IT.TEMP, na.rm=T),
                      min_meas = min(IT.TEMP, na.rm=T),
                      max_meas = max(IT.TEMP, na.rm=T))


summr_vital <- function(df, measure){
  ans <- df %>% 
            summarise(avg_meas =  mean(measure, na.rm=T),
                      sd_meas = sd(measure, na.rm=T),
                      med_meas = median(measure, na.rm=T),
                      min_meas = min(measure, na.rm=T),
                      max_meas = max(measure, na.rm=T))
  return(ans)
}

summr_wt <- summr_vital( orig_vs, orig_vs$IT.WEIGHT)
summr_temp <- summr_vital( orig_vs, orig_vs$IT.TEMP)


######################################################################

dm <- read.csv("dm_raw.csv")
rec_dm <- dm %>% 
  filter( ACTUAL_ARMCD!="Scrnfail" ) %>% 
  select(PATNUM, ACTUAL_ARMCD)

orig_vs <- read.csv("vs_raw.csv")


###########################################################################
summr_vital_rpt <- function(df, rpt_string){
  df$avg_meas <- formatC(df$avg_meas, format = 'f', digits = 1)
  df$sd_meas <- formatC(df$sd_meas, format = 'f', digits = 1)
  df$med_meas <- formatC(df$med_meas, format = 'f', digits = 1)
  df$min_meas <- formatC(df$min_meas, format ='f', digit = 0)
  df$max_meas <- formatC(df$max_meas, format ='f', digit = 0)
  df$n <- formatC(df$n, format ='f', digit = 0)            
  
  df$meas_mn_sd <- paste( df$avg_meas, "(", df$sd_meas, ")")
  df$meas_mm <- paste( "(", df$min_meas, "," , df$max_meas, ")" )
  
  df  <-  df %>% 
    select(ACTUAL_ARMCD, n, meas_mn_sd , med_meas, meas_mm)
  
  df_measure <- df %>% 
    select(ACTUAL_ARMCD, n, meas_mn_sd , med_meas, meas_mm) %>% 
    pivot_longer(-c(ACTUAL_ARMCD), names_to = "stats", values_to = "vals") %>% 
    mutate(arm_stats = paste0(ACTUAL_ARMCD,"_", stats), 
           measure=rpt_string) %>% 
    select(measure, arm_stats, vals) %>% 
    pivot_wider(names_from = "arm_stats", values_from = "vals") 
  
  return(df_measure)
}

### Supine Pulse Rate 
pulse <- orig_vs %>% 
  filter(!INSTANCE %in% c('Screening 1' ,'Screening 2')) %>% 
  select(PATNUM, PULSE, SUBPOS) %>% 
  filter(SUBPOS=="SUPINE") %>% 
  left_join(rec_dm, by = 'PATNUM')

summr_pulse <- pulse %>% 
  group_by(ACTUAL_ARMCD) %>% 
  summarise(n = n(),avg_meas = mean(PULSE, na.rm=T),
            sd_meas = sd(PULSE, na.rm=T),
            med_meas = median(PULSE, na.rm = T),
            min_meas = min(PULSE, na.rm = T),
            max_meas = max(PULSE, na.rm = T) )

summr_pulse_rpt <- summr_vital_rpt(summr_pulse, "Supine Pulse Rate (bpm)")

### Standing 1 min Pulse Rate 
pulse <- orig_vs %>% 
  filter(!INSTANCE %in% c('Screening 1' ,'Screening 2')) %>% 
  select(PATNUM, PULSE, TMPTC) %>% 
  filter(TMPTC=="after Standing for 1 Minute") %>% 
  left_join(rec_dm, by = 'PATNUM')

summr_pulse <- pulse %>% 
  group_by(ACTUAL_ARMCD) %>% 
  summarise(n = n(),avg_meas = mean(PULSE, na.rm=T),
            sd_meas = sd(PULSE, na.rm=T),
            med_meas = median(PULSE, na.rm = T),
            min_meas = min(PULSE, na.rm = T),
            max_meas = max(PULSE, na.rm = T) )

summr_pulse_1_min_rpt <- summr_vital_rpt(summr_pulse, "Standing for 1 min - Pulse Rate (bpm)")

### Standing 3 min Pulse Rate 
pulse <- orig_vs %>% 
  filter(!INSTANCE %in% c('Screening 1' ,'Screening 2')) %>% 
  select(PATNUM, PULSE, TMPTC) %>% 
  filter(TMPTC=="after Standing for 3 Minutes") %>% 
  left_join(rec_dm, by = 'PATNUM')

summr_pulse <- pulse %>% 
  group_by(ACTUAL_ARMCD) %>% 
  summarise(n = n(),avg_meas = mean(PULSE, na.rm=T),
            sd_meas = sd(PULSE, na.rm=T),
            med_meas = median(PULSE, na.rm = T),
            min_meas = min(PULSE, na.rm = T),
            max_meas = max(PULSE, na.rm = T) )

summr_pulse_3_min_rpt <- summr_vital_rpt(summr_pulse, "Standing for 3 min - Pulse Rate (bpm)")

#summr_rpt <- bind_rows(summr_pulse_rpt, summr_pulse_1_min_rpt, summr_pulse_3_min_rpt)



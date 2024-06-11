#Workflow for forecasting aggregation structures
#The above code illustrates the general workflow for hierarchical and grouped forecasts. We use the following pipeline of functions.

#1. data |> 
#2. aggregate_key() |>
#3. model() |>
#4. reconcile() |>
#5. forecast()

#Begin with a tsibble object (here labelled data) containing the individual bottom-level series.
#Define in aggregate_key() the aggregation structure and build a tsibble object that also contains the aggregate series.
#Identify a model() for each series, at all levels of aggregation.
#Specify in reconcile() how the coherent forecasts are to be generated from the selected models.
#Use the forecast() function to generate forecasts for the whole aggregation structure.
install.packages("scales")
install.packages("readxl")
install.packages("fable")
install.packages("fabletools")
install.packages("lubridate")
install.packages("tibble")
install.packages("readxl")
install.packages("tidyr")
install.packages("tsibble")
install.packages("hts")
install.packages("feasts")
install.packages("writexl")
library(scales)
library(readxl)
library(fable)
library(fabletools)
library(lubridate)
library(tibble)
library(tidyr)
library(tsibble)
library(dplyr)
library(hts)
library(ggplot2)
library(feasts)
library(writexl)



# 1. read file excel, filter exclude  kpp 000, & PPh DTP --------
  df_all_yearly <- read_excel(
  "D:\\One-Drive UI\\OneDrive - UNIVERSITAS INDONESIA\\TESIS\\e-riset DJP\\target-realisasi tahunan.xlsx",
  sheet = "Sheet1") 

cek_time_series<-df_all_yearly |> 
  group_by(KD_KPP) |> 
  summarise(panjang_Time_series=n())

recap_length_Ts<-cek_time_series|> 
  group_by(panjang_Time_series) |> 
  summarise(jml_KPP=n())
  
  assign_label<-cek_time_series |> 
  mutate (KD_KPP_baru= ifelse(panjang_Time_series==10, KD_KPP,
                              ifelse(panjang_Time_series!=10,"tdk_tetap","")
                              ))

df_yearly_fix<-merge(x = df_all_yearly,
                     y = assign_label, 
                     by = "KD_KPP", all.x = TRUE) |> 
  mutate(KD_KWL_baru= ifelse(panjang_Time_series==10, KD_KWL,
                             ifelse(panjang_Time_series!=10,"tdk_tetap",""))) |> 
  filter(KD_KPP!="000"& KD_KPP!="990") |> 
  group_by(KD_KWL_baru,KD_KPP_baru,TAHUN) |> 
  summarise(
    TARGET=sum(TARGET),
    realisasi=sum(realisasi)) |> 
  as_tsibble(key=c(KD_KWL_baru,KD_KPP_baru),index=TAHUN) |> 
  rename(KD_KPP=KD_KPP_baru,
         KD_KWL=KD_KWL_baru) |> 
  aggregate_key(KD_KWL/KD_KPP,TARGET=sum(TARGET),realisasi=sum(realisasi)) 

  

# 2. read file internal planning DJP -----------------------------------------
# filter(KD_KPP!="000" & KD_KPP !=) |> 
# as_tsibble(key=c(KD_KWL,KD_KPP),index=TAHUN) |> 
#   aggregate_key(KD_KWL/KD_KPP,SETORAN=sum(SETORAN)) |> 
#   mutate(combine_key = paste(KD_KPP, TAHUN)) |> 
  

#3.0 plotting top & middle series
int_breaks <- function(x, n = 5) {
  l <- pretty(x, n)
  l[abs(l %% 1) < .Machine$double.eps ^ 0.5] 
}  #Formula tampilan rapi sumbu x pada plot

#plotting nasional
df_yearly_fix |>
  filter(is_aggregated(KD_KWL)) |>
  autoplot(realisasi/10^12) +
  labs(y = "realisasi (triliun)",
       title = "Realisasi penerimaan pajak nasional 2014-2023") +
  facet_wrap(vars(KD_KWL), scales = "free_y", ncol = 2) +
  geom_point()+
  theme(legend.position = "none")+
  scale_x_continuous(breaks = int_breaks)


#plotting Kanwil
df_yearly_fix |>
  filter(KD_KWL == "310" | KD_KWL == "130" |
           KD_KWL == "090" | KD_KWL == "tdk_tetap",is_aggregated(KD_KPP)) |>
  select(-KD_KWL) |>
  mutate(KD_KWL = factor(KD_KWL, levels=c("310","130","090","tdk_tetap"))) |>
  autoplot(realisasi/10^12) +
  labs(y = "realisasi (triliun)",
       title = "Realisasi penerimaan pajak 4 Kanwil DJP 2014-2023") +
  facet_wrap(vars(KD_KWL), scales = "free_y", ncol = 2) +
  geom_point()+
  theme(legend.position = "none")+
  scale_x_continuous(breaks = int_breaks)


#plottinng KPP,
df_yearly_fix |>
  filter(KD_KPP == "091" | KD_KPP == "073" |
           KD_KPP == "123" | KD_KPP == "tdk_tetap") |>
  select(-KD_KWL) |>
  mutate(KD_KPP = factor(KD_KPP, levels=c("091","073","123","tdk_tetap"))) |>
  autoplot(realisasi/10^9) +
  geom_point()+
  facet_wrap(vars(KD_KPP), nrow = 2, scales = "free_y", ncol = 2)+
  labs(y = "realisasi (miliar)",
       title = "Realisasi penerimaan pajak 4 KPP")+
  theme(legend.position = "none")+
  scale_x_continuous(breaks = int_breaks)


#3.1create model fit  ---------------- ,
{
  FITT<-df_yearly_fix|> #df_complete
    mutate(KD_KWL_KPP_THN=paste(KD_KWL, KD_KPP, TAHUN, sep = "_")) |> 
    tsibble::fill_gaps() |> 
    filter(TAHUN <= 2023) |> 
    model(base_mean=MEAN(realisasi,data=df_yearly_fix),
          base_Fc_ETS=ETS(realisasi  ~ trend()),
          base_NAIVE=(NAIVE(realisasi)),
          base_RWDrift=(RW(realisasi ~drift(drift = TRUE))), 
          base_lm = TSLM(realisasi ~trend()),
          ARIMA=ARIMA(realisasi)) |>  
    mutate(base_ensemble=(base_mean+base_Fc_ETS+base_NAIVE+
                            base_RWDrift+base_lm+
                            ARIMA)/6) |>
    reconcile(
      mint_Fc=min_trace(base_ensemble,method="mint_shrink")
    ) 
  }
print(FITT,n=354)

fitted_value_year<-FITT |> 
  fitted() |> 
  filter(TAHUN> 2014) |> 
  rename(.mean=.fitted)
print(fitted_value_year,n=30)  

FITT |> 
  select(base_mean) |> 
  glance() 
FITT |> 
  select(base_Fc_ETS) |> 
  glance()  
FITT |> 
  select(ARIMA) |> 
  glance() 
FITT |> 
  select(base_NAIVE) |> 
  glance() 
FITT |> 
  select(base_RWDrift) |> 
  glance() 
FITT |> 
  select(base_lm) |> 
  glance()  

print(FITT, n=354)
# 4. forecast & summary RECAP MASE_MAPE------------------------------------------------------------- 

{
  fc_yearly <- FITT |>
    forecast(h = 1)
}


# 4b. pivot compare MAPE manual ---------------------------------------------------

{
  #basetable_MAPE
  fore<-fitted_value_year |> filter(.model=="mint_Fc" |.model=="base_ensemble" ) |> 
    # pivot_wider(names_from = .model, values_from = .mean) |> 
    filter(!grepl("<aggregated>",KD_KPP)) |>
    as_tibble() |> 
    arrange(KD_KPP)
  #|> 
   # select(mint_Fc) 
  
  Df_target<-df_yearly_fix |> 
    filter(!grepl("<aggregated>",KD_KPP)) |> 
    as_tibble() |> 
    mutate(.model="TARGET") |> 
    rename(.mean=TARGET) |> 
    select(-realisasi)
   
  df_actual<-df_yearly_fix |> 
    filter(!grepl("<aggregated>",KD_KPP)) |> 
    as_tibble() |> 
    mutate(.model="realisasi") |>
    rename(.mean=realisasi) |> 
    select(-TARGET)

  
  comparation_yearly<-bind_rows(Df_target,df_actual,fore) |> 
    filter(TAHUN>2014) |> 
    pivot_wider(names_from = .model, values_from = .mean) |> 
    mutate(MAPE_TARGET=abs((TARGET-realisasi)/realisasi*100)) |> 
    mutate(MAPE_ensemble =abs((base_ensemble -realisasi)/realisasi*100)) |> 
    mutate(MAPE_mint_Fc=abs((mint_Fc-realisasi)/realisasi*100))
    
    
  comparation_nas_year<-comparation_yearly |> 
    group_by(TAHUN) |> 
    summarise(
      realisasi =sum(realisasi ),
      base_ensemble=sum(base_ensemble),
      mint_Fc=sum(mint_Fc),
      TARGET=sum(TARGET)
    ) |> 
    mutate(MAPE_TARGET=abs((TARGET-realisasi)/realisasi*100)) |> 
    mutate(MAPE_ensemble =abs((base_ensemble -realisasi)/realisasi*100)) |> 
    mutate(MAPE_mint_Fc=abs((mint_Fc-realisasi)/realisasi*100)) |> 
    mutate(level="NAS") |> 
    select (-realisasi,-mint_Fc,-TARGET,-base_ensemble)
  
  comparation_KWL_year<-comparation_yearly |> 
    group_by(KD_KWL,TAHUN) |> 
    summarise(
      realisasi =sum(realisasi ),
      base_ensemble=sum(base_ensemble),
      mint_Fc=sum(mint_Fc),
      TARGET=sum(TARGET)
    ) |> 
    mutate(MAPE_TARGET=abs((TARGET-realisasi)/realisasi*100)) |> 
    mutate(MAPE_ensemble =abs((base_ensemble -realisasi)/realisasi*100)) |> 
    mutate(MAPE_mint_Fc=abs((mint_Fc-realisasi)/realisasi*100)) |> 
    mutate(level="Kanwil") |> 
    select (-realisasi,-mint_Fc,-TARGET,-base_ensemble) |> 
    group_by(TAHUN,level) |> 
    summarise(
      MAPE_TARGET =mean(MAPE_TARGET ),
      MAPE_ensemble =mean(MAPE_ensemble),
      MAPE_mint_Fc =mean(MAPE_mint_Fc)
    ) 
  
  comparation_KPP_year<-comparation_yearly |> 
    mutate(level="KPP") |> 
    select (-realisasi,-mint_Fc,-TARGET,-base_ensemble) |> 
    group_by(TAHUN,level) |> 
    summarise(
      MAPE_TARGET =mean(MAPE_TARGET ),
      MAPE_ensemble =mean(MAPE_ensemble),
      MAPE_mint_Fc =mean(MAPE_mint_Fc)
    ) 
  
  
  recap_manual_comparation_year<-bind_rows(comparation_nas_year,comparation_KWL_year,comparation_KPP_year)  
  print(recap_manual_comparation,n=30)
  
  recap_level<-recap_manual_comparation_year |> 
    group_by(level) |> 
    summarise(
      MAPE_TARGET =mean(MAPE_TARGET ),
      MAPE_ensemble =mean(MAPE_ensemble),
      MAPE_mint_Fc =mean(MAPE_mint_Fc)
    ) 
  print(recap_level)
  
  
  write_xlsx(recap_manual_comparation_year, "D:\\One-Drive UI\\OneDrive - UNIVERSITAS INDONESIA\\TESIS\\model_tahunan_MAPE_manual_2023.xlsx")
  
  
}  

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
library(RColorBrewer)

# 1. read file excel hasil olahan, filter exclude  kpp 000, & PPh DTP --------
df_all <- read_excel(
  "D:\\One-Drive UI\\OneDrive - UNIVERSITAS INDONESIA\\TESIS\\e-riset DJP\\actual.xlsx",
                     sheet = "Sheet3") |> 
  mutate(bulan=yearmonth(dmy(tanggal)))|> 
  filter(as.Date(bulan, format = "%Y %b") < ymd("2024-01-01"))|> 
  filter(KD_KPP!="000"& KD_KPP!="990" & FG_hapus !="hapus") 
print(df_all[order(df_all$FG_hapus, decreasing = FALSE), ])


##########################
# 1a. deal with uneven time series for KPP Baru by backcasting apr2021 to jan2021 with ETS -----------------------------------
{
# count how many distinct KPP_baru     filter(!grepl('aggregated', KD_KPP)) 
  jml_baru<-df_all |> 
    filter( FG_hapus =="baru") 
  jml_KPP_baru<-n_distinct(jml_baru$KD_KPP)   
  jml_baru
  jml_KPP_baru
# create new reversed time index then do backcasting 4 month
  backcast_kpp <- df_all |>
    filter( FG_hapus =="baru") |>
    as_tsibble(key=c(KD_KWL,FG_hapus,KD_KPP),index=bulan) |> 
    arrange((bulan))|> 
    mutate(reverse_time = rev(row_number())) |>
    update_tsibble(index = reverse_time) |>
    model(ets = ETS(realisasi  ~ season(period = 12))) |>
    forecast(h = 4) |> 
    arrange(reverse_time)
  backcast_kpp

#create new dataframe with appropriate backdate time index to attach back 
  backdate<-data.frame(bulan=seq(ymd('2021-01-01'), ymd('2021-04-01'),
                                 by = "month")
                       ) |> 
    arrange(desc(bulan)) |> 
    slice(rep(1:n(), each =jml_KPP_baru ))  
  head(backdate)
  count(backdate)

#bind back by column appropriate time index 
  backcast_kpp_baru<-bind_cols(backcast_kpp, backdate) |> 
    mutate(bulann=yearmonth(bulan)) |> 
    arrange(KD_KPP) |>
    update_tsibble(index=bulann) |> 
    select(KD_KWL, KD_KPP, bulan=bulann,realisasi=.mean)
  
#convert original df_all to tsibble
 df_all_tsibble<-df_all |> 
   select(KD_KWL, KD_KPP, bulan=bulan,realisasi=realisasi) |> 
   as_tsibble(key=c(KD_KWL,KD_KPP),index=bulan) 
 
 #concatenate original df_all_tsibble with backcast_kpp_baru 
 #and make aggregate key
 df_complete<-bind_rows(df_all_tsibble,backcast_kpp_baru) |> 
    aggregate_key(KD_KWL/KD_KPP,realisasi=sum(realisasi))  
}  

# 2. read file internal forecast DJP -----------------------------------------
{
FC_internal_KPP <- read_excel(
  "D:\\One-Drive UI\\OneDrive - UNIVERSITAS INDONESIA\\TESIS\\e-riset DJP\\FC_INTERNALKPP_FIX.xlsx") |>
  as.data.frame() |> 
  mutate(bulan=yearmonth(dmy(Tanggal)))|>
  mutate(.model="internal_Fc_KPP") |> 
  mutate(.mean=realisasi) |> 
  filter(KD_KPP!="000"& KD_KPP!="990") |> 
  filter(as.Date(bulan, format = "%Y %b") >= ymd("2021-01-01")) |>
  # filter(as.Date(bulan, format = "%Y %b") < ymd("2024-01-01")) |>
  select(KD_KWL, KD_KPP,bulan,'.mean','.model') |> 
  mutate(KD_KWL_KPP_bln=paste(KD_KWL, KD_KPP, bulan, sep = "_"))


CPIN<-read_excel(
  "D:\\One-Drive UI\\OneDrive - UNIVERSITAS INDONESIA\\TESIS\\e-riset DJP\\R-CPIN.xlsx")
FC_PKP_CPIN<-read_excel(
  "D:\\One-Drive UI\\OneDrive - UNIVERSITAS INDONESIA\\TESIS\\e-riset DJP\\R-CPIN.xlsx") |> 
    mutate(bulan=yearmonth(Tanggal))|> 
    filter(as.Date(bulan, format = "%Y %b") < ymd("2024-01-01")) |>
    select(bulan, forecast=FC_nonDTP) |>   
    mutate(.model="internal_Fc_CPIN") |> 
    group_by(bulan,.model) |> 
    summarise(realisasi=sum(forecast)) |> 
    mutate(linesize=1.1)
CPIN
print(FC_PKP_CPIN,n=36)

}


#3.0 plotting top & middle series
#plottinng nasional
df_complete |>
  filter(is_aggregated(KD_KWL)) |>
  autoplot(realisasi/10^12,color="darkblue") +
  labs(y = "realisasi (triliun)",
       title = "Realisasi penerimaan pajak nasional bulanan 2021-2023") +
  facet_wrap(vars(KD_KWL), scales = "free_y", ncol = 2) +
  geom_point(color="darkblue")+
  theme(legend.position = "none")

#plottinng Kanwil
df_complete |>
  filter(KD_KWL == "310" | KD_KWL == "130" |
           KD_KWL == "090" | KD_KWL == "100",is_aggregated(KD_KPP)) |>
  select(-KD_KWL) |>
  mutate(KD_KWL = factor(KD_KWL, levels=c("310","130","090","100"))) |>
  gg_season(realisasi/10^12,pal = c("red", "#FFCA4B","darkblue")) +
  geom_point()+
  facet_wrap(vars(KD_KWL), nrow = 2, scales = "free_y")+
  labs(y = "realisasi (triliun)",
       title = "Realisasi penerimaan pajak 4 Kanwil DJP")

#plottinng KPP,
df_complete |>
  filter(KD_KPP == "091" | KD_KPP == "095" |
           KD_KPP == "069" | KD_KPP == "086") |>
  select(-KD_KWL) |>
  mutate(KD_KPP = factor(KD_KPP, levels=c("091","095","069","086"))) |>
  gg_season(realisasi/10^9,pal =  c("red", "#FFCA4B","darkblue")) +
  geom_point()+
  facet_wrap(vars(KD_KPP), nrow = 2, scales = "free_y")+
  labs(y = "realisasi (miliar)",
       title = "Realisasi penerimaan pajak 4 KPP")

#3.1 create model fit 
{
  FITT_7<-df_complete|> #df_complete
    mutate(KD_KWL_KPP_bln=paste(KD_KWL, KD_KPP, bulan, sep = "_")) |> 
    tsibble::fill_gaps() |> 
    model(base_mean=MEAN(realisasi,data=df_complete),
          base_NAIVE=(NAIVE(realisasi)),
          base_SNAIVE=(SNAIVE(realisasi)),
          base_Fc_ETS=(ETS(realisasi  ~ season(period="1 year"))),
          base_arima=ARIMA(realisasi),
          base_RWDrift=(RW(realisasi ~drift(drift = TRUE))),
          base_lm = TSLM(realisasi ~ trend() + season(period="1 year"))) |> 
    mutate(base_ensemble=(base_mean+base_NAIVE+base_SNAIVE+base_Fc_ETS+
                            base_arima+base_RWDrift+base_lm)/7) |> 
    reconcile(mint_Fc=min_trace(base_ensemble,method="mint_shrink"))
  print(FITT_7 ,n=19)
  

  fitted_value<-FITT_7 |> 
    fitted() |> 
    filter(as.Date(bulan, format = "%Y %b") > ymd("2021-12-31"))
  print(fitted_value,n=30)  
    
    
   }
#########################################

fc <- FITT_7 |>
  forecast(h = 6)


FITT_7 |> 
  forecast(h=1) |> 
  autoplot(df_complete |> 
             filter(is_aggregated(KD_KWL)))

##4.0 lihat spesifikasi masing-masing model#####
FITT_7 |> 
  select(base_mean) |> 
  glance() 
FITT_7 |> 
  select(base_Fc_ETS) |> 
  glance()  
FITT_7 |> 
  select(base_arima) |> 
  glance() 
FITT_7 |> 
  select(base_NAIVE) |> 
  glance() 
FITT_7 |> 
  select(base_SNAIVE) |> 
  glance() 
FITT_7 |> 
  select(base_RWDrift) |> 
  glance() 
FITT_7 |> 
  select(base_RW) |> 
  glance() 
FITT_7 |> 
  select(base_lm) |> 
  glance()  


# 4. forecast & summary RECAP MASE_MAPE------------------------------------------------------------- 
#forecast

  fc <- FITT_7 |>
    forecast(h = 3)
  print(fc,n=30)
  

# 5. compare MAPE manual ---------------------------------------------------
{
####joined with forecast column####
fc_tibble<-fitted_value |> 
  rename(.mean=.fitted) |> 
  as_tibble() |> 
  select(KD_KWL, KD_KPP,bulan,.mean,'.model') 
fc_tibble

real<-df_complete |> 
  filter(as.Date(bulan, format = "%Y %b") < ymd("2024-01-01")) |>
  mutate(.model="actual") |> 
  select(KD_KWL, KD_KPP,bulan,.mean=realisasi,'.model') |> 
  as_tibble() |> 
  mutate(comb=paste(KD_KWL, KD_KPP,bulan, sep = "_")) |> 
  filter(!grepl('aggregated', comb)) 


joinn<-bind_rows(fc_tibble,real,FC_internal_KPP)|> 
  filter(as.Date(bulan, format = "%Y %b") < ymd("2024-01-01")) |> 
  select(KD_KWL, KD_KPP,bulan,'.mean','.model') |> 
  mutate(comb=paste(KD_KWL, KD_KPP,bulan, sep = "_")) |> 
  filter(!grepl('aggregated', comb)) |>
  group_by(comb,KD_KWL, KD_KPP,bulan,.model) |> 
  summarise(realisasi=sum(.mean)) |> 
  mutate(linesize = ifelse(.model == 'actual', 1.1,
                           ifelse(.model == 'base_ensemble', 1.1,
                                  ifelse(.model =='mint_Fc',1.1,
                                         ifelse(.model =='internal_Fc_KPP',1.1,1)))))




#basetable_MAPE
comparation<-bind_rows(fc_tibble,real,FC_internal_KPP)|> 
  select(KD_KWL, KD_KPP,bulan,'.mean','.model') |> 
  mutate(comb=paste(KD_KWL, KD_KPP, sep = "_")) |> 
  filter(!grepl('aggregated', comb)) |>
  group_by(KD_KWL, KD_KPP,bulan,.model) |> 
  summarise(realisasi=sum(.mean)) |> 
  filter(as.Date(bulan, format = "%Y %b") >= ymd("2022-01-01")) |>
  filter(as.Date(bulan, format = "%Y %b") < ymd("2024-01-01")) |>
  filter(.model %in%  c("internal_Fc_KPP", "base_ensemble", "mint_Fc","actual")) |> 
  pivot_wider(names_from = .model, values_from = realisasi) |> 
  mutate_all(~replace(., is.na(.), 0))
  # filter(internal_Fc_KPP>0) 

print(comparation_KPP |>
  group_by(bulan,.model) |>
  summarise(.mean=sum(realisasi)),n=400)

compare_Quarter<-comparation |> 
  mutate(TW=quarter(bulan)) |> 
  mutate(Tahun=year(bulan)) |> 
  mutate(KD_KWL_KPP_TW=paste(KD_KWL, KD_KPP, Tahun,TW, sep = "_")) |> 
  group_by(KD_KWL,KD_KPP,TW,Tahun,KD_KWL_KPP_TW) |> 
  summarise(actual=sum(actual),
            base_ensemble =sum(base_ensemble),
            internal_Fc_KPP=sum(internal_Fc_KPP),
            mint_Fc=sum(mint_Fc))


#perbandingan tingkat KPP
comparation_KPP<-comparation |> 
  mutate(MAPE_base_ensemble=(abs((base_ensemble-actual)/actual*100))) |> 
  mutate(MAPE_mint_Fc=abs((mint_Fc-actual)/actual*100)) |> 
  mutate(level="KPP-bulanan")#|> 
  #mutate(MAPE_internal_Fc_KPP=abs((internal_Fc_KPP-actual)/actual*100)) 


comparation_KPP_Q<-compare_Quarter|> 
  mutate(MAPE_base_ensemble=(abs((base_ensemble-actual)/actual*100))) |> 
  mutate(MAPE_mint_Fc=abs((mint_Fc-actual)/actual*100)) |> 
  mutate(MAPE_internal_Fc_KPP=abs((internal_Fc_KPP-actual)/actual*100)) |> 
  mutate(level="KPP-Quarteran")



#perbandingan tingkat Kanwil
comparation_KWL<-comparation |>  
  group_by(KD_KWL,bulan) |> 
  summarise(
    actual=sum(actual),
    base_ensemble=sum(base_ensemble),
    mint_Fc=sum(mint_Fc),
    internal_Fc_KPP=sum(internal_Fc_KPP)
    ) |> 
  mutate(MAPE_base_ensemble=abs((base_ensemble-actual)/actual*100)) |> 
  mutate(MAPE_mint_Fc=abs((mint_Fc-actual)/actual*100)) |> 
  mutate(level="Kanwil-bulanan")#|> 
  #mutate(MAPE_internal_Fc_KPP=abs((internal_Fc_KPP-actual)/actual*100))

comparation_KWL_Q<-compare_Quarter |> 
  group_by(KD_KWL, Tahun,TW) |> 
  summarise(
    actual=sum(actual),
    base_ensemble=sum(base_ensemble),
    mint_Fc=sum(mint_Fc),
    internal_Fc_KPP=sum(internal_Fc_KPP)
  ) |> 
  mutate(MAPE_base_ensemble=abs((base_ensemble-actual)/actual*100)) |> 
  mutate(MAPE_mint_Fc=abs((mint_Fc-actual)/actual*100)) |> 
  mutate(MAPE_internal_Fc_KPP=abs((internal_Fc_KPP-actual)/actual*100))|> 
  mutate(level="Kanwil-Quarteran")
  


#perbandingan tingkat Nasional

comparation_NAS<-comparation |>  
  group_by(bulan) |> 
  summarise(
    actual=sum(actual),
    base_ensemble=sum(base_ensemble),
    mint_Fc=sum(mint_Fc),
    internal_Fc_KPP=sum(internal_Fc_KPP)) |> 
  bind_cols(FC_PKP_CPIN |> 
                 filter(as.Date(bulan, format = "%Y %b") >= ymd("2022-01-01"))) |> 
  mutate(MAPE_base_ensemble=abs((base_ensemble-actual)/actual*100)) |> 
  mutate(MAPE_mint_Fc=abs((mint_Fc-actual)/actual*100)) |> 
  mutate(MAPE_internal_Fc_CPIN=abs((realisasi-actual)/actual*100)) |> 
  select (bulan...1,MAPE_base_ensemble,MAPE_mint_Fc,MAPE_internal_Fc_CPIN) |> 
  mutate(level="NAS-bulanan")


comparation_NAS_Q<-compare_Quarter |> 
  group_by(Tahun,TW) |> 
  summarise(
    actual=sum(actual),
    base_ensemble=sum(base_ensemble),
    mint_Fc=sum(mint_Fc),
    internal_Fc_KPP=sum(internal_Fc_KPP)
  ) |> 
  mutate(MAPE_base_ensemble=abs((base_ensemble-actual)/actual*100)) |> 
  mutate(MAPE_mint_Fc=abs((mint_Fc-actual)/actual*100)) |> 
  mutate(MAPE_internal_Fc_KPP=abs((internal_Fc_KPP-actual)/actual*100))|> 
  mutate(level="NAS-Quarteran")

manual_comparison_Quarteran<-bind_rows(comparation_KPP,
                                     comparation_KWL,
                                     comparation_NAS)|> 
  group_by(level,bulan) |> 
  summarise(MAPE_base_ensemble=mean(MAPE_base_ensemble),
            MAPE_mint_Fc=mean(MAPE_mint_Fc),
            MAPE_internal_Fc_CPIN=mean(MAPE_internal_Fc_CPIN,na.rm=TRUE)
  ) |> 
  mutate(level_bulan=paste(level, bulan, sep = "_")) 


write_xlsx(manual_comparison_bulanan, "D:\\One-Drive UI\\OneDrive - UNIVERSITAS INDONESIA\\TESIS\\e-riset DJP\\0\\final push\\model_bulanan_MAPE_manual_2023.xlsx")




manual_comparison_Quarteran<-bind_rows(comparation_KPP_Q,
                                       comparation_KWL_Q,
                                       comparation_NAS_Q)|> 
  group_by(level,Tahun,TW) |> 
  summarise(MAPE_base_ensemble=mean(MAPE_base_ensemble),
            MAPE_mint_Fc=mean(MAPE_mint_Fc),
            MAPE_internal_Fc_KPP=mean(MAPE_internal_Fc_KPP,na.rm=TRUE)
  ) |> 
  mutate(level_bulan=paste(level, Tahun,TW, sep = "_")) 


write_xlsx(manual_comparison_Quarteran, "D:\\One-Drive UI\\OneDrive - UNIVERSITAS INDONESIA\\TESIS\\e-riset DJP\\0\\final push\\model_Quarteran_MAPE_manual_2023.xlsx")


}
library(dplyr)
library(rvest)
library(tidyverse)
library(scales)

#taking links from "https://bkm.com.tr" for every period.
X=c()

for (i in c(1:12)) {
  for (k in c(2010:2018)){
    month=i
    year=k
    link_by_date=paste(paste(paste("https://bkm.com.tr/secilen-aya-ait-sektorel-gelisim/?filter_year=",year,sep=""),"&filter_month=",month,sep=""),
               "&List=Listele",sep="")
    X=append(X, link_by_date)
  }
}

#for adding year and month columns.
year=c()

i=2010
k=1
while (i<=2018){
  year=append(year,i)
  print(year[k])
  i=i+1
  k=k+1
}

month=c()
i=1
k=1
while (i<=12){
  month=append(month,i)
  print(month[k])
  i=i+1
  k=k+1
}

Tarih <- crossing(month, year)

#importing HTML.
Y<-lapply(X, read_html)


#merging files and adding year and month coulmns.

df=data.frame()
for (i in c(1:length(Y))){
  A<-html_table(Y[[i]],header = FALSE, trim = TRUE, fill = TRUE, dec="") [4][[1]][-(1:2),]
  A<- A %>% mutate(Yıl=Tarih$year[i], Ay=Tarih$month[i])
  df=bind_rows(df,A)
}

#to solve "NAs introduced by coercion" problem.
df$X2=as.numeric(gsub("\\.","",df$X2))
df$X3=as.numeric(gsub("\\.","",df$X3))


A=gsub("\\.","",df$X4)
df$X4=as.numeric(gsub("\\,",".",A))

B=gsub("\\.","",df$X5)
df$X5=as.numeric(gsub("\\,",".",B))

df_new <- df

#tidying data
colnames(df_new)=c("Isyeri_Grubu","KK_Islem_Adedi","BK_Islem_Adedi","KK_Islem_Tutari","BK_Islem_Tutari","Yil","Ay")

df_new <- df_new %>% filter(`Isyeri_Grubu`!= "TOPLAM") 

#Analysis

#Seasonal Spending

df_market_adet <- df_new %>% 
  filter(grepl("MARKET",Isyeri_Grubu)) %>%
  mutate(
    sezon = case_when(
      Ay %in% 10:12 ~ "Fall",
      Ay %in%  1:3  ~ "Winter",
      Ay %in%  4:6  ~ "Spring",
      TRUE ~ "Summer")) %>%
  group_by( sezon) %>%
  summarise(Toplam_KK_Adedi=sum(KK_Islem_Adedi),Toplam_BK_Adedi=sum(BK_Islem_Adedi))

df_market_tutar <- df_new %>% 
  filter(grepl("MARKET",Isyeri_Grubu)) %>%
  mutate(
    sezon = case_when(
      Ay %in% 10:12 ~ "Fall",
      Ay %in%  1:3  ~ "Winter",
      Ay %in%  4:6  ~ "Spring",
      TRUE ~ "Summer")) %>%
  group_by( sezon) %>%
  summarise(Ortalama_KK_Tutari=mean(KK_Islem_Tutari),Ortalama_BK_Tutari=mean(BK_Islem_Tutari)) 


  ggplot(df_market_tutar, aes(x=sezon)) + 
  geom_bar(position="dodge", stat="identity", aes(y = Ortalama_KK_Tutari )) +
  scale_y_continuous(labels = comma) +
  labs(subtitle="Sezonlara Göre Kredi Karti (TL)", title= "BKM",
       caption="(based on data from BKM)", y="Ortalama KK Islem Tutari", x="Sezon")
  
  ggplot(df_market_adet, aes(x=sezon)) + 
    geom_bar(position="dodge", stat="identity", aes(y = Toplam_KK_Adedi )) +
    scale_y_continuous(labels = comma) +
    labs(subtitle="Sezonlara Göre Kredi Karti (TL)", title= "BKM",
         caption="(based on data from BKM)", y="Toplam KK Islem Adedi", x="Sezon")
  
  ggplot(df_market_tutar, aes(x=sezon)) + 
    geom_bar(position="dodge", stat="identity", aes(y = Ortalama_BK_Tutari )) +
    scale_y_continuous(labels = comma) +
    labs(subtitle="Sezonlara Göre Banka Islem Tutari (TL)", title= "BKM",
         caption="(based on data from BKM)", y="Ortalama BK Islem Tutari", x="Sezon")
  
  ggplot(df_market_adet, aes(x=sezon)) + 
    geom_bar(position="dodge", stat="identity", aes(y = Toplam_BK_Adedi )) +
    scale_y_continuous(labels = comma) +
    labs(subtitle="Sezonlara Göre Banka Islem Adedi (TL)", title= "BKM",
         caption="(based on data from BKM)", y="Toplam BK Islem Adedi", x="Sezon")

#Seasonal Deviation

market <- df_new %>% filter(grepl("MARKET",Isyeri_Grubu)) %>%
  mutate(
    sezon = case_when(
      Ay %in% 10:12 ~ "Fall",
      Ay %in%  1:3  ~ "Winter",
      Ay %in%  4:6  ~ "Spring",
      TRUE ~ "Summer")) %>%
  group_by( sezon) %>%
  summarise(Toplam_Kredi_Karti=sum(KK_Islem_Tutari)) %>% 
  mutate(Ortalama_Kredi_Karti=mean(Toplam_Kredi_Karti),Standart_Sapma=sd(Toplam_Kredi_Karti))

sezonsal <- market %>% mutate(Fark=round((Toplam_Kredi_Karti - Ortalama_Kredi_Karti)/Standart_Sapma, 2)) %>% 
  mutate(tur = if_else(Fark<0,"below","above"))

ggplot(sezonsal, aes(x=reorder(sezon,Fark), y=Fark, label=Fark)) + 
  geom_bar(stat='identity', aes(fill=tur), width=.5)  +
  scale_fill_manual(name="Mileage", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(subtitle="Sektor Bazinda Kredi Karti Islem Tutari (TL)", title= "BKM",
       caption="(based on data from BKM)", y="FARK", x="SEKTOR") +
  coord_flip()








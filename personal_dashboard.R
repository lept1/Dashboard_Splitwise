library(arrow)
library(magrittr)
library(tidyverse)
library(ggplot2)
renv::load('~/Projects/TopicModelSoftware/res/script')
data<-arrow::read_csv_arrow('./cicciobombi_2023-02-27_export.csv')

data%<>%mutate(across(everything(),tolower)) %>%
        select(Data,Descrizione,Categorie,Costo,Valuta
                )%>%
        mutate(Costo=as.numeric(Costo)
               )%>%
        mutate(Data=lubridate::as_date(Data)
               )%>%
        filter(Categorie!='pagamento' & Categorie!='affitto')%>%
        mutate(Descrizione=
                 case_when(
                          Descrizione!='sede roma' & Categorie=='autobus/treno' ~ 'biglietti gite',
                          TRUE ~ Descrizione)
               )%>%
        mutate(Descrizione=
                 case_when(
                          Categorie=='parcheggio' & Descrizione!='sede roma' ~ 'parcheggio gite',
                          TRUE ~ Descrizione)
               )%>% 
        mutate(Mese=format(as.Date(Data), "%Y-%m"))

#data%<>%filter(Mese>'2022-09')              
data%>%
  filter(Categorie=='autobus/treno' | Categorie=='parcheggio')%>%
  group_by(Descrizione,Mese)%>%
  summarise(Costo=sum(Costo))%>%
  ggplot(aes(x = Costo, y = Mese,fill=Descrizione))+
  geom_bar(stat = 'identity',position = 'dodge')+
  xlab('Costo')+
  ylab('Data')+
  #scale_x_continuous(breaks=seq(0,20,1)) +
  theme_minimal()+
  theme(axis.text = element_text(colour = 'steelblue',
                                 size=10),
        axis.title=element_text(colour = 'steelblue',
                                size=18))
 
data%>%
  filter(Descrizione=='sede roma')%>%
  group_by(Mese)%>%
  summarise(Totale=sum(Costo))%>%
  ggplot(aes(y = Mese, x = Totale)) + 
  geom_bar(stat = 'identity',show.legend = FALSE,fill='steelblue')+
  ylab('Mese')+
  xlab('Totale (eur)')+
  #scale_x_continuous(breaks=seq(0,20,1)) +
  theme_minimal()+
  theme(axis.text = element_text(colour = 'steelblue',
                                 size=10),
        axis.title=element_text(colour = 'steelblue',
                                size=18))

data%>%
  filter(Descrizione=='sede roma')%>%
  summarise(Totale=sum(Costo))

data%>%
  filter(Mese>='2023-01')%>%
  filter(Categorie=='ristorante') %>%
  summarise(Totale=sum(Costo))

data%>%
  filter(Categorie=='regali')%>%
  summarise(Totale=sum(Costo))

data%>%
  filter(Categorie=='casalinghi')%>%
  summarise(Totale=sum(Costo))

data%>%
  filter(Categorie=='autobus/treno'&Descrizione!='sede roma')%>%
  summarise(Totale=sum(Costo))

data%>%
  filter(Categorie=='parcheggio')%>%
  summarise(Totale=sum(Costo))
  
data%>%
  count(Categorie) %>% 
  rename(How_many=n)%>%
  ggplot(aes(y = reorder(Categorie,How_many), x = How_many,fill=-How_many)) + 
    geom_bar(stat = 'identity',show.legend = FALSE)+
    ylab('Categorie')+
    xlab('How many')+
    #scale_x_continuous(breaks=seq(0,20,1)) +
    theme_minimal()+
    theme(axis.text = element_text(colour = 'steelblue',
                                   size=10),
          axis.title=element_text(colour = 'steelblue',
                                  size=18))


data %>% summarise(Totale=sum(Costo), MedianaCosto=median(Costo),MediaCosto=mean(Costo),Transazioni=n())
data %>% 
  group_by(Mese) %>%
  summarise(Totale=sum(Costo), MedianaCosto=median(Costo),MediaCosto=mean(Costo),Transazioni=n())%>%
  ggplot(aes(y = Mese, x = Totale)) + 
  geom_bar(stat = 'identity',show.legend = FALSE,fill='steelblue')+
  ylab('Mese')+
  xlab('Totale (eur)')+
  #scale_x_continuous(breaks=seq(0,20,1)) +
  theme_minimal()+
  theme(axis.text = element_text(colour = 'steelblue',
                                 size=10),
        axis.title=element_text(colour = 'steelblue',
                                size=18))


data%>%
  group_by(Data)%>%
  summarise(dailyCost=sum(Costo)) %>% 
  ungroup()%>%
  ggplot()+
      geom_line(aes(x=Data,y=dailyCost))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  scale_x_date(date_breaks = "4 week", date_minor_breaks = "1 week",date_labels = "%b %d")

data%>%
  group_by(Data)%>%
  summarise(dailyCost=sum(Costo)) %>% 
  ungroup()%>%
  mutate(CostoTotale = cumsum(dailyCost))%>%
  ggplot()+
    geom_point(aes(x=Data,y=CostoTotale),color='steelblue',size=1.5)+
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
    scale_x_date(date_breaks = "4 week", date_minor_breaks = "1 week",date_labels = "%b %d")

## AGGIUUGERE CATEGORIA SEDE ROMA
cost_by_categories<-data%>%group_by(Categorie)%>%summarise(categoriesCost=sum(Costo)) %>% ungroup() #%>% mutate(dailyCost=scales::rescale(dailyCost))
cost_by_categories%>%
  ggplot(aes(y = reorder(Categorie,categoriesCost), x = categoriesCost,fill=-categoriesCost)) + 
  geom_bar(stat = 'identity',show.legend = FALSE)+
  ylab('Categorie')+
  xlab('Euros €')+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  theme_minimal()+
  theme(axis.text = element_text(colour = 'steelblue',
                                 size=10),
        axis.title=element_text(colour = 'steelblue',
                                size=18))




data%>%group_by(Mese,Categorie)%>%summarise(categoriesCost=sum(Costo))%>%ungroup()%>%
ggplot(aes(y = reorder(Categorie,categoriesCost), x = categoriesCost,fill=-categoriesCost)) + 
  geom_bar(stat = 'identity',show.legend = FALSE)+
  ylab('Categorie')+
  xlab('Euros €')+
  #scale_x_continuous(breaks=seq(0,20,1)) +
  theme_minimal()+
  theme(axis.text = element_text(colour = 'steelblue',
                                 size=8),
        axis.title=element_text(colour = 'steelblue',
                                size=18))+
  facet_wrap(~Mese)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5))
  
costiRoma <-data%>% filter(Descrizione=='sede roma') %>% summarise (categoriesCost=sum(Costo))
costiCasa <-data%>% filter(Descrizione=='biglietti casa' | Descrizione=='pedaggio casa' ) %>% summarise (categoriesCost=sum(Costo))
costiGite <-data%>% filter(Descrizione=='biglietti gite' | Descrizione=='parcheggio gite' ) %>% summarise (categoriesCost=sum(Costo))

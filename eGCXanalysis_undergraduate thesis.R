#Load foreign package for R to be able to read SPSS file
#Load pakcage 'foreign' agar R bisa membaca dataset SPSS
library(foreign)

#Load gmodels for contingency table used in chi-square test
#Load package 'gmodels' agar R bisa buat tabel kontingensi uji chi-square
library(gmodels)

#Load grid and vcd packages for mosaic plot visualization
#Load package 'grid' dan 'vcd' berguna untuk visualisasi hasil uji chi-square
library(grid)
library(vcd)

#Load tidyverse, ggplot2 and ggpubr packages
#Load package di bawah berguna untuk visualisasi
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(corrplot)
library(gridExtra)
library(sjPlot)
library(knitr)
library(ggExtra)


#Import data from SPSS
#Data diimport dari komputer ke R
mydata <- read.spss("~/Desktop/090621_eGCXanalysis.sav", use.value.labels = TRUE,
                    to.data.frame = TRUE)


#DATA ANALYSIS FOR CATEGORICAL DATA AND GRAPHS
#ANALISIS DATA KATEGORIK-KATEGORIK DAN VISUALISASINYA
###Chi-square test eLFG-Gender###
#Melakukan uji chi-square untuk variabel eLFG-Jenis kelamin
CrossTable(mydata$Gender, mydata$eLFGCat) #output contingency table

chisq.test(x=mydata$Gender, y=mydata$eLFGCat) #output p-value

###Visualisation of chi-square test###
#Visualisasi hasil uji chi-square berupa mosaic plot

###mosaic plot eLFG-Gender###
mosaic( ~ Gender + eLFGCat, data = mydata,
            labeling_args = list(set_varnames = c(Gender = 'Jenis kelamin (%)',
                                                  eLFGCat = 'Kelompok')),
            set_labels = list(Gender = c('laki-laki', 'perempuan'),
                              eLFGCat = c('G1', 'G2')),
        main = 'Jenis kelamin kelompok sampel',
        highlighting = 'eLFGCat', highlighting_fill = c('#96edc7', '#b8dfd8')
        )

###mosaic plot eLFG-lamaDm###
mosaic( ~ LamaDMCat + eLFGCat, data = mydata,
        labeling_args = list(set_varnames = c(LamaDMCat = 'Durasi menderita DM (%)',
                                              eLFGCat = 'Kelompok')),
        set_labels = list(LamaDMCat = c('< 5 tahun', '≥ 5 tahun'),
                          eLFGCat = c('G1', 'G2')),
        main = 'Durasi menderita DM kelompok sampel',
        highlighting = 'eLFGCat', highlighting_fill = c('#96edc8', '#eeeeee')
      )

###mosaic plot eLFG-rutinitas OR###
mosaic( ~ Olahraga + eLFGCat, data = mydata,
        labeling_args = list(set_varnames = c(Olahraga = 'Rutinitas olahraga (%)',
                                              eLFGCat = 'Kelompok')),
        set_labels = list(Olahraga = c('Tidak rutin', 'Rutin'),
                          eLFGCat = c('G1', 'G2')),
        main = 'Rutinitas olahraga kelompok sampel',
        highlighting = 'eLFGCat', highlighting_fill = c('#96edc7', '#b8dfd8')
)

###mosaic plot eLFG-kebiasaan merokok###
mosaic( ~ Merokok + eLFGCat, data = mydata,
        labeling_args = list(set_varnames = c(Merokok = 'Kebiasaan merokok (%)',
                                              eLFGCat = 'Kelompok')),
        set_labels = list(Merokok = c('Tidak', 'Ya'),
                          eLFGCat = c('G1', 'G2')),
        main = 'Kebiasaan merokok kelompok sampel',
        highlighting = 'eLFGCat', highlighting_fill = c('#96edc8', '#eeeeee')
)

###mosaic plot eLFG-HbA1c###
mosaic( ~ Hba1ccat + eLFGCat, data = mydata,
        labeling_args = list(set_varnames = c(Hba1ccat = 'HbA1c (%)',
                                              eLFGCat = 'Kelompok')),
        set_labels = list(Hba1ccat = c('Terkontrol', 'Tidak terkontrol'),
                          eLFGCat = c('G1', 'G2')),
        main = 'HbA1c kelompok sampel',
        highlighting = 'eLFGCat', highlighting_fill = c('#50c878', '#d0f0c0')
)

###mosaic plot eLFG-UACR###
mosaic( ~ UACRcat + eLFGCat, data = mydata,
        labeling_args = list(set_varnames = c(UACRcat = 'UACR (mg/g) (%)',
                                              eLFGCat = 'Kelompok')),
        set_labels = list(UACRcat = c('Normoalbuminuria', 'Albuminuria'),
                          eLFGCat = c('G1 (n = 33)', 'G2 (n = 42)')),
        main = 'UACR kelompok sampel',
        highlighting = 'eLFGCat', highlighting_fill = c('#b3e4c4', '#76cf95')
)

#DATA ANALYSIS FOR CATEGORICAL-NUMERICAL DATA AND GRAPHS
#ANALISIS DATA KATEGORIK-NUMERIK DAN VISUALISASINYA
###e-LFG-usia TESTING###
#Descriptive for eLFG-usia
mydata %>%
  group_by(eLFGCat) %>%
  summarise_each(funs(min, max, mean, median, sd), Usia)

#Mann-Whitney test for eLFG-usia
wilcox.test(mydata$Usia~ mydata$eLFGCat, 
            exact=FALSE)
#source: https://methods.sagepub.com/dataset/howtoguide/mann-whitney-bcs-2007-8-r

#Visualization for eLFG-usia
ggboxplot(mydata, x='eLFGCat', y='Usia',
          desc_stat='median_iqr', color = '#434343', width = 0.2,
          add = 'jitter',
          palette = 'jco') +
  stat_compare_means(method = 'wilcox.test', 
                     label.x.npc = c('left')) +
  labs(y='Usia (tahun)', x='Kelompok',
       title = 'Perbandingan usia')+
  scale_x_discrete(labels=c('>= 90' = 'G1', '60-89' = 'G2'))

###eLFG-IMT TESTING###
#Descriptive for eLFG-IMT
mydata %>%
  group_by(eLFGCat) %>%
  summarise_each(funs(min, max, mean, median, sd), IMT)

#t-test for eLFG-IMT
t.test(mydata$IMT~ mydata$eLFGCat, 
            exact=FALSE)
#source: https://methods.sagepub.com/dataset/howtoguide/mann-whitney-bcs-2007-8-r

#Visualization for eLFG-IMT
ggboxplot(mydata, x='eLFGCat', y='IMT',
          desc_stat='mean_sd', color = '#5e8b7e', width = 0.2,
          add = 'jitter',
          palette = 'jco') +
  stat_compare_means(method = 't.test', 
                     label.x.npc = c('middle')) +
  geom_hline(yintercept = 25, color = '#c84b31', size = 0.5)+
  labs(y='IMT (kg/m^2)', x='Kelompok',
       title = 'Perbandingan Indeks Massa Tubuh')+
  scale_x_discrete(labels=c('>= 90' = 'G1', '60-89' = 'G2'))

###eLFG-Sistol TESTING###
#Descriptive for eLFG-Sistol
mydata %>%
  group_by(eLFGCat) %>%
  summarise_each(funs(min, max, mean, median, sd), Sistol)

#Mann-Whitney test for eLFG-Sistol
wilcox.test(mydata$Sistol~ mydata$eLFGCat, 
            exact=FALSE)
#source: https://methods.sagepub.com/dataset/howtoguide/mann-whitney-bcs-2007-8-r

#Visualization for eLFG-sistol
ggboxplot(mydata, x='eLFGCat', y='Sistol',
          desc_stat='median_iqr', color = '#434343', width = 0.2,
          add = 'jitter',
          palette = 'jco') +
  stat_compare_means(method = 'wilcox.test', 
                     label.x.npc = c('left')) +
  geom_hline(yintercept = 120, color = '#c84b31', size = 0.5)+
  labs(y='Tekanan sistol (mmHg)', x='Kelompok)',
       title = 'Perbandingan tekanan sistol')+
  scale_x_discrete(labels=c('>= 90' = 'G1', '60-89' = 'G2'))

###eLFG-Diastol TESTING###
#Descriptive for eLFG-Diastol
mydata %>%
  group_by(eLFGCat) %>%
  summarise_each(funs(min, max, mean, median, sd), Diastol)

#Mann-Whitney test for eLFG-Diastol
wilcox.test(mydata$Diastol~ mydata$eLFGCat, 
            exact=FALSE)
#source: https://methods.sagepub.com/dataset/howtoguide/mann-whitney-bcs-2007-8-r

#Visualization for eLFG-diastol
ggboxplot(mydata, x='eLFGCat', y='Diastol',
          desc_stat='median_iqr', color = '#5e8b7e', width = 0.2,
          add = c('jitter', 'green'),
          palette = 'jco') +
  stat_compare_means(method = 'wilcox.test', 
                     label.x.npc = c('middle', 'middle')) +
  geom_hline(yintercept = 80, color = '#c84b31', size=0.5)+
  labs(y='Tekanan diastol (mmHg)', x='Kelompok',
       title = 'Perbandingan tekanan diastol')+
  scale_x_discrete(labels=c('>= 90' = 'G1', '60-89' = 'G2'))

#DATA ANALYSIS FOR NUMERICAL-NUMERICAL DATA AND GRAPH
#ANALISIS DATA NUMERIK-NUMERIK DAN VISUALISASINYA
###eLFG-eGCX TESTING###
#Descriptive for eLFG-eGCX
#Statistik deskriptif untuk eLFG-eGCX
mydata %>%
  group_by(eLFGCat) %>%
  summarise_each(funs(min, max, mean, median, sd), eGCXCr)

#Mann-Whitney test for eLFG-Diastol
wilcox.test(mydata$eGCXCr~ mydata$eLFGCat, 
            exact=FALSE)
#source: https://methods.sagepub.com/dataset/howtoguide/mann-whitney-bcs-2007-8-r

#Visualisation for eLFG-eGCX
#Visualisasi data num-num berupa scatterplot
ggscatter(mydata, x='eLFG', y='eGCXCr',
          color = '#434343',
          add = 'reg.line', conf.int = TRUE,
          palette = 'jco') +
  stat_cor(method = 'spearman', label.x = 94, label.y = 14) +
  #geom_vline(xintercept = 90, color = '#434343', linetype = 'dashed', size=1)+
  labs(y='degradasi glikokaliks urin (mg/g Cr)', x='eLFG (ml/min/1,73m^2)',
       title = 'Korelasi degradasi glikokaliks urin 
       dengan laju filtrasi glomerulus')

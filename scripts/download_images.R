install.packages("ridigbio")
install.packages("downloader")
library("downloader")

library(ridigbio)

oc_df <- read.csv("idigbio/occurrence.csv")
med_df <- read.csv("idigbio/multimedia.csv")

df <- idig_search_media(rq=list(genus="acer"), 
                        mq=list("data.ac:accessURI"=list("type"="exists")), 
                        fields=c("uuid","data.ac:accessURI"), limit=10)


iurl<-med_df$ac.accessURI
names<-med_df$idigbio.uuid
for (i in 1:nrow(med_df)) {
  download(iurl[i], destfile=paste0('data', names[i],'.jpg'), mode = 'wb')
}


hesperiidae <- read.csv("data/iNat_families/hesperiidae_na.csv", stringsAsFactors = F)
pieridae <- read.csv("data/iNat_families/pieridae_na.csv", stringsAsFactors = F)  
lycaenidae <- read.csv("data/iNat_families/lycaenidae_na.csv", stringsAsFactors = F)
papilionidae <- read.csv("data/iNat_families/papilionidae_na.csv", stringsAsFactors = F)
nymphalidae <- read.csv("data/iNat_families/nymphalidae_na.csv", stringsAsFactors = F)
riodinidae <- read.csv("data/iNat_families/riodinidae_na.csv", stringsAsFactors = F)

inaturalist <- rbind(hesperiidae, pieridae, lycaenidae, papilionidae, nymphalidae, riodinidae)

# make df

library(readr)
library("downloader")
observations_37232 <- read_csv("~/Downloads/observations-37232.csv")
iurl<-observations_37232$image_url
names<-observations_37232$id
for (i in 1:length(url)) {
  download(iurl[i], destfile=paste0('/Users/robgur/Desktop/Acermacrophyllum/', names[i],'.jpg'), mode = 'wb')
}

a_celtis <- dplyr::filter(inaturalist, scientific_name == "Asterocampa celtis")

iurl <- a_celtis$image_url
names <- a_celtis$id

for (i in 1:nrow(a_celtis)) {
  download(iurl[i], destfile=paste0('clean_butts/Asterocampa_celtis', names[i],'.jpg'), mode = 'wb')
}

e_clarus <- dplyr::filter(inaturalist, scientific_name == "Epargyreus clarus")

iurl <- e_clarus$image_url
names <- e_clarus$id

for (i in 1:nrow(e_clarus)) {
  download(iurl[i], destfile=paste0('clean_butts/Epargyreus clarus', names[i],'.jpg'), mode = 'wb')
}

# download directly from inat
library(rinat)

inat_obs <- get_inat_obs(query = "Lepidoptera", bounds = c(35, -100, 55, -65), maxresults = 500)

iurl<-inat_obs$image_url
names<-inat_obs$id
for (i in 105:length(iurl)) {
  downloader::download(iurl[i], destfile=paste0('clean_butts/random', names[i],'.jpg'), mode = 'wb')
}


library(readxl) #load data
data.asli <- read.csv("~/COVID-19.csv", header = TRUE, sep = ",")
data.asli$Country <- data.asli$Country #rename
cekduplikat <- dplyr::distinct(data.asli[, -1]) #menghapus data duplikat

aggr_plot <- VIM::aggr(cekduplikat, col=c('navyblue','red'),
                       numbers=TRUE, sortVars=TRUE,
                       labels=names(cekduplikat), cex.axis=.7,
                       gap=3, ylab=c("Histogram of missing data",
                                     "Pattern")) #visualisasi vim

sapply(cekduplikat, function(x) sum(is.na(x)))
dataclean <- na.omit(cekduplikat[, -5]) #data cleaning

write.table(dataclean, sep = ",", row.names = F,
            file = "newdataset.csv") #ekspor dataset
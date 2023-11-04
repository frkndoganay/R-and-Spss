library(readxl)
library(xlsx)
library(ggplot2)
data=read_excel("KararAgaci.xlsx")

which(data$cinsiyet =="erkek"&data$cinsiyet =="kadın")
which(data$egitim>4)
which(data$otomobil>1)

data$egitim[3]=4

data$otomobil <- ifelse(data$otomobil == 0, "yok", "var")
data$cinsiyet <- ifelse(data$cinsiyet == 1, "erkek", "kadın")
data$medenidr <- ifelse(data$medenidr == 1, "hiç evlenmedi", 
                         ifelse(data$medenidr == 2, "evli",
                                ifelse(data$medenidr == 3, "eşi öldü",
                                       ifelse(data$medenidr== 4, "boşandı", "diğer"))))

data$egitim <- ifelse(data$egitim == 1, "bir okul bitirmeyen", 
                         ifelse(data$egitim == 2, "ilköğretim",
                                ifelse(data$egitim == 3, "lise",
                                       ifelse(data$egitim == 4, "yüksekokul ve üzeri", "diğer"))))





write.xlsx(data,"duzenlenmıs_veri.xlsx")
otomobil=data$otomobil
otomobil.freq= table(otomobil)




veri=data.frame(data$otomobil)
grafik = ggplot(data = veri, aes(x = "", fill = data$otomobil)) +
  geom_bar(width = 1, stat = "count") +
  coord_polar(theta = "y") +
  labs(x = NULL, y = NULL, fill = "Otomobil Sahiplik Durumu") +
  ggtitle("Otomobil Sahiplik Durumu")




print(grafik)



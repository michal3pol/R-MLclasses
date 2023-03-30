#a 
a<-20/log(2.78)
b <- (3*a)
c <- a==b
c

#b
help(abs)

#c
v1 <- c(8:75)

#d
help.search("plot")

#e
setwd("D:\\APU\\R-MLclasses")
a <- "tablet"
save(a, file = "lab1.RData")
remove(a)
#a #error
load("lab1.RData")
a

#f
#install.packages("gridExtra") 
library(gridExtra)
data(volcano)
tbl <- tableGrob(head(volcano, n = 10))
grid.arrange(tbl, top = "Tabela z pierwszymi 10 wierszami zbioru danych volcano", bottom = "Dodatkowa linia")

#g 
vec2 <- seq(from = 1000, to = 200, by = -8)

#h
vecA <- c(50:30)
vecB <- c(4:50)
vecD <- c(vecB,vecA)
vecD

#i
nazwa <- c("Tablet Apple iPad (10 gen.) 2022 (srebrny)", 
           "Tablet Apple iPad 2021", 
           "Tablet Apple iPad (10 gen.) 2022 (rozowy)", 
           "Tablet Apple iPad Wi-Fi (gwiezdna szarosc)", 
           "Tablet Apple iPad Air 2022 Wi-Fi (gwiezdna szarosc)", 
           "Tablet Apple iPad Air 2022 Wi-Fi (ksiezycowa poswiata)", 
           "Tablet Apple iPad 2021 Wi-Fi 256GB (srebrny)", 
           "Tablet Apple iPad Pro 2021 (srebrny)", 
           "Tablet Apple iPad Pro (gwiezdna szarosc)", 
           "iPad10")
modem <- c("Wi-Fi 6", "LTE", "Wi-Fi 6", "LTE", "Wi-Fi 802", "Wi-Fi 802", "LTE", "Wi-Fi 6", "Wi-Fi 6", "Wi-Fi 6")
wyswietlacz <- c("10.9\"", "10.2\"", "10.2\"", "10.2\"", "10.9\"", "10.9\"", "10.2\"", "10.2\"", "11\"", "9.7\"")
pamiec_RAM <- c("1GB", "1GB", "1GB", "1GB", "2GB", "1GB", "2GB", "16GB", "8GB", "2GB")
pamiec_wbudowana <- c("64GB", "64GB", "64GB", "256GB", "64GB", "64GB", "16GB", "2TB", "256GB", "128GB")
cena <- c(2719, 2399, 3649, 2749, 3799, 2999, 2749, 12599, 7399, 5299)
liczba_opinii <- c(6, 17, 2, 1, 11, 13, 32, 0, 6, 8)

tablety <- data.frame(nazwa, modem, wyswietlacz, pamiec_RAM, pamiec_wbudowana, cena, liczba_opinii)
tablety
mean(tablety$cena)

#j
nowy_tablet <- c("iPad11", "LTE", "9.7\"",  "1GB", "32GB", 3999, 560)
tablety <- rbind(tablety, nowy_tablet)
tablety$cena <- as.numeric(tablety$cena) 
mean(tablety$cena)

#k
tablety$ocena <- c(6,6,6,6,6,6,6,6,0,6,6)
aggregate(cena ~ ocena, tablety, mean)

#l
nowe_tablety <- data.frame(
  nazwa = c("Tablet Apple iPad 2021(gwiezdna szarosc)",
            "Tablet Apple iPad 2022 (10 gen.) (srebrny)",
            "Tablet Apple iPad 2022 (10 gen.) (niebieski)",
            "Tablet Apple iPad (10 gen.) 2022 (zolty)"),
  modem = c("WiFi 6", "WiFi 6", "LTE", "WiFi 6"),
  wyswietlacz = c("10.2\"", "10.9\"", "10.9\"", "10.9\""),
  pamiec_RAM = c("1GB", "1GB", "1GB", "1GB"),
  pamiec_wbudowana = c("64GB", "256GB", "256GB", "256GB"),
  cena = c(2599, 3199, 3549, 4899),
  liczba_opinii = c(3, 23, 1, 0),
  ocena = c(6,6,6,0)
)
tablety <- rbind(tablety, nowe_tablety)
tablety$liczba_opinii <- as.numeric(tablety$liczba_opinii) 
xy <- aggregate(liczba_opinii ~ ocena, tablety, sum)
plot(xy$ocena, xy$liczba_opinii, xlab="Ocena", ylab="Liczba opinii")

#m 
library(ggplot2)
procentowy_udzial <- round(prop.table(tablety$ocena) * 100, 1)
wykres_kolowy <- ggplot(data = tablety, aes(x = "", fill = factor(ocena))) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Blues") +
  theme_void() +
  theme(legend.position = "bottom")
wykres_kolowy

wykres_wachlarzowy <- ggplot(data = tablety, aes(x = factor(ocena), fill = factor(ocena))) +
  geom_bar() +
  scale_fill_brewer(palette = "Blues") +
  labs(x = "Ocena", y = "Procentowy udzial") +
  theme_minimal() +
  theme(legend.position = "none")
wykres_wachlarzowy

#n
tablety$status_opinii <- cut(tablety$liczba_opinii, 
                             breaks = c(-Inf, 0, 50, 100, Inf),
                             labels = c("nie ma", "mniej niz 50 opinii", "50-100 opinii", "wiecej niz 100 opinii"))
tablety$status_opinii <- as.factor(tablety$status_opinii)

# tabela z procentowym udziałem tabletów w zależności od statusu opinii
table_prop <- prop.table(table(tablety$status_opinii)) * 100
plot_kolo <- pie(table_prop, main = "Status opinii", col = rainbow(nrow(table_prop)))

#o
for (i in 1:nrow(tablety)) {
  zdanie <- paste(tablety$nazwa[i], " ma ocene klientow ", 
                  tablety$ocena[i], " bo ma liczbe opinii ", 
                  tablety$liczba_opinii[i], ".")
  print(zdanie)
}

#p
write.csv(tablety, "tablety.csv")
tabletyCSV <- read.csv("tablety.csv")
tabletyCSV


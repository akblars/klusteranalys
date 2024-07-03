# Analys av socioekonomiska kluster i Halland!

options(scipen=999)

# Paket vi behöver läsa in ----
library(readxl)
library(factoextra)
library(tidyverse)
library(dendextend)
library(writexl)
library(sf)
library(ggplot2)
library(tmap)
library(tmaptools)
library(leaflet)
library(scales)
library(NbClust)
library(gghighlight)
library(ggiraph)

## Gör ett karttema för grafen vi gör senare ---

theme_halland_map <- function() {
  theme(
    axis.line = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.text = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y =element_blank(),
    axis.title = element_blank(),
    axis.title.x =element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    legend.background = element_rect(color = NA, fill = "transparent"),
    legend.position = "right",
    legend.direction = "vertical",
    legend.key = element_rect(fill = "transparent", color = NA),
    legend.key.height = unit(1.2, "lines"),
    legend.key.width = unit(1.2, "lines"),
    legend.text.align = NULL,
    legend.text.color = NULL,
    legend.title = element_text(size = 9.5, face = "bold", family = "Roboto Condensed", color = "#00495D"),
    legend.title.color = NULL,
    legend.spacing.y = unit(0.3, "cm"),
    legend.text = element_text(size = 8, family = "Roboto", color = "#00495D", angle = 0),
    plot.title = element_text(family = "Roboto Condensed", size = 14, color = "#003D4C", face = "bold", hjust = 0, margin = margin(b = 5)), ## Ändra hjust till 0 för titel till vänster, 0,5 för att få den i mitten
    plot.subtitle = element_text(family = "Roboto", size = 10, color = "#00495D", hjust = 0, margin = margin(b = 0)), ## Ändra hjust till 0 för titel till vänster, 0,5 för i mitten
    plot.caption = element_text(family = "Roboto", size = 8.5, color = "#00495D", hjust = 1, margin = margin(t = 10)),
    panel.background = element_rect(fill = "#FFFFFF", color = NA),
    plot.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(5, 5, 0, 5), 
  )
}


# Datainhämtning och bearbetning ----
socioekonomi_grunddata <- read_excel("socioekonomi_halland2023.xlsx") #Läser in socioekonomisk data för Halland på deso-nivå

socioekonomisk_data <- select(socioekonomi_grunddata, -c(deso_namn, deso_typ, Kommun, befolkning))

socioekonomisk_data_num <- socioekonomisk_data  %>% remove_rownames %>% column_to_rownames(var="deso") #När vi gör kluster vill vi bara ha numerisk data, som ska utgöra klustren. Variabeln med deso-kod gör vi istället till row name. Vi vill dock ha kvar variabeln med deso_kod, så vi gör ett nytt dataset.

socioekonomisk_data_num <- scale(socioekonomisk_data_num) #Eftersom vår data har olika skalor, får vi standardisera den. Sen är klusterdatan klar för analys.

deso_data <- st_read("kartfiler/DeSO_2018_v2.gpkg", stringsAsFactors = FALSE) #Läser in kartdata för att senare kunna visualisera resultaten på karta


# PCA ----

socioekonomi_pca <- prcomp(socioekonomisk_data_num, center = TRUE,scale. = TRUE) # Här gör vi en PCA över vårt dataset

summary(socioekonomi_pca) # Här ser vi att det finns fem principal components som förklarar variationer i datan. De två första komponenterna förklarar exempelvis 88 % av variationen i datan.

biplot(socioekonomi_pca) # Här plottar vi ut var deso-områdena befinner sig utifrån dessa två komponenter, och i förhållande till variablerna. Med hjälp av lite lokalkunskap om Halland, tycks det vara så att PC1 är en socioekonomisk dimension. PC2 skulle kunna vara en stad/land-dimension. 


# Faktorsanalys ----

socioekonomi_fa <- factanal(socioekonomisk_data_num, factors = 2) # Faktorsanalys

socioekonomi_fa # Trycker ut resultaten

# Klusteranalays ----

# Skapa distansmått + dendrogram

distans <- dist(socioekonomisk_data_num, method="euclidean") # Här skapar vi ett distansmått, som vi sedan ska mata in i klusteranalysen

# Gör klusteranalysen

klusteranalys <- hclust(distans, method = "ward.D2") #Gör analysen

dend1 <- as.dendrogram(klusteranalys) # Gör ett dendrogram

plot(dend1) # Plottar ut dendrogrammet. Sex kluster tycks vara en rimlig indelning!

palette <- c("#6ca2d5", "#004c93", "#438011", "#c8d556", "#fdb713", "#910000") #Skapa egen färgpalett (valfritt), behöver justeras beroende på klustertilldelning

dend1 <- dend1 %>%
  color_branches(k = 6, col = palette) %>% #k anger antal kluster
  set("labels_cex", 0.7)

plot(dend1) # Plottar ut diagrammet igen. Nu med färger baserat på kluster.

klusteranalys_6 <- cutree(klusteranalys, k = 6) # ta fram en variabel som delar in områdena i sex kluster 

fviz_cluster(list(data = socioekonomisk_data_num, cluster = klusteranalys_6))  # Tryck ut det i ett diagram

socioekonomisk_data$klustertillhörighet <- klusteranalys_6 # Lägg till klustertillhörighetsvariabeln till vårt ostandardiserade dataset
socioekonomi_grunddata$klustertillhörighet <- klusteranalys_6 # Lägg till klusterillhörigetsvariabeln till vårt dataset där vi har namn, kommun, o.s.v. på områdena


socioekonomisk_data <- socioekonomisk_data %>% # Klusterna är inte i "ordning" efter socioekonomi, så det ändrar vi
  mutate(klustertillhörighet = case_when(klustertillhörighet == 6 ~ 1,
                                         klustertillhörighet == 3 ~ 2,
                                         klustertillhörighet == 1 ~ 3,
                                         klustertillhörighet == 2 ~ 4,
                                         klustertillhörighet == 4 ~ 5,
                                         klustertillhörighet == 5 ~ 6))

socioekonomi_grunddata <- socioekonomi_grunddata %>% # Klusterna är inte i "ordning" efter socioekonomi, så det ändrar vi
  mutate(klustertillhörighet = case_when(klustertillhörighet == 6 ~ 1,
                                         klustertillhörighet == 3 ~ 2,
                                         klustertillhörighet == 1 ~ 3,
                                         klustertillhörighet == 2 ~ 4,
                                         klustertillhörighet == 4 ~ 5,
                                         klustertillhörighet == 5 ~ 6))



# Gör en trevlig och interaktiv karta ----

socioekonomi_karta <- inner_join(deso_data, socioekonomisk_data)  #sammanfogar datan med kartfilen

socioekonomi_karta$klustertillhörighet <- as.factor(socioekonomi_karta$klustertillhörighet)

socioekonomi_karta$klustertillhörighet <- factor(socioekonomi_karta$klustertillhörighet, levels=c("1" ,"2", "3", "4", "5", "6"), labels=c("1 - Låg socioekonomisk nivå", "2", "3", "4", "5", "6 - Hög socioekonomisk nivå"))

tooltip_css <- "background-color:black; color:white; padding:5px; border-radius:3px; text-align:left; line-height:20px; font-family: Roboto"

klusterkarta <- ggplot(socioekonomi_karta)+
  geom_sf_interactive(aes(fill=klustertillhörighet, tooltip = paste0("<b>Deso</b>: ", deso, 
                                                                     "<br><b>Kommun:</b> ", kommunnamn, 
                                                                     "<br><b>Hushållsinkomst:</b> ", inkomst, 
                                                                     "<br><b>Andel högutbildade:</b> ", utbildning, "%", 
                                                                     "<br><b>Unga i hushåll  med låg ekonomisk standard:</b> ", ung_stand, "%",
                                                                     "<br><b>Ohälsotal:</b> ", ohals,
                                                                     "<br><b>Aktivitetsgrad:</b> ", forvarvsarb_stud, "%")))+
  geom_sf(fill = "transparent", color = "black", linewidth = 0.5, 
          data = . %>% group_by(kommun) %>% dplyr::summarize()) +
  scale_fill_brewer(palette="Blues")+
  coord_sf(datum = NA)+
  theme_halland_map()+
  labs(fill="Kluster",
       title="Socioekonomiska kluster i Halland",
       subtitle="DeSO indelade i kluster baserat på socioekonomiska förutsättningar",
       caption="Källa: Region Hallands bearbetningar av statistik från SCB")

girafe(ggobj = klusterkarta,
       options = list(opts_tooltip(css = tooltip_css, opacity=1)))

ggsave("karta.png", plot =klusterkarta, width=17.38, height=22.33, units="cm") # Sparar ner kartan (obs ej interaktiv)

# Export av data ----

write_xlsx(socioekonomi_grunddata, "klusterdata.xlsx") #sparar ner datan som "den är"

grupperad_data <- socioekonomi_grunddata %>% # Gör ett grupperat dataset som vi sparar ner för att beräkna värden per kluster
  group_by(klustertillhörighet) %>%
  summarize(inkomst = mean(inkomst),
            utbildning = mean(utbildning),
            ung_stand = mean(ung_stand),
            ohals = mean(ohals),
            forvarvsarb_stud = mean(forvarvsarb_stud),
            befolkning = sum(befolkning),
            antal_deso=n()) %>%
  ungroup() %>%
  mutate(andel_befolkning = befolkning/sum(befolkning))

samtliga_områden <- socioekonomi_grunddata %>% 
  summarize(inkomst = mean(inkomst),
            utbildning = mean(utbildning),
            ung_stand = mean(ung_stand),
            ohals = mean(ohals),
            forvarvsarb_stud = mean(forvarvsarb_stud),
            befolkning = sum(befolkning),
            antal_deso=n()) %>%
  mutate(andel_befolkning = befolkning/sum(befolkning),
         klustertillhörighet = "Samtliga deso")

grupperad_data <- rbind(grupperad_data, samtliga_områden)

write_xlsx(grupperad_data, "grupperad_klusterdata.xlsx") 



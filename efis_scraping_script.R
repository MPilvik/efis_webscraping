# Installi ja laadi paketid

#install.packages("rvest")
library(ggplot2)
library(rvest)
library(dplyr)


# Leia lingid
main_url <- "https://www.efis.ee/et/marksonad/id/13/view/List/page/" # põhilehekülg linkidega

# Tühjad vektorid kohanimede ja veebiaadresside jaoks
places <- vector()
links <- vector()

# Kokku on 108 lehekülge linkidega
pb <- txtProgressBar(0,108,style = 3)
for(i in 1:108){ # Igal leheküljel
  temp_url <- paste0(main_url, i, "#search-results") # võta see lehekülje aadress
  temp_page <- read_html(temp_url) # ja loe lehekülg 
  temp_nodes <- html_nodes(temp_page, xpath = "/html/body/div[1]/div/div[2]/div[1]/div[1]/div/div[2]/div/div[4]/div[2]/div/div/div/ul/li/a") # Leia lehelt linke sisaldav jaotis
  temp_links <- paste0("https://www.efis.ee", html_attr(temp_nodes, "href")) # ja eralda lingid
  temp_places <- html_text(temp_nodes) # leia lehekülje kohanimed
  places <- c(places, temp_places) # lisa kohanimed vektorisse
  links <- c(links, temp_links) # lisa lingid vektorisse
  Sys.sleep(runif(1, min = 1, max = 2)) # pea pisut vahet
  setTxtProgressBar(pb, i)
}
close(pb)

# Tee tühjad vektorid kõikide lehtede pealt kogutud linkide taga peituvate andmete jaoks
allplaces <- vector()
alltitles <- vector()
allyears <- vector()
allgenres <- vector()

# Eralda iga filmi info
pb <- txtProgressBar(0,length(links),style = 3)
for(i in 1:length(links)){ # Iga lingi puhul
  cat(i, "\n") 
  temp_url <- links[i]
  temp_place <- places[i]
  temp_page <- read_html(temp_url) # loe lehekülg
  temp_nodes <- html_nodes(temp_page, xpath = "/html/body/div[1]/div/div[2]/div[1]/div[1]/div/div[2]/div[2]/div/div/ul/li") # leia leheküljel olevad filmid
  if(length(temp_nodes)>0){ # kui päring andis tulemusi
    for(j in 1:length(temp_nodes)){ # siis iga filmi kohta
      temp_node <- temp_nodes[j]
      temp_title <- html_nodes(temp_node, "h6") %>%html_text() # leia filmi nimi
      temp_info <- html_nodes(temp_node, "p.meta:not(.highlightable)") %>% html_text() # võta filmi info
      temp_year <- ifelse(grepl("^[0-9]{4}.*$", temp_info), trimws(gsub("^([^\\|]+)\\|.*$", "\\1", temp_info)), "") # eralda filmi aasta
      temp_genre <- ifelse(grepl("^[0-9]{4}.*$", temp_info), trimws(gsub("^[^\\|]+\\|([^\\|]+)\\|.*$", "\\1", temp_info)), temp_info) # eralda filmi žanr
      allplaces <- c(allplaces, temp_place) # lisa filmi kohanimi vektorisse
      alltitles <- c(alltitles, temp_title) # lisa filmi pealkiri vektorisse
      allyears <- c(allyears, temp_year) # lisa filmi aasta vektorisse
      allgenres <- c(allgenres, temp_genre) # lisa filmi žanr vektorisse
    }
    Sys.sleep(runif(1, min = 1, max = 2)) # pea pausi
    setTxtProgressBar(pb, i)
    # Kontrolli, kas kõikidelt filmidelt on saadud ühepalju infot
    # Kui ei ole,
    if(length(alltitles) != length(allplaces) | length(alltitles) != length(allgenres) | length(alltitles) != length(allyears)){
      break # siis katkesta tsükkel
    }
  }
}
close(pb)


# Pane filmide kohanimed, pealkirjad, aastad ja žanrid kokku ühte tabelisse
data.frame(koht = allplaces, pealkiri = alltitles, aasta = allyears, žanr = allgenres) -> df
unique(df) -> df # viska välja korduvad read

#write.table(df, file = "efis_kohad.csv", quote = F, sep = "\t", row.names = F, fileEncoding = "UTF-8") # kirjuta tabel faili

# Jäta tabelis alles ainult need kohad, mida esineb kõikide aastate ja filmide peale kokku vähemalt 5 korda
df %>% group_by(koht) %>% tally() %>% filter(n >= 5) %>% .$koht %>% unique() -> keep
df[df$koht %in% keep,] -> df_min5

# Muuda kohanimede vormingut: kohanimi (üldisem kohanimi) -> kohanimi, üldisem kohanimi
df_min5 %>% group_by(koht, aasta) %>% tally() %>% mutate(koht = gsub(" \\(", ", ", koht) %>% gsub("\\)", "", .)) -> df_min5

# Kirjuta tabel faili
write.table(df_min5, "efis_geokodeerimiseks.csv", quote = F, sep = ";", fileEncoding = "UTF-8", row.names = F)
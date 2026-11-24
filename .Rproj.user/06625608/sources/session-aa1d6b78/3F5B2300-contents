#R-Basiscode für die Sichtung und Datenvorbereitung

# shortcuts (Positron) ---------------------------------------------------
# Alles, was hier nicht steht, steht in der Command Palette:
# Strg + Umschalt + P, dann ein Stichwort eingeben.

# Ausführen
# Strg + Enter          => führt die Zeile an der Cursorposition aus, bzw. die Markierung
# Strg + Umschalt + Enter => führt alles von oben bis zur Cursorzeile aus
# Alt + Umschalt + Enter => führt alles von der Cursorzeile bis zum Dateiende aus
# Umschalt + Enter      => führt die ganze Datei aus (wie source())
# F1                    => Hilfe zu dem, was unter dem Cursor steht

# Struktur
# Alt + -               => fügt den Zuweisungspfeil <- ein
# Strg + Umschalt + C   => Kommentar ein- und ausschalten; mehrere markierte Zeilen
#                          auf einmal, nochmal drücken nimmt es wieder raus (Funktioniert nur mit eingeschalteten R-Studio-Tasten)

# Strg + Umschalt + N   => neue R-Datei anlegen
# Strg + Umschalt + P   => Command Palette, der schnellste Weg zu allem anderen

# Suchen
# Strg + f              => im Skript suchen
# Strg + h              => im Skript suchen und ersetzen

# Navigation
# Strg + P              => Datei im Kursordner öffnen, tippen genügt
# Strg + Umschalt + Tab => zum nächsten offenen Skript wechseln
# Strg + l              => Konsole leeren

# Darstellung
# Strg + -              => Schriftgröße kleiner
# Strg + +              => Schriftgröße größer
# View > Appearance > Word Wrap => lange Zeilen umbrechen, auch unten rechts
#                                    in der Statusleiste anklickbar

# Datei
# Strg + s              => Skript speichern

# Die Tasten Strg + Umschalt + C, Strg + Umschalt + N, Strg + 1 und Strg + 2 kommen
# aus den RStudio-Tasten, die in den Einstellungen eingeschaltet sind
# (workbench.keybindings.rstudioKeybindings). Ohne diese Einstellung gibt es sie nicht.

# Prüfen, was eine Taste macht: Preferences > Open Keyboard Shortcuts öffnet eine
# Liste aller Belegungen; Strg + K, dann Strg + S listet alle Tastenkürzel auf.
# shortcuts ENDE ----------------------------------------------------------

library(tidyverse)
library(sjmisc)
md <- read_csv("mds12_schoko_milch.csv")
rm(sel)

sel <- md %>% select(starts_with("p026"))  
sel <- md %>% select(contains("026"))
sel <- md %>% select(ends_with("i147"))
sel <- md %>% select(M064krit2_11nachhaltig, M064krit2_12co2, M064krit2_14tierwohl, M064krit2_8fair, M064krit2_2region, M064krit2_6bio) # Bereich deselektieren: -VAR:-VAR
names(sel)
glimpse(sel)
view(sel)
#options(width=1000) # falls es bei offenen Fragen sehr lange Antworten gibt, muss die Displaybreite ggf. deutlich erhöht werden oder auf "unendlich" gesetzt werden: options(tibble.width = Inf)
frq(sel)
summarise(sel)

#Für die systematsiche Häufigkeite-Analysen: besser in txt-Datei exportieren:
sink("haeufigkeiten.txt")
options(width=1000)     # falls bei offenen Fragen sehr lange Antworten
frq(md)
sink()
file.show("haeufigkeiten.txt")

rm(sel) #df löschen
rm(fil)

fil <- md %>%  filter(ab050anlass2 == "1") #Beobachtungen löschen - nicht VAR
fil <- md %>% 
  select(u013pzahl_1weihen) %>%
  filter(u013pzahl_1weihen < 5 | is.na(u013pzahl_1weihen) == TRUE) # nur Beobachtungen mit Werten unter 5€
view(fil)

md <- md %>%
  mutate(Q002alter = 2025 - q002geburt) %>% # neue VAR berechnen (daher in md eintragen)
  mutate(Q002altergru4 = car::recode(Q002alter, "18:29=1; 30:44=2; 45:59=3; 60:hi=4")) #Berechnen + Umkodieren #Hinweis: Paket car wird durch den :: der Funktion zugeordnet und geladen
sel <- md %>% select(Q002alter, Q002altergru4, Q002altergru4f)
view(sel)

md <- md %>%  
  mutate(AB052nps2f = factor(ab052nps2,
                             levels = c(1, 2),     
                             labels = c("konventionell 1,89€", "zuckerreduziert 2,69€")))


library(dplyr)
library(purrr)
recodier-regel <- function(x) {  dplyr::recode(x, `2` = -2, `1` = -1, `0` = 0, `-1` = 1, `-2` = 2)}  # Definition einer recodier-regel, die auf mehrere Variablen angewendet werden soll
varrecode <- c("p011regio_6teuer", "p011regio_7noherkunft", "p011regio_8bio", "p011regio_9regional") # Auswahl der VAR die recodiert werden soll
md <- md %>%  mutate(across(all_of(varrecode), recodier-regel)) # Anwendung der Rekodier-Regel auf alle Variablen in der Liste varrecode


library(sjmisc)i157 <- md %>%
  select(contains("i157logodiff3"), ac157logodiff3)
i157_2 <- i157 %>%
  select(-ac157logodiff3)
library(psych)
describeBy(i157_2, group = i157$ac157logodiff3)



faktorvar <- sapply(s052, is.factor)
s052_faktorvar <- s052[, faktorvar]
rm(s052_faktorvar)


# Ende Sichten ------------------------------------------------------------




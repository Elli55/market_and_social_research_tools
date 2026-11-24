#Datendatei einlesen (Hauptstudie + Spezialstudien 1 Schoko und 2 Milch))
pacman::p_load(tidyverse, janitor, skimr, summarytools, purr, forcats)
md <- read_csv("mds12_schoko_milch.csv") #Datendatei 
##############################################################################################################


#filtert Beobachtungen/Probanden, die die Studie 6 (Handwerk) beantwortet haben
#md <- md |>
#   filter(AXspezial9 %in% c("1", "2"))
#   filter(AXspezial9 == "1")
#names(md)[colSums(!is.na(md)) == 0] # Zeigt mir alle Variablen in einem dataframe an, die nur fehlende Werte (NA) enthalten
#md <- md[,colSums(!is.na(md))>0] #löscht aus der Studie-X (plus Hauptstudie) alle Variablen heraus, in denen nur fehlende Werte (NA) enthalten sind.
 
#(vgl. Word-Fragebogen "Musterstudie")
glimpse(md)

#Daten anzeigen # unter Enviroment sichtbar wie viele Fälle und Variablen in 
#Datendatei enthalten sind


# Daten sichten--------------------------------------------------------------------------------------
 
#Variablennamen anzeigen
names(md)



#Daten umbenennen
#Eine Spalte umbenennen
md <- md |> rename(q002geburtsjahr = q002geburt)
md <- md |> rename(q002geburt = q002geburtsjahr)

#Mehrere Spalten umbenennen
md <- md |> rename(v007freq_1gemuese = v007freq1_ge, v007freq_2schoko = v007freq2_ts, v007freq_3milch = v007freq3_mi)

#Alle Spalten umbenennen, z.B in Kleinbuchstaben
#limo |> rename_all(tolower)


#Anzahl Spalten und Zeilen ermitteln
dim(md)


#tabellarische Datenanzeige
view(md)

#Anzeige Variablentyp
glimpse(md)
str(md)


# Datenvorbereitung Datendatei - Variable-----------------------------------------------------------

#Variablen auswählen (selektieren)####
df1 <- md |>
  select(v041nofleisch, Q004geschlecht)

df324 <- md |>
  select(v032bioanteil, Q004geschlecht) |>
  drop_na()#NA entfernen

#beginnt mit...
p011 <- md |>  select(starts_with("p011")) 

#enthalten ...
v008 <- md |> select(contains("v008")) 
#rm(v008) #löscht Data Frame wieder

#endet mit ...
oth <- md |> select(ends_with("_other"))
#beginnt mit ... und endet mit ...
oth2 <- md |> select(starts_with("v"), ends_with("_other"))

#Bestimmte Spalten bei Auswahl ausschliessen
md2 <- md |>
  select(-q001hheinkauf)
#Beispiel Auschluss Variable bei verschiedenen Analysen
# b010 <- md |> 
#   select(B010midiff7_1quali:B010midiff7_14kauf, AX010midiff7f)                                     
# b010_2 <- b010 |> 
#   select(-AX010midiff7f)


# data frame löschen   
#  rm(md2)


#Fälle auswählen (filter()) bzw. Fallgruppen auswählen####

#Betrachtung des Datensatzes (Dataframe) nur mit einer bestimmten Menge an Fällen (Zeilen).
#Beispiel: Betrachtung nur Variante A eines Variantenvergleich
s050 <- md|>   
  select(S050anlass2_1essen:S050anlass2_8no, ab050anlass2)
#Variante A
s050A <- s050 |>
  filter(ab050anlass2 == "1")


#bestimmte Wertekriterien herausfiltern (Anzeige der Zeile (=Fälle))
#Beispiel: offene Preisfrage, nur Werte unter 5€ anzeigen, da Werte > 5€ unrealistisch sind
u013 <- md |> 
  select(u013pzahl_1weihen) |>
  filter(u013pzahl_1weihen < 5 | is.na(u013pzahl_1weihen) == TRUE)


#Neue Variablen hinzufügen mit mutate()####
md <- md |>  
  mutate(Q002alter = 2025 - q002geburt) #z.B. Alter berechnen

#mutate () + umkodieren (recode()) siehe auch R-Skript Datentransformation -> weitere Beispiele
md <- md |>
  mutate(Q002alter = 2025 - q002geburt) |>
  mutate(Q002altergru4 = car::recode(Q002alter, "18:29=1; 30:44=2; 45:59=3; 60:hi=4")) #Hinweis: Paket car wird durch den :: der Funktion zugeordnet und geladen


#Datenvorbereitung: nützliche Weitere Funktionen---------------------------------------------------------------


#Variablentyp ändern: als Faktorvariable (mit Labelbeschriftung statt Kodierzahl)
#fct_recode -> Paket tidyverse
md <- md |> 
  mutate(AB052nps2fneu = as.factor(ab052nps2))|>
  mutate(AB052nps2fneu = 
           fct_recode(
             AB052nps2fneu,
             "konventionell 1,89€" = "1", 
             "zuckerreduziert 2,69€"= "2"
             )) 


#Überprüfung Häufigkeiten vorher-nachher

#Nur bei Limesurvey: Eckige Klammern [] beim Einlesen von CSV-Dateien aus Limesurvey mit clean_names (Paket janitor)
# library(tidyverse)
# library(janitor)
# mdA <- read_csv("results-survey141776_A.csv") |>
#   clean_names()
# mdB <- read_csv("results-survey271476_B.csv ") |>
#   clean_names


#Datenvorbereitung - Datentransformation(Umkodieren) -----------------------------------------------------
#Umkodieren - Neue Variablen erstellen - Berechnen

#Berechnen und aus metrischer Variable Gruppen zusammenfassen
#Beispiel Alter berechnen und aus der neuen Variable Alter 4 Altersgruppen bilden
md <- md |>
  mutate(Q002alter =2025 - q002geburt) |> #Alter aus Geburtsjahr berechnen
  mutate(Q002altergru4 = car::recode(Q002alter, "18:29=1; 30:44=2; 45:59=3; 60:hi=4")) #Hinweis: Paket car wird durch den :: der Funktion zugeordnet und geladen
#Hinweis: Immer die Häufigkeiten vor und nach der Berechnung überprüfen
md_pruef <- md |>
  select(q002geburt, Q002alter, Q002altergru4)|>
  view()


#Reduzierung der Antwortkategorien nominaler Variablen
#Beispiel1 Fleischreduzierung: Antwortkategorien (2-Vegatierer, 3=Vegan, 4=pescetarisch)
#wegen zu geringer N zu einer Antwortkategorie fleischlos = 2 zusammenfassen
md <- md |>  
  mutate(V041nofleisch3 = car::recode(v041nofleisch, "0=0; 1=1; 2:4=2")) #Hinweis: Paket car wird durch den :: der Funktion zugeordnet und geladen

#Beispiel2 Top-2-Boxes, Neutral und Bottom-2-Boxes bei z.B. 5er Skalen
md <- md |>  
  mutate(P012neo_1probiere = car::recode(p012neo_1probiere, "-2=-1; -1=-1; 0=0; 1=1; 2=1"))


#Index ueber eine Itembatterie bilden, um mehrere Items zu einem IndexWert zusammenzufassen
#Nicht notwendig, wenn man die (Index-Werte) mit der (besseren) Faktorenanalyse erzeugt (=>factor scores)

md <- md |> 
  mutate(Index_localfood = (p011regio_1wirtschaft + p011regio_2vertrauen + p011regio_3preis +
                              p011regio_4umwelt + p011regio_5tierschutz + p011regio_6teuer +
                              p011regio_7noherkunft+ p011regio_8bio + p011regio_9regional)/9)

#Hinweis: alle inversiven Variablen (R) der p001 sind noch nicht umkodiert, daher vor der Indexbildung (R)-Items
#umkodieren in einem Arbeitsgang 

#Dummy-Variable erstellen (Variable mit zwei Ausprägungen 1 und 0)
md$D037schule_7hs <- car::recode(md$d037schule,"0:6=0; 7=1")
md$D044stadt_6dorf <- car::recode(md$d044stadt, "1:5=0; 6=1")

#library(dplyr)
library(purrr) #um eine automatische Rekodierfunktion zu erstellen
md <- md |>  #vor dem Rekodieren alte Variable sichern
  mutate(p011regio_6teuer_alt = p011regio_6teuer) |> 
  mutate(p011regio_7noherkunft_alt = p011regio_7noherkunft) |> 
  mutate(p011regio_8bio_alt = p011regio_8bio) |>
  mutate(p011regio_9regional_alt = p011regio_9regional)
# Definiere eine Rekodierungsfunktion
recoding_function <- function(x) {  dplyr::recode(x, `2` = -2, `1` = -1, `0` = 0, `-1` = 1, `-2` = 2)}
#Variablenliste
varrecode <- c("p011regio_6teuer", "p011regio_7noherkunft", "p011regio_8bio", "p011regio_9regional")
# Wende die Rekodierungsfunktion auf jede Variable in der Liste an
md <- md |>  mutate(across(all_of(varrecode), recoding_function))
#Überprüfung Häufigkeiten vorher-nachher
#Hinweis: Die Haeufigkeiten vor und nach der Berechnung überprüfen
 md_pruef <- md |>
   select(p011regio_8bio_alt, p011regio_8bio)
   view()(md_pruef)
   
# In diesem Fall aber einfacher:
md <- md |>  #vor dem Rekodieren alte Variable sichern
     mutate(p011regio_8bio_alt =- p011regio_8bio)
md_pruef <- md |>
  select(p011regio_8bio_alt, p011regio_8bio)
view()(md_pruef)



#AB-Variable zusammenführen inklusive umkodieren der Kodierzahl
md <- md |>
  mutate(P026gericht2_1austern = case_when(p026gericht1_1austern == 7 ~ 3,
                                           p026gericht1_1austern == 6 ~ 2,
                                           p026gericht1_1austern == 5 ~ 1,
                                           p026gericht1_1austern == 4 ~ 0,
                                           p026gericht1_1austern == 3 ~ -1,
                                           p026gericht1_1austern == 2 ~ -2,
                                           p026gericht1_1austern == 1 ~ -3,
                                           p026gericht2_1austern == 7 ~ 3,
                                           p026gericht2_1austern == 6 ~ 2,
                                           p026gericht2_1austern == 5 ~ 1,
                                           p026gericht2_1austern == 4 ~ 0,
                                           p026gericht2_1austern == 3 ~ -1,
                                           p026gericht2_1austern == 2 ~ -2,
                                           p026gericht2_1austern == 1 ~ -3))#mit |> die nächste Variable der p026 verbinden

#AB-Variable zusammenführen bei metrischen Variablen ohne Umkodierung
# md <- md |>
#   mutate(
#     U016pvonwguenstig2 = coalesce(u016pvonwguenstig1, u016pvonwguenstig2),
#     U017pvonwzuteuer2 = coalesce(u017pvonwzuteuer1, u017pvonwzuteuer2),
#     U018pvonwteuer2 = coalesce(u018pvonwteuer1, u018pvonwteuer2),
#     U019pvonwzuguenstig2 = coalesce(u019pvonwzuguenstig1, u019pvonwzuguenstig2)
#   )



#Antwortkategorie als NA (fehlender Wert) umkodieren 
# md$D049einkommen <- car::recode(md$d049einkommen, "0=NA; 1:2=1; 3:5=2; 6:7=3; 8:hi=4")
# md_pruef <- md |>
#   select(d049einkommen, D049einkommen)|>
#   view()




#Dataframe umkodiert abspeichern
#write_csv(md, file="mds12_schoko_milch2.csv") #Verbesserte Version zu write.csv()
# wenn eine ;-Trennung gewuenscht => "write_csv2" nutzen
# Datei kann später für Folgeberechnungen weiterbenutzt werden (z.B. für statistische Analysen vorbereiteter Datensatz)

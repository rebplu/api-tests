# --- Pakete laden -------------------------------------------------------------

library(plumber)
library(jsonlite)
library(dplyr)
library(readr)
library(stringr)
library(fuzzyjoin)

# --- Daten laden --------------------------------------------------------------

gemeinden <<- read_csv("daten/GEMEINDE_ZH.csv") %>%
  rename_all(tolower) %>%
  mutate(gemeinde_code = as.numeric(gemeinde_code))

bezirke <<- read_csv("daten/BEZIRK_ZH.csv") %>%
  rename_all(tolower)

raumplanungsregionen <<- read_csv("daten/RAUMPLANUNGSREGION_ZH.csv") %>%
  rename_all(tolower)

gemeindezuweisungen <<- read_csv("daten/GEMEINDEZUWEISUNGEN_ZH.csv") %>%
  rename_all(tolower)

gemeindemutationen <<- read_csv("daten/GEMEINDEMUTATIONEN_ZH.csv") %>%
  rename_all(tolower)

gemeindenhist <<- read_csv("daten/GEMEINDEN_HIST.csv") %>%
  rename_all(tolower)

# --- Helper function ----------------------------------------------------------

# Funktion zur Normalisierung
normalize <- function(x) {
  # Alle Buchstaben zu Klein-Buchstaben umwandeln
  str_to_lower(x) %>%
    # Sämtliche Umlaute und das scharfe S ersetzen
    str_replace_all(c("ä"="ae","ö"="oe","ü"="ue","ß"="ss"))
}

# Funktion zur Namenssuche
namens_suche <- function(input = NULL, dictionary = NULL) {
  # Error handling
  if (is.null(input)) return(list(error = "Kein Name angegeben"))
  if (is.null(dictionary)) return(list(error = "Daten nicht verfügbar"))
  
  # Normalisierung
  such_clean <- tibble(eingabe = normalize(input))
  dictionary_normalisiert <- dictionary %>%
    mutate(name_clean = normalize(.[[grep("_name$", 
                                          names(.), 
                                          value = TRUE
                                          )[1]]]))
  
  # Dreistuffige Suche nach Gemeinde Namen
  # 1.) Exakter match
  vorschlaege <- dictionary_normalisiert %>% 
    filter(name_clean == such_clean$eingabe)  
  
  # 2.) grepl via stringr::str_detect
  if (nrow(vorschlaege) < 1) {
    vorschlaege <- dictionary_normalisiert %>%
      filter(str_detect(name_clean, such_clean$eingabe))
  }
  
  # 3.) Fuzzy matching
  if (nrow(vorschlaege) < 1) {
    vorschlaege <- such_clean %>%
      fuzzyjoin::stringdist_left_join(
        dictionary_normalisiert,
        by = c("eingabe" = "name_clean"),
        max_dist = 2
      ) %>%
      # NA matches aussortieren
      filter(!is.na(.[[grep("_name$", 
                            names(.), 
                            value = TRUE
                           )[1]]])) 
  }
  
  # 4.) Erfolglose Suche
  if (nrow(vorschlaege) < 1) {
    return(list(name = unbox(input),
                treffer = list(),
                info = unbox("Kein Treffer gefunden"))
    )
  } 
  # Rückgabe der Vorschläge
  vorschlaege
}

# --- API Info -----------------------------------------------------------------

#* @apiTitle API Gebietsstammdaten Kanton Zürich 
#* @apiVersion 1.0 (Beta)
#* @apiDescription REST API für Gebietsstammdaten des Kantons Zürich
#* @apiLicense list(name = "MIT License", url = "https://opensource.org/licenses/MIT")

# ==============================================================================
#  G E M E I N D E N
# ==============================================================================

#* @get /api/gemeinden
#* @responseContentType application/json
function() {
  if (is.null(gemeinden)) {
    return(list(error = unbox("Daten nicht verfügbar")))
  }
  gemeinden <- gemeinden %>% arrange(gemeinde_code)
  list(gemeinden = gemeinden)
}

#* @get /api/gemeinden/<gemeinde_code:int>
#* @param gemeinde_code Code
#* @responseContentType application/json
function(gemeinde_code) {
  gemeinde_code <- as.numeric(gemeinde_code)
  if (is.null(gemeinden)) return(list(error = unbox("Daten nicht verfügbar")))
  
  info <- gemeinden %>% 
    filter(gemeinde_code == !!gemeinde_code) %>%
    select(-gebietstyp_code)
  
  if (nrow(info) == 0) {
    return(list(error = unbox("Keine Gemeinde gefunden")))
  }
  
  list(gemeinde = unbox(info))
}


#* Suche nach Gemeinde anhand des Gemeindenamens
#*
#* @get /api/gemeinden/gemeindename
#* @param gemeindename Name der Gemeinde
#* @responseContentType application/json
function(gemeindename) {
  # Suche starten
  vorschlaege <- namens_suche(input = gemeindename, dictionary = gemeinden)
  
  # Fall wenn nichts gefunden wurde abhandeln
  if (!is.data.frame(vorschlaege)) {
    return(vorschlaege)
  }
  
  vorschlaege <- vorschlaege %>% arrange(gemeinde_code)
  
  # Listen Rückgabe bei treffer
  treffer <- lapply(seq_len(nrow(vorschlaege)), function(i) {
    code <- vorschlaege$gemeinde_code[i]
    
    gemeinde_info <- gemeinden %>% 
      filter(gemeinde_code == !!code) %>% 
      select(-gebietstyp_code)
    
    bezirk_info <- gemeindezuweisungen %>% 
      filter(gemeinde_code == !!code) %>% 
      select(bezirk_code, bezirk_name) %>% 
      distinct()
    
    regionen_info <- gemeindezuweisungen %>% 
      filter(gemeinde_code == !!code) %>% 
      select(raumplanungsregion_code, raumplanungsregion_name) %>% 
      distinct()
    
    list(
      gemeinde = unbox(gemeinde_info),
      bezirk = unbox(bezirk_info),
      raumplanungsregion = unbox(regionen_info)
    )
  })
  
  list(
    name = unbox(gemeindename),
    treffer = treffer
  )
}

# ==============================================================================
#  GEMEINDEZUWEISUNGEN
# ==============================================================================

#* @get /api/gemeindezuweisungen
#* @responseContentType application/json
function() {
  if (is.null(gemeindezuweisungen)) {
    return(list(error = unbox("Daten nicht verfügbar")))
  }
  gemeindezuweisungen <- gemeindezuweisungen %>% arrange(gemeinde_code)
  list(gemeindezuweisungen = gemeindezuweisungen)
}


#* @get /api/gemeindezuweisungen/<gemeinde_code:int>
#* @param gemeinde_code Code
#* @responseContentType application/json
function(gemeinde_code) {
  gemeinde_code <- as.numeric(gemeinde_code)
  
  if (any(sapply(list(gemeinden, 
                      bezirke, 
                      raumplanungsregionen, 
                      gemeindezuweisungen), 
                 is.null))) {
    return(list(error = unbox("Daten nicht verfügbar")))
  }
  
  gemeinde_info <- gemeinden %>%
    filter(gemeinde_code == !!gemeinde_code) %>%
    select(-gebietstyp_code)
  
  if (nrow(gemeinde_info) == 0) {
    return(list(error = unbox("Keine Gemeinde gefunden")))
  }
  
  bezirk_info <- gemeindezuweisungen %>%
    filter(gemeinde_code == !!gemeinde_code) %>%
    select(bezirk_code, bezirk_name) %>%
    distinct()
  
  region_info <- gemeindezuweisungen %>%
    filter(gemeinde_code == !!gemeinde_code) %>%
    select(raumplanungsregion_code, raumplanungsregion_name) %>%
    distinct()
  
  list(
    gemeinde = unbox(gemeinde_info),
    bezirk = unbox(bezirk_info),
    raumplanungsregion = unbox(region_info)
  )
}

# ==============================================================================
#  B E Z I R K E
# ==============================================================================

#* @get /api/bezirke
#* @responseContentType application/json
function() {
  if (is.null(bezirke)) {
    return(list(error = unbox("Daten nicht verfügbar")))
  }
  bezirke <- bezirke %>% arrange(bezirk_code)
  list(bezirke = bezirke)
}

#* @get /api/bezirke/<bezirk_code:int>
#* @param bezirk_code Code
#* @responseContentType application/json
function(bezirk_code) {
  bezirk_code <- as.numeric(bezirk_code)
  if (is.null(bezirke) || is.null(gemeindezuweisungen)) {
    return(list(error = unbox("Daten nicht verfügbar")))
  }
  
  info <- bezirke %>% 
    filter(bezirk_code == !!bezirk_code) %>% 
    select(-gebietstyp_code)
  
  if (nrow(info) == 0) {
    return(list(error = unbox("Keinen Bezirk gefunden")))
  }
  
  gemeinden_des_bezirks <- gemeindezuweisungen %>%
    filter(bezirk_code == !!bezirk_code) %>%
    select(gemeinde_code, gemeinde_name) %>%
    distinct()%>%
    arrange(gemeinde_code)
  
  list(bezirk = unbox(info), gemeinden = gemeinden_des_bezirks)
}

#* Suche nach Bezirk anhand des Bezirknamens
#*
#* @get /api/bezirke/bezirkname
#* @param bezirkname Name des Bezirkes 
#* @responseContentType application/json
function(bezirkname) {
  # Suche starten
  vorschlaege <- namens_suche(input = bezirkname, dictionary = bezirke)
  
  # Fall wenn nichts gefunden wurde abhandeln
  if (!is.data.frame(vorschlaege)) {
    return(vorschlaege)
  }
  vorschlaege <-  vorschlaege %>% arrange(bezirk_code)
  # Listen Rückgabe bei treffer
  treffer <- lapply(seq_len(nrow(vorschlaege)), function(i) {
    code <- vorschlaege$bezirk_code[i]
    
    bezirk_info <- bezirke %>% 
      filter(bezirk_code == !!code) %>% 
      select(-gebietstyp_code)
    
    gemeinden_des_bezirks <- gemeindezuweisungen %>%
      filter(bezirk_code == !!code) %>%
      select(gemeinde_code, gemeinde_name) %>%
      distinct() %>%
      arrange(gemeinde_code)
    
    list(bezirk = unbox(bezirk_info), gemeinden = gemeinden_des_bezirks)
  })
  
  list(
    name = unbox(bezirkname),
    treffer = treffer
  )
}

# ==============================================================================
#  R A U M P L A N U N G S R E G I O N E N
# ==============================================================================

#* @get /api/raumplanungsregionen
#* @responseContentType application/json
function() {
  if (is.null(raumplanungsregionen)) {
    return(list(error = unbox("Daten nicht verfügbar")))
  }
  raumplanungsregionen <-  raumplanungsregionen %>% arrange(raumplanungsregion_code)
  list(raumplanungsregionen = raumplanungsregionen)
}

#* @get /api/raumplanungsregionen/<region_code:int>
#* @param region_code Code
#* @responseContentType application/json
function(region_code) {
  region_code <- as.numeric(region_code)
  if (is.null(raumplanungsregionen) || is.null(gemeindezuweisungen)) {
    return(list(error = unbox("Daten nicht verfügbar")))
  }
  
  region_info <- raumplanungsregionen %>%
    filter(raumplanungsregion_code == !!region_code) %>%
    select(-gebietstyp_code)
  if (nrow(region_info) == 0) {
    return(list(error = unbox("Keine Raumplanungsregion gefunden")))
  }
  
  gemeinden_der_region <- gemeindezuweisungen %>%
    filter(raumplanungsregion_code == !!region_code) %>%
    select(gemeinde_code, gemeinde_name) %>%
    distinct() %>%
    arrange(gemeinde_code)  
  
  list(raumplanungsregion = unbox(region_info),
       gemeinden = gemeinden_der_region)
}

#* Suche nach Raumplanungsregion anhand des Raumplanungsregionnamens
#*
#* @get /api/raumplanungsregionen/raumplanungsregionname
#* @param raumplanungsregionname Name der Raumplanungsregion
#* @responseContentType application/json
function(raumplanungsregionname) {
  # Suche starten
  vorschlaege <- namens_suche(input = raumplanungsregionname, 
                              dictionary = raumplanungsregionen)
  
  # Fall wenn nichts gefunden wurde abhandeln
  if (!is.data.frame(vorschlaege)) {
    return(vorschlaege)
  }
  vorschlaege <-  vorschlaege %>% arrange(raumplanungsregion_code)
  # Listen Rückgabe bei treffer
  treffer <- lapply(seq_len(nrow(vorschlaege)), function(i) {
    code <- vorschlaege$raumplanungsregion_code[i]
    
    region_info <- raumplanungsregionen %>% 
      filter(raumplanungsregion_code == !!code) %>% 
      select(-gebietstyp_code)
    
    gemeinden_der_region <- gemeindezuweisungen %>%
      filter(raumplanungsregion_code == !!code) %>%
      select(gemeinde_code, gemeinde_name) %>%
      distinct()  %>%
      arrange(gemeinde_code)
    
    list(raumplanungsregion = unbox(region_info),
         gemeinden = gemeinden_der_region)
  })
  
  list(
    name = unbox(raumplanungsregionname),
    treffer = treffer
  )
}

# ==============================================================================
#  G E M E I N D E - M U T A T I O N E N &  H I S T O R I E
# ==============================================================================

#* @get /api/gemeindemutationen
#* @responseContentType application/json
function() {
  if (is.null(gemeindemutationen)) {
    return(list(error = unbox("Daten nicht verfügbar")))
  }
  gemeindemutationen <- gemeindemutationen %>% arrange(mutationsdatum)
  list(gemeindemutationen = gemeindemutationen)
}

#* @get /api/gemeindenhist
#* @responseContentType application/json
function() {
  if (is.null(gemeindenhist)) {
    return(list(error = unbox("Daten nicht verfügbar")))
  }
  
  list(gemeindenhist = gemeindenhist)
}

#* @get /api/gemeindenhist/<jahr:int>
#* @param jahr Jahreszahl
#* @responseContentType application/json
function(jahr) {
  jahr <- as.numeric(jahr)
  if (is.null(gemeindenhist)) {
    return(list(error = unbox("Daten nicht verfügbar")))
  }
  
  daten <- gemeindenhist %>% 
    filter(jahr == !!jahr) %>% arrange(gemeinde_code)
  
  if (nrow(daten) == 0) {
    return(list(error = unbox(sprintf("Keine Gemeinden für das Jahr %s gefunden",
                                      jahr))))
  }
    
  list(jahr = unbox(jahr), gemeinden = daten)
}

#* @get /api/gemeindenhist/<jahr:int>/<gemeinde_code:int>
#* @param jahr Jahreszahl
#* @param gemeinde_code Code
#* @responseContentType application/json
function(jahr, gemeinde_code) {
  gemeinde_code <- as.numeric(gemeinde_code)
  jahr <- as.numeric(jahr)
  if (is.null(gemeindenhist)) {
    return(list(error = unbox("Daten nicht verfügbar")))
  }
  
  daten <- gemeindenhist %>% 
    filter(gemeinde_code == !!gemeinde_code, jahr == !!jahr)
  
  if (nrow(daten) == 0) {
    return(list(error = unbox(sprintf("Keine Gemeinde %s im Jahr %s gefunden", 
                                gemeinde_code, 
                                jahr))))
  }
  
  list(gemeinde_code = unbox(gemeinde_code),
       jahr = unbox(jahr),
       daten = unbox(daten))
}

# ==============================================================================
#  S Y S T E M / H E A L T H
# ==============================================================================

#* @get /api/health
#* @responseContentType application/json
function() {
  list(
    status = unbox("healthy"),
    timestamp = unbox(Sys.time()),
    data_available = unbox(!is.null(gemeinden))
  )
}
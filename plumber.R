# --- Pakete laden -------------------------------------------------------------
library(plumber)
library(jsonlite)
library(dplyr)
library(readr)
library(stringr)
library(fuzzyjoin)
library(httr)

#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type")
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(res)
  }
  plumber::forward()
}

# --- Matomo Tracking ----------------------------------------------------------
MATOMO_URL     <- "https://sa.abx-net.net/matomo.php" 
MATOMO_SITE_ID <- 19

track_matomo <- function(req) {
  if (req$REQUEST_METHOD == "OPTIONS") return(invisible(NULL))
  
  tryCatch({
    httr::GET(
      url   = MATOMO_URL,
      query = list(
        idsite      = MATOMO_SITE_ID,
        rec         = 1,
        url         = paste0("https://", req$HTTP_HOST, req$PATH_INFO),
        action_name = req$PATH_INFO,
        ua          = req$HTTP_USER_AGENT %||% "",
        lang        = req$HTTP_ACCEPT_LANGUAGE %||% "",
        rand        = as.integer(Sys.time())
      ),
      httr::timeout(2)
    )
  }, error = function(e) {
    message("[Matomo] Tracking-Fehler: ", e$message)
  })
}

#* @filter matomo_tracking
function(req, res) {
  track_matomo(req)
  plumber::forward()
}

# --- Daten laden --------------------------------------------------------------
# Verwende rename_with statt des veralteten rename_all
gemeinden            <- read_csv("daten/GEMEINDE_ZH.csv") %>% rename_with(tolower, everything()) %>% mutate(gemeinde_code = as.numeric(gemeinde_code))
bezirke              <- read_csv("daten/BEZIRK_ZH.csv") %>% rename_with(tolower, everything())
raumplanungsregionen <- read_csv("daten/RAUMPLANUNGSREGION_ZH.csv") %>% rename_with(tolower, everything())
gemeindezuweisungen  <- read_csv("daten/GEMEINDEZUWEISUNGEN_ZH.csv") %>% rename_with(tolower, everything())
gemeindemutationen   <- read_csv("daten/GEMEINDEMUTATIONEN_ZH.csv") %>% rename_with(tolower, everything())
gemeindenhist        <- read_csv("daten/GEMEINDEN_HIST.csv") %>% rename_with(tolower, everything())

# --- Helper functions ---------------------------------------------------------
geo_abkuerzungen <- c(
  "\\s*a\\.\\s*a\\."        = " am albis",
  "\\s*a\\.\\s*d\\.\\s*t\\." = " an der thur",
  "\\s*a\\.\\s*d\\.\\s*l\\." = " an der limmat",
  "\\s*a\\.\\s*s\\."        = " am see",
  "\\s*a\\.\\s*a\\b"        = " am albis",
  "\\s*a\\.\\s*d\\.\\s*t\\b" = " an der thur",
  "\\s*a\\.\\s*d\\.\\s*l\\b" = " an der limmat",
  "\\s*a\\.\\s*s\\b"        = " am see"
)

normalize <- function(x) {
  str_to_lower(x) %>%
    str_replace_all(c("ä"="ae","ö"="oe","ü"="ue","ß"="ss")) %>%
    str_replace_all(geo_abkuerzungen)
}

namens_suche <- function(input = NULL, dictionary = NULL) {
  if (is.null(input)) return(list(error = "Kein Name angegeben"))
  if (is.null(dictionary)) return(list(error = "Daten nicht verfügbar"))
  
  such_clean <- tibble(eingabe = normalize(input))
  
  # Dynamisch die Namensspalte finden (endet auf _name)
  name_col <- grep("_name$", names(dictionary), value = TRUE)[1]
  dictionary_normalisiert <- dictionary %>% mutate(name_clean = normalize(.data[[name_col]]))
  
  # 1.) Exakter Match
  vorschlaege <- dictionary_normalisiert %>% filter(name_clean == such_clean$eingabe)
  
  # 2.) Teilstring-Match
  if (nrow(vorschlaege) < 1) {
    vorschlaege <- dictionary_normalisiert %>% filter(str_detect(name_clean, fixed(such_clean$eingabe)))
  }
  
  # 3.) Fuzzy Matching
  if (nrow(vorschlaege) < 1) {
    vorschlaege <- such_clean %>%
      fuzzyjoin::stringdist_left_join(dictionary_normalisiert, by = c("eingabe" = "name_clean"), max_dist = 2) %>%
      filter(!is.na(.data[[name_col]]))
  }
  
  # 4.) Erfolglos
  if (nrow(vorschlaege) < 1) {
    return(list(name = unbox(input), treffer = list(), info = unbox("Kein Treffer gefunden")))
  }
  vorschlaege
}

# --- API Info -----------------------------------------------------------------
#* @apiTitle API Gebietsstammdaten Kanton Zürich
#* @apiVersion 1.0
#* @apiDescription REST API für Gebietsstammdaten des Kantons Zürich
#* @apiLicense list(name = "MIT License", url = "https://opensource.org/licenses/MIT")

# ==============================================================================
#  S T A T I S C H E   D A T E I E N
# ==============================================================================
#* @assets ./static/gemeinde /gemeinde
function() {}
#* @assets ./static/bezirk /bezirk
function() {}
#* @assets ./static/raumplanungsregion /raumplanungsregion
function() {}

# ==============================================================================
#  G E B I E T E
# ==============================================================================

#* Alle Gebiete abrufen
#* @get /api/gebiete
#* @responseContentType application/json
function() {
  gebiete <- bind_rows(
    tibble(gebietstyp_code = 1, gebietstyp_name = "Kanton", gebiet_code = 1, gebiet_name = "Zürich"),
    gemeinden            %>% mutate(gebietstyp_name = "Gemeinde")            %>% rename(gebiet_code = gemeinde_code, gebiet_name = gemeinde_name),
    bezirke              %>% mutate(gebietstyp_name = "Bezirk")              %>% rename(gebiet_code = bezirk_code, gebiet_name = bezirk_name),
    raumplanungsregionen %>% mutate(gebietstyp_name = "Raumplanungsregion")  %>% rename(gebiet_code = raumplanungsregion_code, gebiet_name = raumplanungsregion_name)
  ) %>% arrange(gebietstyp_code, gebiet_code)
  list(gebiete = gebiete)
}

#* Alle Gebietstypen abrufen
#* @get /api/gebietstypen
#* @responseContentType application/json
function() {
  gebietstypen <- tibble(
    gebietstyp_code = c(1, 2, 3, 6),
    gebietstyp_name = c("Kanton", "Bezirk", "Gemeinde", "Raumplanungsregion")
  ) %>% arrange(gebietstyp_code)
  list(gebietstypen = gebietstypen)
}

# ==============================================================================
#  G E M E I N D E N
# ==============================================================================

#* Alle Gemeinden abrufen
#* @get /api/gemeinden
#* @responseContentType application/json
function() {
  list(gemeinden = arrange(gemeinden, gemeinde_code))
}

#* Gemeinde mit dem Gemeindecode (Bfsnr) abrufen
#* @get /api/gemeinden/<gemeinde_code:int>
#* @param gemeinde_code Code
#* @responseContentType application/json
function(gemeinde_code) {
  info <- gemeinden %>% filter(gemeinde_code == !!as.numeric(gemeinde_code)) %>% select(-gebietstyp_code)
  if (nrow(info) == 0) return(list(error = unbox("Keine Gemeinde gefunden")))
  list(gemeinde = unbox(info))
}

#* Suche nach Gemeinde anhand des Gemeindenamens
#* @get /api/gemeinden/gemeinde_name
#* @param gemeinde_name Name der Gemeinde
#* @responseContentType application/json
function(gemeinde_name) {
  vorschlaege <- namens_suche(input = gemeinde_name, dictionary = gemeinden)
  if (!is.data.frame(vorschlaege)) return(vorschlaege)
  
  vorschlaege <- vorschlaege %>% arrange(gemeinde_code)
  
  treffer <- lapply(vorschlaege$gemeinde_code, function(code) {
    list(
      gemeinde           = unbox(gemeinden %>% filter(gemeinde_code == !!code) %>% select(-gebietstyp_code)),
      bezirk             = unbox(gemeindezuweisungen %>% filter(gemeinde_code == !!code) %>% select(bezirk_code, bezirk_name) %>% distinct()),
      raumplanungsregion = unbox(gemeindezuweisungen %>% filter(gemeinde_code == !!code) %>% select(raumplanungsregion_code, raumplanungsregion_name) %>% distinct())
    )
  })
  list(name = unbox(gemeinde_name), treffer = treffer)
}

# ==============================================================================
#  GEMEINDEZUWEISUNGEN
# ==============================================================================

#* Alle Gemeindezuweisungen abrufen
#* @get /api/gemeindezuweisungen
#* @responseContentType application/json
function() {
  list(gemeindezuweisungen = arrange(gemeindezuweisungen, gemeinde_code))
}

#* Gemeindezuweisungen mit dem Gemeindecode (Bfsnr) abrufen
#* @get /api/gemeindezuweisungen/<gemeinde_code:int>
#* @param gemeinde_code Code
#* @responseContentType application/json
function(gemeinde_code) {
  code_num <- as.numeric(gemeinde_code)
  gemeinde_info <- gemeinden %>% filter(gemeinde_code == !!code_num) %>% select(-gebietstyp_code)
  if (nrow(gemeinde_info) == 0) return(list(error = unbox("Keine Gemeinde gefunden")))
  
  list(
    gemeinde           = unbox(gemeinde_info),
    bezirk             = unbox(gemeindezuweisungen %>% filter(gemeinde_code == !!code_num) %>% select(bezirk_code, bezirk_name) %>% distinct()),
    raumplanungsregion = unbox(gemeindezuweisungen %>% filter(gemeinde_code == !!code_num) %>% select(raumplanungsregion_code, raumplanungsregion_name) %>% distinct())
  )
}

# ==============================================================================
#  B E Z I R K E
# ==============================================================================

#* Alle Bezirke abrufen
#* @get /api/bezirke
#* @responseContentType application/json
function() {
  list(bezirke = arrange(bezirke, bezirk_code))
}

#* Bezirk mit dem Bezirkcode abrufen
#* @get /api/bezirke/<bezirk_code:int>
#* @param bezirk_code Code
#* @responseContentType application/json
function(bezirk_code) {
  code_num <- as.numeric(bezirk_code)
  info <- bezirke %>% filter(bezirk_code == !!code_num) %>% select(-gebietstyp_code)
  if (nrow(info) == 0) return(list(error = unbox("Keinen Bezirk gefunden")))
  
  gemeinden_des_bezirks <- gemeindezuweisungen %>% filter(bezirk_code == !!code_num) %>% select(gemeinde_code, gemeinde_name) %>% distinct() %>% arrange(gemeinde_code)
  list(bezirk = unbox(info), gemeinden = gemeinden_des_bezirks)
}

#* Suche nach Bezirk anhand des Bezirknamens
#* @get /api/bezirke/bezirk_name
#* @param bezirk_name Name des Bezirkes
#* @responseContentType application/json
function(bezirk_name) {
  vorschlaege <- namens_suche(input = bezirk_name, dictionary = bezirke)
  if (!is.data.frame(vorschlaege)) return(vorschlaege)
  
  treffer <- lapply(sort(vorschlaege$bezirk_code), function(code) {
    list(
      bezirk    = unbox(bezirke %>% filter(bezirk_code == !!code) %>% select(-gebietstyp_code)),
      gemeinden = gemeindezuweisungen %>% filter(bezirk_code == !!code) %>% select(gemeinde_code, gemeinde_name) %>% distinct() %>% arrange(gemeinde_code)
    )
  })
  list(name = unbox(bezirk_name), treffer = treffer)
}

# ==============================================================================
#  R A U M P L A N U N G S R E G I O N E N
# ==============================================================================

#* Alle Raumplanungsregionen abrufen
#* @get /api/raumplanungsregionen
#* @responseContentType application/json
function() {
  list(raumplanungsregionen = arrange(raumplanungsregionen, raumplanungsregion_code))
}

#* Raumplanungsregion mit dem Raumplanungsregioncode abrufen
#* @get /api/raumplanungsregionen/<raumplanungsregion_code:int>
#* @param raumplanungsregion_code Code
#* @responseContentType application/json
function(raumplanungsregion_code) {
  code_num <- as.numeric(raumplanungsregion_code)
  region_info <- raumplanungsregionen %>% filter(raumplanungsregion_code == !!code_num) %>% select(-gebietstyp_code)
  if (nrow(region_info) == 0) return(list(error = unbox("Keine Raumplanungsregion gefunden")))
  
  gemeinden_der_region <- gemeindezuweisungen %>% filter(raumplanungsregion_code == !!code_num) %>% select(gemeinde_code, gemeinde_name) %>% distinct() %>% arrange(gemeinde_code)
  list(raumplanungsregion = unbox(region_info), gemeinden = gemeinden_der_region)
}

#* Suche nach Raumplanungsregion anhand des Raumplanungsregionnamens
#* @get /api/raumplanungsregionen/raumplanungsregion_name
#* @param raumplanungsregion_name Name der Raumplanungsregion
#* @responseContentType application/json
function(raumplanungsregion_name) {
  vorschlaege <- namens_suche(input = raumplanungsregion_name, dictionary = raumplanungsregionen)
  if (!is.data.frame(vorschlaege)) return(vorschlaege)
  
  treffer <- lapply(sort(vorschlaege$raumplanungsregion_code), function(code) {
    list(
      raumplanungsregion = unbox(raumplanungsregionen %>% filter(raumplanungsregion_code == !!code) %>% select(-gebietstyp_code)),
      gemeinden          = gemeindezuweisungen %>% filter(raumplanungsregion_code == !!code) %>% select(gemeinde_code, gemeinde_name) %>% distinct() %>% arrange(gemeinde_code)
    )
  })
  list(name = unbox(raumplanungsregion_name), treffer = treffer)
}

# ==============================================================================
#  G E M E I N D E - M U T A T I O N E N &  H I S T O R I E
# ==============================================================================

#* Alle Gemeindemutationen abrufen
#* @get /api/gemeindemutationen
#* @responseContentType application/json
function() {
  list(gemeindemutationen = arrange(gemeindemutationen, mutationsdatum))
}

#* Jahresstände aller Gemeinden seit 1990
#* @get /api/gemeindenhist
#* @responseContentType application/json
function() {
  list(gemeindenhist = gemeindenhist)
}

#* Den aktuellen Code (Bfsnr) einer Gemeinde abrufen
#* @get /api/gemeindefusionen/<gemeinde_code:int>
#* @param gemeinde_code Code
#* @responseContentType application/json
function(gemeinde_code) {
  code_num <- as.numeric(gemeinde_code)
  daten <- gemeindemutationen %>% filter(gemeinde_code_alt == !!code_num) %>% arrange(mutationsdatum)
  
  if (nrow(daten) > 0) {
    aktuell <- slice_tail(daten, n = 1)
    return(list(gemeinde_code = unbox(aktuell$gemeinde_code_neu), gemeinde_name = unbox(aktuell$gemeinde_name_neu)))
  }
  
  aktuell <- gemeinden %>% filter(gemeinde_code == !!code_num)
  if (nrow(aktuell) > 0) {
    return(list(gemeinde_code = unbox(aktuell$gemeinde_code), gemeinde_name = unbox(aktuell$gemeinde_name)))
  }
  list(error = unbox("Keine Gemeinde gefunden"))
}

#* Jahresstand aller Gemeinden mit Jahr abrufen 
#* @get /api/gemeindenhist/<jahr:int>
#* @param Jahr Jahreszahl
#* @responseContentType application/json
function(jahr) {
  jahr_num <- as.numeric(jahr)
  daten <- gemeindenhist %>% filter(jahr == !!jahr_num) %>% arrange(gemeinde_code)
  if (nrow(daten) == 0) return(list(error = unbox(sprintf("Keine Gemeinden für das Jahr %s gefunden", jahr))))
  list(jahr = unbox(jahr_num), gemeinden = daten)
}

#* Jahresstand einer Gemeinde mit Jahr und Gemeindecode abrufen 
#* @get /api/gemeindenhist/<jahr:int>/<gemeinde_code:int>
#* @param Jahr Jahreszahl
#* @param gemeinde_code Code
#* @responseContentType application/json
function(jahr, gemeinde_code) {
  code_num <- as.numeric(gemeinde_code)
  jahr_num <- as.numeric(jahr)
  daten <- gemeindenhist %>% filter(gemeinde_code == !!code_num, jahr == !!jahr_num)
  if (nrow(daten) == 0) return(list(error = unbox(sprintf("Keine Gemeinde %s im Jahr %s gefunden", gemeinde_code, jahr))))
  list(gemeinde_code = unbox(code_num), jahr = unbox(jahr_num), daten = unbox(daten))
}

# ==============================================================================
#  R E C O N C I L I A T I O N   S E R V I C E   (v0.2)
# ==============================================================================

get_base_url <- function(req) {
  env_url <- Sys.getenv("BASE_URL")
  if (nzchar(env_url)) return(env_url)
  host  <- req$HTTP_X_FORWARDED_HOST %||% req$HTTP_HOST %||% "localhost:8000"
  proto <- req$HTTP_X_FORWARDED_PROTO %||% "http"
  paste0(proto, "://", host)
}

parse_encoded_id <- function(id) {
  if (grepl("^(gemeinde|bezirk|raumplanungsregion):", id)) {
    parts <- strsplit(id, ":")[[1]]
    list(type = parts[1], id_num = suppressWarnings(as.numeric(parts[2])))
  } else {
    list(type = "gemeinde", id_num = suppressWarnings(as.numeric(id)))
  }
}

#* Reconcile-Service für OpenRefine 
#* @post /api/reconcile
#* @get  /api/reconcile
#* @serializer unboxedJSON
function(req) {
  base_url <- get_base_url(req)
  params   <- req$args
  if (req$REQUEST_METHOD == "POST") {
    form_params <- tryCatch(plumber::parse_form(req), error = function(e) list())
    params <- modifyList(params, form_params)
  }
  
  queries <- params$queries
  extend  <- params$extend
  
  # 1. MANIFEST 
  if (is.null(queries) && is.null(extend)) {
    return(list(
      name            = jsonlite::unbox("Kanton Zürich Gebiets-Reconciliation Service"),
      identifierSpace = jsonlite::unbox(paste0(base_url, "/api/reconcile")),
      schemaSpace     = jsonlite::unbox("https://www.zh.ch/de/politik-staat/statistik-daten/datenkatalog.html#/datasets/3082@statistisches-amt-kanton-zuerich"),
      view            = list(url = jsonlite::unbox(paste0(base_url, "/api/reconcile/view?id={{id}}"))),
      preview = list(
        url    = jsonlite::unbox(paste0(base_url, "/api/reconcile/preview?id={{id}}")),
        width  = 500L,
        height = 350L
      ),
      extend = list(
        propose_properties = list(
          service_url  = jsonlite::unbox(base_url),
          service_path = jsonlite::unbox("/api/reconcile/properties")
        )
      ),
      # --- DIESER BLOCK SCHALTET "CREATE NEW ITEM" FREI ---
      suggest = list(
        entity = list(
          service_url  = jsonlite::unbox(base_url),
          service_path = jsonlite::unbox("/api/reconcile/suggest/entity")
        ),
        property = list(
          service_url  = jsonlite::unbox(base_url),
          service_path = jsonlite::unbox("/api/reconcile/suggest/property")
        ),
        type = list(
          service_url  = jsonlite::unbox(base_url),
          service_path = jsonlite::unbox("/api/reconcile/suggest/type")
        )
      ),
      # ---------------------------------------------------------
      defaultTypes = list(
        list(id = jsonlite::unbox("gemeinde"),           name = jsonlite::unbox("Gemeinde")),
        list(id = jsonlite::unbox("bezirk"),              name = jsonlite::unbox("Bezirk")),
        list(id = jsonlite::unbox("raumplanungsregion"),   name = jsonlite::unbox("Raumplanungsregion"))
      )
    ))
  }
  
  # 2. EXTEND
  if (!is.null(extend)) {
    ext        <- jsonlite::fromJSON(extend, simplifyVector = FALSE)
    ids        <- unlist(ext$ids)
    prop_ids   <-  sapply(ext$properties, `[[`, "id")
    
    rows <- lapply(ids, function(id_str) {
      parsed <- parse_encoded_id(id_str)
      id_num <- parsed$id_num
      
      gz <- gemeindezuweisungen %>% filter(gemeinde_code == id_num)
      gm <- gemeinden           %>% filter(gemeinde_code == id_num)
      bz <- bezirke             %>% filter(bezirk_code   == id_num)
      rr <- raumplanungsregionen %>% filter(raumplanungsregion_code == id_num)
      
      vals <- lapply(prop_ids, function(p) {
        v <- switch(p,
                    gemeinde_code           = if (nrow(gm) > 0) gm$gemeinde_code[1]           else NA,
                    gemeinde_name           = if (nrow(gm) > 0) gm$gemeinde_name[1]           else NA,
                    bezirk_code              = if (nrow(gz) > 0) gz$bezirk_code[1]  else if (nrow(rr) > 0) bz$bezirk_code[1]            else NA,
                    bezirk_name              = if (nrow(gz) > 0) gz$bezirk_name[1]  else if (nrow(rr) > 0) bz$bezirk_name[1]          else NA,
                    raumplanungsregion_code  = if (nrow(gz) > 0) gz$raumplanungsregion_code[1]  else if (nrow(rr) > 0) rr$raumplanungsregion_code[1] else NA,
                    raumplanungsregion_name  = if (nrow(gz) > 0) gz$raumplanungsregion_name[1]  else if (nrow(rr) > 0) rr$raumplanungsregion_name[1] else NA,
                    gemeindemutationen = {
                      # Alle Zeilen wo diese Gemeinde das Ziel einer Mutation war
                      mut <- gemeindemutationen %>%
                        filter(gemeinde_code_neu == id_num) %>%
                        arrange(mutationsdatum)
                      
                      if (nrow(mut) == 0) {
                        NA
                      } else {
                        fusionen <- mut %>%
                          filter(mutationstyp == "Fusion") %>%
                          group_by(mutationsdatum, mutationstyp, gemeinde_name_neu) %>%
                          summarise(
                            text = paste0("Fusion per ", format(as.Date(mutationsdatum[1]), "%d.%m.%Y"), ": ",
                                          paste0(gemeinde_name_alt, " (", gemeinde_code_alt, ")", collapse = ", "),
                                          " → ", gemeinde_name_neu[1], " (", id_num, ")"),
                            .groups = "drop"
                          )
                        
                        andere <- mut %>%
                          filter(mutationstyp != "Fusion") %>%
                          mutate(text = paste0(mutationstyp, " per ", format(as.Date(mutationsdatum), "%d.%m.%Y"), ": ",
                                               gemeinde_name_alt, " → ", gemeinde_name_neu))
                        
                        bind_rows(fusionen, andere) %>%
                          arrange(mutationsdatum) %>%
                          pull(text) %>%
                          paste(collapse = " | ")
                      }
                    },
                NA
        )
        if (is.na(v) || is.null(v)) return(list()) else return(list(list(str = jsonlite::unbox(as.character(v)))))
      })
      names(vals) <- prop_ids
      vals
    })
    names(rows) <- ids
    meta        <- lapply(prop_ids, function(p) list(id = jsonlite::unbox(p), name = jsonlite::unbox(p)))
    return(list(meta = meta, rows = rows))
  }
  
  # 3. STANDARD QUERIES
  q_list  <- jsonlite::fromJSON(queries, simplifyVector = FALSE)
  results <- lapply(names(q_list), function(q_id) {
    q         <- q_list[[q_id]]
    query_str <- q$query
    limit     <- q$limit %||% 5
    type      <- q$type  %||% "gemeinde"
    
    dict      <- switch(type, bezirk = bezirke, raumplanungsregion = raumplanungsregionen, gemeinden)
    col_id    <- switch(type, bezirk = "bezirk_code", raumplanungsregion = "raumplanungsregion_code", "gemeinde_code")
    col_name  <- switch(type, bezirk = "bezirk_name", raumplanungsregion = "raumplanungsregion_name", "gemeinde_name")
    type_name <- switch(type, bezirk = "Bezirk", raumplanungsregion = "Raumplanungsregion", "Gemeinde")
    
    vorschlaege <- namens_suche(input = query_str, dictionary = dict)
    if (!is.data.frame(vorschlaege)) return(list(result = list()))
    
    matches <- head(vorschlaege, limit) %>% mutate(id_val = as.character(.data[[col_id]]), name_val = as.character(.data[[col_name]]))
    
    res_items <- lapply(seq_len(nrow(matches)), function(i) {
      row <- matches[i, ]
      encoded_id <- paste0(type, ":", row$id_val)
      list(
        id    = unbox(encoded_id),
        name  = unbox(row$name_val),
        score = unbox(100L - (i - 1L) * 10L),
        match = unbox(i == 1 && normalize(row$name_val) == normalize(query_str)),
        type  = list(list(id = unbox(type), name = unbox(type_name))),
        uri   = unbox(paste0(base_url, "/api/reconcile/preview?id=", encoded_id))
      )
    })
    list(result = res_items)
  })
  names(results) <- names(q_list)
  results
}

#* @get /api/reconcile/properties
#* @serializer unboxedJSON
function(type = "gemeinde", limit = NULL) {
  all_props <- list(
    list(id = "gemeinde_code",           name = "gemeinde_code"),
    list(id = "gemeinde_name",           name = "gemeinde_name"),
    list(id = "bezirk_code",             name = "bezirk_code"),
    list(id = "bezirk_name",             name = "bezirk_name"),
    list(id = "raumplanungsregion_code", name = "raumplanungsregion_code"),
    list(id = "raumplanungsregion_name", name = "raumplanungsregion_name"),
    list(id = "gemeindemutationen", name = "gemeindemutationen")
  )
  
  relevant <- switch(type,
                     bezirk             = c("bezirk_code", "bezirk_name"),
                     raumplanungsregion = c("raumplanungsregion_code", "raumplanungsregion_name"),
                     c("gemeinde_code", "gemeinde_name", "bezirk_code", "bezirk_name",
                       "raumplanungsregion_code", "raumplanungsregion_name", "gemeindemutationen")
  )
  
  props_unboxed <- lapply(Filter(function(p) p$id %in% relevant, all_props), function(p) {
    list(id = jsonlite::unbox(p$id), name = jsonlite::unbox(p$name))
  })
  
  response <- list(type = jsonlite::unbox(type), properties = props_unboxed)
  if (!is.null(limit)) response$limit <- jsonlite::unbox(as.integer(limit))
  return(response)
}

#* @get /api/reconcile/view
#* @serializer html
function(req, res, id) {
  parsed     <- parse_encoded_id(id)
  res$status <- 302L
  res$setHeader("Location", paste0(get_base_url(req), "/", parsed$type, "/", parsed$id_num, ".html"))
  NULL
}

#* @get /api/reconcile/preview
#* @serializer html
function(req, id) {
  parsed <- parse_encoded_id(id)
  type   <- parsed$type
  id_num <- parsed$id_num
  
  info <- switch(type,
                 bezirk = {
                   b <- bezirke %>% filter(bezirk_code == id_num)
                   if (nrow(b) == 0) return("<div class='error'>Nicht gefunden</div>")
                   g <- gemeindezuweisungen %>% filter(bezirk_code == id_num) %>% select(gemeinde_name) %>% distinct() %>% arrange(gemeinde_name)
                   paste0("<span class='badge'>Bezirk</span><h3>", b$bezirk_name[1], "</h3><div class='meta-grid'><div><span class='label'>Bezirk-Code</span><span class='val'>", b$bezirk_code[1], "</span></div><div><span class='label'>Gemeinden</span><span class='val'>", nrow(g), "</span></div></div><p class='list-title'>Zugehörige Gemeinden:</p><p class='scroll-list'>", paste(g$gemeinde_name, collapse = ", "), "</p>")
                 },
                 raumplanungsregion = {
                   r <- raumplanungsregionen %>% filter(raumplanungsregion_code == id_num)
                   if (nrow(r) == 0) return("<div class='error'>Nicht gefunden</div>")
                   g <- gemeindezuweisungen %>% filter(raumplanungsregion_code == id_num) %>% select(gemeinde_name) %>% distinct() %>% arrange(gemeinde_name)
                   paste0("<span class='badge' style='background:#e1f5fe; color:#0288d1;'>Raumplanungsregion</span><h3>", r$raumplanungsregion_name[1], "</h3><div class='meta-grid'><div><span class='label'>Raumplanungsregion-Code</span><span class='val'>", r$raumplanungsregion_code[1], "</span></div><div><span class='label'>Gemeinden</span><span class='val'>", nrow(g), "</span></div></div><p class='list-title'>Zugehörige Gemeinden:</p><p class='scroll-list'>", paste(g$gemeinde_name, collapse = ", "), "</p>")
                 },
                 {
                   gm <- gemeinden %>% filter(gemeinde_code == id_num)
                   if (nrow(gm) == 0) return("<div class='error'>Nicht gefunden</div>")
                   gz  <- gemeindezuweisungen %>% filter(gemeinde_code == id_num)
                   lon <- 8.2275; lat <- 46.8182
                   
                   tryCatch({
                     res_geo <- jsonlite::fromJSON(paste0("https://api3.geo.admin.ch/rest/services/api/SearchServer?searchText=", utils::URLencode(gm$gemeinde_name[1]), "&type=locations&origins=gg25&sr=4326"))
                     if (length(res_geo$results) > 0) { lon <- res_geo$results$attrs$x[1]; lat <- res_geo$results$attrs$y[1] }
                   }, error = function(e) {})
                   
                   map_url <- paste0("https://wms.geo.admin.ch/?SERVICE=WMS&VERSION=1.3.0&REQUEST=GetMap&LAYERS=ch.swisstopo.pixelkarte-farbe,ch.swisstopo.swissboundaries3d-gemeinde-flaeche.fill&BBOX=", lat - 0.15, ",", lon - 0.485, ",", lat + 0.15, ",", lon + 0.485, "&CRS=EPSG:4326&WIDTH=660&HEIGHT=300&FORMAT=image/png&STYLES=,")
                   
                   paste0("<span class='badge' style='background:#e8f5e9; color:#2e7d32;'>Gemeinde</span><h3>", gm$gemeinde_name[1], "</h3><div class='meta-grid'><div><span class='label'>Gemeinde-Code (Bfsnr)</span><span class='val'>", gm$gemeinde_code[1], "</span></div><div><span class='label'>Bezirk</span><span class='val'>", if (nrow(gz) > 0) gz$bezirk_name[1] else "–", "</span></div><div><span class='label'>Raumplanungsregion</span><span class='val'>", if (nrow(gz) > 0) gz$raumplanungsregion_name[1] else "–", "</span></div></div><div style='position:relative; width:100%; height:auto; border-radius:6px; overflow:hidden; margin-top:12px; box-shadow: 0 1px 4px rgba(0,0,0,0.15);'><img src='", map_url, "' style='width:100%; height:auto; display:block;'><div style='position:absolute; top:50%; left:50%; width:8px; height:8px; background-color:red; border:2px solid white; border-radius:50%; transform:translate(-50%, -50%); box-shadow: 0 0 4px rgba(0,0,0,0.5);'></div></div>")
                 }
  )
  
  html_page <- paste0('<!DOCTYPE html><html><head><meta charset="utf-8"><style>body { font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif; font-size: 12px; margin: 12px; color: #2c3e50; background: #fff; line-height: 1.4; } h3 { margin: 4px 0 8px 0; font-size: 16px; font-weight: 600; color: #1a252f; } .badge { display: inline-block; padding: 2px 6px; font-size: 10px; font-weight: bold; background: #f3e5f5; color: #7b1fa2; border-radius: 4px; text-transform: uppercase; } .meta-grid { display: flex; gap: 10px; margin: 8px 0; background: #f8f9fa; padding: 6px 10px; border-radius: 6px; border: 1px solid #e9ecef; } .meta-grid > div { flex: 1; } .label { font-size: 10px; color: #7f8c8d; display: block; text-transform: uppercase; margin-bottom: 2px; } .val { font-size: 13px; font-weight: bold; color: #2980b9; } .list-title { font-weight: 600; margin: 8px 0 2px 0; color: #7f8c8d; font-size: 11px; } .scroll-list { max-height: 60px; overflow-y: auto; background: #fff; border: 1px dashed #dcdde1; padding: 4px; border-radius: 4px; color: #57606f; margin: 0; font-size: 11px; } .error { color: #c0392b; font-weight: bold; padding: 10px; text-align: center; }</style></head><body>', info, '</body></html>')
  return(html_page)
}

#* @get /api/reconcile/suggest/type
#* @serializer json
function(prefix = "") {
  # OpenRefine fragt diesen Endpunkt ab, um zu wissen, welche Typen es zur Auswahl gibt.
  # Typen anhand des Präfixes.
  all_types <- list(
    list(id = jsonlite::unbox("gemeinde"),           name = jsonlite::unbox("Gemeinde"),           description = jsonlite::unbox("Politische Gemeinden des Kantons Zürich")),
    list(id = jsonlite::unbox("bezirk"),             name = jsonlite::unbox("Bezirk"),             description = jsonlite::unbox("Bezirke des Kantons Zürich")),
    list(id = jsonlite::unbox("raumplanungsregion"), name = jsonlite::unbox("Raumplanungsregion"), description = jsonlite::unbox("Raumplanungsregionen des Kantons Zürich"))
  )
  
  if (nchar(trimws(prefix)) == 0) {
    return(list(result = all_types))
  }
  
  filtered_types <- Filter(function(t) grepl(tolower(prefix), tolower(t$name)), all_types)
  list(result = filtered_types)
}

#* @get /api/reconcile/suggest/entity
#* @param prefix Suchbegriff
#* @serializer json
function(req, prefix = "") {
  if (nchar(trimws(prefix)) == 0) return(list(result = list()))
  
  such_clean <- normalize(prefix)
  
  # Hilfsfunktion: Suche in einem Dictionary
  suche_in <- function(dict, col_id, col_name, type_id, type_label) {
    d <- dict %>% mutate(name_clean = normalize(.data[[col_name]]))
    
    treffer <- d %>% filter(name_clean == such_clean)
    if (nrow(treffer) < 1) treffer <- d %>% filter(str_detect(name_clean, fixed(such_clean)))
    if (nrow(treffer) < 1) {
      treffer <- tibble(eingabe = such_clean) %>%
        fuzzyjoin::stringdist_left_join(d, by = c("eingabe" = "name_clean"), max_dist = 2) %>%
        filter(!is.na(.data[[col_name]]))
    }
    if (nrow(treffer) < 1) return(list())
    
    lapply(seq_len(min(nrow(treffer), 5)), function(i) {
      list(
        id          = jsonlite::unbox(paste0(type_id, ":", treffer[[col_id]][i])),
        name        = jsonlite::unbox(paste0(treffer[[col_name]][i])),
        description = jsonlite::unbox(paste0(type_label, " · Code: ", treffer[[col_id]][i]))
      )
    })
  }
  
  # Alle drei Typen durchsuchen
  res_items <- c(
    suche_in(gemeinden,            "gemeinde_code",           "gemeinde_name",           "gemeinde",           "Gemeinde"),
    suche_in(bezirke,              "bezirk_code",             "bezirk_name",             "bezirk",             "Bezirk"),
    suche_in(raumplanungsregionen, "raumplanungsregion_code", "raumplanungsregion_name", "raumplanungsregion", "Raumplanungsregion")
  )
  
  list(result = res_items)
}

#* @get /api/reconcile/suggest/property
#* @serializer unboxedJSON
function(prefix = "") { list(result = list()) }


#* @get /api/health
#* @responseContentType application/json
function() {
  list(status = unbox("healthy"), timestamp = unbox(Sys.time()), data_available = unbox(!is.null(gemeinden)))
}

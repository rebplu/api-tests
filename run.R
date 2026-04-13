library(plumber)
library(jsonlite)

api <- plumb("plumber.R")

# --- Hilfsfunktion: Spalten eines Dataframes als OpenAPI-Properties ---
df_to_properties <- function(df) {
  lapply(df, function(col) {
    list(type = switch(class(col)[1],
                       "numeric"   = "number",
                       "integer"   = "integer",
                       "character" = "string",
                       "Date"      = "string",
                       "logical"   = "boolean",
                       "string"
    ))
  })
}

# --- Basis-Properties aus den Dataframes ---
p_gemeinde           <- df_to_properties(gemeinden)
p_bezirk             <- df_to_properties(bezirke)
p_region             <- df_to_properties(raumplanungsregionen)
p_zuweisung          <- df_to_properties(gemeindezuweisungen)
p_mutation           <- df_to_properties(gemeindemutationen)
p_hist               <- df_to_properties(gemeindenhist)
p_gemeinde_kurz      <- df_to_properties(gemeindezuweisungen %>% select(gemeinde_code, gemeinde_name))
p_bezirk_kurz        <- df_to_properties(gemeindezuweisungen %>% select(bezirk_code, bezirk_name))
p_region_kurz        <- df_to_properties(gemeindezuweisungen %>% select(raumplanungsregion_code, raumplanungsregion_name))

# --- Schemas ---
schemas <- list(
  
  # /api/gemeinden → { gemeinden: [...] }
  gemeinden = list(
    type = "object",
    properties = list(
      gemeinden = list(type = "array", items = list(type = "object", properties = p_gemeinde))
    )
  ),
  
  # /api/gemeinden/{gemeinde_code} → { gemeinde: {...} }
  `gemeinden-gemeinde_code` = list(
    type = "object",
    properties = list(
      gemeinde = list(type = "object", properties = p_gemeinde_kurz)
    )
  ),
  
  # /api/gemeinden/gemeinde_name → { name, treffer: [{gemeinde, bezirk, raumplanungsregion}] }
  `gemeinden-gemeinde_name` = list(
    type = "object",
    properties = list(
      name    = list(type = "string"),
      treffer = list(
        type = "array",
        items = list(
          type = "object",
          properties = list(
            gemeinde           = list(type = "object", properties = p_gemeinde_kurz),
            bezirk             = list(type = "object", properties = p_bezirk_kurz),
            raumplanungsregion = list(type = "object", properties = p_region_kurz)
          )
        )
      ),
      info = list(type = "string", description = "Gesetzt wenn kein Treffer gefunden")
    )
  ),
  
  # /api/gemeindezuweisungen → { gemeindezuweisungen: [...] }
  gemeindezuweisungen = list(
    type = "object",
    properties = list(
      gemeindezuweisungen = list(type = "array", items = list(type = "object", properties = p_zuweisung))
    )
  ),
  
  # /api/gemeindezuweisungen/{gemeinde_code} → { gemeinde, bezirk, raumplanungsregion }
  `gemeindezuweisungen-gemeinde_code` = list(
    type = "object",
    properties = list(
      gemeinde           = list(type = "object", properties = p_gemeinde_kurz),
      bezirk             = list(type = "object", properties = p_bezirk_kurz),
      raumplanungsregion = list(type = "object", properties = p_region_kurz)
    )
  ),
  
  # /api/bezirke → { bezirke: [...] }
  bezirke = list(
    type = "object",
    properties = list(
      bezirke = list(type = "array", items = list(type = "object", properties = p_bezirk))
    )
  ),
  
  # /api/bezirke/{bezirk_code} → { bezirk, gemeinden: [...] }
  `bezirke-bezirk_code` = list(
    type = "object",
    properties = list(
      bezirk    = list(type = "object", properties = p_bezirk_kurz),
      gemeinden = list(type = "array", items = list(type = "object", properties = p_gemeinde_kurz))
    )
  ),
  
  # /api/bezirke/bezirk_name → { name, treffer: [{bezirk, gemeinden}] }
  `bezirke-bezirk_name` = list(
    type = "object",
    properties = list(
      name    = list(type = "string"),
      treffer = list(
        type = "array",
        items = list(
          type = "object",
          properties = list(
            bezirk    = list(type = "object", properties = p_bezirk_kurz),
            gemeinden = list(type = "array", items = list(type = "object", properties = p_gemeinde_kurz))
          )
        )
      ),
      info = list(type = "string", description = "Gesetzt wenn kein Treffer gefunden")
    )
  ),
  
  # /api/raumplanungsregionen → { raumplanungsregionen: [...] }
  raumplanungsregionen = list(
    type = "object",
    properties = list(
      raumplanungsregionen = list(type = "array", items = list(type = "object", properties = p_region))
    )
  ),
  
  # /api/raumplanungsregionen/{region_code} → { raumplanungsregion, gemeinden: [...] }
  `raumplanungsregionen-region_code` = list(
    type = "object",
    properties = list(
      raumplanungsregion = list(type = "object", properties = p_region_kurz),
      gemeinden          = list(type = "array", items = list(type = "object", properties = p_gemeinde_kurz))
    )
  ),
  
  # /api/raumplanungsregionen/raumplanungsregion_name → { name, treffer: [{raumplanungsregion, gemeinden}] }
  `raumplanungsregionen-raumplanungsregion_name` = list(
    type = "object",
    properties = list(
      name    = list(type = "string"),
      treffer = list(
        type = "array",
        items = list(
          type = "object",
          properties = list(
            raumplanungsregion = list(type = "object", properties = p_region_kurz),
            gemeinden          = list(type = "array", items = list(type = "object", properties = p_gemeinde_kurz))
          )
        )
      ),
      info = list(type = "string", description = "Gesetzt wenn kein Treffer gefunden")
    )
  ),
  
  # /api/gemeindemutationen → { gemeindemutationen: [...] }
  gemeindemutationen = list(
    type = "object",
    properties = list(
      gemeindemutationen = list(type = "array", items = list(type = "object", properties = p_mutation))
    )
  ),
  
  # /api/gemeindefusionen/{gemeinde_code} → { gemeinde_code, gemeinde_name }
  `gemeindefusionen-gemeinde_code` = list(
    type = "object",
    properties = list(
      gemeinde_code = list(type = "integer"),
      gemeinde_name = list(type = "string")
    )
  ),
  
  # /api/gemeindenhist → { gemeindenhist: [...] }
  gemeindenhist = list(
    type = "object",
    properties = list(
      gemeindenhist = list(type = "array", items = list(type = "object", properties = p_hist))
    )
  ),
  
  # /api/gemeindenhist/{jahr} → { jahr, gemeinden: [...] }
  `gemeindenhist-jahr` = list(
    type = "object",
    properties = list(
      jahr      = list(type = "integer"),
      gemeinden = list(type = "array", items = list(type = "object", properties = p_hist))
    )
  ),
  
  # /api/gemeindenhist/{jahr}/{gemeinde_code} → { gemeinde_code, jahr, daten: {...} }
  `gemeindenhist-jahr-gemeinde_code` = list(
    type = "object",
    properties = list(
      gemeinde_code = list(type = "integer"),
      jahr          = list(type = "integer"),
      daten         = list(type = "object", properties = p_hist)
    )
  ),
  
  # /api/health → { status, timestamp, data_available }
  health = list(
    type = "object",
    properties = list(
      status         = list(type = "string"),
      timestamp      = list(type = "string"),
      data_available = list(type = "boolean")
    )
  )
)

# --- Refs pro Endpoint ---
refs <- list(
  "/api/gemeinden"                                   = "gemeinden",
  "/api/gemeinden/{gemeinde_code}"                   = "gemeinden-gemeinde_code",
  "/api/gemeinden/gemeinde_name"                      = "gemeinden-gemeinde_name",
  "/api/gemeindezuweisungen"                         = "gemeindezuweisungen",
  "/api/gemeindezuweisungen/{gemeinde_code}"         = "gemeindezuweisungen-gemeinde_code",
  "/api/bezirke"                                     = "bezirke",
  "/api/bezirke/{bezirk_code}"                       = "bezirke-bezirk_code",
  "/api/bezirke/bezirk_name"                         = "bezirke-bezirk_name",
  "/api/raumplanungsregionen"                        = "raumplanungsregionen",
  "/api/raumplanungsregionen/{region_code}"          = "raumplanungsregionen-region_code",
  "/api/raumplanungsregionen/raumplanungsregion_name"= "raumplanungsregionen-raumplanungsregion_name",
  "/api/gemeindemutationen"                          = "gemeindemutationen",
  "/api/gemeindefusionen/{gemeinde_code}"            = "gemeindefusionen-gemeinde_code",
  "/api/gemeindenhist"                               = "gemeindenhist",
  "/api/gemeindenhist/{jahr}"                        = "gemeindenhist-jahr",
  "/api/gemeindenhist/{jahr}/{gemeinde_code}"        = "gemeindenhist-jahr-gemeinde_code",
  "/api/health"                                      = "health"
)

api$setApiSpec(function(spec) {
  spec$components <- list(schemas = schemas)
  
  for (path in names(refs)) {
    ref <- refs[[path]]
    spec$paths[[path]]$get$responses[["200"]] <- list(
      description = "OK",
      content = list(
        "application/json" = list(
          schema = list(`$ref` = paste0("#/components/schemas/", ref))
        )
      )
    )
  }
  spec
})

api$run(host = "127.0.0.1", port = 8000)

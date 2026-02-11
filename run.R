library(plumber)

# --- Lade die API ---
api <- plumb("plumber.R")

# --- Weiterleitung /api/doc auf __docs__ ---
api$handle("GET", "/api/doc", function(req, res) {
  res$status <- 302                       
  res$setHeader("Location", "/__docs__/")  
  res$body <- "Redirecting to Swagger UI..."
  return(res)
})

api$run(host = "127.0.0.1", port = 8000)

# Set the directory:
setwd('your_project_directory/scripts')

# Start the server
pr <- plumber::plumb("server.R")

# Add CORS headers to the router
pr$handle("OPTIONS", "*", function(res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Origin, X-Requested-With, Content-Type, Accept, Authorization")
  res$status <- 200
  return(list())
})

# Add a filter to add CORS headers to every response
pr$filter("cors", function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Origin, X-Requested-With, Content-Type, Accept, Authorization")
  plumber::forward()
})

# Start the Plumber server
pr$run(port = 8000)


# Stop the server
# pr$stop()


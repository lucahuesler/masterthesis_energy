library(future)

# Define the subset scripts
subset_scripts <- c(
  "04-aml-subset-building-class.R",
  "04-aml-subset-municipality.R",
  "04-aml-subset-construction-period.R",
  "04-aml-subset-cluster.R"

)

# Define the runtimes for each subset script
runtimes <- c(10, 600, 600, 600)  # Adjust the runtimes as needed

# Create a list to store the futures
futures <- list()

# Create a progress bar
# pb <- progress::progress_bar$new(total = length(subset_scripts), format = "[:bar] :percent :eta")

# Loop over the subset scripts
for (i in seq_along(subset_scripts)) {
  script <- subset_scripts[i]
  print(paste0("Running ", script))
  # Create a future for each subset script
  future_script <- future({
    # Adjust the runtime for the subset script
    runtime <- runtimes[i]
    
    # Set the runtime for the subset script
    source_code <- readLines(script)
    modified_code <- gsub("runtime = \\d+", paste0("runtime = ", runtime), source_code)
    eval(parse(text = modified_code))
  })
  
  # Assign a name to the future (optional)
  future::future(name = paste0("Subset_", i), future_script)
  
  # Store the future in the list
  futures[[i]] <- future_script
  
  # Update the progress bar
  #pb$tick()
}

# Close the progress bar
#pb$close()

# Wait for all futures to complete
future::waitForAll()

# Process the results (if needed)


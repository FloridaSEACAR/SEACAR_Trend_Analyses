## Script meant to produce GAM predictive models for each species
## Plots each system as an individual model for each species
library(tidymv)
library(tidygam)
library(nlme)
library(mgcv)

# Use more recent data, seagrass species only
data <- SAV4[SpeciesGroup1=="Seagrass", ]

# Min_years to display amount of data for each system/species combination
min_years <- data %>% 
  group_by(CommonIdentifier, System) %>% 
  summarise(n = n_distinct(Year),
            EarliestYear = min(Year),
            LatestYear = max(Year))
setDT(min_years)

# Grab list of unique species
species <- data[ , unique(CommonIdentifier)]
# Exclude "No grass in Quadrat"
sp_exclude <- "No grass In Quadrat"

# Model list to store model results
model_list <- list()
# Set knot-values for GAM model
k_val <- 3
# Begin looping through systems and species, generating models where possible
for(sys in sys_include){
  for(i in 1:length(species)){
    s <- species[i]
    # Skip species
    if(s %in% sp_exclude){next}
    # subset data for species and system
    species_data <- data %>%
      filter(CommonIdentifier==s, System==sys)
    # Generate gam model and append to model list
    if(nrow(species_data)>0){
      if(min_years[System==sys & CommonIdentifier==s, n] >= 10){
        model_list[[s]][[sys]] <- gam(BB_pct ~ s(relyear, k=k_val, fx=TRUE), 
                                      data=species_data)
      }
    }
  }
}

# Generate new data frame to record predictions
new_data <- expand.grid(Species = names(model_list),
                        System = sys_include,
                        relyear = seq(min(data$relyear), max(data$relyear), 
                                      by = 1))
setDT(new_data)

# model predict function
get_predictions <- function(model, new_data, sp, sys) {
  pred <- predict.gam(model, 
                      newdata = new_data[Species == sp & System == sys, ], 
                      type = "response", se.fit = TRUE)
  data.frame(relyear = new_data[Species == sp & System == sys, ]$relyear, 
             PredictedValue = pred$fit, 
             LowerCI = pred$fit - 1.96 * pred$se.fit,
             UpperCI = pred$fit + 1.96 * pred$se.fit)
}

# Store predicted models
model_results <- list()

for(sp in names(model_list)){
  # If models are generated for at least 3 Systems, plot them
  if(length(names(model_list[[sp]]))>=4){
    for(sys in names(model_list[[sp]])){
      # Get predictions and store results
      predictions <- get_predictions(model_list[[sp]][[sys]], new_data, sp, sys)
      model_results[[sp]][[sys]] <- predictions        
    }
  }
}

# List to store plot objects as they're created
plot_list <- list()

# Iterate over each species
for (sp in names(model_results)) {
  # min year cutoff - the model needs data for each system
  # Grabs the "latest early date" where all data is available
  min_year_cutoff <- min_years[CommonIdentifier==sp, max(EarliestYear)]
  
  # Combined predictions together, use sys as identifier for system
  all_predictions <- bind_rows(model_results[[sp]], .id="sys")
  setDT(all_predictions)
  
  # Scale x-axis data
  year_list <- data %>%
    filter(relyear %in% unique(all_predictions$relyear)) %>%
    group_by(relyear) %>%
    summarise(Year = list(unique(Year))) %>%
    unnest(Year)
  setDT(year_list)
  
  # Convert year cutoff into "relyear" cutoff
  relyear_cutoff <- year_list[Year==min_year_cutoff, relyear]
  
  breaks_seq <- seq(from = relyear_cutoff,
                    to = max(year_list$relyear),
                    by = 2)
  labels_seq <- seq(from = min_year_cutoff,
                    to = max(year_list$Year),
                    by = 2)
  
  # Create a plot for the current species
  plot <- ggplot(all_predictions[relyear>=relyear_cutoff, ], 
                 aes(x = relyear, y = PredictedValue, color = sys)) +
    geom_line() +
    geom_ribbon(aes(ymin = LowerCI, ymax = UpperCI, fill = sys), 
                alpha = 0.2, colour = NA) +
    labs(title = paste(sp, "Predictions for All Systems"),
         x = "Year",
         y = "Median Percent Cover (Predicted)",
         color = "System",
         fill = "System") +
    scale_x_continuous(breaks = breaks_seq, labels = labels_seq) +
    plot_theme
  
  # Store plot object
  plot_list[[sp]] <- plot
}

# Function to loop through plot objects and print/save them
# Run with png and rds below, use "print" within script
# output types = "rds", "png", or "print"
save_display_gam <- function(output){
  for(i in seq_along(plot_list)){
    # Output file name
    file_name <- paste0("output/Figures/BBSAP_",
                        gsub(" ","_",names(plot_list)[[i]]))
    if(output=="rds"){
      # Save RDS object for use in report
      saveRDS(plot_list[[i]], file = paste0(file_name,".rds"))
    } else if(output=="png"){
      # Save .png separately
      ggsave(paste0(file_name,".png"), plot_list[[i]],
             width = 8, height = 6, dpi = 300)
    } else if(output=="print"){
      # Print plots within report
      cat("  \n")
      sp_name <- glue("## {names(plot_list)[[i]]}")
      cat(sp_name, "\n\n")
      cat("  \n")
      print(plot_list[[i]])
      cat("  \n")
    }
  }
}

# save png and rds
save_display_gam("png")
save_display_gam("rds")
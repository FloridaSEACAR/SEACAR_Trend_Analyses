library(brms)
library(tidybayes)
library(stringr)
# mod <- ABAP_den_glmm9_Natural
# mod2 <- ABAP_pct_glmm_Natural
# 
# oyres_den <- broom.mixed::tidy(mod)
# oyres_pct <- broom.mixed::tidy(mod2, exponentiate=T)
# oyres_pct <- broom.mixed::tidy(mod2)
# 
# mod2_predictions <- predict(mod2, mod2$data, type="response")
# mod2_predictions <- mod2_predictions %>% as.data.table()
# mean(mod2_predictions$Estimate)

get_percent_change <- function(mod, var = "full"){
  ## Populate frame to extract results
  cols_to_expand <- c("RelYear")
  # Create a named list of sorted unique values for each column
  grid_args <- lapply(cols_to_expand, function(col) sort(unique(mod$data[[col]])))
  names(grid_args) <- cols_to_expand
  # Generate the full combination grid
  newdata <- expand.grid(grid_args)
  newdata$UniversalReefID <- NA
  if("LiveSuccess" %in% names(mod$data)){
    newdata$Trials <- 100
  } else {
    if("Subtidal" %in% names(mod$data)){
      newdata$Subtidal <- TRUE
    }
    if("QuadSize_m2" %in% names(mod$data)){
      newdata$QuadSize_m2 <- mean(mod$data$QuadSize_m2, na.rm = TRUE)
    }
  }
  # Extract posterior predictions
  epred <- posterior_epred(mod, newdata = newdata, re_formula = NA)
  # Get the mean predicted value for each year
  mean_preds <- colMeans(epred)
  # Get the median predicted value for each year
  median_preds <- apply(epred, 2, median)
  # Compute year-to-year changes (absolute) - in reponse scale
  abs_changes <- diff(median_preds)
  
  out <- data.table("Abbreviation" = ma_abrev,
                    "ParameterName" = ind,
                    "HabitatType" = hab_type,
                    "Estimate" = mean(abs_changes),
                    "StandardError" = sd(abs_changes),
                    "LowerConfidence" = quantile(abs_changes, 0.025),
                    "UpperConfidence" = quantile(abs_changes, 0.975))
  return(out)
}

get_percent_change2 <- function(mod, var = "full"){
  multi <- ifelse("LiveSuccess" %in% names(mod$data), 100, 1)
  pctplots <- plot(conditional_effects(mod, re_formula=NULL), plot=FALSE)
  df <- setDT(pctplots$RelYear$data)
  df$estimate_pct <- multi*df$estimate__
  delta_x <- diff(df$RelYear)
  delta_y <- diff(df$estimate_pct)
  delta_upper <- diff(df$upper__*multi)
  delta_lower <- diff(df$lower__*multi)
  delta_se <- diff(df$se__*multi)
  
  slopes <- delta_y / delta_x
  avg_slope <- mean(slopes)
  avg_upper <- mean(delta_upper / delta_x)
  avg_lower <- mean(delta_lower / delta_x)
  avg_se <- mean(delta_se / delta_x)
  
  out <- data.table("Abbreviation" = ma_abrev,
                    "ParameterName" = ind,
                    "HabitatType" = hab_type,
                    "Estimate" = avg_slope,
                    "StandardError" = mean(df$se__*multi),
                    "LowerConfidence" = avg_lower,
                    "UpperConfidence" = avg_upper,
                    "Intercept" = df[RelYear==min(mod$data$RelYear)]$estimate__*multi)
  return(out)
}

all_oyster_results <- fread("output/GLMM_AllDates_ModelResults.csv")
model_list <- unique(all_oyster_results$filename)
model_list <- str_subset(model_list, "_sh", negate = T)

m_results <- data.table()
for(mod in model_list){
  ma_abrev <- str_split_1(tail(str_split_1(mod, "/"),1),"_")[1]
  ind <- str_split_1(tail(str_split_1(mod, "/"),1),"_")[2]
  hab_type <- str_split_1(str_split_1(tail(str_split_1(mod, "/"),1),"_")[4], ".rds")[1]
  mod_i <- readRDS(mod)
  m_results <- bind_rows(m_results, get_percent_change2(mod_i))
}

merged_results <- merge(m_results, MA_All[, c("ManagedAreaName", "Abbreviation", "AreaID")])
merged_results$ParameterName <- case_when(merged_results$ParameterName=="den" ~ "Density",
                                          merged_results$ParameterName=="pct" ~ "Percent Live")

fwrite(merged_results, file = "output/model_transformations.csv")

# Function to simulate the right() function in Excel
right <- function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
}

# This function will calculate some statistics for continuous variables and
# generate a historgram and boxplot for the variable
cont_desc_analysis <- function(x, col_name) {
  # Save the current graph parameters
  op <- par()
  
  # Set graph parameters to 1 row 2 cols
  par(mfrow=c(1,2))
  
  # Get the number of observations == NA
  num_na <- length(x[is.na(x)])
  x <- na.omit(x)
  
  # Graph histogram and boxplot
  hist(x, main = col_name)
  boxplot(x, main = col_name)
  
  # Outlier detection - check for values higher/lower than the
  # first/third quartile plus/minus 1.5 times the interquartile range (IQR)
  high_outlier_limit <- quantile(x, .75, na.rm = TRUE) + 1.5*IQR(x)
  low_outlier_limit <- quantile(x, .25, na.rm = TRUE) - 1.5*IQR(x)
  
  # Calculate number of outliers
  num_high_outliers <- length(x[x > high_outlier_limit])
  num_low_outliers <- length(x[x < low_outlier_limit])
  
  # Get some standard stats
  xmax <- max(x)
  xmin <- min(x)
  xiqr <- IQR(x)
  xmedian <- median(x)
  xsd <- round(sd(x),4)
  
  #  Shapiro-Wilk Test of normalcy
  if(length(x) > 5000) {
    # Shapiro only takes up to 5000 observations
    x <- sample(x, size=5000)
  }
  
  # Get the p-value of Shapiro-Wilk
  p_val <- shapiro.test(x)$p.value
  
  # Assume normal distribution (H0)
  normal_dist <- TRUE
  if (p_val < 0.05) {
    # Reject the NULL hypothesis
    normal_dist <- FALSE
  }
  
  # Reset the graph params
  par(op)
  
  # Save the descriptive stats into a dataframe row
  toReturn <- data.frame(
    FieldName = col_name,
    Max = xmax,
    Min = xmin,
    IQR = xiqr,
    Median = xmedian,
    StdDev = xsd,
    NumNA = num_na,
    NormalDist = normal_dist,
    NumHighOutliers = num_high_outliers,
    NumLowOutliers = num_low_outliers,
    stringsAsFactors = FALSE
  )
  
  # Return the descriptive stats
  return(toReturn)
}

# This funtion gereates a single stacked bar graph showing the
# proportion of each value
cat_graph <- function(x, col_name) {
  x %>% 
    ggplot(aes(fill = cat_level, y = pct, x = NA)) +
    geom_bar(position="fill", stat="identity", width = .5) +
    theme(panel.background = element_blank()) +
    ggtitle(col_name) + ylab("Proportions") +
    scale_fill_manual(values = c("black", laxGray, laxMaroon, "#856C00", "#00856C", "#001885")) +
    scale_y_continuous(labels = scales::percent) +
    theme(legend.position="right", axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.title=element_blank()) +
    xlab(col_name) +
    geom_text(aes(label = paste0(round(pct*100), "%")), position = position_stack(vjust = 0.5), size = 3, 
              color="white", fontface = "bold")
}

# Helper function for adding correlation coeficient values to a pairwise plot
# (taken from pairs() help page).
# As seen on: https://rpubs.com/mudassirkhan19/regression-model
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

## put histograms on the diagonal
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = laxMaroon, ...)
}

# Use the Chi-Squared Test of Independence between two categorical values
calc_indep <- function(col_names, df, sig_level = 0.05) {
  # Get the number of permutations to size the result dataframe
  n <-((length(col_names)-1) * length(col_names))/2
  
  # Create a dataframe to hold the summary results
  indep_summary <- data.frame(
    "Column1" = character(n),
    "Column2" = character(n),
    "Chi.Squared" = numeric(n),
    "p.value" = numeric(n),
    "Independent" = logical(n),
    stringsAsFactors = FALSE
  )
  
  i <- 1 # Index
  col2_max <- length(col_names) # Max value of col 2 = last col index
  col1_max = col2_max - 1 # Max value of col 1 = second to last col index
  
  # Do a double loop to compare each column to all the subsequent columns
  for(col1 in 1:col1_max) { 
    for(col2 in (col1+1):col2_max) {
      # Get the column names
      col1_name <- col_names[col1]
      col2_name <- col_names[col2]
      
      #print(paste("Names", col1_name, col2_name))
      
      # Get the number of unique values in each category
      col1_levels <- dim(df %>% group_by(df[[col1_name]]) %>% tally())[1]
      col2_levels <- dim(df %>% group_by(df[[col2_name]]) %>% tally())[1]
      
      #print(paste("Levels", col1_levels, col2_levels))
      
      if(col1_levels > 1 && col2_levels > 1) { # Ignore any columns with one level
        #  Perform the independence test
        indep_test <- chisq.test(df[[col1_name]], df[[col2_name]])
        
        # H0 = There are no relationships between the categorical variables (Independent == TRUE)
        
        # Store the values for this test
        indep_summary[i, "Column1"] <- col1_name
        indep_summary[i, "Column2"] <- col2_name
        indep_summary[i, "Chi.Squared"] <- indep_test$statistic
        indep_summary[i, "p.value"] <- indep_test$p.value
        indep_summary[i, "Independent"] <- if (indep_test$p.value < sig_level) FALSE else TRUE
        
        i <- i + 1 # Increment index
      } # end if
      
    } # end inner loop
  } # end outer loop
  
  indep_summary <- indep_summary %>% filter(Column1 != "") # Filter out empty rows
  return(indep_summary)
  
} # end function

# Loops through a vector of continuous column names and
# calculates the coefficient of correlation between all columns
# Pass in a vector of column names and a data frame that they belong to
cor_filter <- function(cols, df) { 
  high_cor <- FALSE # Default to low coef of cor
  
  # Dataframe to store the results
  result <- data.frame(
    col1 = character(),
    col2 = character(),
    cor_coef = double(),
    high_cor = logical(),
    stringsAsFactors = FALSE
  )
  
  n <- length(cols) # Get the number of columns
  
  # The outer loop will start from column 1 and go to the second
  # to  last column since we do not want to compare a column with itself
  for(col1 in 1:(n-1)) {
    
    # The inner loop will start from the column to the right of the current
    # outer loop column and go to the end of the data frame
    for(col2 in (col1+1):n) {
      # Get the column names
      col1_name <- cols[col1]
      col2_name <- cols[col2]
      
      print(paste("Names", col1_name, col2_name))
      
      # Calculate the coefficient of correlation
      cor_coef <- cor(df[[col1_name]], df[[col2_name]])
      
      # Use a threshold of +- 0.9 to say if the variables are highly correlated
      if(cor_coef >= 0.9 | cor_coef <= -0.9) high_cor <- TRUE else high_cor <- FALSE
      
      # Add the result to the dataframe
      result <- rbind.data.frame(result, matrix(c(col1_name, col2_name, cor_coef, high_cor), ncol = 4), stringsAsFactors = FALSE)
    }
  }
  
  # Set the column names
  colnames(result) <- c("Column1", "Column2", "Coef.of.Correlation", "Highly.Correlated")
  
  #  Convert the coef of correlation column to a number from a factor
  result$Coef.of.Correlation <- as.numeric(result$Coef.of.Correlation)
  
  # Send the results back
  return(result)
}


# Calculate the significance between the each continuous variable in a set
# and the response variable.
significant_pred_vars <- function(resp_var, pred_vars, df, chi_sq = TRUE, sig_level = 0.1) {
  n <- length(pred_vars)
  
  result <- data.frame(
    Response.Var = character(n),
    Predictor.Var = character(n),
    p.value = numeric(n),
    Significant = logical(n),
    stringsAsFactors = FALSE
  )

  for(i in 1:n) {
    col_name <- pred_vars[i]
    
    if(chi_sq == TRUE) {
      p <- chisq.test(df[[resp_var]], df[[col_name]])$p.value
    } else {
      p <- wilcox.test(df[[col_name]] ~ df[[resp_var]])$p.value
    }

    result[i, "Response.Var"] <- resp_var
    result[i, "Predictor.Var"] <- col_name
    result[i, "p.value"] <- p
    result[i, "Significant"] <- if(p <= sig_level) TRUE else FALSE
  }
  
  result <- result %>% arrange(p.value)
  return(result)
}

calc_acc = function(actual, predicted) {
  mean(actual == predicted)
}

get_best_log_thresh <- function(act, pred) {
  thres.v <- seq(from = 0.05, to = 0.95, by = 0.05)
  n <- length(thres.v)
  acc.df <- data.frame(
    threshold = numeric(n),
    accuracy = numeric(n),
    stringsAsFactors = FALSE
  )
  
  for(t in 1:n) {
    pred.v <- ifelse(pred >= thres.v[t], 1, 0)
    acc.df[t, 1] <- thres.v[t]
    acc.df[t, 2] <- calc_acc(actual = act, predicted = pred.v)
  }
  
  max.acc <- max(acc.df$accuracy)
  return(acc.df[which(acc.df$accuracy == max.acc),])
}

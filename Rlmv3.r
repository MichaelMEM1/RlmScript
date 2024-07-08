args <- commandArgs(TRUE)
if (length(args) == 0) {
  stop("No CSV file provided. Please specify a CSV file as the first argument.")
}

file_path <- args[1]

if (!file.exists(file_path)) {
  stop(paste("File", file_path, "not found. Please check the file path."))
}

file_name <- tools::file_path_sans_ext(basename(file_path))

tryCatch({

    df <- read.csv(file_path, stringsAsFactors = FALSE)

    png(file = "plot_data_from_csv.png", width = 800, height = 600)
     plot(df$x,df$y, xlab = 'X', ylab = 'Y') 
     title(paste0('Data from (', file_name, ')'))
     legend('topleft',                          # location of the legend
        legend=c('Points'),                 # labels for the lines
        col=c('black'),                 # colors matching the lines
        pch = 1,                        # Adding a point to the legend
        title='Legend',                        # legend title
        bty='n')                              # no box around the legend
    dev.off()

    model <- lm(y~x, data = df )
    summary_stats <- summary (model)
    r_squared <- summary_stats$r.squared
    y_predict <- predict(model, df)

  png(file = "plot_Linear_RegressionR.png", width = 800, height = 600)
    plot(df$x, df$y, xlab = 'X', ylab = 'Y', col = 'black') 
    lines(df$x, y_predict, col = 'red')
    title('Linear regression model R')
    legend('topleft',                          # location of the legend
        legend = c('Points', 'Fit'),                 # labels for the lines
        col = c('black', 'red'),                 # colors matching the lines
        lwd = c(2, 2),                          # line widths
        title = 'Legend',                        # legend title
        bty = 'n')                              # no box around the legend
      text(12, 12, sprintf("R^2 = % f", r_squared), pos = 3) # adding the r square value
    dev.off()

    graphics.off()                         # turning off graphic devices
  

}, error = function(e) {

  stop("Error reading or processing the CSV file:", conditionMessage(e))
})


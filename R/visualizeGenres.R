library(dplyr)
library(stringr)
library(ggplot2)
library(scales)

# column names of the file to read: "genre","freqRecord","freqDataSource"
# diagramTitle="frequency of genre terms in metadata records and data sources", xLabel="genre", yLabel="metadata records", legendTitle = "data sources"
plotGenres <- function(file = NULL, topX = NULL, genreNamePattern = NULL, diagramTitle = NULL, xLabel = NULL, yLabel = NULL, legendTitle = NULL){
  genres <- read.csv(file = file)
  genresFiltered <- NULL
  if (is.null(topX) && is.null(genreNamePattern))
    stop("either topX or genreNamePattern must not be null")
  if (!is.null(topX)){
    genresFiltered <- genres %>% arrange(desc(freqRecord)) %>% top_n(topX, freqRecord)
  }else if (!is.null(genreNamePattern)){
    # eg. all genres with "info:eu-repo" in the term
    genresFiltered <- genres %>% filter(str_detect(genre, genreNamePattern))
  }
  return (plotBar(genresFiltered, diagramTitle, xLabel, yLabel, legendTitle))
}

# plot bar diagram of frequency of genres in metadata records and data sources
# scale axis, unit format
# custom values for titles, x and y axis
plotBar <- function(genres, diagramTitle, xLabel, yLabel, legendTitle){
  genresFiltered_p <- ggplot(data = genres, aes(x=genre)) + 
    geom_bar(stat = "identity", aes(y=freqRecord, fill=freqDataSource)) + 
    theme_minimal() + 
    scale_y_continuous(label=unit_format(unit="M", scale=1e-6)) + 
    labs(title = diagramTitle, x = xLabel, y = yLabel, fill = legendTitle)
  return( genresFiltered_p + coord_flip())
}

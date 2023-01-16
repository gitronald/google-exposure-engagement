# pivot scaling helper functions and pipeline

get_keywords_custom <- function (scores, n_dimensions, n_words = 15, stretch = 3, capture_output = FALSE, 
                                 pivots_only = TRUE, topic) 
{
  all_keywords <- list()
  if (stretch%%2 != 1) 
    stop("Please enter odd integer for \"stretch\"")
  for (i in if (length(n_dimensions) == 1) {
    1:n_dimensions
  }
  else {
    n_dimensions
  }) {
    general_keywords <- scores$vocab[order(scores$pivot_scores[, 
                                                               i + 1] * sqrt(rowSums(scores$pivot_scores[, -1]^2)), 
                                           decreasing = TRUE)]
    specific_keywords <- scores$vocab[order(scores$word_scores[, 
                                                               i + 1]^(stretch) * sqrt(rowSums(scores$pivot_scores[, 
                                                                                                                   -1]^2)), decreasing = TRUE)]
    if (pivots_only) {
      keywords <- data.frame(head(rev(general_keywords), 
                                  n = n_words), head(general_keywords, n = n_words))
      names(keywords) <- c("pivots (-)", "(+) pivots")
    }
    else {
      keywords <- data.frame(head(rev(specific_keywords), 
                                  n = n_words), head(rev(general_keywords), n = n_words), 
                             head(general_keywords, n = n_words), head(specific_keywords, 
                                                                       n = n_words))
      names(keywords) <- c("scores (-)", "pivots (-)", 
                           "(+) pivots", "(+) scores")
    }
    if (capture_output) {
      all_keywords[[paste0("D", i)]] <- keywords
    }
    else {
      if (!requireNamespace("knitr", quietly = TRUE)) {
        cat("\nDimension", i, "keywords\n\n")
        print(keywords, row.names = F)
        cat("\n")
      }
      else {
        print(knitr::kable(keywords, align = "c", format = "pandoc", 
                           caption = paste("Dimension", i, "keywords: ", topic)))
        cat("\n")
      }
    }
  }
  if (capture_output) {
    return(all_keywords)
  }
}

# function to plot pivot scaling results
plot_keywords_custom <- function (scores, x_dimension = 1, y_dimension = 2, q_cutoff = 0.9, 
                                  plot_density = FALSE, unstretch = FALSE, color = FALSE,
                                  subjname = "subject"){
    require(ggrepel)
  if (unstretch) {
    scores$word_scores <- sweep(scores$word_scores, 1, 
                                sqrt(rowSums((scores$importance[-1] * scores$pivot_scores[, -1])^2)) + 1, `/`)
  }
  word_scores <- data.frame(scores$word_scores)
  word_counts <- scores$word_counts
  above_cutoff <- word_counts > quantile(word_counts, q_cutoff)
  x_dimension <- x_dimension + 1
  y_dimension <- y_dimension + 1
  if (color & !("color" %in% names(scores))) {
    scores$color <- factor(kmeans(scores$word_scores[, 2:11], 
                                  centers = 5)$cluster)
  }
  if (!color) {
    g <- ggplot2::ggplot() + 
      ggrepel::geom_text_repel(data = word_scores[above_cutoff,], 
                         ggplot2::aes(x = word_scores[above_cutoff, x_dimension], 
                                      y = word_scores[above_cutoff, y_dimension], 
                                      label = scores$vocab[above_cutoff])) + 
      ggplot2::xlab(paste("Dimension:", x_dimension - 1)) + 
      ggplot2::ylab(paste("Dimension:", y_dimension - 1)) + 
      ggplot2::guides(size = F) + ggplot2::theme_classic() + 
      ggplot2::xlim(-max(abs(word_scores[above_cutoff,  x_dimension])), 
                    max(abs(word_scores[above_cutoff, x_dimension]))) + 
      ggplot2::ylim(-max(abs(word_scores[above_cutoff, y_dimension])), 
                    max(abs(word_scores[above_cutoff, y_dimension])))+
      ggplot2::ggtitle(paste0("Embedded dimensions in respondents' search queries in ", subjname),
                       subtitle = paste0("Top ", 100*(1-q_cutoff), "% of words shown"))
  }
  else {
    g <- ggplot2::ggplot() + 
      ggrepel::geom_text_repel(data = word_scores[above_cutoff, ], 
                         ggplot2::aes(x = word_scores[above_cutoff, x_dimension], 
                                      y = word_scores[above_cutoff, y_dimension], 
                                      label = scores$vocab[above_cutoff], 
                                      color = scores$color[above_cutoff])) + 
      ggplot2::xlab(paste("Dimension:", x_dimension - 1)) + 
      ggplot2::ylab(paste("Dimension:", y_dimension - 1)) + 
      ggplot2::guides(size = F, color = F) + ggplot2::theme_classic() + 
      ggplot2::xlim(-max(abs(word_scores[above_cutoff, 
                                         x_dimension])), 
                    max(abs(word_scores[above_cutoff, x_dimension]))) + 
      ggplot2::ylim(-max(abs(word_scores[above_cutoff, y_dimension])), 
                    max(abs(word_scores[above_cutoff,  y_dimension])))+
      ggplot2::ggtitle(paste0("Embedded dimensions in respondents' search queries in ", subjname),
                       subtitle = paste0("Top ", 100*(1-q_cutoff), "% of words shown"))
  }
  if (!plot_density) {
    return(g)
  }
  else {
    gridExtra::grid.arrange(g, ggplot2::ggplot() + 
                              ggplot2::geom_density(ggplot2::aes(x = word_scores[, x_dimension])) +
                              ggplot2::xlab(paste("Dimension:", x_dimension - 1)) + 
                              ggplot2::theme_classic(), 
                            ggplot2::ggplot() + 
                              ggplot2::geom_density(ggplot2::aes(x = word_scores[,  y_dimension])) + 
                              ggplot2::xlab(paste("Dimension",  y_dimension - 1)) + 
                              ggplot2::theme_classic(), 
                            layout_matrix = rbind(c(1, 1, 2), c(1, 1, 3)))
  }
}

flatten_text_pivot <- function(df = merge_yg, text_col = "qry", flatten_column = "party7"){
    require(stm)
    require(parrot)
    require(fixest)
    
  message("Processing... \n")
  processed <- textProcessor(
    documents = df %>% pull(text_col),
    metadata = data.frame(df),
    removestopwords=T, lowercase=T, stem=TRUE, removenumbers = TRUE,
    removepunctuation = TRUE
  )
   
    message("Prepping... \n")
    out <- prepDocuments(
    processed$documents, processed$vocab, processed$meta
  )
  message("Extracting embeddings.... \n")
  tdm <- doc_to_tdm(out)
      
  message("Calculating word scores... \n")
  scores <- scale_text(
    meta=out$meta,
    tdm=tdm,
    constrain_outliers = TRUE,
    pivot = 3,
    compress_fast = T,
    simple = F
    ##    embeddings=embeddings[["meta"]], ## embeddings have little effect
    ##    on output -- if used, consider setting pivot lower (e.g. pivot = 1/2)
  )
      
  message("Scoring documents... \n")
  df <- parrot::score_documents(scores)
  
    message("Flattening... \n")
   mod <- fixest::feols(as.formula(paste0(flatten_column, " ~ X0 + X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9")),
                                  data = df)
   df$query_right_pivot <- predict(mod)
    
   return(list(appended_data = df,
              textmodel = scores))
}  
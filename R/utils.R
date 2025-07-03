# Utility functions used across modules

clean_candidate_names <- function(names, type = "pres") {
  if (type == "pres") {
    gsub("pres_\\d+_", "", names)
  } else if (type == "vp") {
    gsub("vp_\\d+_", "", names)
  } else if (type == "sen") {
    gsub("sen_\\d+_", "", names)
  } else if (type == "sen25") {
    names <- gsub("^X\\d+\\.\\.|\\.\\.\\w+\\.$", "", names)
    gsub("\\.\\.", " ", names)
  } else {
    names
  }
}

get_top_candidates <- function(data, candidate_cols, n = 20) {
  totals <- colSums(data[, candidate_cols], na.rm = TRUE)
  df <- data.frame(Candidate = names(totals), Votes = totals)
  df[order(-df$Votes), ][1:n, ]
}

get_regions_won <- function(data, candidate_cols, region_col = "region") {
  region_winner <- aggregate(data[, candidate_cols],
                             by = list(Region = data[[region_col]]),
                             FUN = sum)
  
  region_winner$Winner <- apply(region_winner[, -1], 1, function(row) {
    candidate_cols[which.max(row)]
  })
  
  as.data.frame(table(region_winner$Winner))
}

get_provinces_won <- function(data, candidate_cols, province_col = "province") {
  province_winner <- aggregate(data[, candidate_cols],
                               by = list(Province = data[[province_col]]),
                               FUN = sum)
  
  province_winner$Winner <- apply(province_winner[, -1], 1, function(row) {
    candidate_cols[which.max(row)]
  })
  
  as.data.frame(table(province_winner$Winner))
}

create_bar_plot <- function(data, x_var, y_var, title, x_lab, y_lab, fill_color = "steelblue") {
  p <- ggplot(data, aes(x = reorder(.data[[x_var]], -.data[[y_var]]), y = .data[[y_var]])) +
    geom_bar(stat = "identity", fill = fill_color) +
    labs(title = title, x = x_lab, y = y_lab) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggplotly(p)
}

create_candidate_plot <- function(data, title, fill_color = "steelblue") {
  p <- ggplot(data, aes(x = reorder(Candidate, Votes), y = Votes)) +
    geom_bar(stat = "identity", fill = fill_color) +
    coord_flip() +
    labs(title = title, x = "Candidate", y = "Total Votes")
  ggplotly(p)
}

create_winners_plot <- function(data, title, fill_color = "darkred") {
  p <- ggplot(data, aes(x = reorder(Var1, Freq), y = Freq)) +
    geom_bar(stat = "identity", fill = fill_color) +
    coord_flip() +
    labs(title = title, x = "Candidate", y = "Count")
  ggplotly(p)
}
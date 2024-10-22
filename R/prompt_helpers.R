library(here)
library(httr2)

system_prompt <- function(df, name, categorical_threshold = 10) {
  schema <- df_to_schema(df, name, categorical_threshold)

  # Read the prompt file
  prompt_path <- here::here("md", "prompt.md")
  prompt_content <- readLines(prompt_path, warn = FALSE)
  prompt_text <- paste(prompt_content, collapse = "\n")

  # Replace the placeholder with the schema
  prompt_text <- gsub("\\$\\{SCHEMA\\}", schema, prompt_text)

  prompt_text
}

df_to_schema <- function(df, name, categorical_threshold) {
  schema <- c(paste("Table:", name), "Columns:")

  column_info <- lapply(names(df), function(column) {
    info <- list()

    info$colname <- column
    # Map R classes to SQL-like types
    info$sql_type <- if (is.integer(df[[column]])) {
      "INTEGER"
    } else if (is.numeric(df[[column]])) {
      "FLOAT"
    } else if (is.logical(df[[column]])) {
      "BOOLEAN"
    } else if (inherits(df[[column]], "POSIXt")) {
      "DATETIME"
    } else {
      "TEXT"
    }
    info$description <- attr(df[[column]], "label", exact = TRUE)

    # For TEXT columns, check if they're categorical
    if (info$sql_type == "TEXT") {
      unique_values <- length(unique(df[[column]]))
      if (unique_values <= categorical_threshold) {
        categories <- unique(df[[column]])
        categories_str <- paste0("\n  - '", categories, "'", collapse = "")
        info$categorical_values <- categories_str
      }
    }

    if (info$sql_type %in% c("INTEGER", "FLOAT", "DATETIME")) {
      min_val <- min(df[[column]], na.rm = TRUE)
      max_val <- max(df[[column]], na.rm = TRUE)
      info$min <- min_val
      info$max <- max_val
    }

    # Drop nulls
    info <- info[!vapply(info, is.null, logical(1))]
    prefix <- vapply(
      seq_along(info),
      function(i) if (i == 1) "- " else "  ",
      character(1)
    )

    paste0(paste0(prefix, names(info), ": ", unlist(info)), collapse = "\n")
  })

  schema <- c(schema, unlist(column_info))
  return(paste(schema, collapse = "\n"))
}

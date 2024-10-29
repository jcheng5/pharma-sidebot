here::i_am("app.R")

library(shiny)
library(bslib)
library(elmer)
library(shinychat)
library(DBI)
library(duckdb)
library(rlang)
library(dplyr)
library(dotenv)

greeting <- here::here("md", "greeting.md") |>
  readLines(warn = FALSE) |>
  paste(collapse = "\n")

adsl <- haven::read_xpt(here::here("data/adsl.xpt"))
adtte <- haven::read_xpt(here::here("data/adtte.xpt"))

# Duplicate fields are ones that appear in both datasets, besides STUDYID or USUBJID
duplicate_fields <- setdiff(intersect(names(adsl), names(adtte)), c("STUDYID", "USUBJID"))
# Ensure that all duplicate fields are identical
stopifnot(length(waldo::compare(adsl[duplicate_fields], adtte[duplicate_fields], ignore_attr = TRUE)) == 0)

anl <- adsl |>
  dplyr::filter(
    SAFFL == "Y",
    STUDYID == "CDISCPILOT01"
  ) |>
  # dplyr::select(STUDYID, USUBJID, TRT01A) |>
  dplyr::inner_join(
    dplyr::filter(
      adtte, STUDYID == "CDISCPILOT01"
    ) |> dplyr::select(-all_of(duplicate_fields)), #|> dplyr::select(STUDYID, USUBJID, AVAL, CNSR, PARAM, PARAMCD),
    by = c("STUDYID", "USUBJID")
  ) |>
  dplyr::mutate(
    TRT01A = factor(TRT01A, levels = c("Placebo", "Xanomeline Low Dose",  "Xanomeline High Dose")),
    AVAL = AVAL/30.4167
  )

# Open the duckdb database
conn <- dbConnect(duckdb())
# Close the database when the app stops
onStop(\() dbDisconnect(conn))

dbWriteTable(conn, "anl", anl)

ui <- page_sidebar(
  sidebar = sidebar(
    width = 400,
    chat_ui("chat", fill = TRUE),
    open = FALSE
  ),
  tags$head(tags$link(rel = "stylesheet", href = "styles.css")),
  textOutput("show_title", container = h4),
  verbatimTextOutput("show_query"),
  layout_columns(fill = FALSE, col_widths = c(5, 7),
    value_box("Subjects", textOutput("subjects", inline = TRUE)),
    card(
      tableOutput("subjects_by_arm"),
    )
  ),
  card(
    plotOutput("plot", fill = TRUE)
  )
)

server <- function(input, output, session) {
  # 🔄 Reactive state/computation --------------------------------------------

  current_title <- reactiveVal(NULL)
  current_query <- reactiveVal("")

  df <- reactive({
    sql <- current_query()
    if (is.null(sql) || sql == "") {
      sql <- "SELECT * FROM anl;"
    }
    dbGetQuery(conn, sql)
  })

  output$show_title <- renderText({
    current_title()
  })

  output$show_query <- renderText({
    current_query()
  })

  output$subjects <- renderText({
    glue::glue("{nrow(df())}/{nrow(anl)}")
  })

  output$subjects_by_arm <- renderTable({
    df() |> group_by(ARM) |> tally(name = "Shown") |> right_join(
      by = "ARM",
      anl |> group_by(ARM) |> tally(name = "Total")
    )
  })

  output$plot <- renderPlot({
    # detect the error
    shiny::validate(
      shiny::need(nrow(df()) > 5, "Not enough observations for this selection. Modify filters and try again.")
    )

    ## -----------------------------------------------------------------------------------------------------------------------------------
    # estimate survival
    surv_mod <- visR::estimate_KM(data = df(), strata = "TRT01A")

    #
    # # save plot
    ggplot2::theme_set(ggplot2::theme_bw())

    KM <- visR::visr(surv_mod,
      y_label = "Survival Probability (%)",
      x_label = "Time (Months)",
      fun = "pct",
      legend_position = "bottom" ) |>
      visR::add_CNSR() |>
      visR::add_CI()

    KM <- KM +
      ggplot2::theme(axis.text = ggplot2::element_text(size = ggplot2::rel(1.3)),
        axis.title = ggplot2::element_text(size = ggplot2::rel(1.4)),
        legend.text = ggplot2::element_text(size = ggplot2::rel(1.3)),
        legend.title = ggplot2::element_text(size = ggplot2::rel(1.4))) +
      ggplot2::geom_hline(yintercept=0.5, linetype = "dashed")

    # KM <- KM |>
    #   add_risktable2(group = "statlist")

    title <- cowplot::ggdraw() +
      cowplot::draw_label(
        "KM plot for Time to First Dermatologic Event",
        fontfamily = "sans",
        fontface = "bold",
        size=16
      )

    caption <- cowplot::ggdraw() +
      cowplot::draw_label(
        paste(
          "The shaded areas are 95% CI of the survival probability for each group",
          "\n",
          paste0(Sys.time())
        ),
        fontfamily = "sans",
        size=12
      )

    KM <- cowplot::plot_grid(
      title, KM, caption,
      ncol = 1,
      rel_heights = c(0.1,0.8,0.1)
    )
    KM
  })

  # ✨ Sidebot ✨ -------------------------------------------------------------

  update_dashboard <- function(query, title) {
    if (!is.null(query)) {
      current_query(query)
    }
    if (!is.null(title)) {
      current_title(title)
    }
    chat_append("chat", glue::glue("\n\n```sql\n{query}\n```\n\n"))
  }

  query <- function(query) {
    df <- dbGetQuery(conn, query)

    code <- glue::glue("\n\n```sql\n{query}\n```\n\n")
    tbl_html <- df_to_html(df, maxrows = 5)

    chat_append("chat", paste0(code, tbl_html, "\n\n"))

    df |> jsonlite::toJSON(auto_unbox = TRUE)
  }

  prompt <- system_prompt(anl, "anl")

  chat <- chat_openai(model = "gpt-4o", system_prompt = prompt)
  chat$register_tool(tool(
    update_dashboard,
    "Modifies the data presented in the data dashboard, based on the given SQL query, and also updates the title.",
    query = type_string("A DuckDB SQL query; must be a SELECT statement."),
    title = type_string("A title to display at the top of the data dashboard, summarizing the intent of the SQL query.")
  ))
  chat$register_tool(tool(
    query,
    "Perform a SQL query on the data, and return the results as JSON.",
    query = type_string("A DuckDB SQL query; must be a SELECT statement.")
  ))

  observeEvent(input$chat_user_input, {
    stream <- chat$stream_async(input$chat_user_input)
    chat_append("chat", stream)
  })

  chat_append_message("chat", list(role = "assistant", content = greeting))
}

df_to_html <- function(df, maxrows = 5) {
  df_short <- if (nrow(df) > 10) head(df, maxrows) else df

  tbl_html <- capture.output(
    df_short |>
      xtable::xtable() |>
      print(type = "html", include.rownames = FALSE, html.table.attributes = NULL)
  ) |> paste(collapse = "\n")

  if (nrow(df_short) != nrow(df)) {
    rows_notice <- glue::glue("\n\n(Showing only the first {maxrows} rows out of {nrow(df)}.)\n")
  } else {
    rows_notice <- ""
  }

  paste0(tbl_html, "\n", rows_notice)
}

shinyApp(ui, server)

# Sidebot (Pharma Edition)

This is a demonstration of using an LLM to enhance a data dashboard written in [Shiny](https://shiny.posit.co/). It is based on code and data from the R Consortium's [R Submission Pilot 2 Project](https://github.com/rconsortium/submissions-pilot2).

To run locally, you'll need to create an `.Renviron` file in the repo root with `OPENAI_API_KEY=` followed by a valid OpenAI API key. (DO NOT commit `.Renviron` file to git!) Or if that environment value is set some other way, you can skip the `.Renviron` file.

Then run:

```r
pak::pak(c("bslib", "DBI", "dplyr", "duckdb", "fastmap", "fontawesome",
  "ggplot2", "ggridges", "here", "plotly", "reactable", "shiny",
  "hadley/elmer", "jcheng5/shinychat"))
```

(Note that [{elmer}](https://github.com/hadley/elmer) and [{shinychat}](https://github.com/jcheng5/shinychat) are highly experimental and their APIs may change.)

## Warnings and limitations

This app sends at least your data schema to a remote LLM. As written, it also permits the LLM to run SQL queries against your data and get the results back. Please keep these facts in mind when dealing with sensitive data.

library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)
library(bslib)
library(maps)
library(plotly)
library(htmlwidgets)

file_info <- file.info("retraction_watch.csv")
old_locale <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "C")
update_date <- format(file_info$mtime, "%B %d, %Y")
Sys.setlocale("LC_TIME", old_locale)

ui <- navbarPage(
  title = tagList(
    "RWD Dashboard",
    tags$span(
      paste("LATEST UPDATE:", toupper(update_date)),
      style = "position: absolute; right: 25px; top: 18px; font-size: 0.8rem; font-family: 'Inter', sans-serif; color: #666666; font-weight: 700; letter-spacing: 0.5px;"
    )
  ),
  theme = bs_theme(
    primary = "#333333", 
    secondary = "#666666"
  ),
  header = tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;700&display=swap');
      body { background-color: #f4f4f4; font-family: 'Georgia', serif; color: #333333; }
      .navbar { background-color: #ffffff !important; border-bottom: 1px solid #eaeaea; }
      .navbar-brand { font-family: 'Georgia', serif; font-weight: 700; color: #333333 !important; font-size: 1.6rem; }
      .navbar-nav .nav-link { font-family: 'Inter', sans-serif; font-weight: 700; color: #666666 !important; text-transform: uppercase; font-size: 0.85rem; letter-spacing: 1px; }
      .navbar-nav .nav-link.active { color: #222222 !important; }
      .card { background-color: #ffffff; border-radius: 0; border: 1px solid #eaeaea; box-shadow: none; margin-bottom: 2rem; }
      .card-header { font-size: 1.4rem; border-radius: 0 !important; background-color: transparent; border-bottom: none; color: #222222; font-weight: 700; font-family: 'Georgia', serif; padding: 1.5rem 1.5rem 0.5rem 1.5rem; }
      .card-body { padding: 1.5rem; }
      .kpi-card { background-color: #ffffff; color: #333333; border: 1px solid #eaeaea; border-radius: 0; padding: 20px; text-align: center; border-top: 4px solid #333333; }
      .kpi-title { font-family: 'Inter', sans-serif; font-size: 0.75rem; font-weight: 700; color: #555555; text-transform: uppercase; letter-spacing: 1.5px; }
      .kpi-value { font-family: 'Georgia', serif; font-size: 2.5rem; font-weight: 700; margin-top: 10px; color: #222222; line-height: 1; }
      .table { margin-bottom: 0; }
      .table th { border-top: none; color: #555555; font-weight: 700; text-transform: uppercase; font-size: 0.8rem; font-family: 'Inter', sans-serif; letter-spacing: 0.5px; }
      .table td { vertical-align: middle; color: #333333; font-family: 'Georgia', serif; font-size: 0.95rem; }
      .card-footer { background-color: #ffffff; border-top: 1px solid #eaeaea; border-radius: 0 !important; font-family: 'Inter', sans-serif; font-size: 0.8rem; padding: 1rem 1.5rem; }
    "))
  ),
  
  tabPanel("Overview",
           uiOutput("kpi_boxes"),
           fluidRow(
             column(6,
                    div(class = "card mb-4 border-0 shadow-sm",
                        div(class = "card-header",
                            "Original Paper Publication Years"
                        ),
                        div(class = "card-body",
                            plotlyOutput("pub_year_plot", height = "400px")
                        ),
                        div(class = "card-footer text-muted small",
                            textOutput("pub_outliers_text")
                        )
                    )
             ),
             column(6,
                    div(class = "card mb-4 border-0 shadow-sm",
                        div(class = "card-header",
                            "Retraction Years"
                        ),
                        div(class = "card-body",
                            plotlyOutput("retraction_year_plot", height = "400px")
                        ),
                        div(class = "card-footer text-muted small",
                            textOutput("retraction_outliers_text")
                        )
                    )
             )
           ),
           fluidRow(
             column(12,
                    div(class = "card mb-4 border-0 shadow-sm",
                        div(class = "card-header",
                            "Publication vs Retraction Timeline"
                        ),
                        div(class = "card-body",
                            fluidRow(
                              column(6, plotlyOutput("slope_plot", height = "500px")),
                              column(6, 
                                     plotlyOutput("diff_dist_plot", height = "500px"),
                                     div(class = "text-muted small text-center mt-2", textOutput("diff_outliers_text"))
                              )
                            )
                        ),
                        div(class = "card-footer",
                            uiOutput("diff_stats")
                        )
                    )
             )
           ),
           fluidRow(
             column(6,
                    div(class = "card mb-4 border-0 shadow-sm",
                        div(class = "card-header",
                            "Retractions by Subject"
                        ),
                        div(class = "card-body p-0",
                            tableOutput("top_subjects_table")
                        )
                    )
             ),
             column(6,
                    div(class = "card mb-4 border-0 shadow-sm",
                        div(class = "card-header",
                            "Top 10 Retracted Publishers"
                        ),
                        div(class = "card-body p-0",
                            tableOutput("top_publishers_table")
                        ),
                        div(class = "card-footer text-muted small",
                            "* Note: Publishers were grouped by parent companies where possible. 'Springer Nature' includes Springer, Nature, BMC, Palgrave. 'Elsevier' includes Cell Press, Lancet. 'Hindawi' has been separated from 'Wiley' to show its specific impact. 'Taylor & Francis' includes Routledge, Dove Medical Press."
                        )
                    )
             )
           ),
           fluidRow(
             column(12,
                    div(class = "card mb-4 border-0 shadow-sm",
                        div(class = "card-header",
                            "Top 10 Reasons for Retraction"
                        ),
                        div(class = "card-body p-0",
                            tableOutput("top_reasons_table")
                        ),
                        div(class = "card-footer text-muted small",
                            textOutput("reasons_exclusion_text")
                        )
                    )
             )
           )
  ),
  
  tabPanel("Geographical Analysis",
           fluidRow(
             column(8,
                    div(class = "card mb-4 border-0 shadow-sm",
                        div(class = "card-header",
                            "World Map of Retractions"
                        ),
                        div(class = "card-body",
                            plotlyOutput("world_map_plot", height = "500px")
                        ),
                        div(class = "card-footer text-muted small",
                            "* Note: A logarithmic scale (log10) is used because retraction counts vary massively (from 1 to tens of thousands). This scale allows the map to clearly display differences among countries with low counts without being visually overshadowed by extreme outliers like the USA or China."
                        )
                    )
             ),
             column(4,
                    div(class = "card mb-4 border-0 shadow-sm",
                        div(class = "card-header",
                            "Top 15 Countries"
                        ),
                        div(class = "card-body p-0",
                            tableOutput("top_countries_table")
                        )
                    )
             )
           )
  ),
  
  tabPanel("RWD Leaderboard",
           fluidRow(
             column(12,
                    div(class = "card mb-4 border-0 shadow-sm",
                        div(class = "card-header",
                            "Top 35 Most Retracted Authors"
                        ),
                        div(class = "card-body p-0",
                            tableOutput("top_authors_table")
                        ),
                        div(class = "card-footer text-muted small",
                            "* Note: Authors are identified exactly as they appear in the dataset. Variations in spelling or initials might result in separate entries for the same person."
                        )
                    )
             )
           )
  )
)

server <- function(input, output, session) {
  
  # Reactive values to store processed data
  retraction_data <- reactiveVal()
  country_data <- reactiveVal()
  subject_data <- reactiveVal()
  publisher_data <- reactiveVal()
  reason_data <- reactiveVal()
  author_data <- reactiveVal()
  
  # Load and process data when app starts
  observeEvent(TRUE, {
    # Show loading notification
    showNotification("Loading data, please wait...", duration = NULL, id = "load_msg", type = "message")
    
    # Read the dataset
    df <- read.csv("retraction_watch.csv", stringsAsFactors = FALSE)
    
    # Extract 4-digit years (e.g. 19xx or 20xx) from the date strings using regular expressions
    df <- df %>%
      mutate(
        PubYear = as.numeric(str_extract(OriginalPaperDate, "\\b(19|20)\\d{2}\\b")),
        RetYear = as.numeric(str_extract(RetractionDate, "\\b(19|20)\\d{2}\\b")),
        DiffYear = RetYear - PubYear
      )
    
    # Process country data
    country_list <- strsplit(as.character(df$Country), ";")
    country_df <- data.frame(Country = trimws(unlist(country_list)))
    
    country_counts <- country_df %>%
      filter(Country != "", !is.na(Country)) %>%
      count(Country, name = "Retractions") %>%
      arrange(desc(Retractions)) %>%
      mutate(MapCountry = case_when(
        Country == "United States" ~ "USA",
        Country == "United Kingdom" ~ "UK",
        TRUE ~ Country
      ))
    # Process subjects data
    subject_list <- strsplit(as.character(df$Subject), ";")
    subject_df <- data.frame(Subject = trimws(unlist(subject_list)))
    
    subject_counts <- subject_df %>%
      filter(Subject != "", !is.na(Subject)) %>%
      mutate(Acronym = str_extract(Subject, "^\\([A-Z/]+\\)")) %>%
      filter(!is.na(Acronym)) %>%
      mutate(Category = case_when(
        Acronym == "(B/T)" ~ "Business & Technology",
        Acronym == "(BLS)" ~ "Biological Sciences",
        Acronym == "(ENV)" ~ "Environmental Sciences",
        Acronym == "(HSC)" ~ "Health Sciences",
        Acronym == "(HUM)" ~ "Humanities",
        Acronym == "(PHY)" ~ "Physical Sciences",
        Acronym == "(SOC)" ~ "Social Sciences",
        TRUE ~ Acronym
      )) %>%
      count(Category, name = "Retractions") %>%
      arrange(desc(Retractions))
      
    # Process publishers data
    publisher_counts <- df %>%
      filter(Publisher != "", !is.na(Publisher)) %>%
      mutate(PublisherGroup = case_when(
        str_detect(Publisher, "(?i)Springer|Nature|BioMed Central|BMC|Palgrave") ~ "Springer Nature",
        str_detect(Publisher, "(?i)Elsevier|Cell Press|Lancet|Pergamon") ~ "Elsevier",
        str_detect(Publisher, "(?i)Hindawi") ~ "Hindawi (Wiley)",
        str_detect(Publisher, "(?i)Wiley") ~ "Wiley (excl. Hindawi)",
        str_detect(Publisher, "(?i)Taylor & Francis|Taylor and Francis|Routledge|Dove Medical") ~ "Taylor & Francis",
        str_detect(Publisher, "(?i)SAGE") ~ "SAGE Publications",
        str_detect(Publisher, "(?i)IEEE") ~ "IEEE",
        str_detect(Publisher, "(?i)MDPI") ~ "MDPI",
        str_detect(Publisher, "(?i)Frontiers") ~ "Frontiers",
        str_detect(Publisher, "(?i)Public Library of Science|PLOS") ~ "PLOS",
        str_detect(Publisher, "(?i)Oxford University Press|OUP") ~ "Oxford University Press",
        str_detect(Publisher, "(?i)Cambridge University Press|CUP") ~ "Cambridge University Press",
        TRUE ~ Publisher
      )) %>%
      count(PublisherGroup, name = "Retractions") %>%
      arrange(desc(Retractions))
      
    # Process reasons data
    reason_list <- strsplit(as.character(df$Reason), ";")
    reason_df <- data.frame(Reason = trimws(unlist(reason_list)))
    
    reason_counts <- reason_df %>%
      filter(Reason != "", !is.na(Reason)) %>%
      mutate(Reason = str_replace_all(Reason, "^\\+", "")) %>%
      mutate(Reason = trimws(Reason)) %>%
      count(Reason, name = "Retractions") %>%
      arrange(desc(Retractions))
      
    # Process author data
    author_list <- strsplit(as.character(df$Author), ";")
    author_df <- data.frame(Author = trimws(unlist(author_list)))
    
    author_counts <- author_df %>%
      filter(Author != "", !is.na(Author)) %>%
      count(Author, name = "Retractions") %>%
      arrange(desc(Retractions))
      
    retraction_data(df)
    subject_data(subject_counts)
    publisher_data(publisher_counts)
    country_data(country_counts)
    reason_data(reason_counts)
    author_data(author_counts)
    removeNotification(id = "load_msg")
  }, once = TRUE)
  
  output$kpi_boxes <- renderUI({
    req(retraction_data())
    df <- retraction_data()
    
    total <- nrow(df)
    
    retraction_count <- sum(str_detect(df$RetractionNature, "(?i)Retraction"), na.rm = TRUE)
    eoc_count <- sum(str_detect(df$RetractionNature, "(?i)Expression of concern"), na.rm = TRUE)
    correction_count <- sum(str_detect(df$RetractionNature, "(?i)Correction|Update"), na.rm = TRUE)
    
    cards <- div(class = "card mb-4 border-0 shadow-sm",
                 div(class = "card-body text-center p-4", style = "border-top: 4px solid #333333;",
                     div(class = "row text-center d-flex align-items-center justify-content-center",
                         div(class = "col-3", style = "border-right: 2px solid #dddddd;",
                             div(class = "kpi-title mb-2", style = "font-size: 0.9rem;", "TOTAL RWD ENTRIES"),
                             div(class = "kpi-value", style = "font-size: 3rem; margin-bottom: 0;", format(total, big.mark = ","))
                         ),
                         div(class = "col-3", style = "border-right: 1px solid #eaeaea;",
                             div(class = "kpi-title mb-2", "RETRACTIONS"),
                             div(class = "kpi-value", style = "font-size: 2rem; margin-bottom: 0;", format(retraction_count, big.mark = ","))
                         ),
                         div(class = "col-3", style = "border-right: 1px solid #eaeaea;",
                             div(class = "kpi-title mb-2", "EXPRESSIONS OF CONCERN"),
                             div(class = "kpi-value", style = "font-size: 2rem; margin-bottom: 0;", format(eoc_count, big.mark = ","))
                         ),
                         div(class = "col-3",
                             div(class = "kpi-title mb-2", "CORRECTIONS"),
                             div(class = "kpi-value", style = "font-size: 2rem; margin-bottom: 0;", format(correction_count, big.mark = ","))
                         )
                     )
                 )
    )
    cards
  })
  
  output$pub_outliers_text <- renderText({
    req(retraction_data())
    outliers <- sum(retraction_data()$PubYear < 1980, na.rm = TRUE)
    if (outliers > 0) {
      paste("* Note:", outliers, "older papers (before 1980) were excluded to improve visualization.")
    } else {
      ""
    }
  })
  
  output$pub_year_plot <- renderPlotly({
    req(retraction_data())
    df <- retraction_data() %>% filter(!is.na(PubYear), PubYear >= 1980)
    
    p <- ggplot(df, aes(x = PubYear)) +
      geom_histogram(binwidth = 1, fill = "#313695", color = "white", alpha = 0.9) +
      theme_minimal(base_size = 14) +
      scale_x_continuous(breaks = seq(1980, 2030, by = 5)) +
      labs(
        x = "Year of Publication",
        y = "Count of Papers"
      ) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(face = "bold", color = "#333333"),
        axis.text = element_text(color = "#555555")
      )
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$retraction_outliers_text <- renderText({
    req(retraction_data())
    outliers <- sum(retraction_data()$RetYear < 1980, na.rm = TRUE)
    if (outliers > 0) {
      paste("* Note:", outliers, "older retractions (before 1980) were excluded to improve visualization.")
    } else {
      ""
    }
  })
  
  output$retraction_year_plot <- renderPlotly({
    req(retraction_data())
    df <- retraction_data() %>% filter(!is.na(RetYear), RetYear >= 1980)
    
    p <- ggplot(df, aes(x = RetYear)) +
      geom_histogram(binwidth = 1, fill = "#a50026", color = "white", alpha = 0.9) +
      theme_minimal(base_size = 14) +
      scale_x_continuous(breaks = seq(1980, 2030, by = 5)) +
      labs(
        x = "Year of Retraction",
        y = "Count of Retractions"
      ) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(face = "bold", color = "#333333"),
        axis.text = element_text(color = "#555555")
      )
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$slope_plot <- renderPlotly({
    req(retraction_data())
    # Filtrar fechas válidas
    df <- retraction_data() %>% 
      filter(!is.na(PubYear), !is.na(RetYear), PubYear >= 1980, RetYear >= 1980)
    
    if(nrow(df) > 1500) {
      set.seed(42)
      df <- sample_n(df, 1500)
    }
    
    p <- ggplot(df) +
      geom_segment(aes(x = 1, xend = 2, y = PubYear, yend = RetYear, color = DiffYear), 
                   alpha = 0.6, linewidth = 0.6) +
      scale_color_gradientn(colors = c("#313695", "#74add1", "#ffffbf", "#f46d43", "#a50026"), name = "Lag (Years)") +
      geom_point(aes(x = 1, y = PubYear, text = paste("Pub:", PubYear)), color = "#555555", alpha = 0.4, size = 1.5) +
      geom_point(aes(x = 2, y = RetYear, text = paste("Ret:", RetYear)), color = "#222222", alpha = 0.4, size = 1.5) +
      scale_x_continuous(breaks = c(1, 2), labels = c("Publication", "Retraction"), limits = c(0.8, 2.2)) +
      theme_minimal(base_size = 14) +
      labs(x = NULL, y = "Year") +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(face = "bold", size = 14, color = "#222222"),
        axis.text.y = element_text(color = "#555555")
      )
    ggplotly(p, tooltip = c("text", "color"))
  })
  
  output$diff_outliers_text <- renderText({
    req(retraction_data())
    outliers <- sum(retraction_data()$DiffYear > 30, na.rm = TRUE)
    if (outliers > 0) {
      paste("* Note:", outliers, "outliers (lag > 30 years) were excluded to improve visualization.")
    } else {
      ""
    }
  })
  
  output$diff_dist_plot <- renderPlotly({
    req(retraction_data())
    df <- retraction_data() %>% filter(!is.na(DiffYear), DiffYear >= 0, DiffYear <= 30)
    
    p <- ggplot(df, aes(x = DiffYear)) +
      geom_histogram(binwidth = 1, fill = "#74add1", color = "white", alpha = 0.9) +
      theme_minimal(base_size = 14) +
      labs(x = "Lag (Years)", y = "Count") +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_text(face = "bold", color = "#333333"),
        axis.text = element_text(color = "#555555")
      )
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$diff_stats <- renderUI({
    req(retraction_data())
    # Filter out cases with negative diff or missing
    df <- retraction_data() %>% filter(!is.na(DiffYear), DiffYear >= 0)
    
    mean_val <- round(mean(df$DiffYear, na.rm = TRUE), 1)
    median_val <- round(median(df$DiffYear, na.rm = TRUE), 1)
    q1_val <- round(quantile(df$DiffYear, 0.25, na.rm = TRUE), 1)
    q3_val <- round(quantile(df$DiffYear, 0.75, na.rm = TRUE), 1)
    
    tags$div(
      class = "row w-100 text-center py-2",
      tags$div(class = "col-3",
        tags$h2(class = "mb-0", style = "color: #222222; font-weight: 700; font-family: 'Georgia', serif;", paste0(mean_val, " yrs")),
        tags$div(style = "color: #555555; font-family: 'Inter', sans-serif; font-size: 0.75rem; text-transform: uppercase; font-weight: 700; letter-spacing: 1px; margin-top: 5px;", "Mean Lag between Publication and Retraction")
      ),
      tags$div(class = "col-3",
        tags$h2(class = "mb-0", style = "color: #222222; font-weight: 700; font-family: 'Georgia', serif;", paste0(median_val, " yrs")),
        tags$div(style = "color: #555555; font-family: 'Inter', sans-serif; font-size: 0.75rem; text-transform: uppercase; font-weight: 700; letter-spacing: 1px; margin-top: 5px;", "Median Lag between Publication and Retraction")
      ),
      tags$div(class = "col-3",
        tags$h2(class = "mb-0", style = "color: #222222; font-weight: 700; font-family: 'Georgia', serif;", paste0(q1_val, " yrs")),
        tags$div(style = "color: #555555; font-family: 'Inter', sans-serif; font-size: 0.75rem; text-transform: uppercase; font-weight: 700; letter-spacing: 1px; margin-top: 5px;", "Q1 (25th Pct)")
      ),
      tags$div(class = "col-3",
        tags$h2(class = "mb-0", style = "color: #222222; font-weight: 700; font-family: 'Georgia', serif;", paste0(q3_val, " yrs")),
        tags$div(style = "color: #555555; font-family: 'Inter', sans-serif; font-size: 0.75rem; text-transform: uppercase; font-weight: 700; letter-spacing: 1px; margin-top: 5px;", "Q3 (75th Pct)")
      )
    )
  })
  
  output$world_map_plot <- renderPlotly({
    req(country_data())
    
    world <- map_data("world")
    
    map_data_joined <- world %>%
      left_join(country_data(), by = c("region" = "MapCountry")) %>%
      arrange(group, order)
    
    p <- ggplot(map_data_joined, aes(x = long, y = lat, group = group, fill = Retractions, text = paste("Country:", region, "<br>Retractions:", Retractions))) +
      geom_polygon(color = "white", linewidth = 0.2) +
      scale_fill_gradientn(
        colors = c("#313695", "#74add1", "#ffffbf", "#f46d43", "#a50026"),
        na.value = "#e0e0e0", 
        trans = "log10", 
        name = "Retractions (log10)",
        breaks = c(1, 10, 100, 1000, 10000)
      ) +
      theme_void(base_size = 14) +
      theme(
        legend.position = "right",
        plot.margin = margin(10, 10, 10, 10)
      ) +
      coord_fixed(1.3)
      
    ggplotly(p, tooltip = "text") %>%
      style(colorbar = list(len = 0.5, thickness = 15, title = "Retractions<br>(log10)", outlinewidth = 0)) %>%
      layout(hoverlabel = list(bgcolor = "white", font = list(family = "Inter")))
  })
  
  output$top_countries_table <- renderTable({
    req(country_data())
    country_data() %>%
      select(Country, Retractions) %>%
      head(15) %>%
      mutate(Retractions = as.integer(Retractions))
  }, width = "100%", striped = TRUE, hover = TRUE, bordered = TRUE, align = "lr")
  
  output$top_subjects_table <- renderTable({
    req(subject_data())
    subject_data() %>%
      mutate(Retractions = as.integer(Retractions))
  }, width = "100%", striped = TRUE, hover = TRUE, bordered = TRUE, align = "lr")
  
  output$top_publishers_table <- renderTable({
    req(publisher_data())
    publisher_data() %>%
      head(10) %>%
      mutate(Retractions = as.integer(Retractions)) %>%
      rename(Publisher = PublisherGroup)
  }, width = "100%", striped = TRUE, hover = TRUE, bordered = TRUE, align = "lr")
  
  output$reasons_exclusion_text <- renderText({
    req(reason_data())
    journal_inv <- reason_data() %>% filter(str_detect(Reason, "(?i)Investigation by Journal/Publisher")) %>% pull(Retractions) %>% sum(na.rm=TRUE)
    third_party_inv <- reason_data() %>% filter(str_detect(Reason, "(?i)Investigation by Third Party")) %>% pull(Retractions) %>% sum(na.rm=TRUE)
    
    paste("* Note: A single paper can have multiple reasons for retraction.", 
          "We excluded 'Investigation by Journal/Publisher' (", format(journal_inv, big.mark=","), "cases) and", 
          "'Investigation by Third Party' (", format(third_party_inv, big.mark=","), "cases) as they indicate the initiator rather than the underlying reason.")
  })
  
  output$top_reasons_table <- renderTable({
    req(reason_data())
    reason_data() %>%
      filter(!str_detect(Reason, "(?i)Investigation by Journal/Publisher|Investigation by Third Party")) %>%
      head(10) %>%
      mutate(Retractions = as.integer(Retractions)) %>%
      rename(`Reason for Retraction` = Reason)
  }, width = "100%", striped = TRUE, hover = TRUE, bordered = TRUE, align = "lr")
  
  output$top_authors_table <- renderTable({
    req(author_data())
    author_data() %>%
      head(35) %>%
      mutate(Retractions = as.integer(Retractions)) %>%
      rename(`Author` = Author)
  }, width = "100%", striped = TRUE, hover = TRUE, bordered = TRUE, align = "lr")
}

shinyApp(ui, server)

library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)
library(bslib)
library(maps)
library(plotly)
library(htmlwidgets)


file_path <- "retraction_watch.csv"
url <- "https://gitlab.com/crossref/retraction-watch-data/-/raw/main/retraction_watch.csv"
current_year <- format(Sys.Date(), "%Y")
normalization_start_year <- 1990L
normalization_end_year <- as.integer(current_year)

cache_path <- "retraction_watch_processed.rds"
cache_version <- 14L
refresh_script_path <- file.path("scripts", "refresh_rwd_cache.R")
refresh_status_path <- "rwd_refresh_status.rds"
refresh_log_path <- "rwd_refresh.log"
refresh_interval_hours <- 7 * 24

reason_category_definitions <- list(
  "Data & image integrity concerns" = c(
    "Duplication of/in Image",
    "Manipulation of Images",
    "Falsification/Fabrication of Image",
    "Concerns/Issues about Image",
    "Plagiarism of Image",
    "Unreliable Image",
    "Falsification/Fabrication of Data",
    "Falsification/Fabrication of Results",
    "Manipulation of Data",
    "Manipulation of Results"
  ),
  "Reliability / reproducibility concerns" = c(
    "Unreliable Results and/or Conclusions",
    "Unreliable Data",
    "Concerns/Issues about Data",
    "Concerns/Issues about Results and/or Conclusions",
    "Results Not Reproducible",
    "Original Data and/or Images not Provided and/or not Available"
  ),
  "Plagiarism / duplicate publication" = c(
    "Plagiarism of/in Article",
    "Plagiarism of Text",
    "Plagiarism of Data",
    "Duplication of/in Article",
    "Duplication of Text",
    "Euphemisms for Plagiarism",
    "Euphemisms for Duplication",
    "Duplication of Data"
  ),
  "Reported errors" = c(
    "Error in Data",
    "Error in Analyses",
    "Error in Results and/or Conclusions",
    "Error in Methods",
    "Error in Image",
    "Error in Text",
    "Error in Materials",
    "Error in Cell Lines/Tissues"
  ),
  "Referencing / attribution / copyright concerns" = c(
    "Concerns/Issues about Referencing/Attributions",
    "Copyright Claims",
    "Cites Retracted Work",
    "Taken from Dissertation/Thesis",
    "Taken via Peer Review",
    "Taken via Translation"
  ),
  "Research ethics / consent / oversight" = c(
    "Lack of IRB/IACUC Approval and/or Compliance",
    "Informed/Patient Consent - None/Withdrawn",
    "Ethical Violations by Author",
    "Concerns/Issues about Human Subject Welfare",
    "Concerns/Issues about Animal Welfare",
    "Conflict of Interest",
    "Bias Issues or Lack of Balance"
  ),
  "Authorship / affiliation integrity" = c(
    "Concerns/Issues about Authorship/Affiliation",
    "False/Forged Authorship",
    "False/Forged Affiliation"
  ),
  "Peer-review integrity concerns" = c(
    "Compromised Peer Review",
    "Concerns/Issues about Peer Review"
  ),
  "Paper mill" = c(
    "Paper Mill"
  ),
  "Computer-generated content" = c(
    "Computer-Aided Content or Computer-Generated Content"
  ),
  "Formal investigation / misconduct finding" = c(
    "Investigation by Company/Institution",
    "Investigation by ORI",
    "Investigation by Third Party",
    "Misconduct - Official Investigation(s) and/or Finding(s)",
    "Misconduct by Author"
  ),
  "Editorial / publisher process or error" = c(
    "Rogue Editor",
    "Error by Journal/Publisher",
    "Duplication of Content through Error by Journal/Publisher",
    "Retract and Replace",
    "Withdrawn to Publish in Different Journal",
    "Miscommunication with/by Journal/Publisher"
  )
)

reason_category_types <- c(
  "Data & image integrity concerns" = "Substantive category",
  "Reliability / reproducibility concerns" = "Substantive category",
  "Plagiarism / duplicate publication" = "Substantive category",
  "Reported errors" = "Substantive category",
  "Referencing / attribution / copyright concerns" = "Substantive category",
  "Research ethics / consent / oversight" = "Substantive category",
  "Authorship / affiliation integrity" = "Substantive category",
  "Peer-review integrity concerns" = "Substantive category",
  "Paper mill" = "Substantive category",
  "Computer-generated content" = "Substantive category",
  "Formal investigation / misconduct finding" = "Pathway / context",
  "Editorial / publisher process or error" = "Pathway / context"
)

reason_category_descriptions <- c(
  "Data & image integrity concerns" = "Flags involving data or images, including duplication, manipulation, fabrication, plagiarism or unreliability.",
  "Reliability / reproducibility concerns" = "Results, conclusions, data or source materials that are unreliable, unavailable or not reproducible, without assuming intent.",
  "Plagiarism / duplicate publication" = "Reuse or duplication of article text, data or content, including euphemistic RWD labels for plagiarism or duplication.",
  "Reported errors" = "Errors attributed to the article's data, analyses, methods, results, images, text, materials or cell lines.",
  "Referencing / attribution / copyright concerns" = "Problems involving citations, attribution, copyright or material taken from another source or review process.",
  "Research ethics / consent / oversight" = "Human- or animal-research oversight, consent, welfare, conflicts of interest and related ethical concerns.",
  "Authorship / affiliation integrity" = "Concerns about who authored the work or the affiliations claimed by its authors.",
  "Peer-review integrity concerns" = "Peer review reported as compromised or otherwise subject to integrity concerns.",
  "Paper mill" = "Paper-mill involvement explicitly recorded by RWD; false or forged authorship and affiliations remain in the separate authorship category.",
  "Computer-generated content" = "Content identified by RWD as computer-aided or computer-generated.",
  "Formal investigation / misconduct finding" = "The procedural route or formal finding behind the retraction, rather than a specific defect in the paper.",
  "Editorial / publisher process or error" = "Publisher- or editor-side processes and errors that provide context for why or how the notice was issued."
)

reason_procedural_labels <- c(
  "Investigation by Journal/Publisher",
  "Notice - Limited or No Information",
  "Date of Article and/or Notice Unknown",
  "Upgrade/Update of Prior Notice(s)",
  "Removed",
  "Notice - Unable to Access via current resources"
)

build_reason_category_map <- function() {
  bind_rows(lapply(
    names(reason_category_definitions),
    function(category) {
      data.frame(
        Category = category,
        ClassificationRole = unname(reason_category_types[[category]]),
        Reason = reason_category_definitions[[category]],
        stringsAsFactors = FALSE
      )
    }
  ))
}

process_retraction_data <- function(path) {
  df <- read.csv(path, stringsAsFactors = FALSE)

  df <- df %>%
    mutate(
      PubYear = as.numeric(str_extract(OriginalPaperDate, "\\b(19|20)\\d{2}\\b")),
      RetYear = as.numeric(str_extract(RetractionDate, "\\b(19|20)\\d{2}\\b")),
      DiffYear = RetYear - PubYear,
      OriginalPaperDOINormalized = str_to_lower(str_trim(as.character(OriginalPaperDOI))),
      OriginalPaperDOINormalized = str_remove(OriginalPaperDOINormalized, "^https?://(dx\\.)?doi\\.org/"),
      OriginalPaperDOINormalized = str_remove(OriginalPaperDOINormalized, "^doi:"),
      OriginalPaperDOINormalized = str_remove(OriginalPaperDOINormalized, "[\\.,;\\)]+$"),
      PaperKey = if_else(
        !is.na(OriginalPaperDOINormalized) & str_detect(OriginalPaperDOINormalized, "^10\\..+/"),
        paste0("doi:", OriginalPaperDOINormalized),
        paste0("record:", as.character(Record.ID))
      )
    )

  country_list <- strsplit(as.character(df$Country), ";")
  country_df <- data.frame(
    RowID = rep(seq_len(nrow(df)), lengths(country_list)),
    Country = trimws(unlist(country_list)),
    stringsAsFactors = FALSE
  ) %>%
    left_join(
      df %>%
        mutate(RowID = row_number()) %>%
        select(RowID, PaperKey, PubYear, RetractionNature),
      by = "RowID"
    )

  country_counts <- country_df %>%
    filter(
      Country != "",
      !is.na(Country),
      !str_to_lower(Country) %in% c("unknown", "n/a", "not available"),
      str_detect(RetractionNature, "(?i)Retraction"),
      !is.na(PubYear),
      PubYear >= normalization_start_year,
      PubYear <= normalization_end_year
    ) %>%
    distinct(Country, PaperKey) %>%
    count(Country, name = "Retractions") %>%
    arrange(desc(Retractions)) %>%
    mutate(MapCountry = case_when(
      Country == "United States" ~ "USA",
      Country == "United Kingdom" ~ "UK",
      TRUE ~ Country
    ))

  subject_list <- strsplit(as.character(df$Subject), ";")
  subject_df <- data.frame(
    RowID = rep(seq_len(nrow(df)), sapply(subject_list, length)),
    Subject = trimws(unlist(subject_list)),
    stringsAsFactors = FALSE
  ) %>%
    left_join(
      df %>%
        mutate(RowID = row_number()) %>%
        select(RowID, PaperKey, RetractionNature),
      by = "RowID"
  )

  subject_paper_categories <- subject_df %>%
    filter(
      Subject != "",
      !is.na(Subject),
      str_detect(RetractionNature, "(?i)Retraction")
    ) %>%
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
    distinct(PaperKey, Category)

  subject_counts <- subject_paper_categories %>%
    count(Category, name = "Retractions") %>%
    arrange(desc(Retractions))

  publisher_counts <- df %>%
    filter(
      Publisher != "",
      !is.na(Publisher),
      str_detect(RetractionNature, "(?i)Retraction"),
      !is.na(PubYear),
      PubYear >= normalization_start_year,
      PubYear <= normalization_end_year
    ) %>%
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
    distinct(PublisherGroup, PaperKey) %>%
    count(PublisherGroup, name = "Retractions") %>%
    arrange(desc(Retractions))

  retraction_reason_rows <- df %>%
    filter(str_detect(RetractionNature, "(?i)Retraction")) %>%
    select(PaperKey, Reason)

  reason_list <- strsplit(as.character(retraction_reason_rows$Reason), ";", fixed = TRUE)
  reason_paper_labels <- data.frame(
    PaperKey = rep(retraction_reason_rows$PaperKey, lengths(reason_list)),
    Reason = trimws(unlist(reason_list)),
    stringsAsFactors = FALSE
  ) %>%
    filter(Reason != "", !is.na(Reason)) %>%
    mutate(Reason = trimws(str_remove(Reason, "^\\+"))) %>%
    distinct(PaperKey, Reason)

  reason_counts <- reason_paper_labels %>%
    count(Reason, name = "UniqueRetractedPapers") %>%
    arrange(desc(UniqueRetractedPapers), Reason)

  reason_category_map <- build_reason_category_map()

  classified_reason_pairs <- reason_paper_labels %>%
    inner_join(reason_category_map, by = "Reason") %>%
    distinct(PaperKey, Category, ClassificationRole)

  total_unique_retracted_papers <- n_distinct(retraction_reason_rows$PaperKey)
  category_counts <- classified_reason_pairs %>%
    count(Category, name = "UniqueRetractedPapers")

  reason_classification_data <- data.frame(
    Category = names(reason_category_definitions),
    CategoryOrder = seq_along(reason_category_definitions),
    ClassificationRole = unname(reason_category_types[names(reason_category_definitions)]),
    stringsAsFactors = FALSE
  ) %>%
    left_join(category_counts, by = "Category") %>%
    mutate(
      UniqueRetractedPapers = coalesce(UniqueRetractedPapers, 0L),
      SharePct = 100 * UniqueRetractedPapers / total_unique_retracted_papers
    ) %>%
    arrange(CategoryOrder)

  substantive_categories <- names(reason_category_types)[
    reason_category_types == "Substantive category"
  ]
  pathway_categories <- names(reason_category_types)[
    reason_category_types == "Pathway / context"
  ]
  substantive_reason_pairs <- classified_reason_pairs %>%
    filter(Category %in% substantive_categories)
  pathway_reason_pairs <- classified_reason_pairs %>%
    filter(Category %in% pathway_categories)
  substantive_papers <- substantive_reason_pairs %>% distinct(PaperKey)
  pathway_papers <- pathway_reason_pairs %>% distinct(PaperKey)

  mapped_substantive_labels <- unique(unlist(
    reason_category_definitions[substantive_categories],
    use.names = FALSE
  ))
  mapped_pathway_labels <- unique(unlist(
    reason_category_definitions[pathway_categories],
    use.names = FALSE
  ))
  unclassified_label_papers <- reason_paper_labels %>%
    filter(
      !Reason %in% mapped_substantive_labels,
      !Reason %in% mapped_pathway_labels,
      !Reason %in% reason_procedural_labels
    ) %>%
    distinct(PaperKey)

  formal_category <- "Formal investigation / misconduct finding"
  editorial_category <- "Editorial / publisher process or error"
  formal_papers <- classified_reason_pairs %>%
    filter(Category == formal_category) %>%
    distinct(PaperKey)
  formal_with_substantive_papers <- semi_join(
    formal_papers,
    substantive_papers,
    by = "PaperKey"
  )
  editorial_papers <- classified_reason_pairs %>%
    filter(Category == editorial_category) %>%
    distinct(PaperKey)
  editorial_with_substantive_papers <- semi_join(
    editorial_papers,
    substantive_papers,
    by = "PaperKey"
  )

  unclassified_without_substantive_category <- anti_join(
    unclassified_label_papers,
    substantive_papers,
    by = "PaperKey"
  )

  reason_classification_summary <- data.frame(
    TotalUniqueRetractedPapers = total_unique_retracted_papers,
    PapersInSubstantiveCategories = nrow(substantive_papers),
    SubstantiveCoveragePct = 100 * nrow(substantive_papers) / total_unique_retracted_papers,
    PapersInPathwayCategories = nrow(pathway_papers),
    PathwayCoveragePct = 100 * nrow(pathway_papers) / total_unique_retracted_papers,
    PapersWithUnclassifiedLabels = nrow(unclassified_label_papers),
    UnclassifiedLabelPct = 100 * nrow(unclassified_label_papers) / total_unique_retracted_papers,
    UnclassifiedWithoutSubstantiveCategory = nrow(unclassified_without_substantive_category),
    UnclassifiedWithoutSubstantivePct = 100 * nrow(unclassified_without_substantive_category) / total_unique_retracted_papers,
    FormalInvestigationPapers = nrow(formal_papers),
    FormalWithSubstantivePapers = nrow(formal_with_substantive_papers),
    FormalWithSubstantivePct = if (nrow(formal_papers) > 0) {
      100 * nrow(formal_with_substantive_papers) / nrow(formal_papers)
    } else {
      NA_real_
    },
    EditorialPathwayPapers = nrow(editorial_papers),
    EditorialWithSubstantivePapers = nrow(editorial_with_substantive_papers),
    EditorialWithSubstantivePct = if (nrow(editorial_papers) > 0) {
      100 * nrow(editorial_with_substantive_papers) / nrow(editorial_papers)
    } else {
      NA_real_
    }
  )

  author_list <- strsplit(as.character(df$Author), ";")
  author_df <- data.frame(
    RowID = rep(seq_len(nrow(df)), lengths(author_list)),
    Author = trimws(unlist(author_list)),
    stringsAsFactors = FALSE
  ) %>%
    left_join(
      df %>%
        mutate(RowID = row_number()) %>%
        select(RowID, PaperKey, PubYear, RetYear, RetractionNature, Publisher, Reason),
      by = "RowID"
    ) %>%
    filter(
      Author != "",
      !is.na(Author),
      !str_to_lower(Author) %in% c("unknown", "anonymous", "n/a", "not available"),
      str_detect(RetractionNature, "(?i)Retraction")
    )

  min_or_na <- function(values) {
    if (all(is.na(values))) NA_real_ else min(values, na.rm = TRUE)
  }

  median_lag_or_na <- function(values) {
    values <- values[!is.na(values) & values >= 0]
    if (length(values) == 0) NA_real_ else median(values)
  }

  collapse_values <- function(values) {
    values <- sort(unique(trimws(values[!is.na(values) & trimws(values) != ""])))
    paste(values, collapse = ";")
  }

  first_nonempty <- function(values) {
    values <- unique(trimws(as.character(values[!is.na(values) & trimws(values) != ""])))
    if (length(values) == 0) NA_character_ else values[[which.max(nchar(values))]]
  }

  ieee_publisher_name <- "IEEE: Institute of Electrical and Electronics Engineers"

  lag_paper_data <- df %>%
    filter(str_detect(RetractionNature, "(?i)Retraction")) %>%
    group_by(PaperKey) %>%
    summarise(
      IsIEEESpike = any(
        Publisher == ieee_publisher_name &
          (PubYear %in% 2010:2011 | RetYear %in% 2010:2011),
        na.rm = TRUE
      ),
      PubYear = min_or_na(PubYear),
      RetYear = min_or_na(RetYear),
      .groups = "drop"
    ) %>%
    mutate(DiffYear = RetYear - PubYear) %>%
    filter(!is.na(DiffYear), DiffYear >= 0) %>%
    arrange(DiffYear, PaperKey)

  lag_breakdown_data <- bind_rows(
    lag_paper_data %>%
      inner_join(subject_paper_categories, by = "PaperKey") %>%
      transmute(
        PaperKey,
        Breakdown = "Subject",
        Group = Category,
        PubYear,
        RetYear,
        DiffYear
      ),
    lag_paper_data %>%
      inner_join(
        substantive_reason_pairs %>% select(PaperKey, Category),
        by = "PaperKey"
      ) %>%
      transmute(
        PaperKey,
        Breakdown = "Retraction reason category",
        Group = Category,
        PubYear,
        RetYear,
        DiffYear
      )
  ) %>%
    distinct(PaperKey, Breakdown, Group, .keep_all = TRUE) %>%
    arrange(Breakdown, Group, DiffYear, PaperKey)

  paper_metadata <- df %>%
    filter(str_detect(RetractionNature, "(?i)Retraction")) %>%
    group_by(PaperKey) %>%
    summarise(
      Title = first_nonempty(Title),
      Authors = first_nonempty(Author),
      Journal = first_nonempty(Journal),
      PublicationYear = min_or_na(PubYear),
      DOI = first_nonempty(OriginalPaperDOINormalized),
      .groups = "drop"
    )

  author_unique_counts <- author_df %>%
    distinct(Author, PaperKey) %>%
    count(Author, name = "UniqueRetractedPapers")

  author_record_counts <- author_df %>%
    distinct(Author, RowID) %>%
    count(Author, name = "RetractionRecords")

  top_author_names <- author_unique_counts %>%
    left_join(author_record_counts, by = "Author") %>%
    arrange(desc(UniqueRetractedPapers), desc(RetractionRecords), Author) %>%
    head(35) %>%
    pull(Author)

  top_author_papers <- author_df %>%
    filter(Author %in% top_author_names) %>%
    group_by(Author, PaperKey) %>%
    summarise(
      RetractionRecords = n_distinct(RowID),
      PubYear = min_or_na(PubYear),
      RetYear = min_or_na(RetYear),
      Publisher = collapse_values(Publisher),
      Reason = collapse_values(Reason),
      .groups = "drop"
    ) %>%
    mutate(DiffYear = RetYear - PubYear)

  author_counts <- top_author_papers %>%
    group_by(Author) %>%
    summarise(
      UniqueRetractedPapers = n(),
      RetractionRecords = sum(RetractionRecords),
      FirstRetractionYear = min_or_na(RetYear),
      LastRetractionYear = if (all(is.na(RetYear))) NA_real_ else max(RetYear, na.rm = TRUE),
      MedianLagYears = median_lag_or_na(DiffYear),
      .groups = "drop"
    ) %>%
    arrange(desc(UniqueRetractedPapers), desc(RetractionRecords), Author) %>%
    mutate(Rank = row_number()) %>%
    select(Rank, Author, UniqueRetractedPapers, RetractionRecords, FirstRetractionYear, LastRetractionYear, MedianLagYears)

  author_year_counts <- top_author_papers %>%
    filter(!is.na(RetYear)) %>%
    count(Author, RetYear, name = "UniqueRetractedPapers") %>%
    arrange(Author, RetYear)

  author_reason_list <- strsplit(as.character(top_author_papers$Reason), ";", fixed = TRUE)
  author_reason_counts <- data.frame(
    Author = rep(top_author_papers$Author, lengths(author_reason_list)),
    PaperKey = rep(top_author_papers$PaperKey, lengths(author_reason_list)),
    Reason = trimws(unlist(author_reason_list)),
    stringsAsFactors = FALSE
  ) %>%
    filter(Reason != "", !is.na(Reason)) %>%
    mutate(Reason = trimws(str_remove(Reason, "^\\+"))) %>%
    filter(!str_detect(Reason, "(?i)Investigation by Journal/Publisher|Investigation by Third Party")) %>%
    distinct(Author, PaperKey, Reason) %>%
    count(Author, Reason, name = "UniqueRetractedPapers") %>%
    arrange(Author, desc(UniqueRetractedPapers), Reason)

  author_publisher_list <- strsplit(as.character(top_author_papers$Publisher), ";", fixed = TRUE)
  author_publisher_counts <- data.frame(
    Author = rep(top_author_papers$Author, lengths(author_publisher_list)),
    PaperKey = rep(top_author_papers$PaperKey, lengths(author_publisher_list)),
    Publisher = trimws(unlist(author_publisher_list)),
    stringsAsFactors = FALSE
  ) %>%
    filter(Publisher != "", !is.na(Publisher)) %>%
    distinct(Author, PaperKey, Publisher) %>%
    count(Author, Publisher, name = "UniqueRetractedPapers") %>%
    arrange(Author, desc(UniqueRetractedPapers), Publisher)

  ieee_spike_rows <- df %>%
    filter(
      Publisher == ieee_publisher_name,
      PubYear %in% 2010:2011 | RetYear %in% 2010:2011
    )

  ieee_spike_summary <- data.frame(
    Pub2010All = sum(df$PubYear == 2010, na.rm = TRUE),
    Pub2010IEEE = sum(df$PubYear == 2010 & df$Publisher == ieee_publisher_name, na.rm = TRUE),
    Pub2011All = sum(df$PubYear == 2011, na.rm = TRUE),
    Pub2011IEEE = sum(df$PubYear == 2011 & df$Publisher == ieee_publisher_name, na.rm = TRUE),
    Ret2010All = sum(df$RetYear == 2010, na.rm = TRUE),
    Ret2010IEEE = sum(df$RetYear == 2010 & df$Publisher == ieee_publisher_name, na.rm = TRUE),
    Ret2011All = sum(df$RetYear == 2011, na.rm = TRUE),
    Ret2011IEEE = sum(df$RetYear == 2011 & df$Publisher == ieee_publisher_name, na.rm = TRUE),
    IEEESpikeRecords = nrow(ieee_spike_rows),
    EstimatedRetractionDateRecords = sum(
      str_detect(
        ieee_spike_rows$Notes,
        regex("date of retraction unknown, estimated from conference date", ignore_case = TRUE)
      ),
      na.rm = TRUE
    ),
    UnknownDateLabelRecords = sum(
      str_detect(ieee_spike_rows$Reason, fixed("Date of Article and/or Notice Unknown")),
      na.rm = TRUE
    ),
    LimitedInformationRecords = sum(
      str_detect(ieee_spike_rows$Reason, fixed("Notice - Limited or No Information")),
      na.rm = TRUE
    )
  )

  ieee_spike_conferences <- ieee_spike_rows %>%
    filter(!is.na(Journal), trimws(Journal) != "") %>%
    count(Journal, name = "Records") %>%
    arrange(desc(Records), Journal) %>%
    head(6)

  list(
    retraction_data = df %>% select(PubYear, RetYear, DiffYear, RetractionNature, Publisher),
    lag_paper_data = lag_paper_data,
    lag_breakdown_data = lag_breakdown_data,
    country_data = country_counts,
    subject_data = subject_counts,
    publisher_data = publisher_counts,
    reason_data = reason_counts,
    paper_reason_data = reason_paper_labels,
    reason_classification_data = reason_classification_data,
    reason_classification_summary = reason_classification_summary,
    author_data = author_counts,
    author_paper_data = top_author_papers,
    paper_metadata_data = paper_metadata,
    author_year_data = author_year_counts,
    author_reason_data = author_reason_counts,
    author_publisher_data = author_publisher_counts,
    ieee_spike_summary = ieee_spike_summary,
    ieee_spike_conferences = ieee_spike_conferences
  )
}

load_retraction_data <- function(path, cache_file) {
  cached <- NULL

  if (file.exists(cache_file)) {
    cached <- tryCatch(readRDS(cache_file), error = function(e) NULL)
  }

  if (
    is.list(cached) &&
    identical(cached$cache_version, cache_version) &&
    is.list(cached$data) &&
    all(c(
      "retraction_data",
      "lag_paper_data",
      "lag_breakdown_data",
      "paper_reason_data",
      "reason_classification_data",
      "reason_classification_summary",
      "ieee_spike_summary",
      "ieee_spike_conferences"
    ) %in% names(cached$data))
  ) {
    return(list(data = cached$data, cache = cached, loaded_from_cache = TRUE))
  }

  if (!file.exists(path)) {
    stop(
      "No usable processed cache or local Retraction Watch CSV was found. ",
      "Run scripts/refresh_rwd_cache.R once to create them."
    )
  }

  processed <- process_retraction_data(path)
  cache <- list(
    cache_version = cache_version,
    source_md5 = unname(tools::md5sum(path)),
    source_modified_at = format(file.info(path)$mtime, "%Y-%m-%dT%H:%M:%S%z"),
    refreshed_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    data = processed
  )

  tryCatch(
    saveRDS(cache, cache_file, compress = "gzip"),
    error = function(e) message("Processed data cache could not be saved: ", conditionMessage(e))
  )

  list(data = processed, cache = cache, loaded_from_cache = FALSE)
}

loaded_retraction_data <- load_retraction_data(file_path, cache_path)
processed_data <- loaded_retraction_data$data
loaded_cache_metadata <- loaded_retraction_data$cache

cache_display_time <- if (
  !is.null(loaded_cache_metadata$source_modified_at) &&
  nzchar(loaded_cache_metadata$source_modified_at)
) {
  as.POSIXct(loaded_cache_metadata$source_modified_at, format = "%Y-%m-%dT%H:%M:%S%z")
} else {
  file.info(cache_path)$mtime
}
if (is.na(cache_display_time)) cache_display_time <- Sys.time()

format_update_date <- function(value) {
  old_locale <- Sys.getlocale("LC_TIME")
  on.exit(Sys.setlocale("LC_TIME", old_locale), add = TRUE)
  Sys.setlocale("LC_TIME", "C")
  format(value, "%B %d, %Y")
}

update_date <- format_update_date(cache_display_time)

normalization_files <- c(
  countries = "openalex_country_denominators.csv",
  publishers = "openalex_publisher_denominators.csv"
)
normalization_available <- all(file.exists(normalization_files))

if (normalization_available) {
  openalex_country_denominators <- read.csv(
    normalization_files[["countries"]],
    stringsAsFactors = FALSE
  )
  openalex_publisher_denominators <- read.csv(
    normalization_files[["publishers"]],
    stringsAsFactors = FALSE
  )

  normalization_retrieved_date <- openalex_country_denominators$RetrievedDate[[1]]
  normalization_period_end <- openalex_country_denominators$PeriodEnd[[1]]
} else {
  normalization_retrieved_date <- "unavailable"
  normalization_period_end <- paste0(current_year, "-12-31")
}

apply_normalization_data <- function(data) {
  if (normalization_available) {
    data$country_data <- data$country_data %>%
      select(-any_of(c("OpenAlexWorks", "RetractionsPer10000"))) %>%
      left_join(
        openalex_country_denominators %>% select(Country, OpenAlexWorks),
        by = "Country"
      ) %>%
      mutate(RetractionsPer10000 = 10000 * Retractions / OpenAlexWorks)

    data$publisher_data <- data$publisher_data %>%
      select(-any_of(c("OpenAlexWorks", "RetractionsPer10000"))) %>%
      left_join(
        openalex_publisher_denominators %>% select(PublisherGroup, OpenAlexWorks),
        by = "PublisherGroup"
      ) %>%
      mutate(RetractionsPer10000 = 10000 * Retractions / OpenAlexWorks)
  } else {
    data$country_data <- data$country_data %>%
      mutate(OpenAlexWorks = NA_real_, RetractionsPer10000 = NA_real_)
    data$publisher_data <- data$publisher_data %>%
      mutate(OpenAlexWorks = NA_real_, RetractionsPer10000 = NA_real_)
  }

  data
}

processed_data <- apply_normalization_data(processed_data)

data_refresh_needed <- function() {
  if (!file.exists(cache_path)) return(TRUE)
  if (!file.exists(file_path)) return(TRUE)

  csv_age_hours <- as.numeric(
    difftime(Sys.time(), file.info(file_path)$mtime, units = "hours")
  )

  isTRUE(csv_age_hours > refresh_interval_hours) ||
    isTRUE(file.info(file_path)$mtime > file.info(cache_path)$mtime)
}

launch_data_refresh <- function() {
  if (!data_refresh_needed()) return(FALSE)
  if (!file.exists(refresh_script_path) || file.access(".", 2) != 0) return(FALSE)

  rscript_path <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  if (!file.exists(rscript_path)) return(FALSE)

  args <- c(
    normalizePath(refresh_script_path, winslash = "/", mustWork = TRUE),
    normalizePath(".", winslash = "/", mustWork = TRUE),
    url,
    as.character(cache_version),
    as.character(normalization_start_year),
    as.character(normalization_end_year),
    paste(.libPaths(), collapse = .Platform$path.sep),
    as.character(refresh_interval_hours)
  )

  tryCatch(
    {
      system2(
        rscript_path,
        args = shQuote(args),
        stdout = refresh_log_path,
        stderr = refresh_log_path,
        wait = FALSE,
        invisible = TRUE
      )
      TRUE
    },
    error = function(e) {
      message("Background data refresh could not be started: ", conditionMessage(e))
      FALSE
    }
  )
}

refresh_was_launched <- launch_data_refresh()

citation_impact_data <- read.csv("citation_impact_data.csv", stringsAsFactors = FALSE)
citation_impact_total_data <- read.csv("citation_impact_total_data.csv", stringsAsFactors = FALSE)
citation_impact_top_articles <- read.csv("citation_impact_top200.csv", stringsAsFactors = FALSE)
citation_impact_summary <- read.csv("citation_impact_summary.csv", stringsAsFactors = FALSE)

author_identity_files <- c(
  leaderboard = "author_identity_leaderboard.csv",
  papers = "author_identity_papers.csv",
  reasons = "author_identity_reasons.csv",
  publishers = "author_identity_publishers.csv",
  summary = "author_identity_summary.csv"
)
author_identity_available <- all(file.exists(author_identity_files))

if (author_identity_available) {
  author_identity_data <- read.csv(
    author_identity_files[["leaderboard"]],
    stringsAsFactors = FALSE,
    na.strings = c("", "NA")
  )
  author_identity_paper_data <- read.csv(
    author_identity_files[["papers"]],
    stringsAsFactors = FALSE,
    na.strings = c("", "NA")
  )
  author_identity_reason_data <- read.csv(
    author_identity_files[["reasons"]],
    stringsAsFactors = FALSE,
    na.strings = c("", "NA")
  )
  author_identity_publisher_data <- read.csv(
    author_identity_files[["publishers"]],
    stringsAsFactors = FALSE,
    na.strings = c("", "NA")
  )
  author_identity_summary <- read.csv(
    author_identity_files[["summary"]],
    stringsAsFactors = FALSE
  )

  leaderboard_top <- head(author_identity_data, 35)
  leaderboard_author_choices <- setNames(
    leaderboard_top$openalex_author_id,
    paste0(leaderboard_top$author, " · ", leaderboard_top$openalex_author_id)
  )
} else {
  author_identity_data <- NULL
  author_identity_paper_data <- NULL
  author_identity_reason_data <- NULL
  author_identity_publisher_data <- NULL
  author_identity_summary <- NULL
  leaderboard_author_choices <- head(processed_data$author_data$Author, 35)
}

citation_metric <- function(metric, numeric = TRUE) {
  value <- citation_impact_summary$value[match(metric, citation_impact_summary$metric)]
  if (numeric) as.numeric(value) else value
}

author_identity_metric <- function(metric, numeric = TRUE) {
  if (!author_identity_available) return(if (numeric) NA_real_ else NA_character_)
  value <- author_identity_summary$value[match(metric, author_identity_summary$metric)]
  if (numeric) as.numeric(value) else value
}

owid_plot_theme <- function() {
  theme_minimal(base_family = "sans", base_size = 12) +
    theme(
      plot.background = element_rect(fill = "#ffffff", color = NA),
      panel.background = element_rect(fill = "#ffffff", color = NA),
      panel.grid.major = element_line(color = "#dedbd4", size = 0.35),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title = element_text(color = "#303842", size = 11),
      axis.text = element_text(color = "#5b6670", size = 10),
      axis.ticks = element_blank(),
      legend.title = element_text(color = "#303842", face = "bold"),
      legend.text = element_text(color = "#5b6670"),
      plot.margin = margin(12, 14, 8, 8)
    )
}

owid_plotly <- function(plot, tooltip = c("x", "y")) {
  ggplotly(plot, tooltip = tooltip) %>%
    layout(
      font = list(family = "Lato", color = "#303842"),
      paper_bgcolor = "#ffffff",
      plot_bgcolor = "#ffffff",
      hoverlabel = list(bgcolor = "#002147", bordercolor = "#002147", font = list(color = "#ffffff", family = "Lato"))
    ) %>%
    config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d")
    )
}

lag_distribution_plot <- function(valid_lags) {
  df <- valid_lags %>%
    filter(DiffYear <= 30) %>%
    count(DiffYear, name = "Count") %>%
    mutate(Tooltip = paste0(
      "<b>Time Lag:</b> ", DiffYear, ifelse(DiffYear == 1, " year", " years"),
      "<br><b>Count:</b> ", format(Count, big.mark = ",", trim = TRUE)
    ))

  reference_lines <- data.frame(
    Statistic = factor(c("Mean", "Median"), levels = c("Mean", "Median")),
    Value = c(mean(valid_lags$DiffYear), median(valid_lags$DiffYear))
  )

  p <- ggplot(df, aes(x = DiffYear, y = Count, text = Tooltip)) +
    geom_col(width = 0.9, fill = "#4c6a9c", color = "#ffffff", size = 0.35) +
    geom_vline(
      data = reference_lines,
      aes(xintercept = Value, color = Statistic, linetype = Statistic),
      size = 0.9
    ) +
    scale_color_manual(values = c("Mean" = "#b42532", "Median" = "#002147"), name = NULL) +
    scale_linetype_manual(values = c("Mean" = "dashed", "Median" = "dotted"), name = NULL) +
    scale_x_continuous(
      breaks = seq(0, 30, by = 2),
      limits = c(0, 30),
      expand = expansion(mult = c(0, 0.01))
    ) +
    labs(x = "Years from publication to retraction", y = "Unique retracted papers") +
    owid_plot_theme() +
    theme(legend.position = "top", legend.justification = "left")

  owid_plotly(p, tooltip = "text")
}

lag_stats_component <- function(df) {
  mean_val <- round(mean(df$DiffYear, na.rm = TRUE), 1)
  median_val <- round(median(df$DiffYear, na.rm = TRUE), 1)
  q1_val <- round(quantile(df$DiffYear, 0.25, na.rm = TRUE), 1)
  q3_val <- round(quantile(df$DiffYear, 0.75, na.rm = TRUE), 1)

  tags$div(
    class = "lag-stats",
    tags$div(class = "lag-stat",
      tags$div(class = "lag-stat-value", paste0(mean_val, " yrs")),
      tags$div(class = "lag-stat-label", "Mean publication-to-retraction lag")
    ),
    tags$div(class = "lag-stat",
      tags$div(class = "lag-stat-value", paste0(median_val, " yrs")),
      tags$div(class = "lag-stat-label", "Median publication-to-retraction lag")
    ),
    tags$div(class = "lag-stat",
      tags$div(class = "lag-stat-value", paste0(q1_val, " yrs")),
      tags$div(class = "lag-stat-label", "25th percentile")
    ),
    tags$div(class = "lag-stat",
      tags$div(class = "lag-stat-value", paste0(q3_val, " yrs")),
      tags$div(class = "lag-stat-label", "75th percentile")
    )
  )
}

clean_lag_summary_legend <- function(widget) {
  legend_order <- c("IQR", "Median", "Mean")
  labels_seen <- character()

  for (index in seq_along(widget$x$data)) {
    raw_name <- widget$x$data[[index]]$name
    if (is.null(raw_name) || !nzchar(raw_name)) next

    clean_name <- if (str_detect(raw_name, fixed("IQR"))) {
      "IQR"
    } else if (str_detect(raw_name, fixed("Median"))) {
      "Median"
    } else if (str_detect(raw_name, fixed("Mean"))) {
      "Mean"
    } else {
      NA_character_
    }

    if (is.na(clean_name)) next

    widget$x$data[[index]]$name <- clean_name
    widget$x$data[[index]]$legendgroup <- clean_name
    widget$x$data[[index]]$legendrank <- match(clean_name, legend_order)
    widget$x$data[[index]]$showlegend <- !clean_name %in% labels_seen
    labels_seen <- unique(c(labels_seen, clean_name))
  }

  widget$x$layout$legend$traceorder <- "normal"
  widget
}

chart_card <- function(title, subtitle, body, footer = NULL, class = "") {
  div(
    class = paste("data-card", class),
    div(
      class = "data-card-header",
      tags$h2(title),
      tags$p(subtitle)
    ),
    div(class = "data-card-body", body),
    if (!is.null(footer)) div(class = "data-card-footer", footer)
  )
}

ui <- navbarPage(
  title = div(
    class = "brand-lockup",
    span(class = "brand-name", "Retraction Watch Database Dashboard")
  ),
  id = "main_nav",
  windowTitle = "Retraction Watch Database Dashboard",
  collapsible = TRUE,
  theme = bs_theme(
    version = 5,
    bg = "#f7f3ea",
    fg = "#1d242b",
    primary = "#b42532",
    secondary = "#5b6670"
  ),
  header = tagList(
    tags$head(
      tags$meta(name = "description", content = "Interactive analysis of the Retraction Watch Database."),
      tags$script(HTML("
        function openAuthorInfoModal(button) {
          if (!window.bootstrap || !window.bootstrap.Modal) return;

          var modalElement = document.getElementById('author-info-modal');
          var modalTitle = document.getElementById('author-info-modal-title');
          var modalBody = document.getElementById('author-info-modal-body');
          if (!modalElement || !modalTitle || !modalBody) return;

          modalTitle.textContent = button.getAttribute('data-modal-title') || 'Author identity details';
          modalBody.innerHTML = button.getAttribute('data-modal-content') || '';

          window.bootstrap.Modal.getOrCreateInstance(modalElement).show();
        }

        function initializeAuthorInfoButtons() {
          document.querySelectorAll('.author-info-button').forEach(function (button) {
            if (button.dataset.modalInitialized === 'true') return;
            button.dataset.modalInitialized = 'true';
            button.addEventListener('click', function (event) {
              event.preventDefault();
              event.stopPropagation();
              openAuthorInfoModal(button);
            });
          });
        }

        document.addEventListener('DOMContentLoaded', function () {
          var brand = document.querySelector('.navbar-brand');
          if (brand) {
            brand.setAttribute('href', '#');
            brand.setAttribute('aria-label', 'Go to Overview');
          }

          var authorModal = document.getElementById('author-info-modal');
          if (authorModal && authorModal.dataset.cleanupInitialized !== 'true') {
            authorModal.dataset.cleanupInitialized = 'true';
            authorModal.addEventListener('hidden.bs.modal', function () {
              var modalBody = document.getElementById('author-info-modal-body');
              if (modalBody) modalBody.innerHTML = '';
            });
          }

          initializeAuthorInfoButtons();
          window.setTimeout(initializeAuthorInfoButtons, 500);

          if (!window.authorInfoObserver) {
            window.authorInfoObserver = new MutationObserver(function () {
              window.requestAnimationFrame(initializeAuthorInfoButtons);
            });
            window.authorInfoObserver.observe(document.body, { childList: true, subtree: true });
          }
        });

        document.addEventListener('click', function (event) {
          var brand = event.target.closest('.navbar-brand');
          if (!brand) return;
          event.preventDefault();

          var overview = document.querySelector('.navbar-nav .nav-link[data-value=Overview]');
          if (!overview) return;

          if (window.bootstrap && window.bootstrap.Tab) {
            window.bootstrap.Tab.getOrCreateInstance(overview).show();
          } else if (window.jQuery && window.jQuery.fn.tab) {
            window.jQuery(overview).tab('show');
          } else {
            overview.click();
          }

          window.scrollTo({ top: 0, behavior: 'smooth' });
        });

        function registerPageScrollHandler() {
          if (!window.Shiny || window.rwdScrollToPageTopHandler) return;
          window.rwdScrollToPageTopHandler = true;
          window.Shiny.addCustomMessageHandler('scrollToPageTop', function () {
              window.requestAnimationFrame(function () {
                window.requestAnimationFrame(function () {
                  window.scrollTo({ top: 0, behavior: 'smooth' });
                });
              });
            });
        }
        registerPageScrollHandler();
        document.addEventListener('shiny:connected', registerPageScrollHandler);

      ")),
      tags$style(HTML("
        @import url('https://fonts.googleapis.com/css2?family=Lato:wght@400;700;900&display=swap');

        :root {
          --navy: #002147;
          --navy-light: #163b64;
          --red: #b42532;
          --cream: #f7f3ea;
          --paper: #ffffff;
          --ink: #1d242b;
          --muted: #5b6670;
          --rule: #d8d3ca;
          --blue: #4c6a9c;
        }

        html { scroll-behavior: smooth; background: #ffffff; }
        body {
          background-color: var(--cream);
          background-image: linear-gradient(to bottom, #ffffff 0, #ffffff 72px, var(--cream) 72px, var(--cream) 100%);
          background-repeat: no-repeat;
          color: var(--ink); font-family: 'Lato', Arial, sans-serif;
        }
        h1, h2, h3, h4, h5, h6 { font-family: 'Lato', Arial, sans-serif; }
        a { color: var(--navy-light); }

        .navbar,
        .navbar > .container-fluid,
        .navbar-collapse,
        .navbar-nav {
          background-color: #ffffff !important;
          background-image: none !important;
        }
        .navbar {
          position: relative; isolation: isolate;
          background: #ffffff !important;
          border: 0;
          border-bottom: 4px solid var(--red);
          padding: 0;
          min-height: 72px;
        }
        .navbar::before {
          content: ''; position: absolute; z-index: -1; pointer-events: none;
          top: 0; bottom: 0; left: 50%; width: 100vw; transform: translateX(-50%);
          background: #ffffff;
        }
        .navbar > .container-fluid {
          width: 100% !important; max-width: 1264px !important; margin: 0 auto !important;
          padding: 0 28px; background: #ffffff !important;
        }
        .navbar-brand {
          display: flex; align-items: stretch; flex: 0 1 auto; min-width: 0;
          color: #111111 !important; padding: 0; margin-right: 34px; cursor: pointer;
        }
        .brand-lockup { display: flex; align-items: center; min-height: 68px; }
        .brand-name {
          display: flex; align-items: center; min-height: 68px; padding-bottom: 4px;
          color: #111111 !important; font-family: 'Lato', Arial, sans-serif;
          font-size: 1.15rem; font-weight: 900; letter-spacing: .02em;
          line-height: 1.5; white-space: nowrap;
        }
        .navbar-collapse { flex-grow: 0; }
        .navbar-nav .nav-link {
          color: #111111 !important; padding: 25px 14px 21px !important;
          border-bottom: 4px solid transparent; font-size: .75rem; font-weight: 900;
          letter-spacing: .055em; text-transform: uppercase;
        }
        .navbar-nav .nav-link:hover { color: #111111 !important; background: #f2efe8 !important; }
        .navbar-nav .nav-link.active { color: #111111 !important; border-bottom-color: var(--red); background: #f7f3ea !important; }
        .navbar-nav .dropdown-menu {
          min-width: 270px; margin-top: 0; padding: 6px 0; background: #ffffff;
          border: 1px solid var(--rule); border-top: 3px solid var(--red); border-radius: 0;
          box-shadow: 0 8px 20px rgba(29, 36, 43, .12);
        }
        .navbar-nav .dropdown-item {
          padding: 12px 18px; color: #111111; font-family: 'Lato', Arial, sans-serif;
          font-size: .78rem; font-weight: 900; letter-spacing: .035em;
        }
        .navbar-nav .dropdown-item:hover,
        .navbar-nav .dropdown-item:focus { color: #111111; background: #f2efe8; }
        .navbar-nav .dropdown-item.active,
        .navbar-nav .dropdown-item:active {
          color: #111111; background: #f7f3ea; box-shadow: inset 3px 0 0 var(--red);
        }
        .navbar-nav .dropdown.show > .nav-link { color: #111111 !important; background: #f7f3ea !important; }
        .navbar-toggler { margin: 12px 0; border-color: rgba(17,17,17,.55); }
        .navbar-toggler-icon {
          position: relative; width: 1.25rem; height: 1rem; background-image: none !important;
          border-top: 2px solid #111111; border-bottom: 2px solid #111111;
        }
        .navbar-toggler-icon::after {
          content: ''; position: absolute; left: 0; right: 0; top: calc(50% - 1px);
          border-top: 2px solid #111111;
        }

        .tab-content { max-width: 1264px; margin: 0 auto; padding: 0 28px 68px; }
        .tab-spacer { height: 34px; }
        .update-line { margin: 30px 0 22px; color: var(--muted); font-size: .75rem; font-weight: 900; letter-spacing: .08em; line-height: 1.5; text-transform: uppercase; }
        .update-line.is-refreshing { color: #8a5a00; }
        .update-line.is-ready { color: #235c3a; }
        .update-line.is-failed { color: #85202a; }
        .update-line-status { letter-spacing: .035em; }

        .data-card { margin-bottom: 30px; background: var(--paper); border-top: 3px solid var(--navy); box-shadow: 0 1px 0 rgba(0,0,0,.08); }
        .data-card-header { padding: 24px 26px 12px; }
        .data-card-header h2 { margin: 0; color: var(--navy); font-family: 'Lato', Arial, sans-serif; font-size: 1.55rem; font-weight: 900; }
        .data-card-header p { max-width: 720px; margin: 6px 0 0; color: var(--muted); font-size: .9rem; line-height: 1.5; }
        .data-card-body { padding: 8px 24px 24px; }
        .data-card-body .flush { margin: 0 -24px -24px; }
        .data-card-footer { padding: 13px 24px 16px; border-top: 1px solid #ebe7df; color: var(--muted); font-size: .78rem; line-height: 1.5; }

        .pub-year-note { display: flex; flex-wrap: wrap; align-items: center; gap: 4px 7px; }
        .pub-year-note-primary,
        .pub-year-note-context { display: inline; }
        .pub-year-note-context { color: #303842; }
        .ieee-spike-info-button.btn {
          display: inline-flex; align-items: center; justify-content: center;
          width: 17px; height: 17px; min-width: 17px; margin-left: 4px; padding: 0;
          border: 1px solid #8793a0; border-radius: 50%;
          background: #ffffff; color: var(--navy); box-shadow: none;
          font-family: Georgia, serif; font-size: .62rem; font-style: italic;
          font-weight: 700; line-height: 1; vertical-align: -2px;
        }
        .ieee-spike-info-button.btn:hover,
        .ieee-spike-info-button.btn:focus-visible {
          border-color: var(--navy); background: var(--navy); color: #ffffff;
          box-shadow: 0 0 0 3px rgba(0, 33, 71, .16);
        }
        .ieee-spike-modal-body { color: #303842; font-size: .9rem; line-height: 1.62; }
        .ieee-spike-modal-body > p:first-child { margin-top: 0; font-size: 1rem; }
        .ieee-spike-modal-body h3 {
          margin: 25px 0 8px; color: var(--navy); font-size: 1.05rem; font-weight: 900;
        }
        .ieee-spike-modal-body ul,
        .ieee-spike-modal-body ol { padding-left: 22px; }
        .ieee-spike-modal-body li + li { margin-top: 6px; }
        .ieee-spike-modal-body a { font-weight: 700; text-decoration-color: var(--red); }
        .ieee-spike-table-wrap { margin: 15px 0 20px; overflow-x: auto; }
        .ieee-spike-table { min-width: 640px; margin: 0; font-size: .82rem; }
        .ieee-spike-table > thead > tr > th { white-space: nowrap; }
        .ieee-spike-caution {
          margin-top: 15px; padding: 13px 15px; border-left: 3px solid var(--red);
          background: #fbebed; color: #4c2529;
        }

        .kpi-strip {
          margin-bottom: 30px; background: #ffffff; color: #111111;
          border: 1px solid var(--rule); border-top: 3px solid var(--red);
        }
        .kpi-primary {
          display: grid; grid-template-columns: minmax(0, 1fr) auto; align-items: center;
          gap: 32px; padding: 34px 32px 31px; border-bottom: 1px solid var(--rule);
        }
        .kpi-primary-title {
          color: #111111; font-family: 'Lato', Arial, sans-serif;
          font-size: clamp(.95rem, 1.7vw, 1.3rem);
          font-weight: 900; letter-spacing: .075em; line-height: 1.35; text-transform: uppercase;
        }
        .kpi-primary-value {
          color: #111111; font-family: 'Lato', Arial, sans-serif;
          font-size: clamp(2.45rem, 4.2vw, 3.65rem); font-weight: 900; letter-spacing: -.04em;
          line-height: .95; font-variant-numeric: tabular-nums;
        }
        .kpi-secondary { width: 100%; }
        .kpi-row {
          display: grid; grid-template-columns: minmax(0, 1fr) auto; align-items: center;
          gap: 24px; min-height: 78px; padding: 18px 32px; border-bottom: 1px solid var(--rule);
        }
        .kpi-row:last-child { border-bottom: 0; }
        .kpi-row-title {
          color: #111111; font-size: .76rem; font-weight: 900; letter-spacing: .085em;
          line-height: 1.35; text-transform: uppercase;
        }
        .kpi-row-value {
          color: #111111; font-family: 'Lato', Arial, sans-serif; font-size: 1.85rem;
          font-weight: 900; letter-spacing: -.025em; line-height: 1;
          font-variant-numeric: tabular-nums;
        }

        .impact-strip {
          margin-bottom: 30px; background: #ffffff; color: #111111;
          border: 1px solid var(--rule); border-top: 3px solid var(--red);
        }
        .impact-cell { min-height: 124px; padding: 24px; border-right: 1px solid var(--rule); }
        .impact-cell:last-child { border-right: 0; }
        .impact-label { color: #303842; font-size: .7rem; font-weight: 900; letter-spacing: .075em; line-height: 1.35; text-transform: uppercase; }
        .impact-label-with-info { display: inline-flex; align-items: center; gap: 6px; }
        .impact-value { margin-top: 9px; color: #111111; font-family: 'Lato', Arial, sans-serif; font-size: 2rem; font-weight: 900; line-height: 1; font-variant-numeric: tabular-nums; }
        .impact-value.negative { color: var(--red); }
        .impact-note { margin-top: 7px; color: var(--muted); font-size: .72rem; line-height: 1.35; }
        .citation-download-row {
          display: flex; align-items: center; flex-wrap: wrap; gap: 10px 14px;
          margin: -15px 0 30px;
        }
        .citation-download-button {
          display: inline-flex; align-items: center; padding: .55rem .85rem;
          border: 1px solid var(--navy); border-radius: 3px; background: var(--navy); color: #ffffff;
          font-size: .78rem; font-weight: 900; text-decoration: none;
        }
        .citation-download-button:hover,
        .citation-download-button:focus { border-color: var(--red); background: var(--red); color: #ffffff; }
        .citation-download-note { color: var(--muted); font-size: .72rem; line-height: 1.45; }
        .analysis-callout { padding: 2px 2px 4px; color: #303842; font-size: 1rem; line-height: 1.7; }
        .analysis-callout strong { color: var(--navy); }
        .analysis-callout p:last-child { margin-bottom: 0; }

        .analysis-link-wrap { margin-top: 8px; }
        .analysis-link {
          color: var(--red); font-weight: 900; text-decoration: none;
        }
        .analysis-link:hover,
        .analysis-link:focus { color: var(--navy); text-decoration: underline; }

        .lag-explorer-controls {
          display: grid; grid-template-columns: minmax(220px, 420px);
          gap: 22px; margin-bottom: 18px; padding: 16px 18px 14px;
          border: 1px solid var(--rule); background: #fbfaf7;
        }
        .lag-explorer-controls .form-group { margin-bottom: 0; }
        .lag-explorer-controls label {
          color: #303842; font-size: .7rem; font-weight: 900;
          letter-spacing: .06em; text-transform: uppercase;
        }
        .lag-explorer-controls .form-control,
        .lag-explorer-controls .selectize-input,
        .lag-explorer-controls select {
          border-radius: 0; border-color: var(--rule); box-shadow: none;
        }
        .lag-explorer-note div + div { margin-top: 7px; }
        .lag-interpretation-box {
          margin-top: 18px; padding: 17px 19px; border-left: 3px solid var(--red);
          background: #fbebed; color: #4c2529; font-size: .84rem; line-height: 1.6;
        }
        .lag-interpretation-box h3 {
          margin: 0 0 8px; color: var(--navy); font-family: 'Lato', Arial, sans-serif;
          font-size: .95rem; font-weight: 900;
        }
        .lag-interpretation-box p { margin: 0; }
        .lag-interpretation-box p + p { margin-top: 8px; }
        .lag-interpretation-box strong { color: var(--navy); }

        .identity-meta { margin-top: 17px; color: var(--muted); font-size: .76rem; line-height: 1.6; overflow-wrap: anywhere; }
        .identity-meta a { font-weight: 700; }
        .identity-review-note {
          margin-top: 16px; padding: 14px 16px; border-left: 3px solid var(--navy);
          background: #f3f6f9; color: #303842; font-size: .78rem; line-height: 1.55;
        }
        .identity-review-note.review { border-left-color: var(--red); background: #fbebed; }
        .identity-review-note strong { color: var(--navy); }
        .identity-review-note.review strong { color: #85202a; }
        .confidence-badge {
          display: inline-block; margin-left: 4px; padding: 2px 7px; border: 1px solid var(--rule);
          color: #303842; background: #f1eee7; font-size: .62rem; font-weight: 900;
          letter-spacing: .055em; line-height: 1.4; text-transform: uppercase;
        }
        .confidence-badge.review { color: #85202a; border-color: #d8aeb2; background: #fbebed; }

        .author-profile-grid {
          display: grid; grid-template-columns: repeat(2, minmax(0, 1fr));
          margin-top: 20px; border: 1px solid var(--rule); background: #fbfaf7;
        }
        .author-profile-item { min-height: 104px; padding: 18px; border-right: 1px solid var(--rule); border-bottom: 1px solid var(--rule); }
        .author-profile-item:nth-child(2n) { border-right: 0; }
        .author-profile-item:nth-last-child(-n+2) { border-bottom: 0; }
        .author-profile-value {
          color: var(--navy); font-family: 'Lato', Arial, sans-serif; font-size: 1.55rem;
          font-weight: 900; line-height: 1.05; font-variant-numeric: tabular-nums;
        }
        .author-profile-label {
          margin-top: 8px; color: var(--muted); font-size: .66rem; font-weight: 900;
          letter-spacing: .065em; line-height: 1.35; text-transform: uppercase;
        }
        .leaderboard-select .form-group { margin-bottom: 0; }
        .leaderboard-select label { color: #303842; font-size: .72rem; font-weight: 900; letter-spacing: .06em; text-transform: uppercase; }
        .leaderboard-select .form-control,
        .leaderboard-select .selectize-input { border-radius: 0; border-color: var(--rule); box-shadow: none; }

        .ranking-control {
          margin: 0 0 14px; padding: 12px 14px 10px;
          border-bottom: 1px solid var(--rule); background: #fbfaf7;
        }
        .ranking-control .form-group { margin-bottom: 0; }
        .ranking-control .control-label {
          display: block; margin-bottom: 7px; color: #303842; font-size: .69rem;
          font-weight: 900; letter-spacing: .065em; text-transform: uppercase;
        }
        .ranking-control .shiny-options-group {
          display: flex; align-items: center; flex-wrap: wrap; gap: 7px 18px;
        }
        .ranking-control .radio-inline {
          margin: 0 !important; padding-left: 20px; color: #303842;
          font-size: .78rem; font-weight: 700; line-height: 1.35;
        }
        .ranking-control input[type='radio'] { margin-top: 2px; accent-color: var(--red); }

        .author-articles-menu {
          margin-top: 18px; border: 1px solid var(--rule); background: #ffffff;
        }
        .author-articles-menu > summary {
          display: flex; align-items: center; justify-content: space-between; gap: 14px;
          min-height: 48px; padding: 13px 15px; color: var(--navy); cursor: pointer;
          font-size: .72rem; font-weight: 900; letter-spacing: .055em; line-height: 1.35;
          list-style: none; text-transform: uppercase;
        }
        .author-articles-menu > summary::-webkit-details-marker { display: none; }
        .author-articles-menu > summary::before {
          content: '\\25B8'; flex: 0 0 auto; color: var(--red); font-size: .9rem;
          transform-origin: center; transition: transform .15s ease;
        }
        .author-articles-menu[open] > summary::before { transform: rotate(90deg); }
        .author-articles-menu > summary:hover,
        .author-articles-menu > summary:focus-visible { background: #f3f6f9; outline: 0; }
        .author-articles-summary-label { margin-right: auto; }
        .author-articles-count {
          flex: 0 0 auto; padding: 2px 7px; background: var(--navy); color: #ffffff;
          font-size: .65rem; letter-spacing: 0; line-height: 1.4;
        }
        .leaderboard-articles-under-chart .author-articles-menu { margin-top: 12px; }
        .author-articles-list {
          max-height: 520px; overflow-y: auto; border-top: 1px solid var(--rule);
        }
        .author-article-item { padding: 15px; border-bottom: 1px solid #e7e2d9; }
        .author-article-item:last-child { border-bottom: 0; }
        .author-article-title {
          display: block; color: var(--navy); font-size: .86rem; font-weight: 900;
          line-height: 1.4; overflow-wrap: anywhere;
        }
        .author-article-title:hover { color: var(--red); }
        .author-article-authors {
          margin-top: 7px; color: #303842; font-size: .75rem; line-height: 1.45;
          overflow-wrap: anywhere;
        }
        .author-article-meta { margin-top: 7px; color: var(--muted); font-size: .7rem; line-height: 1.4; }
        .author-article-meta strong { color: #303842; }

        .lag-stats { display: grid; grid-template-columns: repeat(4, 1fr); background: #fbfaf7; }
        .lag-stat { padding: 18px 20px; border-right: 1px solid var(--rule); }
        .lag-stat:last-child { border-right: 0; }
        .lag-stat-value { color: var(--navy); font-family: 'Lato', Arial, sans-serif; font-size: 1.75rem; font-weight: 900; line-height: 1; font-variant-numeric: tabular-nums; }
        .lag-stat-label { margin-top: 7px; color: var(--muted); font-size: .68rem; font-weight: 900; letter-spacing: .07em; line-height: 1.35; text-transform: uppercase; }
        .lag-analysis-tabs > .tabbable > .nav-tabs {
          gap: 2px; margin: 0; padding: 0; border-bottom: 1px solid var(--rule);
        }
        .lag-analysis-tabs > .tabbable > .nav-tabs .nav-link {
          margin: 0; padding: 12px 16px 10px; border: 0; border-bottom: 3px solid transparent;
          border-radius: 0; background: transparent; color: #5b6670;
          font-family: 'Lato', Arial, sans-serif; font-size: .72rem; font-weight: 900;
          letter-spacing: .055em; text-transform: uppercase;
        }
        .lag-analysis-tabs > .tabbable > .nav-tabs .nav-link:hover,
        .lag-analysis-tabs > .tabbable > .nav-tabs .nav-link:focus-visible {
          background: #f3f0e9; color: var(--navy);
        }
        .lag-analysis-tabs > .tabbable > .nav-tabs .nav-link.active {
          border-bottom-color: var(--red); background: #ffffff; color: var(--navy);
        }
        .lag-analysis-tabs > .tabbable > .tab-content {
          max-width: none; margin: 0; padding: 10px 0 0;
        }
        .lag-analysis-intro {
          margin: 5px 0 4px; padding: 12px 15px; border-left: 3px solid var(--navy);
          background: #f3f6f9; color: #303842; font-size: .82rem; line-height: 1.55;
        }
        .lag-analysis-footnote {
          padding: 11px 2px 0; color: var(--muted); font-size: .76rem; line-height: 1.5;
        }
        .lag-analysis-footnote .shiny-text-output { display: inline; }
        .lag-sensitivity-summary {
          margin-top: 14px; padding: 15px 17px; border-left: 3px solid var(--red);
          background: #fbebed; color: #4c2529; font-size: .82rem; line-height: 1.55;
        }
        .lag-sensitivity-summary p { margin: 0; }
        .lag-sensitivity-summary p + p { margin-top: 7px; }

        .author-name-with-info { display: inline-flex; align-items: center; gap: 8px; min-width: 0; }
        .author-info-button {
          display: inline-flex; align-items: center; justify-content: center; flex: 0 0 auto;
          width: 17px; height: 17px; min-width: 17px; padding: 0; border: 1px solid #8793a0; border-radius: 50%;
          background: #ffffff; color: var(--navy); font-family: Georgia, serif; font-size: .62rem;
          font-style: italic; font-weight: 700; line-height: 1; cursor: pointer;
        }
        .author-info-button:hover,
        .author-info-button:focus-visible {
          border-color: var(--navy); background: var(--navy); color: #ffffff; outline: 0;
          box-shadow: 0 0 0 3px rgba(0, 33, 71, .16);
        }
        .citation-sample-info-button { vertical-align: middle; }
        .author-popover-section { margin-bottom: 14px; }
        .author-popover-section:last-child { margin-bottom: 0; }
        .author-popover-label {
          margin-bottom: 4px; color: var(--navy); font-size: .63rem; font-weight: 900;
          letter-spacing: .07em; text-transform: uppercase;
        }
        .author-popover-value { color: #303842; overflow-wrap: anywhere; }
        .author-popover-status {
          display: inline-block; margin-right: 5px; padding: 1px 6px; border: 1px solid #9aa5b0;
          background: #f3f6f9; color: var(--navy); font-size: .62rem; font-weight: 900;
          letter-spacing: .04em; text-transform: uppercase;
        }
        .author-popover-status.review { border-color: #d8aeb2; background: #fbebed; color: #85202a; }
        .author-info-modal .modal-dialog { max-width: 840px; }
        .citation-top-articles { max-height: 680px; overflow: auto; }
        .citation-top-articles table { min-width: 980px; margin-bottom: 0; }
        .citation-top-articles thead th {
          position: sticky; top: 0; z-index: 1; background: #ffffff;
          border-bottom: 2px solid var(--navy); color: var(--navy);
        }
        .citation-top-article-title { color: var(--navy); font-weight: 900; text-decoration: none; }
        .citation-top-article-title:hover,
        .citation-top-article-title:focus { color: var(--red); text-decoration: underline; }
        .citation-top-article-meta { margin-top: 4px; color: var(--muted); font-size: .72rem; line-height: 1.45; }
        .citation-top-number { font-family: 'Lato', Arial, sans-serif; font-variant-numeric: tabular-nums; white-space: nowrap; }
        .author-info-modal .modal-content {
          border: 1px solid var(--rule); border-radius: 0; background: #ffffff;
          box-shadow: 0 18px 48px rgba(29, 36, 43, .22);
        }
        .author-info-modal .modal-header {
          padding: 18px 22px 15px; border-bottom: 3px solid var(--navy); background: #ffffff;
        }
        .author-info-modal .modal-title {
          color: var(--navy); font-family: 'Lato', Arial, sans-serif;
          font-size: 1.15rem; font-weight: 900;
        }
        .author-info-modal .modal-body { padding: 22px 24px 8px; }
        .author-info-modal .modal-footer { padding: 13px 22px; border-top: 1px solid var(--rule); }
        .author-info-modal .modal-footer .btn {
          border-radius: 0; background: var(--navy); border-color: var(--navy); color: #ffffff;
          font-size: .75rem; font-weight: 900; letter-spacing: .04em; text-transform: uppercase;
        }
        .author-info-modal-body {
          color: #303842; font-family: 'Lato', Arial, sans-serif;
          font-size: .86rem; line-height: 1.62; user-select: text;
        }
        .author-info-modal-body .author-popover-section {
          margin: 0; padding: 0 0 18px; border-bottom: 1px solid #e7e2d9;
        }
        .author-info-modal-body .author-popover-section + .author-popover-section { padding-top: 18px; }
        .author-info-modal-body .author-popover-section:last-child { border-bottom: 0; }
        .author-info-modal-body .author-popover-label { margin-bottom: 7px; font-size: .69rem; }

        .reason-classification-group + .reason-classification-group { margin-top: 26px; }
        .reason-classification-heading {
          margin: 0 0 4px; color: var(--navy); font-family: 'Lato', Arial, sans-serif;
          font-size: 1rem; font-weight: 900; letter-spacing: -.01em;
        }
        .reason-classification-intro {
          max-width: 900px; margin: 0 0 12px; color: var(--muted);
          font-size: .78rem; line-height: 1.5;
        }
        .reason-classification-table { margin: 0 -24px; overflow-x: auto; }
        .reason-method {
          margin-top: 18px; border: 1px solid var(--rule); background: #fbfaf7;
        }
        .reason-method > summary {
          display: flex; align-items: center; gap: 10px; padding: 14px 16px;
          color: var(--navy); cursor: pointer; font-size: .72rem; font-weight: 900;
          letter-spacing: .06em; line-height: 1.4; list-style: none; text-transform: uppercase;
        }
        .reason-method > summary::-webkit-details-marker { display: none; }
        .reason-method > summary::before {
          content: '\\25B8'; color: var(--red); font-size: .9rem; transition: transform .15s ease;
        }
        .reason-method[open] > summary::before { transform: rotate(90deg); }
        .reason-method > summary:hover,
        .reason-method > summary:focus-visible { background: #f3f6f9; outline: 0; }
        .reason-method-body { padding: 4px 16px 16px; color: #303842; font-size: .78rem; line-height: 1.55; }
        .reason-definition-group-title {
          margin-top: 12px; padding-bottom: 7px; border-bottom: 2px solid var(--navy);
          color: var(--navy); font-size: .68rem; font-weight: 900;
          letter-spacing: .08em; text-transform: uppercase;
        }
        .reason-definition { padding: 12px 0; border-top: 1px solid #e7e2d9; }
        .reason-definition-title { color: var(--navy); font-weight: 900; }
        .reason-definition-role {
          display: inline-block; margin-left: 6px; padding: 1px 6px; border: 1px solid #9aa5b0;
          color: #5b6670; font-size: .6rem; font-weight: 900; letter-spacing: .04em;
          text-transform: uppercase;
        }
        .reason-definition-description { margin-top: 5px; color: #303842; }
        .reason-definition-labels { margin-top: 5px; color: var(--muted); overflow-wrap: anywhere; }
        .reason-method-exclusions {
          margin-top: 8px; padding-top: 14px; border-top: 2px solid var(--navy);
        }
        .reason-method-unclassified {
          margin-top: 14px; padding-top: 14px; border-top: 1px solid #d7d1c6;
          overflow-wrap: anywhere;
        }

        .table { margin: 0; color: var(--ink); font-family: 'Lato', Arial, sans-serif; font-size: .9rem; font-variant-numeric: tabular-nums; }
        .js-plotly-plot, .js-plotly-plot text { font-family: 'Lato', Arial, sans-serif !important; font-variant-numeric: tabular-nums; }
        .table > thead > tr > th { padding: 12px 18px; border: 0; border-bottom: 2px solid var(--navy); background: #f1eee7; color: var(--navy); font-size: .68rem; font-weight: 900; letter-spacing: .08em; text-transform: uppercase; }
        .table > tbody > tr > td { padding: 11px 18px; border-color: #e7e2d9; vertical-align: middle; }
        .table-striped > tbody > tr:nth-of-type(odd) > * { --bs-table-accent-bg: #fbfaf7; color: var(--ink); }
        .table-hover > tbody > tr:hover > * { --bs-table-accent-bg: #eef2f6; color: var(--ink); }

        .site-footer { background: #ffffff; color: #111111; border-top: 4px solid var(--red); }
        .site-footer-inner { display: flex; justify-content: space-between; gap: 32px; max-width: 1264px; margin: 0 auto; padding: 32px 28px; font-size: .8rem; line-height: 1.55; }
        .site-footer strong { color: #111111; }
        .site-footer a { color: #111111; text-decoration-color: var(--red); }
        .site-footer-author { font-weight: 700; }

        .shiny-notification { border-radius: 0; border: 0; border-left: 4px solid var(--red); font-family: 'Lato', Arial, sans-serif; }

        @media (max-width: 1100px) {
          .navbar-brand { margin-right: 20px; }
          .brand-name { font-size: .95rem; }
          .navbar-nav .nav-link { padding-left: 10px !important; padding-right: 10px !important; font-size: .7rem; }
        }
        @media (max-width: 991px) {
          .navbar > .container-fluid { padding-left: 18px; padding-right: 18px; }
          .navbar-brand { margin-right: 20px; }
          .brand-name { font-size: .88rem; }
          .navbar-nav .nav-link { padding: 12px 4px !important; border-bottom: 0; }
          .navbar-nav .nav-link.active { color: #111111 !important; }
          .navbar-nav .dropdown-menu {
            margin: 0 0 8px 10px; padding: 2px 0 2px 10px; border: 0;
            border-left: 3px solid var(--red); box-shadow: none;
          }
          .navbar-nav .dropdown-item { padding: 10px 12px; }
          .navbar-nav .dropdown-item.active,
          .navbar-nav .dropdown-item:active { box-shadow: none; }
          .tab-content { padding-left: 18px; padding-right: 18px; }
          .impact-cell:nth-child(2) { border-right: 0; }
          .impact-cell { border-bottom: 1px solid var(--rule); }
          .lag-explorer-controls { grid-template-columns: 1fr; }
        }
        @media (max-width: 575px) {
          .navbar > .container-fluid { padding: 0 16px; }
          .tab-content { padding-left: 16px; padding-right: 16px; }
          .navbar-brand { margin-right: 12px; }
          .brand-name { max-width: 250px; font-size: .82rem; white-space: normal; }
          .kpi-primary { gap: 14px; padding: 28px 22px 25px; }
          .kpi-primary-title { font-size: clamp(.68rem, 3vw, .85rem); letter-spacing: .055em; }
          .kpi-primary-value { font-size: clamp(1.7rem, 7vw, 2.15rem); }
          .kpi-row { min-height: 72px; padding: 16px 22px; gap: 16px; }
          .kpi-row-title { font-size: .68rem; }
          .kpi-row-value { font-size: 1.55rem; }
          .author-profile-grid { grid-template-columns: 1fr; }
          .author-profile-item,
          .author-profile-item:nth-child(2n),
          .author-profile-item:nth-last-child(-n+2) { border-right: 0; border-bottom: 1px solid var(--rule); }
          .author-profile-item:last-child { border-bottom: 0; }
          .impact-cell { border-right: 0; }
          .lag-stats { grid-template-columns: repeat(2, 1fr); }
          .lag-stat:nth-child(2) { border-right: 0; }
          .lag-stat:nth-child(-n+2) { border-bottom: 1px solid var(--rule); }
          .site-footer-inner { display: block; }
        }
      "))
    )
  ),
  footer = tagList(
    div(
      id = "author-info-modal",
      class = "modal fade author-info-modal",
      tabindex = "-1",
      `aria-labelledby` = "author-info-modal-title",
      `aria-hidden` = "true",
      div(
        class = "modal-dialog modal-lg modal-dialog-centered modal-dialog-scrollable",
        div(
          class = "modal-content",
          div(
            class = "modal-header",
            tags$h2(id = "author-info-modal-title", class = "modal-title", "Author identity details"),
            tags$button(
              type = "button",
              class = "btn-close",
              `data-bs-dismiss` = "modal",
              `aria-label` = "Close"
            )
          ),
          div(id = "author-info-modal-body", class = "modal-body author-info-modal-body"),
          div(
            class = "modal-footer",
            tags$button(type = "button", class = "btn btn-primary", `data-bs-dismiss` = "modal", "Close")
          )
        )
      )
    ),
    div(
      class = "site-footer",
      div(
        class = "site-footer-inner",
        div(
          tags$a(
            class = "site-footer-author",
            "Alejandro Sandoval Lentisco",
            href = "https://sandovallentisco.github.io/",
            target = "_blank",
            rel = "noopener noreferrer"
          ),
          paste0(" · ", current_year),
          tags$br(),
          "Independent analysis of the Retraction Watch Database."
        ),
        div(
          "Source: ",
          tags$a("Retraction Watch Database", href = "https://gitlab.com/crossref/retraction-watch-data", target = "_blank", rel = "noopener noreferrer"),
          tags$br(), "Data refreshes automatically every week."
        )
      )
    )
  ),

  tabPanel(
    "Overview",
    uiOutput("data_refresh_status"),
    uiOutput("kpi_boxes"),
    fluidRow(
      column(
        6,
        chart_card(
          "Original publication years",
          "Number of records by the year the original paper was published.",
          plotlyOutput("pub_year_plot", height = "380px"),
          uiOutput("pub_outliers_note")
        )
      ),
      column(
        6,
        chart_card(
          "Retraction years",
          "Number of records by the year the retraction was issued.",
          plotlyOutput("retraction_year_plot", height = "380px"),
          textOutput("retraction_outliers_text")
        )
      )
    ),
    fluidRow(
      column(
        12,
        chart_card(
          "Publication vs retraction timeline",
          "Distribution of the elapsed time between publication and retraction for unique retracted papers. Compare the full sample with a sensitivity analysis excluding the IEEE 2010–2011 anomaly.",
          div(
            class = "lag-analysis-tabs",
            tabsetPanel(
              id = "lag_timeline_view",
              type = "tabs",
              tabPanel(
                "Full sample",
                value = "full_sample",
                plotlyOutput("diff_dist_plot", height = "410px"),
                uiOutput("diff_stats"),
                div(class = "lag-analysis-footnote", textOutput("diff_outliers_text"))
              ),
              tabPanel(
                "Sensitivity analysis",
                value = "sensitivity",
                div(
                  class = "lag-analysis-intro",
                  tags$strong("Exclusion rule: "),
                  "remove unique retracted papers published by IEEE when either the publication year or the recorded retraction year is 2010 or 2011. All other timeline criteria remain unchanged."
                ),
                plotlyOutput("diff_sensitivity_plot", height = "410px"),
                uiOutput("diff_sensitivity_stats"),
                uiOutput("diff_sensitivity_summary")
              )
            )
          ),
          div(
            class = "analysis-link-wrap",
            actionLink(
              "open_lag_explorer",
              "Explore by retraction reason, and subject.",
              class = "analysis-link"
            )
          )
        )
      )
    ),
    fluidRow(
      column(
        6,
        chart_card(
          "Retractions by subject",
          "Broad subject categories represented in retracted records.",
          div(class = "flush", tableOutput("top_subjects_table")),
          "A paper can have multiple subjects, but each broad category is counted at most once per paper."
        )
      ),
      column(
        6,
        chart_card(
          "Top 10 retracted publishers",
          "Publishers with the most unique retracted papers published since 1990, ranked by total papers or by a publication-normalized rate.",
          tagList(
            div(
              class = "ranking-control",
              radioButtons(
                "publisher_rank_metric",
                "Rank by",
                choices = c(
                  "Retracted papers" = "total",
                  "Per 10,000 OpenAlex works" = "rate"
                ),
                selected = "total",
                inline = TRUE
              )
            ),
            div(class = "flush", tableOutput("top_publishers_table"))
          ),
          tagList(
            "Rate = unique retracted papers ÷ OpenAlex works × 10,000 for publications from January 1, 1990 to ",
            normalization_period_end, ". Publisher groups use manually audited OpenAlex publisher lineages; historical imprint coverage can still be incomplete. ",
            "Hindawi is kept separate from Wiley. OpenAlex snapshot: ", normalization_retrieved_date, ". This is a descriptive coverage rate, not a misconduct-risk estimate."
          )
        )
      )
    ),
    fluidRow(
      column(
        12,
        chart_card(
          "Retraction reason taxonomy",
          "Ten substantive categories describe what was reported about a paper; two pathways/context categories describe how or why the retraction decision was reached. Counts refer to unique retracted papers.",
          tagList(
            div(
              class = "reason-classification-group",
              tags$h3(class = "reason-classification-heading", "Substantive categories"),
              tags$p(
                class = "reason-classification-intro",
                "These categories describe the reported problem or integrity concern in the publication."
              ),
              div(class = "reason-classification-table", tableOutput("substantive_reason_categories_table"))
            ),
            div(
              class = "reason-classification-group",
              tags$h3(class = "reason-classification-heading", "Pathways and context"),
              tags$p(
                class = "reason-classification-intro",
                "These categories provide procedural or editorial context and should not be interpreted as additional defects."
              ),
              div(class = "reason-classification-table", tableOutput("reason_pathways_table"))
            ),
            uiOutput("reason_classification_definitions")
          ),
          uiOutput("reason_classification_note")
        )
      )
    )
  ),

  navbarMenu(
    "Analyses",
    tabPanel(
      "Citation Impact",
    div(class = "tab-spacer", `aria-hidden` = "true"),
    uiOutput("citation_impact_stats"),
    div(
      class = "citation-download-row",
      downloadButton(
        "download_citation_reproducibility",
        "Download detailed reproducibility data (.CSV)",
        class = "btn citation-download-button"
      ),
      span(
        class = "citation-download-note",
        "All 8,000 sampled papers, metadata, DOI and OpenAlex links, inclusion flags and annual citation counts from year −5 through +5."
      )
    ),
    fluidRow(
      column(
        8,
        chart_card(
          "Citations around the retraction year",
          "Annual mean, median and interquartile range of citing works among papers already published in each relative year.",
          plotlyOutput("citation_impact_plot", height = "500px"),
          paste0(
            "Source: OpenAlex. Retrieved ", citation_metric("retrieved_date", numeric = FALSE),
            ". Year 0 is the calendar year in which the retraction was issued. Before year 0, each value includes only papers ",
            "that had already been published; the denominator is shown in the tooltip. All matched papers are observable from year 0 through year +5."
          )
        )
      ),
      column(
        4,
        chart_card(
          "Annual citation-rate summary",
          "Average of the annual citation means across the five years before and after retraction. The retraction year is shown separately.",
          div(class = "flush", tableOutput("citation_impact_table")),
          "The pre-retraction denominator grows as papers enter the published cohort; the post-retraction denominator is fixed."
        )
      )
    ),
    fluidRow(
      column(
        12,
        chart_card(
          "Mean total citations per paper, −5 to +5 years",
          paste0(
            "Running total of citing works from year −5 through each relative year for the fixed cohort of ",
            format(citation_metric("fixed_window_records"), big.mark = ","),
            " papers already published by year −5."
          ),
          plotlyOutput("citation_impact_total_plot", height = "470px"),
          paste0(
            "Source: OpenAlex. The same ", format(citation_metric("fixed_window_records"), big.mark = ","),
            " papers contribute at every point, so these are distributions of actual per-paper totals—not sums of annual means with changing denominators."
          )
        )
      )
    ),
    fluidRow(
      column(
        12,
        chart_card(
          "200 most-cited papers after retraction",
          "Ranked by the number of citing works published in relative years +1 through +5 after each paper's retraction year.",
          div(class = "flush citation-top-articles table-responsive", uiOutput("citation_top_articles_table")),
          paste0(
            "Source: OpenAlex and the Retraction Watch Database. Year 0 is excluded because it combines time before and after the retraction date. ",
            "The ranking is descriptive and citation counts may change as OpenAlex updates its index."
          )
        )
      )
    )
  ),

    tabPanel(
      "Retraction Lag",
      div(class = "tab-spacer", `aria-hidden` = "true"),
      fluidRow(
        column(
          12,
          chart_card(
            "Publication-to-retraction lag by group",
            "Compare the median, mean and interquartile range among unique retracted papers by substantive retraction reason or broad subject.",
            tagList(
              div(
                class = "lag-explorer-controls",
                selectInput(
                  "lag_breakdown_dimension",
                  "Break down by",
                  choices = c(
                    "Retraction reason category",
                    "Subject"
                  ),
                  selected = "Retraction reason category",
                  width = "100%"
                )
              ),
              plotlyOutput("lag_summary_plot", height = "560px"),
              uiOutput("lag_interpretation")
            ),
            uiOutput("lag_explorer_note")
          )
        )
      )
    ),

    tabPanel(
      "Geographical Analysis",
    div(class = "tab-spacer", `aria-hidden` = "true"),
    fluidRow(
      column(
        8,
        chart_card(
          "World map of retractions",
          "Countries are shaded by the selected ranking metric for unique retracted papers originally published since 1990.",
          plotlyOutput("world_map_plot", height = "500px"),
          uiOutput("world_map_note")
        )
      ),
      column(
        4,
        chart_card(
          "Top 15 countries",
          "Countries with the most unique retracted papers published since 1990, ranked by total papers or by a publication-normalized rate.",
          tagList(
            div(
              class = "ranking-control",
              radioButtons(
                "country_rank_metric",
                "Rank table and map by",
                choices = c(
                  "Retracted papers" = "total",
                  "Per 10,000 OpenAlex works" = "rate"
                ),
                selected = "total",
                inline = TRUE
              )
            ),
            div(class = "flush", tableOutput("top_countries_table"))
          ),
          tagList(
            "Rate = unique retracted papers ÷ OpenAlex works × 10,000 from January 1, 1990 to ", normalization_period_end, ". ",
            "Both numerator and denominator assign a work to a country when at least one author affiliation is located there, so international papers can count in several countries. ",
            "OpenAlex snapshot: ", normalization_retrieved_date, ". This is a descriptive coverage rate, not a country-risk estimate."
          )
        )
      )
    )
  ),

    tabPanel(
      "RWD Leaderboard",
      div(class = "tab-spacer", `aria-hidden` = "true"),
      fluidRow(
        column(
          12,
          chart_card(
            if (author_identity_available) "Top authors associated with retracted papers" else "Authors associated with the most unique retracted papers",
            if (author_identity_available) {
              "The 150 most frequent raw RWD author strings resolved against DOI-matched OpenAlex authorships and ranked by unique retracted papers."
            } else {
              "Exact author-name strings ranked by unique retracted papers, rather than by all RWD records."
            },
            div(class = "flush table-responsive author-leaderboard-table", uiOutput("top_authors_table")),
            if (author_identity_available) {
              paste0(
                "Only papers matched through a DOI are included. Profiles are combined only when they share the same ORCID or repeatedly match on coauthors ",
                "and institutions; a similar name is not enough. ‘Review’ means that the available data cannot confirm the full name. Click or tap ",
                "the information icon beside an author to open a large modal, like those for ‘Original publication years’. When profiles are combined, each OpenAlex work is counted once. ",
                "OpenAlex snapshot: ", author_identity_metric("retrieved_date", numeric = FALSE), ". ",
                "Authorship association does not establish responsibility for a retraction."
              )
            } else {
              paste0(
                "Only retraction records are included. Papers are deduplicated using the normalized original-paper DOI, ",
                "with RWD Record ID as a fallback. Name variants are not merged, homonyms may remain combined, and ",
                "authorship association does not establish responsibility for a retraction. Median retraction lag is the ",
                "median number of calendar years from original publication to retraction."
              )
            }
          )
        )
      ),
      fluidRow(
        column(
          4,
          chart_card(
            "Explore an author",
            if (author_identity_available) "Select one of the top 35 resolved OpenAlex identities." else "Select one of the top 35 exact RWD author strings.",
            tagList(
              div(
                class = "leaderboard-select",
                selectInput(
                  "leaderboard_author",
                  "Author",
                  choices = leaderboard_author_choices,
                  selected = leaderboard_author_choices[[1]],
                  width = "100%"
                )
              ),
              uiOutput("leaderboard_profile_stats")
            ),
            if (author_identity_available) {
              "OpenAlex works count is a changing external denominator and should be interpreted as a descriptive normalization, not an estimate of misconduct risk."
            } else {
              "A publication-normalized retraction rate requires author-identity resolution with OpenAlex or ORCID and is not included in this RWD-only view."
            }
          )
        ),
        column(
          8,
          chart_card(
            "Retracted papers over time",
            if (author_identity_available) "DOI-matched unique retracted papers associated with the selected OpenAlex identity by retraction year." else "Unique retracted papers associated with the selected author name by retraction year.",
            plotlyOutput("leaderboard_timeline_plot", height = "390px"),
            "The earliest recorded retraction year is used when duplicate RWD records refer to the same original paper."
          ),
          div(class = "leaderboard-articles-under-chart", uiOutput("leaderboard_articles"))
        )
      ),
      fluidRow(
        column(
          6,
          chart_card(
            "Most frequent retraction reasons from this author",
            if (author_identity_available) "The Overview retraction-reason taxonomy applied to unique retracted papers matched to the selected OpenAlex identity." else "The Overview retraction-reason taxonomy applied to unique retracted papers associated with the selected author name.",
            div(class = "flush table-responsive", tableOutput("leaderboard_reasons_table")),
            "Categories are not mutually exclusive. Substantive categories describe the reported concern; pathways/context describe how or why the retraction decision was reached."
          )
        ),
        column(
          6,
          chart_card(
            "Publishers represented",
            if (author_identity_available) "Publishers most frequently associated with unique retracted papers matched to the selected OpenAlex identity." else "Publishers most frequently associated with the selected author's unique retracted papers.",
            div(class = "flush table-responsive", tableOutput("leaderboard_publishers_table")),
            "Publisher names follow the RWD source strings and are not used to infer responsibility."
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  retraction_data <- reactiveVal(processed_data$retraction_data)
  lag_paper_data <- reactiveVal(processed_data$lag_paper_data)
  lag_breakdown_data <- reactiveVal(processed_data$lag_breakdown_data)
  country_data <- reactiveVal(processed_data$country_data)
  subject_data <- reactiveVal(processed_data$subject_data)
  publisher_data <- reactiveVal(processed_data$publisher_data)
  reason_data <- reactiveVal(processed_data$reason_data)
  paper_reason_data <- reactiveVal(processed_data$paper_reason_data)
  reason_classification_data <- reactiveVal(processed_data$reason_classification_data)
  reason_classification_summary_data <- reactiveVal(processed_data$reason_classification_summary)
  ieee_spike_summary_data <- reactiveVal(processed_data$ieee_spike_summary)
  ieee_spike_conference_data <- reactiveVal(processed_data$ieee_spike_conferences)
  author_data <- reactiveVal(processed_data$author_data)
  author_paper_data <- reactiveVal(processed_data$author_paper_data)
  paper_metadata_data <- reactiveVal(processed_data$paper_metadata_data)
  author_year_data <- reactiveVal(processed_data$author_year_data)
  author_reason_data <- reactiveVal(processed_data$author_reason_data)
  author_publisher_data <- reactiveVal(processed_data$author_publisher_data)
  resolved_author_data <- reactiveVal(author_identity_data)
  resolved_author_paper_data <- reactiveVal(author_identity_paper_data)
  resolved_author_reason_data <- reactiveVal(author_identity_reason_data)
  resolved_author_publisher_data <- reactiveVal(author_identity_publisher_data)
  data_update_date <- reactiveVal(update_date)
  active_cache_mtime <- reactiveVal(file.info(cache_path)$mtime)
  refresh_applied <- reactiveVal(FALSE)

  refresh_status <- reactivePoll(
    1500,
    session,
    checkFunc = function() {
      status_mtime <- if (file.exists(refresh_status_path)) {
        as.numeric(file.info(refresh_status_path)$mtime)
      } else {
        0
      }
      cache_mtime <- if (file.exists(cache_path)) {
        as.numeric(file.info(cache_path)$mtime)
      } else {
        0
      }
      paste(status_mtime, cache_mtime, sep = ":")
    },
    valueFunc = function() {
      if (!file.exists(refresh_status_path)) {
        return(list(state = "idle", message = ""))
      }
      tryCatch(
        readRDS(refresh_status_path),
        error = function(e) list(state = "idle", message = "")
      )
    }
  )

  install_processed_data <- function(data) {
    data <- apply_normalization_data(data)
    retraction_data(data$retraction_data)
    lag_paper_data(data$lag_paper_data)
    lag_breakdown_data(data$lag_breakdown_data)
    country_data(data$country_data)
    subject_data(data$subject_data)
    publisher_data(data$publisher_data)
    reason_data(data$reason_data)
    paper_reason_data(data$paper_reason_data)
    reason_classification_data(data$reason_classification_data)
    reason_classification_summary_data(data$reason_classification_summary)
    ieee_spike_summary_data(data$ieee_spike_summary)
    ieee_spike_conference_data(data$ieee_spike_conferences)
    author_data(data$author_data)
    author_paper_data(data$author_paper_data)
    paper_metadata_data(data$paper_metadata_data)
    author_year_data(data$author_year_data)
    author_reason_data(data$author_reason_data)
    author_publisher_data(data$author_publisher_data)
  }

  observeEvent(refresh_status(), {
    status <- refresh_status()
    if (!identical(status$state, "ready") || !file.exists(cache_path)) return()

    current_mtime <- file.info(cache_path)$mtime
    previous_mtime <- active_cache_mtime()
    cache_is_newer <- is.na(previous_mtime) || isTRUE(current_mtime > previous_mtime)
    if (!cache_is_newer) return()

    refreshed_cache <- tryCatch(readRDS(cache_path), error = function(e) NULL)
    if (
      !is.list(refreshed_cache) ||
      !identical(refreshed_cache$cache_version, cache_version) ||
      !is.list(refreshed_cache$data)
    ) {
      return()
    }

    install_processed_data(refreshed_cache$data)
    active_cache_mtime(current_mtime)

    refreshed_time <- if (
      !is.null(refreshed_cache$source_modified_at) &&
      nzchar(refreshed_cache$source_modified_at)
    ) {
      as.POSIXct(
        refreshed_cache$source_modified_at,
        format = "%Y-%m-%dT%H:%M:%S%z"
      )
    } else {
      current_mtime
    }
    data_update_date(format_update_date(refreshed_time))
    refresh_applied(TRUE)

    showNotification(
      "The latest Retraction Watch data are ready and the dashboard has been updated.",
      type = "message",
      duration = 8
    )
  }, ignoreInit = FALSE)

  output$data_refresh_status <- renderUI({
    status <- refresh_status()
    state <- if (!is.null(status$state)) status$state else "idle"

    if (identical(state, "refreshing") || (isTRUE(refresh_was_launched) && identical(state, "idle"))) {
      return(div(
        class = "update-line is-refreshing",
        span("Latest cached data: ", data_update_date()),
        span(
          class = "update-line-status",
          " · Preparing a newer Retraction Watch dataset in the background. The dashboard remains available."
        )
      ))
    }

    if (identical(state, "ready") && isTRUE(refresh_applied())) {
      return(div(
        class = "update-line is-ready",
        span("Latest data update: ", data_update_date()),
        span(class = "update-line-status", " · Background refresh complete.")
      ))
    }

    if (identical(state, "failed")) {
      return(div(
        class = "update-line is-failed",
        span("Latest cached data: ", data_update_date()),
        span(
          class = "update-line-status",
          " · The automatic refresh failed; the previous cached dashboard remains active."
        )
      ))
    }

    div(class = "update-line", paste("Latest data update:", data_update_date()))
  })

  observeEvent(input$open_lag_explorer, {
    updateNavbarPage(session, "main_nav", selected = "Retraction Lag")
    session$sendCustomMessage("scrollToPageTop", list())
  })

  lag_dimension_data <- reactive({
    req(lag_breakdown_data(), input$lag_breakdown_dimension)
    lag_breakdown_data() %>%
      filter(Breakdown == input$lag_breakdown_dimension)
  })

  lag_group_stats <- reactive({
    lag_dimension_data() %>%
      group_by(Group) %>%
      summarise(
        Papers = n_distinct(PaperKey),
        Mean = mean(DiffYear),
        Median = median(DiffYear),
        Q1 = as.numeric(quantile(DiffYear, 0.25)),
        Q3 = as.numeric(quantile(DiffYear, 0.75)),
        .groups = "drop"
      ) %>%
      filter(Papers > 0) %>%
      arrange(desc(Papers), Group)
  })

  output$lag_summary_plot <- renderPlotly({
    req(lag_group_stats(), input$lag_breakdown_dimension)
    stats <- lag_group_stats()
    validate(need(nrow(stats) > 0, "No eligible groups are available."))

    stats <- stats %>%
      mutate(
        GroupLabel = str_wrap(Group, width = 34),
        Tooltip = paste0(
          "<b>", Group, "</b>",
          "<br><b>Unique Papers:</b> ", format(Papers, big.mark = ",", trim = TRUE),
          "<br><b>Median Lag:</b> ", format(round(Median, 1), nsmall = 1), " years",
          "<br><b>Mean Lag:</b> ", format(round(Mean, 1), nsmall = 1), " years",
          "<br><b>Interquartile Range:</b> ", format(round(Q1, 1), nsmall = 1),
          "–", format(round(Q3, 1), nsmall = 1), " years"
        )
      )

    ordered_labels <- stats %>%
      arrange(Median, Mean, GroupLabel) %>%
      pull(GroupLabel)
    stats$GroupLabel <- factor(stats$GroupLabel, levels = ordered_labels)

    points <- bind_rows(
      stats %>% transmute(GroupLabel, Tooltip, Statistic = "Median", Value = Median),
      stats %>% transmute(GroupLabel, Tooltip, Statistic = "Mean", Value = Mean)
    ) %>%
      mutate(Statistic = factor(Statistic, levels = c("Median", "Mean")))

    p <- ggplot() +
      geom_segment(
        data = stats,
        aes(
          x = Q1,
          xend = Q3,
          y = GroupLabel,
          yend = GroupLabel,
          linetype = "IQR (bar)"
        ),
        color = "#9aa5b0",
        size = 1.7,
        lineend = "round"
      ) +
      geom_point(
        data = points,
        aes(x = Value, y = GroupLabel, color = Statistic, shape = Statistic, text = Tooltip),
        size = 3.1
      ) +
      scale_color_manual(
        values = c("Median" = "#002147", "Mean" = "#b42532"),
        name = NULL
      ) +
      scale_shape_manual(values = c("Median" = 16, "Mean" = 17), name = NULL) +
      scale_linetype_manual(values = c("IQR (bar)" = "solid"), name = NULL) +
      scale_x_continuous(
        breaks = scales::pretty_breaks(n = 8),
        limits = c(0, NA),
        expand = expansion(add = c(0.8, 0.5))
      ) +
      labs(x = "Years from publication to retraction", y = NULL) +
      owid_plot_theme() +
      theme(
        legend.position = "top",
        legend.justification = "left",
        panel.grid.major.x = element_line(color = "#dedbd4", size = 0.35),
        panel.grid.major.y = element_blank(),
        axis.text.x = element_text(margin = margin(t = 8)),
        axis.text.y = element_text(color = "#303842")
      ) +
      guides(
        color = guide_legend(order = 1),
        shape = guide_legend(order = 1),
        linetype = guide_legend(order = 2, override.aes = list(color = "#9aa5b0", size = 1.7))
      )

    clean_lag_summary_legend(owid_plotly(p, tooltip = "text"))
  })

  output$lag_interpretation <- renderUI({
    req(lag_group_stats(), input$lag_breakdown_dimension)
    stats <- lag_group_stats() %>%
      filter(is.finite(Median), is.finite(Mean))
    validate(need(nrow(stats) > 0, "No eligible groups are available for interpretation."))

    shortest <- stats %>%
      arrange(Median, Mean, desc(Papers), Group) %>%
      slice(1)
    longest <- stats %>%
      arrange(desc(Median), desc(Mean), desc(Papers), Group) %>%
      slice(1)

    format_lag <- function(value) {
      paste0(format(round(value, 1), nsmall = 1, trim = TRUE), " years")
    }

    if (identical(input$lag_breakdown_dimension, "Retraction reason category")) {
      return(div(
        class = "lag-interpretation-box",
        tags$h3("Interpretation · Retraction reason categories"),
        tags$p(
          "In the current RWD snapshot, ", tags$strong(shortest$Group),
          " has the shortest group median (", tags$strong(format_lag(shortest$Median)),
          "), while ", tags$strong(longest$Group),
          " has the longest (", tags$strong(format_lag(longest$Median)), ")."
        ),
        tags$p(
          "These substantive categories overlap: one paper can contribute to several rows. Shorter lags can reflect early screening, coordinated publisher action or batch retractions, while longer lags can reflect concerns detected through later post-publication scrutiny or investigations. The comparison is descriptive and does not show that a reason category causes a particular retraction speed."
        )
      ))
    }

    div(
      class = "lag-interpretation-box",
      tags$h3("Interpretation · Subjects"),
      tags$p(
        "In the current RWD snapshot, ", tags$strong(shortest$Group),
        " has the shortest subject median (", tags$strong(format_lag(shortest$Median)),
        "), while ", tags$strong(longest$Group),
        " has the longest (", tags$strong(format_lag(longest$Median)), ")."
      ),
      tags$p(
        "A paper can contribute to more than one broad subject. Differences can reflect each subject's mixture of publishers, journals, retraction reasons, publication practices and investigation workflows. They should not be interpreted as comparative misconduct rates, research quality rankings or causal effects of field membership."
      )
    )
  })

  output$lag_explorer_note <- renderUI({
    req(lag_dimension_data(), lag_group_stats(), input$lag_breakdown_dimension)
    stats <- lag_group_stats()
    groups_shown <- nrow(stats)
    unique_papers <- n_distinct(lag_dimension_data()$PaperKey)

    overlap_note <- switch(
      input$lag_breakdown_dimension,
      "Subject" = "A paper is counted once in each broad RWD subject assigned to it.",
      "Retraction reason category" = "Substantive reason categories can overlap; pathways and contextual categories are not included here.",
      ""
    )

    div(
      class = "lag-explorer-note",
      div(
        "The comparison shows ", groups_shown, " groups and is based on ",
        format(unique_papers, big.mark = ","),
        " unique retracted papers with a valid non-negative lag in this breakdown."
      ),
      div(overlap_note, " Quartiles, means and medians include lags longer than 30 years.")
    )
  })
  
  output$kpi_boxes <- renderUI({
    req(retraction_data())
    df <- retraction_data()
    
    total <- nrow(df)
    
    retraction_count <- sum(str_detect(df$RetractionNature, "(?i)Retraction"), na.rm = TRUE)
    eoc_count <- sum(str_detect(df$RetractionNature, "(?i)Expression of concern"), na.rm = TRUE)
    correction_count <- sum(str_detect(df$RetractionNature, "(?i)Correction|Update"), na.rm = TRUE)
    
    cards <- div(
      class = "kpi-strip",
      div(class = "kpi-primary",
          div(class = "kpi-primary-title", "Total Retraction Watch Database entries"),
          div(class = "kpi-primary-value", format(total, big.mark = ","))
      ),
      div(class = "kpi-secondary",
        div(class = "kpi-row",
            div(class = "kpi-row-title", "Retractions"),
            div(class = "kpi-row-value", format(retraction_count, big.mark = ","))
        ),
        div(class = "kpi-row",
            div(class = "kpi-row-title", "Expressions of concern"),
            div(class = "kpi-row-value", format(eoc_count, big.mark = ","))
        ),
        div(class = "kpi-row",
            div(class = "kpi-row-title", "Corrections and updates"),
            div(class = "kpi-row-value", format(correction_count, big.mark = ","))
        )
      )
    )
    cards
  })

  output$citation_impact_stats <- renderUI({
    year_minus_five <- citation_impact_data %>% filter(relative_year == -5)
    year_plus_five <- citation_impact_data %>% filter(relative_year == 5)
    matched_records <- citation_metric("completed_records")
    fixed_window_records <- citation_metric("fixed_window_records")
    sampled_records <- citation_metric("sampled_records")
    cutoff_year <- citation_metric("retraction_cutoff_year")
    sample_modal_content <- paste0(
      as.character(tagList(
        div(
          class = "author-popover-section",
          div(class = "author-popover-label", "Sampling frame"),
          div(
            class = "author-popover-value",
            paste0(
              "The sampling frame comprises Retraction Watch Database records with a valid DOI for the original paper and a usable retraction year. The retraction-year cutoff is ",
              cutoff_year,
              " so that every matched paper can have a complete five-calendar-year post-retraction window through 2025: a paper retracted in 2020 is followed in 2021, 2022, 2023, 2024 and 2025. The cutoff applies to the retraction year, not the publication year; sampled papers may have been published many years earlier."
            )
          )
        ),
        div(
          class = "author-popover-section",
          div(class = "author-popover-label", "How the 8,000 papers were selected"),
          div(
            class = "author-popover-value",
            paste0(
              "After applying the DOI and date criteria, a proportional stratified random sample of ",
              format(sampled_records, big.mark = ",", trim = TRUE),
              " papers was drawn across retraction years with reproducible random seed ",
              format(citation_metric("random_seed"), scientific = FALSE, trim = TRUE), "."
            )
          )
        ),
        div(
          class = "author-popover-section",
          div(class = "author-popover-label", "Why the sample is stratified"),
          div(
            class = "author-popover-value",
            "Retractions are distributed very unevenly over time: recent years contain many more records, and exceptional cohorts can create large spikes. A simple random sample could therefore be dominated by the largest years and contain very few papers from earlier years. Sampling separately within each retraction year, with the number selected approximately proportional to that year's share of eligible records, preserves the temporal composition of the source data while retaining representation from sparse years. This is proportional stratification, not an equal number of papers from every year."
          )
        ),
        div(
          class = "author-popover-section",
          div(class = "author-popover-label", "How duplicate DOI records are handled"),
          div(
            class = "author-popover-value",
            "Occasionally, more than one RWD row points to the same original-paper DOI. Because those rows refer to the same scholarly paper, counting every row would give that paper extra weight. They are therefore combined into one paper before sampling. If the duplicate rows contain different retraction years, the earliest recorded retraction year is used as the start of follow-up: it is the first year in which the paper is recorded as retracted and prevents later notices or record updates from shifting the citation window forward. This rule does not treat later rows as separate papers."
          )
        ),
        div(
          class = "author-popover-section",
          div(class = "author-popover-label", "OpenAlex matching and observation window"),
          div(
            class = "author-popover-value",
            paste0(
              format(matched_records, big.mark = ",", trim = TRUE),
              " sampled DOI records were matched and successfully processed in OpenAlex (",
              citation_metric("coverage_pct"), "%). Citation counts cover relative years −5 through +5. At negative years, a paper enters the denominator only after it has been published; all matched papers can contribute from the retraction year through year +5."
            )
          )
        ),
        div(
          class = "author-popover-section",
          div(class = "author-popover-label", "How the two analyses differ"),
          div(
            class = "author-popover-value",
            "The annual chart uses the available-paper denominator at each relative year. The total-citations chart instead uses one fixed cohort: papers already published by year −5, followed continuously through year +5. The post-retraction ranking sums citing works in years +1 to +5 and excludes year 0 because that calendar year includes time both before and after the retraction notice."
          )
        ),
        div(
          class = "author-popover-section",
          div(class = "author-popover-label", "Downloadable reproducibility data"),
          div(
            class = "author-popover-value",
            "The downloadable CSV contains one row for every sampled paper, including papers that did not match OpenAlex. It provides the title, authors, journal, publisher, DOI and OpenAlex links, publication and retraction years, sampling stratum and seed, fixed-cohort status, post-retraction total and Top 200 rank. For every relative year from −5 through +5 it also reports the corresponding calendar year, whether the paper was included in that year's denominator, and its citing-work count. A blank citation count means that the paper was not observable in that cell or was not matched; an observed zero is stored explicitly as 0."
          )
        ),
        div(
          class = "author-popover-section",
          div(class = "author-popover-label", "Limitations"),
          div(
            class = "author-popover-value",
            "This is not a causal estimate because there is no counterfactual comparison group showing how the same papers—or comparable non-retracted papers—would have been cited if no retraction had occurred. Retraction is not randomly assigned: its timing and likelihood are related to paper age, field, journal, reason for retraction, prior citation trajectory and public attention. Consequently, changes around year 0 may reflect normal citation ageing, cohort composition or publicity as well as the retraction itself. The retraction year is also measured as a calendar year, so year 0 mixes time before and after the exact notice date. Finally, the sample requires a DOI and OpenAlex match; OpenAlex coverage is incomplete and its citation histories can change when the index is updated. The charts therefore describe citation trajectories around retraction but cannot tell us how many citations the retraction caused or prevented."
          )
        )
      )),
      collapse = ""
    )

    div(
      class = "impact-strip row g-0",
      div(class = "impact-cell col-12 col-sm-6 col-lg-3",
          div(
            class = "impact-label impact-label-with-info",
            span("Total sample"),
            tags$button(
              type = "button",
              class = "author-info-button citation-sample-info-button",
              "i",
              `aria-label` = "How the citation-impact sample was selected",
              `data-modal-title` = "How the citation-impact sample was selected",
              `data-modal-content` = sample_modal_content
            )
          ),
          div(class = "impact-value", format(sampled_records, big.mark = ",")),
          div(
            class = "impact-note",
            paste0(format(matched_records, big.mark = ","), " matched to OpenAlex · ", citation_metric("coverage_pct"), "%")
          )
      ),
      div(class = "impact-cell col-12 col-sm-6 col-lg-3",
          div(class = "impact-label", "Papers observed at year −5"),
          div(class = "impact-value", format(year_minus_five$papers_in_denominator, big.mark = ",")),
          div(class = "impact-note", paste0(round(100 * year_minus_five$papers_in_denominator / matched_records, 1), "% of matched papers"))
      ),
      div(class = "impact-cell col-12 col-sm-6 col-lg-3",
          div(class = "impact-label", "Papers observed at year +5"),
          div(class = "impact-value", format(year_plus_five$papers_in_denominator, big.mark = ",")),
          div(class = "impact-note", "100% of matched papers")
      ),
      div(class = "impact-cell col-12 col-sm-6 col-lg-3",
          div(class = "impact-label", "Fixed −5 to +5 cohort"),
          div(class = "impact-value", format(fixed_window_records, big.mark = ",")),
          div(class = "impact-note", "Published by year −5 and observed through +5")
      )
    )
  })

  output$download_citation_reproducibility <- downloadHandler(
    filename = function() {
      paste0(
        "rwd-citation-impact-reproducibility-",
        citation_metric("retrieved_date", numeric = FALSE),
        ".csv"
      )
    },
    content = function(file) {
      file.copy("citation_impact_reproducibility.csv", file, overwrite = TRUE)
    },
    contentType = "text/csv"
  )

  output$citation_top_articles_table <- renderUI({
    req(nrow(citation_impact_top_articles) > 0)

    table_rows <- lapply(seq_len(nrow(citation_impact_top_articles)), function(index) {
      article <- citation_impact_top_articles[index, ]
      publication_year <- if (is.na(article$publication_year) || article$publication_year == "") {
        "Unknown"
      } else {
        format(as.integer(article$publication_year), scientific = FALSE, trim = TRUE)
      }
      retraction_year <- format(as.integer(article$retraction_year), scientific = FALSE, trim = TRUE)

      tags$tr(
        tags$td(class = "citation-top-number", format(as.integer(article$rank), big.mark = ",", trim = TRUE)),
        tags$td(
          tags$a(
            class = "citation-top-article-title",
            article$title,
            href = paste0("https://doi.org/", article$doi),
            target = "_blank",
            rel = "noopener noreferrer"
          ),
          div(class = "citation-top-article-meta", article$authors),
          div(
            class = "citation-top-article-meta",
            paste(c(article$journal, article$publisher)[c(article$journal, article$publisher) != ""], collapse = " · ")
          )
        ),
        tags$td(class = "citation-top-number", publication_year),
        tags$td(class = "citation-top-number", retraction_year),
        tags$td(
          class = "citation-top-number text-end",
          format(as.integer(article$post_retraction_citations), big.mark = ",", trim = TRUE)
        )
      )
    })

    tags$table(
      class = "table table-hover align-middle",
      tags$thead(
        tags$tr(
          tags$th(scope = "col", "Rank"),
          tags$th(scope = "col", "Article"),
          tags$th(scope = "col", "Publication year"),
          tags$th(scope = "col", "Retraction year"),
          tags$th(scope = "col", class = "text-end", "Citing works, years +1 to +5")
        )
      ),
      tags$tbody(table_rows)
    )
  })

  output$citation_impact_plot <- renderPlotly({
    df <- citation_impact_data %>%
      mutate(
        Tooltip = paste0(
          "<b>Relative Year:</b> ", ifelse(relative_year > 0, paste0("+", relative_year), relative_year),
          "<br><b>Mean:</b> ", format(round(mean_per_paper, 2), nsmall = 2),
          "<br><b>Median:</b> ", format(round(median_per_paper, 2), nsmall = 2),
          "<br><b>IQR:</b> ", format(round(q1_per_paper, 2), nsmall = 2),
          "–", format(round(q3_per_paper, 2), nsmall = 2),
          "<br><b>Citing Works:</b> ", format(citing_works, big.mark = ",", trim = TRUE),
          "<br><b>Papers in Denominator:</b> ", format(papers_in_denominator, big.mark = ",", trim = TRUE)
        )
      )

    p <- ggplot(df, aes(x = relative_year)) +
      geom_vline(xintercept = 0, color = "#b42532", linetype = "dashed", size = 0.65) +
      geom_linerange(
        aes(ymin = q1_per_paper, ymax = q3_per_paper, linetype = "IQR", text = Tooltip),
        color = "#9aa5b0", size = 2.1
      ) +
      geom_line(aes(y = median_per_paper, color = "Median", group = 1), size = 0.85) +
      geom_point(aes(y = median_per_paper, color = "Median", shape = "Median", text = Tooltip), size = 2.8) +
      geom_line(aes(y = mean_per_paper, color = "Mean", group = 1), size = 1.05) +
      geom_point(aes(y = mean_per_paper, color = "Mean", shape = "Mean", text = Tooltip), size = 3.1) +
      scale_color_manual(
        values = c("Median" = "#002147", "Mean" = "#b42532"),
        name = NULL
      ) +
      scale_shape_manual(values = c("Median" = 16, "Mean" = 17), name = NULL) +
      scale_linetype_manual(values = c("IQR" = "solid"), name = NULL) +
      scale_x_continuous(breaks = seq(-5, 5, by = 1)) +
      labs(x = "Years relative to retraction", y = "Annual citing works per paper") +
      owid_plot_theme() +
      guides(
        color = guide_legend(order = 1),
        shape = guide_legend(order = 1),
        linetype = guide_legend(order = 2, override.aes = list(color = "#9aa5b0", size = 1.7))
      ) +
      theme(legend.position = "top", legend.justification = "left")

    clean_lag_summary_legend(owid_plotly(p, tooltip = "text"))
  })

  output$citation_impact_total_plot <- renderPlotly({
    df <- citation_impact_total_data %>%
      mutate(
        Tooltip = paste0(
          "<b>Relative Year:</b> ", ifelse(relative_year > 0, paste0("+", relative_year), relative_year),
          "<br><b>Mean Total:</b> ", format(round(mean_total_per_paper, 2), nsmall = 2),
          "<br><b>Median Total:</b> ", format(round(median_total_per_paper, 2), nsmall = 2),
          "<br><b>IQR:</b> ", format(round(q1_total_per_paper, 2), nsmall = 2),
          "–", format(round(q3_total_per_paper, 2), nsmall = 2),
          "<br><b>Fixed Cohort:</b> ", format(papers_in_denominator, big.mark = ",", trim = TRUE)
        )
      )

    p <- ggplot(df, aes(x = relative_year)) +
      geom_vline(xintercept = 0, color = "#b42532", linetype = "dashed", size = 0.65) +
      geom_linerange(
        aes(ymin = q1_total_per_paper, ymax = q3_total_per_paper, linetype = "IQR", text = Tooltip),
        color = "#9aa5b0", size = 2.1
      ) +
      geom_line(aes(y = median_total_per_paper, color = "Median", group = 1), size = 0.85) +
      geom_point(aes(y = median_total_per_paper, color = "Median", shape = "Median", text = Tooltip), size = 2.8) +
      geom_line(aes(y = mean_total_per_paper, color = "Mean", group = 1), size = 1.05) +
      geom_point(aes(y = mean_total_per_paper, color = "Mean", shape = "Mean", text = Tooltip), size = 3.1) +
      scale_color_manual(values = c("Median" = "#002147", "Mean" = "#b42532"), name = NULL) +
      scale_shape_manual(values = c("Median" = 16, "Mean" = 17), name = NULL) +
      scale_linetype_manual(values = c("IQR" = "solid"), name = NULL) +
      scale_x_continuous(breaks = seq(-5, 5, by = 1)) +
      labs(
        x = "Years relative to retraction",
        y = "Total citing works per paper since year −5"
      ) +
      owid_plot_theme() +
      guides(
        color = guide_legend(order = 1),
        shape = guide_legend(order = 1),
        linetype = guide_legend(order = 2, override.aes = list(color = "#9aa5b0", size = 1.7))
      ) +
      theme(legend.position = "top", legend.justification = "left")

    clean_lag_summary_legend(owid_plotly(p, tooltip = "text"))
  })

  output$citation_impact_table <- renderTable({
    before <- citation_impact_data %>% filter(relative_year < 0)
    year_zero <- citation_impact_data %>% filter(relative_year == 0)
    after <- citation_impact_data %>% filter(relative_year > 0)

    data.frame(
      Period = c("5 years before", "Retraction year", "5 years after"),
      `Mean annual citing works per paper` = c(
        mean(before$mean_per_paper),
        year_zero$mean_per_paper,
        mean(after$mean_per_paper)
      ),
      `Papers in denominator` = c(
        paste0(format(min(before$papers_in_denominator), big.mark = ","), "–", format(max(before$papers_in_denominator), big.mark = ",")),
        format(year_zero$papers_in_denominator, big.mark = ","),
        format(max(after$papers_in_denominator), big.mark = ",")
      ),
      check.names = FALSE
    ) %>%
      mutate(`Mean annual citing works per paper` = format(round(`Mean annual citing works per paper`, 2), nsmall = 2))
  }, width = "100%", striped = TRUE, hover = TRUE, bordered = FALSE, align = "lrr")
  
  output$pub_outliers_note <- renderUI({
    req(retraction_data())
    outliers <- sum(retraction_data()$PubYear < 1990, na.rm = TRUE)
    div(
      class = "pub-year-note",
      if (outliers > 0) {
        span(
          class = "pub-year-note-primary",
          paste(outliers, "papers published before 1990 are excluded from this chart.")
        )
      },
      span(
        class = "pub-year-note-context",
        "The unusual increase in 2010 and 2011 is primarily driven by large-scale IEEE conference-paper retractions.",
        actionButton(
          "ieee_spike_info",
          "i",
          class = "ieee-spike-info-button",
          `aria-label` = "Detailed explanation and sources for the 2010 and 2011 IEEE spike",
          title = "Detailed explanation and sources"
        )
      ),
      span(
        class = "pub-year-note-context",
        "The 2022 publication peak and the 2023 retraction peak are mainly the same Hindawi cohort.",
        actionButton(
          "hindawi_spike_info",
          "i",
          class = "ieee-spike-info-button",
          `aria-label` = "Detailed explanation and sources for the 2022 and 2023 Hindawi peaks",
          title = "Detailed explanation and sources"
        )
      )
    )
  })

  observeEvent(input$ieee_spike_info, {
    req(ieee_spike_summary_data(), ieee_spike_conference_data())
    summary <- ieee_spike_summary_data()
    conferences <- ieee_spike_conference_data()
    req(nrow(summary) == 1)

    format_count <- function(value) {
      format(as.integer(value), big.mark = ",", scientific = FALSE, trim = TRUE)
    }
    format_share <- function(part, total) {
      paste0(format(round(100 * part / total, 1), nsmall = 1, trim = TRUE), "%")
    }

    year_rows <- lapply(c(2010, 2011), function(year) {
      pub_all <- summary[[paste0("Pub", year, "All")]]
      pub_ieee <- summary[[paste0("Pub", year, "IEEE")]]
      ret_all <- summary[[paste0("Ret", year, "All")]]
      ret_ieee <- summary[[paste0("Ret", year, "IEEE")]]

      tags$tr(
        tags$td(year),
        tags$td(class = "text-end", format_count(pub_all)),
        tags$td(class = "text-end", paste0(format_count(pub_ieee), " (", format_share(pub_ieee, pub_all), ")")),
        tags$td(class = "text-end", format_count(pub_all - pub_ieee)),
        tags$td(class = "text-end", format_count(ret_all)),
        tags$td(class = "text-end", paste0(format_count(ret_ieee), " (", format_share(ret_ieee, ret_all), ")")),
        tags$td(class = "text-end", format_count(ret_all - ret_ieee))
      )
    })

    conference_items <- lapply(seq_len(nrow(conferences)), function(index) {
      tags$li(
        tags$strong(conferences$Journal[[index]]),
        paste0(": ", format_count(conferences$Records[[index]]), " RWD records.")
      )
    })

    showModal(modalDialog(
      title = "Why do 2010 and 2011 look unusual?",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      div(
        class = "ieee-spike-modal-body",
        tags$p(
          "The two peaks are not evidence of a general, field-wide surge in retractions. They are dominated by a concentrated set of IEEE conference proceedings whose papers were removed or retracted in bulk."
        ),
        tags$h3("What the current RWD snapshot contains"),
        div(
          class = "ieee-spike-table-wrap",
          tags$table(
            class = "table table-striped ieee-spike-table",
            tags$thead(
              tags$tr(
                tags$th("Year"),
                tags$th(class = "text-end", "All publication records"),
                tags$th(class = "text-end", "IEEE publication records"),
                tags$th(class = "text-end", "Publication records without IEEE"),
                tags$th(class = "text-end", "All retraction-year records"),
                tags$th(class = "text-end", "IEEE retraction-year records"),
                tags$th(class = "text-end", "Retraction-year records without IEEE")
              )
            ),
            tags$tbody(year_rows)
          )
        ),
        tags$p(
          "Once IEEE is separated, the exceptional height of both years largely disappears. The concentration is not produced by duplicate RWD rows: the affected IEEE entries overwhelmingly represent distinct conference papers."
        ),
        tags$h3("What happened at IEEE"),
        tags$p(
          "Retraction Watch reported that IEEE had removed thousands of conference papers after concerns about whether some conference organizers had followed adequate peer-review and technical-program procedures. IEEE said that it had launched an in-depth investigation through its Technical Program Integrity Committee in 2010 after detecting inconsistencies in conference quality control."
        ),
        tags$p(
          "The action affected whole or substantial parts of conference proceedings rather than a random collection of unrelated journal articles. The individual notices generally supplied little information beyond a violation of IEEE publication principles. The available documentation does not consistently distinguish between two materially different situations:"
        ),
        tags$ol(
          tags$li(
            tags$strong("Post-publication retraction: "),
            "the paper had already been published in IEEE Xplore and was part of the citable scholarly record before IEEE withdrew it. The paper should remain identifiable as retracted, its citations before and after the notice can be studied, and the publication-to-retraction interval is a meaningful quantity if the notice date is known."
          ),
          tags$li(
            tags$strong("Cancelled or prevented publication: "),
            "IEEE had agreed to publish the conference proceedings, but the paper was never made available as a normal IEEE Xplore publication, or the proceedings were stopped before publication was completed. In that case there may be no genuine post-publication period, no stable citable version and no meaningful publication-to-retraction interval. Counting it as a conventional retraction can inflate both annual retraction totals and the number of zero- or one-year lags."
          )
        ),
        tags$p(
          "Both situations document an IEEE quality-control action, but they are not bibliometrically equivalent. Because the source records do not let us separate them reliably paper by paper, the dashboard retains the RWD classification while flagging the 2010–2011 peak as a special case."
        ),
        tags$h3("Largest conference groups represented"),
        tags$ul(conference_items),
        tags$h3("Why the retraction-year peak must be interpreted cautiously"),
        tags$p(
          "Within the ", format_count(summary$IEEESpikeRecords),
          " IEEE records contributing to the 2010–2011 anomaly, ",
          format_count(summary$EstimatedRetractionDateRecords),
          " explicitly state in the RWD notes that the retraction date was unknown and estimated from the conference date; ",
          format_count(summary$UnknownDateLabelRecords),
          " carry the label ‘Date of Article and/or Notice Unknown’; and ",
          format_count(summary$LimitedInformationRecords),
          " carry ‘Notice – Limited or No Information’. These indicators overlap and therefore should not be added together."
        ),
        div(
          class = "ieee-spike-caution",
          tags$strong("Interpretation: "),
          "the publication-year peak genuinely identifies a very large cohort of 2010–2011 IEEE conference papers. The retraction-year peak is not an equally reliable chronology of when IEEE made every removal decision, because many notice dates are missing, estimated or reconstructed. It should not be interpreted as evidence that thousands of independent acts of author misconduct suddenly occurred in those two years."
        ),
        tags$h3("Sources"),
        tags$ol(
          tags$li(
            tags$a(
              "Retraction Watch: One publisher appears to have retracted thousands of meeting abstracts. Yes, thousands. (June 25, 2015)",
              href = "https://retractionwatch.com/2015/06/25/one-publisher-appears-to-have-retracted-thousands-of-meeting-abstracts-yes-thousands/",
              target = "_blank",
              rel = "noopener noreferrer"
            ),
            " — documents the bulk IEEE action, the major affected conferences, IEEE's explanation and uncertainty about the exact number and timing."
          ),
          tags$li(
            tags$a(
              "Retraction Watch Database User Guide",
              href = "https://retractionwatch.com/retraction-watch-database-user-guide/",
              target = "_blank",
              rel = "noopener noreferrer"
            ),
            " — warns that overwritten pages and unavailable notice dates can make date-only analyses inherently uncertain."
          )
        )
      )
    ))
  })

  observeEvent(input$hindawi_spike_info, {
    req(retraction_data())

    records <- retraction_data() %>%
      mutate(
        IsRetraction = str_detect(RetractionNature, regex("Retraction", ignore_case = TRUE)),
        IsHindawi = str_detect(coalesce(Publisher, ""), regex("Hindawi", ignore_case = TRUE))
      ) %>%
      filter(IsRetraction)

    summary_rows <- data.frame(
      Cohort = c(
        "Retracted papers published in 2022",
        "Papers retracted in 2023",
        "Published in 2022 and retracted in 2023"
      ),
      AllRecords = c(
        sum(records$PubYear == 2022, na.rm = TRUE),
        sum(records$RetYear == 2023, na.rm = TRUE),
        sum(records$PubYear == 2022 & records$RetYear == 2023, na.rm = TRUE)
      ),
      HindawiRecords = c(
        sum(records$PubYear == 2022 & records$IsHindawi, na.rm = TRUE),
        sum(records$RetYear == 2023 & records$IsHindawi, na.rm = TRUE),
        sum(records$PubYear == 2022 & records$RetYear == 2023 & records$IsHindawi, na.rm = TRUE)
      ),
      stringsAsFactors = FALSE
    )

    format_count <- function(value) {
      format(as.integer(value), big.mark = ",", scientific = FALSE, trim = TRUE)
    }
    format_share <- function(part, total) {
      if (is.na(total) || total == 0) return("not available")
      paste0(format(round(100 * part / total, 1), nsmall = 1, trim = TRUE), "%")
    }

    cohort_rows <- lapply(seq_len(nrow(summary_rows)), function(index) {
      row <- summary_rows[index, ]
      tags$tr(
        tags$td(row$Cohort),
        tags$td(class = "text-end", format_count(row$AllRecords)),
        tags$td(
          class = "text-end",
          paste0(
            format_count(row$HindawiRecords),
            " (", format_share(row$HindawiRecords, row$AllRecords), ")"
          )
        ),
        tags$td(class = "text-end", format_count(row$AllRecords - row$HindawiRecords))
      )
    })

    published_2022 <- summary_rows$AllRecords[[1]]
    retracted_2023 <- summary_rows$AllRecords[[2]]
    linked_cohort <- summary_rows$AllRecords[[3]]
    linked_hindawi <- summary_rows$HindawiRecords[[3]]

    showModal(modalDialog(
      title = "Why do 2022 and 2023 look unusual?",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      div(
        class = "ieee-spike-modal-body",
        tags$p(
          "These are not two independent anomalies. They largely represent one concentrated cohort: papers published in Hindawi journals in 2022 and retracted at scale in 2023."
        ),
        tags$h3("What the current RWD snapshot contains"),
        div(
          class = "ieee-spike-table-wrap",
          tags$table(
            class = "table table-striped ieee-spike-table",
            tags$thead(
              tags$tr(
                tags$th("Cohort"),
                tags$th(class = "text-end", "All retraction records"),
                tags$th(class = "text-end", "Hindawi records"),
                tags$th(class = "text-end", "Records without Hindawi")
              )
            ),
            tags$tbody(cohort_rows)
          )
        ),
        tags$p(
          format_count(linked_cohort), " records connect the two peaks directly: they were published in 2022 and retracted in 2023. This cohort represents ",
          tags$strong(format_share(linked_cohort, retracted_2023)),
          " of the papers retracted in 2023 and ",
          tags$strong(format_share(linked_cohort, published_2022)),
          " of the retracted papers published in 2022. Of these linked records, ",
          tags$strong(format_share(linked_hindawi, linked_cohort)),
          " were published by Hindawi."
        ),
        tags$h3("What happened at Hindawi"),
        tags$p(
          "Hindawi's special-issue programme expanded rapidly. Special issues are managed for a defined topic, often with guest editors, and the model was targeted by paper mills and other actors able to manipulate editor or reviewer identities, compromise peer review and submit fabricated or unreliable content at scale."
        ),
        tags$p(
          "Hindawi announced an initial batch of roughly 500 retractions in September 2022. Wiley and Hindawi then paused special-issue publishing from October 2022 while reassessing manuscripts and strengthening checks on editors, authors and reviewers. During 2023, the investigation became an industrial-scale correction exercise and more than 8,000 Hindawi articles were retracted."
        ),
        tags$h3("Why the annual bars are so sharp"),
        tags$p(
          "The underlying papers were heavily concentrated in the 2022 publication cohort, and the resulting notices were issued in large batches during 2023. The bars therefore reflect both a genuine short publication-to-retraction interval and the publisher's batch-processing timetable; they do not represent thousands of unrelated cases discovered independently on the same dates."
        ),
        div(
          class = "ieee-spike-caution",
          tags$strong("Interpretation: "),
          "the publication-year chart includes only papers that eventually entered RWD, not all scholarly papers published in each year. The 2022 peak therefore does not indicate a general peak in global research output. More recent publication years are also right-censored because their papers have had less time to be investigated and retracted. Finally, a systematic-manipulation notice does not by itself prove that every listed author knowingly participated; Wiley notices commonly state that author awareness could not be established."
        ),
        tags$h3("Sources"),
        tags$ol(
          tags$li(
            tags$a(
              "Retraction Watch: Wiley and Hindawi to retract 1,200 more papers for compromised peer review (April 5, 2023)",
              href = "https://retractionwatch.com/2023/04/05/wiley-and-hindawi-to-retract-1200-more-papers-for-compromised-peer-review/",
              target = "_blank",
              rel = "noopener noreferrer"
            ),
            " — describes the special issues targeted by paper mills, manipulated identities, fabricated content and the publishing pause."
          ),
          tags$li(
            tags$a(
              "Retraction Watch: Hindawi reveals process for retracting more than 8,000 paper mill articles (December 19, 2023)",
              href = "https://retractionwatch.com/2023/12/19/hindawi-reveals-process-for-retracting-more-than-8000-paper-mill-articles/",
              target = "_blank",
              rel = "noopener noreferrer"
            ),
            " — documents the scale of the 2023 retractions and Hindawi's investigation and batch-retraction process."
          ),
          tags$li(
            tags$a(
              "Wiley 2024 annual report filed with the U.S. Securities and Exchange Commission",
              href = "https://www.sec.gov/Archives/edgar/data/107140/000010714024000114/jwa-20240430.htm",
              target = "_blank",
              rel = "noopener noreferrer"
            ),
            " — confirms that the special-issue programme was suspended because compromised articles were present."
          ),
          tags$li(
            tags$a(
              "Example Wiley/Hindawi systematic-manipulation retraction notice",
              href = "https://onlinelibrary.wiley.com/doi/10.1155/2023/9875676",
              target = "_blank",
              rel = "noopener noreferrer"
            ),
            " — illustrates the integrity indicators examined and the distinction between process manipulation and proven author awareness."
          )
        )
      )
    ))
  })
  
  output$pub_year_plot <- renderPlotly({
    req(retraction_data())
    df <- retraction_data() %>%
      filter(!is.na(PubYear), PubYear >= 1990) %>%
      count(PubYear, name = "Count") %>%
      mutate(Tooltip = paste0(
        "<b>Publication Year:</b> ", PubYear,
        "<br><b>Count:</b> ", format(Count, big.mark = ",", trim = TRUE)
      ))
    
    p <- ggplot(df, aes(x = PubYear, y = Count, text = Tooltip)) +
      geom_col(width = 0.9, fill = "#4c6a9c", color = "#ffffff", size = 0.25) +
      scale_x_continuous(
        breaks = seq(1990, 2030, by = 5),
        limits = c(1990, NA),
        expand = expansion(mult = c(0, 0.02))
      ) +
      labs(
        x = "Year of publication",
        y = "Number of records"
      ) +
      owid_plot_theme()
    owid_plotly(p, tooltip = "text")
  })
  
  output$retraction_outliers_text <- renderText({
    req(retraction_data())
    outliers <- sum(retraction_data()$RetYear < 1995, na.rm = TRUE)
    if (outliers > 0) {
      paste(outliers, "retractions issued before 1995 are excluded from this chart.")
    } else {
      ""
    }
  })
  
  output$retraction_year_plot <- renderPlotly({
    req(retraction_data())
    df <- retraction_data() %>%
      filter(!is.na(RetYear), RetYear >= 1995) %>%
      count(RetYear, name = "Count") %>%
      mutate(Tooltip = paste0(
        "<b>Retraction Year:</b> ", RetYear,
        "<br><b>Count:</b> ", format(Count, big.mark = ",", trim = TRUE)
      ))
    
    p <- ggplot(df, aes(x = RetYear, y = Count, text = Tooltip)) +
      geom_col(width = 0.9, fill = "#b42532", color = "#ffffff", size = 0.25) +
      scale_x_continuous(
        breaks = seq(1995, 2030, by = 5),
        limits = c(1995, NA),
        expand = expansion(mult = c(0, 0.02))
      ) +
      labs(
        x = "Year of retraction",
        y = "Number of records"
      ) +
      owid_plot_theme()
    owid_plotly(p, tooltip = "text")
  })

  output$diff_outliers_text <- renderText({
    req(lag_paper_data())
    outliers <- sum(lag_paper_data()$DiffYear > 30, na.rm = TRUE)
    if (outliers > 0) {
      paste(outliers, "unique retracted papers with a lag longer than 30 years are excluded from the chart, but included in the summary statistics.")
    } else {
      ""
    }
  })
  
  output$diff_dist_plot <- renderPlotly({
    req(lag_paper_data())
    lag_distribution_plot(lag_paper_data())
  })
  
  output$diff_stats <- renderUI({
    req(lag_paper_data())
    lag_stats_component(lag_paper_data())
  })

  sensitivity_lag_data <- reactive({
    req(lag_paper_data())
    df <- lag_paper_data()
    req("IsIEEESpike" %in% names(df))
    df %>% filter(!coalesce(IsIEEESpike, FALSE))
  })

  output$diff_sensitivity_plot <- renderPlotly({
    req(sensitivity_lag_data())
    lag_distribution_plot(sensitivity_lag_data())
  })

  output$diff_sensitivity_stats <- renderUI({
    req(sensitivity_lag_data())
    lag_stats_component(sensitivity_lag_data())
  })

  output$diff_sensitivity_summary <- renderUI({
    req(lag_paper_data(), sensitivity_lag_data())
    full <- lag_paper_data()
    sensitivity <- sensitivity_lag_data()

    excluded <- nrow(full) - nrow(sensitivity)
    excluded_share <- 100 * excluded / nrow(full)
    full_mean <- mean(full$DiffYear, na.rm = TRUE)
    sensitivity_mean <- mean(sensitivity$DiffYear, na.rm = TRUE)
    full_median <- median(full$DiffYear, na.rm = TRUE)
    sensitivity_median <- median(sensitivity$DiffYear, na.rm = TRUE)
    outliers <- sum(sensitivity$DiffYear > 30, na.rm = TRUE)

    format_count <- function(value) {
      format(as.integer(value), big.mark = ",", scientific = FALSE, trim = TRUE)
    }
    format_years <- function(value) {
      format(round(value, 1), nsmall = 1, trim = TRUE)
    }

    div(
      class = "lag-sensitivity-summary",
      tags$p(
        tags$strong("Sample change: "),
        format_count(excluded), " of ", format_count(nrow(full)),
        " unique retracted papers are excluded (",
        format(round(excluded_share, 1), nsmall = 1, trim = TRUE),
        "%). The sensitivity sample contains ", format_count(nrow(sensitivity)), " papers."
      ),
      tags$p(
        tags$strong("Result: "),
        "the mean lag changes from ", format_years(full_mean), " to ",
        format_years(sensitivity_mean), " years, while the median changes from ",
        format_years(full_median), " to ", format_years(sensitivity_median),
        " years. This shows how the unusually short IEEE lags affect the full-sample distribution."
      ),
      if (outliers > 0) {
        tags$p(
          format_count(outliers),
          " sensitivity-sample papers with a lag longer than 30 years are excluded from the chart but retained in the summary statistics."
        )
      }
    )
  })
  
  output$world_map_plot <- renderPlotly({
    req(country_data())
    rank_metric <- if (is.null(input$country_rank_metric)) "total" else input$country_rank_metric
    show_rate <- identical(rank_metric, "rate")

    world <- map_data("world")

    map_data_joined <- world %>%
      left_join(country_data(), by = c("region" = "MapCountry")) %>%
      mutate(
        RetractionsLabel = ifelse(
          is.na(Retractions),
          "Not available",
          format(Retractions, big.mark = ",", scientific = FALSE, trim = TRUE)
        ),
        RateLabel = ifelse(
          is.na(RetractionsPer10000),
          "Not available",
          format(round(RetractionsPer10000, 2), nsmall = 2, trim = TRUE)
        ),
        OpenAlexWorksLabel = ifelse(
          is.na(OpenAlexWorks),
          "Not available",
          format(OpenAlexWorks, big.mark = ",", scientific = FALSE, trim = TRUE)
        ),
        MapMetric = if (show_rate) RetractionsPer10000 else Retractions,
        MapTooltip = if (show_rate) {
          paste(
            "Country:", region,
            "<br>Per 10,000 OpenAlex works:", RateLabel,
            "<br>Unique retracted papers:", RetractionsLabel,
            "<br>OpenAlex works:", OpenAlexWorksLabel
          )
        } else {
          paste(
            "Country:", region,
            "<br>Unique retracted papers:", RetractionsLabel,
            "<br>Per 10,000 OpenAlex works:", RateLabel
          )
        }
      ) %>%
      arrange(group, order)

    p <- ggplot(
      map_data_joined,
      aes(
        x = long,
        y = lat,
        group = group,
        fill = MapMetric,
        text = MapTooltip
      )
    ) +
      geom_polygon(color = "#ffffff", size = 0.22)

    if (show_rate) {
      p <- p + scale_fill_gradientn(
        colors = c("#e5ebf1", "#b6c8da", "#7f9fbe", "#4c6a9c", "#002147"),
        na.value = "#e7e3dc",
        name = "Per 10,000 OpenAlex works"
      )
      colorbar_title <- "Per 10,000<br>OpenAlex works"
    } else {
      p <- p + scale_fill_gradientn(
        colors = c("#e5ebf1", "#b6c8da", "#7f9fbe", "#4c6a9c", "#002147"),
        na.value = "#e7e3dc",
        trans = "log10",
        name = "Unique retracted papers",
        breaks = c(1, 10, 100, 1000, 10000)
      )
      colorbar_title <- "Retracted papers<br>(log scale)"
    }

    p <- p +
      theme_void(base_family = "sans", base_size = 12) +
      theme(
        legend.position = "right",
        legend.title = element_text(color = "#303842", face = "bold"),
        legend.text = element_text(color = "#5b6670"),
        plot.background = element_rect(fill = "#ffffff", color = NA),
        plot.margin = margin(10, 10, 10, 10)
      ) +
      coord_fixed(1.3)

    ggplotly(p, tooltip = "text") %>%
      style(colorbar = list(len = 0.5, thickness = 13, title = colorbar_title, outlinewidth = 0)) %>%
      layout(
        font = list(family = "Lato", color = "#303842"),
        paper_bgcolor = "#ffffff",
        hoverlabel = list(bgcolor = "#002147", bordercolor = "#002147", font = list(color = "#ffffff", family = "Lato"))
      ) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"))
  })

  output$world_map_note <- renderUI({
    rank_metric <- if (is.null(input$country_rank_metric)) "total" else input$country_rank_metric

    if (identical(rank_metric, "rate")) {
      tagList(
        "The map shows unique retracted papers ÷ OpenAlex works × 10,000. Countries without a valid OpenAlex denominator are shown as unavailable; the rate uses a linear colour scale. ",
        "A paper can count in every country listed in its RWD affiliation data. This is a descriptive coverage rate, not a country-risk estimate."
      )
    } else {
      tagList(
        "A paper is counted once in every country listed in its RWD affiliation data. The log10 colour scale keeps countries with smaller totals visible. ",
        "Use ‘Rank table and map by’ to switch to the publication-normalized rate."
      )
    }
  })
  
  output$top_countries_table <- renderTable({
    req(country_data())
    rank_metric <- if (is.null(input$country_rank_metric)) "total" else input$country_rank_metric

    ranked_countries <- country_data() %>%
      filter(!is.na(Retractions), Retractions > 0)

    if (identical(rank_metric, "rate")) {
      ranked_countries <- ranked_countries %>%
        filter(!is.na(RetractionsPer10000), OpenAlexWorks > 0) %>%
        arrange(desc(RetractionsPer10000), desc(Retractions), Country)
    } else {
      ranked_countries <- ranked_countries %>%
        arrange(desc(Retractions), desc(RetractionsPer10000), Country)
    }

    ranked_countries %>%
      slice_head(n = 15) %>%
      transmute(
        Country,
        `Retracted papers` = as.integer(Retractions),
        `Per 10,000 OpenAlex works` = round(RetractionsPer10000, 2)
      )
  }, width = "100%", striped = TRUE, hover = TRUE, bordered = FALSE, align = "lrr")
  
  output$top_subjects_table <- renderTable({
    req(subject_data())
    subject_data() %>%
      mutate(Retractions = as.integer(Retractions))
  }, width = "100%", striped = TRUE, hover = TRUE, bordered = FALSE, align = "lr")
  
  output$top_publishers_table <- renderTable({
    req(publisher_data())
    rank_metric <- if (is.null(input$publisher_rank_metric)) "total" else input$publisher_rank_metric

    ranked_publishers <- publisher_data() %>%
      filter(!is.na(Retractions), Retractions > 0)

    if (identical(rank_metric, "rate")) {
      ranked_publishers <- ranked_publishers %>%
        filter(!is.na(RetractionsPer10000), OpenAlexWorks > 0) %>%
        arrange(desc(RetractionsPer10000), desc(Retractions), PublisherGroup)
    } else {
      ranked_publishers <- ranked_publishers %>%
        arrange(desc(Retractions), desc(RetractionsPer10000), PublisherGroup)
    }

    ranked_publishers %>%
      slice_head(n = 10) %>%
      transmute(
        Publisher = PublisherGroup,
        `Retracted papers` = as.integer(Retractions),
        `Per 10,000 OpenAlex works` = round(RetractionsPer10000, 2)
      )
  }, width = "100%", striped = TRUE, hover = TRUE, bordered = FALSE, align = "lrr")
  
  output$substantive_reason_categories_table <- renderTable({
    req(reason_classification_data())
    reason_classification_data() %>%
      filter(ClassificationRole == "Substantive category") %>%
      arrange(CategoryOrder) %>%
      transmute(
        Category,
        `Unique retracted papers` = format(
          as.integer(UniqueRetractedPapers),
          big.mark = ",",
          scientific = FALSE,
          trim = TRUE
        ),
        `Share of retracted papers` = paste0(format(round(SharePct, 2), nsmall = 2), "%")
      )
  }, width = "100%", striped = TRUE, hover = TRUE, bordered = FALSE, align = "lrr")

  output$reason_pathways_table <- renderTable({
    req(reason_classification_data())
    reason_classification_data() %>%
      filter(ClassificationRole == "Pathway / context") %>%
      arrange(CategoryOrder) %>%
      transmute(
        `Pathway / context` = Category,
        `Unique retracted papers` = format(
          as.integer(UniqueRetractedPapers),
          big.mark = ",",
          scientific = FALSE,
          trim = TRUE
        ),
        `Share of retracted papers` = paste0(format(round(SharePct, 2), nsmall = 2), "%")
      )
  }, width = "100%", striped = TRUE, hover = TRUE, bordered = FALSE, align = "lrr")

  output$reason_classification_definitions <- renderUI({
    req(reason_data())
    category_block <- function(category) {
      div(
        class = "reason-definition",
        div(
          span(class = "reason-definition-title", category),
          span(class = "reason-definition-role", reason_category_types[[category]])
        ),
        div(
          class = "reason-definition-description",
          reason_category_descriptions[[category]]
        ),
        div(
          class = "reason-definition-labels",
          tags$strong("RWD labels: "),
          paste(reason_category_definitions[[category]], collapse = "; ")
        )
      )
    }
    substantive_categories <- names(reason_category_types)[
      reason_category_types == "Substantive category"
    ]
    pathway_categories <- names(reason_category_types)[
      reason_category_types == "Pathway / context"
    ]
    mapped_reason_labels <- unique(unlist(
      reason_category_definitions,
      use.names = FALSE
    ))
    unclassified_labels <- reason_data() %>%
      filter(
        !Reason %in% mapped_reason_labels,
        !Reason %in% reason_procedural_labels
      ) %>%
      arrange(desc(UniqueRetractedPapers), Reason) %>%
      transmute(
        LabelWithCount = paste0(
          Reason,
          " (",
          format(UniqueRetractedPapers, big.mark = ",", scientific = FALSE, trim = TRUE),
          ")"
        )
      ) %>%
      pull(LabelWithCount)

    tags$details(
      class = "reason-method",
      tags$summary("How the categories are defined"),
      div(
        class = "reason-method-body",
        div(class = "reason-definition-group-title", "Substantive categories"),
        lapply(substantive_categories, category_block),
        div(class = "reason-definition-group-title", "Pathways and context"),
        lapply(pathway_categories, category_block),
        div(
          class = "reason-method-exclusions",
          tags$strong("Excluded procedural and notice-status labels: "),
          paste(reason_procedural_labels, collapse = "; "),
          ". These labels are not counted in either group because they primarily describe an investigation by the journal, the status of the notice or the availability of information. ",
          "Categories are not mutually exclusive because one paper can carry several RWD labels, but each raw label is assigned to only one category in this taxonomy."
        ),
        div(
          class = "reason-method-unclassified",
          tags$strong("RWD labels not assigned to this taxonomy: "),
          paste(unclassified_labels, collapse = "; "),
          ". They remain visible rather than being forced into a catch-all category. Some describe potentially substantive issues that are too heterogeneous for the present categories; others concern author responses, third parties, legal proceedings or notice processes."
        )
      )
    )
  })

  output$reason_classification_note <- renderUI({
    req(reason_classification_summary_data())
    summary <- reason_classification_summary_data()
    req(nrow(summary) == 1)

    tagList(
      div(
        "Because one paper can carry several RWD labels, categories overlap and percentages do not sum to 100%. ",
        format(summary$PapersInSubstantiveCategories, big.mark = ","), " of ",
        format(summary$TotalUniqueRetractedPapers, big.mark = ","), " unique retracted papers (",
        format(round(summary$SubstantiveCoveragePct, 1), nsmall = 1),
        "%) have at least one label in the ten substantive categories."
      ),
      div(
        style = "margin-top: 8px;",
        format(summary$PapersWithUnclassifiedLabels, big.mark = ","), " papers (",
        format(round(summary$UnclassifiedLabelPct, 1), nsmall = 1),
        "%) carry at least one RWD label that is neither assigned to the taxonomy nor in the explicit exclusion list; ",
        format(summary$UnclassifiedWithoutSubstantiveCategory, big.mark = ","), " (",
        format(round(summary$UnclassifiedWithoutSubstantivePct, 1), nsmall = 1),
        "%) have one of these unclassified labels but no mapped substantive category."
      ),
      div(
        style = "margin-top: 8px;",
        "The pathways are reported separately. Formal investigation / misconduct finding appears in ",
        format(summary$FormalInvestigationPapers, big.mark = ","), " papers, and ",
        format(round(summary$FormalWithSubstantivePct, 1), nsmall = 1),
        "% of them also have a substantive category. Editorial / publisher process or error appears in ",
        format(summary$EditorialPathwayPapers, big.mark = ","), " papers, and ",
        format(round(summary$EditorialWithSubstantivePct, 1), nsmall = 1),
        "% also have a substantive category. Neutral labels are used deliberately: ",
        "“Data & image integrity concerns” and “Reported errors” do not infer intent beyond the underlying RWD labels."
      )
    )
  })
  
  output$top_authors_table <- renderUI({
    if (author_identity_available) {
      req(resolved_author_data())
      df <- resolved_author_data() %>%
        head(35) %>%
        mutate(
          rank = as.integer(rank),
          unique_retracted_papers = as.integer(unique_retracted_papers),
          openalex_works = as.integer(openalex_works),
          retracted_papers_per_100_works = round(retracted_papers_per_100_works, 2),
          first_retraction_year = as.integer(first_retraction_year),
          last_retraction_year = as.integer(last_retraction_year),
          median_retraction_lag_years = round(median_retraction_lag_years, 1)
        )

      format_integer <- function(value) {
        if (is.na(value)) "N/A" else format(as.integer(value), big.mark = ",", scientific = FALSE, trim = TRUE)
      }
      format_decimal <- function(value, digits) {
        if (is.na(value)) "N/A" else format(round(value, digits), nsmall = digits, big.mark = ",", trim = TRUE)
      }
      format_year <- function(value) {
        if (is.na(value)) "N/A" else format(as.integer(value), scientific = FALSE, trim = TRUE)
      }

      headers <- c(
        "Rank", "OpenAlex author", "Unique retracted papers", "OpenAlex works",
        "Retracted papers per 100 OpenAlex works", "First retraction year",
        "Last retraction year", "Median retraction lag (years)"
      )

      rows <- lapply(seq_len(nrow(df)), function(index) {
        row <- df[index, , drop = FALSE]
        status_class <- paste(
          "author-popover-status",
          if (row$match_confidence == "Review") "review" else ""
        )
        confidence_explanation <- if (
          "match_confidence_explanation" %in% names(row) &&
          !is.na(row$match_confidence_explanation) &&
          nzchar(row$match_confidence_explanation)
        ) row$match_confidence_explanation else row$review_evidence
        decision_explanation <- if (
          "identity_decision_explanation" %in% names(row) &&
          !is.na(row$identity_decision_explanation) &&
          nzchar(row$identity_decision_explanation)
        ) row$identity_decision_explanation else row$review_evidence

        modal_content <- div(
          div(
            class = "author-popover-section",
            div(class = "author-popover-label", "RWD name variants"),
            div(class = "author-popover-value", row$rwd_name_variants)
          ),
          div(
            class = "author-popover-section",
            div(class = "author-popover-label", "Match confidence"),
            div(
              class = "author-popover-value",
              span(class = status_class, row$match_confidence),
              confidence_explanation
            )
          ),
          div(
            class = "author-popover-section",
            div(class = "author-popover-label", "Identity decision"),
            div(
              class = "author-popover-value",
              tags$strong(row$identity_decision), tags$br(), decision_explanation
            )
          ),
          div(
            class = "author-popover-section",
            div(class = "author-popover-label", "Evidence used"),
            div(class = "author-popover-value", row$review_evidence)
          ),
          div(
            class = "author-popover-section",
            div(class = "author-popover-label", "Match basis"),
            div(
              class = "author-popover-value",
              paste0(
                format_integer(row$exact_match_papers), " exact DOI-name matches · ",
                format_integer(row$initial_match_papers), " initial-compatible matches. ",
                "OpenAlex profiles represented: ", format_integer(row$raw_openalex_profile_count), "."
              ),
              tags$br(), tags$br(),
              tags$strong("Exact DOI-name match: "),
              "the DOI connects the RWD record to an OpenAlex work, and the normalized full RWD name appears in that work's author list. Normalization ignores capitalization, accents and punctuation, and standardizes names written as ‘surname, given name’.",
              tags$br(), tags$br(),
              tags$strong("Initial-compatible match: "),
              "there is no exact full-name match on the DOI-linked work, but exactly one authorship has the same surname and compatible given-name initials. It is retained only after the identity review checks supporting profile, coauthor or institution evidence; a work with multiple compatible candidates is excluded.",
              tags$br(), tags$br(),
              "The two numbers count unique papers, not name variants or OpenAlex profiles. Together they equal the unique retracted papers assigned to this resolved identity. An exact name match is strong paper-level evidence, but is not by itself proof that two homonymous profiles are the same person.",
              tags$br(),
              paste0("Profile IDs: ", row$merged_openalex_ids),
              tags$br(),
              paste0("Works denominator: ", row$works_count_method, ".")
            )
          )
        )

        tags$tr(
          tags$td(class = "text-end", format_integer(row$rank)),
          tags$td(
            div(
              class = "author-name-with-info",
              span(row$author),
              tags$button(
                type = "button",
                class = "author-info-button",
                `aria-label` = paste("Open identity details for", row$author),
                `data-modal-title` = paste("Identity details ·", row$author),
                `data-modal-content` = as.character(modal_content),
                "i"
              )
            )
          ),
          tags$td(class = "text-end", format_integer(row$unique_retracted_papers)),
          tags$td(class = "text-end", format_integer(row$openalex_works)),
          tags$td(class = "text-end", format_decimal(row$retracted_papers_per_100_works, 2)),
          tags$td(class = "text-end", format_year(row$first_retraction_year)),
          tags$td(class = "text-end", format_year(row$last_retraction_year)),
          tags$td(class = "text-end", format_decimal(row$median_retraction_lag_years, 1))
        )
      })

      tags$table(
        class = "table table-striped table-hover",
        tags$thead(tags$tr(lapply(headers, function(label) tags$th(scope = "col", label)))),
        tags$tbody(rows)
      )
    } else {
      req(author_data())
      df <- author_data() %>%
        head(35) %>%
        mutate(
          UniqueRetractedPapers = as.integer(UniqueRetractedPapers),
          FirstRetractionYear = as.integer(FirstRetractionYear),
          LastRetractionYear = as.integer(LastRetractionYear),
          MedianLagYears = round(MedianLagYears, 1)
        ) %>%
        select(Rank, Author, UniqueRetractedPapers, FirstRetractionYear, LastRetractionYear, MedianLagYears) %>%
        rename(
          `Unique retracted papers` = UniqueRetractedPapers,
          `First retraction year` = FirstRetractionYear,
          `Last retraction year` = LastRetractionYear,
          `Median retraction lag (years)` = MedianLagYears
        )

      tags$table(
        class = "table table-striped table-hover",
        tags$thead(tags$tr(lapply(names(df), function(label) tags$th(scope = "col", label)))),
        tags$tbody(lapply(seq_len(nrow(df)), function(index) {
          tags$tr(lapply(df[index, , drop = TRUE], tags$td))
        }))
      )
    }
  })

  output$leaderboard_profile_stats <- renderUI({
    if (author_identity_available) {
      req(input$leaderboard_author, resolved_author_data())
      selected <- resolved_author_data() %>% filter(openalex_author_id == input$leaderboard_author)
      req(nrow(selected) == 1)

      format_year <- function(value) {
        if (is.na(value)) "N/A" else format(as.integer(value), scientific = FALSE, trim = TRUE)
      }
      median_lag <- if (is.na(selected$median_retraction_lag_years)) {
        "N/A"
      } else {
        paste0(format(round(selected$median_retraction_lag_years, 1), nsmall = 1, trim = TRUE), " yrs")
      }
      normalized_rate <- if (is.na(selected$retracted_papers_per_100_works)) {
        "N/A"
      } else {
        format(round(selected$retracted_papers_per_100_works, 2), nsmall = 2, trim = TRUE)
      }
      confidence_class <- paste("confidence-badge", tolower(selected$match_confidence))
      merged_ids <- trimws(unlist(strsplit(selected$merged_openalex_ids, ";", fixed = TRUE)))
      merged_ids <- merged_ids[merged_ids != ""]
      merged_id_links <- tagList(lapply(seq_along(merged_ids), function(index) {
        tagList(
          if (index > 1) " · ",
          tags$a(
            merged_ids[[index]],
            href = paste0("https://openalex.org/", merged_ids[[index]]),
            target = "_blank",
            rel = "noopener noreferrer"
          )
        )
      }))
      review_note_class <- paste(
        "identity-review-note",
        if (selected$match_confidence == "Review") "review" else ""
      )

      tagList(
        div(
          class = "identity-meta",
          strong("OpenAlex ID: "),
          tags$a(
            selected$openalex_author_id,
            href = paste0("https://openalex.org/", selected$openalex_author_id),
            target = "_blank",
            rel = "noopener noreferrer"
          ),
          span(class = confidence_class, selected$match_confidence),
          paste0(
            " · ", format(selected$exact_match_papers, big.mark = ","), " exact",
            " · ", format(selected$initial_match_papers, big.mark = ","), " initial-compatible"
          ),
          if (!is.na(selected$orcid) && nzchar(selected$orcid)) {
            tagList(" · ", tags$a("ORCID", href = selected$orcid, target = "_blank", rel = "noopener noreferrer"))
          },
          if (length(merged_ids) > 1) {
            tagList(tags$br(), strong("Merged OpenAlex profiles: "), merged_id_links)
          }
        ),
        div(
          class = "author-profile-grid",
          div(class = "author-profile-item",
              div(class = "author-profile-value", format(selected$unique_retracted_papers, big.mark = ",")),
              div(class = "author-profile-label", "Unique retracted papers")
          ),
          div(class = "author-profile-item",
              div(class = "author-profile-value", format(selected$openalex_works, big.mark = ",")),
              div(class = "author-profile-label", "OpenAlex works")
          ),
          div(class = "author-profile-item",
              div(class = "author-profile-value", normalized_rate),
              div(class = "author-profile-label", "Retracted papers per 100 works")
          ),
          div(class = "author-profile-item",
              div(class = "author-profile-value", format_year(selected$first_retraction_year)),
              div(class = "author-profile-label", "First retraction year")
          ),
          div(class = "author-profile-item",
              div(class = "author-profile-value", format_year(selected$last_retraction_year)),
              div(class = "author-profile-label", "Last retraction year")
          ),
          div(class = "author-profile-item",
              div(class = "author-profile-value", median_lag),
              div(class = "author-profile-label", "Median retraction lag")
          )
        ),
        div(
          class = review_note_class,
          strong("Why this match confidence: "), selected$match_confidence_explanation,
          tags$br(), tags$br(),
          strong("Identity decision: "), selected$identity_decision,
          tags$br(), selected$identity_decision_explanation,
          tags$br(), tags$br(),
          strong("Evidence used: "), selected$review_evidence,
          tags$br(), tags$br(),
          strong("How to read the match counts: "),
          "‘Exact’ means that the DOI identifies the RWD paper in OpenAlex and the normalized full RWD name appears in that work's author list. ‘Initial-compatible’ means that no exact full-name match was available, but one unique authorship shared the surname and compatible initials and was supported by the identity review. Ambiguous initials are excluded. These are counts of unique papers; the exact and initial-compatible counts add up to the papers assigned to this identity, and an exact name alone does not prove identity across homonyms.",
          tags$br(), tags$br(),
          span("Works denominator: ", selected$works_count_method, ".")
        )
      )
    } else {
      req(input$leaderboard_author, author_data())
      selected <- author_data() %>% filter(Author == input$leaderboard_author)
      req(nrow(selected) == 1)

      median_lag <- if (is.na(selected$MedianLagYears)) {
        "N/A"
      } else {
        paste0(format(round(selected$MedianLagYears, 1), nsmall = 1, trim = TRUE), " yrs")
      }

      div(
        class = "author-profile-grid",
        div(class = "author-profile-item",
            div(class = "author-profile-value", format(selected$UniqueRetractedPapers, big.mark = ",")),
            div(class = "author-profile-label", "Unique retracted papers")
        ),
        div(class = "author-profile-item",
            div(class = "author-profile-value", format(selected$FirstRetractionYear, scientific = FALSE, trim = TRUE)),
            div(class = "author-profile-label", "First retraction year")
        ),
        div(class = "author-profile-item",
            div(class = "author-profile-value", format(selected$LastRetractionYear, scientific = FALSE, trim = TRUE)),
            div(class = "author-profile-label", "Last retraction year")
        ),
        div(class = "author-profile-item",
            div(class = "author-profile-value", median_lag),
            div(class = "author-profile-label", "Median retraction lag")
        )
      )
    }
  })

  output$leaderboard_articles <- renderUI({
    req(input$leaderboard_author, paper_metadata_data())

    if (author_identity_available) {
      req(resolved_author_paper_data())
      papers <- resolved_author_paper_data() %>%
        filter(openalex_author_id == input$leaderboard_author) %>%
        transmute(PaperKey = paper_key)
    } else {
      req(author_paper_data())
      papers <- author_paper_data() %>%
        filter(Author == input$leaderboard_author) %>%
        select(PaperKey)
    }

    articles <- papers %>%
      distinct(PaperKey) %>%
      left_join(paper_metadata_data(), by = "PaperKey") %>%
      mutate(
        Title = if_else(is.na(Title) | trimws(Title) == "", "Title unavailable in RWD", Title),
        Authors = if_else(is.na(Authors) | trimws(Authors) == "", "Authors unavailable in RWD", Authors),
        Journal = if_else(is.na(Journal) | trimws(Journal) == "", "Journal unavailable in RWD", Journal)
      ) %>%
      arrange(desc(PublicationYear), Title)

    req(nrow(articles) > 0)

    article_items <- lapply(seq_len(nrow(articles)), function(index) {
      article <- articles[index, , drop = FALSE]
      publication_year <- if (is.na(article$PublicationYear)) {
        "Year unavailable"
      } else {
        format(as.integer(article$PublicationYear), scientific = FALSE, trim = TRUE)
      }
      has_doi <- !is.na(article$DOI) && nzchar(article$DOI) && str_detect(article$DOI, "^10\\..+/")
      title_tag <- if (has_doi) {
        tags$a(
          class = "author-article-title",
          article$Title,
          href = paste0("https://doi.org/", article$DOI),
          target = "_blank",
          rel = "noopener noreferrer"
        )
      } else {
        div(class = "author-article-title", article$Title)
      }

      div(
        class = "author-article-item",
        title_tag,
        div(class = "author-article-authors", tags$strong("Authors: "), article$Authors),
        div(
          class = "author-article-meta",
          tags$strong("Journal: "), article$Journal,
          " · ",
          tags$strong("Publication year: "), publication_year
        )
      )
    })

    tags$details(
      class = "author-articles-menu",
      tags$summary(
        span(class = "author-articles-summary-label", "Retracted articles"),
        span(class = "author-articles-count", format(nrow(articles), big.mark = ","))
      ),
      div(class = "author-articles-list", article_items)
    )
  })

  output$leaderboard_timeline_plot <- renderPlotly({
    req(input$leaderboard_author)
    if (author_identity_available) {
      req(resolved_author_paper_data())
      df <- resolved_author_paper_data() %>%
        filter(openalex_author_id == input$leaderboard_author, !is.na(retraction_year)) %>%
        count(retraction_year, name = "UniqueRetractedPapers") %>%
        rename(RetYear = retraction_year)
    } else {
      req(author_year_data())
      df <- author_year_data() %>% filter(Author == input$leaderboard_author)
    }

    df <- df %>%
      mutate(
        Tooltip = paste0(
          "<b>Retraction Year:</b> ", RetYear,
          "<br><b>Unique Retracted Papers:</b> ", format(UniqueRetractedPapers, big.mark = ",", trim = TRUE)
        )
      )
    req(nrow(df) > 0)

    p <- ggplot(df, aes(x = RetYear, y = UniqueRetractedPapers, text = Tooltip)) +
      geom_col(width = 0.85, fill = "#b42532", color = "#ffffff", size = 0.25) +
      scale_x_continuous(breaks = pretty(range(df$RetYear), n = 8)) +
      labs(x = "Retraction year", y = "Unique retracted papers") +
      owid_plot_theme()

    owid_plotly(p, tooltip = "text")
  })

  leaderboard_reason_categories <- reactive({
    req(input$leaderboard_author)

    if (author_identity_available) {
      req(resolved_author_paper_data())
      selected_papers <- resolved_author_paper_data() %>%
        filter(openalex_author_id == input$leaderboard_author) %>%
        transmute(PaperKey = paper_key)
    } else {
      req(author_paper_data())
      selected_papers <- author_paper_data() %>%
        filter(Author == input$leaderboard_author) %>%
        select(PaperKey)
    }

    selected_papers <- selected_papers %>% distinct(PaperKey)
    req(nrow(selected_papers) > 0, paper_reason_data())

    selected_reason_labels <- paper_reason_data() %>%
      semi_join(selected_papers, by = "PaperKey")

    selected_reason_labels %>%
      inner_join(build_reason_category_map(), by = "Reason") %>%
      distinct(PaperKey, Category, ClassificationRole) %>%
      count(ClassificationRole, Category, name = "UniqueRetractedPapers") %>%
      mutate(
        RoleOrder = match(
          ClassificationRole,
          c("Substantive category", "Pathway / context")
        ),
        CategoryOrder = match(Category, names(reason_category_definitions))
      ) %>%
      arrange(desc(UniqueRetractedPapers), RoleOrder, CategoryOrder)
  })

  output$leaderboard_reasons_table <- renderTable({
    categories <- leaderboard_reason_categories()
    validate(need(
      nrow(categories) > 0,
      "No labels from the Overview retraction-reason taxonomy were found for this author."
    ))

    categories %>%
      transmute(
        Classification = ClassificationRole,
        `Retraction reason category` = Category,
        `Unique retracted papers` = as.integer(UniqueRetractedPapers)
      )
  }, width = "100%", striped = TRUE, hover = TRUE, bordered = FALSE, align = "llr")

  output$leaderboard_publishers_table <- renderTable({
    req(input$leaderboard_author)
    if (author_identity_available) {
      req(resolved_author_publisher_data())
      resolved_author_publisher_data() %>%
        filter(openalex_author_id == input$leaderboard_author) %>%
        head(10) %>%
        select(publisher, unique_retracted_papers) %>%
        mutate(unique_retracted_papers = as.integer(unique_retracted_papers)) %>%
        rename(Publisher = publisher, `Unique retracted papers` = unique_retracted_papers)
    } else {
      req(author_publisher_data())
      author_publisher_data() %>%
        filter(Author == input$leaderboard_author) %>%
        head(10) %>%
        select(Publisher, UniqueRetractedPapers) %>%
        mutate(UniqueRetractedPapers = as.integer(UniqueRetractedPapers)) %>%
        rename(`Unique retracted papers` = UniqueRetractedPapers)
    }
  }, width = "100%", striped = TRUE, hover = TRUE, bordered = FALSE, align = "lr")
}

shinyApp(ui, server)

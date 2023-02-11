# -*- coding: utf-8 -*-
# SPDX-FileCopyrightText: © 2023 Pen-Yuan Hsing
# SPDX-License-Identifier: AGPL-3.0-or-later

# Load packages
library(dplyr)
library(forcats)
library(ggplot2)
library(readr)
library(readxl)
library(stringr)
library(tibble)
library(tidyr)

# ------------------------------------------------------------------------------
# Read data
#

# The column headings in the raw survey data exported from EUSurvey are very
# long and sometimes duplicated. Read in a set of mappings from
# `col_headings.csv` which gives shorter names to act as variable names in the
# main data frame.
col_headings <- read_csv(
    file = "data/col_headings.csv",
    show_col_types = FALSE # Quiet terminal output
)
# Read raw data with those column names into `survey_xls` data frame
survey_xls <- read_xls(
    path = "data/baseline_survey_results_20230206.xls",
    skip = 4L, # Data begins on line 4 after column headings on line 3
    col_names = colnames(col_headings)
)
# The variable with columns are no longer needed, so remove.
remove(col_headings)


# ------------------------------------------------------------------------------
# Initial cleaning
#

# Remove personal information contained in acknowledgements and prize
# information responses (if present)
survey_data <- survey_xls %>%
    select(-starts_with("Acknowledgement"), -starts_with("Prize")) %>%
    select(-"Contribution_ID") # Extraneous column
# Assign each response its own ID
# See: https://www.statology.org/add-index-column-to-data-frame-r/
survey_data <- rowid_to_column(survey_data, "Response_ID")
# Remove test responses
survey_data <- survey_data %>%
    slice_head(n = -5) # The first 5 test responses are at the end of the data frame

# ------------------------------------------------------------------------------
# Separate open-ended free text answers
#

survey_data_text <- survey_data %>%
    select("Response_ID", ends_with("_other"))
survey_data <- survey_data %>%
    select(!ends_with("_other"))

# ------------------------------------------------------------------------------
# Convert variables to factors
#

# Most variables in our data are ordinal, e.g. career length or "very low to
# very high". Specify that in the data frame.

# Career field
survey_data$Field <- fct(survey_data$"Field")

# Career length
career_lengths <- c("Less than 1 year",
                    "1 to 3 years",
                    "3 to 5 years",
                    "5 to 10 years",
                    "more than 10 years")
survey_data$"Career_length" <- fct(survey_data$"Career_length",
                                   levels = career_lengths)
remove(career_lengths)
# Create new grouping variable with cut-off at 5-years
survey_data <- survey_data %>%
    mutate(`career_stage` = if_else(`Career_length` %in% c("Less than 1 year", "1 to 3 years", "3 to 5 years"),
                                    "less than 5 years",
                                    "more than 5 years"))

# Location
survey_data$"Location" <- fct_infreq(survey_data$"Location")
survey_data$"Location" <- fct_rev(survey_data$"Location")

# Research culture, e.g. levels of support, career prospects, etc.
Level_order <- c("very low", "low", "high", "very high")
# https://stackoverflow.com/a/75364919/186904
survey_data <- survey_data %>%
    mutate(across(starts_with("Level_"),
                  function(x) fct(x, levels = Level_order)))
remove(Level_order)

# Barriers to publishing research
survey_data$"Not_publish" <- fct(survey_data$"Not_publish",
                                 levels = c("Yes", "No"))

# Division of labour
Tasks_order <- c("do it mostly on my own",
                 "do it mostly as a team",
                 "don't do it because someone else does it",
                 "not applicable")
survey_data <- survey_data %>%
    mutate(across(starts_with("Tasks_"),
                  function(x) fct(x, levels = Tasks_order)))
remove(Tasks_order)

# Research assessment
Assessment_order <- c("no influence",
                      "some influence",
                      "strong influence")
survey_data <- survey_data %>%
    mutate(across(starts_with("Assessment_"),
                  function(x) fct(x, levels = Assessment_order)))
remove(Assessment_order)

# ------------------------------------------------------------------------------
# Research fields
#

# Visualise the research fields by number of responses

# Subset the data
research_fields <- survey_data %>%
    select("Response_ID", "Field")
# Order the factors (i.e. fields) by number of responses followed by "Other"
research_fields <- research_fields %>%
    mutate(across(contains("Field"), fct_infreq)) %>%
    mutate(across(contains("Field"), fct_rev)) %>%
    mutate(across(contains("Field"),
                  function(x) fct_relevel(x, "Other", after = 0L)))
# Create plot
research_fields_fig <- ggplot(data = research_fields) +
    geom_bar(mapping = aes(y = `Field`), fill = "#fc8d59") +
    labs(x = "Number of responses", y = NULL) +
    theme_classic()
research_fields_fig

# ------------------------------------------------------------------------------
# Career length
#

# Visualise responses by how long they've been in their research fields

# Subset the data
career_length <- survey_data %>%
    select("Response_ID", "Career_length")
# Create plot
career_length_fig <- ggplot(data = career_length) +
    geom_bar(mapping = aes(y = `Career_length`), fill = "#fc8d59") +
    labs(x = "Number of responses", y = NULL) +
    theme_classic()
career_length_fig

# ------------------------------------------------------------------------------
# Location
#

# Visualise responses by where they are based in the world

# Subset the data
location <- survey_data %>%
    select("Response_ID", "Location")
location_fig <- ggplot(data = location) +
    geom_bar(mapping = aes(y = `Location`), fill = "#fc8d59") +
    labs(x = "Number of responses", y = NULL) +
    theme_classic()
location_fig

# ------------------------------------------------------------------------------
# Research culture
#

# Visualise how strongly respondents feel about certain factors in research
# culture, such as support from others, career prospects, or research impact.
# Measured by four levels from "very low" to "very high".
# Data wrangling and visualisation code adapted from:
# https://stackoverflow.com/a/75415517/186904

# Subset the data
research_culture <- survey_data %>%
    select("Response_ID", starts_with("Level_")) %>%
    pivot_longer(!`Response_ID`, names_to = "Factor" , values_to = "Level") %>%
    mutate(across(contains("Factor"), function(x) fct(x)))

# Rename factors for visualisation, see:
# http://www.cookbook-r.com/Manipulating_data/Renaming_levels_of_a_factor/
levels(research_culture$"Factor") <-
    list("Support from colleagues" = "Level_support_colleagues",
         "Career satisfaction" = "Level_career_satisfaction",
         "Collaboration between groups" = "Level_collaboration",
         "Research impact" = "Level_career_impact",
         "Support from community" = "Level_support_community",
         "Fairness of assessment" = "Level_assessment_fairness",
         "Career prospects" = "Level_career_prospects")

# Somehow the `Factor` variable needs to be of `chr` instead of `fct` type at
# this step for the `fct_relevel()` code below to work, which will turn it back
# into `fct`.
research_culture$"Factor" <- as.character(research_culture$"Factor")

# Order the eventual display of factors by the proportion of "very low"
prop_order <- research_culture %>%
    filter(`Level` == "very low") %>%
    group_by(`Factor`, `Level`) %>%
    # Get number of responses (`votes`) for "very low" for each factor.
    summarise(votes = n()) %>%
    arrange(votes) %>%
    # Keep just the `Factor` variable as a list of factors, which is now ordered
    # by "very low" and can be used to order them in a visualisation.
    pull(`Factor`)
# Relevel the `Factor`s by this new ordering.
research_culture$"Factor" <- fct_relevel(research_culture$"Factor", prop_order)
rm(prop_order)

# Create 100% stacked bar plot
research_culture_fig <- research_culture %>%
    group_by(`Factor`, `Level`) %>%
    summarise(prop = n()) %>% # I.e. number of responses for each level
    ggplot(aes(x = `prop`, y = `Factor`, fill = `Level`)) +
    geom_col(position = "fill") +
    labs(x = "Proportion of responses", y = NULL) +
    theme_classic() +
    scale_x_continuous(breaks = c(0, 0.5, 1.0), labels = scales::percent) +
    scale_fill_brewer(palette = "OrRd") +
    # https://stackoverflow.com/q/27130610/186904
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(title = NULL, nrow = 1, byrow = TRUE))
research_culture_fig

# ------------------------------------------------------------------------------
# Barriers to not publishing
#

# Visualise barriers to not publishing research work

# Subset the data
not_publish <- survey_data %>%
    filter(Not_publish == "Yes") %>%
    select("Response_ID", starts_with("Not_publish_"))
# Responses are all "nested" together, separated by the `;` character. Unnest
# this data.
not_publish <- not_publish %>%
    mutate(across(-`Response_ID`, function(x) str_split(x, pattern = "; "))) %>%
    pivot_longer("Not_publish_barriers", values_to = "barriers") %>%
    unnest_longer("barriers") %>%
    select(-`name`) %>%
    # End with `barriers` as `fct`:
    mutate(across(`barriers`, function(x) fct(x)))

# Rename factors for visualisation.
levels(not_publish$"barriers") <-
    list("Did not conform to established thinking" = "It did not conform to established thinking (e.g. a \"desired\" result, etc.)",
         "Could not find outlet to publish" = "I could not find an outlet that would publish work on this topic",
         "It would not help my career" = "It would not help my career",
         "Could not pass peer review" = "It was rejected by peer reviewers and I did not/could not do the requested revisions",
         "Did not fit a traditional paper" = "It did not fit in a traditional peer-reviewed paper",
         "Lack of confidence in analysis" = "I did not have confidence in the analysis/interpretation",
         "Insufficient \"impact\"" = "It did not have sufficient \"impact\"",
         "Findings did not make clean \"story\"" = "The findings did not make a clean \"story\"" ,
         "There was not enough data" = "There was not enough data",
         "Not enough time to write it up" = "I did not have time/resources to write it up"
    )

# Order `barriers` by frequency
not_publish <- not_publish %>%
    mutate(across(`barriers`, function(x) fct_infreq(x))) %>%
    # Reverse order of `barriers` for visualisation
    mutate(across(`barriers`, function(x) fct_rev(x)))

# Create bar plot
not_publish_fig <- not_publish %>%
    group_by(`barriers`) %>%
    summarise(n_responses = n()) %>%
    ggplot(aes(
        # Calculate proportion of responses for each barrier
        x = `n_responses`/n_distinct(not_publish$"Response_ID"),
        y = `barriers`)) +
    geom_col(fill = "#fc8d59") +
    labs(x = "Proportion of responses", y = NULL) +
    theme_classic() +
    scale_x_continuous(labels = scales::percent) +
    scale_fill_brewer(palette = "OrRd")
not_publish_fig

# ------------------------------------------------------------------------------
# Barriers to publishing early
#

# Visualise barriers to not publishing early stages of research

# Subset the data
publish_early <- survey_data %>%
    select(`Response_ID`, starts_with("Publish_early_"))
# Responses are all "nested" together, separated by the `;` character. Unnest
# this data.
publish_early <- publish_early %>%
    mutate(across(-`Response_ID`, function(x) str_split(x, pattern = "; "))) %>%
    pivot_longer("Publish_early_barriers", values_to = "barriers") %>%
    unnest_longer("barriers") %>%
    select(-`name`) %>%
    # End with `barriers` as `fct`:
    mutate(across(`barriers`, function(x) fct(x)))

# Rename factors for visualisation.
levels(publish_early$"barriers") <-
    list("Fear of being \"wrong\"" = "When evidence is collected it might not support the theory/hypothesis, and I would have been shown to be \"wrong\"",
         "Don't expect useful feedback" = "I wouldn't expect useful feedback from readers or reviewers at this stage",
         "Don't know how to write it up" = "I wouldn’t know how to write up a publication like this",
         "Not how things are done" = "This is not the way things are done in my field",
         "Insufficient \"impact\"" = "It would not have enough \"impact\", so is not worth sharing",
         "No benefit to career" = "There is no benefit to me and my career to publish something like this",
         "Don't know where to publish" = "I wouldn't know where to publish or share this type of output",
         "Fear of being \"scooped\"" = "Fear of being \"scooped\" or plagiarised because I would want to collect the evidence myself, and someone else might do so then claim credit for the idea"
    )

# Order `barriers` by frequency
publish_early <- publish_early %>%
    mutate(across(`barriers`, function(x) fct_infreq(x))) %>%
    # Reverse order of `barriers` for visualisation
    mutate(across(`barriers`, function(x) fct_rev(x)))

# Create bar plot
publish_early_fig <- publish_early %>%
    group_by(`barriers`) %>%
    summarise(n_responses = n()) %>%
    ggplot(aes(
        # Calculate proportion of responses for each barrier
        x = `n_responses`/n_distinct(publish_early$Response_ID),
        y = `barriers`)) +
    geom_col(fill = "#fc8d59") +
    labs(x = "Proportion of responses", y = NULL) +
    theme_classic() +
    scale_x_continuous(labels = scales::percent) +
    scale_fill_brewer(palette = "OrRd")
publish_early_fig

# ------------------------------------------------------------------------------
# Division of labour
#

# Visualise whether tasks through the research process are mainly done by the
# respondent, as a team, by someone else, or not applicable.

# Subset the data
division_of_labour <- survey_data %>%
    select(`Response_ID`, starts_with("Tasks_")) %>%
    pivot_longer(!`Response_ID`, names_to = "Task" , values_to = "Who") %>%
    mutate(across(contains("Task"), function(x) fct(x)))

Task_order <- c("not applicable",
                "don't do it because someone else does it",
                "do it mostly on my own",
                "do it mostly as a team")
division_of_labour$"Who" <- fct_relevel(division_of_labour$"Who",
                                         Task_order)
rm(Task_order)


# Rename factors for visualisation, see:
# http://www.cookbook-r.com/Manipulating_data/Renaming_levels_of_a_factor/
levels(division_of_labour$"Task") <-
    list("Defining question" = "Tasks_definition",
         "Reviewing state of the art" = "Tasks_review",
         "Defining hypothesis" = "Tasks_hypothesis",
         "Developing methodology" = "Tasks_methodology",
         "Data collection (qualitative)" = "Tasks_data_qualitative",
         "Data collection (quantitative)" = "Tasks_data_quantitative",
         "Curating and cleaning data" = "Tasks_data_cleaning",
         "Analysing results" = "Tasks_analyses",
         "Interpreting the analysis" = "Tasks_interpretation",
         "Applying to different context" = "Tasks_application"
    )

# Somehow the `Task` variable needs to be of `chr` instead of `fct` type at this
# step for the `fct_relevel()` code below to work, which will turn it back into
# `fct`.
division_of_labour$"Task" <- as.character(division_of_labour$"Task")

# Order the eventual display of tasks by the proportion of "do it mostly on my
# own"
prop_order <- division_of_labour %>%
    filter(`Who` == "do it mostly as a team") %>%
    group_by(`Task`, `Who`) %>%
    # Get number of responses (`votes`) for "do it mostly on my own" for each
    # factor.
    summarise(votes = n()) %>%
    arrange(votes) %>%
    # Keep just the `Task` variable as a list of factors, which is now ordered
    # by "do it mostly on my own" and can be used to order them in a
    # visualisation.
    pull(`Task`)
# Relevel the `Task`s by this new ordering.
division_of_labour$"Task" <- fct_relevel(division_of_labour$"Task", prop_order)
rm(prop_order)

# Create 100% stacked bar plot
division_of_labour_fig <- division_of_labour %>%
    group_by(`Task`, `Who`) %>%
    summarise(prop = n()) %>% # I.e. number of responses for each task
    ggplot(aes(x = `prop`, y = `Task`, fill = `Who`)) +
    geom_col(position = "fill") +
    labs(x = "Proportion of responses", y = NULL) +
    theme_classic() +
    scale_x_continuous(breaks = c(0, 0.5, 1.0), labels = scales::percent) +
    scale_fill_brewer(palette = "OrRd") +
    # Format legend, see:
    # https://stackoverflow.com/q/27130610/186904
    # http://www.cookbook-r.com/Graphs/Legends_(ggplot2)/
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(title = NULL, nrow = 2, byrow = TRUE))
division_of_labour_fig

# ------------------------------------------------------------------------------
# Research assessment
#

# Visualise the influence of various factors in research assessment, i.e.
# "no influence", "some influence", and "strong influence".

# Subset the data
assessment <- survey_data %>%
    select(`Response_ID`, starts_with("Assessment_")) %>%
    pivot_longer(!`Response_ID`, names_to = "Factor" , values_to = "Influence") %>%
    mutate(across(contains("Factor"), function(x) fct(x)))

# Rename factors for visualisation.
levels(assessment$"Factor") <-
    list("\"Novelty\" of research" = "Assessment_novelty",
         "Having positive results" = "Assessment_positive_results",
         "Research impact" = "Assessment_impact",
         "Prestige of institution" = "Assessment_prestige",
         "Publication record" = "Assessment_publications",
         "Personal connections" = "Assessment_personal_connections",
         "Personal characteristics" = "Assessment_personal_characteristics",
         "Trendiness of research" = "Assessment_trendiness" ,
         "Rigour of methods" = "Assessment_rigour",
         "Open research practices" = "Assessment_openness",
         "Confirming existing ideas" = "Assessment_orthodoxy"
    )

# Somehow the `Factor` variable needs to be of `chr` instead of `fct` type at
# this step for the `fct_relevel()` code below to work, which will turn it back
# into `fct`.
assessment$"Factor" <- as.character(assessment$"Factor")

# Order the eventual display of factors by the proportion of "no influence"
prop_order <- assessment %>%
    filter(`Influence` == "no influence") %>%
    group_by(`Factor`, `Influence`) %>%
    # Get number of responses (`votes`) for each factor.
    summarise(votes = n()) %>%
    arrange(votes) %>%
    # Keep just the `Factor` variable as a list of factors, which is now ordered
    # by "do it mostly on my own" and can be used to order them in a
    # visualisation.
    pull(`Factor`)
# Relevel the `Factor`s by this new ordering.
assessment$"Factor" <- fct_relevel(assessment$"Factor", prop_order) %>%
    fct_rev()
rm(prop_order)

# Create 100% stacked bar plot
assessment_fig <- assessment %>%
    group_by(`Factor`, `Influence`) %>%
    summarise(prop = n()) %>% # I.e. number of responses for each task
    ggplot(aes(x = `prop`, y = `Factor`, fill = `Influence`)) +
    geom_col(position = "fill") +
    labs(x = "Proportion of responses", y = NULL) +
    theme_classic() +
    scale_x_continuous(breaks = c(0, 0.5, 1.0), labels = scales::percent) +
    scale_fill_brewer(palette = "OrRd") +
    # Format legend, see:
    # https://stackoverflow.com/q/27130610/186904
    # http://www.cookbook-r.com/Graphs/Legends_(ggplot2)/
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(title = NULL, nrow = 1, byrow = TRUE))
assessment_fig

# ------------------------------------------------------------------------------
# Publishing platforms
#

# Visualise respondents' awareness of various research publishing platforms.

# Subset the data
platforms <- survey_data %>%
    select(`Response_ID`, `Platforms`) %>%
    mutate(across(`Platforms`, function(x) str_split(x, pattern = "; "))) %>%
    pivot_longer("Platforms", values_to = "Platforms") %>%
    # Remove extra variable `name` created by `pivot_longer()`:
    select(-`name`) %>%
    unnest_longer(`Platforms`) %>%
    # End of `Platforms` as `fct` %>%
    mutate(across(`Platforms`, function(x) fct(x)))

# Rename factors for visualisation, see:
# https://stackoverflow.com/a/69501369/186904
platforms$Platforms <- platforms$Platforms %>%
    factor(levels = c(
        "Preprint servers (e.g. arXiv, bioRxiv, AfricArXiv, EcoEvoRxiv, PsyArXiv, etc.)",
        "Institutional repositories",
        "FigShare",
        "Multimedia repositories (e.g. Flickr, Internet Archive, Wikimedia Commons, etc.)",
        "Video hosting platforms (e.g. YouTube, PeerTube, Vimeo, etc.)",
        "Github, Gitlab, Wikifactory, or similar service",
        "Specialist data repositories (e.g. ProteomeXchange, OpenNeuro, PubChem, PANGAEA, Qualitative Data Repository, etc.)",
        "Wikipedia",
        "Research Ideas and Outcomes (RIO)",
        "Open Science Framework (OSF)",
        "Registered reports (i.e. journal articles with pre-study peer review)",
        "Zenodo",
        "Protocols.io",
        "Dryad",
        "ResearchEquals",
        "Octopus.ac",
        "Peer Community In (PCI)"
    ),
    labels = c(
        "Preprint servers",
        "Institutional repositories",
        "FigShare",
        "Multimedia repositories",
        "Video hosting platforms",
        "Version control platforms",
        "Specialist data repositories",
        "Wikipedia",
        "Research Ideas & Outcomes (RIO)",
        "Open Science Framework (OSF)",
        "Registered reports",
        "Zenodo",
        "Protocols.io",
        "Dryad",
        "ResearchEquals",
        "Octopus.ac",
        "Peer Community In (PCI)"
    ))

# Order `Platforms` by level of awareness, i.e. number of responses
platforms <- platforms %>%
    mutate(across(`Platforms`, function(x) fct_infreq(x))) %>%
    # Reverse order of `Platforms` for visualisation
    mutate(across(`Platforms`, function(x) fct_rev(x)))

# Create bar plot
platforms_fig <- platforms %>%
    group_by(`Platforms`) %>%
    summarise(n_responses = n()) %>%
    ggplot(aes(
        # Calculate proportion of responses for each barrier
        x = `n_responses`/n_distinct(platforms$Response_ID),
        y = `Platforms`)) +
    geom_col(fill = "#fc8d59") +
    labs(x = "Proportion of responses", y = NULL) +
    theme_classic() +
    scale_x_continuous(labels = scales::percent) +
    scale_fill_brewer(palette = "OrRd")
platforms_fig
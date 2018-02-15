# library(knitr)
# library(rmarkdown)
# Library KnitFmt sur github :
# devtools::install_github("statnmap/Rmarkdown_tips",
# lib = "/usr/local/lib/R/site-library")
# emojifont
# devtools::install_github("GuangchuangYu/emojifont")
library(KnitFmt)

# Directories ------------------------------------------------------------------
wd <- here()
# "/mnt/Data/Formation_TD-GLM_Cours_Agro/TD_Vilaine_SIG-et-GLM_2017_01_3days"
setwd(wd)

fig.auto.WD <-
  "/mnt/Data/ThinkR/Gitlab/vignette-thinkr/img"

# CASES ------------------------------------------------------------------------
# Create a HTML only with Alldata and another for Delta model
ClassicWay <- eval.AllDataExploration <- eval.AllDataModel <-
  eval.PresAbsModel <- eval.PositiveModel <- eval.HSI <- FALSE

# -- Choose your way --
# === Normal way - 2 days :
ClassicWay <- TRUE
# -- DAY 1 : set to TRUE --
eval.AllDataExploration <- eval.AllDataModel <- TRUE
# -- DAY 2 : set to TRUE --
eval.PresAbsModel <- eval.PositiveModel <- FALSE
eval.HSI <- FALSE

# === Rapid way - 1 day :
ClassicWay <- FALSE
eval.PositiveModel <- FALSE
eval.HSI <- FALSE
# -- Morning : set to TRUE --
eval.AllDataExploration <- eval.AllDataModel <- FALSE
# -- Afternoon : set to TRUE --
eval.PresAbsModel <- TRUE

Case <- paste(na.omit(c(
  ifelse(ClassicWay, "Classic", "Quick"),
  ifelse(eval.AllDataModel, "AllDataModel", NA),
  ifelse(eval.PresAbsModel, "PresAbs", NA),
  ifelse(eval.PositiveModel, "Positive", NA),
  ifelse(eval.HSI, "HSI", NA)
)), collapse = "_")


for (CourseVersion in 1:2) {
  # CourseVersion <- 2
  Version <- c("Student", "Teacher")[CourseVersion]

  for (lang in c("EN", "FR")[2]) {
    # lang <- c("EN", "FR")[2]

    # knit or purl ? ----
    for (Action in c("KnitHTML", "KnitPDF", "Purl")) {
      # Action <- c("KnitHTML", "KnitPDF", "Purl")[2]

      # _Graphic options ----
      eval.ggplot <- TRUE # Les figures du support de cours avec {ggplot2}
      purl.S.ggplot <- TRUE # Le code (purl) pour les fig des stagiaires avec ggplot2
      purl.T.ggplot <- ifelse(Version == "Teacher", TRUE, FALSE)

      eval.widget <- ifelse(Action == "KnitHTML", TRUE, FALSE)
      purl.ggplot <- ifelse(purl.S.ggplot | purl.T.ggplot, TRUE, FALSE)
      purl.T <- purl.S <- (Action == "Purl")

      # Document options for PDF ----
      lang.latex <- ifelse(lang == "EN", "en", "fr")
      title.lang <- ifelse(lang == "EN",
        "Tutorial on GLM and species distribution models",
        "Formation aux GLM et aux modèles de distribution d'espèces"
      )

      # KNIT -------------------------------------------------------------------------
      dirname <- paste0(lang, "_", Version, "_", Case)
      if (!dir.exists(dirname)) {
        dir.create(dirname)
      }

      if (grepl("Knit", Action)) {
        eval.S <- FALSE
        if (Version == "Student") {
          # PDF version - Students
          echo.S <- FALSE
          echo.T <- FALSE # purl.S <- TRUE; purl.T <- FALSE
          res.show <- "hide"
        }
        if (Version == "Teacher") {
          # PDF version - Teacher
          echo.S <- FALSE
          echo.T <- FALSE # purl.S <- FALSE; purl.T <- TRUE
          res.show <- "hide" #' markup'
        }

        if (Action == "KnitHTML") {
          bookdown::render_book(
            "00_MainFile.Rmd", output_format = "bookdown::gitbook",
            output_dir = dirname, clean = TRUE
          )
          if (lang == "EN") {
            system(paste0("firefox ", dirname, "/1-preface.html", " &"))
            # system(paste0("firefox ", dirname, "/4-delta-model-application.html", " &"))
          } else if (lang == "FR") {
            system(paste0("firefox ", dirname, "/1-preface.html", " &"))
          }
        } else if (Action == "KnitPDF") {
          filename <- paste0(lang, "_", Case, "_", Version, ".pdf")
          # dirname <- paste0(lang, "_", Case, "_", Version)
          # rmarkdown::render(
          bookdown::render_book(
            "00_MainFile.Rmd", output_format = "bookdown::pdf_book",
            output_file = filename, output_dir = dirname
          )
          system(paste0("evince ", dirname, "/", filename, " &"))
        }
      } # end of knit

      # PURL --------
      if (Action == "Purl") {
        knitr::opts_chunk$set(
          echo = FALSE,
          purl = FALSE
        )
        # Define purl according to Version (see upper)
        if (Version == "Student") {
          purl.S <- TRUE
          purl.T <- FALSE
          eval.S <- TRUE # Student
        }
        if (Version == "Teacher") {
          purl.S <- FALSE
          purl.T <- TRUE
          eval.S <- FALSE # Teacher
          eval.widget <- FALSE
        }

        filename <- paste0(dirname, "/", Version, "_", Case, ".R")
        if (file.exists(filename)) {
          file.remove(filename)
        }
        knitr::purl(
          "00_MainFile.Rmd", encoding = "UTF-8",
          documentation = 0
        )
        file.rename(from = "00_MainFile.R", to = filename)
      } # end of purl
    } # end of Action
  } # End of lang
} # End of CourseVersion

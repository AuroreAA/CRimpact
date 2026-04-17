# function to get references as HTML tags to be displayed in the GUI or as
# lines to be written into a file for import into into a reference manager
#' Get references
#'
#' @param format
#'
#' @noRd
#'
#' @author Andreas Alfons and Aurore Archimbaud
#'
#' @importFrom utils packageDescription packageVersion packageDate
get_references <- function(format = "HTML") {
  # FIXME: author information is currently hardcoded
  # get information for package 'CRimpact'
  package <- "CRimpact"
  description <- packageDescription(package)
  version <- toString(packageVersion(package))
  year <- format(packageDate(package), "%Y")
  year <- 2024
  title <- gsub("(\\s|\\()([A-Z])", "\\1\\L\\2", description$Title,
                perl = TRUE, fixed = FALSE)
  note <- paste("package version", version)
  URL <- description$URL
  if (format == "HTML") {
    title <- paste0("<strong>", package, "</strong>: ", title)
    note <- paste("<strong>R</strong>", note)
  } else {
    title <- paste(package, title, sep = ": ")
    note <- paste("R", note)
  }
  # construct references in requested format
  if (format == "HTML") {
    # author information for package 'CRimpact'
    author <- "Archimbaud, A., Alfons, A., & Welz, M."
    # return list of HTML tags
    tagList(
      # HTML tag for PsyArXiv paper
      RATINGBIAS = p(
        HTML("Welz, M., Archimbaud, A., & Alfons, A. (2024).
             How much carelessness is too much? Quantifying the impact of careless responding."),
        HTML("<em>PsyArXiv Preprints</em>"),
        a("https://osf.io/preprints/psyarxiv/8fj6p",
          href = "https://osf.io/preprints/psyarxiv/8fj6p")
      ),
      # # HTML tag for package 'ratingbias'
      # ratingbias = p(
      #   paste0( "Welz, M., Alfons, A., & Archimbaud, A.", " (", year, ")."),
      #   HTML(paste0("ratingbias: Quantifying the effects of response bias", ".")),
      #   a("https://github.com/mwelz/ratingbias", href = "https://github.com/mwelz/ratingbias")
      # ),
      # HTML tag for package 'CRimpact'
      CRimpact = p(
        paste0(author, " (", year, ")."), HTML(paste0(title, ".")),
        HTML(paste0(note, ".")), a(URL, href = URL)
      )
    )
  } else if (format == "EndNote") {
    # return vector of lines to be written to .enw file
    c(
      # EndNote reference for ORM paper
      "%0 Journal Article",
      "%A Welz, A.",
      "%A Archimbaud, A.",
      "%A Alfons, A.",
      "%D 2024",
      "%T How much carelessness is too much? Quantifying the impact of careless responding",
      "%J PsyArXiv Preprints",
      "%U https://osf.io/preprints/psyarxiv/8fj6p",
      "",
      # # EndNote reference for package 'ratingbias'
      # "%0 Computer Program",
      # "%M Welz, A.",
      # "%A Alfons, A.",
      # "%A Archimbaud, A.",
      # paste("%D", year),
      # paste("%T", "raingbias: Quantifying the effects of response bias"),
      # #paste("%Z", note),
      # paste("%U", "https://github.com/mwelz/ratingbias"),
      # "",
      # EndNote reference for package 'CRimpact'
      "%0 Computer Program",
      "%A Alfons, A.",
      "%A Archimbaud, A.",
      "%M Welz, A.",
      paste("%D", year),
      paste("%T", title),
      paste("%Z", note),
      paste("%U", URL)
    )
  } else if (format == "BibTeX") {
    # author information for package 'ratingbiasExtra'
    author <- "Archimbaud, A., Alfons, A. and Welz, M."
    # return vector of lines to be written to .bib file
    c(
      # BibTeX entry for OSF paper
      "@article{welz2024much,",
      "  title={How much carelessness is too much? Quantifying the impact of careless responding},",
      "  author={Welz, Max and Archimbaud, Aurore and Alfons, Andreas},",
      "  year={2024},",
      " journal={PsyArXiv Preprints}",
      "  publisher={OSF}",
      "  url={https://osf.io/preprints/psyarxiv/8fj6p}",
      "}",
      "",
      # # BibTeX entry for package 'ratingbias'
      # sprintf("@manual{welz%s,", year),
      # sprintf("  author = {%s}", "Welz, M., Alfons, A., & Archimbaud, A."),
      # sprintf("  year = {%s}", year),
      # sprintf("  title = {%s}", "ratingbias: Quantifying the effects of response bias"),
      # #sprintf("  note = {%s}", note),
      # sprintf("  url = {%s}", "https://github.com/mwelz/ratingbias"),
      # "}",
      # "",
      # BibTeX entry for package 'CRimpact'
      sprintf("@manual{archimbaud%s,", year),
      sprintf("  author = {%s}", author),
      sprintf("  year = {%s}", year),
      sprintf("  title = {%s}", title),
      sprintf("  note = {%s}", note),
      sprintf("  url = {%s}", URL),
      "}"
    )
  }
}

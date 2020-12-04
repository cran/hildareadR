#' Read and Combine HILDA waves
#'
#' Extracts and combines variables from selected HILDA waves
#'
#' The \code{read_hilda} function reads, extracts and combines variables from individual waves (a wave refers to each separate survey in a series of related surveys) of the panel dataset
#' 'The Household, Income and Labour Dynamics in Australia' (HILDA), into an R data.frame.
#' If you are using many waves \code{read_hilda} can take a while to run, go grab a cup of coffee! Don't forget to save the resulting \code{data.frame} so you don't have to do it again.
#'
#' The identifier \code{xwaveid} is included by default. An indicator \code{wave} is created, keeping track of from which wave a variable originates (denoted by wave prefixes a:q for waves 1:17).
#' The function reads from the HILDA .dta (STATA) files.
#'
#' @param domain The domain you want to extract. \code{Domain} refers to the variable prefixes (of any length), excluding any wave indicators. For several domains separate by c("x", "y").
#' @param waves The wave(s) you want the domain variables to come from. Can be left empty if using custom file names.
#' @param dir Directory where the STATA files are located. Default is the current working directory.
#' @param filenames Vector of .dta files to be read. Default is names used by the Australian Data Archive.
#' @param release Release number of the wave to be read. Defaults to 18. (newest wave)
#'
#' @author Sara Kalucza & Sebastian Kalucza
#'
#' @import haven
#' @import dplyr
#'
#' @return A \code{data.frame} with the chosen \code{domain} variables, xwaveid and wave indicator.
#' @export
#'
#' @examples
#' path <- system.file("extdata", package = "hildareadR")
#' # Reads variable hibiff and hgage from waves 1 to 3 in directory fdir and from release 17
#' read_hilda(c("hibiff", "hgage"), waves = 1:3, dir = path, release = 17)
#' # Reads variable aiopeye from custom files in directory fdir and from release 17
#' read_hilda("aiopeye", dir = path, filenames = c("custom1.dta", "custom2.dta"), release = 17)

read_hilda <- function (domain, waves = NULL, dir = NULL, filenames = NULL, release = 18){
  stopifnot(is.character(domain), is.numeric(waves) | is.null(waves),
            is.null(dir) | is.character(dir), is.null(filenames) |
            is.character(filenames))
  if (is.null(waves) & is.null(filenames)) {
    stop("waves and filenames can't both be empty")
  }
  if (is.null(filenames)) {
    file_list <- paste0("Combined_", letters[waves], release, "0c.dta")
  }
  else {
    file_list <- filenames
  }
  if (!is.null(dir)) {
    file_list <- paste0(dir, "/", file_list)
  }
  domain <- paste0("^.", domain)
  df <- read_dta(file = file_list[1])
  df <- df[, c(1, grep(paste(domain, collapse = "|"), names(df)))]
  df$wave <- substring(names(df)[2], 1, 1)
  names(df)[c(-1, -length(names(df)))] <- substring(names(df)[c(-1, -length(names(df)))], 2)
  df <- zap_labels(df)
  for (i in file_list[-1]) {
    temp <- read_dta(file = i)
    temp <- temp[, c(1, grep(paste(domain, collapse = "|"), names(temp)))]
    temp$wave <- substring(names(temp)[2], 1, 1)
    names(temp)[c(-1, -length(names(temp)))] <- substring(names(temp)[c(-1, -length(names(temp)))], 2)
    temp <- zap_labels(temp)
    df <- suppressMessages(bind_rows(df, temp))
  }
  return(df)
}

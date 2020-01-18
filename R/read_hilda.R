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
#' # Reads variable hibiff and all variables named hgage from waves 1 to 3 in directory fdir
#' read_hilda(c("hibiff", "hgage"), waves = 1:3, dir = path)
#' # Reads variable aiopeye from custom files in directory fdir
#' read_hilda("aiopeye", dir = path, filenames = c("custom1.dta", "custom2.dta"))

read_hilda <- function(domain, waves = NULL, dir = NULL, filenames = NULL){
  stopifnot(is.character(domain), is.numeric(waves) | is.null(waves), is.null(dir) | is.character(dir),
            is.null(filenames) | is.character(filenames))
  #Check that user has entered either waves or custom filenames and return error if not
  if(is.null(waves) & is.null(filenames)){
    stop("waves and filenames can't both be empty")
  }
  # Add .dta filenames into list
  if(is.null(filenames)) {
    file_list <- paste0("Combined_", letters[waves], "170c.dta")
  } else {
    file_list <- filenames
  }
  #Adds file directory to filenames if added by user
  if(!is.null(dir)){
    file_list <- paste0(dir, "/", file_list)
  }
  #Add REGEX to domain name to only grab correct variables
  domain <- paste0("^.", domain)
  #Load first STATA file into temporary df
  df <- read_dta(file = file_list[1])
  #Subset df with domain and id
  df <- df[,c(1, grep(paste(domain,collapse = "|"), names(df)))]
  #Add wave ID
  df$wave <- substring(names(df)[2], 1, 1)
  #Remove wave letter from beginning each variable name
  names(df)[c(-1, -length(names(df)))] <- substring(names(df)[c(-1, -length(names(df)))], 2)
  #Remove STATA labels
  df <- zap_labels(df)
  #Loop to add all waves into one df
  for(i in file_list[-1]){
    temp <- read_dta(file = i)
    temp <- temp[,c(1, grep(paste(domain,collapse = "|"), names(temp)))]
    temp$wave <- substring(names(temp)[2], 1, 1)
    names(temp)[c(-1, -length(names(temp)))] <- substring(names(temp)[c(-1, -length(names(temp)))], 2)
    temp <- zap_labels(temp)
    df <- suppressMessages(bind_rows(df, temp))
  }
  return(df)
}

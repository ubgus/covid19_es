# Download new pdfs
update_ccaes_pdf <- function() {
  if (!dir.exists("pdfs")) {
    message("There is not 'pdfs' directory")
  } else {
    today <- Sys.Date()
    
    # list files in pdfs folder
    all_files <- list.files(path = "pdfs", full.names = TRUE)
    
    # Get the last one and extract the date
    last_file <- tail(all_files, 1)
    date_last_file <- str_extract(last_file, "\\d{4}-\\d{2}-\\d{2}") %>% 
      as.Date()
    
    # compare with today and if it is lower, then download the specified files
    if (date_last_file < today) {
      message("Download in progress")
      
      seq_of_dates <- seq.Date(from = date_last_file + 1, to = today, by = "day")
      
      file_number <- str_extract(last_file, "_\\d{2,}_") %>% 
        str_remove_all("_") %>% 
        as.numeric()
      seq_of_files <- seq(from = file_number + 1, length.out = length(seq_of_dates))
      
      
      url_build <- function(seq_of_files){
        glue::glue("https://www.mscbs.gob.es/profesionales/",
                   "saludPublica/ccayes/alertasActual/nCov-China/documentos/",
                   "Actualizacion_{seq_of_files}_COVID-19.pdf")
      }
      
      # output to the pdfs folder
      download_pdf <- function(seq_of_files, url_in, seq_of_dates){
        download.file(
          url = url_in,
          destfile = glue::glue("pdfs/ccaes_report_{seq_of_files}_covid19_{seq_of_dates}.pdf")
        )
      }
      
      # build all the input urls and attach to an input dataframe
      full_df <- tibble::tibble(seq_of_files) %>% 
        dplyr::mutate(url_in = purrr::map(.x = seq_of_files, .f = url_build)) %>% 
        dplyr::mutate(seq_of_dates = seq_of_dates)
      
      # The pwalk here takes all 3 inputs and applies them to download_pdf function
      purrr::pwalk(full_df, .f = download_pdf)
      
    } else {
      message("Files are up to date")
    }
  }
} 

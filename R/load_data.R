# load data

read_survey <- function(dir = "data") {
  read <- function(file) {
    readr::read_csv(file.path(dir, file), show_col_types = FALSE)
  }

  list(
    participants = read("grant_writing.csv"),
    groups       = read("grupos_subrepresentados.csv"),
    fields       = read("investigacion.csv"),
    practices    = read("frecuencia.csv"),
    barriers     = read("barreras.csv")
  )
}

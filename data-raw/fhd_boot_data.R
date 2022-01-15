# This script gathers and formats Johnston et al. (2014) bootstrap samples of generic flight height
# distributions from main species of interest, into a single list object to facilitate
# use in simulations

fhd_boot_dir <- "data-raw/Johnston_FHD_bootstraps/"

fhd_boot_files <- tibble::tibble(
  filename = list.files(fhd_boot_dir),
  filepath = list.files(fhd_boot_dir, pattern = ".+ht_dflt.csv", full.names = TRUE),
  species = stringr::str_replace(filename, "_ht_dflt.csv", replacement = "")
)


generic_fhd_bootstraps <- fhd_boot_files %>%
  dplyr::group_by(species) %>%
  dplyr::group_map(~ readr::read_csv(.$filepath))

# attribute species names to list elements
names(generic_fhd_bootstraps) <- fhd_boot_files$species

# 1. rename height column
# 2. change height values the lower limit of bands (e.g. 0-1 band now
#    represented by 0; 1-2 by 1, and so on)
generic_fhd_bootstraps <- generic_fhd_bootstraps %>%
  purrr::map(function(x){
    x %>%
      dplyr::rename(height = Height_m) %>%
      dplyr::mutate(height = height-1)
  })


usethis::use_data(generic_fhd_bootstraps, overwrite = TRUE, compress = "xz")

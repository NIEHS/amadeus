################################################################################
# Development file for NOAA HRRR dataset functions.
# 30 September 2026
# Mitchell Manware

################################################################################
# Initial scope
# ---1. CONUS
# ---2. 2D/Surface (sfc), Native (nat), and 3D pressure level (prs) products
# ---3. Hourly (not including sub-hourly data for now)

################################################################################
# Download each of 3 initial scope products for function development.
chr_day <- "20260929"
chr_prod <- c("prs", "nat", "sfc")
chr_awshrrr <- c("hrrr.t12z.wrf", "f05.grib2")

chr_names <- paste0(chr_awshrrr[1], chr_prod, chr_awshrrr[2])
stopifnot(length(unique(chr_names)) == 3)

chr_urls <- paste0(
  "https://noaa-hrrr-bdp-pds.s3.amazonaws.com/hrrr.",
  chr_day,
  "/conus/",
  chr_names
)
stopifnot(length(unique(chr_urls)) == 3)

amadeus::download_run_method(
  urls = chr_urls,
  destfiles = paste0("data/hrrr/", chr_names),
  show_progress = FALSE
)

################################################################################
# Extract HRRR variable metadata.
chr_hrrr <- list.files("data/hrrr/", full.names = TRUE, recursive = TRUE)
df_hrrrmeta1 <- data.frame()
for (h in chr_hrrr) {
  rast_h <- terra::rast(h, md = FALSE)
  df_h <- data.frame(
    product = substr(strsplit(h, "wrf")[[1]][2], 1, 3),
    description = names(rast_h),
    units = terra::units(rast_h)
  )
  df_hrrrmeta1 <- rbind(df_hrrrmeta1, df_h)
}

################################################################################
# Manually parse HRRR variable metadata.
chr_desc <- df_hrrrmeta1$description
# Goal 10/01: manage multi-length variable descriptoins which were broken
list_desc <- strsplit(chr_desc, "; ")
chr_l1 <- unlist(lapply(list_desc, function(x) x[[1]]))

list_level <- strsplit(chr_l1, "=")
df_levels$level_desc <- unlist(lapply(list_level, function(x) x[[2]]))

list_levelcode1 <- lapply(list_level, function(x) x[[1]])
df_levels <- data.frame(
  do.call(
    rbind,
    lapply(
      list_levelcode1,
      function(x) {
        x_parts <- unlist(strsplit(x, "\\] "))
        ifelse(
          length(x_parts) == 1,
          return(c(x_parts, NA_character_)),
          return(c(x_parts[2], paste0(x_parts[1], "]")))
        )
      }
    )
  )
)
names(df_levels) <- c("level_code", "level_Z")




################################################################################






################################################################################
# download_hrrr
# User inputs:
# ---1. Sector (conus/alaska)
# ---2. Z dimension (2D, native, 3D)


################################################################################
# Parse multi-layer metadata.

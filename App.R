# ==============================================================================
#  Crop Cut — Combined QC & Field Monitoring Dashboard
#  Pula Advisors  |  v2 — improved visibility, box-distance tab, ID-upload filter
# ==============================================================================
#
#  CREDENTIALS (never entered in UI — set via environment or config):
#
#  METHOD 1 — Environment variables (recommended for Posit Connect)
#    DB_HOST | DB_PORT | DB_NAME | DB_USER | DB_PASSWORD
#
#  METHOD 2 — config.yml next to app.R
#  METHOD 3 — .Renviron
#
#  AWS S3 (optional URL validation):
#    AWS_ACCESS_KEY_ID | AWS_SECRET_ACCESS_KEY
#    AWS_DEFAULT_REGION  (defaults to eu-west-1)
#    AWS_SESSION_TOKEN   (optional)
#
#  SHAPEFILES:
#    gadm_data/    — GADM Level-2  .rds files
#    cluster_data/ — Cluster / UAI .rds files
#
#  Required packages:
#    install.packages(c(
#      "pacman","shiny","shinydashboard","shinyjs","shinyWidgets",
#      "DBI","RPostgres","dplyr","tidyr","stringr","lubridate","purrr",
#      "sf","writexl","readxl","DT","leaflet","leaflet.extras","plotly",
#      "waiter","glue","config","jsonlite","aws.s3"
#    ))
# ==============================================================================

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  shiny, shinydashboard, shinyjs, shinyWidgets,
  DBI, RPostgres,
  dplyr, tidyr, stringr, lubridate, purrr,
  sf, writexl, readxl,
  DT, leaflet, leaflet.extras,
  plotly, waiter, glue, config,
  jsonlite, aws.s3
)

# ==============================================================================
# 1.  CREDENTIAL RESOLUTION
# ==============================================================================

get_credential <- local({
  cfg <- tryCatch(config::get(), error = function(e) list())
  function(env_key, cfg_key, default = "") {
    val <- Sys.getenv(env_key, unset = "")
    if (nzchar(val)) return(val)
    val <- cfg[[cfg_key]]
    if (!is.null(val) && nzchar(as.character(val))) return(as.character(val))
    default
  }
})

make_conn <- function() {
  tryCatch(
    dbConnect(
      RPostgres::Postgres(),
      host     = Sys.getenv("DB_HOST",     "db_host"),
      port     = as.integer(Sys.getenv("DB_PORT", "5432")),
      dbname   = Sys.getenv("DB_NAME",     "db_name"),
      user     = Sys.getenv("DB_USER",     "db_user"),
      password = Sys.getenv("DB_PASSWORD", "db_password")
    ),
    error = function(e) { message("DB connection failed: ", conditionMessage(e)); NULL }
  )
}

conn_ok <- function(conn) {
  !is.null(conn) && tryCatch(dbIsValid(conn), error = function(e) FALSE)
}

# ==============================================================================
# 2.  DATABASE QUERIES
# ==============================================================================

get_projects <- function(conn) {
  tryCatch(
    dbGetQuery(conn,
               "SELECT DISTINCT project_id AS id, project_name AS name
       FROM   common_cropcut
       WHERE  project_name IS NOT NULL
       ORDER  BY project_name"),
    error = function(e) data.frame(id = integer(), name = character())
  )
}

fetch_image_data <- function(conn, project_ids) {
  ids_str <- paste(as.integer(project_ids), collapse = ",")
  q <- sprintf("
    WITH user_supervisor_data AS (
      SELECT e.user_id,
             e.name            AS enumerator_name,
             e.phone_number    AS enumerator_phone,
             s.name            AS supervisor_name
      FROM   common_fieldstaff e
      LEFT JOIN common_fieldstaff s
             ON e.supervisor_id = s.id AND s.role = 'SUPERVISOR'
      WHERE  e.role = 'ENUMERATOR'
    )
    SELECT
        cc.id                    AS cropcut_id,
        cc.boxes_pula_id,
        cc.project_id,
        cc.project_name,
        cc.boxes_farmer_name,
        uai_t.identifier         AS uai,
        res.position,
        res.response_id,
        res.responses,
        res.farmer_responses,
        res.submitted_by_id,
        res.start_time,
        res.end_time,
        i.cce_id,
        i.question_id,
        i.s3_url,
        u.enumerator_name,
        u.enumerator_phone,
        u.supervisor_name
    FROM  common_questionnaireresponse res
    LEFT JOIN common_cropcut                 cc    ON res.cropcut_id       = cc.id
    LEFT JOIN common_surveyimage             i     ON res.response_id      = i.response_id
    LEFT JOIN user_supervisor_data           u     ON res.submitted_by_id  = u.user_id
    LEFT JOIN common_unitareaofinsurance     uai_t ON res.uai_id           = uai_t.id
    WHERE cc.project_id IN (%s)
      AND cc.status != 'REJECTED';
  ", ids_str)
  tryCatch(dbGetQuery(conn, q),
           error = function(e) { message("Image query failed: ", conditionMessage(e)); NULL })
}

# ── Field monitoring query — now includes box GPS ─────────────────────────────
fetch_monitoring_data <- function(conn, project_ids) {
  ids_str <- paste(as.integer(project_ids), collapse = ",")
  q <- glue("
    WITH user_supervisor_data AS (
      SELECT
        enumerator.user_id,
        enumerator.name         AS enumerator_name,
        enumerator.phone_number AS enumerator_phone,
        supervisor.name         AS supervisor_name
      FROM common_fieldstaff AS enumerator
      LEFT JOIN common_fieldstaff AS supervisor
        ON enumerator.supervisor_id = supervisor.id
       AND supervisor.role = 'SUPERVISOR'
      WHERE enumerator.role = 'ENUMERATOR'
    )
    SELECT
      cc.boxes_pula_id,
      cc.id                                              AS cce_id,
      cc.status,
      cc.cce_adm1,
      cc.cce_adm2,
      cc.project_name,
      uai.identifier                                     AS uai,
      p.name                                             AS partner,
      f.farmer_type,
      f.phone_number                                     AS farmer_phone,
      f.name                                             AS farmer_name,
      res.start_time,
      res.farmer_responses ->> 'crop'                    AS crop,
      res.position,
      res.responses -> 'q_field_gps' ->> 'latitude'     AS latitude_m,
      res.responses -> 'q_field_gps' ->> 'longitude'    AS longitude_m,
      res.responses -> 'q_farm_gps'  ->> 'latitude'     AS latitude_v,
      res.responses -> 'q_farm_gps'  ->> 'longitude'    AS longitude_v,
      res.responses -> 'q_box1_gps'  ->> 'latitude'     AS box1_latitude,
      res.responses -> 'q_box1_gps'  ->> 'longitude'    AS box1_longitude,
      res.responses -> 'q_box2_gps'  ->> 'latitude'     AS box2_latitude,
      res.responses -> 'q_box2_gps'  ->> 'longitude'    AS box2_longitude,
      u.enumerator_name,
      u.supervisor_name
    FROM common_questionnaireresponse AS res
    LEFT JOIN common_cropcut             AS cc  ON res.cropcut_id  = cc.id
    LEFT JOIN common_project             AS p   ON res.project_id  = p.id
    LEFT JOIN common_unitareaofinsurance AS uai ON res.uai_id      = uai.id
    LEFT JOIN common_farmer              AS f   ON res.farmer_id   = f.id
    LEFT JOIN user_supervisor_data       AS u   ON res.submitted_by_id = u.user_id
    WHERE res.project_id IN ({ids_str})
      AND cc.status != 'REJECTED';
  ")
  tryCatch(dbGetQuery(conn, q),
           error = function(e) { message("Monitor query failed: ", conditionMessage(e)); NULL })
}

# ==============================================================================
# 3.  IMAGE QC — DATA PROCESSING
# ==============================================================================

COMMON_COLS <- c(
  "cropcut_id", "boxes_pula_id", "project_name", "boxes_farmer_name",
  "uai", "enumerator_name", "enumerator_phone", "supervisor_name",
  "start_time", "end_time"
)

process_image_data <- function(raw) {
  if (is.null(raw) || nrow(raw) == 0) return(NULL)
  colnames(raw) <- make.unique(colnames(raw))
  photocols     <- na.omit(unique(raw$question_id))
  
  image_links <- raw %>%
    select(cce_id, boxes_pula_id, position, uai, question_id, s3_url) %>%
    distinct() %>%
    pivot_wider(names_from  = question_id,
                values_from = s3_url,
                values_fn   = ~ paste(unique(na.omit(.x)), collapse = ", "))
  
  parsed <- raw %>%
    mutate(
      responses        = map(responses,        ~ tryCatch(fromJSON(.x, flatten = TRUE), error = function(e) list())),
      farmer_responses = map(farmer_responses, ~ tryCatch(fromJSON(.x, flatten = TRUE), error = function(e) list()))
    ) %>%
    unnest_wider(responses,        names_repair = "unique") %>%
    unnest_wider(farmer_responses, names_repair = "unique")
  
  Data <- parsed %>%
    select(-any_of(c(photocols, "question_id", "s3_url"))) %>%
    distinct() %>%
    left_join(image_links, by = c("boxes_pula_id", "position")) %>%
    select(any_of(c(COMMON_COLS, "position")), starts_with("q_"))
  
  list(
    full        = Data,
    css         = Data %>% filter(position == 1),
    wet         = Data %>% filter(position == 2),
    dry         = Data %>% filter(position == 3),
    photocols   = photocols,
    image_links = image_links
  )
}

# ==============================================================================
# 4.  MISSING IMAGE CHECKS
# ==============================================================================

check_missing <- function(data, photo_col,
                          cond_col  = NULL,
                          cond_type = c("equal", "contains", "in"),
                          cond_val  = NULL,
                          section   = "Unknown") {
  cond_type <- match.arg(cond_type)
  if (!photo_col %in% names(data)) return(NULL)
  d <- data
  if (!is.null(cond_col) && cond_col %in% names(d) && !is.null(cond_val)) {
    d <- switch(cond_type,
                "equal"    = d %>% filter(.data[[cond_col]] == cond_val),
                "contains" = d %>% filter(str_detect(.data[[cond_col]], cond_val)),
                "in"       = d %>% filter(.data[[cond_col]] %in% cond_val)
    )
  }
  result <- d %>%
    filter(is.na(.data[[photo_col]])) %>%
    select(any_of(c(COMMON_COLS, "position"))) %>%
    mutate(question_id = photo_col, section = section)
  if (nrow(result) == 0) NULL else result
}

build_missing <- function(css, wet, dry) {
  loss_conditions <- c("average", "below_average", "no_crop_survived")
  all_checks <- list(
    check_missing(css, "q_farmer_sign",              section = "CSS"),
    check_missing(css, "q_photo_both_boxes",         section = "CSS"),
    check_missing(css, "q_field_photo",              section = "CSS"),
    check_missing(css, "q_crop_closeup_photo",       section = "CSS"),
    check_missing(css, "q_farmer_photo",             section = "CSS"),
    check_missing(css, "q_irrigation_type_photo",
                  cond_col = "q_field_irrigated", cond_type = "equal", cond_val = "yes",
                  section = "CSS"),
    check_missing(css, "q_weeds_photo",
                  cond_col = "q_problem", cond_type = "contains", cond_val = "weeds",
                  section = "CSS"),
    check_missing(css, "q_flood_evidence_photo",
                  cond_col = "q_has_flood_evidence", cond_type = "equal", cond_val = "yes",
                  section = "CSS"),
    check_missing(css, "q_pests_or_diseases_evidence_photo",
                  cond_col = "q_problem", cond_type = "contains", cond_val = "pest",
                  section = "CSS"),
    check_missing(css, "q_secondary_issue_photo",
                  cond_col = "q_secondary_issue_present", cond_type = "equal", cond_val = "yes",
                  section = "CSS"),
    check_missing(css, "q_corner1_total_loss_photo",
                  cond_col = "q_crop_condition", cond_type = "in", cond_val = loss_conditions,
                  section = "CSS"),
    check_missing(css, "q_corner2_total_loss_photo",
                  cond_col = "q_crop_condition", cond_type = "in", cond_val = loss_conditions,
                  section = "CSS"),
    check_missing(css, "q_corner3_total_loss_photo",
                  cond_col = "q_crop_condition", cond_type = "in", cond_val = loss_conditions,
                  section = "CSS"),
    check_missing(css, "q_corner4_total_loss_photo",
                  cond_col = "q_crop_condition", cond_type = "in", cond_val = loss_conditions,
                  section = "CSS"),
    check_missing(wet, "q_farmer_sign",        section = "Wet Harvest"),
    check_missing(wet, "q_wet_harvest_photo",
                  cond_col = "q_box2_harvest_possible", cond_type = "equal", cond_val = "yes",
                  section = "Wet Harvest"),
    check_missing(wet, "q_harvest_bags_photo",
                  cond_col = "q_box2_harvest_possible", cond_type = "equal", cond_val = "yes",
                  section = "Wet Harvest"),
    check_missing(wet, "q_attestation_form",
                  cond_col = "q_box2_harvest_possible", cond_type = "equal", cond_val = "yes",
                  section = "Wet Harvest"),
    check_missing(wet, "q_box1_wet_weight_photo",
                  cond_col = "q_box1_harvest_possible", cond_type = "equal", cond_val = "yes",
                  section = "Wet Harvest"),
    check_missing(wet, "q_box2_wet_weight_photo",
                  cond_col = "q_box2_harvest_possible", cond_type = "equal", cond_val = "yes",
                  section = "Wet Harvest"),
    check_missing(wet, "q_box1_corner1_total_loss_photo",
                  cond_col = "q_why_unable_to_capture_box1_weight",
                  cond_type = "in", cond_val = loss_conditions, section = "Wet Harvest"),
    check_missing(wet, "q_box1_corner2_total_loss_photo",
                  cond_col = "q_why_unable_to_capture_box1_weight",
                  cond_type = "in", cond_val = loss_conditions, section = "Wet Harvest"),
    check_missing(wet, "q_box1_corner3_total_loss_photo",
                  cond_col = "q_why_unable_to_capture_box1_weight",
                  cond_type = "in", cond_val = loss_conditions, section = "Wet Harvest"),
    check_missing(wet, "q_box1_corner4_total_loss_photo",
                  cond_col = "q_why_unable_to_capture_box1_weight",
                  cond_type = "in", cond_val = loss_conditions, section = "Wet Harvest"),
    check_missing(wet, "q_box2_corner1_total_loss_photo",
                  cond_col = "q_why_unable_to_capture_box2_weight",
                  cond_type = "in", cond_val = loss_conditions, section = "Wet Harvest"),
    check_missing(wet, "q_box2_corner2_total_loss_photo",
                  cond_col = "q_why_unable_to_capture_box2_weight",
                  cond_type = "in", cond_val = loss_conditions, section = "Wet Harvest"),
    check_missing(wet, "q_box2_corner3_total_loss_photo",
                  cond_col = "q_why_unable_to_capture_box2_weight",
                  cond_type = "in", cond_val = loss_conditions, section = "Wet Harvest"),
    check_missing(wet, "q_box2_corner4_total_loss_photo",
                  cond_col = "q_why_unable_to_capture_box2_weight",
                  cond_type = "in", cond_val = loss_conditions, section = "Wet Harvest"),
    check_missing(dry, "q_farmer_sign",        section = "Dry Harvest"),
    check_missing(dry, "q_attestation_form",
                  cond_col = "q_capture_wet_weight_1", cond_type = "equal", cond_val = "yes",
                  section = "Dry Harvest"),
    check_missing(dry, "q_box1_unthreshed_photo",
                  cond_col = "q_capture_wet_weight_1", cond_type = "equal", cond_val = "yes",
                  section = "Dry Harvest"),
    check_missing(dry, "q_box1_dry_weight_photo_unthreshed",
                  cond_col = "q_capture_wet_weight_1", cond_type = "equal", cond_val = "yes",
                  section = "Dry Harvest"),
    check_missing(dry, "q_box1_threshed_photo",
                  cond_col = "q_capture_wet_weight_1", cond_type = "equal", cond_val = "yes",
                  section = "Dry Harvest"),
    check_missing(dry, "q_box1_dry_weight_photo_threshed",
                  cond_col = "q_capture_wet_weight_1", cond_type = "equal", cond_val = "yes",
                  section = "Dry Harvest"),
    check_missing(dry, "q_box2_unthreshed_photo",
                  cond_col = "q_capture_wet_weight_2", cond_type = "equal", cond_val = "yes",
                  section = "Dry Harvest"),
    check_missing(dry, "q_box2_dry_weight_photo_unthreshed",
                  cond_col = "q_capture_wet_weight_2", cond_type = "equal", cond_val = "yes",
                  section = "Dry Harvest"),
    check_missing(dry, "q_box2_threshed_photo",
                  cond_col = "q_capture_wet_weight_2", cond_type = "equal", cond_val = "yes",
                  section = "Dry Harvest"),
    check_missing(dry, "q_box2_dry_weight_photo_threshed",
                  cond_col = "q_capture_wet_weight_2", cond_type = "equal", cond_val = "yes",
                  section = "Dry Harvest")
  )
  bind_rows(Filter(Negate(is.null), all_checks)) %>%
    mutate(`Days Since Submission` = as.numeric(Sys.Date() - as.Date(end_time))) %>%
    filter(`Days Since Submission` >= 0) %>%
    distinct()
}

# ==============================================================================
# 5.  URL VALIDATION — AWS S3
# ==============================================================================

S3_BUCKET <- "mavuno-files"

parse_s3_url <- function(url) {
  url <- trimws(url)
  if (is.na(url) || !nzchar(url)) return(NULL)
  m1 <- regmatches(url, regexec(
    "^https?://([^.]+)\\.s3[.-]([^.]+)\\.amazonaws\\.com/(.+)$", url, perl = TRUE))[[1]]
  if (length(m1) == 4) return(list(bucket = m1[2], region = m1[3], key = m1[4]))
  m2 <- regmatches(url, regexec(
    "^https?://s3[.-]([^.]+)\\.amazonaws\\.com/([^/]+)/(.+)$",   url, perl = TRUE))[[1]]
  if (length(m2) == 4) return(list(bucket = m2[3], region = m2[2], key = m2[4]))
  m3 <- regmatches(url, regexec(
    "^https?://([^.]+)\\.s3\\.amazonaws\\.com/(.+)$",            url, perl = TRUE))[[1]]
  if (length(m3) == 3)
    return(list(bucket = m3[2],
                region = Sys.getenv("AWS_DEFAULT_REGION", "eu-west-1"),
                key    = m3[3]))
  NULL
}

check_s3_url <- function(url) {
  if (is.na(url) || !nzchar(trimws(url))) return("no_url")
  parsed <- parse_s3_url(url)
  if (is.null(parsed)) return("invalid_s3_format")
  tryCatch({
    result <- aws.s3::head_object(object = parsed$key,
                                  bucket = parsed$bucket,
                                  region = parsed$region)
    if (isTRUE(result) || is.list(result)) "ok" else "http_404"
  }, error = function(e) {
    msg <- conditionMessage(e)
    if      (grepl("404", msg, fixed = TRUE)) "http_404"
    else if (grepl("403", msg, fixed = TRUE)) "http_403"
    else if (grepl("401", msg, fixed = TRUE)) "http_403"
    else {
      code <- regmatches(msg, regexpr("[45][0-9]{2}", msg))
      if (length(code) == 1 && nzchar(code)) paste0("http_", code) else "unreachable"
    }
  })
}

run_url_validation <- function(image_links) {
  meta_cols <- c("cce_id", "boxes_pula_id", "uai", "position")
  url_cols  <- setdiff(names(image_links), meta_cols)
  image_links %>%
    pivot_longer(cols = all_of(url_cols), names_to = "question_id", values_to = "url") %>%
    filter(!is.na(url), nzchar(url)) %>%
    mutate(url = strsplit(url, ",\\s*")) %>%
    unnest(url) %>%
    mutate(url = trimws(url)) %>%
    filter(!is.na(url), nzchar(url)) %>%
    mutate(
      parsed_bucket   = map_chr(url, ~ { p <- parse_s3_url(.x); if (is.null(p)) NA_character_ else p$bucket }),
      bucket_mismatch = !is.na(parsed_bucket) & parsed_bucket != S3_BUCKET
    ) %>%
    distinct()
}

# ==============================================================================
# 6.  FIELD MONITORING HELPERS
# ==============================================================================

clean_coords <- function(df) {
  df %>%
    mutate(
      lat = coalesce(as.numeric(latitude_m), as.numeric(latitude_v)),
      lon = coalesce(as.numeric(longitude_m), as.numeric(longitude_v))
    ) %>%
    select(-contains("itude_"))
}

# ── NEW: Box-to-box distance within a CCE ─────────────────────────────────────
calc_box_dist <- function(data, thr = 30) {
  d <- data %>%
    filter(position == 1) %>%
    mutate(
      b1_lat = as.numeric(box1_latitude),
      b1_lon = as.numeric(box1_longitude),
      b2_lat = as.numeric(box2_latitude),
      b2_lon = as.numeric(box2_longitude)
    ) %>%
    filter(!is.na(b1_lat), !is.na(b1_lon), !is.na(b2_lat), !is.na(b2_lon))
  
  if (nrow(d) == 0) return(tibble())
  
  d %>%
    rowwise() %>%
    mutate(
      box_distance_m = round(as.numeric(
        st_distance(
          st_sfc(st_point(c(b1_lon, b1_lat)), crs = 4326),
          st_sfc(st_point(c(b2_lon, b2_lat)), crs = 4326)
        )
      ), 1)
    ) %>%
    ungroup() %>%
    filter(box_distance_m > thr) %>%
    mutate(
      status = case_when(
        box_distance_m <= 10  ~ "Within standard (≤10 m)",
        box_distance_m <= 30  ~ "Within allowance (10–30 m)",
        TRUE                  ~ "Exceeds allowance (>30 m)"
      )
    ) %>%
    select(
      boxes_pula_id, cce_id, uai, crop, farmer_name,
      enumerator_name, supervisor_name, cce_adm2,
      start_time,
      box1_lat = b1_lat, box1_lon = b1_lon,
      box2_lat = b2_lat, box2_lon = b2_lon,
      box_distance_m, status
    ) %>%
    arrange(desc(box_distance_m))
}

calc_visit_dist <- function(data, thr_v1v2 = 300, thr_v1v3 = 3000, thr_v2v3 = 3000) {
  data %>%
    pivot_wider(
      id_cols     = c(boxes_pula_id, cce_id, uai, crop, farmer_name,
                      enumerator_name, supervisor_name),
      names_from  = position,
      values_from = c(lat, lon, start_time),
      names_glue  = "{.value}_v{position}"
    ) %>%
    rowwise() %>%
    mutate(
      dist_v1_v2 = if (any(is.na(c(lat_v1, lon_v1, lat_v2, lon_v2)))) NA_real_ else
        as.numeric(st_distance(st_sfc(st_point(c(lon_v1, lat_v1)), crs = 4326),
                               st_sfc(st_point(c(lon_v2, lat_v2)), crs = 4326))),
      dist_v1_v3 = if (any(is.na(c(lat_v1, lon_v1, lat_v3, lon_v3)))) NA_real_ else
        as.numeric(st_distance(st_sfc(st_point(c(lon_v1, lat_v1)), crs = 4326),
                               st_sfc(st_point(c(lon_v3, lat_v3)), crs = 4326))),
      dist_v2_v3 = if (any(is.na(c(lat_v2, lon_v2, lat_v3, lon_v3)))) NA_real_ else
        as.numeric(st_distance(st_sfc(st_point(c(lon_v2, lat_v2)), crs = 4326),
                               st_sfc(st_point(c(lon_v3, lat_v3)), crs = 4326)))
    ) %>%
    ungroup() %>%
    filter((!is.na(dist_v1_v2) & dist_v1_v2 > thr_v1v2) |
             (!is.na(dist_v1_v3) & dist_v1_v3 > thr_v1v3) |
             (!is.na(dist_v2_v3) & dist_v2_v3 > thr_v2v3))
}

calc_cce_dist <- function(data, thr = 1000) {
  d <- data %>% filter(position == 1, !is.na(lat), !is.na(lon))
  if (nrow(d) < 2) return(tibble())
  pts          <- d %>% st_as_sf(coords = c("lon","lat"), crs = 4326, remove = FALSE) %>%
    st_transform(3857)
  dm           <- st_distance(pts)
  diag(dm)     <- units::set_units(Inf, "m")
  ids          <- pts$boxes_pula_id
  rownames(dm) <- ids; colnames(dm) <- ids
  as.data.frame(dm) %>%
    mutate(from_id = rownames(dm)) %>%
    pivot_longer(-from_id, names_to = "to_id", values_to = "distance_m") %>%
    mutate(
      distance_m = as.numeric(distance_m),
      pair       = paste(pmin(from_id, to_id), pmax(from_id, to_id), sep = "-")
    ) %>%
    distinct(pair, .keep_all = TRUE) %>%
    left_join(select(d, boxes_pula_id, uai, crop, enumerator_name,
                     farmer_name, cce_adm2, lat, lon),
              by = c("to_id" = "boxes_pula_id")) %>%
    left_join(select(d, boxes_pula_id, uai, crop, enumerator_name,
                     farmer_name, cce_adm2, lat, lon),
              by = c("from_id" = "boxes_pula_id"), suffix = c("_to","_from")) %>%
    filter(from_id != to_id, uai_from == uai_to,
           crop_from == crop_to, distance_m < thr)
}

calc_dup_gps <- function(data) {
  empty_result <- tibble(
    visit = character(), boxes_pula_id = character(), coord_key = character(),
    lat = numeric(), lon = numeric(), uai = character(), crop = character(),
    enumerator_name = character(), farmer_name = character(),
    start_time = as.POSIXct(character())
  )
  result <- map_dfr(c(1, 2, 3), function(v) {
    d <- data %>%
      filter(position == v, !is.na(lat), !is.na(lon)) %>%
      mutate(lat_r = round(lat, 6), lon_r = round(lon, 6),
             coord_key = paste(lat_r, lon_r, sep = "|"))
    if (nrow(d) < 2) return(empty_result)
    dup_keys <- d %>% count(coord_key) %>% filter(n > 1) %>% pull(coord_key)
    if (length(dup_keys) == 0) return(empty_result)
    d %>%
      filter(coord_key %in% dup_keys) %>%
      arrange(coord_key, start_time) %>%
      select(visit = position, boxes_pula_id, coord_key, lat, lon,
             uai, crop, enumerator_name, farmer_name, start_time) %>%
      mutate(visit = paste0("Visit ", visit))
  })
  if (nrow(result) == 0) empty_result else result
}

run_spatial <- function(raw, gadm_sf, cluster_sf) {
  cluster_sf <- cluster_sf %>%
    filter(!st_is_empty(geometry)) %>%
    mutate(cluster = as.character(cluster)) %>%
    st_cast("POLYGON")
  cluster_sf2 <- cluster_sf %>%
    group_by(cluster) %>% summarise() %>% ungroup() %>%
    st_cast("MULTIPOLYGON") %>% st_make_valid()
  touch_list   <- st_touches(cluster_sf2)
  neighbor_tbl <- tibble(
    cluster   = cluster_sf2$cluster,
    neighbors = sapply(touch_list, function(x)
      if (length(x) == 0) NA_character_ else paste(cluster_sf2$cluster[x], collapse = ", "))
  )
  d1 <- raw %>% filter(position == 1, !is.na(lat), !is.na(lon))
  d2 <- d1 %>%
    st_as_sf(coords = c("lon","lat"), crs = 4326, remove = FALSE) %>%
    st_transform(st_crs(cluster_sf)) %>%
    st_join(cluster_sf %>% select(cluster_collected = cluster)) %>%
    st_drop_geometry() %>%
    st_as_sf(coords = c("lon","lat"), crs = 4326, remove = FALSE) %>%
    st_transform(st_crs(gadm_sf)) %>%
    st_join(gadm_sf %>% select(starts_with("NAME_"))) %>%
    st_drop_geometry() %>%
    mutate(cluster_collected = coalesce(as.character(cluster_collected), "0"),
           cluster_assigned  = as.character(uai))
  assigned_geoms <- cluster_sf %>%
    group_by(cluster) %>% summarise(geometry = st_union(geometry)) %>% ungroup() %>%
    st_make_valid() %>% st_transform(st_crs(cluster_sf))
  d3 <- d2 %>%
    left_join(assigned_geoms %>% rename(cluster_assigned = cluster),
              by = "cluster_assigned") %>%
    rowwise() %>%
    mutate(
      point_proj = st_sfc(st_point(c(lon, lat)), crs = 4326) %>%
        st_transform(st_crs(cluster_sf)),
      dist_to_assigned_uai =
        if (is.na(cluster_assigned) || st_is_empty(geometry)) NA_real_
      else as.numeric(st_distance(point_proj, geometry))
    ) %>%
    ungroup() %>%
    select(-point_proj, -geometry) %>%
    left_join(neighbor_tbl, by = c("cluster_assigned" = "cluster")) %>%
    mutate(
      uai_match = if_else(cluster_collected == cluster_assigned, "yes", "no"),
      in_neighbouring_aez = case_when(
        uai_match == "yes" ~ "UAI Match",
        !is.na(neighbors) &
          str_detect(neighbors,
                     paste0("(?<![0-9])", cluster_collected, "(?![0-9])")) ~ "yes",
        TRUE ~ "no"
      ),
      dist_to_assigned_uai = round(dist_to_assigned_uai, 1)
    )
  d3
}

list_rds <- function(folder) {
  if (!dir.exists(folder)) return(character(0))
  sort(list.files(folder, pattern = "\\.rds$", full.names = FALSE))
}

# ==============================================================================
# 7.  CSS  — improved visibility: rich midnight-slate palette
# ==============================================================================

app_css <- "
@import url('https://fonts.googleapis.com/css2?family=DM+Sans:ital,wght@0,400;0,500;0,600;0,700;1,400&family=Space+Mono:wght@400;700&display=swap');

/* ── Base ── */
body, .content-wrapper, .right-side {
  background: #141b2d !important;
  font-family: 'DM Sans', sans-serif;
  color: #e2e8f0;
}

/* ── Header ── */
.skin-black .main-header .logo,
.skin-black .main-header .navbar {
  background: #1c2641 !important;
  border-bottom: 1px solid #2e3f6e !important;
}
.skin-black .main-header .logo {
  font-weight: 700;
  letter-spacing: .02em;
  font-size: .88rem;
}

/* ── Sidebar ── */
.skin-black .main-sidebar {
  background: #1c2641 !important;
  border-right: 1px solid #2e3f6e !important;
}
.skin-black .sidebar a {
  color: #94a3b8 !important;
  font-size: .83rem;
}
.skin-black .sidebar-menu > li.active > a,
.skin-black .sidebar-menu > li:hover  > a {
  background: #253460 !important;
  color: #f8fafc !important;
  border-left: 3px solid #34d399;
}
.skin-black .sidebar-menu .treeview-menu > li > a {
  color: #64748b !important;
  font-size: .79rem;
}
.skin-black .sidebar-menu .treeview-menu > li.active > a,
.skin-black .sidebar-menu .treeview-menu > li:hover  > a {
  color: #34d399 !important;
}

/* ── Boxes / Cards ── */
.box {
  background: #1c2641 !important;
  border: 1px solid #2e3f6e !important;
  border-top: none !important;
  border-radius: 14px !important;
  color: #cbd5e1;
  box-shadow: 0 4px 20px rgba(0,0,0,.35);
}
.box-header {
  background: #1c2641 !important;
  color: #f1f5f9 !important;
  border-bottom: 1px solid #2e3f6e !important;
  border-radius: 14px 14px 0 0 !important;
  padding: 12px 18px;
}
.box-title { font-weight: 700; letter-spacing: .04em; font-size: .92rem; }

/* ── KPI value boxes ── */
.small-box {
  border-radius: 14px !important;
  border: 1px solid #2e3f6e !important;
  box-shadow: 0 4px 16px rgba(0,0,0,.3);
}
.small-box h3 {
  font-family: 'Space Mono', monospace;
  font-size: 1.7rem !important;
  font-weight: 700;
  color: #f8fafc;
}
.small-box p {
  font-size: .7rem;
  text-transform: uppercase;
  letter-spacing: .08em;
  opacity: .9;
  color: #e2e8f0;
}

/* ── Form controls ── */
.form-control, .selectize-input {
  background: #253460 !important;
  border: 1px solid #2e3f6e !important;
  color: #f1f5f9 !important;
  border-radius: 8px !important;
}
.selectize-dropdown {
  background: #253460 !important;
  color: #f1f5f9 !important;
  border: 1px solid #2e3f6e !important;
}
.selectize-dropdown .active { background: #2e3f6e !important; }
label {
  color: #94a3b8 !important;
  font-size: .72rem;
  text-transform: uppercase;
  letter-spacing: .07em;
}

/* ── Sliders ── */
.irs--shiny .irs-bar, .irs--shiny .irs-handle {
  background: #34d399 !important;
  border-color: #34d399 !important;
}
.irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
  background: #34d399 !important;
  color: #0f172a !important;
}
.irs--shiny .irs-line { background: #2e3f6e !important; }

/* ── Buttons ── */
.btn-green {
  background: linear-gradient(135deg, #34d399, #059669) !important;
  border: none !important;
  border-radius: 9px !important;
  font-weight: 700;
  letter-spacing: .04em;
  color: #0f172a !important;
  transition: all .2s;
  width: 100%;
}
.btn-green:hover {
  transform: translateY(-2px);
  box-shadow: 0 6px 18px rgba(52,211,153,.4);
}
.btn-blue {
  background: linear-gradient(135deg, #60a5fa, #2563eb) !important;
  border: none !important;
  border-radius: 9px !important;
  font-weight: 700;
  letter-spacing: .04em;
  color: #fff !important;
  transition: all .2s;
  width: 100%;
}
.btn-blue:hover {
  transform: translateY(-2px);
  box-shadow: 0 6px 18px rgba(96,165,250,.4);
}
.btn-dl {
  background: #253460 !important;
  border: 1px solid #2e3f6e !important;
  color: #94a3b8 !important;
  border-radius: 8px !important;
  font-size: .78rem;
  font-weight: 600;
  transition: all .15s;
}
.btn-dl:hover {
  background: #34d399 !important;
  color: #0f172a !important;
  border-color: #34d399 !important;
}

/* ── File input ── */
.btn-default {
  background: #253460 !important;
  border: 1px solid #2e3f6e !important;
  color: #94a3b8 !important;
  border-radius: 6px !important;
}
.btn-default:hover {
  background: #2e3f6e !important;
  color: #f1f5f9 !important;
}

/* ── DataTables ── */
table.dataTable { background: #1c2641 !important; color: #e2e8f0 !important; border: none !important; }
table.dataTable thead th {
  background: #253460 !important;
  color: #94a3b8 !important;
  border-bottom: 1px solid #2e3f6e !important;
  font-size: .72rem;
  text-transform: uppercase;
  letter-spacing: .06em;
}
table.dataTable tbody tr { border-bottom: 1px solid #1c2641 !important; }
table.dataTable tbody tr:hover { background: #253460 !important; }
.dataTables_wrapper .dataTables_filter input,
.dataTables_wrapper .dataTables_length select {
  background: #253460 !important;
  border: 1px solid #2e3f6e !important;
  color: #f1f5f9 !important;
  border-radius: 6px;
}
.dataTables_wrapper .dataTables_info,
.dataTables_wrapper .dataTables_paginate { color: #64748b !important; font-size: .75rem; }
.dataTables_wrapper .dataTables_paginate .paginate_button.current {
  background: #34d399 !important;
  color: #0f172a !important;
  border-radius: 6px;
  border: none !important;
}
.dataTables_wrapper .dataTables_paginate .paginate_button:hover {
  background: #253460 !important;
  color: #f1f5f9 !important;
  border: none !important;
}

/* ── Misc labels ── */
.sec-lbl {
  color: #475569;
  font-size: .63rem;
  text-transform: uppercase;
  letter-spacing: .14em;
  padding: 10px 16px 3px;
  font-family: 'Space Mono', monospace;
  display: block;
}
.pill {
  display: inline-block;
  padding: 2px 12px;
  border-radius: 999px;
  font-size: .7rem;
  font-weight: 700;
  letter-spacing: .06em;
}
.pill-ok   { background: #064e3b; color: #6ee7b7; }
.pill-err  { background: #7f1d1d; color: #fca5a5; }
.pill-idle { background: #1e2a4a; color: #64748b; }
.pill-warn { background: #78350f; color: #fcd34d; }

.thr-panel {
  background: #1c2641;
  border: 1px solid #2e3f6e;
  border-radius: 12px;
  padding: 14px 18px;
  margin-bottom: 14px;
}
.thr-panel h5 {
  color: #94a3b8;
  font-size: .71rem;
  text-transform: uppercase;
  letter-spacing: .1em;
  margin: 0 0 10px;
  font-family: 'Space Mono', monospace;
}

/* ── Upload filter banner ── */
.filter-active-banner {
  background: linear-gradient(90deg, #064e3b, #065f46);
  border: 1px solid #34d399;
  border-radius: 8px;
  padding: 7px 12px;
  font-size: .75rem;
  color: #6ee7b7;
  font-weight: 600;
  text-align: center;
  margin: 4px 0;
}

.sidebar-divider { border-color: #2e3f6e; margin: 8px 0; }
"

# ==============================================================================
# 8.  UI
# ==============================================================================

ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = tags$span(
      style = "font-family:'Space Mono',monospace;font-size:.88rem;letter-spacing:.03em;",
      "🌾 CropCut Dashboard"),
    titleWidth = 250
  ),
  
  # ── Sidebar ────────────────────────────────────────────────────────────────
  dashboardSidebar(
    width = 250,
    useShinyjs(),
    use_waiter(),
    tags$head(tags$style(HTML(app_css))),
    
    # Credential note
    div(style = "padding:10px 14px 2px;",
        tags$small(style = "color:#475569;font-size:.68rem;line-height:1.5;",
                   "Credentials via env vars:",
                   tags$br(),
                   tags$code(style = "font-size:.62rem;color:#64748b;",
                             "DB_HOST DB_NAME DB_USER DB_PASSWORD"))
    ),
    
    # Connect
    div(style = "padding:4px 12px;",
        actionButton("connect_btn", "🔌  Connect / Refresh Projects",
                     class = "btn btn-block btn-green"),
        div(style = "margin-top:5px;text-align:center;",
            uiOutput("conn_status_ui"))
    ),
    
    tags$hr(class = "sidebar-divider"),
    
    # Project multiselect
    span(class = "sec-lbl", "Projects"),
    div(style = "padding:0 12px;",
        uiOutput("project_ui")
    ),
    
    div(style = "padding:0 12px;",
        numericInput("days_threshold",
                     "Highlight if outstanding ≥ (days)",
                     value = 3, min = 1, step = 1, width = "100%")
    ),
    
    div(style = "padding:4px 12px;",
        actionButton("load_btn", "📥  Load Data",
                     class = "btn btn-block btn-green"),
        uiOutput("last_updated_ui")
    ),
    
    tags$hr(class = "sidebar-divider"),
    
    # ── ID Upload Filter (NEW) ─────────────────────────────────────────────
    span(class = "sec-lbl", "Filter by Farm IDs"),
    div(style = "padding:0 12px 4px;",
        fileInput("upload_ids",
                  label    = NULL,
                  accept   = c(".csv", ".xlsx", ".xls"),
                  placeholder = "Upload CSV / Excel…",
                  buttonLabel = tags$span("📂 Browse")),
        uiOutput("upload_status_ui"),
        actionButton("clear_upload", "✕  Clear ID Filter",
                     class = "btn btn-block btn-dl",
                     style = "margin-top:2px;")
    ),
    
    tags$hr(class = "sidebar-divider"),
    
    # Shapefiles
    span(class = "sec-lbl", "Shapefiles (.rds)"),
    div(style = "padding:0 12px;",
        selectInput("gadm_rds",    "GADM Level-2",  choices = list_rds("gadm_data"),    selected = NULL),
        selectInput("cluster_rds", "Cluster / UAI", choices = list_rds("cluster_data"), selected = NULL)
    ),
    div(style = "padding:0 12px 8px;",
        actionButton("btn_spatial", "⟳  Run Spatial Analysis",
                     class = "btn btn-block btn-blue")
    ),
    
    tags$hr(class = "sidebar-divider"),
    
    sidebarMenu(id = "tabs",
                menuItem("🖼️  Image QC", icon = icon("images"),
                         menuSubItem("QC Overview",    tabName = "tab_qc_ov",  icon = icon("chart-pie")),
                         menuSubItem("Missing Images", tabName = "tab_missing",icon = icon("triangle-exclamation")),
                         menuSubItem("URL Check",      tabName = "tab_url",    icon = icon("link")),
                         menuSubItem("By Enumerator",  tabName = "tab_enum",   icon = icon("user-group")),
                         menuSubItem("By Project",     tabName = "tab_proj",   icon = icon("folder"))
                ),
                menuItem("📡  Field Monitor", icon = icon("satellite-dish"),
                         menuSubItem("Field Overview",  tabName = "tab_field_ov", icon = icon("gauge")),
                         menuSubItem("Visit Distances", tabName = "tab_vd",       icon = icon("route")),
                         menuSubItem("CCE Distances",   tabName = "tab_cd",       icon = icon("ruler")),
                         menuSubItem("Box Distances",   tabName = "tab_box_dist", icon = icon("arrows-left-right")),
                         menuSubItem("Duplicate GPS",   tabName = "tab_dup",      icon = icon("copy")),
                         menuSubItem("UAI / AEZ",       tabName = "tab_uai",      icon = icon("map-pin")),
                         menuSubItem("Map",             tabName = "tab_map",      icon = icon("map")),
                         menuSubItem("Raw Data",        tabName = "tab_raw",      icon = icon("table"))
                )
    )
  ),
  
  # ── Body ───────────────────────────────────────────────────────────────────
  dashboardBody(
    tabItems(
      
      # ── QC OVERVIEW ─────────────────────────────────────────────────────────
      tabItem("tab_qc_ov",
              uiOutput("filter_active_banner_qc"),
              fluidRow(
                valueBoxOutput("vb_total_cuts",  width = 3),
                valueBoxOutput("vb_total_miss",  width = 3),
                valueBoxOutput("vb_affected",    width = 3),
                valueBoxOutput("vb_enum_issues", width = 3)
              ),
              fluidRow(
                valueBoxOutput("vb_avg_days", width = 3),
                valueBoxOutput("vb_css_miss", width = 3),
                valueBoxOutput("vb_wet_miss", width = 3),
                valueBoxOutput("vb_dry_miss", width = 3)
              ),
              fluidRow(
                box(width = 6, title = "Missing Images by Survey Section",          status = "success",
                    plotlyOutput("chart_section",  height = "260px")),
                box(width = 6, title = "Top 10 Questions with Most Missing Images", status = "success",
                    plotlyOutput("chart_question", height = "260px"))
              ),
              fluidRow(
                box(width = 6, title = "Top 10 Enumerators with Missing Images", status = "primary",
                    plotlyOutput("chart_enum",     height = "260px")),
                box(width = 6, title = "Missing Images by Submission Date",      status = "primary",
                    plotlyOutput("chart_timeline", height = "260px"))
              )
      ),
      
      # ── MISSING IMAGES ───────────────────────────────────────────────────────
      tabItem("tab_missing",
              uiOutput("filter_active_banner_miss"),
              box(width = 12, status = "danger",
                  title = div(class = "d-flex justify-content-between align-items-center",
                              span("Missing Images Detail"),
                              downloadButton("dl_missing", "⬇️ Excel", class = "btn btn-sm btn-dl")),
                  fluidRow(
                    column(3, uiOutput("filter_section_ui")),
                    column(3, uiOutput("filter_supervisor_ui")),
                    column(3, uiOutput("filter_enumerator_ui")),
                    column(3, uiOutput("filter_uai_ui"))
                  ),
                  DTOutput("tbl_missing")
              )
      ),
      
      # ── URL CHECK ────────────────────────────────────────────────────────────
      tabItem("tab_url",
              box(width = 12, status = "warning",
                  title = "Image URL Accessibility Check — AWS S3",
                  p(style = "color:#64748b;font-size:.81rem;",
                    "Sends an authenticated HEAD request to S3 for every submitted image URL.",
                    " Set ", tags$code("AWS_ACCESS_KEY_ID"), " and ",
                    tags$code("AWS_SECRET_ACCESS_KEY"), " in environment variables before running."),
                  fluidRow(
                    column(3, actionButton("validate_btn", "🚀 Start Validation",
                                           class = "btn btn-warning")),
                    column(9, selectizeInput("url_status_filter", "Filter results by status",
                                             choices  = c("All","ok","http_404","http_403",
                                                          "invalid_s3_format","unreachable","no_url"),
                                             selected = "All", multiple = TRUE,
                                             options  = list(placeholder = "All statuses…",
                                                             plugins = list("remove_button"))))
                  ),
                  tags$hr(style = "border-color:#2e3f6e;"),
                  uiOutput("url_kpi_row"),
                  br(),
                  DTOutput("tbl_urls"),
                  br(),
                  downloadButton("dl_urls", "⬇️ Download Filtered Report (.xlsx)",
                                 class = "btn btn-dl btn-sm")
              )
      ),
      
      # ── BY ENUMERATOR ────────────────────────────────────────────────────────
      tabItem("tab_enum",
              uiOutput("filter_active_banner_enum"),
              box(width = 12, status = "primary",
                  title = div(class = "d-flex justify-content-between align-items-center",
                              span("Per-Enumerator / Supervisor Breakdown"),
                              downloadButton("dl_enum", "⬇️ Excel", class = "btn btn-sm btn-dl")),
                  DTOutput("tbl_enum")
              )
      ),
      
      # ── BY PROJECT ───────────────────────────────────────────────────────────
      tabItem("tab_proj",
              box(width = 12, status = "success",
                  title = "Missing Images per Project",
                  DTOutput("tbl_project"))
      ),
      
      # ── FIELD OVERVIEW ───────────────────────────────────────────────────────
      tabItem("tab_field_ov",
              uiOutput("filter_active_banner_field"),
              fluidRow(
                valueBoxOutput("vb_total_resp", width = 3),
                valueBoxOutput("vb_cces",       width = 3),
                valueBoxOutput("vb_v1",         width = 3),
                valueBoxOutput("vb_v2",         width = 3)
              ),
              fluidRow(
                valueBoxOutput("vb_v3",      width = 3),
                valueBoxOutput("vb_miss_v1", width = 3),
                valueBoxOutput("vb_miss_v2", width = 3),
                valueBoxOutput("vb_miss_v3", width = 3)
              ),
              fluidRow(
                box(width = 6, title = "CCEs by UAI",          status = "success",
                    plotlyOutput("plot_uai",        height = 300)),
                box(width = 6, title = "CCEs by Crop",         status = "success",
                    plotlyOutput("plot_crop",       height = 300))
              ),
              fluidRow(
                box(width = 6, title = "CCEs by Field Agent",     status = "primary",
                    plotlyOutput("plot_enum_field", height = 300)),
                box(width = 6, title = "Visit Completion by UAI", status = "primary",
                    plotlyOutput("plot_visits",     height = 300))
              )
      ),
      
      # ── VISIT DISTANCES ──────────────────────────────────────────────────────
      tabItem("tab_vd",
              div(class = "thr-panel",
                  tags$h5("Adjust thresholds — KPIs & table update live"),
                  fluidRow(
                    column(4, sliderInput("thr_v1v2", "V1 ↔ V2  flag if > (m)",
                                          min = 0, max = 5000,  value = 300,  step = 50)),
                    column(4, sliderInput("thr_v1v3", "V1 ↔ V3  flag if > (m)",
                                          min = 0, max = 10000, value = 3000, step = 100)),
                    column(4, sliderInput("thr_v2v3", "V2 ↔ V3  flag if > (m)",
                                          min = 0, max = 10000, value = 3000, step = 100))
                  )
              ),
              fluidRow(
                valueBoxOutput("vb_vd_flags", width = 4),
                valueBoxOutput("vb_vd_cces",  width = 4),
                valueBoxOutput("vb_vd_fas",   width = 4)
              ),
              box(width = 12, title = "⚠️  Visit Distance Flags", status = "warning",
                  div(style = "margin-bottom:8px;",
                      downloadButton("dl_visits", "⬇ Export Excel", class = "btn btn-dl")),
                  DTOutput("tbl_visits"))
      ),
      
      # ── CCE DISTANCES ────────────────────────────────────────────────────────
      tabItem("tab_cd",
              div(class = "thr-panel",
                  tags$h5("Adjust threshold — KPIs & table update live"),
                  fluidRow(
                    column(6, sliderInput("thr_cce", "Flag CCE pairs closer than (m)",
                                          min = 0, max = 5000, value = 1000, step = 50))
                  )
              ),
              fluidRow(
                valueBoxOutput("vb_cd_pairs", width = 4),
                valueBoxOutput("vb_cd_cces",  width = 4),
                valueBoxOutput("vb_cd_fas",   width = 4)
              ),
              box(width = 12, status = "danger",
                  title = "🔴  CCE Pairs Within Threshold (same UAI & crop, Visit 1)",
                  div(style = "margin-bottom:8px;",
                      downloadButton("dl_cce", "⬇ Export Excel", class = "btn btn-dl")),
                  DTOutput("tbl_cce"))
      ),
      
      # ── BOX DISTANCES (NEW) ──────────────────────────────────────────────────
      tabItem("tab_box_dist",
              div(class = "thr-panel",
                  tags$h5("Box-to-box distance within each CCE (Visit 1 / CSS)"),
                  p(style = "color:#94a3b8;font-size:.82rem;margin:0 0 12px;",
                    "Standard separation: ", tags$strong(style = "color:#34d399;", "10 m"),
                    " | Maximum allowance: ", tags$strong(style = "color:#fcd34d;", "30 m"),
                    " | Flag all CCEs where box distance exceeds the slider threshold."),
                  fluidRow(
                    column(6,
                           sliderInput("thr_box", "Flag if box distance > (m)",
                                       min = 0, max = 100, value = 30, step = 1,
                                       post = " m")),
                    column(6,
                           div(style = "padding-top:24px;",
                               tags$small(style = "color:#64748b;",
                                          "Move slider below 10 m to see all CCEs outside standard range.",
                                          tags$br(),
                                          "Move to 30 m to flag only those exceeding the maximum allowance.")))
                  )
              ),
              fluidRow(
                valueBoxOutput("vb_bd_flagged",  width = 3),
                valueBoxOutput("vb_bd_over30",   width = 3),
                valueBoxOutput("vb_bd_avg_dist", width = 3),
                valueBoxOutput("vb_bd_fas",      width = 3)
              ),
              fluidRow(
                box(width = 6, title = "Box Distance Distribution (m)", status = "warning",
                    plotlyOutput("plot_box_hist", height = 280)),
                box(width = 6, title = "Top 15 Enumerators — Flagged CCEs", status = "warning",
                    plotlyOutput("plot_box_enum", height = 280))
              ),
              box(width = 12, status = "danger",
                  title = div(class = "d-flex justify-content-between align-items-center",
                              span("📏  Box Distance Flags"),
                              downloadButton("dl_box_dist", "⬇ Export Excel",
                                             class = "btn btn-sm btn-dl")),
                  DTOutput("tbl_box_dist"))
      ),
      
      # ── DUPLICATE GPS ────────────────────────────────────────────────────────
      tabItem("tab_dup",
              fluidRow(
                valueBoxOutput("vb_dup_total", width = 3),
                valueBoxOutput("vb_dup_v1",   width = 3),
                valueBoxOutput("vb_dup_v2",   width = 3),
                valueBoxOutput("vb_dup_v3",   width = 3)
              ),
              box(width = 12, status = "warning",
                  title = "📍  Exact Duplicate GPS Points — all visits",
                  div(style = "margin-bottom:8px;",
                      downloadButton("dl_dup", "⬇ Export Excel", class = "btn btn-dl")),
                  DTOutput("tbl_dup"))
      ),
      
      # ── UAI / AEZ ────────────────────────────────────────────────────────────
      tabItem("tab_uai",
              fluidRow(
                valueBoxOutput("vb_uai_total", width = 3),
                valueBoxOutput("vb_uai_pct",   width = 3),
                valueBoxOutput("vb_uai_avg_d", width = 3),
                valueBoxOutput("vb_uai_neigh", width = 3)
              ),
              fluidRow(
                box(width = 6, title = "Mismatches by UAI",         status = "warning",
                    plotlyOutput("plot_uai_mm", height = 300)),
                box(width = 6, title = "Mismatches by Field Agent",  status = "warning",
                    plotlyOutput("plot_fa_mm",  height = 300))
              ),
              box(width = 12, title = "📋  UAI Mismatch Detail", status = "warning",
                  div(style = "margin-bottom:8px;",
                      downloadButton("dl_spatial", "⬇ Export Excel", class = "btn btn-dl")),
                  DTOutput("tbl_spatial"))
      ),
      
      # ── MAP ──────────────────────────────────────────────────────────────────
      tabItem("tab_map",
              box(width = 12, title = "Field Points Map", status = "success",
                  div(style = "margin-bottom:10px;",
                      fluidRow(
                        column(3, selectizeInput("map_visit", "Visit",
                                                 choices = c("All","1","2","3"), selected = "All")),
                        column(3, selectizeInput("map_uai_f", "UAI",
                                                 choices = c("All"), selected = "All")),
                        column(3, selectizeInput("map_fa",    "Field Agent",
                                                 choices = c("All"), selected = "All")),
                        column(3, selectizeInput("map_crop",  "Crop",
                                                 choices = c("All"), selected = "All"))
                      )
                  ),
                  leafletOutput("map_main", height = 560))
      ),
      
      # ── RAW DATA ─────────────────────────────────────────────────────────────
      tabItem("tab_raw",
              uiOutput("filter_active_banner_raw"),
              box(width = 12, title = "Raw Field Monitor Results", status = "primary",
                  div(style = "margin-bottom:8px;",
                      downloadButton("dl_raw", "⬇ Export Excel", class = "btn btn-dl")),
                  DTOutput("tbl_raw"))
      )
    )
  )
)

# ==============================================================================
# 9.  SERVER
# ==============================================================================

server <- function(input, output, session) {
  
  w <- Waiter$new(
    html  = tagList(
      spin_ripple(),
      h4("Loading data…",
         style = "color:#34d399;margin-top:16px;font-family:'Space Mono',monospace;")
    ),
    color = "rgba(20,27,45,.94)"
  )
  
  rv <- reactiveValues(
    conn          = NULL,
    projects      = NULL,
    filter_ids    = NULL,   # ← uploaded ID filter
    # Image QC
    img_processed = NULL,
    missing       = NULL,
    url_df        = NULL,
    url_results   = NULL,
    # Field monitor
    raw_monitor   = NULL,
    spatial       = NULL,
    last_updated  = NULL
  )
  
  # ── CONNECT ────────────────────────────────────────────────────────────────
  observeEvent(input$connect_btn, {
    if (conn_ok(rv$conn)) try(dbDisconnect(rv$conn), silent = TRUE)
    rv$conn <- make_conn()
    if (conn_ok(rv$conn)) {
      rv$projects <- get_projects(rv$conn)
      showNotification("✅ Connected to database", type = "message")
    } else {
      showNotification("❌ Connection failed — check DB_* environment variables",
                       type = "error", duration = 10)
    }
  })
  
  output$conn_status_ui <- renderUI({
    if (conn_ok(rv$conn))
      tags$span(class = "pill pill-ok",   "● Connected")
    else
      tags$span(class = "pill pill-idle", "● Not connected")
  })
  
  # ── PROJECT MULTISELECT ────────────────────────────────────────────────────
  output$project_ui <- renderUI({
    req(rv$projects)
    choices <- setNames(as.character(rv$projects$id),
                        paste0("[", rv$projects$id, "] ", rv$projects$name))
    selectizeInput("project_ids", NULL,
                   choices  = choices,
                   multiple = TRUE,
                   selected = choices[1],
                   options  = list(placeholder = "Select one or more projects…",
                                   plugins     = list("remove_button")))
  })
  
  output$last_updated_ui <- renderUI({
    req(rv$last_updated)
    tags$small(style = "color:#475569;font-size:.67rem;display:block;margin-top:6px;text-align:center;",
               "Loaded: ", format(rv$last_updated, "%Y-%m-%d %H:%M"))
  })
  
  # ── SHAPEFILE DROPDOWNS ────────────────────────────────────────────────────
  observe({
    updateSelectInput(session, "gadm_rds",    choices = list_rds("gadm_data"))
    updateSelectInput(session, "cluster_rds", choices = list_rds("cluster_data"))
  })
  
  # ── ID UPLOAD FILTER (NEW) ─────────────────────────────────────────────────
  observeEvent(input$upload_ids, {
    req(input$upload_ids)
    file <- input$upload_ids
    ext  <- tolower(tools::file_ext(file$name))
    tryCatch({
      df <- if (ext == "csv") {
        read.csv(file$datapath, stringsAsFactors = FALSE)
      } else {
        readxl::read_excel(file$datapath)
      }
      # Auto-detect the boxes_pula_id column
      col <- grep("boxes_pula_id|pula_id|farm.?id|box.?id",
                  names(df), ignore.case = TRUE, value = TRUE)
      col <- if (length(col) > 0) col[1] else names(df)[1]
      ids <- unique(as.character(df[[col]]))
      ids <- ids[!is.na(ids) & nzchar(ids)]
      rv$filter_ids <- ids
      showNotification(
        sprintf("✅ ID filter loaded — %d farm IDs from column '%s'", length(ids), col),
        type = "message"
      )
    }, error = function(e) {
      showNotification(paste("Upload error:", conditionMessage(e)), type = "error")
    })
  })
  
  observeEvent(input$clear_upload, {
    rv$filter_ids <- NULL
    # Reset the file input widget
    reset("upload_ids")
    showNotification("ID filter cleared — showing all records.", type = "message")
  })
  
  output$upload_status_ui <- renderUI({
    if (!is.null(rv$filter_ids) && length(rv$filter_ids) > 0) {
      div(class = "filter-active-banner",
          sprintf("🔍 Filtering: %d farm IDs active", length(rv$filter_ids)))
    } else {
      NULL
    }
  })
  
  # ── Helper: apply ID filter to any data frame ─────────────────────────────
  apply_id_filter <- function(data) {
    if (is.null(data) || is.null(rv$filter_ids) || length(rv$filter_ids) == 0) return(data)
    data %>% filter(boxes_pula_id %in% rv$filter_ids)
  }
  
  # ── Banner helper ──────────────────────────────────────────────────────────
  id_filter_banner <- function() {
    if (!is.null(rv$filter_ids) && length(rv$filter_ids) > 0) {
      div(class = "filter-active-banner",
          style = "margin-bottom:12px;",
          sprintf("🔍 ID Filter Active — showing %d farm IDs only", length(rv$filter_ids)))
    } else NULL
  }
  
  output$filter_active_banner_qc    <- renderUI(id_filter_banner())
  output$filter_active_banner_miss  <- renderUI(id_filter_banner())
  output$filter_active_banner_enum  <- renderUI(id_filter_banner())
  output$filter_active_banner_field <- renderUI(id_filter_banner())
  output$filter_active_banner_raw   <- renderUI(id_filter_banner())
  
  # ── LOAD DATA ──────────────────────────────────────────────────────────────
  observeEvent(input$load_btn, {
    req(conn_ok(rv$conn), input$project_ids)
    w$show()
    tryCatch({
      # 1) Image QC
      raw_img <- fetch_image_data(rv$conn, input$project_ids)
      if (!is.null(raw_img) && nrow(raw_img) > 0) {
        rv$img_processed <- process_image_data(raw_img)
        rv$missing       <- build_missing(rv$img_processed$css,
                                          rv$img_processed$wet,
                                          rv$img_processed$dry)
        rv$url_df        <- run_url_validation(rv$img_processed$image_links)
        rv$url_results   <- NULL
      } else {
        showNotification("⚠️ No image QC data for selected project(s).", type = "warning")
      }
      
      # 2) Field monitor
      raw_mon <- fetch_monitoring_data(rv$conn, input$project_ids)
      if (!is.null(raw_mon) && nrow(raw_mon) > 0) {
        rv$raw_monitor <- clean_coords(raw_mon)
        updateSelectizeInput(session, "map_uai_f",
                             choices = c("All", sort(unique(na.omit(rv$raw_monitor$uai)))))
        updateSelectizeInput(session, "map_fa",
                             choices = c("All", sort(unique(na.omit(rv$raw_monitor$enumerator_name)))))
        updateSelectizeInput(session, "map_crop",
                             choices = c("All", sort(unique(na.omit(rv$raw_monitor$crop)))))
      } else {
        showNotification("⚠️ No field monitor data for selected project(s).", type = "warning")
      }
      
      rv$last_updated <- Sys.time()
      showNotification(
        sprintf("✅ Loaded  %d image rows | %d monitor rows | %d project(s)",
                if (!is.null(raw_img)) nrow(raw_img) else 0,
                if (!is.null(raw_mon)) nrow(raw_mon) else 0,
                length(input$project_ids)),
        type = "message"
      )
    }, error = function(e) {
      showNotification(paste("Error loading data:", conditionMessage(e)),
                       type = "error", duration = 10)
    })
    w$hide()
  })
  
  # ── SPATIAL ANALYSIS ───────────────────────────────────────────────────────
  observeEvent(input$btn_spatial, {
    req(rv$raw_monitor)
    if (is.null(input$gadm_rds)    || !nzchar(input$gadm_rds))    { showNotification("Please select a GADM shapefile.",    type = "error"); return(NULL) }
    if (is.null(input$cluster_rds) || !nzchar(input$cluster_rds)) { showNotification("Please select a Cluster shapefile.", type = "error"); return(NULL) }
    gadm_path    <- file.path("gadm_data",    input$gadm_rds)
    cluster_path <- file.path("cluster_data", input$cluster_rds)
    if (!file.exists(gadm_path))    { showNotification(paste("File not found:", gadm_path),    type = "error"); return(NULL) }
    if (!file.exists(cluster_path)) { showNotification(paste("File not found:", cluster_path), type = "error"); return(NULL) }
    w$show()
    tryCatch({
      rv$spatial <- run_spatial(rv$raw_monitor, readRDS(gadm_path), readRDS(cluster_path))
      showNotification("✔  Spatial analysis complete!", type = "message", duration = 4)
    }, error = function(e) {
      showNotification(paste("Spatial Error:", conditionMessage(e)), type = "error", duration = 10)
    })
    w$hide()
  })
  
  # ── PLOTLY DARK THEME ──────────────────────────────────────────────────────
  dark_ly <- function(p) {
    p %>% layout(
      paper_bgcolor = "#141b2d",
      plot_bgcolor  = "#141b2d",
      font  = list(color = "#94a3b8", family = "DM Sans"),
      xaxis = list(gridcolor = "#2e3f6e", zerolinecolor = "#2e3f6e"),
      yaxis = list(gridcolor = "#2e3f6e", zerolinecolor = "#2e3f6e")
    )
  }
  
  # ════════════════════════════════════════════════════════════════════════════
  # FILTERED DATA REACTIVES (apply ID filter)
  # ════════════════════════════════════════════════════════════════════════════
  
  filtered_missing <- reactive({
    apply_id_filter(rv$missing)
  })
  
  filtered_monitor <- reactive({
    apply_id_filter(rv$raw_monitor)
  })
  
  # ════════════════════════════════════════════════════════════════════════════
  # IMAGE QC OUTPUTS
  # ════════════════════════════════════════════════════════════════════════════
  
  output$vb_total_cuts <- renderValueBox({
    n <- if (!is.null(rv$img_processed)) {
      d <- apply_id_filter(rv$img_processed$full)
      n_distinct(d$cropcut_id, na.rm = TRUE)
    } else 0
    valueBox(n, "Total Cropcuts", icon("clipboard-list"), color = "green")
  })
  output$vb_total_miss <- renderValueBox({
    n <- if (!is.null(filtered_missing())) nrow(filtered_missing()) else 0
    valueBox(n, "Missing Images", icon("image"), color = "red")
  })
  output$vb_affected <- renderValueBox({
    n <- if (!is.null(filtered_missing())) n_distinct(filtered_missing()$boxes_pula_id, na.rm = TRUE) else 0
    valueBox(n, "Affected Farms", icon("house"), color = "orange")
  })
  output$vb_enum_issues <- renderValueBox({
    n <- if (!is.null(filtered_missing())) n_distinct(filtered_missing()$enumerator_name, na.rm = TRUE) else 0
    valueBox(n, "Enumerators Affected", icon("person"), color = "yellow")
  })
  output$vb_avg_days <- renderValueBox({
    n <- if (!is.null(filtered_missing()) && nrow(filtered_missing()) > 0)
      round(mean(filtered_missing()$`Days Since Submission`, na.rm = TRUE), 1) else 0
    valueBox(n, "Avg Days Outstanding", icon("clock"), color = "blue")
  })
  output$vb_css_miss <- renderValueBox({
    n <- if (!is.null(filtered_missing())) nrow(filtered_missing() %>% filter(section == "CSS")) else 0
    valueBox(n, "CSS Missing", icon("file-circle-xmark"), color = "red")
  })
  output$vb_wet_miss <- renderValueBox({
    n <- if (!is.null(filtered_missing())) nrow(filtered_missing() %>% filter(section == "Wet Harvest")) else 0
    valueBox(n, "Wet Harvest Missing", icon("droplet"), color = "blue")
  })
  output$vb_dry_miss <- renderValueBox({
    n <- if (!is.null(filtered_missing())) nrow(filtered_missing() %>% filter(section == "Dry Harvest")) else 0
    valueBox(n, "Dry Harvest Missing", icon("sun"), color = "orange")
  })
  
  output$chart_section <- renderPlotly({
    req(filtered_missing())
    d <- filtered_missing() %>% count(section)
    pal <- c(CSS = "#34d399", `Wet Harvest` = "#60a5fa", `Dry Harvest` = "#fbbf24")
    plot_ly(d, x = ~section, y = ~n, color = ~section,
            colors = unname(pal[d$section]), type = "bar") %>%
      layout(showlegend = FALSE, xaxis = list(title = ""),
             yaxis = list(title = "Count"), margin = list(t = 10)) %>% dark_ly()
  })
  output$chart_question <- renderPlotly({
    req(filtered_missing())
    d <- filtered_missing() %>% count(question_id, sort = TRUE) %>% head(10)
    plot_ly(d, x = ~n, y = ~reorder(question_id, n),
            type = "bar", orientation = "h", marker = list(color = "#60a5fa")) %>%
      layout(xaxis = list(title = "Count"), yaxis = list(title = ""),
             margin = list(l = 220, t = 10)) %>% dark_ly()
  })
  output$chart_enum <- renderPlotly({
    req(filtered_missing())
    d <- filtered_missing() %>% filter(!is.na(enumerator_name)) %>%
      count(enumerator_name, sort = TRUE) %>% head(10)
    plot_ly(d, x = ~n, y = ~reorder(enumerator_name, n),
            type = "bar", orientation = "h", marker = list(color = "#fbbf24")) %>%
      layout(xaxis = list(title = "Missing Images"), yaxis = list(title = ""),
             margin = list(l = 160, t = 10)) %>% dark_ly()
  })
  output$chart_timeline <- renderPlotly({
    req(filtered_missing())
    d <- filtered_missing() %>% mutate(date = as.Date(end_time)) %>%
      filter(!is.na(date)) %>% count(date)
    plot_ly(d, x = ~date, y = ~n, type = "scatter", mode = "lines+markers",
            line = list(color = "#34d399"), marker = list(color = "#34d399")) %>%
      layout(xaxis = list(title = "Submission Date"),
             yaxis = list(title = "Count"), margin = list(t = 10)) %>% dark_ly()
  })
  
  # ── Missing images filters ─────────────────────────────────────────────────
  output$filter_section_ui <- renderUI({
    req(filtered_missing())
    selectInput("filter_section", "Section",
                choices = c("All", sort(unique(filtered_missing()$section))), selected = "All")
  })
  output$filter_supervisor_ui <- renderUI({
    req(filtered_missing())
    selectInput("filter_supervisor", "Supervisor",
                choices = c("All", sort(na.omit(unique(filtered_missing()$supervisor_name)))),
                selected = "All")
  })
  output$filter_enumerator_ui <- renderUI({
    req(filtered_missing())
    selectInput("filter_enumerator", "Enumerator",
                choices = c("All", sort(na.omit(unique(filtered_missing()$enumerator_name)))),
                selected = "All")
  })
  output$filter_uai_ui <- renderUI({
    req(filtered_missing())
    selectInput("filter_uai", "UAI",
                choices = c("All", sort(na.omit(unique(filtered_missing()$uai)))),
                selected = "All")
  })
  
  missing_filtered <- reactive({
    req(filtered_missing())
    d <- filtered_missing()
    if (!is.null(input$filter_section)    && input$filter_section    != "All")
      d <- d %>% filter(section         == input$filter_section)
    if (!is.null(input$filter_supervisor) && input$filter_supervisor != "All")
      d <- d %>% filter(supervisor_name == input$filter_supervisor)
    if (!is.null(input$filter_enumerator) && input$filter_enumerator != "All")
      d <- d %>% filter(enumerator_name == input$filter_enumerator)
    if (!is.null(input$filter_uai)        && input$filter_uai        != "All")
      d <- d %>% filter(uai             == input$filter_uai)
    d
  })
  
  output$tbl_missing <- renderDT({
    req(missing_filtered())
    thresh  <- if (is.null(input$days_threshold) || is.na(input$days_threshold)) 3 else input$days_threshold
    display <- missing_filtered() %>%
      select(-any_of("position")) %>%
      arrange(desc(`Days Since Submission`))
    datatable(display, rownames = FALSE,
              extensions = "Buttons",
              options    = list(pageLength = 25, scrollX = TRUE, dom = "Bfrtip",
                                buttons = c("copy","csv")),
              class = "table-striped table-hover table-sm") %>%
      formatStyle("Days Since Submission",
                  backgroundColor = styleInterval(
                    c(thresh - 0.01, thresh * 2),
                    c("#14532d", "#78350f", "#7f1d1d")
                  ),
                  color = "#f8fafc")
  })
  output$dl_missing <- downloadHandler(
    filename = function() paste0("missing_images_", Sys.Date(), ".xlsx"),
    content  = function(file) write_xlsx(missing_filtered(), file)
  )
  
  # ── URL validation ─────────────────────────────────────────────────────────
  observeEvent(input$validate_btn, {
    req(rv$url_df)
    if (!nzchar(Sys.getenv("AWS_ACCESS_KEY_ID")) ||
        !nzchar(Sys.getenv("AWS_SECRET_ACCESS_KEY"))) {
      showModal(modalDialog(
        title = "⚠️ AWS Credentials Missing",
        tags$p("Set these environment variables before running URL validation:"),
        tags$ul(
          tags$li(tags$code("AWS_ACCESS_KEY_ID")),
          tags$li(tags$code("AWS_SECRET_ACCESS_KEY")),
          tags$li(tags$code("AWS_DEFAULT_REGION"), " (optional — defaults to eu-west-1)"),
          tags$li(tags$code("AWS_SESSION_TOKEN"),  " (optional)")
        ),
        easyClose = TRUE, footer = modalButton("Close")
      ))
      return()
    }
    df <- rv$url_df; n <- nrow(df)
    if (n == 0) { showNotification("No URLs to validate.", type = "warning"); return() }
    statuses <- character(n)
    withProgress(message = "Checking S3 objects…", value = 0, {
      for (i in seq_len(n)) {
        incProgress(1/n, detail = sprintf("[%d/%d]  %s", i, n, basename(df$url[i])))
        statuses[i] <- check_s3_url(df$url[i])
      }
    })
    status_labels <- c(
      ok                = "✅ Accessible",
      http_403          = "🔒 Access Denied (403)",
      http_404          = "❌ Not Found (404)",
      invalid_s3_format = "⚠️ Invalid URL Format",
      unreachable       = "🌐 Network Unreachable",
      no_url            = "— No URL"
    )
    rv$url_results <- df %>%
      mutate(status       = statuses,
             status_label = dplyr::recode(status, !!!status_labels, .default = paste0("❓ ", status)),
             accessible   = (status == "ok"),
             bucket_ok    = (!bucket_mismatch | is.na(bucket_mismatch))) %>%
      select(boxes_pula_id, position, question_id,
             url, parsed_bucket, bucket_ok, status, status_label, accessible)
    n_ok  <- sum(rv$url_results$status == "ok")
    n_403 <- sum(rv$url_results$status == "http_403")
    n_404 <- sum(rv$url_results$status == "http_404")
    showNotification(sprintf("✅ Done — %d ok | %d 404 | %d 403 | %d other",
                             n_ok, n_404, n_403, n - n_ok - n_403 - n_404),
                     type = "message", duration = 10)
  })
  
  url_filtered <- reactive({
    req(rv$url_results)
    d   <- rv$url_results
    sel <- input$url_status_filter
    if (!is.null(sel) && length(sel) > 0 && !"All" %in% sel)
      d <- d %>% filter(status %in% sel)
    d
  })
  
  output$url_kpi_row <- renderUI({
    req(rv$url_results); d <- rv$url_results; total <- nrow(d)
    n_ok     <- sum(d$status == "ok")
    n_403    <- sum(d$status == "http_403")
    n_404    <- sum(d$status == "http_404")
    n_format <- sum(d$status == "invalid_s3_format")
    n_reach  <- sum(d$status == "unreachable")
    pct_ok   <- round(100 * n_ok / max(total, 1), 1)
    fluidRow(
      valueBox(total,                             "Checked",           icon("link"),                 color = "navy",   width = 2),
      valueBox(paste0(n_ok, " (", pct_ok, "%)"), "Accessible",        icon("circle-check"),         color = "green",  width = 2),
      valueBox(n_404,                             "Not Found (404)",   icon("file-circle-xmark"),    color = "red",    width = 2),
      valueBox(n_403,                             "Access Denied",     icon("lock"),                 color = "orange", width = 2),
      valueBox(n_format,                          "Bad URL Format",    icon("triangle-exclamation"), color = "yellow", width = 2),
      valueBox(n_reach,                           "Unreachable",       icon("wifi"),                 color = "black",  width = 2)
    )
  })
  
  output$tbl_urls <- renderDT({
    req(url_filtered())
    status_bg <- c(ok = "#14532d", http_403 = "#78350f", http_404 = "#7f1d1d",
                   invalid_s3_format = "#312e81", unreachable = "#1e3a5f", no_url = "#1e2a4a")
    display <- url_filtered() %>%
      select(boxes_pula_id, position, question_id, bucket_ok, status, status_label, accessible, url)
    datatable(display, rownames = FALSE,
              colnames = c("Farm ID","Position","Question","Bucket OK","Status Code","Status Detail","Accessible","URL"),
              extensions = "Buttons",
              options    = list(pageLength = 25, scrollX = TRUE, dom = "Bfrtip",
                                buttons = c("copy","csv"),
                                columnDefs = list(list(targets = 7,
                                                       render = JS("function(d){return d?'<a href=\"'+d+'\" target=\"_blank\" style=\"color:#60a5fa;\">🔗 view</a>':'—';}")))),
              escape = FALSE, class = "table-sm table-striped table-hover") %>%
      formatStyle("status",         backgroundColor = styleEqual(names(status_bg), unname(status_bg)), color = "#f8fafc") %>%
      formatStyle("accessible",     color = styleEqual(c(TRUE, FALSE), c("#6ee7b7","#fca5a5")), fontWeight = "bold") %>%
      formatStyle("bucket_ok",      backgroundColor = styleEqual(c(TRUE, FALSE), c("transparent","#78350f")))
  })
  output$dl_urls <- downloadHandler(
    filename = function() paste0("s3_url_validation_", Sys.Date(), ".xlsx"),
    content  = function(file) { req(url_filtered()); write_xlsx(url_filtered() %>% select(-status_label), file) }
  )
  
  # ── Enumerator summary ─────────────────────────────────────────────────────
  enum_summary <- reactive({
    req(filtered_missing())
    filtered_missing() %>%
      group_by(project_name, supervisor_name, enumerator_name, enumerator_phone) %>%
      summarise(
        `Total Missing`        = n(),
        `Distinct Farms`       = n_distinct(boxes_pula_id, na.rm = TRUE),
        `CSS Missing`          = sum(section == "CSS"),
        `Wet Harvest Missing`  = sum(section == "Wet Harvest"),
        `Dry Harvest Missing`  = sum(section == "Dry Harvest"),
        `Avg Days Outstanding` = round(mean(`Days Since Submission`, na.rm = TRUE), 1),
        `Max Days Outstanding` = max(`Days Since Submission`,  na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(`Total Missing`))
  })
  
  output$tbl_enum <- renderDT({
    req(enum_summary())
    datatable(enum_summary(), rownames = FALSE,
              options = list(pageLength = 25, scrollX = TRUE),
              class   = "table-sm table-striped table-hover") %>%
      formatStyle("Total Missing",
                  background         = styleColorBar(range(enum_summary()$`Total Missing`), "#7f1d1d"),
                  backgroundSize     = "100% 90%",
                  backgroundRepeat   = "no-repeat",
                  backgroundPosition = "center") %>%
      formatStyle("Max Days Outstanding",
                  color = styleInterval(c(7, 14), c("#e2e8f0","#fbbf24","#fca5a5")))
  })
  output$dl_enum <- downloadHandler(
    filename = function() paste0("enumerator_summary_", Sys.Date(), ".xlsx"),
    content  = function(file) { req(enum_summary()); write_xlsx(enum_summary(), file) }
  )
  
  # ── By Project ─────────────────────────────────────────────────────────────
  output$tbl_project <- renderDT({
    req(filtered_missing(), rv$img_processed)
    total_per_proj <- apply_id_filter(rv$img_processed$full) %>%
      group_by(project_name) %>%
      summarise(`Total Cropcuts` = n_distinct(cropcut_id, na.rm = TRUE), .groups = "drop")
    proj_summary <- filtered_missing() %>%
      group_by(project_name) %>%
      summarise(
        `Total Missing`        = n(),
        `Affected Farms`       = n_distinct(boxes_pula_id, na.rm = TRUE),
        `CSS Missing`          = sum(section == "CSS"),
        `Wet Harvest Missing`  = sum(section == "Wet Harvest"),
        `Dry Harvest Missing`  = sum(section == "Dry Harvest"),
        `Avg Days Outstanding` = round(mean(`Days Since Submission`, na.rm = TRUE), 1),
        .groups = "drop"
      ) %>%
      left_join(total_per_proj, by = "project_name") %>%
      mutate(`Missing per Cropcut` = round(`Total Missing` / `Total Cropcuts`, 2)) %>%
      arrange(desc(`Total Missing`))
    datatable(proj_summary, rownames = FALSE,
              options = list(pageLength = 25, scrollX = TRUE),
              class   = "table-sm table-striped table-hover")
  })
  
  # ════════════════════════════════════════════════════════════════════════════
  # FIELD MONITOR OUTPUTS
  # ════════════════════════════════════════════════════════════════════════════
  
  cce_level <- reactive({
    req(filtered_monitor())
    filtered_monitor() %>% distinct(crop, boxes_pula_id, uai, enumerator_name, .keep_all = FALSE)
  })
  
  missing_visit_n <- function(v) {
    d <- filtered_monitor()
    if (is.null(d)) return(0)
    if (v == 1) {
      need <- d %>% filter(position %in% c(2, 3)) %>% pull(boxes_pula_id) %>% unique()
      have <- d %>% filter(position == 1)          %>% pull(boxes_pula_id) %>% unique()
      length(setdiff(need, have))
    } else if (v == 2) {
      need <- d %>% filter(position == 3) %>% pull(boxes_pula_id) %>% unique()
      have <- d %>% filter(position == 2) %>% pull(boxes_pula_id) %>% unique()
      length(setdiff(need, have))
    } else {
      have <- d %>% filter(position == 3) %>% pull(boxes_pula_id) %>% unique()
      length(setdiff(unique(d$boxes_pula_id), have))
    }
  }
  
  # Field overview KPI boxes
  output$vb_total_resp <- renderValueBox({
    n <- if (!is.null(filtered_monitor())) nrow(filtered_monitor()) else 0
    valueBox(n, "Total Responses", icon("database"), color = "green")
  })
  output$vb_cces <- renderValueBox({
    n <- if (!is.null(filtered_monitor())) nrow(cce_level()) else 0
    valueBox(n, "Unique CCEs", icon("seedling"), color = "blue")
  })
  output$vb_v1 <- renderValueBox({
    n <- if (!is.null(filtered_monitor())) nrow(filter(filtered_monitor(), position == 1)) else 0
    valueBox(n, "V1 — CSS + Box Placement", icon("box"), color = "green")
  })
  output$vb_v2 <- renderValueBox({
    n <- if (!is.null(filtered_monitor())) nrow(filter(filtered_monitor(), position == 2)) else 0
    valueBox(n, "V2 — Wet Harvest", icon("droplet"), color = "blue")
  })
  output$vb_v3 <- renderValueBox({
    n <- if (!is.null(filtered_monitor())) nrow(filter(filtered_monitor(), position == 3)) else 0
    valueBox(n, "V3 — Dry Harvest", icon("sun"), color = "yellow")
  })
  output$vb_miss_v1 <- renderValueBox(
    valueBox(missing_visit_n(1), "CCEs Missing V1", icon("circle-xmark"), color = "red"))
  output$vb_miss_v2 <- renderValueBox(
    valueBox(missing_visit_n(2), "CCEs Missing V2", icon("circle-xmark"), color = "red"))
  output$vb_miss_v3 <- renderValueBox(
    valueBox(missing_visit_n(3), "CCEs Missing V3", icon("circle-xmark"), color = "red"))
  
  output$plot_uai <- renderPlotly({
    req(cce_level())
    d <- cce_level() %>% count(uai, sort = TRUE) %>% slice_head(n = 20)
    plot_ly(d, x = ~reorder(uai, n), y = ~n, type = "bar",
            marker = list(color = "#34d399")) %>%
      layout(xaxis = list(title = "UAI"), yaxis = list(title = "CCEs"),
             margin = list(b = 80)) %>% dark_ly()
  })
  output$plot_crop <- renderPlotly({
    req(cce_level())
    d <- cce_level() %>% count(crop, sort = TRUE)
    plot_ly(d, labels = ~crop, values = ~n, type = "pie",
            marker = list(colors = c("#34d399","#60a5fa","#fbbf24","#f87171","#a78bfa","#22d3ee","#f472b6")),
            textinfo = "label+percent") %>%
      layout(showlegend = FALSE) %>% dark_ly()
  })
  output$plot_enum_field <- renderPlotly({
    req(cce_level())
    d <- cce_level() %>% count(enumerator_name, sort = TRUE) %>% slice_head(n = 15)
    plot_ly(d, x = ~n, y = ~reorder(enumerator_name, n),
            type = "bar", orientation = "h", marker = list(color = "#60a5fa")) %>%
      layout(xaxis = list(title = "CCEs"), yaxis = list(title = "")) %>% dark_ly()
  })
  output$plot_visits <- renderPlotly({
    req(filtered_monitor())
    d <- filtered_monitor() %>%
      distinct(uai, position, boxes_pula_id, crop) %>%
      count(uai, position) %>%
      mutate(visit_label = paste0("V", position))
    plot_ly(d, x = ~uai, y = ~n, color = ~visit_label, type = "bar",
            colors = c("V1" = "#34d399", "V2" = "#60a5fa", "V3" = "#fbbf24")) %>%
      layout(barmode = "group", xaxis = list(title = "UAI"), yaxis = list(title = "CCEs")) %>%
      dark_ly()
  })
  
  # ── Threshold-aware reactives ──────────────────────────────────────────────
  visit_dist_r <- reactive({
    req(filtered_monitor())
    calc_visit_dist(filtered_monitor(), input$thr_v1v2, input$thr_v1v3, input$thr_v2v3)
  })
  cce_dist_r <- reactive({
    req(filtered_monitor())
    calc_cce_dist(filtered_monitor(), input$thr_cce)
  })
  dup_gps_r <- reactive({
    req(filtered_monitor())
    calc_dup_gps(filtered_monitor())
  })
  
  # ── Box distance reactive (NEW) ────────────────────────────────────────────
  box_dist_r <- reactive({
    req(filtered_monitor())
    calc_box_dist(filtered_monitor(), input$thr_box)
  })
  
  # All box distances (unfiltered by threshold) for histogram
  box_dist_all_r <- reactive({
    req(filtered_monitor())
    calc_box_dist(filtered_monitor(), thr = 0)
  })
  
  # ── Visit distance outputs ─────────────────────────────────────────────────
  output$vb_vd_flags <- renderValueBox(
    valueBox(nrow(visit_dist_r()), "Flagged CCEs", icon("triangle-exclamation"), color = "yellow"))
  output$vb_vd_cces <- renderValueBox({
    n <- if (nrow(visit_dist_r()) > 0) n_distinct(visit_dist_r()$boxes_pula_id) else 0
    valueBox(n, "Unique CCEs Affected", icon("seedling"), color = "orange")
  })
  output$vb_vd_fas <- renderValueBox({
    n <- if (nrow(visit_dist_r()) > 0) n_distinct(visit_dist_r()$enumerator_name) else 0
    valueBox(n, "Field Agents Involved", icon("person"), color = "red")
  })
  output$tbl_visits <- renderDT({
    datatable(visit_dist_r(), options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  })
  
  # ── CCE distance outputs ───────────────────────────────────────────────────
  output$vb_cd_pairs <- renderValueBox(
    valueBox(nrow(cce_dist_r()), "Flagged Pairs", icon("circle-exclamation"), color = "red"))
  output$vb_cd_cces <- renderValueBox({
    n <- if (nrow(cce_dist_r()) > 0)
      n_distinct(c(cce_dist_r()$from_id, cce_dist_r()$to_id)) else 0
    valueBox(n, "CCEs Affected", icon("seedling"), color = "orange")
  })
  output$vb_cd_fas <- renderValueBox({
    n <- if (nrow(cce_dist_r()) > 0)
      n_distinct(c(cce_dist_r()$enumerator_name_from, cce_dist_r()$enumerator_name_to)) else 0
    valueBox(n, "Field Agents Involved", icon("person"), color = "yellow")
  })
  output$tbl_cce <- renderDT({
    datatable(cce_dist_r(), options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  })
  
  # ── Box distance outputs (NEW) ─────────────────────────────────────────────
  output$vb_bd_flagged <- renderValueBox({
    n <- nrow(box_dist_r())
    valueBox(n, paste0("Flagged (> ", input$thr_box, " m)"),
             icon("arrows-left-right"), color = "red")
  })
  output$vb_bd_over30 <- renderValueBox({
    d <- box_dist_all_r()
    n <- if (nrow(d) > 0) nrow(filter(d, box_distance_m > 30)) else 0
    valueBox(n, "Exceed Max Allowance (>30 m)", icon("triangle-exclamation"), color = "orange")
  })
  output$vb_bd_avg_dist <- renderValueBox({
    d <- box_dist_all_r()
    avg <- if (nrow(d) > 0) round(mean(d$box_distance_m, na.rm = TRUE), 1) else 0
    valueBox(paste0(avg, " m"), "Avg Box Distance (all CCEs)", icon("ruler"), color = "yellow")
  })
  output$vb_bd_fas <- renderValueBox({
    n <- if (nrow(box_dist_r()) > 0) n_distinct(box_dist_r()$enumerator_name) else 0
    valueBox(n, "Field Agents Involved", icon("person"), color = "blue")
  })
  
  output$plot_box_hist <- renderPlotly({
    req(box_dist_all_r())
    d <- box_dist_all_r()
    if (nrow(d) == 0) return(plotly_empty())
    plot_ly(d, x = ~box_distance_m, type = "histogram",
            marker = list(color = "#60a5fa", line = list(color = "#1c2641", width = 1)),
            nbinsx = 30) %>%
      add_segments(x = 10, xend = 10, y = 0, yend = Inf,
                   line = list(color = "#34d399", dash = "dot", width = 2),
                   name = "Standard (10 m)") %>%
      add_segments(x = 30, xend = 30, y = 0, yend = Inf,
                   line = list(color = "#fbbf24", dash = "dot", width = 2),
                   name = "Max allowance (30 m)") %>%
      layout(xaxis = list(title = "Box distance (m)"),
             yaxis = list(title = "# CCEs"),
             showlegend = TRUE,
             legend = list(font = list(size = 10))) %>%
      dark_ly()
  })
  
  output$plot_box_enum <- renderPlotly({
    req(box_dist_r())
    d <- box_dist_r()
    if (nrow(d) == 0) return(plotly_empty())
    d2 <- d %>% count(enumerator_name, sort = TRUE) %>% slice_head(n = 15)
    plot_ly(d2, x = ~n, y = ~reorder(enumerator_name, n),
            type = "bar", orientation = "h",
            marker = list(color = "#f87171")) %>%
      layout(xaxis = list(title = "Flagged CCEs"), yaxis = list(title = "")) %>%
      dark_ly()
  })
  
  output$tbl_box_dist <- renderDT({
    req(box_dist_r())
    d <- box_dist_r()
    if (nrow(d) == 0) {
      return(datatable(
        data.frame(Message = "No CCEs exceed the current threshold."),
        rownames = FALSE, options = list(dom = "t")
      ))
    }
    datatable(
      d, rownames = FALSE,
      extensions = "Buttons",
      options    = list(pageLength = 20, scrollX = TRUE, dom = "Bfrtip",
                        buttons = c("copy","csv")),
      class = "table-sm table-striped table-hover"
    ) %>%
      formatRound("box_distance_m", digits = 1) %>%
      formatStyle("box_distance_m",
                  backgroundColor = styleInterval(
                    c(10, 30),
                    c("#14532d", "#78350f", "#7f1d1d")
                  ),
                  color = "#f8fafc",
                  fontWeight = "bold") %>%
      formatStyle("status",
                  color = styleEqual(
                    c("Within standard (≤10 m)",
                      "Within allowance (10–30 m)",
                      "Exceeds allowance (>30 m)"),
                    c("#6ee7b7", "#fcd34d", "#fca5a5")
                  ))
  })
  
  output$dl_box_dist <- downloadHandler(
    filename = function() paste0("box_distances_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      req(box_dist_r())
      write_xlsx(box_dist_r(), file)
    }
  )
  
  # ── Duplicate GPS outputs ──────────────────────────────────────────────────
  output$vb_dup_total <- renderValueBox(
    valueBox(nrow(dup_gps_r()), "Total Exact Duplicates", icon("copy"), color = "yellow"))
  dup_n <- function(v) {
    d <- dup_gps_r()
    if (nrow(d) == 0) 0 else nrow(filter(d, visit == paste0("Visit ", v)))
  }
  output$vb_dup_v1 <- renderValueBox(
    valueBox(dup_n(1), "V1 — CSS + Box",     icon("box"),     color = "green"))
  output$vb_dup_v2 <- renderValueBox(
    valueBox(dup_n(2), "V2 — Wet Harvest",   icon("droplet"), color = "blue"))
  output$vb_dup_v3 <- renderValueBox(
    valueBox(dup_n(3), "V3 — Dry Harvest",   icon("sun"),     color = "orange"))
  output$tbl_dup <- renderDT({
    datatable(dup_gps_r(), options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE) %>%
      formatStyle("coord_key", backgroundColor = "#253460")
  })
  
  # ── UAI / AEZ outputs ─────────────────────────────────────────────────────
  spatial_mm <- reactive({
    req(rv$spatial)
    rv$spatial %>% filter(uai_match == "no")
  })
  output$vb_uai_total <- renderValueBox(
    valueBox(nrow(spatial_mm()), "UAI Mismatches", icon("triangle-exclamation"), color = "red"))
  output$vb_uai_pct <- renderValueBox({
    pct <- if (nrow(rv$spatial) > 0)
      round(nrow(spatial_mm()) / nrow(rv$spatial) * 100, 1) else 0
    valueBox(paste0(pct, "%"), "Mismatch Rate", icon("percent"), color = "orange")
  })
  output$vb_uai_avg_d <- renderValueBox({
    avg <- if (nrow(spatial_mm()) > 0)
      round(mean(spatial_mm()$dist_to_assigned_uai, na.rm = TRUE)) else 0
    valueBox(paste0(format(avg, big.mark = ","), " m"),
             "Avg Dist to Correct UAI", icon("ruler"), color = "yellow")
  })
  output$vb_uai_neigh <- renderValueBox({
    n <- if (nrow(spatial_mm()) > 0)
      nrow(filter(spatial_mm(), in_neighbouring_aez == "yes")) else 0
    valueBox(n, "In Neighbouring AEZ", icon("map"), color = "blue")
  })
  output$plot_uai_mm <- renderPlotly({
    req(rv$spatial)
    d <- spatial_mm() %>% count(cluster_assigned, sort = TRUE) %>% rename(uai = cluster_assigned)
    if (nrow(d) == 0) return(plotly_empty())
    plot_ly(d, x = ~n, y = ~reorder(uai, n), type = "bar", orientation = "h",
            marker = list(color = "#fbbf24")) %>%
      layout(xaxis = list(title = "Mismatches"), yaxis = list(title = "")) %>% dark_ly()
  })
  output$plot_fa_mm <- renderPlotly({
    req(rv$spatial)
    d <- spatial_mm() %>% count(enumerator_name, sort = TRUE)
    if (nrow(d) == 0) return(plotly_empty())
    plot_ly(d, x = ~n, y = ~reorder(enumerator_name, n), type = "bar", orientation = "h",
            marker = list(color = "#f87171")) %>%
      layout(xaxis = list(title = "Mismatches"), yaxis = list(title = "")) %>% dark_ly()
  })
  output$tbl_spatial <- renderDT({
    req(rv$spatial)
    datatable(rv$spatial, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE) %>%
      formatStyle("uai_match",
                  backgroundColor = styleEqual(c("yes","no"), c("#14532d","#7f1d1d")),
                  color           = styleEqual(c("yes","no"), c("#6ee7b7","#fca5a5")))
  })
  
  # ── Map ────────────────────────────────────────────────────────────────────
  output$map_main <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.DarkMatter") %>%
      setView(lng = 28, lat = -13, zoom = 6)
  })
  
  observe({
    req(filtered_monitor())
    d <- filtered_monitor() %>% filter(!is.na(lat), !is.na(lon))
    if (input$map_visit != "All") d <- filter(d, position == as.integer(input$map_visit))
    if (input$map_uai_f != "All") d <- filter(d, uai             == input$map_uai_f)
    if (input$map_fa    != "All") d <- filter(d, enumerator_name == input$map_fa)
    if (input$map_crop  != "All") d <- filter(d, crop            == input$map_crop)
    pal <- colorFactor(palette = c("#34d399","#60a5fa","#fbbf24"), domain = c("1","2","3"))
    leafletProxy("map_main", data = d) %>%
      clearMarkers() %>% clearControls() %>%
      addCircleMarkers(
        lng = ~lon, lat = ~lat, radius = 5,
        color = ~pal(as.character(position)), fillOpacity = .85, stroke = FALSE,
        popup = ~paste0(
          "<div style='font-family:DM Sans,sans-serif;min-width:190px;background:#1c2641;",
          "color:#e2e8f0;padding:8px;border-radius:8px;'>",
          "<b style='font-size:1rem;color:#f8fafc;'>", farmer_name, "</b><br>",
          "<span style='color:#64748b;font-size:.79rem;'>", boxes_pula_id, "</span>",
          "<hr style='border-color:#2e3f6e;margin:6px 0;'>",
          "UAI: <b>", uai, "</b><br>Crop: <b>", crop, "</b><br>",
          "Visit: <b>V", position,
          dplyr::case_when(
            position == 1 ~ " — CSS + Box",
            position == 2 ~ " — Wet Harvest",
            TRUE          ~ " — Dry Harvest"
          ), "</b><br>",
          "FA: ", enumerator_name, "<br>Supervisor: ", supervisor_name,
          "<br>ADM2: ", cce_adm2, "</div>"
        )
      ) %>%
      addLegend("bottomright", pal = pal, values = c("1","2","3"), title = "Visit",
                labFormat = labelFormat(
                  transform = function(x)
                    c("V1 — CSS+Box","V2 — Wet Harvest","V3 — Dry Harvest")[as.integer(x)]
                ), opacity = .9)
  })
  
  # ── Raw data ───────────────────────────────────────────────────────────────
  output$tbl_raw <- renderDT({
    req(filtered_monitor())
    datatable(filtered_monitor(), options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  })
  
  # ── Downloads ──────────────────────────────────────────────────────────────
  proj_slug <- reactive({
    d <- filtered_monitor()
    if (!is.null(d) && nrow(d) > 0)
      gsub("[[:punct:]]|\\s+", "-", d$project_name[1])
    else "project"
  })
  
  make_dl <- function(data_fn, suffix) {
    downloadHandler(
      filename = function() paste0(proj_slug(), "_", suffix, "_", Sys.Date(), ".xlsx"),
      content  = function(f) write_xlsx(data_fn(), f)
    )
  }
  
  output$dl_raw      <- make_dl(function() filtered_monitor(), "raw_data")
  output$dl_visits   <- make_dl(function() visit_dist_r(),     "visit_distances")
  output$dl_cce      <- make_dl(function() cce_dist_r(),       "CCE_distances")
  output$dl_box_dist <- make_dl(function() box_dist_r(),       "box_distances")
  output$dl_dup      <- make_dl(function() dup_gps_r(),        "duplicate_GPS")
  output$dl_spatial  <- make_dl(function() rv$spatial,         "UAI_AEZ_analysis")
  
  # ── Cleanup ────────────────────────────────────────────────────────────────
  onSessionEnded(function() {
    if (conn_ok(isolate(rv$conn)))
      try(dbDisconnect(isolate(rv$conn)), silent = TRUE)
  })
}

# ==============================================================================
# 10.  LAUNCH
# ==============================================================================
shinyApp(ui = ui, server = server)

# ==============================================================================
#  Crop Cut — Combined QC & Field Monitoring Dashboard
#  Pula Advisors
# ==============================================================================
#
#  CREDENTIALS (never entered in UI — set via environment or config):
#
#  METHOD 1 — Environment variables (recommended for Posit Connect)
#    DB_HOST | DB_PORT | DB_NAME | DB_USER | DB_PASSWORD
#
#  METHOD 2 — config.yml next to app.R
#    default:
#      db_host: "your-host"
#      db_port: 5432
#      db_name: "your-db"
#      db_user: "your-user"
#      db_password: "your-password"
#
#  METHOD 3 — .Renviron (auto-loaded by R on startup)
#
#  AWS S3 URL validation (optional):
#    AWS_ACCESS_KEY_ID | AWS_SECRET_ACCESS_KEY
#    AWS_DEFAULT_REGION  (optional, defaults to eu-west-1)
#    AWS_SESSION_TOKEN   (optional)
#
#  SHAPEFILES:
#    gadm_data/    — GADM Level-2  .rds files
#    cluster_data/ — Cluster / UAI .rds files
#    Both folders must sit next to app.R.
#
#  Required packages (run once):
#    install.packages(c(
#      "pacman","shiny","shinydashboard","shinyjs","shinyWidgets",
#      "DBI","RPostgres","dplyr","tidyr","stringr","lubridate","purrr",
#      "sf","writexl","DT","leaflet","leaflet.extras","plotly",
#      "waiter","glue","config","jsonlite","aws.s3"
#    ))
# ==============================================================================

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  shiny, shinydashboard, shinyjs, shinyWidgets,
  DBI, RPostgres,
  dplyr, tidyr, stringr, lubridate, purrr,
  sf, writexl,
  DT, leaflet, leaflet.extras,
  plotly, waiter, glue, config,
  jsonlite, aws.s3
)

# ==============================================================================
# 1.  CREDENTIAL RESOLUTION  (Method 1 → 2 → 3)
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
      port     = as.integer(Sys.getenv("DB_PORT", "db_port", "5432")),
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

# ── Image QC query — includes UAI ──────────────────────────────────────────────
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
  tryCatch(
    dbGetQuery(conn, q),
    error = function(e) { message("Image query failed: ", conditionMessage(e)); NULL }
  )
}

# ── Field monitoring query — multi-project ────────────────────────────────────
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
      cc.id                                           AS cce_id,
      cc.status,
      cc.cce_adm1,
      cc.cce_adm2,
      cc.project_name,
      uai.identifier                                  AS uai,
      p.name                                          AS partner,
      f.farmer_type,
      f.phone_number                                  AS farmer_phone,
      f.name                                          AS farmer_name,
      res.start_time,
      res.farmer_responses ->> 'crop'                 AS crop,
      res.position,
      res.responses -> 'q_field_gps' ->> 'latitude'  AS latitude_m,
      res.responses -> 'q_field_gps' ->> 'longitude' AS longitude_m,
      res.responses -> 'q_farm_gps'  ->> 'latitude'  AS latitude_v,
      res.responses -> 'q_farm_gps'  ->> 'longitude' AS longitude_v,
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
  tryCatch(
    dbGetQuery(conn, q),
    error = function(e) { message("Monitor query failed: ", conditionMessage(e)); NULL }
  )
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
    pivot_wider(
      names_from  = question_id,
      values_from = s3_url,
      values_fn   = ~ paste(unique(na.omit(.x)), collapse = ", ")
    )

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
    # ── CSS (position = 1) ──────────────────────────────────────────────────
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
                  cond_col = "q_problem", cond_type = "contains", cond_val = "flood",
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

    # ── Wet Harvest (position = 2) ───────────────────────────────────────────
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

    # ── Dry Harvest (position = 3) ───────────────────────────────────────────
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
# 5.  URL VALIDATION — AWS S3 (authenticated HEAD)
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
  meta_cols <- c("cce_id", "boxes_pula_id", "position")
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
  pts           <- d %>% st_as_sf(coords = c("lon","lat"), crs = 4326, remove = FALSE) %>%
                          st_transform(3857)
  dm            <- st_distance(pts)
  diag(dm)      <- units::set_units(Inf, "m")
  ids           <- pts$boxes_pula_id
  rownames(dm)  <- ids; colnames(dm) <- ids
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
# 7.  CSS
# ==============================================================================

app_css <- "
@import url('https://fonts.googleapis.com/css2?family=DM+Sans:wght@400;500;600;700&family=Space+Mono:wght@400;700&display=swap');

body,.content-wrapper,.right-side{
  background:#07090f!important;font-family:'DM Sans',sans-serif;
}
.skin-black .main-header .logo,
.skin-black .main-header .navbar{background:#0e1117!important;border-bottom:1px solid #1a2035;}
.skin-black .main-sidebar{background:#0e1117!important;border-right:1px solid #1a2035;}
.skin-black .sidebar a{color:#6b7280!important;font-size:.83rem;}
.skin-black .sidebar-menu>li.active>a,
.skin-black .sidebar-menu>li:hover>a{
  background:#161d2e!important;color:#f0f4ff!important;border-left:3px solid #22c55e;
}
.skin-black .sidebar-menu .treeview-menu>li>a{color:#4b5563!important;font-size:.79rem;}
.skin-black .sidebar-menu .treeview-menu>li.active>a,
.skin-black .sidebar-menu .treeview-menu>li:hover>a{color:#22c55e!important;}
.box{
  background:#0e1117!important;border:1px solid #1a2035!important;
  border-top:none!important;border-radius:14px!important;color:#cbd5e1;
}
.box-header{
  background:#0e1117!important;color:#f0f4ff!important;
  border-bottom:1px solid #1a2035!important;border-radius:14px 14px 0 0!important;
  padding:12px 18px;
}
.box-title{font-weight:700;letter-spacing:.04em;font-size:.92rem;}
.small-box{border-radius:14px!important;border:1px solid #1a2035!important;}
.small-box h3{font-family:'Space Mono',monospace;font-size:1.7rem!important;font-weight:700;}
.small-box p{font-size:.7rem;text-transform:uppercase;letter-spacing:.08em;opacity:.85;}
.form-control,.selectize-input{
  background:#161d2e!important;border:1px solid #1a2035!important;
  color:#f0f4ff!important;border-radius:8px!important;
}
.selectize-dropdown{
  background:#161d2e!important;color:#f0f4ff!important;border:1px solid #1a2035!important;
}
.selectize-dropdown .active{background:#1a2035!important;}
label{color:#6b7280!important;font-size:.72rem;text-transform:uppercase;letter-spacing:.07em;}
.irs--shiny .irs-bar,.irs--shiny .irs-handle{background:#22c55e!important;border-color:#22c55e!important;}
.irs--shiny .irs-from,.irs--shiny .irs-to,.irs--shiny .irs-single{background:#22c55e!important;}

.btn-green{
  background:linear-gradient(135deg,#22c55e,#16a34a)!important;border:none!important;
  border-radius:9px!important;font-weight:700;letter-spacing:.04em;color:#000!important;
  transition:all .2s;width:100%;
}
.btn-green:hover{transform:translateY(-2px);box-shadow:0 6px 18px rgba(34,197,94,.35);}
.btn-blue{
  background:linear-gradient(135deg,#3b82f6,#1d4ed8)!important;border:none!important;
  border-radius:9px!important;font-weight:700;letter-spacing:.04em;color:#fff!important;
  transition:all .2s;width:100%;
}
.btn-blue:hover{transform:translateY(-2px);box-shadow:0 6px 18px rgba(59,130,246,.35);}
.btn-dl{
  background:#161d2e!important;border:1px solid #1a2035!important;color:#9ca3af!important;
  border-radius:8px!important;font-size:.78rem;font-weight:600;transition:all .15s;
}
.btn-dl:hover{background:#22c55e!important;color:#000!important;border-color:#22c55e!important;}
table.dataTable{background:#0e1117!important;color:#cbd5e1!important;border:none!important;}
table.dataTable thead th{
  background:#161d2e!important;color:#6b7280!important;
  border-bottom:1px solid #1a2035!important;font-size:.72rem;
  text-transform:uppercase;letter-spacing:.06em;
}
table.dataTable tbody tr{border-bottom:1px solid #0e1117!important;}
table.dataTable tbody tr:hover{background:#161d2e!important;}
.dataTables_wrapper .dataTables_filter input,
.dataTables_wrapper .dataTables_length select{
  background:#161d2e!important;border:1px solid #1a2035!important;
  color:#f0f4ff!important;border-radius:6px;
}
.dataTables_wrapper .dataTables_info,
.dataTables_wrapper .dataTables_paginate{color:#6b7280!important;font-size:.75rem;}
.dataTables_wrapper .dataTables_paginate .paginate_button.current{
  background:#22c55e!important;color:#000!important;border-radius:6px;border:none!important;
}
.dataTables_wrapper .dataTables_paginate .paginate_button:hover{
  background:#161d2e!important;color:#f0f4ff!important;border:none!important;
}
.sec-lbl{
  color:#374151;font-size:.63rem;text-transform:uppercase;letter-spacing:.14em;
  padding:10px 16px 3px;font-family:'Space Mono',monospace;display:block;
}
.pill{display:inline-block;padding:2px 12px;border-radius:999px;font-size:.7rem;font-weight:700;letter-spacing:.06em;}
.pill-ok  {background:#14532d;color:#4ade80;}
.pill-err {background:#450a0a;color:#f87171;}
.pill-idle{background:#1a2035;color:#6b7280;}
.thr-panel{
  background:#0e1117;border:1px solid #1a2035;border-radius:12px;
  padding:14px 18px;margin-bottom:14px;
}
.thr-panel h5{
  color:#9ca3af;font-size:.71rem;text-transform:uppercase;
  letter-spacing:.1em;margin:0 0 10px;font-family:'Space Mono',monospace;
}
.sidebar-divider{border-color:#1a2035;margin:8px 0;}
"

# ==============================================================================
# 8.  UI
# ==============================================================================

ui <- dashboardPage(
  skin  = "black",

  dashboardHeader(
    title = tags$span(
      style = "font-family:'Space Mono',monospace;font-size:.88rem;letter-spacing:.03em;",
      "🌾 CropCut Dashboard"
    ),
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
        tags$small(style = "color:#374151;font-size:.68rem;line-height:1.5;",
                   "Credentials via env vars:",
                   tags$br(),
                   tags$code(style = "font-size:.62rem;color:#4b5563;",
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

    # Project multiselect (rendered after connect)
    span(class = "sec-lbl", "Projects"),
    div(style = "padding:0 12px;",
        uiOutput("project_ui")
    ),

    # Days threshold
    div(style = "padding:0 12px;",
        numericInput("days_threshold",
                     "Highlight if outstanding ≥ (days)",
                     value = 3, min = 1, step = 1, width = "100%")
    ),

    # Load data
    div(style = "padding:4px 12px;",
        actionButton("load_btn", "📥  Load Data",
                     class = "btn btn-block btn-green"),
        uiOutput("last_updated_ui")
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
        menuSubItem("QC Overview",      tabName = "tab_qc_ov",  icon = icon("chart-pie")),
        menuSubItem("Missing Images",   tabName = "tab_missing",icon = icon("triangle-exclamation")),
        menuSubItem("URL Check",        tabName = "tab_url",    icon = icon("link")),
        menuSubItem("By Enumerator",    tabName = "tab_enum",   icon = icon("user-group")),
        menuSubItem("By Project",       tabName = "tab_proj",   icon = icon("folder"))
      ),
      menuItem("📡  Field Monitor", icon = icon("satellite-dish"),
        menuSubItem("Field Overview",   tabName = "tab_field_ov", icon = icon("gauge")),
        menuSubItem("Visit Distances",  tabName = "tab_vd",       icon = icon("route")),
        menuSubItem("CCE Distances",    tabName = "tab_cd",       icon = icon("ruler")),
        menuSubItem("Duplicate GPS",    tabName = "tab_dup",      icon = icon("copy")),
        menuSubItem("UAI / AEZ",        tabName = "tab_uai",      icon = icon("map-pin")),
        menuSubItem("Map",              tabName = "tab_map",      icon = icon("map")),
        menuSubItem("Raw Data",         tabName = "tab_raw",      icon = icon("table"))
      )
    )
  ),

  # ── Body ───────────────────────────────────────────────────────────────────
  dashboardBody(
    tabItems(

      # ── QC OVERVIEW ─────────────────────────────────────────────────────────
      tabItem("tab_qc_ov",
        fluidRow(
          valueBoxOutput("vb_total_cuts",  width = 3),
          valueBoxOutput("vb_total_miss",  width = 3),
          valueBoxOutput("vb_affected",    width = 3),
          valueBoxOutput("vb_enum_issues", width = 3)
        ),
        fluidRow(
          valueBoxOutput("vb_avg_days",  width = 3),
          valueBoxOutput("vb_css_miss",  width = 3),
          valueBoxOutput("vb_wet_miss",  width = 3),
          valueBoxOutput("vb_dry_miss",  width = 3)
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
            p(style = "color:#6b7280;font-size:.81rem;",
              "Sends an authenticated HEAD request to S3 for every submitted image URL.",
              " Large datasets may take several minutes.",
              " Set ", tags$code("AWS_ACCESS_KEY_ID"), " and ",
              tags$code("AWS_SECRET_ACCESS_KEY"), " in environment variables before running."),
            fluidRow(
              column(3,
                     actionButton("validate_btn", "🚀 Start Validation",
                                  class = "btn btn-warning")),
              column(9,
                     selectizeInput("url_status_filter",
                                    "Filter results by status",
                                    choices  = c("All", "ok", "http_404", "http_403",
                                                 "invalid_s3_format", "unreachable", "no_url"),
                                    selected = "All",
                                    multiple = TRUE,
                                    options  = list(placeholder  = "All statuses (click to filter)…",
                                                    plugins      = list("remove_button"))))
            ),
            tags$hr(style = "border-color:#1a2035;"),
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
          box(width = 6, title = "CCEs by Field Agent",  status = "primary",
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
          box(width = 6, title = "Mismatches by UAI",        status = "warning",
              plotlyOutput("plot_uai_mm", height = 300)),
          box(width = 6, title = "Mismatches by Field Agent", status = "warning",
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
         style = "color:#22c55e;margin-top:16px;font-family:'Space Mono',monospace;")
    ),
    color = "rgba(7,9,15,.92)"
  )

  rv <- reactiveValues(
    conn          = NULL,
    projects      = NULL,
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
                   options  = list(
                     placeholder = "Select one or more projects…",
                     plugins     = list("remove_button")
                   ))
  })

  output$last_updated_ui <- renderUI({
    req(rv$last_updated)
    tags$small(style = "color:#4b5563;font-size:.67rem;display:block;margin-top:6px;text-align:center;",
               "Loaded: ", format(rv$last_updated, "%Y-%m-%d %H:%M"))
  })

  # ── SHAPEFILE DROPDOWNS ────────────────────────────────────────────────────
  observe({
    updateSelectInput(session, "gadm_rds",    choices = list_rds("gadm_data"))
    updateSelectInput(session, "cluster_rds", choices = list_rds("cluster_data"))
  })

  # ── LOAD DATA (both queries) ───────────────────────────────────────────────
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
    req_input <- function(x, msg) {
      
      if (is.null(x) || !nzchar(x)) {
        
        showNotification(msg, type = "error")
        return(FALSE)
      }
      
      TRUE
    }
    
    observeEvent(input$btn_spatial, {
      
      if (!req_input(input$gadm_rds,
                     "Please select a GADM shapefile.")) return(NULL)
      
      if (!req_input(input$cluster_rds,
                     "Please select a Cluster shapefile.")) return(NULL)
      
      # continue processing
    })
    gadm_path    <- file.path("gadm_data",    input$gadm_rds)
    cluster_path <- file.path("cluster_data", input$cluster_rds)
    if (!file.exists(gadm_path)) {
      
      showNotification(
        paste("File not found:", gadm_path),
        type = "error"
      )
      
      return(NULL)
    }
    
    if (!file.exists(cluster_path)) {
      
      showNotification(
        paste("File not found:", cluster_path),
        type = "error"
      )
      
      return(NULL)
    }
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
      paper_bgcolor = "#07090f",
      plot_bgcolor  = "#07090f",
      font  = list(color = "#6b7280", family = "DM Sans"),
      xaxis = list(gridcolor = "#1a2035", zerolinecolor = "#1a2035"),
      yaxis = list(gridcolor = "#1a2035", zerolinecolor = "#1a2035")
    )
  }

  # ════════════════════════════════════════════════════════════════════════════
  # IMAGE QC OUTPUTS
  # ════════════════════════════════════════════════════════════════════════════

  # KPI value boxes
  output$vb_total_cuts <- renderValueBox({
    n <- if (!is.null(rv$img_processed)) n_distinct(rv$img_processed$full$cropcut_id, na.rm = TRUE) else 0
    valueBox(n, "Total Cropcuts", icon("clipboard-list"), color = "green")
  })
  output$vb_total_miss <- renderValueBox({
    n <- if (!is.null(rv$missing)) nrow(rv$missing) else 0
    valueBox(n, "Missing Images", icon("image"), color = "red")
  })
  output$vb_affected <- renderValueBox({
    n <- if (!is.null(rv$missing)) n_distinct(rv$missing$boxes_pula_id, na.rm = TRUE) else 0
    valueBox(n, "Affected Farms", icon("house"), color = "orange")
  })
  output$vb_enum_issues <- renderValueBox({
    n <- if (!is.null(rv$missing)) n_distinct(rv$missing$enumerator_name, na.rm = TRUE) else 0
    valueBox(n, "Enumerators Affected", icon("person"), color = "yellow")
  })
  output$vb_avg_days <- renderValueBox({
    n <- if (!is.null(rv$missing) && nrow(rv$missing) > 0)
      round(mean(rv$missing$`Days Since Submission`, na.rm = TRUE), 1) else 0
    valueBox(n, "Avg Days Outstanding", icon("clock"), color = "blue")
  })
  output$vb_css_miss <- renderValueBox({
    n <- if (!is.null(rv$missing)) nrow(rv$missing %>% filter(section == "CSS")) else 0
    valueBox(n, "CSS Missing", icon("file-circle-xmark"), color = "red")
  })
  output$vb_wet_miss <- renderValueBox({
    n <- if (!is.null(rv$missing)) nrow(rv$missing %>% filter(section == "Wet Harvest")) else 0
    valueBox(n, "Wet Harvest Missing", icon("droplet"), color = "blue")
  })
  output$vb_dry_miss <- renderValueBox({
    n <- if (!is.null(rv$missing)) nrow(rv$missing %>% filter(section == "Dry Harvest")) else 0
    valueBox(n, "Dry Harvest Missing", icon("sun"), color = "orange")
  })

  # Charts
  output$chart_section <- renderPlotly({
    req(rv$missing)
    d <- rv$missing %>% count(section)
    pal <- c(CSS = "#22c55e", `Wet Harvest` = "#3b82f6", `Dry Harvest` = "#f59e0b")
    plot_ly(d, x = ~section, y = ~n, color = ~section,
            colors = unname(pal[d$section]), type = "bar") %>%
      layout(showlegend = FALSE, xaxis = list(title = ""),
             yaxis = list(title = "Count"), margin = list(t = 10)) %>% dark_ly()
  })

  output$chart_question <- renderPlotly({
    req(rv$missing)
    d <- rv$missing %>% count(question_id, sort = TRUE) %>% head(10)
    plot_ly(d, x = ~n, y = ~reorder(question_id, n),
            type = "bar", orientation = "h", marker = list(color = "#3b82f6")) %>%
      layout(xaxis = list(title = "Count"), yaxis = list(title = ""),
             margin = list(l = 220, t = 10)) %>% dark_ly()
  })

  output$chart_enum <- renderPlotly({
    req(rv$missing)
    d <- rv$missing %>% filter(!is.na(enumerator_name)) %>%
      count(enumerator_name, sort = TRUE) %>% head(10)
    plot_ly(d, x = ~n, y = ~reorder(enumerator_name, n),
            type = "bar", orientation = "h", marker = list(color = "#f59e0b")) %>%
      layout(xaxis = list(title = "Missing Images"), yaxis = list(title = ""),
             margin = list(l = 160, t = 10)) %>% dark_ly()
  })

  output$chart_timeline <- renderPlotly({
    req(rv$missing)
    d <- rv$missing %>% mutate(date = as.Date(end_time)) %>%
      filter(!is.na(date)) %>% count(date)
    plot_ly(d, x = ~date, y = ~n, type = "scatter", mode = "lines+markers",
            line = list(color = "#22c55e"), marker = list(color = "#22c55e")) %>%
      layout(xaxis = list(title = "Submission Date"),
             yaxis = list(title = "Count"), margin = list(t = 10)) %>% dark_ly()
  })

  # ── Missing images filters ─────────────────────────────────────────────────
  output$filter_section_ui <- renderUI({
    req(rv$missing)
    selectInput("filter_section", "Section",
                choices = c("All", sort(unique(rv$missing$section))), selected = "All")
  })
  output$filter_supervisor_ui <- renderUI({
    req(rv$missing)
    selectInput("filter_supervisor", "Supervisor",
                choices = c("All", sort(na.omit(unique(rv$missing$supervisor_name)))),
                selected = "All")
  })
  output$filter_enumerator_ui <- renderUI({
    req(rv$missing)
    selectInput("filter_enumerator", "Enumerator",
                choices = c("All", sort(na.omit(unique(rv$missing$enumerator_name)))),
                selected = "All")
  })
  output$filter_uai_ui <- renderUI({
    req(rv$missing)
    selectInput("filter_uai", "UAI",
                choices = c("All", sort(na.omit(unique(rv$missing$uai)))),
                selected = "All")
  })

  missing_filtered <- reactive({
    req(rv$missing)
    d <- rv$missing
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
    datatable(
      display, rownames = FALSE,
      extensions = "Buttons",
      options    = list(pageLength = 25, scrollX = TRUE, dom = "Bfrtip",
                        buttons = c("copy","csv")),
      class = "table-striped table-hover table-sm"
    ) %>%
      formatStyle("Days Since Submission",
                  backgroundColor = styleInterval(
                    c(thresh - 0.01, thresh * 2),
                    c("#14532d", "#78350f", "#450a0a")
                  ),
                  color = "#f0f4ff")
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
          tags$li(tags$code("AWS_SESSION_TOKEN"),  " (optional — for assumed-role credentials)")
        ),
        easyClose = TRUE, footer = modalButton("Close")
      ))
      return()
    }
    df <- rv$url_df
    n  <- nrow(df)
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
      mutate(
        status       = statuses,
        status_label = dplyr::recode(status, !!!status_labels, .default = paste0("❓ ", status)),
        accessible   = (status == "ok"),
        bucket_ok    = (!bucket_mismatch | is.na(bucket_mismatch))
      ) %>%
      select(boxes_pula_id, position, question_id,
             url, parsed_bucket, bucket_ok, status, status_label, accessible)

    n_ok    <- sum(rv$url_results$status == "ok")
    n_403   <- sum(rv$url_results$status == "http_403")
    n_404   <- sum(rv$url_results$status == "http_404")
    n_other <- n - n_ok - n_403 - n_404
    showNotification(
      sprintf("✅ Done — %d ok | %d not found (404) | %d access denied (403) | %d other",
              n_ok, n_404, n_403, n_other),
      type = "message", duration = 10
    )
  })

  # Status filter reactive
  url_filtered <- reactive({
    req(rv$url_results)
    d   <- rv$url_results
    sel <- input$url_status_filter
    if (!is.null(sel) && length(sel) > 0 && !"All" %in% sel)
      d <- d %>% filter(status %in% sel)
    d
  })

  output$url_kpi_row <- renderUI({
    req(rv$url_results)
    d        <- rv$url_results        # KPIs always based on full results
    total    <- nrow(d)
    n_ok     <- sum(d$status == "ok")
    n_403    <- sum(d$status == "http_403")
    n_404    <- sum(d$status == "http_404")
    n_format <- sum(d$status == "invalid_s3_format")
    n_reach  <- sum(d$status == "unreachable")
    pct_ok   <- round(100 * n_ok / max(total, 1), 1)
    fluidRow(
      valueBox(total,                             "Checked",
               icon("link"),              color = "navy",   width = 2),
      valueBox(paste0(n_ok, " (", pct_ok, "%)"), "Accessible",
               icon("circle-check"),      color = "green",  width = 2),
      valueBox(n_404,                             "Not Found (404)",
               icon("file-circle-xmark"), color = "red",    width = 2),
      valueBox(n_403,                             "Access Denied (403)",
               icon("lock"),              color = "orange", width = 2),
      valueBox(n_format,                          "Bad URL Format",
               icon("triangle-exclamation"), color = "yellow", width = 2),
      valueBox(n_reach,                           "Unreachable",
               icon("wifi"),              color = "black",  width = 2)
    )
  })

  output$tbl_urls <- renderDT({
    req(url_filtered())
    status_bg <- c(
      "ok"                = "#14532d",
      "http_403"          = "#78350f",
      "http_404"          = "#450a0a",
      "invalid_s3_format" = "#312e81",
      "unreachable"       = "#1e3a5f",
      "no_url"            = "#1a2035"
    )
    display <- url_filtered() %>%
      select(boxes_pula_id, position, question_id,
             bucket_ok, status, status_label, accessible, url)
    datatable(
      display, rownames = FALSE,
      colnames = c("Farm ID","Position","Question",
                   "Bucket OK","Status Code","Status Detail","Accessible","URL"),
      extensions = "Buttons",
      options    = list(
        pageLength = 25, scrollX = TRUE, dom = "Bfrtip",
        buttons    = c("copy","csv"),
        columnDefs = list(
          list(targets = 7,
               render  = JS("function(data){
                 return data
                   ? '<a href=\"'+data+'\" target=\"_blank\" style=\"color:#3b82f6;\">🔗 view</a>'
                   : '—'; }"))
        )
      ),
      escape = FALSE,
      class  = "table-sm table-striped table-hover"
    ) %>%
      formatStyle("status",
                  backgroundColor = styleEqual(names(status_bg), unname(status_bg)),
                  color = "#f0f4ff") %>%
      formatStyle("accessible",
                  color      = styleEqual(c(TRUE, FALSE), c("#4ade80","#f87171")),
                  fontWeight = "bold") %>%
      formatStyle("bucket_ok",
                  backgroundColor = styleEqual(c(TRUE, FALSE), c("transparent","#78350f")))
  })

  output$dl_urls <- downloadHandler(
    filename = function() paste0("s3_url_validation_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      req(url_filtered())
      write_xlsx(url_filtered() %>% select(-status_label), file)
    }
  )

  # ── Enumerator summary ─────────────────────────────────────────────────────
  enum_summary <- reactive({
    req(rv$missing)
    rv$missing %>%
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
                  background        = styleColorBar(range(enum_summary()$`Total Missing`), "#450a0a"),
                  backgroundSize    = "100% 90%",
                  backgroundRepeat  = "no-repeat",
                  backgroundPosition = "center") %>%
      formatStyle("Max Days Outstanding",
                  color = styleInterval(c(7, 14), c("#cbd5e1","#f59e0b","#f87171")))
  })

  output$dl_enum <- downloadHandler(
    filename = function() paste0("enumerator_summary_", Sys.Date(), ".xlsx"),
    content  = function(file) { req(enum_summary()); write_xlsx(enum_summary(), file) }
  )

  # ── By Project ─────────────────────────────────────────────────────────────
  output$tbl_project <- renderDT({
    req(rv$missing, rv$img_processed)
    total_per_proj <- rv$img_processed$full %>%
      group_by(project_name) %>%
      summarise(`Total Cropcuts` = n_distinct(cropcut_id, na.rm = TRUE), .groups = "drop")
    proj_summary <- rv$missing %>%
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
    req(rv$raw_monitor)
    rv$raw_monitor %>% distinct(crop, boxes_pula_id, uai, enumerator_name, .keep_all = FALSE)
  })

  missing_visit_n <- function(v) {
    if (is.null(rv$raw_monitor)) return(0)
    d <- rv$raw_monitor
    if (v == 1) {
      need_v1 <- d %>% filter(position %in% c(2, 3)) %>% pull(boxes_pula_id) %>% unique()
      have_v1 <- d %>% filter(position == 1)          %>% pull(boxes_pula_id) %>% unique()
      length(setdiff(need_v1, have_v1))
    } else if (v == 2) {
      need_v2 <- d %>% filter(position == 3) %>% pull(boxes_pula_id) %>% unique()
      have_v2 <- d %>% filter(position == 2) %>% pull(boxes_pula_id) %>% unique()
      length(setdiff(need_v2, have_v2))
    } else {
      have_v3 <- d %>% filter(position == 3) %>% pull(boxes_pula_id) %>% unique()
      length(setdiff(unique(d$boxes_pula_id), have_v3))
    }
  }

  # Field overview KPI boxes
  output$vb_total_resp <- renderValueBox({
    n <- if (!is.null(rv$raw_monitor)) nrow(rv$raw_monitor) else 0
    valueBox(n, "Total Responses", icon("database"), color = "green")
  })
  output$vb_cces <- renderValueBox({
    n <- if (!is.null(rv$raw_monitor)) nrow(cce_level()) else 0
    valueBox(n, "Unique CCEs (crop × ID × UAI)", icon("seedling"), color = "blue")
  })
  output$vb_v1 <- renderValueBox({
    n <- if (!is.null(rv$raw_monitor)) nrow(filter(rv$raw_monitor, position == 1)) else 0
    valueBox(n, "V1 — CSS + Box Placement", icon("box"), color = "green")
  })
  output$vb_v2 <- renderValueBox({
    n <- if (!is.null(rv$raw_monitor)) nrow(filter(rv$raw_monitor, position == 2)) else 0
    valueBox(n, "V2 — Wet Harvest", icon("droplet"), color = "blue")
  })
  output$vb_v3 <- renderValueBox({
    n <- if (!is.null(rv$raw_monitor)) nrow(filter(rv$raw_monitor, position == 3)) else 0
    valueBox(n, "V3 — Dry Harvest", icon("sun"), color = "yellow")
  })
  output$vb_miss_v1 <- renderValueBox(
    valueBox(missing_visit_n(1), "CCEs Missing V1", icon("circle-xmark"), color = "red"))
  output$vb_miss_v2 <- renderValueBox(
    valueBox(missing_visit_n(2), "CCEs Missing V2", icon("circle-xmark"), color = "red"))
  output$vb_miss_v3 <- renderValueBox(
    valueBox(missing_visit_n(3), "CCEs Missing V3", icon("circle-xmark"), color = "red"))

  # Field overview charts
  output$plot_uai <- renderPlotly({
    req(cce_level())
    d <- cce_level() %>% count(uai, sort = TRUE) %>% slice_head(n = 20)
    plot_ly(d, x = ~reorder(uai, n), y = ~n, type = "bar",
            marker = list(color = "#22c55e")) %>%
      layout(xaxis = list(title = "UAI"), yaxis = list(title = "CCEs"),
             margin = list(b = 80)) %>% dark_ly()
  })
  output$plot_crop <- renderPlotly({
    req(cce_level())
    d <- cce_level() %>% count(crop, sort = TRUE)
    plot_ly(d, labels = ~crop, values = ~n, type = "pie",
            marker = list(colors = c("#22c55e","#3b82f6","#f59e0b",
                                     "#ef4444","#8b5cf6","#06b6d4","#ec4899")),
            textinfo = "label+percent") %>%
      layout(showlegend = FALSE) %>% dark_ly()
  })
  output$plot_enum_field <- renderPlotly({
    req(cce_level())
    d <- cce_level() %>% count(enumerator_name, sort = TRUE) %>% slice_head(n = 15)
    plot_ly(d, x = ~n, y = ~reorder(enumerator_name, n),
            type = "bar", orientation = "h", marker = list(color = "#3b82f6")) %>%
      layout(xaxis = list(title = "CCEs"), yaxis = list(title = "")) %>% dark_ly()
  })
  output$plot_visits <- renderPlotly({
    req(rv$raw_monitor)
    d <- rv$raw_monitor %>%
      distinct(uai, position, boxes_pula_id, crop) %>%
      count(uai, position) %>%
      mutate(visit_label = paste0("V", position))
    plot_ly(d, x = ~uai, y = ~n, color = ~visit_label, type = "bar",
            colors = c("V1" = "#22c55e", "V2" = "#3b82f6", "V3" = "#f59e0b")) %>%
      layout(barmode = "group",
             xaxis   = list(title = "UAI"),
             yaxis   = list(title = "CCEs")) %>% dark_ly()
  })

  # Threshold-aware reactives
  visit_dist_r <- reactive({
    req(rv$raw_monitor)
    calc_visit_dist(rv$raw_monitor, input$thr_v1v2, input$thr_v1v3, input$thr_v2v3)
  })
  cce_dist_r <- reactive({
    req(rv$raw_monitor)
    calc_cce_dist(rv$raw_monitor, input$thr_cce)
  })
  dup_gps_r <- reactive({
    req(rv$raw_monitor)
    calc_dup_gps(rv$raw_monitor)
  })

  # Visit distance outputs
  output$vb_vd_flags <- renderValueBox(
    valueBox(nrow(visit_dist_r()), "Flagged CCEs",
             icon("triangle-exclamation"), color = "yellow"))
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

  # CCE distance outputs
  output$vb_cd_pairs <- renderValueBox(
    valueBox(nrow(cce_dist_r()), "Flagged Pairs", icon("circle-exclamation"), color = "red"))
  output$vb_cd_cces <- renderValueBox({
    n <- if (nrow(cce_dist_r()) > 0)
      n_distinct(c(cce_dist_r()$from_id, cce_dist_r()$to_id)) else 0
    valueBox(n, "CCEs Affected", icon("seedling"), color = "orange")
  })
  output$vb_cd_fas <- renderValueBox({
    n <- if (nrow(cce_dist_r()) > 0)
      n_distinct(c(cce_dist_r()$enumerator_name_from,
                   cce_dist_r()$enumerator_name_to)) else 0
    valueBox(n, "Field Agents Involved", icon("person"), color = "yellow")
  })
  output$tbl_cce <- renderDT({
    datatable(cce_dist_r(), options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  })

  # Duplicate GPS outputs
  output$vb_dup_total <- renderValueBox(
    valueBox(nrow(dup_gps_r()), "Total Exact Duplicates", icon("copy"), color = "yellow"))
  dup_n <- function(v) {
    d <- dup_gps_r()
    if (nrow(d) == 0) 0 else nrow(filter(d, visit == paste0("Visit ", v)))
  }
  output$vb_dup_v1 <- renderValueBox(
    valueBox(dup_n(1), "V1 — CSS + Box", icon("box"),     color = "green"))
  output$vb_dup_v2 <- renderValueBox(
    valueBox(dup_n(2), "V2 — Wet Harvest", icon("droplet"), color = "blue"))
  output$vb_dup_v3 <- renderValueBox(
    valueBox(dup_n(3), "V3 — Dry Harvest", icon("sun"),     color = "orange"))
  output$tbl_dup <- renderDT({
    datatable(dup_gps_r(),
              options  = list(pageLength = 15, scrollX = TRUE),
              rownames = FALSE) %>%
      formatStyle("coord_key", backgroundColor = "#1a2035")
  })

  # UAI / AEZ outputs
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
    d <- spatial_mm() %>% count(cluster_assigned, sort = TRUE) %>%
      rename(uai = cluster_assigned)
    if (nrow(d) == 0) return(plotly_empty())
    plot_ly(d, x = ~n, y = ~reorder(uai, n), type = "bar", orientation = "h",
            marker = list(color = "#f59e0b")) %>%
      layout(xaxis = list(title = "Mismatches"), yaxis = list(title = "")) %>% dark_ly()
  })
  output$plot_fa_mm <- renderPlotly({
    req(rv$spatial)
    d <- spatial_mm() %>% count(enumerator_name, sort = TRUE)
    if (nrow(d) == 0) return(plotly_empty())
    plot_ly(d, x = ~n, y = ~reorder(enumerator_name, n), type = "bar", orientation = "h",
            marker = list(color = "#ef4444")) %>%
      layout(xaxis = list(title = "Mismatches"), yaxis = list(title = "")) %>% dark_ly()
  })
  output$tbl_spatial <- renderDT({
    req(rv$spatial)
    datatable(rv$spatial, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE) %>%
      formatStyle("uai_match",
                  backgroundColor = styleEqual(c("yes","no"), c("#14532d","#450a0a")),
                  color           = styleEqual(c("yes","no"), c("#4ade80","#f87171")))
  })

  # ── Map ────────────────────────────────────────────────────────────────────
  output$map_main <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.DarkMatter") %>%
      setView(lng = 28, lat = -13, zoom = 6)
  })

  observe({
    req(rv$raw_monitor)
    d <- rv$raw_monitor %>% filter(!is.na(lat), !is.na(lon))
    if (input$map_visit != "All") d <- filter(d, position == as.integer(input$map_visit))
    if (input$map_uai_f != "All") d <- filter(d, uai             == input$map_uai_f)
    if (input$map_fa    != "All") d <- filter(d, enumerator_name == input$map_fa)
    if (input$map_crop  != "All") d <- filter(d, crop            == input$map_crop)
    pal <- colorFactor(palette = c("#22c55e","#3b82f6","#f59e0b"), domain = c("1","2","3"))
    leafletProxy("map_main", data = d) %>%
      clearMarkers() %>% clearControls() %>%
      addCircleMarkers(
        lng = ~lon, lat = ~lat, radius = 5,
        color = ~pal(as.character(position)), fillOpacity = .85, stroke = FALSE,
        popup = ~paste0(
          "<div style='font-family:DM Sans,sans-serif;min-width:190px;'>",
          "<b style='font-size:1rem;'>", farmer_name, "</b><br>",
          "<span style='color:#9ca3af;font-size:.79rem;'>", boxes_pula_id, "</span>",
          "<hr style='border-color:#1a2035;margin:6px 0;'>",
          "UAI: <b>", uai, "</b><br>",
          "Crop: <b>", crop, "</b><br>",
          "Visit: <b>V", position,
          dplyr::case_when(
            position == 1 ~ " — CSS + Box",
            position == 2 ~ " — Wet Harvest",
            TRUE          ~ " — Dry Harvest"
          ), "</b><br>",
          "FA: ", enumerator_name, "<br>",
          "Supervisor: ", supervisor_name, "<br>",
          "ADM2: ", cce_adm2,
          "</div>"
        )
      ) %>%
      addLegend("bottomright", pal = pal, values = c("1","2","3"), title = "Visit",
                labFormat = labelFormat(
                  transform = function(x)
                    c("V1 — CSS+Box","V2 — Wet Harvest","V3 — Dry Harvest")[as.integer(x)]
                ),
                opacity = .9)
  })

  # ── Raw data ───────────────────────────────────────────────────────────────
  output$tbl_raw <- renderDT({
    req(rv$raw_monitor)
    datatable(rv$raw_monitor,
              options  = list(pageLength = 15, scrollX = TRUE),
              rownames = FALSE)
  })

  # ── Downloads ──────────────────────────────────────────────────────────────
  proj_slug <- reactive({
    if (!is.null(rv$raw_monitor) && nrow(rv$raw_monitor) > 0)
      gsub("[[:punct:]]|\\s+", "-", rv$raw_monitor$project_name[1])
    else "project"
  })

  make_dl <- function(data_fn, suffix) {
    downloadHandler(
      filename = function() paste0(proj_slug(), "_", suffix, "_", Sys.Date(), ".xlsx"),
      content  = function(f) write_xlsx(data_fn(), f)
    )
  }

  output$dl_raw     <- make_dl(function() rv$raw_monitor,  "raw_data")
  output$dl_visits  <- make_dl(function() visit_dist_r(),  "visit_distances")
  output$dl_cce     <- make_dl(function() cce_dist_r(),    "CCE_distances")
  output$dl_dup     <- make_dl(function() dup_gps_r(),     "duplicate_GPS")
  output$dl_spatial <- make_dl(function() rv$spatial,      "UAI_AEZ_analysis")

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

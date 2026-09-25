# Shared calendar, validation, and uncertainty calculations. No data are loaded.

clean_numeric <- function(x) {
  x |> as.character() |> stringr::str_replace_all(",", ".") |> as.numeric()
}

lag_by_calendar_days <- function(x, date, n = 1L) {
  if (length(x) != length(date) || anyNA(date) || anyDuplicated(date) > 0) {
    stop("Calendar lags require equally sized vectors and unique, non-missing dates.")
  }
  if (length(n) != 1L || is.na(n) || n < 1 || n != as.integer(n)) {
    stop("`n` must be one positive whole number.")
  }
  x[match(date - as.integer(n), date)]
}

previous_sleep_sum <- function(x, date, nights) {
  # Every preceding calendar night is required; the current night is excluded.
  x <- as.numeric(x)
  purrr::map(seq_len(nights), \(k) lag_by_calendar_days(x, date, k)) |>
    purrr::reduce(`+`)
}

calendar_series_id <- function(date) {
  if (anyNA(date) || is.unsorted(date) || anyDuplicated(date) > 0) {
    stop("Calendar sequences require sorted, unique, non-missing dates.")
  }
  cumsum(tidyr::replace_na(as.integer(date - dplyr::lag(date)) != 1L, TRUE))
}

prepare_nw_data <- function(data, fml = NULL) {
  if (!"date" %in% names(data)) stop("Newey-West data require `date`.")
  if (!is.null(fml)) {
    data <- tidyr::drop_na(data, dplyr::all_of(intersect(all.vars(fml), names(data))))
  }
  data <- data[order(data$date), , drop = FALSE]
  data$series_id <- calendar_series_id(data$date)
  data
}

fit_nw <- function(fml, data, family = NULL) {
  data <- prepare_nw_data(data, fml)
  NW <- fixest::NW
  estimate <- function(sample) {
    if (is.null(family)) {
      fixest::feols(fml, data = sample, vcov = NW(7) ~ series_id + date)
    } else {
      fixest::feglm(fml, data = sample, family = family,
                   vcov = NW(7) ~ series_id + date)
    }
  }
  model <- estimate(data)
  used <- fixest::obs(model)
  # GLM fixed effects can remove constant-outcome months internally. Rebuild
  # calendar sequences on the actual estimation sample before final inference.
  if (length(used) < nrow(data)) {
    message("Fixed effects retained ", length(used), " of ", nrow(data), " complete rows.")
    data <- prepare_nw_data(data[used, , drop = FALSE], fml)
    model <- estimate(data)
  }
  model
}

calendar_mean_ci <- function(value, date, probability = FALSE) {
  data <- tibble::tibble(value = value, date = date) |> tidyr::drop_na()
  n <- nrow(data)
  estimate <- mean(data$value)
  bounds <- c(NA_real_, NA_real_)
  if (n > 1L && length(unique(data$value)) == 1L) bounds <- rep(estimate, 2)
  if (n > 1L && length(unique(data$value)) > 1L) {
    bounds <- as.numeric(stats::confint(fit_nw(value ~ 1, data)))
  }
  if (probability) bounds <- pmin(1, pmax(0, bounds))
  tibble::tibble(n = n, estimate = estimate, ci_low = bounds[1], ci_high = bounds[2])
}

grouped_mean_ci <- function(data, groups, outcome, probability = FALSE) {
  # Estimate all group means together so intervening days in other groups stay
  # in the time series. Subsetting each group first would lose these lag pairs.
  sample <- data |>
    tidyr::drop_na(dplyr::all_of(c("date", groups, outcome))) |>
    dplyr::mutate(.value = .data[[outcome]],
                  .group = interaction(dplyr::pick(dplyr::all_of(groups)), drop = TRUE))
  keys <- sample |>
    dplyr::count(dplyr::across(dplyr::all_of(c(groups, ".group"))), name = "n")
  if (nrow(keys) == 1L) {
    ci <- calendar_mean_ci(sample$.value, sample$date, probability)
    return(dplyr::bind_cols(keys |> dplyr::select(-.group), ci |> dplyr::select(-n)))
  }
  model <- fit_nw(.value ~ .group, sample)
  contrasts <- stats::model.matrix(~ .group, keys)
  contrasts <- contrasts[, names(coef(model)), drop = FALSE]
  estimate <- drop(contrasts %*% coef(model))
  std_error <- sqrt(rowSums((contrasts %*% vcov(model)) * contrasts))
  lower <- estimate - 1.96 * std_error
  upper <- estimate + 1.96 * std_error
  if (probability) {
    lower <- pmax(0, lower)
    upper <- pmin(1, upper)
  }
  keys |> dplyr::select(-.group) |>
    dplyr::mutate(estimate = estimate, ci_low = lower, ci_high = upper)
}

sensor_night_date <- function(datetime, end_hour = 8L) {
  local <- lubridate::with_tz(datetime, "Europe/Helsinki")
  # Subtract from the local calendar date, never from a UTC-derived date or
  # from a timestamp that might fall into a daylight-saving transition.
  as.Date(local, tz = "Europe/Helsinki") - as.integer(lubridate::hour(local) < end_hour)
}

validate_diary_rows <- function(data) {
  # Reject entire rows with unsupported codes, retaining an explicit audit.
  # Positive aivotyo values are valid legacy measurements (see AGENTS.md).
  domains <- list(
    unituntia = \(x) x >= 0 & x <= 24,
    unettomuus = \(x) x %in% 0:2, myohaan = \(x) x %in% 0:2,
    urheilu = \(x) x %in% 0:2, kahvi = \(x) x %in% 0:3,
    sauna = \(x) x %in% 0:2, suihku = \(x) x %in% 0:1,
    puhelinparkki = \(x) x %in% 0:3, vauvahuoneessa = \(x) x %in% 0:6,
    Mg = \(x) x %in% 0:1, muu = \(x) x %in% 0:2,
    mittaripaalla = \(x) x %in% 0:1,
    tukevaruoka = \(x) x %in% c(0, 2),
    dota = \(x) x %in% 0:2, alko = \(x) x %in% 0:1,
    hyvin = \(x) x %in% 0:1, nukkumapaikka = \(x) x %in% 1:10,
    ressi = \(x) x %in% 0:1, aivotyo = \(x) x >= 0,
    unilaake = \(x) x %in% 0:2, kipea = \(x) x %in% 0:2,
    lomalla = \(x) x %in% 0:1, painopeitto = \(x) x %in% 0:1,
    `pääkipu` = \(x) x %in% 0:1, paino = \(x) x > 0,
    tv = \(x) x >= 0 & x <= 24, PC = \(x) x >= 0 & x <= 24,
    kvalo = \(x) x >= 0 & x <= 24
  )
  numeric_cols <- intersect(names(domains), names(data))
  issues <- purrr::map_dfr(numeric_cols, function(variable) {
    raw <- data[[variable]]
    value <- suppressWarnings(clean_numeric(raw))
    entered <- !is.na(raw) & stringr::str_trim(as.character(raw)) != ""
    invalid <- entered & (is.na(value) | !is.finite(value) | !domains[[variable]](value))
    tibble::tibble(row = which(invalid), variable = variable,
                   value = as.character(raw[invalid]))
  })
  rejected <- sort(unique(issues$row))
  list(data = data[setdiff(seq_len(nrow(data)), rejected), , drop = FALSE],
       issues = issues, rejected_rows = rejected)
}

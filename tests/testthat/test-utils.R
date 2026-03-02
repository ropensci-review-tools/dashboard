fmt <- function (t) format (t, "%Y-%m-%d %H:%M:%S")

test_that ("get_elapsed_time returns correct list structure", {

    ts <- fmt (Sys.time () - 3 * 24 * 3600)
    res <- get_elapsed_time (ts)

    expect_type (res, "list")
    expect_named (res, c ("dtime_days", "dtime"))
    expect_length (res$dtime_days, 1L)
    expect_length (res$dtime, 1L)
})

test_that ("get_elapsed_time dtime_days minimum is 1", {

    # Timestamp only 30 minutes in the past — well under 1 day
    ts <- fmt (Sys.time () - 1800)
    res <- get_elapsed_time (ts)

    expect_equal (res$dtime_days, 1L)
})

test_that ("get_elapsed_time singular 'day' for ~1-day-old timestamp", {

    ts <- fmt (Sys.time () - 1 * 24 * 3600)
    res <- get_elapsed_time (ts)

    # dtime_days rounds to 1; unit must be singular
    expect_equal (res$dtime_days, 1L)
    expect_equal (res$dtime, "1 day")
})

test_that ("get_elapsed_time plural 'days' for ~3-day-old timestamp", {

    ts <- fmt (Sys.time () - 3 * 24 * 3600)
    res <- get_elapsed_time (ts)

    # 3 * 52/365 ≈ 0.43 → 0 weeks; unit stays "days"
    expect_equal (res$dtime_days, 3L)
    expect_equal (res$dtime, "3 days")
})

test_that ("get_elapsed_time switches to 'weeks' for ~14-day-old timestamp", {

    # 14 * 52/365 ≈ 1.99 → rounds to 2 weeks
    # 14 * 12/365 ≈ 0.46 → rounds to 0 months
    ts <- fmt (Sys.time () - 14 * 24 * 3600)
    res <- get_elapsed_time (ts)

    expect_equal (res$dtime_days, 14L)
    expect_equal (res$dtime, "2 weeks")
})

test_that ("get_elapsed_time switches to 'months' for ~90-day-old timestamp", {

    # 90 * 52/365 ≈ 12.8 → 13 weeks
    # 90 * 12/365 ≈ 2.96 → rounds to 3 months
    ts <- fmt (Sys.time () - 90 * 24 * 3600)
    res <- get_elapsed_time (ts)

    expect_equal (res$dtime_days, 90L)
    expect_equal (res$dtime, "3 months")
})

test_that ("get_elapsed_time handles NA input", {

    res <- get_elapsed_time (NA_character_)

    expect_length (res$dtime_days, 1L)
    expect_true (is.na (res$dtime_days))
    expect_true (is.na (res$dtime))
})

test_that ("get_elapsed_time handles vector input", {

    ts_3 <- fmt (Sys.time () - 3 * 24 * 3600)
    ts_90 <- fmt (Sys.time () - 90 * 24 * 3600)
    tvec <- c (ts_3, NA_character_, ts_90)

    res <- get_elapsed_time (tvec)

    expect_length (res$dtime_days, 3L)
    expect_length (res$dtime, 3L)

    expect_equal (res$dtime [1], "3 days")
    expect_true (is.na (res$dtime [2]))
    expect_equal (res$dtime [3], "3 months")
})

test_that ("get_elapsed_time dtime is always a character string (non-NA)", {

    ts <- fmt (Sys.time () - 7 * 24 * 3600)
    res <- get_elapsed_time (ts)

    expect_type (res$dtime, "character")
    expect_false (is.na (res$dtime))
    # Must contain a number and a time-unit word
    expect_match (res$dtime, "^[0-9]+ (day|week|month)s?$")
})

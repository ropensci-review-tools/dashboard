# Helper: build a minimal mock airtabler object
mock_airtable <- function (table_name, dat) {
    obj <- list ()
    obj [[table_name]] <- list (select_all = function (...) dat)
    obj
}

# Minimal reviewers-prod table returned by m_eic_rev_prod_table()
rev_prod_fixture <- function () {
    data.frame (
        id = c ("rev001", "rev002"),
        github = c ("edA", "edB"),
        name = c ("Ed A", "Ed B"),
        stringsAsFactors = FALSE
    )
}

# ---------------------------------------------------------------------------
# eic_airtable_data()
# ---------------------------------------------------------------------------

test_that ("eic_airtable_data returns data.frame with required columns", {

    # Clear any cached result from m_eic_rev_prod_table
    memoise::forget (m_eic_rev_prod_table)

    today <- Sys.Date ()
    eic_data <- data.frame (
        id = c ("rec1", "rec2"),
        acting_eic = c ("rev001", "rev002"),
        period_start = c (
            format (today - 30L),
            format (today + 1L)
        ),
        period_end = c (
            format (today + 1L),
            format (today + 90L)
        ),
        stringsAsFactors = FALSE
    )
    eic_data$acting_eic_name <- list ("Jane Doe", "John Smith")

    local_mocked_bindings (
        airtable = function (base, table, ...) {
            if (table == "editor-in-chief-rotation") {
                mock_airtable ("editor-in-chief-rotation", eic_data)
            } else {
                mock_airtable ("reviewers-prod", rev_prod_fixture ())
            }
        },
        .package = "airtabler"
    )

    res <- eic_airtable_data ("fake-base-id")

    expect_s3_class (res, "data.frame")
    expect_named (res, c ("name", "github", "start_date", "what"))
    expect_true ("current" %in% res$what)
    expect_true ("next" %in% res$what)
    expect_equal (res$github [res$what == "current"], "edA")
    expect_equal (res$github [res$what == "next"], "edB")
})

# ---------------------------------------------------------------------------
# add_editor_airtable_data()
# ---------------------------------------------------------------------------

test_that ("add_editor_airtable_data adds other_langs and domain_expertise", {

    editors <- data.frame (
        editor = c ("edA", "edB"),
        status = c ("BUSY", "FREE"),
        stats = c (FALSE, FALSE),
        general = c (TRUE, TRUE),
        number = c (1L, 2L),
        inactive_for = c ("3 days", "5 days"),
        inactive_days = c (3L, 5L),
        stringsAsFactors = FALSE
    )

    rev_prod_data <- data.frame (
        id = c ("r1", "r2"),
        github = c ("edA", "edB"),
        name = c ("Ed A", "Ed B"),
        stringsAsFactors = FALSE
    )
    rev_prod_data$other_langs <- list (c ("Python", "Julia"), character (0))
    rev_prod_data$domain_expertise <- list (c ("Ecology", "Other special"), c ("Statistics"))
    rev_prod_data$editor <- list (c ("editor"), character (0))

    local_mocked_bindings (
        airtable = function (...) mock_airtable ("reviewers-prod", rev_prod_data),
        .package = "airtabler"
    )

    res <- add_editor_airtable_data (editors, "fake-base-id")

    expect_true ("other_langs" %in% names (res))
    expect_true ("domain_expertise" %in% names (res))

    expect_equal (res$other_langs [res$editor == "edA"], "Python, Julia")
    expect_equal (res$other_langs [res$editor == "edB"], "")

    # "Other special" should be stripped from domain_expertise
    expect_equal (res$domain_expertise [res$editor == "edA"], "Ecology")
    expect_equal (res$domain_expertise [res$editor == "edB"], "Statistics")
})

test_that ("add_editor_airtable_data removes emeritus editors", {

    editors <- data.frame (
        editor = c ("emeritus-ed", "active-ed"),
        status = c ("FREE", "BUSY"),
        stringsAsFactors = FALSE
    )

    rev_prod_data <- data.frame (
        id = c ("r1", "r2"),
        github = c ("emeritus-ed", "active-ed"),
        name = c ("Old Ed", "Active Ed"),
        stringsAsFactors = FALSE
    )
    rev_prod_data$other_langs <- list (character (0), character (0))
    rev_prod_data$domain_expertise <- list (character (0), character (0))
    # First editor's "editor" field contains "emeritus"
    rev_prod_data$editor <- list (c ("editor-emeritus"), c ("editor"))

    local_mocked_bindings (
        airtable = function (...) mock_airtable ("reviewers-prod", rev_prod_data),
        .package = "airtabler"
    )

    res <- add_editor_airtable_data (editors, "fake-base-id")

    expect_equal (nrow (res), 1L)
    expect_equal (res$editor, "active-ed")
})

# ---------------------------------------------------------------------------
# edvac_status_airtable()
# ---------------------------------------------------------------------------

test_that ("edvac_status_airtable returns data.frame with 'away' logical column", {

    memoise::forget (m_eic_rev_prod_table)

    today <- Sys.Date ()
    # Date columns must be plain character vectors; editor is a linked-record list
    edvac_data <- data.frame (
        id = c ("ev1", "ev2"),
        Action = c ("Out of office", "Vacation"),
        `Start Date` = c (format (today - 5L), format (today - 10L)),
        `Return Date` = c (format (today + 5L), NA_character_),
        stringsAsFactors = FALSE,
        check.names = FALSE
    )
    edvac_data$editor <- list (c ("rev001"), c ("rev002"))

    local_mocked_bindings (
        airtable = function (base, table, ...) {
            if (table == "editor-vacation-status") {
                mock_airtable ("editor-vacation-status", edvac_data)
            } else {
                mock_airtable ("reviewers-prod", rev_prod_fixture ())
            }
        },
        .package = "airtabler"
    )

    res <- edvac_status_airtable ("fake-base-id")

    expect_s3_class (res, "data.frame")
    expect_true ("away" %in% names (res))
    expect_type (res$away, "logical")
})

# ---------------------------------------------------------------------------
# editor_vacation_status()
# ---------------------------------------------------------------------------

test_that ("editor_vacation_status returns data.frame with 'away' logical column", {

    # edA: away in airtable, no slack vacation status
    # edB: not away in airtable, but 'On vacation' in slack
    # edC: neither away in airtable nor in slack
    fake_airtable <- data.frame (
        github = c ("edA", "edB", "edC"),
        name = c ("Ed A", "Ed B", "Ed C"),
        away = c (TRUE, FALSE, FALSE),
        stringsAsFactors = FALSE
    )

    fake_slack <- data.frame (
        id = c ("U001", "U002", "U003"),
        name = c ("edA", "edB", "edC"),
        real_name = c ("Ed A", "Ed B", "Ed C"),
        status = c ("", "On vacation", ""),
        stringsAsFactors = FALSE
    )

    local_mocked_bindings (
        edvac_status_airtable = function (...) fake_airtable,
        get_slack_editors_status = function (...) fake_slack,
        .package = "dashboard"
    )

    res <- suppressWarnings (editor_vacation_status ("fake-base-id"))

    expect_s3_class (res, "data.frame")
    expect_true ("away" %in% names (res))
    expect_type (res$away, "logical")

    # edA: airtable says away → TRUE
    expect_true (res$away [res$name == "edA"])
    # edB: slack vacation keyword → TRUE
    expect_true (res$away [res$name == "edB"])
    # edC: neither → FALSE
    expect_false (res$away [res$name == "edC"])
})

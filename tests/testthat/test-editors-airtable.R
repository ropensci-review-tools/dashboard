# Helper: build a minimal mock airtabler object
mock_airtable <- function (table_name, dat) {
    obj <- list ()
    obj [[table_name]] <- list (select_all = function (...) dat)
    obj
}

# Minimal reviewers-prod table returned by m_eic_rev_prod_table(), with ids
# "rev001", "rev002", ... for the given GitHub logins
rev_prod_fixture <- function (github) {
    data.frame (
        id = sprintf ("rev%03d", seq_along (github)),
        github = github,
        name = github,
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
    eic_data$acting_eic_name <- list ("Current EiC", "Next EiC")

    local_mocked_bindings (
        airtable = function (base, table, ...) {
            if (table == "editor-in-chief-rotation") {
                mock_airtable ("editor-in-chief-rotation", eic_data)
            } else {
                mock_airtable (
                    "reviewers-prod",
                    rev_prod_fixture (c ("current-eic", "next-eic"))
                )
            }
        },
        .package = "airtabler"
    )

    res <- eic_airtable_data ("fake-base-id")

    expect_s3_class (res, "data.frame")
    expect_named (res, c ("name", "github", "start_date", "what"))
    expect_true ("current" %in% res$what)
    expect_true ("next" %in% res$what)
    expect_equal (res$github [res$what == "current"], "current-eic")
    expect_equal (res$github [res$what == "next"], "next-eic")
})

# ---------------------------------------------------------------------------
# add_editor_airtable_data()
# ---------------------------------------------------------------------------

test_that ("add_editor_airtable_data adds other_langs and domain_expertise", {

    editors <- data.frame (
        editor = c ("editor-with-other-langs", "editor-without-other-langs"),
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
        github = c ("editor-with-other-langs", "editor-without-other-langs"),
        name = c ("Editor With Other Langs", "Editor Without Other Langs"),
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

    expect_equal (res$other_langs [res$editor == "editor-with-other-langs"], "Python, Julia")
    expect_equal (res$other_langs [res$editor == "editor-without-other-langs"], "")

    # "Other special" should be stripped from domain_expertise
    expect_equal (res$domain_expertise [res$editor == "editor-with-other-langs"], "Ecology")
    expect_equal (res$domain_expertise [res$editor == "editor-without-other-langs"], "Statistics")
})

test_that ("add_editor_airtable_data removes emeritus editors", {

    editors <- data.frame (
        editor = c ("emeritus-editor", "active-editor"),
        status = c ("FREE", "BUSY"),
        stringsAsFactors = FALSE
    )

    rev_prod_data <- data.frame (
        id = c ("r1", "r2"),
        github = c ("emeritus-editor", "active-editor"),
        name = c ("Emeritus Editor", "Active Editor"),
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
    expect_equal (res$editor, "active-editor")
})

# ---------------------------------------------------------------------------
# edvac_status_airtable()
# ---------------------------------------------------------------------------

test_that ("edvac_status_airtable returns data.frame with 'away' logical column", {

    memoise::forget (m_eic_rev_prod_table)

    today <- Sys.Date ()
    # First editor returns in 5 days; second has no return date.
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
                mock_airtable (
                    "reviewers-prod",
                    rev_prod_fixture (c ("editor-back-soon", "editor-away-no-return-date"))
                )
            }
        },
        .package = "airtabler"
    )

    res <- edvac_status_airtable ("fake-base-id")

    expect_s3_class (res, "data.frame")
    expect_true ("away" %in% names (res))
    expect_type (res$away, "logical")
    expect_equal (res$github, c ("editor-back-soon", "editor-away-no-return-date"))
    expect_equal (res$away, c (TRUE, TRUE))
})

# ---------------------------------------------------------------------------
# editor_vacation_status()
# ---------------------------------------------------------------------------

test_that ("editor_vacation_status returns data.frame with 'away' logical column", {

    fake_airtable <- data.frame (
        github = c ("away-editor", "available-editor"),
        name = c ("Away Editor", "Available Editor"),
        away = c (TRUE, FALSE),
        stringsAsFactors = FALSE
    )

    fake_all_editors <- data.frame (
        login = c ("away-editor", "available-editor", "guest-editor-not-in-airtable"),
        name = c ("Away Editor", "Available Editor", NA),
        stringsAsFactors = FALSE
    )

    local_mocked_bindings (
        edvac_status_airtable = function (...) fake_airtable,
        m_all_editors = function (...) fake_all_editors,
        .package = "dashboard"
    )

    res <- editor_vacation_status ("fake-base-id")

    expect_s3_class (res, "data.frame")
    expect_named (res, c ("name", "github", "away"))
    expect_type (res$away, "logical")

    expect_true (res$away [res$github == "away-editor"])
    expect_false (res$away [res$github == "available-editor"])
    expect_false (res$away [res$github == "guest-editor-not-in-airtable"])
})

# ---------------------------------------------------------------------------
# guest_editors_airtable() and guest_editors()
# ---------------------------------------------------------------------------

test_that ("guest_editors_airtable returns unique GitHub handles", {

    guest_table <- data.frame (id = c ("g1", "g2", "g3", "g4"))
    # One guest edited two submissions; one record has no GitHub handle
    guest_table$github <- list (
        "guest-twice", "guest-twice", "guest-once", NULL
    )

    local_mocked_bindings (
        airtable = function (...) mock_airtable ("guest-editors", guest_table),
        .package = "airtabler"
    )

    res <- guest_editors_airtable ("fake-base-id")
    expect_equal (res, c ("guest-twice", "guest-once"))
})

test_that ("guest_editors combines current and past guests, without regular editors", {

    fake_all_editors <- data.frame (
        login = c ("regular-editor", "current-guest"),
        guest = c (FALSE, TRUE)
    )
    fake_website_editors <- data.frame (
        github = c ("regular-editor", "Former-Guest-Now-Editor", "past-editor"),
        current = c (TRUE, TRUE, FALSE)
    )

    local_mocked_bindings (
        m_all_editors = function () fake_all_editors,
        m_website_editors = function () fake_website_editors,
        m_guest_editors_airtable = function (...) {
            c ("past-guest", "former-guest-now-editor", "current-guest")
        },
        .package = "dashboard"
    )

    res <- guest_editors ("fake-base-id")
    expect_equal (res, c ("current-guest", "past-guest"))
})

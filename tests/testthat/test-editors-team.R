test_that ("current_editors returns only entries with 'editor' role", {

    fake_team <- list (
        list (
            name = "General Editor", github = "general-editor",
            roles = list ("staff", "editor"), generalreview = TRUE
        ),
        list (
            name = "Staff Not Editor", github = "staff-not-editor",
            roles = list ("staff")
        ),
        list (
            name = "Former Editor", github = "former-editor",
            past_roles = list ("editor")
        ),
        list (
            name = "Stats Editor", github = "stats-editor",
            roles = list ("editor"), stats = TRUE
        ),
        list (
            name = "Stats And General Editor", github = "stats-and-general-editor",
            roles = list ("editor"), stats = TRUE, generalreview = TRUE
        ),
        list (
            # Neither 'stats' nor 'generalreview': counts as general
            name = "Editor Without Flags", github = "editor-without-flags",
            roles = list ("editor")
        )
    )

    local_mocked_bindings (
        req_perform = function (req, ...) structure (list (), class = "httr2_response"),
        resp_check_status = function (resp, ...) invisible (resp),
        resp_body_json = function (resp, ...) fake_team,
        .package = "httr2"
    )

    res <- current_editors ()
    expect_s3_class (res, "data.frame")
    expect_named (res, c ("name", "github", "stats", "general"))
    expect_equal (
        res$github,
        c ("general-editor", "stats-editor", "stats-and-general-editor", "editor-without-flags")
    )
    expect_equal (res$stats, c (FALSE, TRUE, TRUE, FALSE))
    expect_equal (res$general, c (TRUE, FALSE, TRUE, TRUE))
})

test_that ("all_editors combines website editors with GitHub team guests", {

    fake_website_editors <- data.frame (
        name = c ("General Editor", "Stats Editor"),
        # Capitalisation differs from the GitHub login "General-Editor":
        github = c ("general-editor", "stats-editor"),
        stats = c (FALSE, TRUE),
        general = c (TRUE, FALSE)
    )

    local_mocked_bindings (
        m_current_editors = function () fake_website_editors,
        gh_team_members = function (stats = FALSE) {
            if (stats) {
                c ("stats-editor", "guest-stats-editor")
            } else {
                c ("General-Editor", "stats-editor", "guest-general-editor", "guest-stats-editor")
            }
        },
        .package = "dashboard"
    )

    res <- get_all_editors ()
    expect_named (res, c ("login", "name", "stats", "general", "guest"))
    # GitHub capitalisation is used for logins:
    expect_equal (
        res$login,
        c ("General-Editor", "stats-editor", "guest-general-editor", "guest-stats-editor")
    )
    expect_equal (res$stats, c (FALSE, TRUE, FALSE, TRUE))
    expect_equal (res$general, c (TRUE, FALSE, TRUE, FALSE))
    expect_equal (res$guest, c (FALSE, FALSE, TRUE, TRUE))
    expect_equal (res$name, c ("General Editor", "Stats Editor", NA, NA))
})

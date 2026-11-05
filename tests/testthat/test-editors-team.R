test_that ("website_editors returns current and past editors", {

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
            name = "Past Editor", github = "past-editor",
            past_roles = list ("editor")
        ),
        list (
            name = "Past Editor Without GitHub",
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

    res <- website_editors ()
    expect_s3_class (res, "data.frame")
    expect_named (res, c ("name", "github", "stats", "general", "current"))
    expect_equal (
        res$github,
        c (
            "general-editor", "past-editor", "stats-editor",
            "stats-and-general-editor", "editor-without-flags"
        )
    )
    expect_equal (res$current, c (TRUE, FALSE, TRUE, TRUE, TRUE))
    expect_equal (res$stats, c (FALSE, FALSE, TRUE, TRUE, FALSE))
    expect_equal (res$general, c (TRUE, TRUE, FALSE, TRUE, TRUE))
})

test_that ("all_editors combines current website editors with GitHub team guests", {

    fake_website_editors <- data.frame (
        name = c ("General Editor", "Stats Editor", "Past Editor"),
        # Capitalisation differs from the GitHub login "General-Editor":
        github = c ("general-editor", "stats-editor", "past-editor"),
        stats = c (FALSE, TRUE, FALSE),
        general = c (TRUE, FALSE, TRUE),
        current = c (TRUE, TRUE, FALSE)
    )

    local_mocked_bindings (
        m_website_editors = function () fake_website_editors,
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

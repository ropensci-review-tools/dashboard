test_that ("current_editors returns only entries with 'editor' role", {

    fake_team <- list (
        list (name = "Ed A", github = "edA", roles = list ("staff", "editor"),
              generalreview = TRUE),
        list (name = "Not Ed", github = "noted", roles = list ("staff")),
        list (name = "Past Ed", github = "pasted", past_roles = list ("editor")),
        list (name = "Ed B", github = "edB", roles = list ("editor"),
              stats = TRUE),
        list (name = "Ed C", github = "edC", roles = list ("editor"),
              stats = TRUE, generalreview = TRUE),
        list (name = "Ed D", github = "edD", roles = list ("editor"))
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
    expect_equal (res$github, c ("edA", "edB", "edC", "edD"))
    expect_equal (res$stats, c (FALSE, TRUE, TRUE, FALSE))
    expect_equal (res$general, c (TRUE, FALSE, TRUE, TRUE))
})

test_that ("all_editors combines website editors with GitHub team guests", {

    fake_web <- data.frame (
        name = c ("Ed A", "Ed B"),
        github = c ("eda", "edB"), # 'eda' differs in case from GitHub
        stats = c (FALSE, TRUE),
        general = c (TRUE, FALSE)
    )

    local_mocked_bindings (
        m_current_editors = function () fake_web,
        gh_team_members = function (stats = FALSE) {
            if (stats) c ("edB", "guestStats") else c ("edA", "edB", "guest", "guestStats")
        },
        .package = "dashboard"
    )

    res <- get_all_editors ()
    expect_named (res, c ("login", "name", "stats", "general", "guest"))
    expect_equal (res$login, c ("edA", "edB", "guest", "guestStats"))
    expect_equal (res$stats, c (FALSE, TRUE, FALSE, TRUE))
    expect_equal (res$general, c (TRUE, FALSE, TRUE, FALSE))
    expect_equal (res$guest, c (FALSE, FALSE, TRUE, TRUE))
    expect_true (is.na (res$name [3]))
})

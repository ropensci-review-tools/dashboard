# ---------------------------------------------------------------------------
# get_slack_token()
# ---------------------------------------------------------------------------

test_that ("get_slack_token returns value of SLACK_TOKEN env var", {

    withr::with_envvar (
        c (SLACK_TOKEN = "xoxb-test-12345"),
        expect_equal (get_slack_token (), "xoxb-test-12345")
    )
})

test_that ("get_slack_token errors when no slack-named env var is present", {

    # Skip if any SLACK_* var exists in the current environment (would
    # override the test expectation)
    skip_if (
        any (grepl ("slack", names (Sys.getenv ()), ignore.case = TRUE)),
        "A SLACK_* env var is present; cannot test the missing-token error path"
    )
    expect_error (get_slack_token (), "Slack API token")
})

# ---------------------------------------------------------------------------
# Helpers: minimal fake httr2 response + body
# ---------------------------------------------------------------------------

fake_httr2_resp <- function () structure (list (), class = "httr2_response")

# ---------------------------------------------------------------------------
# get_editors_user_group_id()
# ---------------------------------------------------------------------------

test_that ("get_editors_user_group_id returns the editors group id", {

    local_mocked_bindings (
        get_slack_token = function () "xoxb-fake",
        .package = "dashboard"
    )
    local_mocked_bindings (
        req_perform = function (req, ...) fake_httr2_resp (),
        resp_check_status = function (resp, ...) invisible (NULL),
        resp_body_json = function (resp, ...) {
            list (usergroups = data.frame (
                id = c ("G123", "G456"),
                handle = c ("editors", "other-group"),
                stringsAsFactors = FALSE
            ))
        },
        .package = "httr2"
    )

    res <- get_editors_user_group_id ()
    expect_type (res, "character")
    expect_length (res, 1L)
    expect_equal (res, "G123")
})

# ---------------------------------------------------------------------------
# get_editors_user_group_members()
# ---------------------------------------------------------------------------

test_that ("get_editors_user_group_members returns a character vector", {

    local_mocked_bindings (
        get_slack_token = function () "xoxb-fake",
        get_editors_user_group_id = function () "G123",
        .package = "dashboard"
    )
    local_mocked_bindings (
        req_perform = function (req, ...) fake_httr2_resp (),
        resp_check_status = function (resp, ...) invisible (NULL),
        resp_body_json = function (resp, ...) list (users = c ("U001", "U002")),
        .package = "httr2"
    )

    res <- get_editors_user_group_members ()
    expect_type (res, "character")
    expect_equal (res, c ("U001", "U002"))
})

# ---------------------------------------------------------------------------
# get_slack_editors_status()
# ---------------------------------------------------------------------------

test_that ("get_slack_editors_status returns data.frame with expected columns", {

    local_mocked_bindings (
        get_slack_token = function () "xoxb-fake",
        get_editors_user_group_members = function () c ("U001", "U002"),
        .package = "dashboard"
    )

    # Build a nested data.frame mirroring simplifyVector = TRUE output
    fake_members <- data.frame (
        id = c ("U001", "U002"),
        name = c ("ed1", "ed2"),
        stringsAsFactors = FALSE
    )
    fake_members$profile <- data.frame (
        real_name = c ("Editor One", "Editor Two"),
        status_text = c ("", "On vacation"),
        stringsAsFactors = FALSE
    )

    local_mocked_bindings (
        req_perform = function (req, ...) fake_httr2_resp (),
        resp_is_error = function (resp, ...) FALSE,
        resp_body_json = function (resp, ...) list (members = fake_members),
        .package = "httr2"
    )

    res <- get_slack_editors_status ()
    expect_s3_class (res, "data.frame")
    expect_named (res, c ("id", "name", "real_name", "status"))
    expect_equal (nrow (res), 2L)
    expect_equal (res$name, c ("ed1", "ed2"))
    expect_equal (res$status, c ("", "On vacation"))
})

test_that ("get_slack_editors_status filters to editor group members only", {

    local_mocked_bindings (
        get_slack_token = function () "xoxb-fake",
        get_editors_user_group_members = function () c ("U001"), # only U001
        .package = "dashboard"
    )

    fake_members <- data.frame (
        id = c ("U001", "U002"), # both returned by API
        name = c ("ed1", "non-editor"),
        stringsAsFactors = FALSE
    )
    fake_members$profile <- data.frame (
        real_name = c ("Editor One", "Non Editor"),
        status_text = c ("", ""),
        stringsAsFactors = FALSE
    )

    local_mocked_bindings (
        req_perform = function (req, ...) fake_httr2_resp (),
        resp_is_error = function (resp, ...) FALSE,
        resp_body_json = function (resp, ...) list (members = fake_members),
        .package = "httr2"
    )

    res <- get_slack_editors_status ()
    # Only U001 should be in the result
    expect_equal (nrow (res), 1L)
    expect_equal (res$id, "U001")
})

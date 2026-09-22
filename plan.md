# Coverage Extension Plan

Current overall coverage: **1.16%**

| File | Current |
|---|---|
| `R/gh-queries.R` | 8.97% |
| `R/utils.R` | 0% |
| `R/reviews.R` | 0% |
| `R/editors.R` | 0% |
| `R/review-history.R` | 0% |
| `R/gt-table.R` | 0% |
| `R/editors-airtable.R` | 0% |
| `R/editors-slack.R` | 0% |

The existing test file (`tests/testthat/test-editors.R`) covers only
`gh_editors_team_qry()` via `httptest2` mocks. All other functions are
untested. The strategy is to prioritize pure / locally-testable functions
first, then add `httptest2` HTTP mocks for functions that call external APIs.

---

## 1. `R/gh-queries.R` — GraphQL query builders

All four functions are pure string-builders with no side effects.

- [x] **`gh_issues_qry()`**: test default args, `open_only = FALSE`, and
  non-NULL `end_cursor` (verify `after:` and `states:` appear/disappear
  appropriately in the returned string).
- [x] **`gh_editors_team_qry()`**: extend existing test — verify `stats-editors`
  vs `editors` slug appears in string; confirm structure beyond just making the
  live call (check for `organization`, `team`, `members` tokens).
- [x] **`gh_issue_assignees_qry()`**: test with and without `end_cursor`;
  verify `orderBy` clause always present.
- [x] **`gh_issues_qry_dates_states()`**: test with and without `end_cursor`.

---

## 2. `R/utils.R` — `get_elapsed_time()`

Pure calculation with predictable structure. Use a fixed past timestamp.

- [x] Returns a named list with elements `dtime_days` and `dtime`.
- [x] `dtime_days` is a positive integer (minimum 1).
- [x] `dtime` is a character string containing a numeric and a time unit word.
- [x] Handles `NA` input — corresponding output element should be `NA`.
- [x] Units cycle correctly: recent timestamp → "days", older → "weeks" →
  "months".
- [x] Singular vs. plural unit (e.g., "1 day" vs. "2 days").

---

## 3. `R/reviews.R` — pure helper functions

### `submission_type_from_body()`

Accepts a list of `edges` (each with `node$body`). Construct minimal fake
`edges` lists in-process — no HTTP needed.

- [x] Returns `NA_character_` when body has no "Submission type:" line.
- [x] Parses `>Standard<` HTML-style markup correctly (returns `"Standard"`).
- [x] Parses plain `Submission type: Stats` colon format (returns `"Stats"`).
- [x] Works for multi-line bodies (only the matching line is parsed).

### `extract_event_timeline_data()`

- [x] Returns a list of the same length as `edges`.
- [x] `what = "labels"` extracts `j$label$name` from timeline nodes.
- [x] `what = "dates"` extracts `j$createdAt`.
- [x] `what = "actors"` extracts `j$actor$login`.
- [x] Invalid `what` value triggers `stopifnot` error.

### `extract_holding_events()`

- [x] Returns `NA_character_` when no "holding" label present.
- [x] Returns latest holding date as character when "holding" appears.
- [x] Handles multiple "holding" events (takes the maximum date).

### `extract_comment_info()`

Build a minimal `dat` with `dat$comments` as a list of flattened
comment vectors (triplets of date/actor/body, mirroring the real format).

- [x] Returns a `data.frame` with exactly 8 columns:
  `editor`, `editor_date`, `rev1`, `rev1_assigned`, `rev1_due`,
  `rev2`, `rev2_assigned`, `rev2_due`.
- [x] Correctly extracts editor when a bot comment ends with `"editor"`.
- [x] Extracts reviewer 1 from a `"reviewers list"` bot comment.
- [x] Extracts reviewer 2 from a second `"reviewers list"` bot comment.
- [x] Returns empty strings when no relevant bot comments present.

---

## 4. `R/editors.R` — pure helper functions

### `clean_assignees()`

- [x] Returns a character vector of the same length as `assignees`.
- [x] Picks the first assignee that is in `editors$login`.
- [x] Returns `NA` (not an error) when no assignee matches editors list.

### `editor_latest_issue()`

Construct small fake `editors`, `assignees`, `number`, `state`,
`updated_at` data frames / vectors.

- [x] Returns a `data.frame` with columns `editor`, `stats`, `general`,
  `number`, `state`, `updated_at`.
- [x] `number` and `state` match the most-recently-updated issue for each editor.
- [x] `number` is `NA` for editors with no assigned issues.

### `editor_reviews()`

- [x] Returns a `data.frame` with columns `editor`, `number`, `title`,
  `state`, `opened_at`, `closed_at`.
- [x] Rows are sorted by editor name then issue number.
- [x] `closed_at` is a `Date` object.

### `editor_timeline()`

This is the most complex pure function. Build a small synthetic dataset
spanning a few known quarters.

- [x] Returns a list with elements `issues_total` and `issues_new`, both
  `data.frame` objects.
- [x] `aggregation_period = "month"`: row names are `"YYYY-MM"` format.
- [x] `aggregation_period = "quarter"`: row names are `"YYYY-MM"` format with
  only quarter-start months.
- [x] `aggregation_period = "semester"`: row names are `"YYYY-01"` or `"YYYY-07"`.
- [x] `issues_new` counts only the period an issue *opened*; `issues_total`
  counts all periods the issue was *active*.
- [x] Invalid `aggregation_period` triggers `match.arg` error.

---

## 5. `R/gt-table.R` — table helpers

### `define_urgency_colours()`

- [x] Returns a character vector of length 5.
- [x] All elements are valid hex colour strings.

### `add_gt_html()`

- [x] Wraps the target column in `<a href=...>` HTML.
- [x] Returns a `data.frame` with the same number of rows.
- [x] Modified column is a list (each element is a `gt::html()` object).

### `open_gt_table()` / `add_urgency_cols()` / `add_bg_colours()`

- [x] These require `gt`; test with a minimal `data.frame` that has all
  expected columns; assert the return value inherits from `"gt_tbl"`.

---

## 6. `R/editors-slack.R` — HTTP mock tests

Use `httptest2::with_mock_api()` to record/replay Slack API responses.
Requires a `SLACK_TOKEN` env var to record; playback requires no token.

- [x] **`get_slack_token()`**: errors when no `SLACK_*` env var present;
  returns the value when exactly one matching var is set.
- [x] **`get_editors_user_group_id()`**: with recorded mock, returns a
  single non-empty character string.
- [x] **`get_editors_user_group_members()`**: with recorded mock, returns a
  character vector.
- [x] **`get_slack_editors_status()`**: with recorded mock, returns a
  `data.frame` with columns `id`, `name`, `real_name`, `status`.

---

## 7. `R/editors-airtable.R` — HTTP mock tests

Use `httptest2` to mock the `airtabler` package's HTTP calls.

- [x] **`edvac_status_airtable()`**: with mock, returns a `data.frame` with
  an `away` logical column.
- [x] **`add_editor_airtable_data()`**: with mock, adds `other_langs` and
  `domain_expertise` columns; removes emeritus editors.
- [x] **`eic_airtable_data()`**: with mock, returns a `data.frame` with
  columns `name`, `github`, `start_date`, `what`.
- [x] **`editor_vacation_status()`**: integration test with mocked airtable +
  slack — returned `data.frame` has `away` logical column.

---

## Implementation notes

- Create one test file per source file:
  - `tests/testthat/test-gh-queries.R`
  - `tests/testthat/test-utils.R`
  - `tests/testthat/test-reviews.R`
  - `tests/testthat/test-editors.R` (extend existing)
  - `tests/testthat/test-gt-table.R`
  - `tests/testthat/test-editors-slack.R`
  - `tests/testthat/test-editors-airtable.R`
- All internal (non-exported) functions are accessible in tests via `:::` or
  by running `devtools::load_all()` (the default in `testthat`).
- Record HTTP fixtures with `httptest2::capture_requests()` or
  `httptest2::with_mock_dir()` (already used for the editors team query).
- Mock directories for each new `with_mock_dir` block go under
  `tests/testthat/` alongside the existing `editors/` and `editors-stats/`
  directories.

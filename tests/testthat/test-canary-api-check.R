source(here::here(".github", "scripts", "canary_api_check.R"))

test_that("validate_urban_ipeds validates presence of results, unitid, and year", {
  good_json <- '{"results": [{"unitid": 240727, "year": 2024}]}'
  bad_json1 <- '{"results": []}'
  bad_json2 <- '{"results": [{"some_other_field": 123}]}'

  resp_good <- httr2::response(200, headers = list("Content-Type" = "application/json"), body = charToRaw(good_json))
  resp_bad1 <- httr2::response(200, headers = list("Content-Type" = "application/json"), body = charToRaw(bad_json1))
  resp_bad2 <- httr2::response(200, headers = list("Content-Type" = "application/json"), body = charToRaw(bad_json2))

  expect_true(validate_urban_ipeds(resp_good)$ok)
  expect_false(validate_urban_ipeds(resp_bad1)$ok)
  expect_false(validate_urban_ipeds(resp_bad2)$ok)
})

test_that("validate_urban_ccd validates presence of results, leaid, and year", {
  good_json <- '{"results": [{"leaid": 5600001, "year": 2023}]}'
  bad_json <- '{"results": [{"unitid": 240727, "year": 2023}]}'

  resp_good <- httr2::response(200, headers = list("Content-Type" = "application/json"), body = charToRaw(good_json))
  resp_bad <- httr2::response(200, headers = list("Content-Type" = "application/json"), body = charToRaw(bad_json))

  expect_true(validate_urban_ccd(resp_good)$ok)
  expect_false(validate_urban_ccd(resp_bad)$ok)
})

test_that("validate_wsba detects valid WSBA signature", {
  resp_good <- httr2::response(200, headers = list("Content-Type" = "text/html"), body = charToRaw("<html><body>Wyoming WSBA Jobs</body></html>"))
  resp_bad <- httr2::response(200, headers = list("Content-Type" = "text/html"), body = charToRaw("<html><body>502 Bad Gateway</body></html>"))

  expect_true(validate_wsba(resp_good)$ok)
  expect_false(validate_wsba(resp_bad)$ok)
})

test_that("check_endpoint handles network timeouts and 5xx errors gracefully", {
  httr2::local_mocked_responses(list(
    httr2::response(503, headers = list("Content-Type" = "text/plain"), body = charToRaw("Service Unavailable"))
  ))

  res <- check_endpoint("Mock Service", "https://api.example.com", validate_urban_ipeds)
  expect_false(res$ok)
  expect_match(res$msg, "503")
})

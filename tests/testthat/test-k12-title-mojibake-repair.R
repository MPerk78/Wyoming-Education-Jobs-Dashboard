# Builds corrupted fixtures the same way the real bug produced them (run
# genuinely-correct text through the OLD unconditional-CP1252-decode
# behavior) rather than hand-typing mojibake bytes, since literal special
# characters are easy to get subtly wrong across tools/encodings.
simulate_old_bug <- function(correct_text) {
  iconv(correct_text, from = "WINDOWS-1252", to = "UTF-8", sub = "byte")
}

test_that("repair_mojibake reverses real corruption patterns exactly", {
  apostrophe_correct <- paste0("Teacher", intToUtf8(0x2019), "s Aide")
  endash_correct <- paste0("1 Part Time (10:00 am ", intToUtf8(0x2013), " 2:00 pm) Assistant Cook")
  ellipsis_correct <- paste0("Now hiring", intToUtf8(0x2026))

  corrupted <- vapply(
    c(apostrophe_correct, endash_correct, ellipsis_correct),
    simulate_old_bug, character(1), USE.NAMES = FALSE
  )

  result <- repair_mojibake(corrupted)
  expect_equal(result$repaired, c(apostrophe_correct, endash_correct, ellipsis_correct))
  expect_true(all(result$changed))
})

test_that("repair_mojibake leaves genuinely-correct accented text untouched", {
  # These characters were never corrupted -- confirming the exactness
  # round-trip check (not a guessed signature) is what protects them, since
  # a real e-acute/n-tilde does contain a non-ASCII byte and would be a
  # candidate for the cheap prescreen.
  cafe <- paste0("Caf", intToUtf8(0x00e9), " Manager")
  nino <- paste0("Ni", intToUtf8(0x00f1), "o Program Coordinator")

  result <- repair_mojibake(c(cafe, nino))
  expect_equal(result$repaired, c(cafe, nino))
  expect_false(any(result$changed))
})

test_that("repair_mojibake leaves plain ASCII and NA untouched without erroring", {
  result <- repair_mojibake(c("Plain ASCII Title", NA_character_, ""))
  expect_equal(result$repaired, c("Plain ASCII Title", NA_character_, ""))
  expect_false(any(result$changed))
})

test_that("repair_title_column applies both the invalid-byte fix and the mojibake reversal", {
  apostrophe_correct <- paste0("Teacher", intToUtf8(0x2019), "s Aide")
  mojibake_row <- simulate_old_bug(apostrophe_correct)
  # A genuinely-invalid raw byte (0x96, WINDOWS-1252's en dash) that was
  # never passed through any encoding fix at all -- the same real case
  # found in a pre-fix archive snapshot.
  invalid_byte_row <- rawToChar(as.raw(c(0x41, 0x20, 0x96, 0x42)))
  Encoding(invalid_byte_row) <- "unknown"

  df <- data.frame(
    title = c(mojibake_row, invalid_byte_row, "Untouched Plain Title"),
    stringsAsFactors = FALSE
  )
  result <- repair_title_column(df, "title", "fixture")

  expect_equal(result$data$title[1], apostrophe_correct)
  expect_equal(result$data$title[2], paste0("A ", intToUtf8(0x2013), "B"))
  expect_equal(result$data$title[3], "Untouched Plain Title")
  expect_equal(nrow(result$report), 2)
  expect_equal(result$report$file, c("fixture", "fixture"))
})

test_that("repair_title_column errors on a missing column instead of silently no-op'ing", {
  expect_error(repair_title_column(data.frame(x = 1), "title", "fixture"), "missing column")
})

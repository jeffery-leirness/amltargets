test_that("token helpers roundtrip correctly", {
  tmp_dir <- withr::local_tempdir()

  withr::with_dir(tmp_dir, {
    amltargets:::write_token("tok_target", "run-tok-001")
    expect_true(amltargets:::token_exists("tok_target"))
    expect_equal(amltargets:::read_token("tok_target"), "run-tok-001")
    amltargets:::delete_token("tok_target")
    expect_false(amltargets:::token_exists("tok_target"))
  })
})


test_that("delete_token removes azure_tokens directory when last token is deleted", {
  tmp_dir <- withr::local_tempdir()

  withr::with_dir(tmp_dir, {
    amltargets:::write_token("tok_target", "run-tok-001")
    token_dir <- dirname(amltargets:::token_path("tok_target"))
    expect_true(dir.exists(token_dir))

    amltargets:::delete_token("tok_target")

    expect_false(file.exists(amltargets:::token_path("tok_target")))
    expect_false(dir.exists(token_dir))
  })
})


test_that("delete_token preserves azure_tokens directory when other tokens remain", {
  tmp_dir <- withr::local_tempdir()

  withr::with_dir(tmp_dir, {
    amltargets:::write_token("tok_one", "run-001")
    amltargets:::write_token("tok_two", "run-002")

    token_dir <- dirname(amltargets:::token_path("tok_one"))
    expect_true(dir.exists(token_dir))

    amltargets:::delete_token("tok_one")

    expect_false(file.exists(amltargets:::token_path("tok_one")))
    expect_true(file.exists(amltargets:::token_path("tok_two")))
    expect_true(dir.exists(token_dir))
  })
})

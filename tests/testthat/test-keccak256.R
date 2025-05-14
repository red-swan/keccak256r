
# strings ----------------------------------------------------------------------

test_that("strings", {
  # Test vector from Ethereum
  expect_equal(
    keccak256(""),
    "c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"
  )

  expect_equal(
    keccak256("0x",hexConvert = TRUE),
    keccak256("")
  )

  expect_equal(
    keccak256("hello world"),
    "47173285a8d7341e5e972fc677286384f802f8ef42a5ec5f03bbfa254cb01fad"
  )

  expect_equal(
    keccak256("123456789"),
    "2a359feeb8e488a1af2c03b908b3ed7990400555db73e1421181d97cac004d48"
  )

  expect_equal(
    keccak256("123456789123456789123456789123456789"),
    "6539bf72f42c41bbae3a56b35130202a3c769163a776349bf54af833b6450d35"
  )

  expect_equal(
    keccak256("There once was a man from nantucket"),
    "97d16385165d64fe024ea3e453192bcbdd655fa667e2f2d8a3f27dbcc57daa72"
  )

  expect_equal(
    keccak256("getOpenFeeRatio()"),
    "673835fd4bf12979f904dafe562ba127f576bd8c59451a7bcbbe7156847ed320"
  )

  expect_equal(
    keccak256("keccak256 works correctly"),
    "d59f184d3735ea6ef9783524595a1165d20123e12ce0e03e2849bf501ae99761"
  )

  expect_equal(
    keccak256("?(){}[]==!`"),
    "21c2fc0cb59ae728692772997d9a0cb77a209bbea1e07a177e61f57fe3716d78"
  )

  expect_equal(
    keccak256("*0z99182mnakjhvnsah\\]["),
    "0992afe7ea5c2a3067b3ff5256ad0eb8775f192380890089f0b1b0f451b3f6a8"
  )

  expect_equal(
    keccak256("\\\\"),
    "1ff3e907b5e1affa131303ddefc1ce21b86400030cdcaa742e55f57e066614f4"
  )

  expect_equal(
    keccak256("0x1234567890abcdef", hexConvert = TRUE),
    keccak256(wkb::hex2raw("1234567890abcdef"))
  )

})


# raw --------------------------------------------------------------------------
test_that("keccak256_raw works correctly", {
  expect_equal(
    keccak256_raw(charToRaw("hello world")),
    "47173285a8d7341e5e972fc677286384f802f8ef42a5ec5f03bbfa254cb01fad"
  )
})


# integer ----------------------------------------------------------------------

test_that("integers work correctly",{
  expect_equal(keccak256_integer(0, 32),
               keccak256(raw(32)))

  expect_equal(keccak256_integer(0,1),
               keccak256(raw(1)))

  expect_error(keccak256_integer(1337,0))

  expect_equal(keccak256_integer(c(0:9)),
               keccak256(str_c("0x", str_dup("0",63), 0:9)))

})


# other inputs -----------------------------------------------------------------

test_that("Bad inputs error", {
  expect_error(keccak256(list(raw(32),
                              raw(32))))
})






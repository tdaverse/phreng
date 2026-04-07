#if (requireNamespace("tinytest", quietly = TRUE)) {
#  tinytest::test_package("phreng")
#}


library(ripserr)
library(phutil)

if (requireNamespace("tinytest", quietly = TRUE)) {
  tinytest::test_all()
}



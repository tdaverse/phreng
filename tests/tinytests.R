# REVIEW: One of these files 'tinytests.R' and 'tinytest.R' is (probably)
# unnecessary; find out how to properly organize tests.

#if (requireNamespace("tinytest", quietly = TRUE)) {
#  tinytest::test_package("phreng")
#}


library(ripserr)
library(phutil)

if (requireNamespace("tinytest", quietly = TRUE)) {
  tinytest::test_all()
}



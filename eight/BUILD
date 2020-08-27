load(
    "@rules_scala_annex//rules:scala.bzl",
    "scala_binary",
)

scala_binary(
    name = "testing",
    srcs = [
        "Testing.scala",
    ],
    deps = [
        "//five:stream_lib",
        "//six:random_lib",
    ],
)

load(
    "@rules_scala_annex//rules:scala.bzl",
    "scala_binary",
    "scala_library",
)

scala_binary(
    name = "random",
    srcs = [
        "Random.scala",
    ],
)

scala_library(
    name = "random_lib",
    srcs = [
        "Random.scala",
    ],
    resource_strip_prefix = package_name(),
    visibility = ["//visibility:public"],
)

load(
    "@rules_scala_annex//rules:scala.bzl",
    "scala_binary",
    "scala_library",
)

scala_binary(
    name = "eleven",
    srcs = [
        "Main.scala",
    ],
    deps = [
        "//six:random_lib",
    ],
)

scala_library(
    name = "eleven_lib",
    srcs = [
        "Main.scala",
    ],
    resource_strip_prefix = package_name(),
    visibility = ["//visibility:public"],
    deps = [
        "//six:random_lib",
    ],
)

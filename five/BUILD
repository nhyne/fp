load(
    "@rules_scala_annex//rules:scala.bzl",
    "scala_binary",
    "scala_library",
)

scala_binary(
    name = "stream",
    srcs = [
        "MyStream.scala",
    ],
)

scala_library(
    name = "stream_lib",
    srcs = ["MyStream.scala"],
    resource_strip_prefix = package_name(),
    visibility = ["//visibility:public"],
)

load(
    "@rules_scala_annex//rules:scala.bzl",
    "scala_binary",
)

scala_binary(
    name = "twelve",
    srcs = [
        "Main.scala",
    ],
    deps = [
        "//eleven:eleven_lib",
    ],
)

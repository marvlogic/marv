#lang marv/dsl/v1-parse

x = 1
y = gcp:compute.instance {
                          name = "compute1"
                               size = "f1-micro"
                               labels = { tag1 = "hello" tag2 = "there"}
                               }
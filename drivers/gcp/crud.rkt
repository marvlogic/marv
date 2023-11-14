#lang racket/base

(provide (struct-out crud))

(struct crud (create read update delete) #:prefab)
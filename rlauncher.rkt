#!/usr/bin/gracket

#lang racket
(define-values (subproc stdin stdout stderr)
  (subprocess #f #f #f "main.rkt"))
(subprocess-pid subproc)



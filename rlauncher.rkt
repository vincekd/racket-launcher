#!/usr/bin/gracket

#lang racket/base

(require racket/udp)

(define socket (udp-open-socket "localhost" 34543))
(udp-connect! socket "localhost" 34543)
(udp-send socket (make-bytes 1))


;;; scsnowman-normal.el --- normal shape for scsnowman.el -*- lexical-binding: t -*-

;; Copyright (C) 2020 by Masahiro Nakamura

;; Author: Masahiro Nakamura <tsuucat@icloud.com>
;; Version: 0.0.2
;; URL: https://github.com/tsuu32/emacs-scsnowman
;; Keywords: hypermedia image snowman

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A `normal' shape definition for scsnowman.el.  The `normal' shape is
;; used as default shape in `scsnowman-create'.

;;; Code:

(require 'scsnowman)

;; https://github.com/aminophen/scsnowman/blob/master/scsnowman-normal.def
(scsnowman-define-shape-function normal
  (let ((svg (svg-create 100 100 :fill-color "transparent"
                         :stroke-linecap  "round"
                         :stroke-linejoin "round")))
    ;; body
    (apply #'scsnowman--svg-path
           svg
           (concat
            "M50 28 C 64 28, 76 35,"
            " 76 45 C 76 49, 72 53,"
            " 67 56 C 79 59, 84 68,"
            " 84 75 C 84 87, 75 92,"
            " 68 92 "
            "L32 92 C 25 92, 16 87,"
            " 16 75 C 16 68, 21 59,"
            " 33 56 C 28 53, 24 49,"
            " 24 45 C 24 35, 36 28,"
            " 50 28 "
            "Z")
           body-path)

    ;; eyes
    (when eyes
      (apply #'svg-ellipse svg 40 44 2 3 eye-path)
      (apply #'svg-ellipse svg 60 44 2 3 eye-path))

    ;; nose
    (when nose
      (apply #'scsnowman--svg-path
             svg
             (concat
              "M49 50 C 52 49, 51 46,"
              " 48 46 C 46 46, 40 48,"
              " 40 48 C 39 49, 46 50,"
              " 49 50"
              " Z")
             nose-path))

    ;; mouth
    (when mouth
      (pcase mouth-shape
        ('tight
         (apply #'scsnowman--svg-path svg "M40 53 L60 53" mouth-path))
        ('frown
         (apply #'scsnowman--svg-path svg "M40 54 C 45 51, 55 51, 60 54" mouth-path))
        ('smile
         (apply #'scsnowman--svg-path svg "M40 52 C 45 55, 55 55, 60 52" mouth-path))))

    ;; sweat
    (when sweat
      (apply #'scsnowman--svg-path
             svg
             "M70 44 C 64 54, 75 54, 70 44 Z"
             sweat-path))

    ;; hat
    (when hat
      (apply #'scsnowman--svg-path
             svg
             (concat
              "M58 10 "
              "L77 19 "
              "L74 39 C 66 40, 50 34,"
              " 46 28 "
              "L58 10 "
              "Z")
             hat-path))

    ;; mikan
    (when mikan
      (apply #'svg-ellipse svg 50 20 15 12 mikan-path))

    ;; leaf
    (when leaf
      (apply #'scsnowman--svg-path
             svg
             (concat
              "M50  7 Q 58  5,"
              " 65 12 Q 50 13,"
              " 50  7 "
              "Z")
             leaf-path))

    ;; broom
    (when broom
      (apply #'svg-line svg  3 94 12 50
             `(,@broom-path :stroke-width 3.2 :stroke-linecap "butt"))
      (apply #'svg-line svg 11 50  6 25
             `(,@broom-path :stroke-width 1.2 :stroke-linecap "butt"))
      (apply #'svg-line svg 12 50 12 28
             `(,@broom-path :stroke-width 1.2 :stroke-linecap "butt"))
      (apply #'svg-line svg 12 50 18 24
             `(,@broom-path :stroke-width 1.2 :stroke-linecap "butt"))
      (apply #'svg-line svg 12 50 21 30
             `(,@broom-path :stroke-width 1.2 :stroke-linecap "butt"))
      (apply #'svg-line svg 13 50 27 26
             `(,@broom-path :stroke-width 1.2 :stroke-linecap "butt")))

    ;; arms
    (when arms
      (apply #'scsnowman--svg-path
             svg
             (concat
              "M20 69 C 19 67, 14 59,"
              " 13 58 C 12 57, 10 57,"
              "  7 56 C  4 54,  6 54,"
              "  8 54 C  9 54, 11 56,"
              " 12 56 C 14 54, 14 53,"
              " 15 51 C 16 49, 16 51,"
              " 16 52 C 16 54, 14 56,"
              " 15 57 C 16 58, 21 65,"
              " 22 67 C 23 69, 21 70,"
              " 20 69 "
              "Z")
             arms-path)
      (apply #'scsnowman--svg-path
             svg
             (concat
              "M80 69 C 81 67, 86 59,"
              " 87 58 C 88 57, 90 57,"
              " 93 56 C 96 54, 94 54,"
              " 92 54 C 91 54, 89 56,"
              " 88 56 C 86 54, 86 53,"
              " 85 51 C 84 49, 84 51,"
              " 84 52 C 84 54, 86 56,"
              " 85 57 C 84 58, 79 65,"
              " 78 67 C 77 69, 79 70,"
              " 80 69 "
              "Z")
             arms-path))

    ;; muffler
    (when muffler
      (apply #'scsnowman--svg-path
             svg
             (concat
              "M27 52 C 42 62, 58 62,"
              " 73 52 C 75 56, 76 56,"
              " 77 59 C 77 61, 75 63,"
              " 73 63 C 74 67, 74 69,"
              " 76 74 C 75 75, 72 76,"
              " 66 77 C 66 73, 65 70,"
              " 63 66 C 42 70, 32 65,"
              " 24 59 C 25 55, 26 53,"
              " 27 52 "
              "Z")
             muffler-path))

    ;; buttons
    (when buttons
      (if muffler
          (progn
            (apply #'svg-circle svg 50 84 3 button-path)
            (apply #'svg-circle svg 50 74 3 button-path))
        (apply #'svg-circle svg 50 83 3 button-path)
        (apply #'svg-circle svg 50 71 3 button-path)))

    ;; snow
    (when snow
      (if broom
          (apply #'svg-circle svg 13 19 4 snow-path)
        (apply #'svg-circle svg  7 72 4 snow-path)
        (apply #'svg-circle svg 13 45 4 snow-path)
        (unless note
          (apply #'svg-circle svg  8 32 4 snow-path)
          (apply #'svg-circle svg 23 24 4 snow-path)))
      (apply #'svg-circle svg 42 11 4 snow-path)
      (apply #'svg-circle svg 74 11 4 snow-path)
      (apply #'svg-circle svg 88 27 4 snow-path)
      (apply #'svg-circle svg 92 47 4 snow-path)
      (apply #'svg-circle svg 94 77 4 snow-path))

    ;; note
    (when note
      (apply #'scsnowman--svg-path
             svg
             (concat
              "M11.9 21.1 C 11.4 21.2, 11.4 21.2,"
              " 11.5 21.9 C 12.2 26.0, 12.4 26.8,"
              " 12.5 27.2 C 12.7 28.3, 12.7 28.3,"
              " 11.8 28.3 C 10.5 28.3, 09.6 29.1,"
              " 09.6 30.2 C 09.6 30.9, 10.0 31.4,"
              " 10.6 31.7 C 11.6 32.1, 13.1 31.3,"
              " 13.4 30.2 C 13.5 30.0, 13.4 29.0,"
              " 13.2 28.0 C 12.6 24.8, 12.5 23.6,"
              " 12.4 23.2 C 12.4 22.7, 12.4 22.7,"
              " 13.2 22.7 C 14.1 22.6, 14.5 22.9,"
              " 14.8 23.6 C 15.0 24.0, 15.4 24.2,"
              " 15.5 23.9 C 15.6 23.9, 15.5 23.4,"
              " 15.3 22.9 C 15.1 22.0, 14.6 21.5,"
              " 13.9 21.1 C 13.6 20.9, 12.6 20.9,"
              " 11.9 21.1 "
              "Z")
             note-path)
      (apply #'scsnowman--svg-path
             svg
             (concat
              "M23.5 23.0 C 23.4 23.1, 23.2 23.5,"
              " 23.1 23.9 C 22.8 25.0, 22.3 25.1,"
              " 21.0 24.0 C 20.3 23.4, 20.1 23.4,"
              " 19.5 23.4 C 18.6 23.4, 17.9 23.9,"
              " 17.4 25.0 C 17.0 25.8, 16.9 26.2,"
              " 17.2 26.5 C 17.6 26.8, 17.9 26.6,"
              " 18.3 25.9 C 18.7 25.0, 19.1 24.5,"
              " 19.4 24.5 C 19.6 24.5, 20.0 24.8,"
              " 20.3 25.1 C 21.0 25.7, 22.0 26.2,"
              " 22.5 26.2 C 23.1 26.2, 23.7 25.8,"
              " 24.1 25.1 C 24.5 24.3, 24.6 23.4,"
              " 24.2 23.0 C 23.9 22.8, 23.8 22.8,"
              " 23.5 23.0 "
              "Z")
             note-path))

    svg))

(provide 'scsnowman-normal)

;;; scsnowman-normal.el ends here

;;; scsnowman.el --- Emacs Lisp port of scsnowman.sty -*- lexical-binding: t -*-

;; Copyright (C) 2020 by Masahiro Nakamura

;; Author: Masahiro Nakamura <tsuucat@icloud.com>
;; Version: 0.0.2
;; URL: https://github.com/tsuu32/emacs-scsnowman
;; Package-Requires: ((emacs "26.1") (svg "1.0"))
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

;; scsnowman.el is a Emacs Lisp port of LaTeX scsnowman.sty.
;; In scsnowman.el scsnowman is a svg object from svg.el.
;; To get image descriptor:
;;  (svg-image (scsnowman-create))
;; To see SVG source:
;;  (svg-print (scsnowman-create :body t :arms t :hat t :muffler "blue"))
;;
;; You can insert scsnowman to buffer simply by `scsnowman'.
;;  (scsnowman :mouth-shape 'frown :snow "skyblue" :sweat t)

;;; Code:

(require 'svg)
(require 'cl-lib)

(defvar scsnowman-defaultcolor     "black")
(defvar scsnowman-defaultanticolor "white")

(defun scsnowman--arghandler (arg)
  "If ARG is t, return `scsnowman-defaultcolor'.  Otherwise return ARG."
  (if (eq arg t)
      scsnowman-defaultcolor
    arg))

(defun scsnowman--svg-p (svg)
  "Return t if SVG is svg object from svg.el."
  (and (listp svg)
       (eq (car svg) 'svg)))


(defmacro scsnowman-define-shape-function (shape &rest form)
  "Define scsnowman--shape- SHAPE function that return SVG.
You can write SVG creating FORM in state of SVG fill and stroke
atributes `let' binded.
  For more details, please refer to scsnowman-normal.el."
  (declare (indent 1))
  `(cl-defun ,(intern (format "scsnowman--shape-%s" shape))
       (&key
        body
        eyes
        nose
        mouth-shape
        mouth
        sweat
        hat
        arms
        muffler
        buttons
        snow
        note
        mikan
        leaf
        broom)
     ,(format "Create %s shape scsnowman SVG image." shape)
     (let* ((body-stroke    (if body body scsnowman-defaultcolor))
            (body-fill      (if body body "transparent"))
            (eye-fill       (if body scsnowman-defaultanticolor scsnowman-defaultcolor))
            (nose-stroke    (if body scsnowman-defaultanticolor nose))
            (nose-fill      (if body nose nose))
            (mouth-stroke   (if body scsnowman-defaultanticolor scsnowman-defaultcolor))
            (sweat-stroke   (if body scsnowman-defaultanticolor sweat))
            (sweat-fill     (if body sweat scsnowman-defaultanticolor))
            (muffler-stroke (if body scsnowman-defaultanticolor muffler))
            (muffler-fill   (if body muffler muffler))
            (button-stroke  (if body scsnowman-defaultanticolor buttons))
            (button-fill    (if body buttons buttons))
            (snow-stroke    (if body snow snow))
            (snow-fill      (if body snow "transparent"))

            (body-path    `(:stroke-color ,body-stroke :fill-color ,body-fill))
            (eye-path     `(:fill-color ,eye-fill))
            (nose-path    `(:stroke-color ,nose-stroke :fill-color ,nose-fill))
            (mouth-path   `(:stroke-color ,mouth-stroke))
            (sweat-path   `(:stroke-color ,sweat-stroke :fill-color ,sweat-fill))
            (hat-path     `(:stroke-color ,hat :fill-color ,hat))
            (mikan-path   `(:stroke-color ,mikan :fill-color ,mikan))
            (leaf-path    `(:stroke-color ,leaf :fill-color ,leaf))
            (broom-path   `(:stroke-color ,broom))
            (arms-path    `(:stroke-color ,arms :fill-color ,arms))
            (muffler-path `(:stroke-color ,muffler-stroke :fill-color ,muffler-fill))
            (button-path  `(:stroke-color ,button-stroke :fill-color ,button-fill))
            (snow-path    `(:stroke-color ,snow-stroke :fill-color ,snow-fill))
            (note-path    `(:fill-color ,note)))
       ,@form)))

;; Emacs 27's svg.el supports `svg-path' but I use `scsnowman--svg-path'
;; for ease of porting.
(defun scsnowman--svg-path (svg d &rest args)
  "Create a path D in SVG."
  (svg--append
   svg
   (dom-node 'path
	     `((d . ,d)
	       ,@(svg--arguments svg args)))))


;;;###autoload
(cl-defun scsnowman-create (&key
                            (shape 'normal)
                            body
                            (eyes t)
                            nose
                            (mouth-shape 'smile)
                            (mouth t)
                            sweat
                            hat
                            arms
                            muffler
                            buttons
                            snow
                            note
                            mikan
                            leaf
                            broom)
  "Create scsnowman SVG image.
Created SVG is a svg object from svg.el.

To get image descriptor:
  (svg-image (scsnowman-create))
To see SVG source:
  (svg-print (scsnowman-create :body t :arms t :hat t :muffler \"blue\"))"
  (let* ((shape-fn (intern (format "scsnowman--shape-%s" shape)))

	 (body    (scsnowman--arghandler body))
         (eyes    (scsnowman--arghandler eyes))
         (nose    (scsnowman--arghandler nose))
         (mouth   (scsnowman--arghandler mouth))
         (sweat   (scsnowman--arghandler sweat))
         (hat     (scsnowman--arghandler hat))
         (arms    (scsnowman--arghandler arms))
         (muffler (scsnowman--arghandler muffler))
         (buttons (scsnowman--arghandler buttons))
         (snow    (scsnowman--arghandler snow))
         (note    (scsnowman--arghandler note))
         (mikan   (scsnowman--arghandler mikan))
         (leaf    (scsnowman--arghandler leaf))
         (broom   (scsnowman--arghandler broom)))
    (if (fboundp shape-fn)
        (let ((svg (funcall shape-fn
                            :body body
                            :eyes eyes
                            :nose nose
                            :mouth-shape mouth-shape
                            :mouth mouth
                            :sweat sweat
                            :hat hat
                            :arms arms
                            :muffler muffler
                            :buttons buttons
                            :snow snow
                            :note note
                            :mikan mikan
                            :leaf leaf
                            :broom broom)))
          (if (scsnowman--svg-p svg)
              svg
            (error "Function %s doesn't return svg" shape-fn)))
      (error "Function %s not found" shape-fn))))

;;;###autoload
(defun scsnowman-insert (&rest plist)
  "Insert scsnowman SVG image.
This function always returns t.

\(fn &key (SHAPE \\='normal) BODY (EYES t) NOSE (MOUTH-SHAPE \\='smile) (MOUTH t) SWEAT HAT ARMS MUFFLER BUTTONS SNOW NOTE MIKAN LEAF BROOM)"
  (svg-insert-image (apply #'scsnowman-create plist))
  t)

;;;###autoload
(defalias 'scsnowman #'scsnowman-insert)

(provide 'scsnowman)
(require 'scsnowman-normal)

;;; scsnowman.el ends here

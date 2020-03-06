# scsnowman.el
scsnowman.el is a Emacs Lisp port of LaTeX [scsnowman.sty](https://github.com/aminophen/scsnowman).

You can insert customizable scsnowman to buffer in graphical Emacs.

## Screenshot
![scsnowmans.png](img/scsnowmans.png)

## Usage
You can insert scsnowman to buffer simply by `scsnowman` function.
```elisp
(scsnowman :mouth-shape 'frown :snow "skyblue" :sweat t)
```

In scsnowman.el scsnowman is a SVG derived from svg.el, so you can use some functions from svg.el such as `svg-print` and `svg-image`.

Get image descriptor:
```elisp
(svg-image (scsnowman-create))
```

See SVG source:
```elisp
(svg-print (scsnowman-create :body t :arms t :hat t :muffler "blue"))
```

## Extensibility
As well as original scsnowman.sty You can define new scsnowman shape.
Use `scsnowman-define-shape-function` macro to define new shape.

```elisp
(scsnowman-define-shape-function "myfavorite"
  (let ((svg (svg-create 100 100)))
    ;; define scsnowman SVG
    ;; you can use `body', `eyes', `nose'... arguments in this form
    ;; useful scsnowman attributes such as `body-path' also can be used

    ;; make sure that finally return created svg
    svg))
```

## Some example
### scsnowman-replace-snowman-mode
Replace ☃(U+2603), ⛄(U+26c4) and ⛇(U+26c7) characters in the buffer with scsnowman images.

```elisp
(defvar scsnowman-replace-snowman-alist
  `(("☃" . (svg-image (scsnowman-create :muffler t :snow t) :scale 0.2 :ascent 80))
    ("⛄" . (svg-image (scsnowman-create :muffler t) :scale 0.2 :ascent 80))
    ("⛇" . (svg-image (scsnowman-create :body t :muffler t :snow t) :scale 0.2 :ascent 80))))

(define-minor-mode scsnowman-replace-snowman-mode
  "Toggle SCsnowman replace snowman mode."
  :lighter " SCsnowman"
  :global nil
  (if scsnowman-replace-snowman-mode
      (let* ((light-p (eq (frame-parameter nil 'background-mode) 'light))
             (scsnowman-defaultcolor (if light-p "black" "white"))
             (scsnowman-defaultanticolor (if light-p "white" "black")))
        (save-excursion
          (dolist (snowman scsnowman-replace-snowman-alist)
            (goto-char (point-min))
            (while (search-forward (car snowman) nil t)
              (add-text-properties (car (match-data t)) (cadr (match-data t))
                                   `(display ,(eval (cdr snowman))))))))
    (save-excursion
      (dolist (snowman scsnowman-replace-snowman-alist)
        (goto-char (point-min))
        (while (search-forward (car snowman) nil t)
          (remove-text-properties (car (match-data t)) (cadr (match-data t))
                                  '(display nil)))))))
```

```
あ、雪だるまだ☃

雪が降ってるね☃

⛄あっちは雪が降ってないみたい

！⛇大雪だ！
```

![replaced.png](img/replaced.png)

### Bouncing scsnowman
Dependency: [bouncing-dvd-logo.el](https://github.com/tsuu32/emacs-bouncing-dvd-logo)
Make sure that Emacs loads bouncing-dvd-logo.el.

You can enjoy editing with boucing scsnowman :)

```elisp
(setq bouncing-dvd-logo-insert-form '(scsnowman :body t
                                                :arms t
                                                :muffler "blue"
                                                :buttons t
                                                :note t))
;; And M-x bouncing-dvd-logo-mode
```

![bouncing.png](img/bouncing.png)

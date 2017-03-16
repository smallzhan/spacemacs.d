;;; packages.el --- latex-enhanced layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Guoqiang <guoqiang@guoqiangMBP>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `latex-enhanced-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `latex-enhanced/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `latex-enhanced/pre-init-PACKAGE' and/or
;;   `latex-enhanced/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst latex-enhanced-packages
  '(auctex
    cdlatex)
  "The list of Lisp packages required by the latex-enhanced layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun latex-enhanced/post-init-cdlatex()
    (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex))

(defun latex-enhanced/post-init-auctex ()
  ;;(with-eval-after-load 'tex
  (progn
    (add-hook 'TeX-mode-hook
              (lambda()
                (setq TeX-engine 'xetex)
                (setq reftex-plug-into-AUCTeX t)
                (turn-on-reftex)))
    (setq reftex-section-levels
          '(("part" . 0) ("chapter" . 1) ("section" . 2) ("subsection" . 3)
            ("frametitle" . 4) ("subsubsection" . 4) ("paragraph" . 5)
            ("subparagraph" . 6) ("addchap" . -1) ("addsec" . -2)))

    (defvar beamer-frame-begin "^[ ]*\\\\begin{frame}"
      "Regular expression that matches the frame start")

    (defvar beamer-frame-end "^[ ]*\\\\end{frame}"
      "Regular expression that matches the frame start")


    (defun beamer-find-frame-begin ()
      "Move point to the \\begin of the current frame."
      (re-search-backward beamer-frame-begin))


    (defun beamer-find-frame-end ()
      "Move point to the \\end of the current environment."
      (re-search-forward beamer-frame-end))



    (defun beamer-mark-frame ()
      "Set mark to end of current frame and point to the matching begin.
The command will not work properly if there are unb(defun latex-enhanced/init-cdlatex()
  (use-package cdlatex
    :defer t
    :init
    (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)))alanced
begin-end pairs in comments and verbatim environments."
      (interactive)
      (let ((cur (point))
            beg end)
        (setq end (beamer-find-frame-end))
        (goto-char cur)
        (setq beg (beamer-find-frame-begin))
        (goto-char beg)
        (set-mark end)))

    ;; (defun beamer-mark-frame ()
    ;;   "beamer-mark-frame"
    ;;   (interactive)
    ;;   (let ((pos (point)))
    ;;     (when (re-search-backward "\\\\begin{frame}")
    ;;       (set-mark)
    ;;       (if (re-search-forward "\\\\end{frame}")
    ;; 		  (message "frame marked")
    ;; 		(message "not in frame")))))


    (defun beamer-indent-frame ()
      (interactive)
      (let ((pos (point))
            beg end)
        (setq beg (beamer-find-frame-begin))
        (goto-char pos)
        (setq end (beamer-find-frame-end))
        (indent-region beg end)
        (goto-char pos)))

    (defun beamer-narrow-to-frame ()
      (interactive)
      (let ((pos (point))
            beg end)
        (setq beg (beamer-find-frame-begin))
        (goto-char pos)
        (setq end (beamer-find-frame-end))
        (narrow-to-region beg end)
        (goto-char pos)))

    )
  )
;;)
;;; packages.el ends here

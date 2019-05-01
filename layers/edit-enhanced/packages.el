;;; packages.el --- edit-enhanced layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Guoqiang Jin <ustczhan@gmail.com>
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
;; added to `edit-enhanced-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `edit-enhanced/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `edit-enhanced/pre-init-PACKAGE' and/or
;;   `edit-enhanced/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst edit-enhanced-packages
  '(
    doom-modeline
    rainbow-delimiters
    posframe
    expand-region
    hangry-delete

    (auto-save :location local)
    (lazy-search :location local)
    (thing-edit :location local)
    (awesome-pair :location local)
    (color-rg :location local)
    (aweshell :location local)
    )
  "The list of Lisp packages required by the edit-enhanced layer.

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


(defun edit-enhanced/init-color-rg ()
  (use-package color-rg
    :config (evil-set-initial-state 'color-rg-mode 'emacs))
  )

(defun edit-enhanced/init-aweshell ()
  (use-package aweshell
    :defer t))

(defun edit-enhanced/init-doom-modeline ()
  (use-package doom-modeline
    :ensure t
    :config (doom-modeline-mode 1)))

(defun edit-enhanced/init-auto-save ()
  (use-package auto-save
    :init
    (setq auto-save-silent t)
    :config
    (auto-save-enable)))

(defun edit-enhanced/init-lazy-search ()
  (use-package lazy-search
    :bind (("M-s s" . lazy-search))
    :config (evil-set-initial-state 'lazy-search-mode 'emacs))
  )

(defun edit-enhanced/init-thing-edit ()
  (use-package thing-edit
    :defer t))

(defun edit-enhanced/init-posframe ()
  (use-package posframe
    :defer t))

(defun edit-enhanced/init-awesome-pair ()
  (use-package awesome-pair
    :bind (:map awesome-pair-mode-map
                ("(" . awesome-pair-open-round)
                ("[" . awesome-pair-open-bracket)
                ("{" . awesome-pair-open-curly)
                (")" . awesome-pair-close-round)
                ("]" . awesome-pair-close-bracket)
                ("}" . awesome-pair-close-curly)
                ("=" . awesome-pair-equal)

                ("%" . awesome-pair-match-paren)
                ("\"" . awesome-pair-double-quote)
                ("SPC" . awesome-pair-space)

                ("M-o" . awesome-pair-backward-delete)
                ("C-d" . awesome-pair-forward-delete)
                ("C-k" . awesome-pair-kill)

                ("M-\"" . awesome-pair-wrap-double-quote)
                ("M-[" . awesome-pair-wrap-bracket)
                ("M-{" . awesome-pair-wrap-curly)
                ("M-(" . awesome-pair-wrap-round)
                ("M-)" . awesome-pair-unwrap)
                ("M-p" . awesome-pair-jump-right)
                ("M-n" . awesome-pair-jump-left)
                ("M-:" . awesome-pair-jump-out-pair-and-newline)
                )
    :hook ((prog-mode ielm-mode minibuffer-inactive-mode sh-mode) . awesome-pair-mode))
  )

(defun edit-enhanced/init-expand-region ()
  (use-package expand-region
    :defer t
    :init (spacemacs/set-leader-keys "v" 'er/expand-region)
    :config
    (progn
      ;; add search capability to expand-region
      (when (configuration-layer/package-used-p 'helm-ag)
        (defadvice er/prepare-for-more-expansions-internal
            (around helm-ag/prepare-for-more-expansions-internal activate)
          ad-do-it
          (let ((new-msg (concat (car ad-return-value)
                                 ", / to search in project, "
                                 "f to search in files, "
                                 "b to search in opened buffers"))
                (new-bindings (cdr ad-return-value)))
            (cl-pushnew
             '("/" (lambda ()
                     (call-interactively
                      'spacemacs/helm-project-smart-do-search-region-or-symbol)))
             new-bindings)
            (cl-pushnew
             '("f" (lambda ()
                     (call-interactively
                      'spacemacs/helm-files-smart-do-search-region-or-symbol)))
             new-bindings)
            (cl-pushnew
             '("b" (lambda ()
                     (call-interactively
                      'spacemacs/helm-buffers-smart-do-search-region-or-symbol)))
             new-bindings)
            (setq ad-return-value (cons new-msg new-bindings)))))
      (setq expand-region-contract-fast-key "V"
            expand-region-reset-fast-key "r"))))

(defun edit-enhanced/init-hungry-delete ()
  (use-package hungry-delete
    :defer t
    :init
    (spacemacs|add-toggle hungry-delete
      :mode hungry-delete-mode
      :documentation "Delete consecutive horizontal whitespace with a single key."
      :evil-leader "td")
    :config
    (progn
      (setq-default hungry-delete-chars-to-skip " \t\f\v") ; only horizontal whitespace
      (define-key hungry-delete-mode-map (kbd "DEL") 'hungry-delete-backward)
      (define-key hungry-delete-mode-map (kbd "S-DEL") 'delete-backward-char))))

;;; packages.el ends here

;;; packages.el --- my-swift layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
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
;; added to `my-swift-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-swift/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-swift/pre-init-PACKAGE' and/or
;;   `my-swift/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-swift-packages
  '(
    company
    (swift-mode :location local)
    company-sourcekit
    )
  "The list of Lisp packages required by the my-swift layer.

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


(when (configuration-layer/layer-usedp 'auto-completion)
  (defun my-swift/post-init-company ()
    (spacemacs|add-company-hook swift-mode))

  (defun my-swift/init-company-sourcekit ()
    (use-package company-sourcekit
      :if (configuration-layer/package-usedp 'company)
      :defer t
      :init
      (push 'company-sourcekit company-backends-swift-mode))))

(defun my-swift/init-swift-mode ()
  "Initialize swift-mode package"
  (use-package swift-mode
    :mode ("\\.swift\\'" . swift-mode)
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'swift-mode
        ;; repl
        "sS" 'swift-mode-run-repl
        "ss" 'swift-mode-send-region
        "sb" 'swift-mode-send-buffer
        "sr" 'swift-mode-send-region)
      (with-eval-after-load 'swift-repl-mode-map
        (spacemacs/set-leader-keys-for-major-mode 'swift-repl-mode
          "ss" 'spacemace/swift-repl-mode-switch-back)
        (define-key swift-repl-mode-map
          (kbd "C-c C-z") 'spacemacs/swift-repl-mode-switch-back))
      (with-eval-after-load 'swift-package-mode-map
        (spacemacs/set-leader-keys-for-major-mode 'swift-package-mode
          "g" 'swift-package-refresh
          "n" 'swift-package-next
          "p" 'swift-package-prev
          "v" 'swift-package-view
          "m" 'swift-package-mark
          "u" 'swift-package-unmark
          "x" 'swift-package-install
          "q" 'quit-window))
      )))
;;; packages.el ends here

;;; packages.el --- elpy Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;(add-to-list 'package-archives
;;             '("elpy" . "http://jorgenschaefer.github.io/packages/"))

(setq elpy-packages
      '(
        company
        elpy
        flycheck
        pyvenv
        jedi
        py-yapf
        pyenv-mode
        ))

;;(setq elpy-excluded-packages '())

(defun elpy/init-elpy ()
  (use-package elpy
    :diminish elpy-mode
    :config

    ;; Elpy removes the modeline lighters. Let's override this
    (defun elpy-modules-remove-modeline-lighter (mode-name))

    (setq elpy-modules '(elpy-module-sane-defaults
                         elpy-module-eldoc
                         elpy-module-pyvenv))

    (when (configuration-layer/layer-usedp 'auto-completion)
      (add-to-list 'elpy-modules 'elpy-module-company)
      (add-to-list 'elpy-modules 'elpy-module-yasnippet))

    (setq elpy-rpc-backend "jedi")
    (elpy-enable)
    (elpy-use-ipython)
    ;;(add-hook 'python-mode-hook 'jedi:setup)
    ;;(setq jedi:complete-on-dot t)

;;    (elpy-use-ipython)
    ))

(defun elpy/post-init-company ()
  (spacemacs|add-company-hook inferior-python-mode)
  (push 'company-capf company-backends-inferior-python-mode)
  (add-hook 'inferior-python-mode-hook
            (lambda ()
              (setq-local company-minimum-prefix-length 0)
              (setq-local company-idle-delay 0.5)))
  )

(defun elpy/post-init-flycheck ()
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  )

(defun elpy/init-py-yapf ()
  (use-package py-yapf
    :init
    (spacemacs/set-leader-keys-for-major-mode 'python-mode "=" 'py-yapf-buffer)
    :config
    (when python-enable-yapf-format-on-save
      (add-hook 'elpy-mode-hook 'py-yapf-enable-on-save))))

(defun elpy/init-jedi ()
  (use-package jedi
    :ensure t))

(defun elpy/init-pyenv-mode()
  (use-package pyenv-mode
    :ensure t))

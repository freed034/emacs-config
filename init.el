;; Created from scratch at Caen - France (2015)
;; Mayor changes made at Orsay - France (2021)
;; Author: Frank Taipe (フランク) <freed034@pm.me>
;; URL: https://github.com/freed034/emacs-config

;;====================================================================
;; Initial variables
;;====================================================================

(setq user-DIR (concat (getenv "HOME") "/.config/emacs/"))
(setq user-emacs-directory (concat (getenv "HOME") "/.config/emacs/"))
;; (setq user-manual-packages (concat user-DIR "packages"))

;;====================================================================
;; Set the package manager
;;====================================================================

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ;; ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(setq package-enable-at-startup nil)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;;====================================================================
;; Use-package
;;====================================================================

;; use-package to simplify the config of for each package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-verbose t)
;; (setq use-package-always-ensure t)

;;====================================================================
;; Reload emacs config
;;====================================================================

;; Function to reload the init file
(defun freed034/reload-init-file ()
  (interactive)
  (load-file (concat user-DIR "init.el")))

;; Shorcut for reload the init file
(global-set-key (kbd "\C-c l") 'freed034/reload-init-file)

;;====================================================================
;; Server emacs
;;====================================================================

;; Function to save buffers, quit and shutdown emacs server instance
(defun freed034/server-shutdown ()
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(global-set-key (kbd "\C-c \C-k \C-r") 'freed034/server-shutdown)

;;====================================================================
;; Environment variables from shell
;;====================================================================

(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize)
  )

;;====================================================================
;; Functions to recognize the os host
;;====================================================================

(defun freed034/system-is-mac ()
  (interactive)
  (string-equal system-type "darwin"))

(defun freed034/system-is-linux ()
  (interactive)
  (string-equal system-type "gnu/linux"))

;; This function makes use of lsb_release
(defun freed034/which-linux-distribution ()
  (interactive)
  ;; (when (eq system-type 'gnu/linux) (shell-command-to-string "lsb_release -sd")) ;; distro with version
  (when (eq system-type 'gnu/linux) (replace-regexp-in-string
                                     "Description:\\|[\t\n\r]+" ""
                                     (shell-command-to-string "lsb_release --id --short"))))

;;====================================================================
;; Set configs and variables based in the os host
;;====================================================================

(if (freed034/system-is-linux)
    (progn
      (when (string-equal (freed034/which-linux-distribution) 'Arch)
        (setq custom-file (concat user-DIR "custom_arch.el")
              config-file (concat user-DIR "config_arch.el")
	      user-file (concat user-DIR "data/user_arch.el")
	      mu4e-path-var "/usr/local/share/emacs/site-lisp/mu4e"))
      (when (string-equal (freed034/which-linux-distribution) 'Ubuntu)
        (setq custom-file (concat user-DIR "custom_ubuntu.el")
	      user-file (concat user-DIR "data/user_ubuntu.el")))))

(if (freed034/system-is-mac)
      (setq custom-file (concat user-DIR "custom_macos.el")
            config-file (concat user-DIR "config_macos.el")
	    user-file (concat user-DIR "data/user_macos.el")
	    ;; the correct path may differ on your system, check it!
            mu4e-path-var "/usr/local/share/emacs/site-lisp/mu/mu4e"))

;; Load the custom file with customizations and the package list
(load-file custom-file)

;; Installing packages
(package-install-selected-packages)

;; Load the path for mu4e
(add-to-list 'load-path mu4e-path-var)

;; Load the custom file with user informations
(load-file user-file)

;; Load the path for manually download packages
;;(add-to-list 'load-path user-manual-packages)

;; Load the custom file with all package configurations
(load-file config-file)

;;====================================================================
;; Bindings for define word
;;====================================================================

(global-set-key (kbd "\C-c d") 'define-word-at-point)
(global-set-key (kbd "\C-c D") 'define-word)

;;====================================================================
;; Created by フランク <freed034@pm.me> at Caen - France, February 2015
;; Last modification made at Orsay - France, December 2021
;;===================================================================

(setq DIR "~/Repos/GLab/Emacs/")

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(when (not package-archive-contents) (package-refresh-contents))

;;===================================================================
;; Reload emacs configuration
;;===================================================================

;; Function to reload the init file
(defun reload-init-file () (interactive) (load-file "~/.emacs.d/init.el"))

;; Shorcut for reload the init file
(global-set-key (kbd "\C-c l") 'reload-init-file)

;;===================================================================
;; Server emacs
;;===================================================================

;;(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;; Function to save buffers, quit and shutdown emacs server instance
(defun server-shutdown () (interactive) (save-some-buffers) (kill-emacs))

(global-set-key (kbd "\C-c \C-k \C-r") 'server-shutdown)

;;===================================================================
;; Environment variables from shell
;;===================================================================

(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;;===================================================================
;; My functions for recognize the operative system host
;;===================================================================

(defun system-is-mac () (interactive) (string-equal system-type "darwin"))

(defun system-is-linux () (interactive) (string-equal system-type "gnu/linux"))

;; Use lsb_release
(defun which-linux-distribution ()
  (interactive)
  ;; (when (eq system-type 'gnu/linux) (shell-command-to-string "lsb_release -sd")) ;; distro with version
  (when (eq system-type 'gnu/linux) (replace-regexp-in-string
                                     "Description:\\|[\t\n\r]+" ""
                                     (shell-command-to-string "lsb_release --id --short"))))

;;===================================================================
;; Load configurations based in the operating system host
;;===================================================================

(if (system-is-linux)
    (progn
      (when (string-equal (which-linux-distribution) 'Arch)
        (setq custom-file (concat DIR "custom_arch.el"))
        (setq my_config (concat DIR "conf_arch.el")))
      (when (string-equal (which-linux-distribution) 'Ubuntu)
        (setq custom-file (concat DIR "custom_ubun.el"))
        (setq my_config (concat DIR "conf_ubun.el")))))

(if (system-is-mac)
    (progn
      (setq custom-file (concat DIR "custom_macos.el"))
      (setq my_config (concat DIR "conf_macos.el"))))

;; First, I load the custom-file for install the packages
(load custom-file)

;; Installing packages
(package-install-selected-packages)
;; (unless (package-installed-p 'auctex) (package-install 'auctex))

(load-file my_config)

;;===================================================================
;; Path for the manually download packages
;;===================================================================

;;(add-to-list 'load-path (expand-file-name "~/.elisp/"))
;;(add-to-list 'load-path (concat DIR "packages"))
;;(setq manual-directory (concat DIR "packages"))

;;===================================================================
;; Define word bindings
;;===================================================================

(global-set-key (kbd "\C-c d") 'define-word-at-point)
(global-set-key (kbd "\C-c D") 'define-word)

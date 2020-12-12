;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; TODO(Austin): Migrate to emacs 27 when this is fixed?
;; https://github.com/hlissner/doom-emacs/issues/1170

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Austin Green"
      user-mail-address "agreen@liftoff.io")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-outrun-electric)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Begin Austin

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(map! :leader
      :desc "Split window vertically"
      "w /" #'evil-window-vsplit)

(map! :leader
      :desc "Split window horizontally"
      "w -" #'evil-window-split)

(map! :leader
      :desc "Switch window right"
      "w <right>" #'evil-window-right)

(map! :leader
      :desc "Switch window left"
      "w <left>" #'evil-window-left)

(map! :leader
      :desc "Switch window down"
      "w <down>" #'evil-window-down)

(map! :leader
      :desc "Switch window up"
      "w <up>" #'evil-window-up)

(evil-ex-define-cmd "W" #'evil-write)
(evil-ex-define-cmd "Q" #'evil-quit)

(defun sync-env ()
  (when (memq window-system '(mac ns x))
    (let ((env-pair-re "^\\([^=[:space:]]+\\)=\\(.*\\)$"))
      (with-temp-buffer
        (shell-command (concat shell-file-name " -i -c e") t)
        (goto-char (point-min))
        (while (re-search-forward env-pair-re nil t)
          (let ((name (match-string 1))
                (val (match-string 2)))
            (setenv name val)
            (when (string-equal "PATH" name)
              (setq eshell-path-env val
                    exec-path (append (parse-colon-path val) (list exec-directory))))))))))

(sync-env)

(setq projectile-project-search-path "${REPOS}/liftoff/")

; Cider config (for my packages)
; (setq cider-required-middleware-version "0.25.5")
; (setq cider-inject-dependencies-at-jack-in nil)
(setq cider-clojure-cli-global-options "-A:liftoff:dev:nrepl")

;; Enable formatting on save
(load! "$REPOS/liftoff/exp/emacs/cljfmt.el")
(add-hook 'before-save-hook 'cljfmt-before-save)

;; Add go commands
(setq gofmt-command "goimports")
(setq gofmt-args '("-local" "liftoff/"))
(add-hook 'before-save-hook 'gofmt-before-save)

;; Increase size of recentf
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 200)

; Use diff-hl-mode always
(global-diff-hl-mode)

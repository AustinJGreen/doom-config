;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; TODO(Austin): Migrate to emacs 27 when this is fixed?
;; https://github.com/hlissner/doom-emacs/issues/1170

(setq user-full-name "Austin Green"
      user-mail-address "agreen@liftoff.io"
      doom-theme 'doom-outrun-electric
      org-directory "~/org/"
      display-line-numbers-type nil
      ; Add go commands
      gofmt-command "goimports"
      gofmt-args '("-local" "liftoff/")
      ; Cider config (for my packages)
      ; (setq cider-required-middleware-version "0.25.5")
      ; (setq cider-inject-dependencies-at-jack-in nil)
      cider-clojure-cli-global-options "-A:liftoff:dev:nrepl"
      ;; Increase size of recentf
      recentf-max-menu-items 25
      recentf-max-saved-items 200
      projectile-project-search-path "${REPOS}/liftoff/"
      centaur-tabs-height 16
      centaur-tabs-set-close-button nil)

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

;; Enable formatting on save
(load! "$REPOS/liftoff/exp/emacs/cljfmt.el")
(add-hook 'before-save-hook 'cljfmt-before-save)
(add-hook 'before-save-hook 'gofmt-before-save)

; Use diff-hl-mode always
(global-diff-hl-mode)

; Use centaur tabs mode
(map! :leader
      :desc "Go to left tab"
      "<left>" #'centaur-tabs-backward)

(map! :leader
      :desc "Go to right tab"
      "<right>" #'centaur-tabs-forward)

; Enable centuar mode by default
(centaur-tabs-mode)

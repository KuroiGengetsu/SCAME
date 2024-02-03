

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; �����ļ��� ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ���������ļ��ض���
;; �� ~/.emacs.d/.emacs �����������, �Ϳ��Խ������ļ��ض��򵽱��ļ�
;; (load-file "D:/Coding/MyGithub/SCAME/.emacs")

;; ���ļ�Ϊ
;; D:/Coding/MyGithub/SCAME/.emacs

;;; code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ���� ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ���ù��ھ���
(setq package-archives '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")))
(require 'package)
;(add-to-list 'package-archives
;             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

;; package-install �����Զ��������ð�
(setq package-install-upgrade-built-in t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ��ʾ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ������ʾ�к�
(global-display-line-numbers-mode 1)


;;;; ������������
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
		    charset
		    (font-spec :family "WenQuanYi Micro Hei Mono" :size 16)))


;;;; frame �ߴ�
;(set-frame-position (selected-frame) 0 0)
;; Ĭ�����
(add-to-list 'default-frame-alist '(fullscreen . maximized))


;;;; neotree
(use-package neotree
  :ensure t
  :config (global-set-key [f8] 'neotree-toggle)  ;; ���� f8 ����/�ر� neotree
  (setq neo-smart-open t))


;;;; Emacs�˵�������ʾ�ļ���
(use-package centaur-tabs
  :ensure t
  :config
  (setq centaur-tabs-style "bar"
    centaur-tabs-height 22
    centaur-tabs-set-icons t
    centaur-tabs-plain-icons t
    centaur-tabs-gray-out-icons t
    centaur-tabs-set-close-button t
    centaur-tabs-set-modified-marker t
    centaur-tabs-show-navigation-buttons t
    centaur-tabs-set-bar 'left
    centaur-tabs-cycle-scope 'tabs
    x-underline-at-descent-line nil)
  (centaur-tabs-headline-match)
  ;; (setq centaur-tabs-gray-out-icons 'buffer)
  ;; (centaur-tabs-enable-buffer-reordering)
  ;; (setq centaur-tabs-adjust-buffer-order t)
  (centaur-tabs-mode t)
  (setq uniquify-separator "/")
  (setq uniquify-buffer-name-style 'forward)
  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.
 Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
 All buffer name start with * will group to \"Emacs\".
 Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
      ((ignore-errors
     (and (string= "*xwidget" (substring (buffer-name) 0 8))
          (not (string= "*xwidget-log*" (buffer-name)))))
       "Xwidget")
      ((or (string-equal "*" (substring (buffer-name) 0 1))
       (memq major-mode '(magit-process-mode
                  magit-status-mode
                  magit-diff-mode
                  magit-log-mode
                  magit-file-mode
                  magit-blob-mode
                  magit-blame-mode
                  )))
       "Emacs")
      ((derived-mode-p 'prog-mode)
       "Editing")
      ((derived-mode-p 'dired-mode)
       "Dired")
      ((memq major-mode '(helpful-mode
              help-mode))
       "Help")
      ((memq major-mode '(org-mode
              org-agenda-clockreport-mode
              org-src-mode
              org-agenda-mode
              org-beamer-mode
              org-indent-mode
              org-bullets-mode
              org-cdlatex-mode
              org-agenda-log-mode
              diary-mode))
       "OrgMode")
      (t
       (centaur-tabs-get-group-name (current-buffer))))))
  :hook
  (dashboard-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  (helpful-mode . centaur-tabs-local-mode)
  :bind
  ("C-c b" . centaur-tabs-backward)
  ("C-c n" . centaur-tabs-forward)
  ("C-c m" . centaur-tabs-forward-group)
  ("C-c v" . centaur-tabs-backward-group))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; �������ͨ�� ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; �Զ���ȫ����
(electric-pair-mode t)


;;;; ������ɫ
(use-package highlight-parentheses
  :ensure t
  :init (highlight-parentheses-mode))

;;;; �Զ���ȫ company-mode
(use-package company
  :ensure t  ;; ȷ������װ
  :init (global-company-mode)
  :config
  (setq company-minimum-prefix-length 1)  ;; ֻ���� 1 ����ĸ�Ϳ�ʼ�����Զ���ȫ
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay 0.0)
  (setq company-show-numbers t)  ;; ��ѡ���� (����ݼ� M-1��M-2 �ȵ�������ѡ��).
  (setq company-selection-wrap-around t)
  (setq company-transformers '(company-sort-by-occurrence)))  ;; ����ѡ���Ƶ�ʽ������򣬶��������ϲ������ȥ��


;;;; flycheck�﷨����
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; C���� ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CC-mode
(add-hook 'c-mode-hook '(lambda ()
        (setq ac-sources (append '(ac-source-semantic) ac-sources))
        (local-set-key (kbd "RET") 'newline-and-indent)
        (linum-mode t)
        (semantic-mode t)))


;;;; cedet
(require 'cedet)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Python���� ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; �ı��� ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Markdown Mode

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  (setq markdown-enable-math t))

;; enable markdown-enable-math
;;(setq markdown-enable-math t)


;;;; plantuml
;; active Org-babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(;; other Babel languages
   (plantuml . t)))

(setq org-plantuml-jar-path
      (expand-file-name "F:/Linux/plantuml/plantuml-mit-1.2024.0.jar"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ������ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(fset 'yes-or-no-p 'y-or-n-p)

(provide 'emacs)
;;; .emacs ends here


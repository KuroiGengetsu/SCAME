;;; -*- coding: utf-8 -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 配置文件类 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 设置配置文件重定向
;; 在 ~/.emacs.d/.emacs 中添加以下行, 就可以将配置文件重定向到本文件
;; (load-file "D:/Coding/MyGithub/SCAME/.emacs")

;; 本文件为
;; D:/Coding/MyGithub/SCAME/.emacs

(defun open-init-file ()
  "Open .emacs ."
  (interactive)
  (find-file "D:/Coding/MyGithub/SCAME/.emacs"))

;;; code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 镜像 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 配置国内镜像
(setq package-archives '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")))
(require 'package)
;(add-to-list 'package-archives
;             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

;; package-install 命令自动更新内置包
(setq package-install-upgrade-built-in t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 显示 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 设置显示行号
(global-display-line-numbers-mode 1)


;;;; 设置中文字体
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
		    charset
		    (font-spec :family "WenQuanYi Micro Hei Mono" :size 16)))


;;;; frame 尺寸
;(set-frame-position (selected-frame) 0 0)
;; 默认最大化
(add-to-list 'default-frame-alist '(fullscreen . maximized))


;;;; neotree
(use-package neotree
  :ensure t
  :defer 5
  :config (global-set-key [f8] 'neotree-toggle)  ;; 设置 f8 来打开/关闭 neotree
  (setq neo-smart-open t))


;;;; Emacs菜单栏下显示文件名
(use-package centaur-tabs
  :ensure t
  :defer 6
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 编程语言通用 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 自动补全括号
(electric-pair-mode t)


;;;; 括号颜色
(use-package highlight-parentheses
  :ensure t
  :defer 7
  :init (highlight-parentheses-mode))

;;;; 自动补全 company-mode
(use-package company
  :ensure t  ;; 确保被安装
  :defer 8
  :init (global-company-mode)
  :config
  (setq company-minimum-prefix-length 1)  ;; 只需敲 1 个字母就开始进行自动补全
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay 0.0)
  (setq company-show-numbers t)  ;; 给选项编号 (按快捷键 M-1、M-2 等等来进行选择).
  (setq company-selection-wrap-around t)
  (setq company-transformers '(company-sort-by-occurrence)))  ;; 根据选择的频率进行排序，读者如果不喜欢可以去掉


;;;; flycheck语法纠错
(use-package flycheck
  :ensure t
  :defer 9
  :init (global-flycheck-mode))

;;;; Emacs 备份文件目录
(setq backup-directory-alist (quote (("." . "E:\\softwares\\emacs\\emacs-backup-files"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; C语言 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CC-mode
(add-hook 'c-mode-hook '(lambda ()
        (setq ac-sources (append '(ac-source-semantic) ac-sources))
        (local-set-key (kbd "RET") 'newline-and-indent)
        (linum-mode t)
        (semantic-mode t)))


;;;; cedet
(require 'cedet)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Python语言 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 文本类 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;;;; todotxt
(use-package todotxt
  :mode ("todo.txt'" . todotxt-mode)
;  :init (todotxt-mode)
  :config (setq todotxt-file "D:/Coding/Todo/todo.txt"))

;;;; pyim
(use-package pyim
  :defer 13
  :ensure t
  :config (pyim-basedict-enable)
  (setq default-input-method "pyim"))

(use-package pyim-basedict
  :ensure t
  :after (pyim))

(use-package cnfonts
  :ensure t
  :config (cnfonts-mode t))
;  (cnfonts-edit-profile)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 命令类 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(fset 'yes-or-no-p 'y-or-n-p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 工具类 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package wgrep
  :defer 10
  :ensure t)
(use-package s
  :defer 11
  :ensure t)
(use-package transient
  :defer 12
  :ensure t)
(use-package ripgrep
  :ensure t
  :defer 13
  :after (wgrep s transient)
  :config (global-set-key (kbd "C-c s") #'rg-menu))

(provide 'emacs)
;;; .emacs ends here

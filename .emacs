

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; �����ļ��� ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ���������ļ��ض���
;; �� ~/.emacs.d/.emacs �����������, �Ϳ��Խ������ļ��ض��򵽱��ļ�
;; (load-file "D:/Coding/MyGithub/SCAME/.emacs")

;; ���ļ�Ϊ
;; D:/Coding/MyGithub/SCAME/.emacs


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ���� ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ���ù��ھ���
(setq package-archives '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")))
(require 'package)
;(add-to-list 'package-archives
;             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ����� ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CC-mode
(add-hook 'c-mode-hook '(lambda ()
        (setq ac-sources (append '(ac-source-semantic) ac-sources))
        (local-set-key (kbd "RET") 'newline-and-indent)
        (linum-mode t)
        (semantic-mode t)))


;;;; cedet
(require 'cedet)


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; �ı��� ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Markdown Mode

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

;; enable markdown-enable-math
(setq markdown-enable-math t)



;;;; plantuml
;; active Org-babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(;; other Babel languages
   (plantuml . t)))

(setq org-plantuml-jar-path
      (expand-file-name "F:/Linux/plantuml/plantuml-mit-1.2024.0.jar"))



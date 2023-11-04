# Learn use-package

<https://www.gnu.org/software/emacs/manual/html_mono/use-package.html>

## 0 ʹ�÷����ܽ� ##

ʹ�÷����ܽ�����

* `use-package ��`
* `:preface` ����ִ�е����
* `:init` ���ذ�֮ǰִ�е����
* `:config` ���ذ�֮��ִ�е����
* `:defer` ����Ϊ t ���Զ����ذ�, ����Ϊ����, ��������֮����ذ�, ����Ϊ nil, �����ذ�
* `:if` ��ͬ�� `:when` ������emacs����ʱ���ذ�, �� `:unless` �෴, ������ʱ���ذ�
* `:after` ���л����ذ�, ��ĳЩ������֮������ظð�
* `:requires` ����������ʱ��ֹ����
* `:load-path` ����·���µİ�
* `:command` �Զ�����һ���ɽ���������
* `:autoload` �Զ����ز��ɽ����ĺ���
* `:bind` һ�� cons ����һ�б�� cons, �����󶨵Ŀ�ݼ����Զ����ذ�, `(��ݼ� . ���еĺ���)`, ��ݼ��ĸ�ʽ `"M-x"`��`"M-<f5>"`��`[f10]`��`[S-f10]`
* `:hook` �Ӱ��м���һ��ģʽȻ�������һ��ģʽ�� `:hook (prog-mode-hook . ace-jump-mode)`
* `:mode` ��ָ�����ļ�ʱ���ذ�, ����������ʽ `:mode "\\.rb\\'"`��`:mode ("\\.py\\'" . python-mode)`
* `:interpreter` ���ý����� `:interpreter "ruby"`��`:interpreter ("python" . python-mode)`
* `:magic` ִ��һ�����, ����ļ���ͷƥ������ذ�
* `:custom` ���ð��еľֲ�����, �� setq ��ȫ
* `:custom-face` �Զ�����Ľ���
* `:diminish` ������Ҫģʽ, ��Ҫ��װ diminish ��, `:delight` ����
* `:ensure t` �Զ���װ��
* `:pin` ָ��������Դ
* `:disabled t` ����һ����

## 1 �����½�  ##

1. ��һ�������������ö�����һ��, �����ƶ����߷����ļ���
2. �����ظ������, �����൱ֱ�۵عؼ���
3. ����Emacs������ʱ��
4. ȷ�������������������Ĵ���ֻ������������İ������Ҿ������ٵؽ������������Ӷ�ʹEmacs�����ܽӽ�����ȫ���ܡ�
5. ����Գ�ʼ���ļ������ֽڱ��룬����������ʱ�������κξ���������������ġ�ͨ�����ַ�ʽ����ʹ��ʹ���ֽڱ���������ٶ�(�μ�����ĵ�3��)������Ȼ�������������Լ�顣

use-package �������������ԭ�е� `M-x customize`, �������໥����

## 2 ʹ�÷��� ##

### ���ذ��ĳ��÷��� ###

#### `use-package` �� `require` ####

`use-package` ����Լ���һ����, ÿ�ε��� `use-package`�걻����һ�� *����declaration*

``` elisp
(use-package foo)
```

���������ͬ��ʹ�� `require`

#### `:init` ���ذ�֮ǰִ�е� elisp ####

``` elisp
(use-package foo
  :init
  (setq foo-variable t))
```

#### `:config` ���ذ�֮��ִ�е� elisp ####

``` elisp
(use-package foo
  :init
  (setq foo-variable t)
  :config
  (foo-mode 1))
```

#### `:defer t` �Զ����ذ� ####

��Щ����֧���Զ����ص�, ��ʹ�õ�ʱ������, ����������emacs��ʱ��

``` elisp
(use-package foo
  :defer t)
```

#### �Զ����Լ����Զ����� autoload ####

����������ӻ��Զ��� *color-moccur.el* ���� `isearch-moccur` ���� �� `isearch-all` ����, Ȼ��󶨰�����

�������������õ���ʱ��, ������ͻ��Զ�����, ���ʱ��, *moccur-edit* Ҳ�����, ����༭ *moccur* buffer

``` elisp
(use-package color-moccur
  :commands (isearch-moccur isearch-all)
  :bind (("M-s 0" . moccur)
         :map isearch-mode-map
         ("M-o" . isearch-moccur)
         ("M-0" . isearch-moccur-all))
  :init
  (setq isearch-lazy-highlight t)
  :config
  (use-package moccur-edit))
```

## 3 ���ذ� ##

��ʹ�� emacs ���õ� `install-package` ���װ��ʱ, �����Զ��ӵ� *load-path* ·���С�

�����еİ���ֹһ����(library), �����������Ҫд��� `use-package` ������

use-package ���Ժ� *package.el* ����, �Ӷ��� emacs ������ʱ��װ��


### 3.1 use-package ������ʲôʱ�� ���ذ� ###

`use-package` ������̼��ذ��������ڵ�һ��ʹ��ʱͨ��autoloading���ذ���

����򵥵����, ��ִ�� `use-package` ��ʱ����ذ�, ������������ ��ʼ���ļ���ʱ, ����emacs������ʱ����ذ���

����:

``` elisp
(use-package foo)
```

�ͻ����̼��� library foo, �� `require` һ����

��� foo ���� *load-path* ��ʱ, ���� Message buffer ��������

��Ҫע�����, һ�� *��package* �� elisp�� *��library* �ǲ�һ����

�ϱ������������� library *foo.el*, ����������, *foo.el* ������Ϊ foo �İ��С�

���� ��foo ���ܻ������һ�� *foo-extra.el* �Ŀ�, �������Ⲣ�����Զ����ص�, ����Ҫ����д `use-package` ������ȷ�����ļ��ء�

### 3.2 �ӳٰ��ļ��� deferring package loading ###

#### `:defer` �ؼ��� ####

���û��ָ���κ��Զ�����ؼ���, `use-package` �����̼��ذ�

`:defer` �ؼ�����һ����������, һ����nil ��ֵ��ʾ��Ҫ���̼���

``` elisp
(use-package foo
  :defer t)
```

һ�㲻��ֱ��ʹ�� `:defer t`, ʹ�� `:defer t` ��ʱ��ͨ�����Ǻ� `:config` �� `:ensure` һ��ʹ��


##### N��֮����� #####

30��֮�����:

``` elisp
(use-package foo
  :defer 30)
```

##### ʲôʱ��ʹ�� `:defer` #####

ʹ���Զ����صĹؼ���ʱ, ��û��Ҫ���� defer ��

���� `use-package-always-defer` �ǿ�, ��ô `use-package` ���Ĭ���� `:defer t`, �������ͨ��ָ�� `:defer nil` ���� `:demand t` ������

### 3.3 �ð����̼��� ###

��ʹ����һЩ�Զ����صĹؼ���, ��������� `:demant t`, ��ô���ͻ����̼���

### 3.4 �����������жϼ��� ###

�⼸���ؼ�������:

* `:if`
* `:when`
* `:unless`


���Ƕ�����һ������

`:if` �ؼ��ֵĲ�����nilʱ, ���ͻᱻ����

`:when` �ؼ��ֺ� `:if` �ȼ�

`:unless` �պ��෴, `:unless foo` �� `:if (not foo)` �ȼ�

����, ���ֻ����ͼ�ν����emacs�м��ذ�, ��ô:

``` elisp
(use-package foo
  :if (display-graphic-p))
```

#### һЩ���õĳ��� ####

##### ����ϵͳ #####

�� Linux ��:

``` elisp
:if (eq system-type 'gnu/linux')
```

�� ����ϵͳ macOS �� X:

``` elisp
:if (memq window-system '(ns x))
```

�Ѿ���װ�İ�:

``` elisp
:if (package-installed-p 'foo)
```

���� `load-path` ��:

``` elisp
:if (locate-library "foo.el")
```

#### �����Ҫ��ĳ�������ʹ�� `use-package` ####

``` elisp
(when (memq window-system '(mac ns))
  (use-package foo
    :ensure t))
```

### 3.5 ���л����ذ� ###

`:after` �ؼ��ֿ������Ѿ�������ĳЩ����ǰ����, ���ذ�

``` elisp
(use-package hydra)

(use-package ivy)

(use-package ivy-hydra
  :after (ivy hydra))
```

`:after` �ؼ���Ҳ���Խ��� *a list of selectors*

Ĭ�������, `:after (foo bar)` �ȼ��� `:after (:all foo bar)`

��˼���ǵ� foo �� bar �����ص�ʱ��Ż���ذ�

������һЩ���:

``` elisp
:after (foo bar)
:after (:all foo bar)
:after (:any foo bar)
:after (:all (:any foo bar) (:any baz quux))
:after (:any (:all foo bar) (:all baz quux))
```

`:all` ����ȫ��Ҫ����, `:any` ������������һ��

> ��ʹ�� `use-package-always-defer` �� nil, ͬʱʹ���� `:after` ʱ, ��Ҫָ���������Ҫ��ʲô����¼���, ����ʹ�� `:bind` ���Զ����ء����ʲô��ûָ��, ��ô�����������ԶҲ�������

### 3.6 �������������ʱ��ֹ���� ###

`:requires`  �ؼ��ֻ�����������ʱ��ֹ����

``` elisp
(use-package abbrev
  :requires foo)
```

�ȼ���:

``` elisp
(use-package abbrev
  :if (featurep 'foo))
```

Ҳ����ָ��һ���б�

``` elisp
(use-package abbrev
  :requires (foo bar baz))
```

### 3.7 �ֶ���װ�� ###

#### 3.7.1 �����Զ���� *load-path* ####

���ֶ���װһ����ʱ, ����ȷ�����Ŀ��� *load-path* ��

`:load-path` �ؼ��ֿ������ *load-path*, �����ܵĹؼ��ְ��� symbol��function��string��list of strings

``` elisp
(use-package org
  :load-path "site-lisp/org/lisp/
  :commands org-mode")
```

��ʹ�� symbol ���� function ��ʱ��, ����֪ͨ byte-compiler ���ǵĶ���, �����ͨ�� `eval-and-compile` (��`eval-when-compile`�պ��෴)ʵ�ֵ�

``` elisp
(eval-and-compile
  (defun ess-site-load-path ()
    (shell-command-to-string "find ~ -path ess/lisp")))

(use-package ess-site
  :load-path (lambda () (list (ess-site-load-path)))
  :commands R)
```

#### 3.7.2 �����Զ����� ####

���һ�������ĵ���û��ָ����������Զ�����, ��ô��Ҫ�Լ��ֶ�����

�Զ�����һ�� *interactive command* ʹ�� `:command` �ؼ���, �����ܵĲ�����һ������һ�б�� symbols

�����Զ�������Щ����, ����Щ����ʹ�õ�ʱ��, �Ż���ذ�

`:autoload` �ؼ��ֺ� `:commands` �ؼ��ֽ��ܵĲ���һ��, ���� `:autoload` �������Զ����� *non-interactive functions*

``` elisp
(use-package org-crypt
  :autoload org-crypt-use-before-save-magic)
```

## 4 ���ð� Configuring Packages ##

### 4.1 ʹ�� Lisp���� ���ð� ###

���õ��Զ���ؼ�����:

* `:preface`: ����ִ��
* `:init`: �ڵ����֮ǰִ�д���
* `:config`: �ڵ����֮��ִ�д���

#### 4.1.1 `:preface` ����ִ�� ####

preface ��ǰ�Ե���˼

���� `:disabled` �� `:ensure` ����, `:preface` ������ִ�е�

`:preface` �����������������ͱ����Ķ���:

1. �����ᱧԹδ֪�����Ķ���
2. ���庯�������, ������ `:if` ��ʹ��

���������� `:preface` ������ side-effects

#### 4.1.2 `:init` �ڼ��ذ�֮ǰִ�� ####

`:init` ���ڼ��ذ�֮ǰִ�е�, ������������ִ��, ��ʹϵͳ��û��װ�����, ������Ҫ���� `:init` �Ĵ���, ������ִ�гɹ�

Ȼ�������Ĳ��ַŵ� `:config` ����

#### 4.1.3 `:config` �ڼ��ذ�֮��ִ�� ####

`:config` ���ڰ�������֮��ִ�е�

����������̼��ص�, ��ô������ڼ���֮���ִ��

ͨ����˵, Ӧ���� `:init` �����򵥿��, Ȼ�������Ķ��ŵ� `:config` ��

#### ʲôʱ��ʹ�� `:preface`��`:config` �� `:init` ####

������ܵĻ�, ��������ʹ��������, ȡ����֮����ʹ�� `:bind`��`:hook`��`:mode`, ��Ϊ���ǻ������Զ�����, ����Ҫ�κ� *�������boilerplate code*

����(����):

``` elisp
(use-package foo
  :init
  (add-hook 'some-hook 'foo-mode))
```

���������������:

����, �������������� foo �����

���ǿ���ͨ����� `:defer t` ����ֹ��:

``` elisp
(use-package foo
  :defer t
  :init
  (add-hook 'some-hook 'foo-mode))
```

�ڶ�������������Ҫд�ܶ���ͬ���������, �� use-package ����Ϊ�˽���������������

���������, Ӧ��ʹ�� `:hook`:

``` elisp
(use-package foo
  :hook some-hook)
```

�����ͻ��ڴ��� some-hook ʱ�Զ����� foo

### 4.2 ������ Key bindings ###

һ��������������ǰѰ��ĵ�����Ǹ����İ����󶨵�һ��

���û�� use-package, ���������Ҫ `key-map-local-set`��`keymap-global-set` �ͺܶ� autoloads һ����ܹ����

ʹ�� `use-package` ֻ��Ҫ `:bind` ����ؼ��־Ϳ���

#### 4.2.1 ȫ�ְ����� Global keybindings ####

ȫ�ְ����󶨵�ʱ��, `:bind` �ؼ��ֽ���һ�� cons ���� a list of conses

ÿ�� cons ��������ʽ:

`(KEY . COMMAND)`

KEY ��һ���ַ���, ����Ҫ�󶨵İ���, COMMAND �� command ������(a symbol)

keys���﷨�� `kbd` ��������

##### `:bind` ���� cons #####

����

``` elisp
(use-package ace-jump-mode
  :bind ("C-." . ace-jump-mode))
```

1. ��ʹ�� `ace-jump-mode` ��ʱ����Զ����ذ� ace-jump-mode
2. �� `ace-jump-mode` �󶨵� "C-."

##### `:bind` �󶨶�� cons #####

����:

``` elisp
(use-package hi-lock
  :bind (("M-o l" . highlight-lines-matching-regexp)
         ("M-o r" . highlight-regexp)
         ("M-o w" . highlight-phrase)))
```

�������������������Ӧ������


##### ʹ������İ��� #####

����İ������� *TAB* *F1-F12* �ȱ���д���ַ����еļ���������:

`"C-<up>"`

���������ⰴ��������Ͽ���ֱ��д�ڷ�������:

`[tab]`

����:

``` elisp
(use-package helm
  :bind (("M-x"    . helm-M-x)
         ("M-<f5>" . helm-find-files)
         ([f10]    . helm-buffers-list)
         ([S-f10]  . helm-recentf)))
```

##### ����ӳ������ Remapping Commands #####

������������ `fill-paragraph`(Ĭ���� `M-q`) ���°󶨵� `unfill-toggle`

``` elisp
(use-package unfill
  :bind ([remap fill-paragraph] . unfill-toggle))
```

##### `:bind` ��������ʲô #####

�������������

``` elisp
(use-package ace-jump-mode
  :bind ("C-." . ace-jump-mode))
```

����������һ�ַ�ʽʵ��:

``` elisp
(use-package ace-jump-mode
  :commands ace-jump-mode
  :init
  (bind-key "C-." 'ace-jump-mode))
```

������� `:commands` �Ļ�

``` elisp
(use-package ace-jump-mode
  :defer t
  :init
  (autoload 'ace-jump-mode "ace-jump-mode" nil t)
  (bind-key "C-." 'ace-jump-mode))
```

#### 4.2.2 �ֲ������� Key bindings in local keymaps ####

`use-package` ֧�������ڰ�����֮�����Ч�ľֲ�������

`:map`

``` elisp
(use-package helm
  :bind (:map help-command-map
              ("C-c h" . helm-execute-persistent-action)))
```

�ȵ� helm ������֮��, "C-c h" �Żᱻ�󶨵� helm-execute-persistent-action

`:map` ���Զ��ʹ��

``` elisp
(use-package term
  :bind (("C-c t" . term)
         :map term-mode-map
         ("M-p"   . "term-send-up")
         ("M-n"   . term-send-down)
         :map term-raw-map
         ("M-o"   . other-window)
         ("M-p"   . term-send-up)
         ("M-n"   . term-send-down)))
```

������ `:map` ֮ǰ�Ķ���ȫ�ְ�����

#### 4.2.3 �󶨵� keymaps ####

ͨ�� `:bind` �ڴ��Ĳ���command�Ǻ���, ���Դӵ���İ����Զ�����

��� command ��һ��������, ���ڰ�����keymap����һ������, ����ͨ�����õ� `autoload` ��������

`use-package` �ṩ��һ������Ĺؼ���: `:bind-keymap`

`:bind-keymap` Ψһ��������� command �����ǰ��е� keymap, �����ǿɻ����ĺ���

``` elisp
(use-package foo
  :bind-keymap ("C-c p" . foo-command-map))
```

#### 4.2.4 �󶨵��ظ���ӳ�� ####

һ��ʹ�þֲ�����ӳ������Ӿ��� *repeat-mode*

��Щ����ӳ��ͨ������Ϊ�˶������

`:repeat-map` �ؼ��ִ���һ�� map ������, ������к����İ���, Ȼ������ *repeat-map*����

�����������ӻᴴ��һ������ *git-gutter+-repeat-map*, ����4����, Ȼ��Ϊÿ���󶨵�command���� `repeat-map` ����

``` elisp
(use-package git-gutter+
  :bind
  (:repeat-map git-gutter+-repeat-map
   ("n" . git-gutter+-next-huk)
   ("p" . git-gutter+-previous-hunk)
   ("s" . git-gutter+-stage-hunks)
   ("r" . git-gutter+-revert-hunk)))
```

�� `:repeat-map` ������� `:exit` ������ֹ����

``` elisp
(use-package git-gutter+
  :bind
  (:repeat-map my/git-gutter+-repeat-map
   ("n" . git-gutter+-next-hunk)
   ("p" . git-gutter+-previous-hunk)
   ("s" . git-gutter+-stage-hunk)
   ("r" . git-gutter+-revert-hunk)
   :exit
   ("c" . magit-commit-create)
   ("C" . magit-commit)
   ("b" . magit-blame)))
```

�� `:exit` ����ָ�� `:continue` ���Խ��Ű�

``` elisp
(use-package git-gutter+
  :bind
  (:repeat-map my/git-gutter+-repeat-map
   :exit
   ("c" . magit-commit-create)
   ("C" . magit-commit)
   ("b" . magit-blame)
   :continue
   ("n" . git-gutter+-next-hunk)
   ("p" . git-gutter+-previous-hunk)
   ("s" . git-gutter+-stage-hunk)
   ("r" . git-gutter+-revert-hunk)))
```

#### 4.2.5 ��ʾ���˵İ����� Display personal keybindings ####

`:bind` ʹ�� `bind-key.el` �е� `bind-keys` ��

����׷�����еİ�����

`M-x describe-personal-keybindings`

### 4.3 ���� Hooks ###

`:hook` �ؼ��ְѺ�����ӵ� hooks

���Ĳ����в���Ҫ�ֶ����� `-hook` ��׺(suffix), `:hook` ���Զ�����

����������ӻ��Զ����� company ���е� `company-mode`, Ȼ��� `company-mode` �ӵ� `prog-mode-hook`

``` elisp
(use-package company
  :commands company-mode
  :init
  (add-hook 'prog-mode-hook #'company-mode))
```

���ʹ�� `:hook` ����ӷ���

``` elisp
(use-package company
  :hook (prog-mode . company-mode))
```

������, `:hook` �ؼ��ֻ��Զ����� `company-mode` ������Զ�����, ���Բ���Ҫʹ�� `:commands`

����, use-package ���Զ��ٶ������������Ǳ��ص����ּ� `-mode`

���Կ��Լ򻯵�:

``` elisp
(use-package company
  :hook prog-mode)
```

Ҳ�����ṩһ��hooks���б�

``` elisp
(use-package company
  :hook (prog-mode text-mode))

(use-package company
  :hook ((prog-mode text-mode) . company-mode))

(use-package company
  :hook ((prog-mode . company-mode)
         (text-mode . company-mode)))

(use-package company
  :commands company-mode
  :init
  (add-hook 'prog-mode-hook #'company-mode)
  (add-hook 'text-mode-hook #'company-mode))
```

һ��Ҫ��סʹ�� `:hook` ��ʱ��Ҫ�� `-hook` ��׺, ��������Ǹ�����

``` elisp
;; ����
(use-package ace-jump-mode
  :hook (prog-mode-hook . ace-jump-mode))
```

### 4.4 ģʽ�ͽ����� Modes and interpreters ###

`:mode` �� `:interpreter` �Ĳ��������� a cons cell, a list of cons cells, or a string or regexp

���������չʾ�� `ruby-mode` ��Ĭ�����á�

��һ���ļ�ƥ����������ʽ `"\\.rb\\'"`, Ҳ������չ���� `.rb`, ���ߵ�һ�� *shebang* ƥ���ַ��� "ruby"

``` elisp
(use-package ruby-mode
  :mode "\\.rb\\'"
  :interpreter "ruby")
```


Ĭ�ϵ�python��������, ���Ǳ���ʹ��һ�� cons

``` elisp
;; The package is "python" but the mode is python-mode:
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))
```

`:mode` �� `:interpreter` �ؼ��ֶ�����������ʽ���б�

``` elisp
(use-package foo
  ;; Equivalent to "\\(ba[rz]\\)\\'":
  :mode ("\\.bar\\'" "\\.baz\\'")
  ;; Equivalent to "\\(foo[ab]\\)":
  :interpreter ("fooa" "foob"))
```

### 4.5 ħ����� Magic Handlers ###

����ʹ�� `:magic` ������ĳ������, ����ļ��Ŀ�ͷƥ��һ��������ʽ, ��Щ������ʽ���ӵ��� 'magic-model-alist' �� 'magic-fallback-mode-alist' ��

`:magic` �� `:magic-fallback` �������� `:magic-fallback` �� `:mode` ���ȼ���

``` elisp
(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query))
```

������û����� *pdf-view-mode* ���Զ�����, �ӳ� *pdf-tools* �ļ���, ���buffer��ͷƥ�� "%PDF" �ͻ����� *pdf-view-mode*

### 4.6 �û�ѡ�� User options ###

�� Emacs ��, ����ͨ�� `M-x customize` �ӿ��������û�ѡ�������

�� `use-package` �п���ʹ�� `:custom` �ؼ���

``` elisp
(use-package comint
  :defer t
  :custom
  (comint-buffer-maximum-size 20000 "Increase comint buffer size.")
  (comint-prompt-read-only t "Make the prompt read only."))
```

��������� `:config` ��ʹ�� `setq` Ҫ��

�� emacs29 ���� `setopt` �����Ƶ�����

ʹ�� `:custom` ���õ�ֵ�������ŵ�emacs�� `custom-file` ��

### 4.7 Faces ###

`:custom-face` �ؼ��������Զ������ faces.

``` elisp
(use-package eruby-mode
  :custom=face
  (eruby-standard-face ((t (:slant italic)))))

(use-package example
  :custom-face
  (example-1-face ((t (:foreground "LightPink"))))
  (example-2-face ((t (:foreground "LightGreen"))) face-defspec-spec))

(use-package zenburn-theme
  :preface
  (setq my/zenburn-colors-alist
        '((fg . "DCDCCC") (bg . "#1C1C1C") (cyan . "93E0E3")))
  :custom-face
  (region ((t (:background ,(alist-get my/zenburn-colors-alist 'cyan)))))
  :config
  (load-theme 'zenburn t))
```

### 4.8 �� diminish�ݼ� �� delightϲ�� �����ش�Ҫģʽminor modes ###

`use-package` ֧�� *diminish* �� *delight* ��, �������������Ƴ���ı� mode-line �Ĵ�Ҫģʽ�ַ���

ͨ��ʹ������֮һ, ��Ҫһ��ʹ��

#### 4.8.1 Diminish ####

����װ�� `diminish` ��ʱ, �Ϳ���ʹ�� `:diminish` �ؼ���

�����ڳ�ʼ���ļ������

``` elisp
(use-package diminish
  :ensure t)
```

`:ensure t` ��֤���ᱻ��װ

`:diminish` �ؼ��ֵĲ���:

* a monor mode symbol
* a cons of the symbol
* or its replacement string

``` elisp
(use-package abbrev
  :diminish abbrev-mode
  :config
  (if (file=exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))
```

#### 4.8.2 Delight ####

����װ�� `Delight` ��, �Ϳ���ʹ�� `:delight` �ؼ���

``` elisp
(use-package delight
  :ensure t)
```

`:delight` �Ĳ���:

* a monor mode symbol
* a replacement string
* quoted mode line data

�������л����� `foo-mode` ���еĴ�Ҫģʽ

``` elisp
(use-package foo
  :delight)
```

�������л����� `auto-revert-mode`

``` elisp
;; Don't show anything for auto revert-mode, which doesn't match its package name.
(use-package autorevert
  :delight auto-revert-mode)
```

����Ҳ����ʹ������ Lisp ����

�����õ�ǰ buffer ������

``` elisp
(use-package foo
  :delight '(:eval buffer-file-name))
```

�����������չʾ�����ض�����ô�Ҫģʽ

``` elisp
;; Completely hide visual-line-mode Ȼ��� auto-file-mode ��Ϊ " AF".
(use-package emacs
  :delight
  (auto-fill-function " AF")
  (visual-line-mode))
```

## 5 �Զ���װ�� ##

`:ensure` �� `:pin` �����Զ���װ��

### 5.1 ��װ�� ###

`:ensure` ���emacsû��������Ͱ�װ

``` elisp
(use-package magit
  :ensure t)
```

��������ȫ�ֱ���

``` elisp
(require 'use-package-ensure)

(setq use-package-always-ensure t)
```

����ͨ�� `:ensure nil` ����

### 5.2 ָ����װ��Դ pin ###

����ͨ�� `:pin` ��ָ���Ӳ�ͬ�� archive(����) ��ƥ���

�� GNU ELPA ���� NonGNU ELPA, ���������

``` elisp
(use-package company
  :ensure t
  :pin gnu)  ; GNU ELPA
```

�������ʹ�õ������Ŀ� MELPA, ����� `use-package-always-pin` ����Ϊ `nongnu`, ��Ϊ���漰����������

���Ҫʹ���ֶ���װ�İ汾, ������ `:pin manual`

``` elisp
(use-package org
  :ensure t
  ;; ignore org-mode from upstream and use a manually installed version
  :pin manual)
```

�����һ���� pin ��һ��û���� `package-archives` ���õ���Դ, �ͻᱨ��

### 5.3 �Ǳ�׼�������� ###

use-package Ĭ��ʹ�����õ� *package.el*

���Ҫʹ�ñ�İ�������

�Ͱ� `use-package-ensure-function` ����Ϊ����������

����ο���������������

## 6 ���� init �ļ� ##

ͨ��������ѳ�ʼ���ļ�����ɶ�����

`:defines` �� `:functions` ������������ͺ���������ֻ��Ϊ�������ֽڱ������ľ���

## 7 �쳣���� ##

ͨ�� use-package �����Ⲣ����Ӱ�� emacs ����, ��Ѿ������ warning ��

��� emacs ����������, ����ʱ�ͼ��� `--debug-init`, ����ø���� debug ��Ϣ

``` elisp
(when init-file-debug
  (setq use-package-verbose t
        use-package-expand-minimally nil
        use-package-compute-statistics t
        debug-on-error t))
```

### 7.1 �쳣�����ѡ�� ###

use-package Ĭ�ϻᱨ�����

�����ص�, ��ʹ�� `use-package-expand-minimally` ��Ϊ��nil

�������ͨ�� `:catch` ������

`:catch t` ��ʾʹ�ܲ����쳣

Ҳ������һ���������������ĺ���

* ����ʱ�Ĺؼ���
* ������� (�� `condition-case` ����)

``` elisp
(use-package example
  :preface (message "I'm here at byte-compile and load time")
  :init (message "I'm always here at startup")
  :config
  (message "I'm always here after the package is loaded")
  (error "oops")
  ;; Don't try to (require 'example), this is just an example!
  :no-require t
  :catch (lambda (keyword err)
           (message (error-message-string err))))
```

ִ����������ͻ��ӡ

```
I'm here at byte-compile and load time
I'm always here at startup
Configuring package example...
I'm always here after the package is loaded
oops
```

### 7.2 �ռ�ͳ�� ###

��� `use-package-verbose t` ���߰��ļ��س��� 0.1 ��, �ͻῴ���������Ϣ

����뿴�����е���Ϣ, �� `use-package-compute-statistics t`

���� `M-x use-package-report` ���鿴���

���� `M-x use-package-reset-statistics` �����ý��

### 7.3 ����һ���� ###

`:disabled` �����һ����

``` elisp
(use-package foo
  :disabled)
```

## ��չ�ؼ��� ##

### A.1 ###

`:ensure-system-package` ����ʹ�� apt-get ���� yum ֮�������װ��

### A.2 ��δ����ؼ��� ###

��


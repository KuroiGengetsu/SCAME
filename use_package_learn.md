# Learn use-package

<https://www.gnu.org/software/emacs/manual/html_mono/use-package.html>

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





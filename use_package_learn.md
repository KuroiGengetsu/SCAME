# Learn use-package

<https://www.gnu.org/software/emacs/manual/html_mono/use-package.html>

## 0 使用方法总结 ##

使用方法总结如下

* `use-package 包`
* `:preface` 最先执行的语句
* `:init` 加载包之前执行的语句
* `:config` 加载包之后执行的语句
* `:defer` 设置为 t 则自动加载包, 设置为整数, 在整数秒之后加载包, 设置为 nil, 不加载包
* `:if` 等同于 `:when` 在满足emacs条件时加载包, 和 `:unless` 相反, 不满足时加载包
* `:after` 序列化加载包, 当某些包加载之后则加载该包
* `:requires` 依赖不满足时阻止加载
* `:load-path` 加载路径下的包
* `:command` 自动加载一个可交互的命令
* `:autoload` 自动加载不可交互的函数
* `:bind` 一个 cons 或者一列表的 cons, 触发绑定的快捷键就自动加载包, `(快捷键 . 包中的函数)`, 快捷键的格式 `"M-x"`、`"M-<f5>"`、`[f10]`、`[S-f10]`
* `:hook` 从包中加载一个模式然后挂在另一个模式中 `:hook (prog-mode-hook . ace-jump-mode)`
* `:mode` 打开指定的文件时加载包, 接受正则表达式 `:mode "\\.rb\\'"`、`:mode ("\\.py\\'" . python-mode)`
* `:interpreter` 设置解释器 `:interpreter "ruby"`、`:interpreter ("python" . python-mode)`
* `:magic` 执行一个语句, 如果文件开头匹配则加载包
* `:custom` 配置包中的局部变量, 比 setq 安全
* `:custom-face` 自定义包的界面
* `:diminish` 隐含次要模式, 需要安装 diminish 包, `:delight` 类似
* `:ensure t` 自动安装包
* `:pin` 指定包的来源
* `:disabled t` 禁用一个包

## 1 基础章节  ##

1. 将一个包的所有配置都放在一起, 方便移动或者放在文件中
2. 减少重复的语句, 都是相当直观地关键字
3. 减少Emacs的启动时间
4. 确保在启动过程中遇到的错误只禁用引发错误的包，并且尽可能少地禁用其他包，从而使Emacs尽可能接近于完全功能。
5. 允许对初始化文件进行字节编译，这样在启动时看到的任何警告或错误都是有意义的。通过这种方式，即使不使用字节编译来提高速度(参见上面的第3项)，它仍然可以用作完整性检查。

use-package 并不是用来替代原有的 `M-x customize`, 反而是相互工作

## 2 使用方法 ##

### 加载包的常用方法 ###

#### `use-package` 或 `require` ####

`use-package` 宏可以加载一个包, 每次调用 `use-package`宏被称作一个 *声明declaration*

``` elisp
(use-package foo)
```

这个声明等同于使用 `require`

#### `:init` 加载包之前执行的 elisp ####

``` elisp
(use-package foo
  :init
  (setq foo-variable t))
```

#### `:config` 加载包之后执行的 elisp ####

``` elisp
(use-package foo
  :init
  (setq foo-variable t)
  :config
  (foo-mode 1))
```

#### `:defer t` 自动加载包 ####

有些包是支持自动加载的, 在使用的时候会加载, 而不是启动emacs的时候

``` elisp
(use-package foo
  :defer t)
```

#### 自定义自己的自动加载 autoload ####

下面这个例子会自动从 *color-moccur.el* 加载 `isearch-moccur` 命令 和 `isearch-all` 命令, 然后绑定按键。

当这两个命令用到的时候, 这个包就会自动加载, 这个时候, *moccur-edit* 也会加载, 允许编辑 *moccur* buffer

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

## 3 加载包 ##

当使用 emacs 内置的 `install-package` 命令安装包时, 包会自动加到 *load-path* 路径中。

但是有的包不止一个库(library), 这种情况必须要写多个 `use-package` 声明。

use-package 可以和 *package.el* 互动, 从而在 emacs 启动的时候安装包


### 3.1 use-package 怎样、什么时候 加载包 ###

`use-package` 宏会立刻加载包、或者在第一次使用时通过autoloading加载包。

在最简单的情况, 在执行 `use-package` 的时候加载包, 如果这个声明在 初始化文件中时, 会在emacs启动的时候加载包。

例如:

``` elisp
(use-package foo)
```

就会立刻加载 library foo, 和 `require` 一样。

如果 foo 不在 *load-path* 中时, 会在 Message buffer 产生警告

需要注意的是, 一个 *包package* 和 elisp的 *库library* 是不一样的

上边这个声明会加载 library *foo.el*, 大多数情况下, *foo.el* 会在名为 foo 的包中。

但是 包foo 可能还会包括一个 *foo-extra.el* 的库, 如果这个库并不是自动加载的, 必须要额外写 `use-package` 声明来确保它的加载。

### 3.2 延迟包的加载 deferring package loading ###

#### `:defer` 关键字 ####

如果没有指定任何自动导入关键字, `use-package` 会立刻加载包

`:defer` 关键字有一个布尔参数, 一个非nil 的值表示不要立刻加载

``` elisp
(use-package foo
  :defer t)
```

一般不会直接使用 `:defer t`, 使用 `:defer t` 的时候通常都是和 `:config` 或 `:ensure` 一起使用


##### N秒之后加载 #####

30秒之后加载:

``` elisp
(use-package foo
  :defer 30)
```

##### 什么时候使用 `:defer` #####

使用自动加载的关键字时, 就没必要再用 defer 了

假如 `use-package-always-defer` 非空, 那么 `use-package` 宏就默认是 `:defer t`, 这个可以通过指定 `:defer nil` 或者 `:demand t` 来覆盖

### 3.3 让包立刻加载 ###

即使加了一些自动加载的关键字, 如果设置了 `:demant t`, 那么包就会立刻加载

### 3.4 根据条件来判断加载 ###

这几个关键字如下:

* `:if`
* `:when`
* `:unless`


他们都接受一个参数

`:if` 关键字的参数非nil时, 包就会被加载

`:when` 关键字和 `:if` 等价

`:unless` 刚好相反, `:unless foo` 和 `:if (not foo)` 等价

例如, 如果只想在图形界面的emacs中加载包, 那么:

``` elisp
(use-package foo
  :if (display-graphic-p))
```

#### 一些常用的场景 ####

##### 操作系统 #####

在 Linux 下:

``` elisp
:if (eq system-type 'gnu/linux')
```

在 窗口系统 macOS 和 X:

``` elisp
:if (memq window-system '(ns x))
```

已经安装的包:

``` elisp
:if (package-installed-p 'foo)
```

包在 `load-path` 中:

``` elisp
:if (locate-library "foo.el")
```

#### 如果需要在某种情况下使用 `use-package` ####

``` elisp
(when (memq window-system '(mac ns))
  (use-package foo
    :ensure t))
```

### 3.5 序列化加载包 ###

`:after` 关键字可以在已经加载了某些包的前提下, 加载包

``` elisp
(use-package hydra)

(use-package ivy)

(use-package ivy-hydra
  :after (ivy hydra))
```

`:after` 关键字也可以接受 *a list of selectors*

默认情况下, `:after (foo bar)` 等价于 `:after (:all foo bar)`

意思就是当 foo 和 bar 都加载的时候才会加载包

以下是一些情况:

``` elisp
:after (foo bar)
:after (:all foo bar)
:after (:any foo bar)
:after (:all (:any foo bar) (:any baz quux))
:after (:any (:all foo bar) (:all baz quux))
```

`:all` 就是全都要满足, `:any` 就是至少满足一个

> 当使用 `use-package-always-defer` 非 nil, 同时使用了 `:after` 时, 需要指定这个包需要在什么情况下加载, 例如使用 `:bind` 等自动加载。如果什么都没指定, 那么这个包可能永远也不会加载

### 3.6 如果依赖不存在时阻止加载 ###

`:requires`  关键字会在依赖不足时阻止加载

``` elisp
(use-package abbrev
  :requires foo)
```

等价于:

``` elisp
(use-package abbrev
  :if (featurep 'foo))
```

也可以指定一个列表

``` elisp
(use-package abbrev
  :requires (foo bar baz))
```

### 3.7 手动安装包 ###

#### 3.7.1 设置自定义的 *load-path* ####

当手动安装一个包时, 必须确保它的库在 *load-path* 中

`:load-path` 关键字可以添加 *load-path*, 它接受的关键字包括 symbol、function、string、list of strings

``` elisp
(use-package org
  :load-path "site-lisp/org/lisp/
  :commands org-mode")
```

当使用 symbol 或者 function 的时候, 必须通知 byte-compiler 它们的定义, 这个是通过 `eval-and-compile` (和`eval-when-compile`刚好相反)实现的

``` elisp
(eval-and-compile
  (defun ess-site-load-path ()
    (shell-command-to-string "find ~ -path ess/lisp")))

(use-package ess-site
  :load-path (lambda () (list (ess-site-load-path)))
  :commands R)
```

#### 3.7.2 设置自动加载 ####

如果一个包的文档中没有指导如何设置自动加载, 那么就要自己手动配置

自动加载一个 *interactive command* 使用 `:command` 关键字, 它接受的参数是一个或者一列表的 symbols

它会自动加载这些命令, 当这些命令使用的时候, 才会加载包

`:autoload` 关键字和 `:commands` 关键字接受的参数一样, 但是 `:autoload` 是用来自动加载 *non-interactive functions*

``` elisp
(use-package org-crypt
  :autoload org-crypt-use-before-save-magic)
```

## 4 配置包 Configuring Packages ##

### 4.1 使用 Lisp代码 配置包 ###

常用的自定义关键字有:

* `:preface`: 最先执行
* `:init`: 在导入包之前执行代码
* `:config`: 在导入包之后执行代码

#### 4.1.1 `:preface` 最先执行 ####

preface 是前言的意思

除了 `:disabled` 和 `:ensure` 以外, `:preface` 是最先执行的

`:preface` 可以用来构建函数和变量的定义:

1. 它不会抱怨未知函数的定义
2. 定义函数或变量, 可以在 `:if` 中使用

尽量避免在 `:preface` 中引入 side-effects

#### 4.1.2 `:init` 在加载包之前执行 ####

`:init` 是在加载包之前执行的, 并且是无条件执行, 即使系统中没有装这个包, 所以需要限制 `:init` 的代码, 让它能执行成功

然后把其余的部分放到 `:config` 部分

#### 4.1.3 `:config` 在加载包之后执行 ####

`:config` 是在包被加载之后执行的

如果包是立刻加载的, 那么这个会在加载之后就执行

通常来说, 应该让 `:init` 尽量简单快捷, 然后把其余的都放到 `:config` 中

#### 什么时候使用 `:preface`、`:config` 和 `:init` ####

如果可能的话, 尽量避免使用这三个, 取而代之的是使用 `:bind`、`:hook`、`:mode`, 因为他们会设置自动加载, 不需要任何 *样板代码boilerplate code*

例如(反例):

``` elisp
(use-package foo
  :init
  (add-hook 'some-hook 'foo-mode))
```

这个会有两个问题:

首先, 它会无条件加载 foo 这个包

我们可以通过添加 `:defer t` 来阻止它:

``` elisp
(use-package foo
  :defer t
  :init
  (add-hook 'some-hook 'foo-mode))
```

第二个问题是你需要写很多相同的样板代码, 而 use-package 就是为了解决这种问题而生的

这种情况下, 应当使用 `:hook`:

``` elisp
(use-package foo
  :hook some-hook)
```

这样就会在触发 some-hook 时自动加载 foo

### 4.2 按键绑定 Key bindings ###

一个常做的事情就是把包的导入和那个包的按键绑定到一起

如果没有 use-package, 这个操作需要 `key-map-local-set`、`keymap-global-set` 和很多 autoloads 一起才能够完成

使用 `use-package` 只需要 `:bind` 这个关键字就可以

#### 4.2.1 全局按键绑定 Global keybindings ####

全局按键绑定的时候, `:bind` 关键字接受一个 cons 或者 a list of conses

每个 cons 有如下形式:

`(KEY . COMMAND)`

KEY 是一个字符串, 表明要绑定的按键, COMMAND 是 command 的名字(a symbol)

keys的语法和 `kbd` 函数累死

##### `:bind` 单个 cons #####

例如

``` elisp
(use-package ace-jump-mode
  :bind ("C-." . ace-jump-mode))
```

1. 当使用 `ace-jump-mode` 的时候会自动加载包 ace-jump-mode
2. 把 `ace-jump-mode` 绑定到 "C-."

##### `:bind` 绑定多个 cons #####

例如:

``` elisp
(use-package hi-lock
  :bind (("M-o l" . highlight-lines-matching-regexp)
         ("M-o r" . highlight-regexp)
         ("M-o w" . highlight-phrase)))
```

这样会绑定三个按键到对应的命令


##### 使用特殊的按键 #####

特殊的按键例如 *TAB* *F1-F12* 等必须写在字符串中的尖括号里面:

`"C-<up>"`

单个的特殊按键或者组合可以直接写在方括号内:

`[tab]`

例子:

``` elisp
(use-package helm
  :bind (("M-x"    . helm-M-x)
         ("M-<f5>" . helm-find-files)
         ([f10]    . helm-buffers-list)
         ([S-f10]  . helm-recentf)))
```

##### 重新映射命令 Remapping Commands #####

下面的声明会把 `fill-paragraph`(默认是 `M-q`) 重新绑定到 `unfill-toggle`

``` elisp
(use-package unfill
  :bind ([remap fill-paragraph] . unfill-toggle))
```

##### `:bind` 究竟会做什么 #####

考虑下面的例子

``` elisp
(use-package ace-jump-mode
  :bind ("C-." . ace-jump-mode))
```

可以用另外一种方式实现:

``` elisp
(use-package ace-jump-mode
  :commands ace-jump-mode
  :init
  (bind-key "C-." 'ace-jump-mode))
```

如果不用 `:commands` 的话

``` elisp
(use-package ace-jump-mode
  :defer t
  :init
  (autoload 'ace-jump-mode "ace-jump-mode" nil t)
  (bind-key "C-." 'ace-jump-mode))
```

#### 4.2.2 局部按键绑定 Key bindings in local keymaps ####

`use-package` 支持设置在包导入之后才生效的局部按键绑定

`:map`

``` elisp
(use-package helm
  :bind (:map help-command-map
              ("C-c h" . helm-execute-persistent-action)))
```

等到 helm 包加载之后, "C-c h" 才会被绑定到 helm-execute-persistent-action

`:map` 可以多次使用

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

出现在 `:map` 之前的都是全局按键绑定

#### 4.2.3 绑定到 keymaps ####

通常 `:bind` 期待的参数command是函数, 可以从导入的包中自动加载

如果 command 是一个按键绑定, 由于按键绑定keymap不是一个函数, 不能通过内置的 `autoload` 函数加载

`use-package` 提供了一个特殊的关键字: `:bind-keymap`

`:bind-keymap` 唯一的区别就是 command 必须是包中的 keymap, 而不是可互动的函数

``` elisp
(use-package foo
  :bind-keymap ("C-c p" . foo-command-map))
```

#### 4.2.4 绑定到重复的映射 ####

一个使用局部按键映射的例子就是 *repeat-mode*

这些按键映射通常就是为此而定义的

`:repeat-map` 关键字传入一个 map 的名字, 会绑定所有后续的按键, 然后设置 *repeat-map*属性

接下来的例子会创建一个叫做 *git-gutter+-repeat-map*, 配置4个绑定, 然后为每个绑定的command设置 `repeat-map` 属性

``` elisp
(use-package git-gutter+
  :bind
  (:repeat-map git-gutter+-repeat-map
   ("n" . git-gutter+-next-huk)
   ("p" . git-gutter+-previous-hunk)
   ("s" . git-gutter+-stage-hunks)
   ("r" . git-gutter+-revert-hunk)))
```

在 `:repeat-map` 后面加上 `:exit` 可以阻止设置

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

在 `:exit` 后面指定 `:continue` 可以接着绑定

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

#### 4.2.5 显示个人的按键绑定 Display personal keybindings ####

`:bind` 使用 `bind-key.el` 中的 `bind-keys` 宏

它会追踪所有的按键绑定

`M-x describe-personal-keybindings`

### 4.3 钩子 Hooks ###

`:hook` 关键字把函数添加到 hooks

它的参数中不需要手动输入 `-hook` 后缀(suffix), `:hook` 会自动输入

下面这个例子会自动导入 company 包中的 `company-mode`, 然后把 `company-mode` 加到 `prog-mode-hook`

``` elisp
(use-package company
  :commands company-mode
  :init
  (add-hook 'prog-mode-hook #'company-mode))
```

如果使用 `:hook` 会更加方便

``` elisp
(use-package company
  :hook (prog-mode . company-mode))
```

在这里, `:hook` 关键字会自动设置 `company-mode` 命令的自动加载, 所以不需要使用 `:commands`

另外, use-package 会自动假定函数的名字是保重的名字加 `-mode`

所以可以简化到:

``` elisp
(use-package company
  :hook prog-mode)
```

也可以提供一个hooks的列表

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

一定要记住使用 `:hook` 的时候不要加 `-hook` 后缀, 下面这个是个反例

``` elisp
;; 反例
(use-package ace-jump-mode
  :hook (prog-mode-hook . ace-jump-mode))
```

### 4.4 模式和解释器 Modes and interpreters ###

`:mode` 和 `:interpreter` 的参数可以是 a cons cell, a list of cons cells, or a string or regexp

下面的例子展示了 `ruby-mode` 的默认配置。

当一个文件匹配了正则表达式 `"\\.rb\\'"`, 也就是扩展名是 `.rb`, 或者第一行 *shebang* 匹配字符串 "ruby"

``` elisp
(use-package ruby-mode
  :mode "\\.rb\\'"
  :interpreter "ruby")
```


默认的python配置如下, 我们必须使用一个 cons

``` elisp
;; The package is "python" but the mode is python-mode:
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))
```

`:mode` 和 `:interpreter` 关键字都接受正则表达式的列表

``` elisp
(use-package foo
  ;; Equivalent to "\\(ba[rz]\\)\\'":
  :mode ("\\.bar\\'" "\\.baz\\'")
  ;; Equivalent to "\\(foo[ab]\\)":
  :interpreter ("fooa" "foob"))
```

### 4.5 魔术句柄 Magic Handlers ###

可以使用 `:magic` 来运行某个函数, 如果文件的开头匹配一个正则表达式, 这些正则表达式被加到了 'magic-model-alist' 和 'magic-fallback-mode-alist' 中

`:magic` 和 `:magic-fallback` 的区别是 `:magic-fallback` 比 `:mode` 优先级低

``` elisp
(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query))
```

这个配置会配置 *pdf-view-mode* 的自动加载, 延迟 *pdf-tools* 的加载, 如果buffer开头匹配 "%PDF" 就会运行 *pdf-view-mode*

### 4.6 用户选项 User options ###

在 Emacs 中, 可以通过 `M-x customize` 接口来配置用户选项变量。

在 `use-package` 中可以使用 `:custom` 关键字

``` elisp
(use-package comint
  :defer t
  :custom
  (comint-buffer-maximum-size 20000 "Increase comint buffer size.")
  (comint-prompt-read-only t "Make the prompt read only."))
```

这样会比在 `:config` 中使用 `setq` 要好

在 emacs29 中有 `setopt` 起到类似的作用

使用 `:custom` 设置的值并不会存放到emacs的 `custom-file` 中

### 4.7 Faces ###

`:custom-face` 关键字允许自定义包的 faces.

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

### 4.8 用 diminish递减 和 delight喜悦 来隐藏次要模式minor modes ###

`use-package` 支持 *diminish* 和 *delight* 包, 两个包都可以移除或改变 mode-line 的次要模式字符串

通常使用其中之一, 不要一起使用

#### 4.8.1 Diminish ####

当安装了 `diminish` 包时, 就可以使用 `:diminish` 关键字

首先在初始化文件中添加

``` elisp
(use-package diminish
  :ensure t)
```

`:ensure t` 保证包会被安装

`:diminish` 关键字的参数:

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

当安装了 `Delight` 后, 就可以使用 `:delight` 关键字

``` elisp
(use-package delight
  :ensure t)
```

`:delight` 的参数:

* a monor mode symbol
* a replacement string
* quoted mode line data

下面这行会隐藏 `foo-mode` 所有的次要模式

``` elisp
(use-package foo
  :delight)
```

下面这行会隐藏 `auto-revert-mode`

``` elisp
;; Don't show anything for auto revert-mode, which doesn't match its package name.
(use-package autorevert
  :delight auto-revert-mode)
```

参数也可以使用任意 Lisp 代码

例如用当前 buffer 的名字

``` elisp
(use-package foo
  :delight '(:eval buffer-file-name))
```

下面这个例子展示了隐藏多个内置次要模式

``` elisp
;; Completely hide visual-line-mode 然后把 auto-file-mode 改为 " AF".
(use-package emacs
  :delight
  (auto-fill-function " AF")
  (visual-line-mode))
```

## 5 自动安装包 ##

`:ensure` 和 `:pin` 可以自动安装包

### 5.1 安装包 ###

`:ensure` 如果emacs没有这个包就安装

``` elisp
(use-package magit
  :ensure t)
```

可以设置全局变量

``` elisp
(require 'use-package-ensure)

(setq use-package-always-ensure t)
```

可以通过 `:ensure nil` 覆盖

### 5.2 指定安装来源 pin ###

可以通过 `:pin` 来指定从不同的 archive(档案) 中匹配包

从 GNU ELPA 或者 NonGNU ELPA, 例如第三方

``` elisp
(use-package company
  :ensure t
  :pin gnu)  ; GNU ELPA
```

如果经常使用第三方的库 MELPA, 建议把 `use-package-always-pin` 设置为 `nongnu`, 因为会涉及到包的日期

如果要使用手动安装的版本, 就设置 `:pin manual`

``` elisp
(use-package org
  :ensure t
  ;; ignore org-mode from upstream and use a manually installed version
  :pin manual)
```

如果把一个包 pin 到一个没有用 `package-archives` 设置的来源, 就会报错

### 5.3 非标准包管理器 ###

use-package 默认使用内置的 *package.el*

如果要使用别的包管理器

就把 `use-package-ensure-function` 设置为函数的名字

具体参考第三方包管理器

## 6 编译 init 文件 ##

通常不建议把初始化文件编译成二进制

`:defines` 和 `:functions` 引入虚拟变量和函数声明，只是为了消除字节编译器的警告

## 7 异常处理 ##

通常 use-package 出问题并不会影响 emacs 启动, 会把警告放在 warning 中

如果 emacs 启动有问题, 启动时就加上 `--debug-init`, 来获得更多的 debug 信息

``` elisp
(when init-file-debug
  (setq use-package-verbose t
        use-package-expand-minimally nil
        use-package-compute-statistics t
        debug-on-error t))
```

### 7.1 异常处理的选项 ###

use-package 默认会报告错误

如果想关掉, 就使用 `use-package-expand-minimally` 设为非nil

这个可以通过 `:catch` 来覆盖

`:catch t` 表示使能捕获异常

也可以是一个具有两个参数的函数

* 错误时的关键字
* 错误对象 (由 `condition-case` 生成)

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

执行上面的语句就会打印

```
I'm here at byte-compile and load time
I'm always here at startup
Configuring package example...
I'm always here after the package is loaded
oops
```

### 7.2 收集统计 ###

如果 `use-package-verbose t` 或者包的加载超过 0.1 秒, 就会看到具体的信息

如果想看到所有的信息, 就 `use-package-compute-statistics t`

运行 `M-x use-package-report` 来查看结果

运行 `M-x use-package-reset-statistics` 来重置结果

### 7.3 禁用一个包 ###

`:disabled` 会禁用一个包

``` elisp
(use-package foo
  :disabled)
```

## 扩展关键字 ##

### A.1 ###

`:ensure-system-package` 可以使用 apt-get 或者 yum 之类的来安装包

### A.2 如何创建关键字 ###

略


# Learn use-package

<https://www.gnu.org/software/emacs/manual/html_mono/use-package.html>

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





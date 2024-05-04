# Learn cc-mode 学习 cc-mode

## 3 起步 ##

最常用的设置

缩进, 例如把缩进改成4

在 `.emacs` 中设置 `c-basic-offset` 的值

``` elisp
(setq c-basic-offset 4)
```

设置代码风格

``` elisp
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
					    (c-mode . "k&r")
                        (other . "linux")))
```

设置自动缩进, 例如输入 `;` 或者 `{` 会自动缩进, 可以通过按下 `C-c C-l` 来开关这个自动缩进, 或者配置这个变量:

``` elisp
(setq-default c-electric-flag nil)
```

让 emacs 的 `<RET>` 自动缩进

``` elisp
(defun mu-make-CR-do-indent ()
  (define-key c-mode-base-map "\C-m" 'c-context-line-break))

(add-hook 'c-initialization-hook 'my-make-CR-do-indent)
```

### 4 命令 ###

#### 在括号之间快速移动 ####

##### 在表达式之间移动 #####

* `C-M-f`: 移到下一个括号 `forward-sexp`
* `C-M-b`: 移到上一个括号 `backward-sexp`
* `C-M-k`: 删除表达式, 前移 `kill-sexp`
* `C-M-t`: 移动上一个表达式和下一个表达式 `transpose-sexps`
* `C-M-@` 或 `C-M-<SPC>`: 在下一个表达式后面放一个mark

##### 在 emacs 括号之间移动 #####

* `C-M-n`: 移动到下一个后括号, `forward-list`
* `C-M-p`: 移动到上一个前括号, `backward-list`
* `C-M-u`: 移动到上一个括号结构, `backward-up-list`
* `C-M-d`: 移动到下一个括号结构, `down-list`

### 4.1 缩进命令 ###

`<TAB>` 命令 (`c-indent-command`), 这个命令会缩进当前行, 它的行为取决于 `c-syntactic-indentation`

当 `c-syntactic-indentation` 非nil 时, 会根据风格来缩进, 为nil时, 会插入一个 `c-basic-offset` 列

如果希望插入的是制表符, 就把 `indent-tabs-mode` 设置为 t, 如果希望插入的是空格, 就设置为 nil

下面这些命令会范围缩进

`C-M-q` (c-indent-exp) 会缩进整个花括号或者括号中的表达式, 光标必须在括号上面

`C-c C-q` (c-indent-defun) 缩进整个函数, 但是不能用来缩进嵌套的类或者函数, 例如 Java 的方法Method

`C-M-\` (indent-region) 缩进所选区域

`C-M-h` (c-mark-function) 把当前的函数选中为区域

用户选项:

* *indent-tabs-mode* 为 t 时插入 制表符, nil 时插入空格
* *c-progress-interval* 把很大区域的代码缩进时, 这个变量用来控制当前进度的消息打印频率, nil 时不显示, 整数时每几秒显示一次

### 4.2 注释 ###

`C-c C-c` (comment-region) 注释区域中的行

`M-;` (comment-dwim 或 indent-for-comment) 在本行结尾插入一个注释, 然后根据 *comment-column* 进行缩进

`C-u M-;` 会删掉本行的注释, 包括注释前面的空格

`M-;` 是 emacs 的命令, 但是 cc-mode 增强了下面两个变量

* *c-indent-comment-alist* 这个变量允许更改 `M-;` 中空格的数量
* *c-indent-comments-syntactically-p* 把这个变量设置为 t, 就可以在空行使用 `M-;`, 设置为 nil 则无法使用

### 4.3 移动命令 ###

在 C mode 中移动

* `C-M-a` (c-beginning-of-defun) 函数定义开头
* `C-M-e` (c-end-of-defun) 函数定义结尾

* `M-a` (c-beginning-of-statement) 移动到语句开头
* `M-e` (c-end-of-statement) 移动到语句结尾

* `C-c C-u` (c-up-conditional) 移动到预处理语句 `#if` 或者 `#elif`, 并在当前位置留下一个 mark
* `M-x c-up-conditional-with-else` 是 c-up-conditional 的变种, 额外多了一个会停在 `#else` 处
* `M-x c-down-conditional` 移动到下一个预处理器条件语句, 会在当前位置留一个mark, 支持前缀, 如果是负数, 就往前移动
* `M-x c-up-conditional-with-else` 是 `c-down-conditional` 的变种, 会额外停在 `#else`


* `C-c C-p` (c-backward-conditional) 移动到上一个预处理条件表达式
* `C-c C-n` (c-forward-conditional) 移动到下一个预处理条件表达式

`M-x c-backward-into-nomenclature` C++

### 4.4 填充、Break ###

* `M-q` (c-fill-paragraph)
* `M-j` (c-indent-new-comment-line) 如果所在之处是注释, 那么换行之后还是注释, 如果是宏, 会在结尾自动插入反斜杠
* `M-x c-context-line-break` 
* `M-x c-context-open-line`

### 4.5 辅模式 ###

#### comment style ####

指定注释相关的命令, 插入 `/**/` 还是 `//`

`C-c C-k` (c-toggle-comment-style) 开关 comment style

#### electric mode ####

特定的字符输入之后会 reformatting

`C-c C-l` (c-toggle-electric-state) 开关 electric mode

#### auto-newline mode ####

在特定的地方会自动换行, 需要开启 electric mode

`C-c C-a` (c-toggle-auto-newline) 开关 auto-newline mode

#### hungry-delete mode ####

用来删除空格, 尤其是缩进后的

`M-x c-toggle-hungry-state` 开关 hungry-delete mode

`M-x c-toggle-auto-hungry-state` 开关 auto-newline 和 hungry delete mode

#### subword mode ####

将 *NSGraohicsContext* 识别为 *NS*, *Graphics* 和 *Context* 三个部分

`C-c C-w` (`M-x subword-mode`) 开关 subword mode

#### syntactic-indentation mode ####

`C-j` 会自动缩进了

`M-x c-toggle-syntactic-indentation` 开关 syntactic-indentation mode

### 4.6 Electric Keys 和 关键字 ###

* `#` 会自动移动到行首
* `/*` 或者 `//` 触发自动缩进
* `<` 和 `>` C++
* `(` 和 `)` 触发自动缩进
* `{` 和 `}` 自动缩进, 如果开启了 auto-newline mode, 会自动换行
* `:` 触发自动缩进
* `;` 和 `,` 触发自动缩进, 如果开启了 auto-newline mode, 会自动换行

命令 `c-electric-continued-statement`

会自动对齐没有花括号的 `if`、`else` 之类的

### 4.7 自动换行 auto-newline ###

自动换行需要以下条件:

* auto-newline mode 使能
* 输入的字符是在行尾

有时候需要在 `}` 后面输入 `;`, 这个时候 hungry-deletion 可以快速删去额外的空白

### 4.8 Hungry Deletion of Whitespace ###

有两种方法可以使用

第一种方法是启用 *hungry delete mode* 的 `<DEL>` 和 `C-d`

第二种方法是使用快捷键

`C-c C-<DEL>` 或者 `C-c <DEL>` 删除光标之前的空白
`C-c C-d`、`C-c C-<DELETE>` 或者 `C-c <DELETE>` 删除光标之后的空白

### 4.9 Subword 移动和编辑 ###

使用 `C-c C-w` 来开关该模式

让它自动启用:

``` elisp
(add-hook 'c-mode-common-hook
          (lambda () (subword-mode 1)))
```

"GtkWindow" 的 Subword 是 "Gtk" 和 "Window"

* `M-f` *c-forward-word*
* `M-b` *c-backward-word*
* `M-@` *c-mark-subword* 标记subword
* `M-d` *c-kill-subword*
* `M-<DEL>` *c-backward-kill-subword*
* `M-t` *c-transpose-subwords* 把光标所在的词和前面的词换位置
* `M-c` *c-capitalize-subword* 首字母大写
* `M-u` *c-upcase-word* 大写
* `M-l` *c-downcase-word* 小写

可以在其他模式使用 `M-x subword-mode`

### 4.10 其他命令 ###

#### 设置 c style ####

`C-c .` *c-set-style*

`C-c . STYLE-NAME <RET>`

#### C++ 两个冒号 ####

`C-c :` 之后插入两个冒号

#### 区域结尾插入反斜杠 ####

`C-c C-\` 在结尾插入反斜杠

#### 展开宏 ####

`C-c C-e` *c-macro-expand*

## 5 字体锁定 Font Locking ##

可以让不同的语句使用特定的字体(例如注释)

### 5.1 字体锁定预备知识 ###

字体锁定有几个等级

1. **最小的等级**, 仅仅字符串、注释和预处理
2. **快速字体锁定**, 关键字、简单的类型、声明。 `*-font-lock-extra-types` 星表示语言, 这个变量用来识别类型; Javadoc 根据 `c-doc-comment-style` 来锁定字体
3. **精确字体锁定** 用户定义的类型也会被识别

因为用户定义的类型比较难识别, 可以通过正则表达式来识别

就是 *c-font-lock-extra-types*

### 5.2 外观 ###

* 普通的注释: *font-lock-comment-face*
* 文档注释: *font-lock-doc-face*
* 字符串和字符: *font-lock-string-face*
* 变量: *font-lock-variable-name-face*
* 内建常量: *font-lock-constant-face*
* 类型: *font-lock-type-face*
* 标签: *font-lock-constant-face*
* 预处理: *font-lock-preprocessor-face*
* 错误: *font-lock-warning-face*

### 5.3 文档注释 ###

*c-doc-comment-style*

默认的风格:

`((java-mode . javadoc) (pike-mode . autodoc) (c-mode . gtkdoc))`

如果在钩子里面改这个值, 就需要调用 *c-setup-doc-comment-style*

目前已有:

* javadoc
* autodoc: Pile
* gtkdoc: Gnome 社区
* doxygen: C C++ Java 等多种语言

可以自己写, 参考他们的源代码

### 5.4 标记错误注释风格 ###

这个不用管, 只使用一种注释的时候需要这个

例如只使用 `//` 或者 `/**/` 中的一种, 那么另一种会标记出来

### 5.5 杂项字体锁定 ###

C++的引用有关

### 5.6 AWK ###

跳过

## 6 基础配置 ##

``` elisp
;; 设置 c-basic-offset
(setq c-basic-offset 4)
```

不同语言用不同的 hook

``` elisp
(defun my-c-mode-hook ()
  (setq c-basic-offset 4))

(add-hook 'c-mode-hook 'my-c-mode-hook)

(defun my-java-mode-hook ()
  (setq c-basic-offset 6))

(add-hook 'java-mode-hook 'my-java-mode-hook)
```

风格

``` elisp
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "free-group-style")))
```

``` elisp
(defun my-c-mode-hook ()
  (c-set-style
    (if (and (buffer-file-name)
             (string-match "/usr/src/linux" (buffer-file-name)))
	    "linux"
        "free-group-style")))

(add-hook 'c-mode-hook 'my-c-mode-hook)
```

### 6.1 Hooks ###

* *c-initialization-hook* 仅在 CC Mode 初始化的时候运行
* *c-mode-common-hook* 所有语言的钩子
* *c-mode-hook* C语言的钩子
* *c++-mode-hook* C++
* *objc-mode-hook*
* *java-mode-hook*
* *idl-mode-hook*
* *pike-mode-hook*
* *awk-mode-hook*

### 6.2 风格变量 ###

风格变量默认是局部的, 可以通过修改 `c-style-variables-are-local-p` 为 nil 来把风格变量改为全局的

`c-offset-alist` 存放一系列的变量, 可以通过 `s-set-offset` 来添加变量

`c-special-indent-hook` 只能添加函数, 不能移除

风格变量如下

* *c-indent-comment-alist*, *c-indent-comments0syntactically-p*
* *c-doc-comment-style*
* *c-block-comment-prefix*, *c-comment-prefix-regexp*
* *c-hanging-braces-alist*
* *c-hanging-colons-alist*
* *c-hanging-semi&comma-criteria*
* *c-cleanup-list*
* *c-basic-list*
* *c-offsets-alist*
* *c-comment-only-line-offset*
* *c-special-indent-hook*, *c-label-minimum-indentation*
* *c-backslash-column*, *c-backslash-max-column*

### 6.3 风格 Style ###

#### 6.3.1 内置的风格 ####

内置的风格有:

* `gnu`: GNU 的 C语言
* `k&r`: Kernighan and Ritchie C语言
* `bsd`: Allman style
* `whitesmith`: C语言
* `stroustrup`: C++
* `ellemtel`: C++
* `linux`: Linux内核 C语言
* `python`: python扩展模块的 C语言标准
* `java`: Java
* `awk`: awk
* `user`: 由用户创建的 style

#### 6.3.2 选择一个风格 ####

当创建一个新buffer的时候, 它的风格会被设置为 *c-default-style*, 默认是 `gnu`

可以在 buffer 中使用 `C-c .` 来设置风格

1. 当 *c-default-style* 是一个字符串的时候, 它必须是一个存在的风格名称, 然后会应用于所有的mode
2. 当 *c-default-style* 是一个列表的时候, 会根据对应的模式寻找对应的字符串
3. 当这个列表里面没有找到的时候, 会用 other 中的风格
4. 如果 other 中没有找到, 就用 gnu

*c-default-style* 默认值是 `((java-mode . "java') (awk-mode . "awk") (other . "gnu"))`

#### 6.3.3 添加和修改一个风格 ####

略

#### 6.3.4 猜测一个风格 ####

可以猜测buffer的风格, 会生成一个 "guessed style"

之后可以用 `c-guess-install` 设置风格

可以用 `c-guess-view` 来预览任何buffer应用风格之后的代码

这些命令分析当前buffer的风格:

* `M-x c-guess-no-install`
* `M-x c-guess-buffer-no-install`
* `M-x c-guess-region-no-install`

这些命令分析当前buffer的一部分:

* `M-x c-guess`
* `M-x c-guess-buffer`
* `M-x c-guess-region`

用户选项 *c-guess-region-max*, 这个变量默认是 50000, 是 buffer 中会分析的字符个数, 如果设置为 nil, 整个 buffer 会被分析

用户选项 *c-guess-offset-threshold*, 这个变量默认是 10, 是最大的偏移量

把当前 buffer 的风格设置为猜测的风格:

* `M-x c-guess-install`

在一个临时的buffer中预览猜测的风格:

* `M-x c-guess-view`

#### 6.3.5 文件风格 ####

文件的局部变量可以设置文件风格

两个变量: `c-file-style` 和 `c-file-offsets`



# learn config emacs

## 1 简单自定义接口 ##

大部分的设置都是 *customizable variables*, 也叫 *user options*

使用 `M-x customize`

### 1.1 自定义组 ###

最上层的组叫 EMACS, 下面有很多的组, 比如 `[Editing]`, `[Convenience]` 等等

可以使用 `<TAB>`(widget-forward) 移动到下一个更改区域, `S-<TAB>`(widget-backward) 移动到上一个更改区域

### 1.2 浏览和搜索设置 ###

如果想直接去设置一个组, 使用命令 `M-x customize-option`, `M-x customize-face`, `M-x customize-group`

如果不知道名字, 可以在上边的搜索区域搜索, 可以搜索多个, 用空格分开, 或者使用正则表达式, 然后按回车

这个搜索只会搜索到已经加载的组

`M-x customize-apropos` 和搜索区域类似

`M-x customize-browse` 是另一个搜索命令, 它会把结果放到一个buffer中, 自带链接

### 1.3 更改一个变量 ###

有的是直接更改值, 有的是下拉菜单

如果想撤销更改, 有四种撤销:

1. Undo Edits: 取消更改, 更改了值但是还没有保存, 这个可以撤销
2. Revert This Session's Customizations: 回退当前会话的更改, 撤销到上一次保存的值
3. Erase Customization: 设置为默认值
4. Set to Backup Value: 设置为上一次丢弃的值

使用 Add Item 按钮可以添加注释

`C-c C-c`('Custom-set') 等同于 Set for Current Session 按钮

`C-x C-s`('Custom-save') 等同于 Save for Future Sessions 按钮

### 1.4 保存设置 ###

默认保存到 `.emacs` 文件, 可以添加如下代码更改

``` elisp
(setq custom-file "~/.config/emacs-custom.el")
(load custom-file)
```

对于不同的版本设置不同的配置:

``` elisp
(cond ((< emacs-major-version 28)
       ;; Emacs 27 customization.
       (setq custom-file "~/.config/custom-27.el"))
       ((and (= emacs-major-version 26)
             (< emacs-minor-version 3))
        ;; Emacs 26 customization, before version 26.3.
        (setq custom-file "~/.config/custom-26.el"))
        (t
         ;; Emacs version 28.1 or lator.
         (setq custom-file "~/.config/emacs-custom.el")))

(load custom-file)
```

### 1.5 自定义界面 faces ###

例如在编程语言模式, 源代码的注释通过 `font-lock-comment-face` 设置face

颜色可以通过 颜色的名字 或者 RGB三元组 `RRGGBB` 例如颜色名 *medium sea green* 是 `#3CB371`

不同类型的显示都可以设置, 点击 For All Kinds of Displays

### 1.6 自定义指定的item ###

* `M-x customize-option <RET> OPTION <RET>`
* `M-x customize-variable <RET> OPTION <RET>` 设置一个选项
* `M-x customize-face <RET> FACE <RET>` 设置 face
* `M-x customize-group <RET> GROUP <RET>` 设置一个组
* `M-x customize-apropos <RET> REGEXP <RET>` 搜索所有匹配正则表达式的选项
* `M-x customize-changed <RET> VERSION <RET>` 指定版本的emacs改过的所有的选项
* `M-x customize-saved` 保存过的设置
* `M-x customize-unsaved` 更改但是还没保存的设置

### 1.7 自定义主题  ###

一个主题是在一个文件中保存的, 名为 *NAME-theme.el*

使用 `M-x customize-themes` 列出所有已知的主题

emacs 默认会从两个路径搜索, 一个是变量 *custom-theme-directory*, 默认在 *~/.emacs.d/*,  另一个是 *etc/themes*, 在 *data-directory* 变量

如果想从别的路径找到主题, 就在 *custom-theme-load-path* 列表中添加, 默认值为 `(custom-theme-directory t)`, t 表示默认的 *etc/themes*

可以通过 `M-x load-theme` 加载一个指定的主题

如果已经加载过, 就可以使用 `M-x enable-theme` 来应用主题, 或者通过 `M-x disable-theme` 来禁用主题

使用 `M-x describe-theme` 来查看主题的描述

### 1.8 创建主题 ###

使用 `M-x customize-create-theme` 来创建一个主题

设置完之后会保存在 `custom-theme-directory` 中, 也就是 */etc/themes* 中以 *NAME-theme.el* 保存

在 `*Custom Theme*` buffer 中, 可以点击 `[Visit Theme]` 来读取一个theme

可以通过 `[Merge Theme]` 来把非主题的设置合并到一个自定义主题中

## 2 变量 ##

### 2.1 检查和设置变量 ###

可以通过 `C-h v`(describe-variable) 来查看一个变量的文档

更改变量: `M-x set-variable <RET> VAR <RET> VALUE <RET>`

也可以通过 `M-:` 然后输入 `(setq fill-column 75)`

### 2.2 Hooks ###

Emacs 中, 一个 hook 是一个 Lisp 变量, 带有一列表的函数

hook 是定制Emacs的重要机制。钩子是一个Lisp变量，它包含一个函数列表，这些函数将在某些定义良好的场合被调用。

例如, hook `kill-emacs-hook` 在退出emacs之前运行

大多数 hook 都是 "normal hooks" 普通的hook, 也就是依次执行函数, 不带参数。

所有名字中带 `-hook` 的 hook 都是普通hook

少数 hook 是 "abnormal hooks" 异常的钩子, 他们的名字以 `-functions` 结尾

例如, `find-file-not-fount-functions` 是一个异常钩子, 当其中一个函数返回 非nil 值时, 后面的函数就不执行了

可以通过 `setq` 来设置钩子, 但是建议使用 `add-hook`

大多数主要模式在初始化最后一步运行一个或多个钩子, 模式钩子自定义独立的模式很方便, 它们一直都是普通的钩子

例如, 这是一个钩子在 *text-mode* 打开 *auto-file-mode*

``` elisp
(add-hook 'text-mode-hook 'auto-fill-mode)
```

但是在 LaTeX模式中, 并不想打开 *auto-fill-mode*, 可以这样做:

``` elisp
(add-hook 'latex-mode-hook (lambda() (auto-fill-mode -1)))
```

使用 lambda 表达式来禁用 *auto-fill-mode*

这里有个更复杂的例子, 展示如何设置C语言的缩进:

``` elisp
(setq my-c-style
  '((c-comment-only-line-offset . 4)
    (c-cleanup-list . (scope-operator
                       empty-defun-braces
					   defun-close-semi))))
(add-hook 'c-mode-common-hook
  (lambda() (c-add-style "my-style" my-c-style t)))
```

可以通过 `remove-hook` 来移除钩子, 或者 `(setq HOOK-VARIABLE nil)`

### 2.3 局部变量 ###

`M-x make-local-variable <RET> VAR <RET>` 在当前 buffer 创建一个局部变量

`M-x kill-local-variable <RET> VAR <RET>` 在当前 buffer 使用全局的 VAR 值

`M-x make-variable-buffer-local <RET> VAR <RET>` 在当前buffer把一个全局变量标记为局部变量, 这种变量叫做 *per-buffer variables*, 在其他buffer相当于有了一个默认值

Major mode 主模式中的变量都是局部变量, 这样就不会影响别的buffer的该模式了

如果想设置全局变量, 就是用 `setq-default`

查看默认值

`(default-value 'fill-column)`

### 2.4 文件中的局部变量 ###

#### 2.4.1 设置文件中的局部变量 ####

由两种方式可以在文件中设置局部变量

第一种方式, 在文件的第一行添加下面这个

``` elisp
-*- mode:MODENAME; VAR: VALUE; ... -*-
```

也可以使用 `M-x add-file-local-variable-prop-line`

`M-x delete-file-local-variable-prop-line` 可以删掉

`M-x copy-dir-locals-to-file-locals-prop-line` 把当前目录的局部变量拷贝到文件第一行

这是一个例子:

``` elisp
;; -*- mode: Lisp; fill-column: 75; comment-column:50; -*-
```

如果要在 shell 脚本中指定, 那么可以在第二行写这个

写不下可以用 `\` 来换行

在文件的最后一页可以设置局部变量列表:

``` c
/* Local Variables:  */
/* mode: c           */
/* comment-column: 0 */
/* End:              */
```

也可以通过 `M-x add-file-local-variable` 来创建

下面是一些特殊的含义:

* *mode* 使能指定的主模式
* *eval* 运行指定的 Lisp 表达式, 返回值被忽略
* *coding* 指定编码格式
* *unibyte*

对于副模式, 想要关闭的话:

``` elisp
;; Local Variables:
;; eval: (eldoc-mode)
;; eval: (font-lock-mode -1)
;; end:
```

最好是使用 hook 而不是这种方式来关闭辅模式

使用 `M-x normal-mode` 来重置局部变量和主模式

#### 2.4.2 文件变量的安全性 ####

### 2.5 目录中的局部变量 ###

在一个目录中放一个 `.dir-locals.el`

如果是远程文件, 默认不搜索, 可以把 `enable-remote-dir-locals` 设置为 t

也可以使用 `.dir-locals-2.el`, 两个都会加载

nil 指的是所有mode

``` elisp
((nil . ((indent-tabs-mode . t)
	     (fill-column . 80)
         (mode . auto-fill)))
 (c-mode . ((c-file-style . "BSD")
            (subdirs . nil)))
 ("src/imported"  ;; subdirectory
  . ((nil . ((change-log-default-name
              . "ChangeLog.local"))))))
```

优先级:

``` elisp
((nil . ((fill-column . 40)))
 (c-mode . ((fill-column . 50)))
 (prog-mode . ((fill-column . 60)))
 ("narrow-files" . ((nil . ((fill-column . 20))))))
```

虽然 c-mode 是继承 prog-mode, 但是 c-mode 的 fill-column 依然是 50

在目录变量中, 可以设置 *mode*、*eval*、*unibyte*, 但是不能设置 *coding*

在目录变量中, 可以指定 *auto-mode-alist*, 设置主模式

``` elisp
((auto-mode-alist . (("\\.def\\'" . c-mode))))
```

可以使用 `M-x add-dir-local-variable` 来生成目录变量

`M-x delete-dor-local-variable` 删除一个实体

`M-x copy-file-locals-to-dor-locals` 把文件局部变量拷贝到目录变量中

另外一种方式就是定义一个 *directory-local* 变量, 使用 `dir-locals-set-directory-class` 函数在 *directory class* 中定义 变量/值

然后再用 `M-x dir-locals-set-directory-class` 函数

这个通常写在初始化文件中

``` elisp
(dir-locals-set-class-variables 'unwriteable-directory
   '((nil . ((some-useful-setting . value)))))

(dir-locals-set-directory-class
   "/usr/include" 'unwriteable-directory)
```

### 2.6 远程局部变量 ###

如果在使用shell, 本地是 bash, 但是远程是 ksh

可以使用 `connection-local-set-profile-variables` 函数

`connection-local-set-profiles` 可以应用profile

``` elisp
(connection-local-set-profile-variables 'remote-ksh
  '((shell-file-name . "/bin/ksh")
    (shell-command-switch . "-c")))

(connection-local-set-profile-variables 'remote-bash
  '((shell-file-name . "/bin/bash")
    (shell-command-switch . "-c")))

(connection-local-set-profiles
  '(:application tramp :machine "remotemachine")
  'remote-ksh)

```

这段代码定义了两个不同的 profile, 一个叫 *remote-ksh*, 另一个是 *remote-bash*

*remote-ksh* 会为匹配正则表达式 "remotemachine" 的目录应用该配置文件

也可以使用 `:protocol` 属性(在这里是tramp) 或者 `:user` 属性(远程用户名)

nil 会匹配所有的远程目录 buffers

## 3 自定义按键绑定 ##

按键绑定:

1. 把命令绑定到按键上
2. 把一系列按键绑定到 keymaps 上

用户用下面的按键是安全的:

* `C-c` 开头, 后面跟着一个字母
* `<F5>` 到 `<F9>`

### 3.1 Keymaps ###

global keymaps 全局按键绑定是一直生效的

主模式或者次要模式的按键会覆盖全局按键

使用 `C-h b` 查看当前按键的按键绑定

### 3.2 前缀按键 ###

菜单栏的前缀

鼠标的前缀

一些以名字出现:

* `ctrl-x-map` 表示 `C-x`
* `help-map` 表示 `C-h`
* `esc-map` 表示 `<ESC>`, 所有 meta字符都是通过它来定义的
* `ctl-x-4-map` 表示 `C-x 4`
* `mode-specific-map` 表示 `C-c`
* `project-prefix-map` 表示 `C-x p` 用于 project 相关的命令

### 3.3 局部按键绑定 ###

次要模式的按键会覆盖主模式和全局按键绑定

### 3.4 Minibuffer 按键绑定 ###

* `minibuffer-local-map` 用于 ordinary input (no completion)
* `minibuffer0local-ns-map` 类似, 只是 `<SPC>` 会像 `<RET>` 一样退出
* `minibuffer-local-completion-map` 用于 permissive completion 宽容完成
* `minibuffer-local-must-match-map` 用于 strict completion 严格完成 和 cautions completoin
* `minibuffer-local-filename-completion-map` 会补全文件名, 不绑定 `<SPC>`

默认情况下, `<TAB>`、`<SPC>`、和 `<?>` 会在 `minibuffer-local-completion-map` 自动补全

### 3.5 交互式地更改按键绑定 ###

影响当前会话的按键绑定

* `M-x global-set-key <RET> KEY CMD <RET>` 全局按键绑定
* `M-x local-set-key <RET> KEY CMD <RET>` 当前主模式的局部按键绑定
* `M-x global-unset-key <RET> KEY` 取消全局按键绑定
* `M-x local-unset-key <RET> KEY` 让按键在局部失效

例如, 把 `C-z` 绑定到 shell 模式, `C-z` 会替换全局的 `C-z`

``` elisp
M-x global-set-key <RET> C-z shell <RET>
```

如果直接输入 `C-f`, 那么按键输入就结束, 接着输入命令

如果输入的是前缀按键, 那么就一直等到不是前缀按键为止

``` elisp
M-x global-set-key <RET> C-x 4 $ spell-other-window <RET>
```

如果忘了之前的按键绑定是什么, 那就去一个新的 emacs 中的 Fundamental mode buffer 使用`C-h c`

### 3.6 在初始化文件中重新绑定按键 ###

有好几种方式, 最简单的就是 `kbd` 函数把字符串转换为按键, 然后作为按键传给 `global-set-key`

``` elisp
(global-set-key (kbd "C-z") 'shell)
```

`'shell` 中的单引号把它标记为一个 常量符号, 而不是一个变量

``` elisp
(global-set-key (kbd "C-c y") 'clipboard-yank)
(global-set-key (kbd "C-M-q") 'query-replace)
(global-set-key (kbd "<f5>") 'flyspell-mode)
(global-set-key (kbd "C-<f5>") 'display-line-number-mode)
(global-set-key (kbd "C-<right>") 'forward-sentence)
(global-set-key (kbd "<mouse-2>") 'mouse-save-then-kill)
```

还有另一种方式只用于 ascii 字符和一些转义字符, 例如下面这个表示 `C-x M-l`

``` elisp
(global-set-key "\C-x\M-l" 'make-symbolic-link)
```

转义字符有

* `\t` 表示 `<TAB>`
* `\r` 表示 `<RET>`
* `\e` 表示 `<ESC>`
* `\d` 表示 `<DEL>`

下面这个例子表示把 `C-x <TAB>` 绑定到 `idnent-rigidly`

``` elisp
(global-set-key "\C-x\t" 'indent-rigidly)
```

还有另一种方式, 使用向量 vector

``` elisp
(global-set-key [?\C-=] 'make-symbolic-link)
(global-set-key [?\M-\C-=] 'make-symbolic-link)
(global-set-key [?\H-a] 'make-symbolic-link)
(global-set-key [f7] 'make-symbolic-link)
(global-set-key [C-mouse-1] 'make-symbolic-link)
(global-set-key [?\C-z ?\M-l] 'make-symbolic-link)
```

Hook

下面是 texinfo-mode-hook 使用 hook 绑定局部按键的例子, `C-c n`, `C-c p` 和 `C-c C-x x`

``` elisp
(add-hook 'texinfo-mode-hook
          (lanbda ()
            (define-key texinfo-mode-map "\C-cp"
                        'backward-paragraph)
            (define-key texinfo-mode-map "\C-cn"
                        'forward-paragraph)
            (define-key texinfo-mode-map "\C-c\C-xx" nil)))
```

### 3.7 Modifier Keys ###

emacs 把 `C-A` 视为 `C-a`, 这种 alpha字母是大小写不敏感的, 但是对于 `C-@` 和 `C-2` 就是不一样的

但是可以绑定 ctrl-shift 按键

``` elisp
(global-set-key (kbd "C-S-n" #'previous-line))
```

有些按键没有, 比如 Hyper "H-"

可以通过 `C-x @ h` 前缀来代表 `H-`, `C-x @ s` 代表 super

`C-x @ h C-a` 表示 "Hyper-Control-a"

### 3.8 重新绑定函数按键 ###

有些名字对应函数symbol

光标方向键:

* left
* up
* right
* down

光标相关的按键

* begin
* end
* home
* next
* prior

Miscellaneous function keys 杂项功能键

* select
* print
* execute
* backtab
* insert
* undo
* redo
* clearline
* insertline
* deleteline
* insertchar
* deletechar

数字功能按键 Numbered function keys f1, f2 .. f35

数字小键盘按键 keypad keys

* kp-add
* kp-subtract
* kp-multiply
* kp-divide
* kp-backtab
* kp-space
* kp-tab
* kp-enter
* kp-separator
* kp-decimal
* kp-equal
* kp-prior
* kp-next
* kp-end
* kp-home
* kp-left
* kp-up
* kp-right
* kp-down
* kp-insert
* kp-delete

keypad keys with digits

* kp-0
* kp-1
* ...
* kp-9

keypad PF keys

* kp-f1
* kp-f2
* kp-f3
* kp-f4

keypad相关的按键可以通过 `keypad-setup`、`keypad-numlock-setup`、`keypad-shifted-setup`、`keypad-numlock-shifted-setup` 设置

### 3.9 有名字的 ASCII 控制字符 ###

`<TAB> <RET> <BS> <LFD> <ESC> <DEL>`

例如 `<TAB>` 是 `C-i` 的另一个名字

函数按键 *tab return backspace linefeed escape delete*

### 3.10 重新绑定鼠标按键 ###

鼠标点击 click event

鼠标拖动 drag event

基础的鼠标左键点击操作是 `mouse-1`

中键是 `mouse-2`

右键是 `mouse-3`

向上滚轮是 `wheel-up` 或 `mouse-4`

向下滚轮是 `wheel-down` 或 `mouse-5`

下面这个会把中键绑定到 `split-window-below`

``` elisp
(global-set-key [mouse-2] 'split-window-below)
```

拖动的事件需要在 mouse 之前加一个 `drag-` 前缀, 例如 `drag-mouse-1`

当一个鼠标按下的时候, 加一个 `down-` 前缀

双击的事件以 `double-` 作为前缀, 例如 `double-mouse-3` 双击右键

三击的事件以 `triple-` 作为前缀, 但是不识别四击

emacs 也可以识别 双击之后拖动 `double-drag-` 事件

变量 `double-click-time` 指定双击的间隔, 单位是 milliseconds 毫秒, 默认是 500 毫秒

变量 `double-click-fuzz` 指定双击的容差距离, 单位是像素, 字符宽的1/8, 默认是3

另外对于buffer区域外的地方, 例如按到 mode line, 那么就会有一个 `mode-line-` 前缀

下面这个例子表示点击 mode line 来运行 `scroll-up-command`

``` elisp
(global-set-key [mode-line mouse-1] 'scroll-up-command)
```

下面是这些特殊区域的前缀:

* `mode-line`
* `vertical-line` 垂直线
* `vertical-scroll-bar` 垂直的滚轴区域
* `menu-bar`
* `tab-bar`
* `tab-line`
* `header-line`

### 3.11 禁用命令 ###

禁用一个命令, 当这个命令被使用的时候, 会提示命令的名字、说明等信息, 然后提示是否执行, 在当前会话期间还是永久

例如禁用 `delete-region`

``` elisp
(put 'delete-region 'disabled t)
```

如果把 t 换成字符串, 到时候就会显示这个字符串

``` elisp
(put 'delete-region 'disabled
     "It's better to use `kill-region' instead.\n")
```

可以通过修改初始化文件或者使用 `M-x disable-command` 来禁用命令

## 4 Emacs 初始化文件 ##

Emacs 默认使用 *~/.emacs* 作为初始化文件, 当然也会使用 *~/.emacs.el*、*~/.emacs.d/init.el*、*~/.config/emacs/init.el* 或其他的地方

建议使用 *~/.emacs.d/init.el* 或者 XGD-compatible *~/.config/emacs/init.el*

可以使用 `-q` 来阻止加载初始化文件

可以使用 `-u` 或 `--user` 来指定不同用户的初始化文件

可以创建一个默认的初始化文件 *default.el*, 放在 library库中, 当启动 Emacs 的时候会加载它(除非使用 `-q`)

如果设置了 `inhibit-default-init` 为非nil, 就不会加载 default 设置

变量 `load-path` 指定 Emacs 搜索的文件夹, 很多网站把这些文件放在 Emacs 安装目录的 *site-lisp* 子文件夹中, 例如 */usr/local/share/emacs/site-lisp*

### 4.1 初始化文件的格式 ###

设置变量用 `setq`, 但是有些变量是局部变量

可以用 `setq-default` 来设置默认值

有些变量要通过自定义的那个菜单来设置

`setq` 的参数:

* 数字
* 字符串: 可以包含转义字符 `\n \t \b \r \\ \"`, `\f` 表示清屏(formfeed: Control-L), `\e` 表示 escape, `\000` 表示八进制代码是 000 的字符; `\C-` 可以用于控制字符的前缀, 例如 `\C-s` 表示 ASCII 中的 control-S, `\M-` 表示 Meta
* 字符: Lisp字符常量的语法, 问号加一个字符: `?x`, `?\n`, `?\"`, `?\)`。Lisp 中的字符是不能修改的
* 真: `t` 表示 True
* 假: `nil` 表示 False
* 其他 Lisp 对象: 单引号开头, `'` 后面加上想要的 Lisp对象

### 4.2 初始化文件的例子 ###

#### 4.2.1 `load-path` 添加路径 ####

把一个路径添加到变量 `load-path`, 然后使用 `M-x load-library`

``` elisp
(add-to-list 'load-path "/path/to/lisp/libraries")
```

#### 4.2.2 设置C模式缩进 ####

``` elisp
(setq c-tab-always-indent nil)
```

#### 4.2.3 搜索默认大小写敏感 ####

所有 buffer 启用

``` elisp
(setq-default case-fold-search nil)
```

#### 4.2.4 设置email ####

`(setq user-mail-address "cheney@torture.gov")`

#### 4.2.5 设置 new buffer 的默认主模式 ####

``` elisp
(setq default major-mode 'text-mode)
```

如果不加单引号, 就会被当成用另一个变量赋值一个变量, 而不是用对象赋值

#### 4.2.6 设置行号模式 ####

``` elisp
(line-number-mode t)
```

#### 4.2.7 自动填充模式hook ####

``` elisp
(add-hook 'text-mode-hook 'auto-fill-mode)
```

#### 4.2.8 加载一个 Lisp Library ####

加载 *foo.el* 或 *foo.elc*

``` elisp
(load "foo")

(load "~/.foo.elc")
```

#### 4.2.9 加载库中的函数 ####

加载 *mypackage.el* 或 *mypackage.elc* 中的 `myfunction`

``` elisp
(autoload 'myfunction "mypackage" "Do what I say." t)
```

如果包没有加载, 就会打印 *Do what I say.*

`t` 表示函数是可交互的

#### 4.2.10 重新绑定按键 ####

``` elisp
(global-set-key "\C-xl" 'make-symbolic-link)

(define-key global-map "\C-xl" 'make0symbolic-link)
```

仅在 *Lisp mode* 使用

``` elisp
(define-key lisp-mode-map "\C-xl" 'make-symbolic-link)
```

让一个按键失效:

``` elisp
(global-unset-key "\C-x\C-v")
```

#### 4.2.11 使 `$` 具有文本模式下的标点符号语法。 ####

``` elisp
(modify-syntax-entry ?\$ "." text-mode-syntax-table)
```

#### 4.2.12 启用一个按键, 无需询问 ####

``` elisp
(put 'narrow-to-region 'disabled nil)
```

#### 4.2.13 条件判断 ####

``` elisp
(if (fboundp 'blink-cursor-mode)
    (blink-cursor-mode 0))

(if (boundp 'coding-category-utf-8)
    (set-coding-priority '(coding-category-utf-8)))
```

#### 4.2.14 无视报错 ####

``` elisp
(ignore-errors (set-face-background 'region "grey75"))
```

### 4.3 指定终端的初始化 ###

对于终端类型 TERMTYPE, 对应的库叫做 *term/TERMTYPE*

### 4.4 Emacs 如何找到初始化文件 ###

会以这样的顺序: *~/.emacs.el*, *~/.emacs*, *~/.emacs.d/init.el*

### 4.5 初始化文件中的非 ASCII字符  ###

如果想用非ASCII码字符, 就在第一行写上

``` elisp
-*-coding:CODING-SYSTEM-*-
```

### 4.6 远程密码信息记录 ###

一些包可以登陆服务器, 如果不想一次次输入密码, 那就配置 *~/.authinfo* 或者 *~/.authinfo.gpg* 或者 *~/.netrc*

像这样

```
machine MYMACHINE login MYLOGINNAME password MYPASSWORD port MYPORT
```

可以通过配置 用户选项 *auth-sources*


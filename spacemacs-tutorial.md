# 学习 spacemacs #

## 1 Quick Start ##

### 1.1 Configuration layers ###

[Quick Start](https://www.spacemacs.org/doc/QUICK_START)

在 `~/.epacemacs` 配置 layers

一个 configuration layer 是一个目录, 它至少包含一个 `packages.el` 用于决定下载哪些包

如果有自己的 Emacs 配置, 可以放在自己的 layer 中

* 在 *private* 目录下创建一个 layer: `SPC : configuration-layer/create-layer RET`

创建的任何 configure layers 都必须显示地在 `~/.spacemacs` 中加载

注意:为了保护您的隐私，私有目录的内容不受源代码控制。请参阅文档中的私有 配置管理部分 private configuration management。

### 1.2 Dotfile (`.spacemacs`) ###

* 在家目录创建一个 `.spacemacs` 文件: `SPC : dotspacemacs/install RET`
* 打开 `.epacemacs`: `SPC f e d`
* 热加载配置文件 `SPC f e R`

使用变量 `dotspacemacs-configuration-layers` 设置加载哪些 layers:

``` Elisp
;; List of configuration layers to load.
dotspacemacs-configuration-layers '(auto-completion smex)
```

一些配置层支持配置变量，以暴露对特定于层的特性的粒度控制(granular control)，git层就是这样一个例子。变量可以直接在 `dotspacemacs-configuration-layers` 中设置，如下所示:

``` Elisp
;; List of configuration layers to load.
dotspacemacs-configuration-layers '(auto-completion
                                    (git :variables
                                         git-magit-status-fullscreen t)
                                    smex)
```

[dotfile template](https://www.spacemacs.org/core/templates/.spacemacs.template)

[documentation](https://www.spacemacs.org/doc/DOCUMENTATION.html)

### 1.3 Dotdirectory (`~/.spacemacs.d`) ###

和Emacs一样，Spacemacs的初始化也可以包含在 `init.el` 中。在一个特殊的目录 `~/.spacemacs.d` 中。然后应该在 `init.el` 中复制dotfile的内容。

Emacs dotfile或dotdirectory不会被替换，而是由Spacemacs dotfile或dotdirectory补充。在启动过程中，Emacs仍然使用 `~/.emacs.d/init.el`(或`~/.emacs`)用于初始化，变量 `user-emacs-directory` 仍将指向 `~/.emacs.d/`，即使 `~/.spacemacs.d` 或 `~/.spacemacs` 已经就位。`~/.emacs.d/init.el` 是由Spacemacs提供的(例如，将Spacemacs的git库克隆到空的 `~/.emacs.d/` 后)，您自己的个人配置将进入 `~/.spacemacs.d/init.el`(或`~/.spacemacs`)。

请查看FAQ，了解如何同时维护(以前的)vanilla Emacs和(新的)Spacemacs配置，而无需重命名和备份~/. Emacs .d/。

### 1.4 学习 spacemacs ###

#### 1.4.1 Editing Styles ####

在 `~/.spacemacs` 中

变量 `dotspacemacs-editing-style`:

* `vim`
* `emacs`
* `hybrid`

#### 1.4.2 The leader keys ####

leader key `SPC`:
* 在 `vim` 或 `hybrid` 模式下为 `SPC`
* 在 `emacs` 模式下为 `M-m`

变量:

* `vim` 模式下对应变量 `dotspacemacs-leader-key`
* `emacs` 模式下对应变量 `dotspacemacs-emacs-leader-key`

第二个 leader key 英文逗号 `,`, 是 `SPC m` 的快捷键

#### 1.4.3 Evil-tutor ####

* `EPC h T`: vim教学

#### 1.4.4 Universal argument ####

在 vim 模式下, emacs 的前缀不是 `C-u`, 而是 `SPC u`

#### 1.4.5 Configuration layers and Package discovery ####

* `SPC h SPC`: `helm-spacemacs-help`, 可以快速搜索一个包和使用它的 layer

#### 1.4.6 Key bindings discovery ####

多亏了which-key，无论何时按下前缀命令(如SPC)，都会在一秒钟后出现一个缓冲区，列出该前缀的可能键。

也可以通过按以下键来搜索特定的键绑定: `SPC ?`

要将绑定列表缩小到以SPC为前缀的绑定列表，请键入像下面这样的正则表达式: `SPC\ b`

它将列出所有与缓冲区相关的绑定。注意:您处于HELM-Descbind提示符，模式由6个字母组成:大写SPC，反斜杠，实际空格和小写b。

#### 1.4.7 Describe functions ####

| 按键绑定    | 描述              |
|:------------|:------------------|
| `SPC h d f` | describe-function |
| `SPC h d k` | describe-key      |
| `SPC h d m` | describe-mode     |
| `SPC h d v` | describe-variable |

#### 1.4.8 How-To's ####

在 [FAQ.org](https://www.spacemacs.org/doc/FAQ.html#MissingReference) 文件中编译了一些快速操作指南。

## 2 FAQ ##

[FAQ](https://www.spacemacs.org/doc/FAQ.html#MissingReference)

* 查看 spacemacs 版本: `SPC f e v`

如何添加额外包:

* 使用 `package-install` 的包会被删掉
* 使用变量 `dotspacemacs-additional-packages` 安装额外包
* 或创建 layer 然后把 layer 加到 `dotspacemacs-configuration-layers`
* 或把变量 `dotspacemacs-install-packages` 设置为 `used-but-keep-unused` 就可以防止spacemacs自动删除包

* 启动 emacs 加上 `--insecure` 参数可以强制使用 http, 或者把变量 `dotspacemacs-elpa-https` 设置为 nil

层配置代码将 read 和 act on 的任何变量都必须在 `user-init` 中设置，而Spacemacs显式设置但您希望覆盖的任何变量都必须在 `user-config` 中设置。

任何不只是设置变量的东西都应该99%在 `user-config` 中。

快速按下 `fd` 会退出 insert mode, 由包 `evil-escape` 控制, 不想要可以把 evil-escape 加到 `dotspacemacs-excluded-packages` 中

防止 Emacs 创建 `.#开头的文件`: `(setq create-lockfiles nil)`

提示知道当前缓冲按键主模式的名称: `SPC h d v major-mode RET`

在 python-mode 中禁止 company, 在 dotspacemacs/user-config 中添加: `(spacemacs|disable-company python-mode)`

启用可视行浏览:

``` Elisp
;; Make evil-mode up/down operate in screen lines instead of logical lines
(define-key evil-motion-state-map "j" 'evil-next-visual-line)
(define-key evil-motion-state-map "k" 'evil-previous-visual-line)
;; Also in visual mode
(define-key evil-visual-state-map "j" 'evil-next-visual-line)
(define-key evil-visual-state-map "k" 'evil-previous-visual-line)
```

**在一个模式中禁止 Evil**

让一个模式以 emacs 打开:

``` Elisp
(evil-set-initial-state 'magit-status-mode 'emacs)
```

或者使用正则:

``` Elisp
(push '("*magit" . emacs) evil-buffer-regexps)
```

但是如果想要使能 leaderkey:

``` Elisp
(with-eval-after-load 'magit
  (define-key magit-status-mode-map
    (kbd dotspacemacs-leader-key) spacemacs-default-map))
```

在词语移动时包含下划线, 在 `dotspacemacs/user-config` 中添加:

``` Elisp
;; For python
(add-hook 'python-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
;; For ruby
(add-hook 'ruby-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
;; For Javascript
(add-hook 'js2-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
```

为 leader key 创建别名

可以通过将首键映射绑定到另一个序列来更改首键。例如，如果你想把SPC S(拼写)和SPC d(破折号)切换，使前者更容易达到，你可以使用:

``` Elisp
(defun dear-leader/swap-keys (key1 key2)
  (let ((map1 (lookup-key spacemacs-default-map key1))
        (map2 (lookup-key spacemacs-default-map key2)))
    (spacemacs/set-leader-keys key1 map2 key2 map1)))
(dear-leader/swap-keys "S" "d")
```

如果你想定义你自己的别名，像使用 `SPC é` (因为它是一个没有使用的键在你的键盘布局，例如)访问 `SPC w` (窗口管理)，你可以使用这个:

``` Elisp
(defun dear-leader/alias-of (key1 key2)
  (let ((map (lookup-key spacemacs-default-map key2)))
    (spacemacs/set-leader-keys key1 map)))
(dear-leader/alias-of "é" "w")
```

使用Spacemacs作为git提交的`$EDITOR`:

在 `dotspacemacs/user-config` 中:

``` Elisp
(global-git-commit-mode t)
```

## 3 Spacemacs documentation ##

[Spacemacs documentation](https://develop.spacemacs.org/doc/DOCUMENTATION.html)

### 3.6 Configuration layers ###

可以扩展阅读下面这个连接:

[LAYERS](https://develop.spacemacs.org/doc/LAYERS.html)

每个 layer 有以下目录结构:

```
[layer_name]
  |__ [local]
  | |__ [package 1]
  | |     ...
  | |__ [package n]
  |-- layers.el
  |__ packages.el
  |__ funcs.el
  |__ config.el
  |__ keybindings.el

[] = directory
```

| File           | Usage                                                                                            |
|:---------------|:-------------------------------------------------------------------------------------------------|
| layers.el      | The place to declare additional layers                                                           |
| packages.el    | The list of packages and their configuration functions (init, post-init, etc…)                  |
| funcs.el       | All functions defined in the layer (used in package configuration for instance)                  |
| config.el      | Layer configuration (defines the layer variables default values and setup some config variables) |
| keybindings.el | General key bindings not tied to a specific package configuration                                |

packages 可以是:


* ELPA packages installed from an ELPA compliant repository
* local packages in a layer's local folder
* installed from an online source using quelpa.

#### 3.6.3 Configure packages ####

##### 3.6.3.1 With a layer #####

###### 3.6.3.1.1 Declaration ######

Packages 在变量 `<layer>-packages` 中声明, 其中 `<layer>` 就是 layer 的名称

例子:

``` Elisp
(setq <layer>-packages '(package1 package2 ...))
```

来自所有层的所有包都是按字母顺序处理的，因此有时您必须使用一些带有加载后评估的黑魔法来正确配置它们。例如，如果包A依赖于包B，那么你可以这样配置A:

``` Elisp
(with-eval-after-load 'B ...)
```

从 quelpa 或 local packages 见 [LAYERS](https://develop.spacemacs.org/doc/LAYERS.html#packagesel)

###### 3.6.3.1.2 初始化 ######

在 `packages.el` 初始化一个包 xxx 按照以下的函数:

``` Elisp
(defun <layer>/init-xxx () ...body )
```
通常使用 use-package

###### 3.6.3.1.3 Exclusion ######

可以在每层的基础上从Spacemacs中排除一些包。当配置层的目标是替换在Spacemacs层中声明的stock包时，这很有用。

为此，将要排除的包名添加到变量 `<layer>-excluded-packages` 中。

``` Elisp
(setq <layer>-excluded-packages '(package1 package2 ...)
```

###### 3.6.3.1.4 Overriding a layer package ######

[Overriding a layer package](https://develop.spacemacs.org/doc/FAQ.html#how-to-override-a-layer-package)

##### 3.6.3.2 Without a layer #####

有时，层可能是不必要的开销，如果您只想安装一个与它关联的配置很少的包，就会出现这种情况。一个很好的例子是一些你只对语法高亮显示感兴趣的小众语言。

您可以通过将这些包添加到 dotfile 中的 dotspacemacs/layers 函数下的 dotspacemacs-additional-packages 变量中来安装它们。

``` Elisp
(defun dotspacemacs/layers ()
  "Configuration Layers declaration..."
  (setq-default
   ;; ...
   dotspacemacs-additional-packages '(llvm-mode dts-mode)
   ;; ...
   ))
```

如果您想为它们添加一些配置，可以在dotspacemacs/user-config函数中这样做，或者考虑创建一个层。

分布式层(在layers目录中，这些层是由社区共享和上游合并的贡献)
private(在private目录中，它们被Git忽略)

这是一个简单的配置层，列出了一堆主题，你可以在这里找到。

要安装它，只需将themes-megapack添加到 `~/.spacemacs`

`(setq-default dotspacemacs-configuration-layers '(theme -megapack))`

添加这一层将安装大约100个主题;要卸载它们，请从dotspacemacs-configuration-layers中删除该层，然后按下 `SPC f e R`。

### 3.7 dotfile ###

* 测试 spacemacs文件 `SPC SPC dotspacemacs/test-dotfile `

#### 3.7.5 Dotfile Contents ####

##### 7.5.1 Configuration functions #####

`~/.spacemacs` 中的五个特殊函数可用于在spacemacs加载过程的开始和结束时执行配置:

* `dotspacemacs/init`: 在Spacemacs初始化启动时，在层配置之前调用。除了修改以 `spacemacs-` 为前缀的spacemacs变量值外，不应该在其中放入任何用户代码
* `dotspacemacs/user-init`: 在dotspacemacs/init之后，在层配置之前立即调用。这个函数主要用于需要在加载包之前设置的变量。
* `dotspacemacs/user-env`: 在层和包配置之前被调用，它负责设置环境变量
* `dotspacemacs/layers`: 在启动Spacemacs初始化时调用，这是您设置Spacemacs分发并声明要在配置中使用的层的地方。您还可以添加或排除您选择的包，并调整Spacemacs加载的一些行为
* `dotspacemacs/user-config`: 在Spacemacs初始化的最后，在层配置后调用。这是应该完成大多数配置的地方。除非显式指定在加载包之前应该设置变量，否则应该将代码放在这里
* `dotspacemacs/emacs-custom-settings`: 是一个生成的函数，其中写入所有Emacs自定义设置。你不应该编辑这个函数。有关更多信息，请参阅 自定义变量Custom variables 部分。

##### 7.5.2 Custom variables #####

默认情况下，`M-x customize-group` 中的自定义变量会自动保存在 `~/.spacemacs`的末尾。在dotspacemacs/emacs-custom-settings函数中，除非您在dotspacemacs/user-init函数中声明了一个自定义的自定义文件，在这种情况下，自定义变量将被写入用户定义的自定义文件，而函数dotspacemacs/emacs-custom-settings将保持空。

注意:如果您设置了自定义自定义文件值，那么您有责任在适当的时间加载该文件。加载它的最佳时间通常是在dotspacemacs/user-init函数中设置完custom-file的值之后

#### 3.7.6 Declaring Configuration layers ####

To use a configuration layer, declare it in your dotfile by adding it to the `dotspacemacs-configuration-layers` variable of your `~/.spacemacs`.

``` Elisp
(setq-default dotspacemacs-configuration-layers
              '(
                ;; other layers
                ;; rms layer added at the end of the list
                rms
                ))
```

不使用 `~/.emacs.d/private`, 而是指定目录:

``` Elisp
(setq-default dotspacemacs-configuration-layer-path '("~/.myconfig/"))
```

##### 3.7.6.1. Setting configuration layers variables #####

配置 layer 的变量:

dotspacemacs-configuration-layers

``` Elisp
(defun dotspacemacs/layers ()
  ;; List of configuration layers to load.
  (setq-default dotspacemacs-configuration-layers
    '(auto-completion
      (git :variables
           git-magit-status-fullscreen t
           git-variable-example nil)
      smex)))
```

`:variables` 关键字可以方便地将层配置与它们的声明保持在一起。在dotfile的dotspacemacs/user-init函数中设置层变量也是配置层的一种非常有效的方法。

##### 3.7.6.2 Disabling layer services in other layers #####

想要在 `org` 模式和 `git` 模式下禁用 `auto-completion`:

``` Elisp
(defun dotspacemacs/layers ()
  ;; List of configuration layers to load.
  (setq-default dotspacemacs-configuration-layers
    '(org git
      (auto-completion :disabled-for org git))))
```

也可以使用 `enabled-for` 仅仅为某些模式生效:

``` Elisp
(defun dotspacemacs/layers ()
  ;; List of configuration layers to load.
  (setq-default dotspacemacs-configuration-layers
    '(java python c-c++
      (auto-completion :enabled-for java python))))
```

`:enabled-for` 也可以是一个空列表

``` Elisp
(defun dotspacemacs/layers ()
  ;; List of configuration layers to load.
  (setq-default dotspacemacs-configuration-layers
    '(java python c-c++
      (auto-completion :enabled-for))))
```

`:enabled-for` takes precedence over `:disabled-for` if both are present. 

##### 3.7.6.3 Selecting/Ignoring packages of a layer #####

例如无视 `spacemacs-ui-visual` layer 中的 `treemacs` 和 `fancy-battery` 包:

``` Elisp
(defun dotspacemacs/layers ()
  ;; List of configuration layers to load.
  (setq-default dotspacemacs-configuration-layers
    '(auto-completion
      (spacemacs-ui-visual :packages (not treemacs fancy-battery))))
```

也可以只使能两个包:

``` Elisp
(defun dotspacemacs/layers ()
  ;; List of configuration layers to load.
  (setq-default dotspacemacs-configuration-layers
    '(auto-completion
      (spacemacs-ui-visual :packages treemacs fancy-battery)))
```

##### 3.7.6.4 Excluding packages #####

想要不安装某些包:

在变量 `dotspacemacs-excluded-packages`:

``` Elisp
(setq-default dotspacemacs-excluded-packages '(rainbow-delimiters))
```

### 3.8 Concepts ###

#### 3.8.1 Editing Styles ####

`dotspacemacs-editing-style `:

* `vim`
* `emacs`
* `hybrid`

##### 3.8.1.1 Vim #####

在 `insert state` 模式下 绑定按键:

``` Elisp
(define-key evil-insert-state-map (kbd "C-]") 'forward-char)
```

| 变量                                     | 描述                                                           |
|:-----------------------------------------|:---------------------------------------------------------------|
| `vim-style-visual-feedback`              | 如果非 nil, 对象会被高亮                                       |
| `vim-style-remap-Y-to-y$`                | 如果非 nil, 在 Evil states 下 `Y` 会被重新匹配到 `y$`          |
| `vim-style-retain-visual-state-on-shift` | 如果非 nil, shift mappings `<` 和  `>` 保持在 visual state     |
| `vim-style-visual-line-move-text`        | 如果非 nil, `J` 和 `K` 会在 visual mode 把行上下移动           |
| `vim-style-ex-substitute-global`         | 如果非 nil, 在 `:substitute` Evil ex-command 中逆转 `g` 的含义 |

默认配置如下:

``` Elisp
(setq-default dotspacemacs-editing-style '(vim :variables
                                               vim-style-visual-feedback nil
                                               vim-style-remap-Y-to-y$ nil
                                               vim-style-retain-visual-state-on-shift t
                                               vim-style-visual-line-move-text nil
                                               vim-style-ex-substitute-global nil)
```

##### 3.8.1.2 Emacs #####

Emacs中 leader key 是 `M-m`

可以通过 `SPC t E e` 和 `M-m t E e` 来切换 vim 和 emacs 模式

在 emacs 模式中绑定按键:

``` Elisp
(define-key evil-emacs-state-map (kbd "C-]") 'forward-char)
```

##### 3.8.1.3 Hybrid #####

将 Vim 的 insert state 替换成 hybrid, 也就是 Emacs 按键生效

让 hybrid 生效: `SPC t E h` 和 `M-m t E h`

在 `hybrid state` 中绑定按键:

``` Elisp
(define-key evil-hybrid-state-map (kbd "C-]") 'forward-char)
```

| 变量                                | 描述                                                                                 |
|:------------------------------------|:-------------------------------------------------------------------------------------|
| hybrid-style-visual-feedback        | If non-nil then objects are briefly highlighted                                      |
| hybrid-style-default-state          | The default state when opening a new buffer. Set it to emacs for a more emacsy style |
| hybrid-style-enable-hjkl-bindings   | If non-nil then packages will configure h j k l key bindings for navigation          |
| hybrid-style-enable-evilified-state | If non-nil buffer are evilified when supported, if nil then emacs state is enabled   |
| hybrid-style-use-evil-search-module | If non-nil then use evil own search module which is closer to Vim search behavior    |

默认的配置如下:

``` Elisp
(setq-default dotspacemacs-editing-style '(hybrid :variables
                                                  hybrid-style-visual-feedback nil
                                                  hybrid-style-enable-evilified-state t
                                                  hybrid-style-enable-hjkl-bindings nil
                                                  hybrid-style-use-evil-search-module nil
                                                  hybrid-style-default-state 'normal)
```

#### 3.8.2 状态 States ####

spacemacs有10种状态

| 状态         | 默认颜色    | 描述                             |
|:-------------|:------------|:---------------------------------|
| normal       | 橙色        | Vim 的 normal mode, 用于执行命令 |
| insert       | 绿色        | Vim 的 insert mode               |
| visual       | 灰色        | Vim 的 visual mode, 用于选择文本 |
| motion       | purple      | 用于浏览只读buffer               |
| emacs        | blue        | Emacs模式                        |
| replace      | chotolate   | Vim的replace mode                |
| hybrid       | 蓝色        | Emacs模式                        |
| evilified    | light brown | Emacs状态, 使用vim浏览选择搜索   |
| lisp         | pink        | 专用于 Lisp代码                  |
| iedit        | red         | 用于编辑多region                 |
| iedit-insert | red         | 用于替换多区域文本               |




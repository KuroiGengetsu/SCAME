# Emacs tutorial #

## 0 Emacs Tutorial ##

### 基础操作 ###

保存和退出:

* `C-x C-s`: 保存当前buffer
* `C-x C-c`: 退出emacs

翻页和居中:

* `C-v`: 往下翻页
* `M-v`: 往上翻页
* `C-l`: 按一下居中, 再按一下将这行变为第一行, 再按一下将这行变成最后一行

光标上下左右移动:

* `C-p` 上移, previous
* `C-n` 下移, next
* `C-b` 左移, backward
* `C-f` 右移, forward

跳到单词前后:

* `M-f` 移到词的末尾
* `M-b` 移到词的开始

行首行尾、句首句尾:

* `C-a` 移到行首
* `C-e` 移到行尾
* `M-a` 句首
* `M-e` 句尾

文件最开始、结尾:

* `M-<`: 文件开始
* `M->`: 文件结尾

指定命令重复次数:

* `C-u 8 command` 重复命令 8 次
* `C-u 15 a` 重复输入15个a: aaaaaaaaaaaaaaa

终止命令:

* `C-g` 终止命令

默认被禁用的命令:

* 有些命令被禁用了, 使用时会有告警信息, 例如 `C-x C-l`, 将 region 变成 lowercase 小写, 对应的, `C-x C-u` 是变成大写

窗格 Windows(窗格还是在本窗口, 窗口是 frame):

* `C-x 1` 只保留当前窗格
* `C-u 0 C-l` 当前行成为屏幕第一行
* `C-h k C-f`: 查看 `C-f` 的帮助说明

删除delete, 移除kill:

* `C-d` 相当于 `<DEL>` 键, 删除光标后边的字符
* `M-d` 删除光标后边的单词
* `C-k` 移除光标到行尾的字符
* `M-k` 移除光标到句尾的字符

Mark set 选中区域:

* `C-<SPC>` Ctrl + 空格可以开启选中模式
* `C-@` 同上
* 选中一块区域之后按下 `C-w` 就可以移除(kill)选中的区域

移除kill 和 删除delete 的区别:

* 被移除kill的东西可以被重新插入, 会被emacs记录下来, 重新插入被移除的文字叫做 **召回yank**, `C-y`
* 被删除的东西已经被emacs抛弃了

召回:

* `C-y` 可以多次召回上一次的移除
* `M-y` 可以召回以前的移除, 多按几下切换以前不同的移除, 是循环的

撤销 undo:

* `C-/` 可以撤销改变的内容, 但是对于没有更改任何内容的命令是无法撤销的
* 从键盘输入的字符以组为单位, 每组最多 20个字符
* `C-_`, `C-x u` 和 `C-/` 一样

### 文件 FILE ###

寻找一个文件:

* `C-x C-f` 寻找一个文件
* `C-x C-s` 保存文件
* 输入的文件名会出现在屏幕最底端的一行, 这一行被称为 **小缓冲minibuffer**
* 第一次保存的时候, emacs 会将文件重命名, 以 ~ 字符结尾
* 关掉备份可以通过 `M-x customize-variable <Return> make-backup-files <Return>` 来实现

缓冲区:

* emacs 把打开的文件都放在 **缓冲区 buffer**
* `C-x C-b` 列出缓冲区
* `C-x 1` 离开缓冲区列表 (将当前编辑的缓冲区放大)

切换缓冲区:

* `C-x b` 输入缓冲区的名字 可以切换缓冲区
* `*Message*` 存放的都是在 Emacs 底部出现的消息

保存多个缓冲区:

* `C-x s` 找出所有已被修改但尚未存盘的缓冲区, 然后逐个询问是否需要保存

### 命令集扩展 Extending the command set ###

扩展命令有两种风格:

1. `C-x` 字符扩展, `C-x` 之后输入另一个字符或者组合键
2. `M-x` 命令名扩展, `M-x` 之后输入一个命令

关闭emacs:

* `C-x C-c` 关闭 emacs, 同时询问未保存的文件是否需要保存
* `C-z` 可以暂时离开emacs, 通常的终端可以用 `fg` 命令或者 `%emacs` 命令再次回到emacs中

下面是已经学过的扩展命令, 如:

* `C-x C-f` 寻找文件
* `C-x C-s` 保存文件
* `C-x C-b` 列出缓冲区
* `C-x C-c` 离开emacs
* `C-x 1` 关掉其他所有窗格, 只保留一个
* `C-x u` 撤销

用命令名扩展的命令

例如 **字符串替换**:

* `replace-string` 命令可以把一个字符串替换成另一个, 从光标之后全部替换
* 字符串替换有两个参数,  被替换的字符串和用来替换它的字符串
* `M-x repl s<Return>changed<Return>altered<Return>`

自动保存:

* emacs 会把文件存在一个 `#hello.c#` 中, 当保存之后消失
* 如果不幸发生了, 打开原来的文件, 然后输入 `M-x recover file <Return>` , 输入yes来恢复自动保存的文件

回显区:

输入的命令速度很慢时, 会在下方提示已经输入的前缀

状态栏:

* 例如: `-:**- TUTORIAL.cn 63% L749 (Fundamental)`
* 最开头的 `*` 号表示已经对文字做过改动, 小括号中的内容表示当前正在使用的 **编辑模式**, 默认的模式是 Fundamental, 是一种主模式

主模式 Major mode:

* 任何情况下只能应用一个主模式
* 切换到 Fundamental: `M-x fundamental-mode`
* 切换到 text-mode: `M-x text-mode`
* 使用 `C-h m` 可以查看当前主模式的文档

辅模式 minor mode:

* 每个辅模式都可以单独关闭, 可以拥有多个辅模式
* 辅模式 auto fill mode: `M-x auto-fill-mode <Return>`, 再次输入命令可以关闭

行边界:

* 通常被设置为70字符, 可以使用 `C-u 70 C-x f` 来重新设置
* `M-q` 手动折行

### 搜索 searching ###

搜索:

* `C-s` 向前搜索
* `C-r` 向后搜索
* `C-g` 返回搜索之前的位置
* `<Return>` 留在搜索的位置
* 按 Backspace 会撤回到最近一次搜索的命中位置

### 多窗格 Multiple Windows ###

* `C-x 2` 将屏幕划分成两个窗格(下方)
* `C-x 3` 将屏幕划分成两个窗格(右方)
* `C-M-v` 可以向下滚动下方的窗格 `ESC C-v`
* `C-M-S-v` 可以向上滚动下方的窗格 `ESC C-S-v`
* `C-x o` o指的是other, 将光标移动到下方的窗格, 可以遍历所有窗格
* `C-x 1` 只保留当前窗格
* `C-x 4 C-f` 在新的窗格打开文件

### 多窗口 Multiple Frames ###

创建新窗口 frame

* `M-x make-frame <Return>` 创建新的窗口
* `M-x delete-frame <Return>` 删除选中的窗口

### 递归编辑 ###

* `M-%` 进行交互式替换, 此时 **不能** 通过 `C-g` 来关闭, 必须通过 `ESC ESC ESC` 来关闭

### 获得更多帮助 ###

* `C-h` 帮助
* `F1` 或者 `M-x help <Return>` 都可以
* `C-h ?` 可以查看能够获得什么帮助

查看组合键的帮助

* `C-h c` 然后再输入一个组合键可以查看 **简要** 说明
* `C-h k` 然后再输入一个组合键可以查看 **详细** 说明

解释一个函数

* `C-h f` 解释一个函数, 需要输入函数名

解释一个变量

* `C-h v` 后面是变量

相关命令搜索

* `C-h a`: command apropos 搜索命令

阅读手册 Info:

* `C-h i`: 会打开一个 `*info*` 缓冲区, 可以阅读系统软件包里的手册

emacs 的文档:

* `C-h i m emacs` 可以进入emacs的文档
* `C-h r` 也可以进入emacs的文档

两个有用的功能:

1. completion 自动补全
2. dired 目录编辑

## 主模式 info 的快捷键

### 在不同节点之间移动 ###

* `h`: invoke the info tutorial
* `q`: quit
* `mouse-2`: 鼠标左键: 点链接
* `RET`: 点链接
* `n`: 移动到下一个节点
* `p`: 移动到上一个节点
* `^`: 从当前节点 move "up"
* `m`: 选中菜单item通过名字
* `d`: 前往 info 目录节点
* `<`: 前往该文件的最顶节点
* `>`: 前往该文件最后一个节点
* `[`: 回到上一个节点
* `]`: 前往下一个节点
* `TAB`: 移动光标到下一个 cross-reference or menu item
* `C-M-i`: 移动光标到上一个 cross-reference or menu item
* `f`: 跟随一个 cross reference, reads name of reference
* `l`: 回到上一个访问的节点
* `r`: 前往按 `l` 之前的那个节点
* `L`: 前往访问过的节点菜单
* `T`: 前往当前 info 文件中的 内容列表

### 在一个节点中移动 ###

* `SPC`: 空格, 类似于 `C-v`, 但是到底之后可以前往下一个节点, 如果是最后一个节点, 返回父节点
* `DEL`: Backspace, 往上翻, 如果是顶部, 返回菜单节点
* `b`: 前往节点开头 beginning of node
* `e`: 前往节点的结尾 end of node

### 高级命令 ###

* `s`: 搜索
* `S`: 大小写敏感的搜索
* `C-s`, `C-M-s`: 在多个 info 节点搜索
* `i`: 搜索topic, 然后前往话题的index
* `，`: 逗号, 前往 i 搜索的下一个匹配点
* `I`: 搜索一个字符串, 并且展示结果的index节点
* `M-x info-apropos`: 在所有教程中搜索字符串
* `g`: 前往指定名字的节点
* `1 .. 9`: 1-9的数字, 选择节点菜单中前9个节点
* `c`: 将当前 info node 的名字放在 kill ring
* `M-n`: 在另一个窗格选择一个新的 cloned info buffer
* `C-u C-h i`: 移动到新的info文件
* `C-u 数字 C-h i` : 从 `*info*<N>` 中选择 info buffer

### 总结 ###

* 上一个节点: `p`, `[`
* 下一个节点: `n`, `]`
* 前往 info 目录: `d`
* 往下翻页: `SPC`
* 网上翻页: `DEL` backspace
* 目录: `T`

## 书签功能 bookmark ##

设置一个书签:

* `M-x bookmark-set` 或 `C-x r m`

列出保存的标签:

* `bookmark-bmenu-list` 或 `C-x r l`, 会打开一个书签的buffer: **Bookmark List**

在当前buffer, 有以下快捷键可以使用:

* `a` 显示当前书签的标注信息
* `A` 在另一个buffer中显示所有书签的所有信息
* `d` 标记书签, 以便用来删除 (`x` 执行删除)
* `e` 编辑当前书签的标注信息
* `m` 标记书签, 以便用于进一步显示和其他操作 (`v` 访问这个标签)
* `o` 选中当前书签, 并显示在另一个 window 中
* `C-o` 在另一个 window 中切换到当前这个书签
* `r` 重命名当前书签
* `w` 将当前书签的位置信息显示在 minibuffer 里

跳转到一个书签:

* 使用 `bookmark-jump` 函数, 可以跳转到一个特定的书签, 快捷键 `C-x r b`




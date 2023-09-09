# Markdown Mode #

## 如何安装 Markdown Mode

[markdown mode official site](https://jblevins.org/projects/markdown-mode/)

首先在自己的配置文件(Windows上是 `C:/User/username/AppData/Roaming/.emacs.d/.emacs`)中添加以下几行:

```elisp
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)
```

实际上就是将这个网站添加到 emacs 的源中, 然后通过菜单栏的 **Options->Manage Emacs Packages, 然后搜索 Markdown-mode, 安装即可**

### Markdown-Mode 的快捷键

#### `C-c C-l` markdown-insert-link

`C-c C-l` 是 link, 插入 url 用的, 先输入的是 url, 然后输入的是说明文字

例如: [百度](https://www.baidu.com/)

[B站](https://www.bilibili.com/ "哔哩哔哩")

#### `C-c C-i` markdown-insert-image

`C-c C-i` 是 image, 插入图片用的, 先输入的是 url, 然后输入的是说明, 例如:

![image](https://www.example.com/image.png)

设置图片的尺寸: `markdown-max-image-size`, 然后设置 `(max-width . max-height)`

#### `C-c C-s` Text Styles

`C-c C-s i`: 使一个区域变成斜体 *italic*, 或者插入两个星

`C-c C-s b`: 变粗体 **bold**

`C-c C-s c`: 是 `inline code` 代码

`C-c C-s k`: 插入 `<kbd></kbd>`

`C-c C-s q`: 插入 `>` blockquote:

> blockquote

`C-c C-s p`: 缩进

`C-c C-s C`: 多段代码, 需要输入语言

#### Headings

`C-c C-s h`: 自动标题

`C-c C-s H`: 类似自动标题

`C-c C-s 1` 到 `C-c C-s 6` 手动插入标题

`C-c C-s !` 插入这样的标题:

C-c C-s !
=========

`C-c C-s @` 插入这样的标题:

C-c C-s @
---------

`C-c C-k`: 删掉这行标题

#### `C-c C-s -` Horizontal Rules

相当于 `---`

#### `C-c C-s f` 插入 footnote 脚标

这个脚标比较蠢[^1]

#### `C-c C-s w` Wiki Links

[[WikiLink]]

### Markdown and Maintenance 命令 `C-c C-c` ###

`C-c C-c m`: 在当前buffer运行Markdown, 然后在另一个buffer显示HTML输出 *markdown-output* buffer

`C-c C-c p`: previews, 在浏览器预览

`C-c C-c e`: 保存到 **basename.html**, basename 就是 markdown文件的名字

`C-c C-c v` export and view 导出和预览

`C-c C-c o`: 打开 Markdown 源文件, 使用的是 `markdown-open-command`

`C-c C-c l`: *Live Export*: 打开 `markdown-live-preview-mode`, 也就是在 emacs 打开一个实时的 buffer 可以供预览, 再次输入快捷键可以关闭

通过设置 `markdown-split-window-direction` 可以设置 `*eww*` 打开的方向(底部或右边)

总结一下:

| 命令        | 说明                                            |
|:------------|:------------------------------------------------|
| `C-c C-c m` | `markdown-command` > `*markdown-output*` buffer |
| `C-c C-c p` | `markdown-command` > temporary file > browser   |
| `C-c C-c e` | `markdown-command` > `basename.html`            |
| `C-c C-c v` | `markdown-command` > `basename.html` > browser  |
| `C-c C-c w` | `markdown-command` > kill ring                  |
| `C-c C-c o` | `markdown-open-command`                         |
| `C-c C-c l` | `markdown-live-preview-mode` > `*eww*` buffer   |

#### `C-c C-o` Following Links 打开链接 ####

`C-c C-o` 打开一个链接, 可以使用 `M-p` 和 `M-n` 来切换上一个/下一个页面

#### `C-c C-d` Todo List ####

  * [ ] Unfinished
  * [x] Finished

#### `C-c C-` 和 `C-c C-=` Promotion and Demotion ####

`C-c C-` 就是 `C-c C--`, 可以将四级标题变成三级标题, 以此类推

`C-c C-=` 可以将三级标题变成四级标题

#### `C-c C-]` Completion ####

#### `M-RET`, `C-c Up`, `C-c DOWN`, `C-c LEFT`, `C-c RIGHT` Editing Lists ####

创建列表 `M-RET` 或者 `C-c C-j`:

* 第一行 `C-c C-j` 创建同级列表
* 第二行 `C-u C-u C-c C-j` 创建下一级列表
  * 第三行, `C-u C-c C-j` 创建上一级列表
* 第四行

#### `C-c C-k` Killing Elements ####

删除当前的region 元素, 例如在下面用会删掉其中一个元素

*adwd* **awd**

#### `C-c C-n`, `C-c C-p`, `C-c C-f`, `C-c C-b`, `C-c C-u` 大纲浏览 Outline Navigation ####

* `C-c C-n`: 下一个标题
* `C-c C-p`: 上一个标题
* `C-c C-f`: 下一个同级标题
* `C-c C-b`: 上一个同级标题
* `C-c C-u`: 移动到父级标题

#### `M-{`, `M-}`, `M-h` 在 markdown 段落移动 ####

* `M-{` 即 `M-S-[`, 回到上一段落, `markdown-forward-paragraph`
* `M-}` 即 `M-S-]`, 前往下一段落, `markdown-backward-paragraph`
* `M-h` 标记一个段落, `markdown-mark-block`

#### `C-M-a`, `C-M-e`, `C-M-h` Movement by defuns ####

#### 编辑表格 ####

创建表格: `C-c C-s t`, `markdown-insert-table`

Markdown 的表格:

先写好第一行, 然后再第二行输入 `|-`, 然后按下 `TAB` 键, 就会自动补全

```
|Right|Left|Center|Default|
|-<TAB>
```

第二行可以通过 `|:-`, `|-`, `|-:` 来设置对齐方式

```
|Left|Right|Center|Default|
|:- |-: |:-: |-<TAB>
```

| Left   | Right  | Center   | Default |
| 左对齐 | 右对齐 | 中间对齐 | 默认    |
| 企鹅   | 猪     | 猫       | 狗      |

在表格中通过 `TAB` 来调到下一个单元格, 通过 `S-TAB` 跳到上一个单元格, `RET` 会移动到下放的单元格

以下是一些表格操作:

* `C-c UP`, `C-c DOWN`: 当前行上移/下移
* `C-c LEFT`, `C-c RIGHT`: 当前行左移/右移
* `C-c S-UP`: 删除当前行
* `C-c S-LEFT`: 删除当前列
* `C-c S-RIGHT`: 左边插入新行
* `C-c C-d` Re-align the current table (`markdown-do`)
* `C-c C-c ^`: 根据指定的某列 排序行
* `C-c C-c |`: 将当前 region 转换为表格
* `C-c C-c t`: 转置

#### 只读浏览模式 Viewing Mode ####

Read-only viewing modes: `markdown-view-mode` 和 `gfm-view-mode`

#### Miscellaneous Commands 各种各样的命令 ####

当安装了 **edit-indirect** 包时, `C-c '` (markdown-edit-code-block) 可以用来编辑 Code block, 按下 `C-c C-c` 来 commit changes, 按下 `C-c C-k` 取消编辑

可以使用 `C-u C-c C-s C` 来编辑 code block

``` C
#include <stdio.h>

int main(void)
{
    return 0;
}
```

### GitHub Flavored Markdown (GFM) ###

支持一些GitHub的操作

[^1]: 脚标1


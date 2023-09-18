# Learning Elisp #

`C-u` 被称为是 unisersal argument, 它会传递一个参数给子命令

## 1. List Processing ##

LISP 代表 **LISt Processing**

圆括号: parentheses: `()`
单引号: single-quote: `'`

LISP的基础就是 list列表

### 1.1 Lisp Lists ###

在 Lisp 中, 一个列表如下:

``` elisp
'(rose
  violet
  daisy
  buttercup)
```

#### Numbers, Lists inside of Lists ####

`(+ 2 2)`

数据和程序以相同的方式呈现, 即: 他们都是 单词、数字、其他列表的 列表

``` elisp
'(this list has (a list inside of it))
```

#### 1.1.1 List Atoms ####

在 Lisp 中, 把所有的单词都叫做 **atoms**

因为 word atom 意思是 **indivisible** 不可分的

每个 atoms 都是由 whitespace 分开的, 通常就是空格、换行

空列表: `()` **empty list**, 空列表同时表示一个 atom 和 一个 list

atom 和 list 都被叫做 **符号表达式** *symbolic expressions* 或 简称为 *s-expressions*

被双引号括起来的算是一个 atom:

``` elisp
'(this list includes "text between quotation marks.")
```

#### 1.1.2 Whitespace in Lists ####

一个列表中的空格数量是无所谓的:

``` elisp
'(this list
  looks like this)
```

等同于:

``` elisp
'(this list looks like this)
```

额外的空格和换行都是为了阅读方便

#### 1.1.3 在 GNU Emacs 中输入列表 ####

两个模式: Lisp Interaction mode 和 Emacs Lisp mode

使用 `M-C-\` 来缩进一个区域

### 1.2 Run a Program ###

1. 如果一个列表以单引号 `'` 开头的时候, 它告诉 Lisp 什么也不要做, 而是返回该列表
2. 如果一个列表没有以单引号开头, 那么列表中第一个元素是一个命令(函数)

在emacs中, 把光标放到 `(+ 2 2)` 这个列表后括号的后面, 然后执行 `C-x C-e`, 那么就会在 **回显区域 echo area** 返回 4

执行 `'(this is a quoted list)`: 可以在底部看到 `'(this is a quoted list)`

`C-x C-e` 是将这个表达式传给 Emacs 的 **Lisp interpreter**

### 1.3 Generate an Error Message ###

error message 是由 GNU Emacs debugger 生成的

在 GNU Emacs debugger 下, 按 `q` 退出

执行下面这句话:

``` elisp
(this is an unquoted list)
```

会在 `*Backtrace*` buffer中报下面的错误:

```
Debugger entered--Lisp error: (void-function this)
  (this is an unquoted list)
  elisp--eval-last-sexp(nil)
  eval-last-sexp(nil)
  funcall-interactively(eval-last-sexp nil)
  command-execute(eval-last-sexp)
```

解释:

* 当按下 `C-x C-e` 时, 调用了 `eval-last-sexp` 这个命令, 其中 `eval` 是 *evaluate* 的缩写, `sexp` 是 *symbolic exporession* 的缩写, 所以这个命令的意思就是 *evaluate last symbolic expression*, 也就是执行前面的符号表达式
* 第一行 *Debugger entered--Lisp error: (void-function this)* 表明 Lisp解释器 尝试执行列表中的第一个 atom: `this`, 然而 `this` 这个函数并没有定义

### 1.4 Symbol Names and Function Definitions ###

列表中的第一个 atom, 作为 symbol 是用来寻找计算机中的函数

### 1.5 The Lisp Interpreter ###

Lisp解释器的工作原理实际上就是:

1. 先判断有没有以单引号 `'` 开头, 如果以单引号开头, 就返回该列表
2. 如果没有单引号, 它会把第一个 atom 也就是 symbol 当作命令, 然后区寻找同名的函数, 如果找到了就执行, 如果没找到, 就报错

#### Complications 并发 ####

除了基本的形式, Lisp 还有其他形式:

1. 变量 Variables
2. 特殊形式 special forms
3. 宏 macros

例如, `if` 就是特殊形式, 而 `when` 是一个宏

在emacs老版本, `defun` 是一个特殊形式, 但现在是一个宏, 虽然表现一样

当Lisp遇到列表里面还有列表, 那么它会先去计算里面的列表, 然后再计算外层的

#### 1.5.1 Byte compiling ####

Lisp解释器可以解释两种实体 entity:

1. 人类能读懂的代码 hymanly readable code
2. specially processed code, 被称为 *byte compiling code*, 人类是无法阅读的, 编译后的代码

*byte compiling code* 运行得更快

可以通过 `byte-compiled-file` 这个命令来编译代码, 将 `.el` 文件编译之后, 会产生以 `.elc` 结尾的文件, 就是由 *byte compiling code* 组成的

### 1.6 Evaluation 评估 ###

当Lisp解释器计算一个表达式的时候, 这个过程叫做 评估:

* The interpreter evaluates the expression

评估的意思就是确定 XXX 的价值/数量

#### How the Lisp Interpreter Acts ####

计算完表达式之后, Lisp 会返回结果, 同时还会做一些其他事, 这个其他事就被成为 side effect, 副作用, 例如移动光标等

#### 1.6.1 Evaluating Inner Lists ####

``` elisp
(+ 2 (+ 3 3))
```

会返回8

### 1.7 Variables 变量 ###

Lisp 中的函数和变量可以重名

#### `fill-column` 变量 ####

变量 `fill-column` 

``` elisp
fill-column
```

`C-x C-e` 执行之后结果是 70

我们可以 bind 绑定 一个值到一个变量, 甚至可以绑定一个变量到一个函数

#### 1.7.1 Error Message for a Symbol Without a Function ####

如果我们执行下面这行:

``` elisp
(fill-column)
```

同样会得到一个 void-function 的报错, 因为Lisp会把fill-common当作一个函数

#### 1.7.2 Error Message for a Symbol Without a Value ####

把光标放到 + 号后面的空格, 然后 `C-x C-e`, 会得到报错

``` elisp
(+ 2 2)
```

报错:

```
Debugger entered--Lisp error: (void-variable +)
  elisp--eval-last-sexp(nil)
  eval-last-sexp(nil)
  funcall-interactively(eval-last-sexp nil)
  command-execute(eval-last-sexp)
```

因为 `+` 不是一个变量

### 1.8 Arguments 参数 ###

``` elisp
(+ 2 2)
```

两个 2 就是参数

#### 1.8.1 Arguments' Data Types 参数的数据类型 ####

例如 `concat` 函数可以把两个字符串拼接起来

``` elisp
(concat "abc" "def")
```

会得到 "abcdef"

例如 `substring` 函数把一个字符串和两个数字当作参数, 第一个参数是字符串, 第二、三个参数分别是子字符串的起始和结束位置

``` elisp
(substring "The quick brown fox jumped." 16 19)
```

会返回 "fox"

#### 1.8.2 An Argument as the Value of a Variable or List 参数作为变量或列表的值 ####

``` elisp
(+ 2 fill-column)
```

将 `fill-column` 变量作为参数

``` elisp
(concat "The " (number-to-string (+ 2 fill-column)) " red foxes.")
```

结果是 "The 72 red foxes."

函数 `number-to-string` 可以把数字转换为字符串, 相当于 `int-to-string`

#### 1.8.3 Variable Number of Arguments 参数的变量数量 ####

`+` `*` 等函数接收任意数量的参数

``` elisp
; 结果是0
(+)

; 结果是1
(*)

; 结果是3
(+ 3)

; 结果是3
(* 3)

; 结果是12
(+ 3 4 5)

; 结果是60
(* 3 4 5)
```

#### 1.8.4 Using the Wrong Type Object as an Argument 使用错误的对象类型作为参数 ####

`'hello` 的值就是 hello

``` elisp
(+ 2 'hello)
'hello
```

会得到报错:

```
Debugger entered--Lisp error: (wrong-type-argument number-or-marker-p hello)
  +(2 hello)
  elisp--eval-last-sexp(nil)
  eval-last-sexp(nil)
  funcall-interactively(eval-last-sexp nil)
  command-execute(eval-last-sexp)
```

*number-or-marker-p* 解释器遇到未预料的参数时, 在判断是否是一个 marker, 在 Emacs 中, buffer 中的位置被记录为 marker, 当使用 `C-@` 或者 `C-<SPC>` 命令标记 set 时, 它的位置被标记为 marker

“number-or-marker-p”中的“p”是Lisp编程早期开始的一种实践的体现。“p”代表“谓词”。在早期Lisp研究人员使用的术语中，谓词指的是一个函数，用于确定某些属性是真还是假。所以" p "告诉我们" number-or-marker-p "是一个函数的名字，它决定提供的参数是数字还是标记是真还是假。其他以' p '结尾的Lisp符号包括' zerop '，一个测试其实参是否为0的函数，以及' listp '，一个测试其实参是否为列表的函数。

#### 1.8.5 The 'message' function ####

message 函数就是将参数打印到 echo area

``` elisp
(message "This message appears in the echo area!")

(message "Hello World!\n")

(message "%s\n" "Hello World!")

(message "The name of this buffer is %s." (buffer-name))

(message "The value of fill-column is %d." fill-column)

(message "There are %d %s in the office!"
         (- fill-column 14) "pink elephants")
```

函数 `buffer-name` 返回当前 buffer 的名字

``` elisp
(message "He saw %d %s"
	     (- fill-column 32)
		 (concat "red "
		         (substring
				  "The quick brown foxes jumped." 16 21)
				  " leaping."))
```

(message "%s" 1511111)

### 1.9 给变量赋值 ###

使用函数 `set` 或 特殊形式 `setq`, 还有一种方式是用 `let`

专业术语: bind a variable to a value

#### 1.9.1 使用 `set` ####

给变量 `flowers` 赋值为列表 `'(rose violet daisy buttercup)`

``` elisp
(set 'flowers '(rose violet daisy buttercup))

flowers
```

执行完之后, `(rose violet daisy buttercup` 会出现在回显区域

这是由 `set` 函数返回的值

每个 Lisp 函数必须有返回值, 除非报错

然后执行 `flowers` 就会出现刚才的列表

如果执行下面这行:

``` elisp
'flowers
```

则会出现 `flowers`

当使用 set 的时候, 变量和列表都必须用 `'` 前缀, 否则就会报错:

``` elisp
(set flowers '(rose violet daisy buttercup))
```

```
Debugger entered--Lisp error: (wrong-type-argument symbolp (rose violet daisy buttercup))
  set((rose violet daisy buttercup) (rose violet daisy buttercup))
  elisp--eval-last-sexp(nil)
  eval-last-sexp(nil)
  funcall-interactively(eval-last-sexp nil)
  command-execute(eval-last-sexp)
```

#### 1.9.2 Using `setq` ####

`setq` 表示 `set 'variable`, 也就是 set 和 quote 的组合

``` elisp
(setq carnivores '(lion tiger leopard))

(set 'carnivores '(lion tiger leopard))
```

两者是等效的

同时, `setq` 可以同时给多个变量赋值:

``` elisp
(setq trees '(pine fir oak maple)
      herbivores '(gazelle antelope zebra))
```

会返回 (gazelle antelope zebra)

实际上这里说的赋值更像是将变量 指向 列表, 类似指针 pointer

#### 1.9.3 Counting 计数 ####

``` elisp
(setq counter 0)              ; 计数器初始化
(setq counter (+ counter 1))  ; 自增
counter                       ; 计数器的值
```

### 1.10 总结 ###

总结:

* Lisp 程序由表达式组成, 也就是列表或单个atoms
* 列表由0个或多个atoms或者另一个列表组成, 由空格分开, 列表可以为空
* atoms 是多字符的符号 symbols, 例如 `forward-paragraph`, 单字符的符号例如 `+`, 字符串是由 "" 括起来的, 当然还有数字
* 一个数字计算(评估evaluate)为它自己
* 一个字符串计算为它自己
* 当 evaluate 一个符号本身, 会返回它自己的值
* 当 evaluate 一个列表, 解释器会把第一个符号当作函数, 然后找定义
* 单引号 `'` 告诉解释器, 应当按原样返回表达式
* 参数是传给函数的信息, 除了列表中第一个符号以外的都是参数
* 函数永远都会返回值(除非报错), 并且除了返回值, 还会有 side effect, 大部分时候, 一个函数就是为了创建一个 side effect

### 1.11 练习 ###

``` elisp
(setq counter 0)
(setq counter (+ counter 2))
counter

(message "The counter now is %d." counter)
```

## 2. Practicing Evaluation ##

### 2.1 Buffer Names ###

``` elisp
(buffer-name)       ; elisp_learn.md
(buffer-file-name)  ; ~/MyGitHub/SCAME/elisp_learn.md
```

`nil` 在 emacs 中指的是 `()`

`C-u C-x C-e` 会将结果放到光标的位置

``` elisp
(buffer-name)"elisp_learn.md"
(buffer-file-name)"d:/Coding/MyGitHub/SCAME/elisp_learn.md"
```

### 2.2 Getting Buffers ###

``` elisp
(current-buffer)  ;; #<buffer elisp_learn.md> 
```

表示 buffer 本身被返回, 不仅仅是名字

``` elisp
(other-buffer)  ;; #<buffer *scratch*>
```

(other-buffer) 返回的是最近访问的其他buffer

### 2.3 Switching Buffers ###

`switch-to-buffer` function

快捷键都是调用函数, 例如 `C-f` 调用 `forward-char`, `M-e` 调用的是 `forward-sentence`

``` elisp
(switch-to-buffer (other-buffer))  ;; 等同于 C-x b <RET>
```

`set-buffer`

``` elisp
(switch-to-buffer (other-buffer (current-buffer) t))
```

### 2.4 Buffer Size and the Location of Point ###

当前 buffer 的尺寸

``` elisp
(buffer-size)  ;; 8775
```

在 emacs 中, 光标当前的位置叫做 `point`

``` elisp
(point)  ;; 8847
```

``` elisp
(point-min)  ;; 1
```

``` elisp
(point-max)  ;; 8922 当前 buffer point 最大的值
```

## 3 How To Write Function Definitions ##

### An Aside about Primitive Functions ###

有些函数是用C写的, 有些是用 elisp 写的, 但是对我们并不重要, 该怎么写就怎么写

### 3.1 The `defun` Macro ###

`defun` 宏

`mark-whole-buffer`: `C-x h`

一个函数定义在 `defun` 之后由五个部分组成:

1. 函数名
2. 参数, 如果没参数就是 `()`
3. 函数的文档
4. 可选, 是否是可交互的, 也就是在 M-x 或者 快捷键可以使用
5. 函数体, 就是具体的要实现什么功能的代码

``` elisp
(defun FUNCTION-NAME (ARGUMENTS...)
  "OPTIONAL-DOCUMENTATION..."
  (interactive ARGUMENT-PASSING-INFO)  ; optional
   BODY...)
```

例子: 定义一个函数, 将一个数乘7:

``` elisp
(defun multiply-by-seven (number)
       "Multiply NUMBER by seven."
       (* 7 number))
(multiply-by-seven 3)
```

### 3.2 Install a Function Definition ###

运行函数的定义就会 install 这个函数, 会返回函数的名字

永久性 installing  code 的方法: **Installing Code Permanently**章节

#### The effect of installation ####

使用 `C-h f 函数名`  可以查看一个函数的文档

#### 3.2.1 Change a Function Definition ####

想要更改函数的定义, 直接更改然后再运行函数的定义就可以

``` elisp
(defun multiply-by-seven (number)        ; second version
  "Multiply NUMBER by seven."
  (+ number number number number number number))
```

### 3.3 Makefa Function Interactive ###

要将一个函数变得可交互, 需要在函数说明文档的后面加上以特殊形式 `interactive` 开头的列表

当调用一个可交互的函数时, 并不会把返回值显示在回显区

#### An Interactive `multiply-by-seven` ####

``` elisp
(defun multiply-by-seven (number)     ; Interactive version
  "Multiply NUMBER by seven."
  (interactive "p")
  (message "The result is %d" (* 7 number)))
```

然后按下 `C-u 7 M-x multiply-by-seven <RET>`

就会显示 "The result is 49"

一个 "prefix argument" 传给交互式函数, 通过按下 `M-3 M-e` 或者 `C-u 3 M-e`

`C-u` 默认是4

交互式的那一行 `interactive "p"`, "p" 告诉 emacs 传递一个前缀参数给这个函数, 然后把这个值用作函数的参数

### 3.4 Different Options for 'interactive' ###

interactive 有超过20种参数, 见 **Code Characters for interactive(elisp)**

有一个函数叫 `zap-to-char`, 它的交互式表达式是:

``` elisp
(interactive "p\ncZap to char: ")
```

* 第一个参数是 p, 也就是前缀
* c 告诉函数要删除的名字

``` elisp
(defun NAME-OF-FUNCTION (arg char)
  "DOCUMENTATION..."
  (interactive "p\ncZap to char: ")
  BODY-OF-FUNCTION...)
```

给函数参数列表的多个参数设置交互, 在字符串中用 `\n` 分开, 按同样的顺序

有些函数虽然是交互模式, 但是并不需要任何参数, 例如 `mark-whole-buffer` 的 interactive 就是这样:

`(interactive)`

### 3.5 Install Code Permanently ###

永久安装函数的方法:

1. 如果是自己的代码, 那么可以把它放在 `.emacs` 文件中
2. 可以把函数写在一个或多个文件中, 然后使用命令 `load` 来加载它们, 见 Loading Files.
3. 如果你的代码要放在整个网站上, 那么就把它放在 `site-init.el` 这个文件中, 这样所有人都可以使用

### `let` ###

`let` 表达式是 Lisp 中的特殊形式, 用于创建函数中的局部变量, 不会和 emacs 中的变量冲突, 有效期至 let 表达式结束

let 表达式由三部分组成:

``` elisp
(let VARLIST BODY...)
```

在 VARLIST 中: 符号会被初始化为 nil

例如: `(thread (needles 3))` 中 thread 会被初始化为 nil, needles 会被初始化为 3

let 的模板像这样:

``` elisp
(let ((VARIABLE VALUE)
      (VARIABLE VALUE)
       ...)
   BODY...)
```

#### 3.6.2 Sample `let` Expression ####

``` elisp
(let ((zebra "stripes")
      (tiger "fierce"))
 (message "One kind of animal has %s and another is %s."
          zebra tiger))
```

结果是 "One kind of animal has stripes and another is fierce."

#### 3.6.3 Uninitialized Variables in a `let` 表达式中未初始化的变量 ####

``` elisp
(let ((birch 3)
       pine
       fir
       (oak 'some))
     (message
	  "Here are %d variables with %s, %s, and %s value."
      birch pine fir oak))
```

结果是: "Here are 3 variables with nil, nil, and some value."

### 3.7 The 'if' Special Form ###

`beginning_of_buffer` 函数中用到了 if

if 表达式的测试部分叫做 if-part, 第二个参数叫做 then-part

``` elisp
(if TRUE-OR-FALSE-TEST
    ACTION-TO-CARRY-OUT-IF-TEST-IS-TRUE)
```

5 是否大于 4:

``` elisp
(if (> 5 4)                            ;; if-part
    (message "5 is greater than 4!"))  ;; then-part
```

判断一个字符串是否是 "fierce", 是就打印一句话, 否则返回 nil

``` elisp
(defun type-of-animal (characteristic)
       "Print message in echo area depending on CHARACTERISTIC.
       If the CHARACTERISTIC is the string \"fierce\",
       then warn of a tiger."
       (if (equal characteristic "fierce")
           (message "It is a tiger!")))

(type-of-animal "tiger")
(type-of-animal "fierce")
```

### 3.9 if-then-else ###

if-then-else 表达式如下:

``` elisp
(if TRUE-OR-FALSE-TEST
    ACTION-TO-CARRY-OUT-IF-THE-TEST-RETURNS-TRUE
    ACTION-TO-CARRY-OUT-IF-THE-TEST-RETURNS-FALSE)
```

else 语句就是紧接着 then 语句

例如:

``` elisp
(if (> 4 5)                                ;; if-part
    (message "4 falsely greater than 5!")  ;; then-part
    (message "4 is not greater than 5!"))  ;; else-part
```

Emacs有些命令可以自动缩进 if 语句, 见 GNU Emacs Helps You Type Lists.

``` elisp
(defun type-of-animal (characteristic)
	"Print message in echo area depending on CHARACTERISTIC.
If the CHARACTERISTIC is the string \"fierce\",
then warn of a tiger; else say it is not fierce."
    (if (equal characteristic "fierce")
        (message "It is a tiger!")
		(message "It is not fierce!")))

(type-of-animal "tiger")
(type-of-animal "fierce")
```

### 3.9 Truth and Falsehood ###

false 就是 nil, 而除了 nil 以外的都是 true

#### nil 的含义 ####

nil 有两层含义:

1. 表示空列表 `()`
2. 表示 false 并且是 true-or-false-test 为 false 时的返回值

``` elisp
(if 4
   'true
   'false)

(if nil
   ' true
   'false)
```

如果没有合适的true返回, 那么解释器会返回 `t`:

``` elisp
(> 5 4)  ;; t
(> 4 5)  ;; nil
```

### 3.10 save-excursion ###

`save-excursion` 是特殊形式

save-excursion 会存储上一个位置

#### Point and Mark ####

在 emacs 中, buffer中的第一个字符, point 是 1

函数 `point` 会返回光标当前的位置, 一个数字

mark 是 buffer 中的另一个位置, 可以用 `C-<SPC>` 或者 `set-mark-command` 来设置

如果一个 mark 被设置, 可以通过 `C-x C-x` 或 `exchange-point-and-mark` 来交换当前位置和 mark 位置, 并且保持选中的区域

如果设置了新的 mark, 之前的 mark 会保存到 *mark ring*

可以通过 `C-u C-<SPC>` 来跳转到这些 mark

在 point 和 mark 之间的区域叫做 region, 很多命令都用于 region:

* `center-reigon`
* `count-words-region`
* `kill-region`
* `print-region`

`count-words-region` 这个命令实际上会移动 point

为了防止光标一直跳动, `save-excursion` 经常被使用

#### 3.10.1 save-excursion 的使用模板 ####

如下:

``` elisp
(save-excursion
   BODY...)
```

``` elisp
(save-excursion
    FIRST-EXPRESSION-IN-BODY
    SECOND-EXPRESSION-IN-BODY
    THIRD-EXPRESSION-IN-BODY
    ...
    LAST-EXPRESSION-IN-BODY)

(let VARLIST
   (save-excursion
      BODY...))

```

### 3.11 复习 ###

#### `eval-last-sexp` ####

* `C-x C-e` 的命令, 执行前面的符号表达式

#### `defun` ####

`defun` 定义函数, 是一个宏, `dired-unmark-all-marks`的定义如下:

``` elisp
(defun dired-unmark-all-marks ()
   "Remove all marks from all files in the Dired buffer."
   (interactive)
   (dired-unmark-all-files ?\r))
```

#### `interactive` ####

`interactive`

* 交互式函数, 是一个特殊形式

有如下的参数:

* `b`: 一个已经存在的 buffer 的名字
* `f` 已经存在的文件的名字
* `p` 数值前缀参数
* `r` Point and the mark, 作为两个数值参数, 最小的在前面, 这是指定两个参数的唯一一个代码符号

#### `let` ####

声明一系列的局部变量, let 语句最终会返回最后一个表达式的值

``` elisp
(let ( (foo (buffer-name))
       (bar (buffer-size)) )
     (message
      "This buffer is %s and has %d characters."
      foo bar)
)
```

#### `save-excursion` ####

记录光标当前的位置和 buffer

``` elisp
(message "We are %d characters into this buffer."
         (- (point)
            (save-excursion
                (goto-char (point-min)) (point))))
```

#### `if` ####

``` elisp
(if (= 22 emacs-major-version)
    (message "This is version 22 Emacs")
    (message "This is not version 22 Emacs"))
```

常用的比较:

这五个的参数必须是数值或者 markers:

* `<`
* `>`
* `<=`
* `>=`
* `=`

`equal` 和 `eq` 测试两个对象是否一样, 但是有区别:

* `equal`: 只是判断内容是否一样
* `eq`: 必须指向同一个对象才相等

* `string<` 或 `(a STRING)`、 `string-lessp` 如果第一个参数比第二个小, 返回true, 大小写敏感, "" 空字符串是最小的

* `string=` 或 `string-equal`: 比较字符串是否相等, 字符串**没有** `>`、`>=`、`<=`

#### message ####

``` elisp
(message STRING ARGUMENTS)
```

#### setq ####

``` elisp
(set 'var1 'hello)

(setq var2 'hello)
```

#### buffer-name ####

``` elisp
(buffer-name)  ;; "elisp_learn.md"
```

返回 buffer 的 名字, 是字符串

#### buffer-file-name ####

``` elisp
(buffer-file-name)  ;; "d:/Coding/MyGitHub/SCAME/elisp_learn.md"
```

返回文件的名字

#### current-buffer ####

``` elisp
(current-buffer)  ;; #<buffer elisp_learn.md>
```

返回当前 active 的 buffer

#### other-buffer ####

返回最近一次选择的buffer

``` elisp
(other-buffer)  ;; #<buffer quick_tutorial.md>
```

#### switch-to-buffer ####

即: `C-x b`

#### set-buffer ####

将 emacs 的注意切换到另一个buffer, 不会改变当前显示的 buffer

#### point ####

当前光标的位置, 是一个数字, 从1开始

#### point-min ####

point可取的最小的值

#### point-max ####

point 在本 buffer 中可取的最大的值

``` elisp
(point-max)
(point)
```

### 3.12 练习 ###

1. 写一个 add 函数, 将两个数相加, 一个不交互, 一个交互

``` elisp
(defun my-add (number-1 number-2)
    "add NUMBER-1 and NUMBER-2"
    (+ number-1 number-2))

(my-add 3 4)
```

2. 写一个函数测试当前的fill-column值是否大于传入函数的参数, 如果是, 则打印信息

``` elisp
(defun my-compare (number)
     "compare NUMBER with fill-column, if NUMBER is larger than fill-column, then message"
     (if (> number fill-column)
         (message "%d is larger than fill-column %d" number fill-column))
)

(my-compare 71)  ;; "71 is larger than fill-column 70"
```

## 4 Buffer 相关的函数 ##

### 4.1 找更多的信息 ###

* `C-h f` 查看函数说明
* `C-h v` 查看变量说明
* `xref-find-definitions`: 跳转到定义
* `C-h p` **The GNU Emacs Manual**

### 4.2 `beginning-of-buffer` 定义 ###

`beginning-of-buffer` 通常绑定在 `M-<`

简短定义

包含一下几项:

* 必须是 interactive
* 必须在原来的位置留一个 mark
* 将光标移动到开头

``` elisp
(defun simplified-beginning-of-buffer ()
	"Move point to the beginning of the buffer;
	leave mark at previous position."
	(interactive)
	(push-mark)
	(goto-char (point-min))
)
```

`(push-mark)` 会在光标当前位置放一个mark, 这个mark会保存在 *mark ring*

`(goto-char (point-min))` 前往buffer的开头

另外 `C-x C-x` 可以返回刚才的位置

`goto-char` 的 doc:

```
Set point to POSITION, a number or marker.
Beginning of buffer is position (point-min), end is (point-max).
```

`goto-char` 的参数是想要前往的位置

另外, `end-of-buffer` 函数和 `beginning-of-buffer` 差不多, 唯一区别就是

``` elisp
(defun simplified-end-of-buffer ()
	"goto end of buffer"
	(interactive)
	(push-mark)
	(goto-char (point-max))
)
```

### 4.3 `mark-whole-buffer` 的定义 ###

通常绑定 `C-x h`

在 GNU Emacs 22, 完整的函数如下:

``` elisp
(defun mark-whole-buffer ()
	"Put point at beginning and mark at end of buffer.
	You probably should not use this function in Lisp programs;
	it is usually a mistake for a Lisp function to use any subroutine that uses or sets the mark."
	(interactive)
	(push-mark (point))
	(push-mark (point-max) nil t)
	(goto-char (point-min))
)
```

基本上不难理解, 除了

`(push-mark (point-max) nil t)`

`nil` 是告诉 push-mark 应该留下一个 "Mark set" 的信息, `t` 是告诉 push-mark 当 *Transient Mark mode* 被打开的时候应该激活 mark

*Transient Mark mode* 会高亮当前激活的区域

### 4.4 `append-to-buffer` 的定义 ###

这个函数是把 region 拷贝到指定的 buffer

它使用 `insert-buffer-string` 这个函数来拷贝区域

这个函数是 从一个buffer去一个子字符串, 然后插入到另一个buffer中


代码必须指定将要前往的buffer、从哪个窗口来、到哪个窗口去、要拷贝的区域

``` elisp
(defun append-to-buffer (buffer start end)
       "Append to specified buffer the text of the region.
     It is inserted into that buffer before its point.

     When calling from a program, give three arguments:
     BUFFER (or buffer name), START and END.
     START and END specify the portion of the current buffer to be copied."
  (interactive
    (list (read-buffer "Append to buffer: "
             (other-buffer
               (current-buffer) t
             )
          )
          (region-beginning)
          (region-end)

    )
  )

  (let ((oldbuf (current-buffer)))
     (save-excursion
        (let* ((append-to (get-buffer-create buffer))
               (windows (get-buffer-window-list append-to t t))
               point
              )
              (set-buffer append-to)
              (setq point (point))
              (barf-if-buffer-read-only)
              (insert-buffer-substring oldbuf start end)
              (dolist (window windows)
                 (when (= (window-point window) point)
                    (set-window-point window (point))
                 )
              )
        )
     )
  )

)
```

首先, 这个函数有三个参数:

1. buffer
2. start
3. end

接下来, 看 interactive 部分:

``` elisp
(interactive
 (list (read-buffer
        "Append to buffer: "
        (other-buffer (current-buffer) t))
        (region-beginning)
        (region-end)))
```

interactive 有三个部分:

第一个部分是 `read-buffer`, 会读取buffer的名字然后返回一个字符串。
* 这个函数第一个参数是一个提示字符串 "Append to buffer: ". 
* 第二个参数是默认的参数, 也就是什么也没指定的时候用的值。
  * 第二个参数是函数 `other-buffer`、exception 和 `t`
  * exception 调用了另一个函数 `current-buffer`
  * `t` 表示的是 TRUE, 它告诉 `other-buffer` 应该显示 visible buffers

``` elisp
(other-buffer (current-buffer) t)
```

`(rregion-beginning)` 和 `(region-end)` 制定了要添加的文字的开始和结束

原本这个命令使用的字符是 `B` 和 `r`:

``` elisp
(interactive "BAppend to buffer:\nr")
```

但是这样的话, 就没有默认参数了

`r` 是告诉 Emacs 绑定两个参数 start 和 end

#### 4.4.2 函数体 ####

`append-to-buffer` 函数的函数体:

首先以 `let` 开始, 也就是构建局部参数

``` elisp
(defun append-to-buffer (buffer start end)
	"DOCUMENTATION..."
    (interactive ...)
    (let ((VARIABLE VALUE))
         BODY...)
   ;; ...
)
```

let 表达式一共有三个部分:

1. 符号 let
2. 参数列表
3. let 表达式的 body

参数列表如下:

``` elisp
(oldbuf (current-buffer))
```

let 的函数体有 `save-excursion` 表达式

也就是保存 point 的位置, 在执行完之后还原 point 

在 `save-excursion` 的函数体中, 有一个 `let*`, 表示按顺序设置每个参数 *It enables Emacs to set each variable in its varlist in sequence, one after another.*

它重要的特性是让 **后面的参数能够用到前面的参数**, 这就是 `let*`

接下来是

``` elisp
(set-buffer append-to)
```

`append-to` 在 `let*` 中被绑定到了 `(get-buffer-create buffer)`

### 4.5 复习 ###

‘describe-function’
‘describe-variable’:

* 打印函数或者变量的说明文档, 通常绑定到 ‘C-h f’ and ‘C-h v’.

‘xref-find-definitions’:

* 找到函数的定义

‘save-excursion’:

* 记录当前光标位置, 在执行完函数体之后把光标移动回来

‘push-mark’:

* 在光标位置放一个 mark, 这个mark会存到 mark ring 中

‘goto-char’:

* 把光标移动到某个位置, 例如 (point-min)

‘insert-buffer-substring’:
* 把选中的区域插入到另一个buffer中, 然后再返回当前buffer

‘mark-whole-buffer’

* 全选

‘set-buffer’:

* 切换buffer

‘get-buffer-create’

* 如果 buffer 不存在就创建

‘get-buffer’:

* 获得一个buffer的名字, 如果不存在, 就返回 nil

### 4.6 练习 ###

   • Use ‘if’ and ‘get-buffer’ to write a function that prints a message
     telling you whether a buffer exists.

``` elisp
(defun if-a-buffer-exists (buffer)
   "根据 BUFFER 判断一个 buffer 是否存在, 并打印消息"
   (interactive "sEnter buffer name: ")
   (if (get-buffer buffer)
       (message "Buffer %s exists." buffer)
       (message "Buffer %s don't exists." buffer)
)
)
```


``` elisp
    (defun multiple-hello (someone num)
      "Say hello to SOMEONE via M-x hello, for NUM times."
      (interactive "sWhom do you want to say hello to? \nnHow many times? ")
      (dotimes (i num)
        (insert (format "Hello %s!\n" someone))))
```

``` elisp
(defun someone-say-hello-to-someone (first second)
   "FIRST say hello to SECOND"
   (interactive "sFirst person's name: \nsSecond person's name: ")
   (message "%s says hello to %s." first second)
)
```


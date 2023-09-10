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




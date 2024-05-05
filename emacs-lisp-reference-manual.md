<!-- Local Variables: -->
<!-- eval: (global-display-line-numbers-mode 0) -->
<!-- eval: (flycheck-mode 0) -->
<!-- eval: (company-mode -1) -->
<!-- End: -->
# 学习Elisp #

[TOC]

## 2 Lisp 数据类型 ##

内置到Emacs中的类型叫做 *primitive types*

每个对象仅属于一个 *primitive types*

*primitive types* 有以下这几种:

* *integer*
* *float*
* *cons*
* *symbol*
* *string*
* *vector*
* *hash-table*
* *subr*
* *byte-code function*
* *record*
* *buffer*

### 2.1 Printed Representation and Read Syntax ###

一个对象的 *printed representation* 是由函数 `prin1` 生成的输出

``` Elisp
(prin1 1)
```

一个对象的 *read syntax* 是由函数 `read` 的为该对象接受的输入格式, 这个可以不唯一

在大多数情况下，对象的打印表示也是该对象的读语法。

然而，有些类型没有读语法，因为在Lisp程序中将这些类型的对象作为常量输入是没有意义的。

这些对象以散列表示法打印，散列表示法由字符“#<”、描述性字符串(通常是类型名称后面跟着对象名称)和结尾的“>”组成。

(这被称为“哈希符号”，因为它以“#”字符开头，称为“哈希”或“数字符号”)。

例如:

``` Elisp
(current-buffer)
;; => #<buffer learn_emacs.md>
```

在其他语言中，表达式是文本;它没有其他形式。

在Lisp中，表达式主要是一个Lisp对象，其次是作为对象的读语法的文本。

通常没有必要强调这种区别，但你必须把它记在心里，否则你偶尔会很困惑。

### 2.2 Special Read Syntax ###

Emacs Lisp通过特殊的哈希符号表示许多特殊的对象和结构。

* `#<...>`: 没有 *read syntax* 的对象是这样表示的
* `##`: 称为空字符串的内部符号的打印表示形式
* `#'`: This is a shortcut for function, see Anonymous Functions. 
* `#:`: 名称为foo的 非内部符号(uninterned symbol) 的打印表示为' #:foo '(参见符号类型)。
* `#N`: 当打印 圆形结构(circular structures) 时，这个结构用来表示结构返回到自身的位置，'N' 是开始列表计数:

``` Elisp
(let ((a (list 1)))
  (setcdr a a))
;; => (1 . #0)
```

* `#N=` 和 `#N#`: `#N=` 给出对象的名称，`#N#` 表示该对象，因此当回读对象时，它们将是相同的对象，而不是副本(参见循环对象的读取语法)。
* `#xN`: 用十六进制数表示的 N `#x2a`。
* `#oN`: N 表示为八进制数 `#o52`
* `#bN`: N 表示二进制数 `#b101010`
* `#(...)`: 字符串文本属性(参见字符串中的文本属性 Text Properties in Strings)。
* `#^`: 一个字符表(参见字符表类型 Char-Table Type)。
* `#s(hash-table ...)`: 一个哈希表
* `?C`: 一个字符
* `#$`: 字节编译文件(byte-compiled files)中的当前文件名(请参阅文档字符串和编译)。这并不意味着在Emacs Lisp源文件中使用。
* `#@N`: 跳过下一个“N”字符(参见注释)。这是在字节编译文件中使用的，并不打算在Emacs Lisp源文件中使用。
* `#f`: 指示以下格式不能被Emacs Lisp阅读器读取。这只在文本中用于显示目的(当这比其他指示不可读形式的方法看起来更漂亮时)，并且永远不会出现在任何Lisp文件中。

### 2.3 Comments ###

#### 注释 ####

注释是在程序中编写的仅供阅读程序的人使用的文本，它对程序的含义没有影响。

在Lisp中，如果未转义的分号(';')不在字符串或字符常量中，则开始注释。

注释继续到行尾。Lisp读者抛弃了注释;它们不会成为在Lisp系统中表示程序的Lisp对象的一部分。

`#@count` 结构跳过下一个计数字符，对于包含二进制数据的程序生成注释很有用。

Emacs Lisp字节编译器在它的输出文件中使用这个(参见字节编译)。但是，它并不适用于源文件。

#### Tips on Writing Comments ####

我们建议对这些约定进行注释:

##### `;` #####

以单个分号“;”开头的注释都应该对齐到源代码右侧的同一列。这样的注释通常解释该行上的代码是如何完成其工作的。例如:

``` Elisp
(setq base-version-list                 ; There was a base
      (assoc (substring fn 0 start-vn)  ; version to which
             file-version-assoc-list))  ; this looks like
                                        ; a subversion.
```

##### `;;` #####

以两个分号“;;”开头的注释应该与代码对齐到相同的缩进级别。

这样的注释通常描述了下面几行代码的目的或者程序在这一点上的状态。例如:

``` Elisp
(prog1 (setq auto-fill-function
             …
             …
  ;; Update mode line.
  (force-mode-line-update)))
```

我们通常也使用两个分号来表示函数外的注释。

``` Elisp
;; This Lisp code is run in Emacs when it is to operate as
;; a server for other processes.
```

如果一个函数没有文档字符串，它应该在函数前面有一个双分号注释，解释函数做什么以及如何正确调用它。

准确地解释每个参数的含义以及函数如何解释其可能的值。

不过，将这些注释转换为文档字符串要好得多。

##### `;;;` #####

以三个(或更多)分号“;;;”开头的注释应该从左空白处开始。

我们将它们用于注释，这些注释应该被Outline次要模式视为标题。

默认情况下，以至少三个分号开头的注释(后面跟着一个空格和一个非空格字符)被视为节标题，以两个或更少的注释不被视为节标题。

(从历史上看，三分号注释也用于在函数中注释掉行，但不鼓励使用这种用法，而只使用两个分号。

这也适用于注释整个函数;这样做时，也使用两个分号。)


三个分号用于顶级部分，四个分号用于子部分，五个分号用于子部分，以此类推。

通常库至少有四个顶级部分。

例如，当所有这些部分的主体被隐藏时:

``` Elisp
;;; backquote.el --- implement the ` Lisp construct...
;;; Commentary:...
;;; Code:...
;;; backquote.el ends here
```

(从某种意义上说，最后一行不是节标题，因为它绝不能后面跟着任何文本;毕竟，它标志着文件的结束。)

对于较长的库，建议将代码分成多个部分。

这可以通过将“Code:”部分分割成多个子部分来实现。尽管这是长期以来唯一推荐的方法，但许多人还是选择使用多个顶级代码段。

你可以选择任何一种风格。


使用多个顶级代码段的优点是避免引入额外的嵌套层，但这也意味着名为“code”的部分不包含所有代码，这很尴尬。为了避免这种情况，你不应该在section中放入任何代码;这样，它可以被视为分隔符而不是节标题。


最后，我们建议不要以冒号或其他标点符号结束标题。

由于历史原因，“Code:”和“Commentary:”标题以冒号结尾，但我们建议您不要为其他标题做同样的事情。

一般来说，`M-;(comment-dwim)` 命令自动启动相应类型的注释;或将现有注释缩进到正确的位置，具体取决于分号的数量。

请参阅《GNU Emacs手册》中的操作注释。

### 2.4 Programming Types ###

在Emacs Lisp中有两大类类型:

1. 与Lisp编程有关的类型 Lisp programming
2. 与编辑有关的类型。 editing

前者以这样或那样的形式存在于许多Lisp实现中。

后者是Emacs Lisp所特有的。

* Integer Type
* Floating-Point Type
* Character Type
* Symbol Type
* Sequence Types
* Cons Cell and List Types
* Array Type
* String Type
* Vector Type
* Char-Table Type
* Bool-Vector Type
* Hash Table Type
* Function Type
* Macro Type
* Primitive Function Type
* Byte-Code Function Type
* Record Type
* Type Descriptors
* Autoload Type
* Finalizer Type

#### 2.4.1 Integer Type ####

在底层(under the hood)，有两种整数

1. 小整数(称为 *fixnums*) *fixnums* 的取值范围取决于机器。
2. 大整数(称为 *bignum*)。 *bignum* 的值任意大小

所有的数值都可以用 `eql` 或者 `=` 来比大小

*fixnums* 也可以用 `eq` 来进行比较

要测试一个整数是固定数还是大数，您可以将其与 `most-negative-fixnum` 和 `most-positive-fixnum` 进行比较

* `most-negative-fixnum` is -2305843009213693952
* `most-positive-fixnum` is 2305843009213693951

或者您可以在任何对象上使用方便的谓词 `fixnump` 和 `bignump`

`(fixnump 2305843009213693952)`

整数的读语法是一个以10为基数的数字序列，开头是一个可选的符号，结尾是一个可选的句点。

Lisp解释器生成的打印表示从来没有前导 `+` 或结尾 `.`

``` Elisp
-1               ; The integer −1.
1                ; The integer 1.
1.               ; Also the integer 1.
+1               ; Also the integer 1.
```

#### 2.4.2 Floating-Point Type ####

浮点数在计算机上相当于科学记数法;你可以把浮点数想象成一个分数加上10的幂。

有效数字的精确数量和可能的指数范围是特定于机器的;Emacs使用C数据类型 `double` 来存储该值，并在内部记录2的幂而不是10的幂。

浮点数的打印表示要么需要一个小数点(后面至少有一个数字)，要么需要一个指数，或者两者都需要。例如,

* `1500.0`
* `+15e2`
* `15.0e+2`
* `+1500000e-3`
* `.15e4`

都表示浮点数 1500

#### 2.4.3 Character Type ####

##### 2.4.3.0 Character Type 概述 #####

在Emacs Lisp中，字符只不过是一个整数。

换句话说，字符是由字符代码表示的。

例如，字符 `A` 表示为整数 `65`。

这也是他们通常的印刷形式;参见基本Char语法。 Basic Char Syntax

个别字符偶尔会在程序中使用，但更常见的是与字符串一起工作，字符串是由字符组成的序列。

请参见字符串类型。 String Type

字符串和buffer中的字符目前限制在 `0 到 4194303` - 22bits 的范围内(参见字符代码)。

码 `0 到 127` 为ASCII码;

其余为非ascii字符(请参阅非ascii字符)。

表示键盘输入的字符具有更广泛的范围，以编码诸如Control, Meta和Shift之类的修饰键 *modifier keys*。

有一些特殊的函数用于生成人类可读的字符文本描述，以便传递消息。

请参见 *Describing Characters for Helping Messages*

##### 2.4.3.1 Basic Char Syntax #####

由于字符实际上是整数，因此字符的打印表示形式是十进制数。

这也是字符的一种可能的读语法，但是在Lisp程序中这样写字符并不是清晰的编程。

您应该始终使用Emacs Lisp为字符提供的特殊读取语法格式。这些语法格式以 *问号question mark* 开头。

对于字母数字字符，通常的读取语法是问号后面跟着字符;

因此,`?A` 表示字符 A，`?B` 代表字符B，而 `?a` 代表字符a。

``` Elisp
?Q  ; => 81
?q  ; => 113
```

您可以对 *标点符号 punctuation characters* 使用相同的语法。

但是，如果标点符号在Lisp中具有特殊的语法含义，则必须用 `\` 来引用它。

例如，`?\(` 是书写开括号字符的方式。

同样，如果字符是 `\` ，则必须使用第二个 `\` 来引用它 `?\\`。

您可以将control-g、退格、制表符、换行符、垂直制表符、换行符、空格、回车、del和转义等字符表示为 `?\`, `?\`, `?\t`, `?\n`, `?\v`, `?\f`, `?\`, `?\r`, `?\d` 和 `?\e`。

(' ?\s’后面跟一个破折号有不同的含义——它对后面的字符应用Super修饰符。)

因此,

``` Elisp
?\a ⇒ 7                 ; control-g, C-g
?\b ⇒ 8                 ; backspace, BS, C-h
?\t ⇒ 9                 ; tab, TAB, C-i
?\n ⇒ 10                ; newline, C-j
?\v ⇒ 11                ; vertical tab, C-k
?\f ⇒ 12                ; formfeed character, C-l
?\r ⇒ 13                ; carriage return, RET, C-m
?\e ⇒ 27                ; escape character, ESC, C-[
?\s ⇒ 32                ; space character, SPC
?\\ ⇒ 92                ; backslash character, \
?\d ⇒ 127               ; delete character, DEL
```

这些以反斜杠开头的序列也被称为转义序列，因为反斜杠起转义字符的作用;这与ESC字符无关。

`\s` 用于字符常量;在字符串常量中，只写空格。

在没有特殊转义含义的任何字符之前允许使用反斜杠，并且是无害的;因此, `?\+` 相当于`?+`。

没有理由在大多数字符之前添加反斜杠。

但是，必须在任何字符 `()[]\`; 你应该添加一个反斜杠之前的任何角色的 `|'\`#.`。

以避免在编辑Lisp代码时混淆Emacs命令。

您还应该在Unicode字符(类似于前面提到的ASCII字符)之前添加反斜杠，以避免让阅读代码的人感到困惑。

Emacs将突出显示一些非转义的常见混淆字符，如 `'`，以鼓励这样做。

您还可以在空格、制表符、换行符和formfeed等空白字符之前添加反斜杠。

但是，使用易于阅读的转义序列之一，如 `\t` 或 `\s`，而不是实际的空白字符，如制表符或空格，会更简洁。

(如果你写反斜杠后面跟着空格，你应该在字符常量后面写一个额外的空格，把它和后面的文本分开。)

##### 2.4.3.2 General Escape Syntax #####

除了特殊重要控制字符的特定转义序列之外，Emacs还提供了几种类型的转义语法，您可以使用它们来指定非ascii文本字符。

您可以通过Unicode名称(如果有的话)指定字符。

###### `?\N{NAME}` ######

`?\N{NAME}` 表示名为 *NAME* 的 Unicode字符。

因此, `?\N{LATIN SMALL LETTER A WITH GRAVE}` 相当于 `?à`，表示Unicode字符 `U+00E0`。

为了简化多行字符串的输入，您可以用非空的空白序列(例如，换行符)替换名称中的空格。

###### `?\N{U+X}` ######

可以通过Unicode值指定字符。

`\N{U+X}` 表示Unicode编码X的字符，其中X是十六进制数。

此外，`?\uxxxx` 和 `?\Uxxxxxxxx` 分别表示代码点 *xxxx* 和 *xxxxxxxx*，其中每个x是一个十六进制数字。

例如，`?\N{U+E0}`， `?\u00e0` 和 `?\U000000E0` 都等价于 `?à` 和 `?\N{LATIN SMALL LETTER A WITH GRAVE}`。

Unicode标准只定义最高为 `U+10ffff` 的码位，因此如果指定的码位高于此值，Emacs将发出错误信号。

###### `?\xN` ######

可以通过十六进制字符码来指定字符。

十六进制转义序列由反斜杠、x 和十六进制字符代码组成。

因此, `?\x41` 是字符A，`?\x1` 是字符 `C-a`，而 `?\xe0` 是字符 `à` (带有重音的字符a)。

您可以在 x 后面使用一个或多个十六进制数字，因此您可以以这种方式表示任何字符代码。

###### `?\oct` ######

您可以通过八进制的字符代码来指定字符。

八进制转义序列由反斜杠后跟最多三个八进制数字组成;

因此, `?\101` 表示字符 A，`?\001` 表示字符 `C-a`， `?\002` 表示字符 `C-b`。

只能以这种方式指定八进制代码777以内的字符。

##### 2.4.3.3 Control-Character Syntax #####

控制字符可以使用另一种 read syntax 来表示。

它包括一个问号，后面跟着一个反斜杠、插入符号和相应的非控制字符(大写或小写)。

例如，`?\^I` 和 `?\^i` 是字符 `C-i`(其值为9)的有效 read syntax。

你可以用 `C-` 代替 `^` ;

因此, `?\C-i` 等于 `?\^I` 和 `?\^i`:

``` Elisp
?\^I   ; 9
?\C-I  ; 9
```

在字符串和buffer中，唯一允许的控制字符是那些存在于ASCII中的字符;但是对于键盘输入，您可以使用 `C-` 将任何字符转换为控制字符。

这些非ascii控制字符的字符代码包括 `2**26` 位以及相应的非控制字符的代码。

并非所有文本终端都可以生成非ascii控制字符，但是使用X和其他窗口系统生成它们是很简单的。

由于历史原因，Emacs将 `DEL` 字符视为 `?`

``` Elisp
?\^?  ; 127
?\C-? ; 127
```

因此，目前不可能表示字符 `Control-?`，它是X下有意义的输入字符，使用 `\C-`。

更改这一点并不容易，因为各种Lisp文件都以这种方式引用 `DEL`。

为了表示要在文件或字符串中找到的控制字符，我们推荐使用 `^` 语法;

对于键盘输入中的控制字符，我们更倾向于使用 `C-` 语法。

你使用哪一个并不影响程序的意思，但可能会引导阅读它的人的理解。

##### 2.4.3.4 Meta-Character Syntax #####

*meta* 是使用 meta 修饰符键键入的字符。

表示该字符的整数集为 `2**27` 位。

我们为这个和其他修饰符使用高比特，以使广泛的基本字符代码成为可能。

在字符串中，附加在ASCII字符后面的 `2**7` 位表示元字符;

因此，可以在字符串中容纳的元字符的代码范围从 `128到255`，并且是普通ASCII字符的元版本。

请参阅 Putting Keyboard Events in Strings，以了解字符串中元处理的详细信息。

元字符的读取语法使用 `\M-`。

例如，`?M-A` 代表 `M-A`。

您可以将 `\M-` 与八进制字符代码(见下文)、`\C-` 或任何其他字符语法一起使用。

因此，你可以把 `M-A` 写成 `?\M-A`，或 `?\M-\101`。

同样地，你可以把 `C-M-b` 写成 `?\M-\C-b`、`?\C-\M-b` 或 `?\M-\002`。

##### 2.4.3.5 Other Character Modifier Bits #####

图形字符graphic character 的大小写由其字符代码表示;

例如，ASCII区分字符 `a` 和 `A`。

但是ASCII没有办法表示控制字符是大写还是小写。

Emacs使用 `2**25` 位表示在输入控制字符时使用了shift键。

这种区别只可能在图形显示(如X上的GUI显示)上实现;文本终端不报告这种区别。

*shift bit* 的Lisp语法是 `\S-` ;因此, `?\C-\S-o` or `?\C-\S-O ` 表示 `shift-control-o` 字符。

X窗口系统定义了另外三个可以在字符中设置的修饰位: *hyper*, *super* 和 *alt*。

这些位的语法是 `\H-`， `\s-` 和 `\A-`。

(这些前缀的大小写很重要)

因此, `?\H-\M-\A-X` 表示 `Alt-Hyper-Meta-x`。

(注意，后面没有 `-` 的 `\s` 表示空格字符。)

数值上，位值为 `2**22` 表示 *alt*, `2**23` 表示 *super*, `2**24` 表示 *hyper*。

#### 2.4.4 Symbol Type ####

在GNU Emacs Lisp中，symbol 是一个有名称的对象。

符号名称作为符号的 print representation

在普通的Lisp使用中，对于单个obarray(见Creating and Interning S

ymbols)，符号的名称是唯一的——没有两个符号具有相同的名称。

符号可以用作变量、函数名或保存属性列表。

或者，它可以只用于区别于所有其他Lisp对象，以便它在数据结构中的存在可以被可靠地识别出来。

在给定的上下文中，通常只打算使用其中一种用法。

但是你可以在所有这些方法中独立地使用一个符号。

名称以冒号(':') 开头的符号称为关键字符号。

这些符号自动充当常量，通常仅在将未知符号与少数特定替代符号进行比较时使用。

参见 Variables that Never Change

符号名可以包含任何字符。

大多数符号名称由字母、数字和标点符号 `-+=*/` 组成。

这样的名字不需要特殊的标点符号;

只要名称看起来不像数字，名称的字符就足够了。

(如果是这样，在名称的开头写一个' \ '来强制解释为一个符号。)

字符 `_~!@$%^&:<>{}?`，使用频率较低，但也不需要特殊的标点符号。

任何其他字符都可以通过用反斜杠转义包含在符号名称中。

然而，与它在字符串中的用法相反，符号名称中的反斜杠只是引用反斜杠后面的单个字符。

例如，在字符串中，`t` 表示制表符;

然而，在符号的名称中，`\t` 只是引用了字母 t。

要使一个符号的名称中有制表符，必须实际使用制表符(前面有反斜杠)。

但这种事很少发生。

>Common Lisp 注意: 在公共Lisp中，小写字母总是被折叠成大写字母，除非它们被显式转义。在Emacs Lisp中，大写字母和小写字母是不同的。

这里有几个符号名称的例子。

注意，第四个示例中的 `+` 被转义，以防止将其读取为数字。

在第六个示例中不需要这样做，因为名称的其余部分使其作为数字无效。

``` Elisp
 A symbol named ‘foo’.
FOO                 ; A symbol named ‘FOO’, different from ‘foo’.

1+                  ; A symbol named ‘1+’
                    ;   (not ‘+1’, which is an integer).

\+1                 ; A symbol named ‘+1’
                    ;   (not a very readable name).

\(*\ 1\ 2\)         ; A symbol named ‘(* 1 2)’ (a worse name).
+-*/_~!@$%^&=:<>{}  ; A symbol named ‘+-*/_~!@$%^&=:<>{}’.
                    ;   These characters need not be escaped.
```

作为符号名称作为其打印表示的规则的一个例外，`##` 是名称为空字符串的内部符号的打印表示。

此外，`#:foo` 是名称为foo的非内部符号的打印表示形式。

(通常情况下，Lisp阅读器会实习所有的符号;参见Creating and Interning Symbols.)

#### 2.4.5 Sequence Types ####

序列是一个Lisp对象，表示元素的有序集合。在Emacs Lisp中有两种序列: list 和 array。

**列表** 是最常用的序列。

列表可以保存任何类型的元素，并且可以通过添加或删除元素轻松更改其长度。

有关列表的更多信息，请参见下一小节。

**数组** 是固定长度的序列。

它们又进一步细分为 字符串、向量、字符表和布尔向量。

* strings
* vectors
* char-tables
* bool-vectors

vector可以保存任何类型的元素，而string元素必须是字符，bool-vector元素必须是t或nil。

char-tables 类似于向量，不同之处在于它们是由 **任何有效字符代码** 索引的。

字符串中的字符可以像buffer中的字符一样具有文本属性(参见文本属性)，但是向量不支持文本属性，即使它们的元素恰好是字符。

列表、字符串和其他数组类型也有重要的相似之处。

例如，它们的长度都是l，并且它们的元素都可以从 `0` 索引到 `l-1`。

有几个函数，称为序列函数，可以接受任何类型的序列。

例如，函数 `length` 报告任意类型序列的长度。

参见序列、数组和向量。

通常不可能读取相同的序列两次，因为在读取时总是重新创建序列。

如果两次读取序列的read语法，将得到两个具有相同内容的序列。

有一个例外: empty list `()` 总是代表同一个对象 `nil`。

#### 2.4.6 Cons Cell and List Types ####

`cons cell` 是由两个 *槽slot* 组成的对象，称为 `CAR槽` 和 `CDR槽`。

每个槽可以容纳任何Lisp对象。

我们还说此 cons cell 的 CAR 是其 CAR 槽当前持有的任何对象，CDR也是如此。

list 是一系列 cons cells，它们链接在一起，以便每个 cons cell 的 CDR槽 保存下一个 cons cell 或 空列表。

空列表实际上是nil符号。

详情请参见列表。

因为大多数cons单元格被用作列表的一部分，所以我们将任何由cons单元格组成的结构称为列表结构。

> 给C程序员的提示: Lisp列表的工作方式是一个由单元格组成的链表。因为Lisp中的指针是隐式的，所以我们不区分保存值的cell槽和指向该值的cell槽。

因为cons cell在Lisp中如此重要，所以我们也有一个词来表示不是cons cell的对象。

这些对象被称为 *原子atom*。

列表的读取语法和打印表示是相同的，并且由左括号、任意数量的元素和右括号组成。

下面是一些列表的例子:

``` Elisp
(A 2 "A")            ; A list of three elements.
()                   ; A list of no elements (the empty list).
nil                  ; A list of no elements (the empty list).
("A ()")             ; A list of one element: the string "A ()".
(A ())               ; A list of two elements: A and the empty list.
(A nil)              ; Equivalent to the previous.
((A B C))            ; A list of one element
                     ;   (which is a list of three elements).
```

读取时，括号内的每个对象都成为列表的一个元素。

也就是说，为每个元素创建一个cons单元格。

cons单元的 CAR槽 保存元素，它的 CDR槽 指向列表的下一个cons单元，它保存列表中的下一个元素。

最后一个cons单元的 CDR slot 设置为nil。

CAR和CDR这两个名字来源于Lisp的历史。

最初的Lisp实现在IBM 704计算机上运行，它将单词分成两部分，地址和减数:

1. the address
2. the decrement

CAR是提取寄存器地址部分内容的指令，CDR是提取减量部分内容的指令。

相反，cons单元格是以创建它们的函数命名的，而该函数又以其目的命名，即单元格的构造。

##### 2.4.6.1 Drawing Lists as Box Diagrams #####

列表可以用图表来表示，其中的cons单元格显示为成对的框，就像多米诺骨牌一样。

(Lisp读者无法阅读这样的插图;与人类和计算机都能理解的文本符号不同，盒子插图只能由人类理解。)

该图表示三元素表 (玫瑰、紫罗兰、毛茛) (rose violet buttercup):

``` Plaintext
    --- ---      --- ---      --- ---
   |   |   |--> |   |   |--> |   |   |--> nil
    --- ---      --- ---      --- ---
     |            |            |
     |            |            |
      --> rose     --> violet   --> buttercup
```

在这个图中，每个框代表一个槽，它可以容纳或引用任何Lisp对象。

每对盒子代表一个cell。

每个箭头表示对Lisp对象的引用，该对象可以是原子，也可以是另一个cons单元格。

在本例中，保存第一个 cons cell 的CAR的第一个框引用或保存rose(一个 symbol)。

第二个盒子，保存第一个cons单元的CDR，指向的是下一对盒子，第二个cons cell。

第二个cell的 CAR 是 violet，它的CDR是第三个cell。

第三个(也是最后一个) cons cell 的 CDR 为 nil。

这是同一列表的另一张图 (rose violet buttercup)，以不同的方式绘制:

``` Plaintext
 ---------------       ----------------       -------------------
| car   | cdr   |     | car    | cdr   |     | car       | cdr   |
| rose  |   o-------->| violet |   o-------->| buttercup |  nil  |
|       |       |     |        |       |     |           |       |
 ---------------       ----------------       -------------------
```

没有元素的列表是空列表;它与符号nil相同。

换句话说，nil既是一个符号又是一个列表。


下面是列表 `(A())`，或者等价的 `(A nil)`，用方框和箭头表示:

``` Plaintext
    --- ---      --- ---
   |   |   |--> |   |   |--> nil
    --- ---      --- ---
     |            |
     |            |
      --> A        --> nil
```

这里有一个更复杂的例子，显示了一个三元素列表 `((pine needles) oak maple)`，其中的第一个元素是一个两元素列表:

``` Plaintext
    --- ---      --- ---      --- ---
   |   |   |--> |   |   |--> |   |   |--> nil
    --- ---      --- ---      --- ---
     |            |            |
     |            |            |
     |             --> oak      --> maple
     |
     |     --- ---      --- ---
      --> |   |   |--> |   |   |--> nil
           --- ---      --- ---
            |            |
            |            |
             --> pine     --> needles
```

在第二个方框符号中表示的相同列表看起来像这样:

``` Plaintext
 --------------       --------------       --------------
| car   | cdr  |     | car   | cdr  |     | car   | cdr  |
|   o   |   o------->| oak   |   o------->| maple |  nil |
|   |   |      |     |       |      |     |       |      |
 -- | ---------       --------------       --------------
    |
    |
    |        --------------       ----------------
    |       | car   | cdr  |     | car     | cdr  |
     ------>| pine  |   o------->| needles |  nil |
            |       |      |     |         |      |
             --------------       ----------------
```

##### 2.4.6.2 Dotted Pair Notation #####

点对表示法是显式表示CAR和CDR的单元格的通用语法。

在这个语法中，`(a . b)` 表示一个 cons cell，其CAR是对象a, CDR是对象b。

点对表示法比列表语法更通用，因为CDR不一定是列表。

然而，在列表语法可以工作的情况下，它更麻烦。

在点对表示法中，列表 `(1 2 3)` 写成 `(1 . (2 . (3 . nil)))`。

对于以nil结尾的列表，您可以使用任意一种表示法，但列表表示法通常更清晰、更方便。

在打印列表时，只有当cons cell的 CDR 不是列表时才使用点对表示法。

下面是一个使用方框来说明点对表示法的示例。这个例子展示了 `(rose . violet)`:

``` Plaintext
    --- ---
   |   |   |--> violet
    --- ---
     |
     |
      --> rose
```

您可以将点对表示法与列表表示法结合起来，方便地表示具有 非nil 最终CDR的cons cell链。

在列表的最后一个元素后面写一个点，后面是最后一个cons单元格的CDR。

例如，`(rose violet . buttercup)` 相当于 `(rose . (violet . buttercup))` 。

对象看起来是这样的:

``` Plaintext
    --- ---      --- ---
   |   |   |--> |   |   |--> buttercup
    --- ---      --- ---
     |            |
     |            |
      --> rose     --> violet
```

语法 `(rose . violet . buttercup)` 是无效的，因为它没有任何意义。

如果有的话，它会说把毛茛放在一个CDR已经用于 violet 的cell的CDR中。

列表 `(rose violet)` 相当于 `(rose . (violet))`，看起来像这样:

``` Plaintext
    --- ---      --- ---
   |   |   |--> |   |   |--> nil
    --- ---      --- ---
     |            |
     |            |
      --> rose     --> violet
```

类似地，三元素列表 `(rose violet buttercup)` 等价于 `(rose . (violet . (buttercup)))`

它是这样的:

``` Plaintext
    --- ---      --- ---      --- ---
   |   |   |--> |   |   |--> |   |   |--> nil
    --- ---      --- ---      --- ---
     |            |            |
     |            |            |
      --> rose     --> violet   --> buttercup
```

##### 2.4.6.3 Association List Type #####

*association list* 或 *alist* 是一种特殊构造的列表，其元素为 cons cells。

在每个元素中，CAR被认为是一个键，CDR被认为是一个 *associated value*。

(在某些情况下，关联值存储在 cons cell 的CAR中。)

关联列表通常用作 stacks，因为在列表的前面添加或删除关联很容易。

``` Elisp
(setq alist-of-colors
      '((rose . red) (lily . white) (buttercup . yellow)))
```

将变量 `list-of-colors` 设置为包含三个元素的列表。

在第一个元素中，rose是键，red是值。


请参阅关联列表，以获得关于列表的进一步解释以及在列表上工作的函数。

请参阅哈希表，了解另一种查找表，它在处理大量键时要快得多。

#### 2.4.7 Array Type ####

array 由任意数量的 slot 组成，用于存放或引用其他Lisp对象，排列在连续的内存块中。

访问数组的任何元素都需要大约相同的时间。

相反，访问列表中的元素所需的时间与该元素在列表中的位置成正比。

(访问列表末尾的元素比访问列表开头的元素需要更长的时间。)

Emacs定义了四种类型的数组:

1. 字符串 strings
2. 向量 vectors
3. 布尔向量 bool-vectors
4. 字符表 char-tables

字符串是字符数组，而向量是任意对象数组。

bool向量只能保存t或nil。

这些类型的数组可以有任何长度，直到最大的固定值，受系统架构限制和可用内存的限制。

char -table是由任何有效字符代码索引的稀疏数组;它们可以容纳任意的物体。

数组的第一个元素的索引为0，第二个元素的索引为1，依此类推。

这被称为零源索引 *zero-origin indexing*。

例如，一个包含四个元素的数组的下标为0、1、2和3。

最大可能的索引值比数组的长度小1。

一旦创建了数组，它的长度是固定的。

所有Emacs Lisp数组都是一维的。

(大多数其他编程语言支持多维数组，但它们不是必需的;您可以使用嵌套的一维数组获得相同的效果。)

每种类型的数组都有自己的读语法;有关详细信息，请参见以下部分。

数组类型是序列类型的一个子集，包含字符串类型、向量类型、bool-vector类型和char-table类型。

#### 2.4.8 String Type ####

字符串是字符的数组。

字符串在Emacs中有很多用途，正如在文本编辑器中所期望的那样;例如，作为Lisp符号的名称，作为用户的消息，以及表示从缓冲区中提取的文本。

Lisp中的字符串是常量:对字符串求值返回相同的字符串。

有关操作字符串的函数，请参阅字符串和字符。

##### 2.4.8.1 Syntax for Strings #####

字符串的读取语法是一个双引号，任意数量的字符，再加上另一个双引号，"like this"。

若要在字符串中包含双引号，请在其前面加上反斜杠;

因此，"\""是一个只包含一个双引号字符的字符串。

同样，你也可以在反斜杠前面加上另一个反斜杠，像这样: "this \\ is a single embedded backslash"。

由于字符串是一个字符数组，因此可以使用字符的read语法指定字符串字符，但不带前导问号。

这对于在字符串常量中包含不代表自身的字符很有用。

因此，控制字符可以指定为以反斜杠开头的转义序列;例如，"foo\r" 产生 "foo" 后跟回车符。

有关其他控制字符的转义序列，请参阅基本字符语法。

类似地，您可以为控制字符使用特殊的read语法(参见控制字符语法)，如"foo\^Ibar"，它会在字符串中嵌入一个制表符。

您还可以对通用转义语法中描述的非ascii字符使用转义序列，如 "\N{LATIN SMALL LETTER A WITH GRAVE}" 和 "\u00e0" (但是，请参阅非ascii字符中的非ascii字符的警告)。

换行符在字符串的读语法中并不特殊;如果在双引号之间添加新行，它将成为字符串中的一个字符。

但是转义的换行符(前面有' \ ')不会成为字符串的一部分;也就是说，Lisp阅读器在读取字符串时忽略转义换行符。

转义的空格 `\ `同样会被忽略。

``` Elisp
"It is useful to include newlines
in documentation strings,
but the newline is \
ignored if escaped."
     ⇒ "It is useful to include newlines
in documentation strings,
but the newline is ignored if escaped."
```

##### 2.4.8.2 Non-ASCII Characters in Strings #####

Emacs字符串中的非ascii字符有两种文本表示:多字节和单字节(请参阅文本表示)。

1. multibyte
2. unibyte

粗略地说，单字节字符串存储 raw bytes，而多字节字符串存储人类可读的文本。

单字节字符串中的每个字符都是一个字节，即它的值在0到255之间。

相比之下，多字节字符串中的每个字符的值可能在0到4194303之间(参见字符类型)。

在这两种情况下，大于127的字符都是非ascii字符。

您可以在字符串常量中包含非ascii字符，方法是按字面意思写它。

如果从 multibyte source(例如多字节缓冲区或字符串)或将作为多字节访问的文件中读取字符串常量，则Emacs将每个非ascii字符作为多字节字符读取，并自动将字符串转换为多字节字符串。

如果从单字节源读取字符串常量，则Emacs将非ascii字符作为单字节读取，并使字符串成为单字节。

与将字符直接写入多字节字符串不同，您可以使用转义序列将其写成字符代码。

有关转义序列的详细信息，请参阅通用转义语法。

如果您在字符串常量(即使是ASCII字符)中使用任何unicode风格的转义序列 `\uNNNN` 或 `\U00NNNNNN`， Emacs会自动假定它是多字节的。

您还可以在字符串常量中使用十六进制转义序列 (`\xn`) 和八进制转义序列 (`\n`)。

但要注意:如果字符串常量包含十六进制或八进制转义序列，并且这些转义序列都指定了单字节字符(即，小于256)，并且字符串中没有其他非ascii字符或unicode风格的转义序列，那么Emacs会自动假定它是一个单字节字符串。

也就是说，它假设字符串中出现的所有非ascii字符都是8位原始字节。

在十六进制和八进制转义序列中，转义的字符代码可能包含可变数量的数字，因此后面的第一个不是有效十六进制或八进制数字的字符终止转义序列。

如果字符串中的下一个字符可以解释为十六进制或八进制数字，则写入 ``\ ` (反斜杠和空格)以终止转义序列。

例如，`\xe0\ ` 表示一个字符，a带有重音。

字符串常量中的 `\ ` 就像反斜杠-换行符;它不会为字符串提供任何字符，但会终止前面的十六进制转义。

##### 2.4.8.3 Nonprinting Characters in Strings #####

可以在字符串常量中使用与字符字面量相同的反斜杠转义序列(但不要使用字符常量开头的问号)。

例如，您可以编写一个包含非打印字符tab和C-a的字符串，它们之间有逗号和空格，像这样: `\t`， `\C-a`。

有关字符的各种读语法的描述，请参阅字符类型及其子部分。

但是，并不是所有可以用反斜杠转义序列编写的字符在字符串中都有效。

字符串可以包含的唯一控制字符是ASCII控制字符。字符串在ASCII控制字符中不区分大小写。

正确地说，字符串不能包含 meta characters;但是当字符串被用作键序列时，有一个特殊的约定，它提供了一种在字符串中表示ASCII字符的 meta version 的方法。

如果您使用 `\M-` 语法来表示字符串常量中的元字符，则会在字符串中设置该字符的 `2**7` 位。

如果在 `define-key` 或 `lookup-key` 中使用该字符串，则该数字代码将被转换为等效的元字符。参见字符类型。

字符串不能包含具有hyper、super或alt修饰符的字符。

##### 2.4.8.4 Text Properties in Strings #####

除了字符本身之外，字符串还可以保存它所包含的字符的属性。

这使得在字符串和 buffer 之间复制文本的程序不需要特别的努力就可以复制文本的属性。

有关文本属性含义的解释，请参阅文本属性 Text Properties。

具有文本属性的字符串使用特殊的读取和打印语法:

``` Elisp
#("characters" property-data...)
```

其中属性数据由零个或多个元素组成，每组三个如下:

``` Elisp
beg and plist
```

元素beg和end都是整数，它们一起指定了字符串中的索引范围;

plist是该范围的属性列表。例如:

``` Elisp
#("foo bar" 0 3 (face bold) 3 4 nil 4 7 (face italic))
```

表示一个字符串，其文本内容为 `foo bar`，其中前三个字符的 face 属性值为粗体，后三个字符的face属性值为斜体。

(第四个字符没有文本属性，所以它的属性列表为nil。实际上没有必要提及nil作为属性列表的范围，因为在任何范围中未提及的任何字符将默认没有属性。)

#### 2.4.9 Vector Type ####

vector 是包含任意类型元素的一维数组。

访问一个向量的任何元素都需要一个常数的时间。

(在列表中，元素的访问时间与该元素到列表开头的距离成正比。)

向量的打印表示由左方括号、元素和右方括号组成。

这也是读语法。

与数字和字符串一样，向量也被视为求值的常量。

``` Elisp
[1 "two" (three)]      ; A vector of three elements.
     ⇒ [1 "two" (three)]
```

#### 2.4.10 Char-Table Type ####

字符表是由任意类型的元素组成的一维数组，由字符代码索引。

char-table具有某些额外的特性，使它们对于许多涉及将信息分配给字符代码的作业更有用——例如，一个char-table可以有一个父节点来继承，一个默认值，以及少量用于特殊目的的额外槽。

字符表还可以为整个字符集指定单个值。

字符表的打印表示形式类似于向量，只是在开头多了一个 `#^`

有关在Char-Tables上操作的特殊函数，请参阅Char-Tables。 char-tables 的用途包括:

* Case tables (see The Case Table).
* Character category tables (see Categories).
* Display tables (see Display Tables).
* Syntax tables (see Syntax Tables). 

#### 2.4.11 Bool-Vector Type ####

bool-vector是一维数组，其元素必须为t或nil。

布尔向量的打印表示形式类似于字符串，不同之处在于它以 `#&` 开头，后跟长度。

后面的字符串常量实际上将布尔向量的内容指定为位图——字符串中的每个字符包含8位，它们指定了布尔向量的下8个元素(1代表t, 0代表nil)。

字符的最低有效位对应于布尔向量中的最低下标。

``` Elisp
(make-bool-vector 3 t)
     ⇒ #&3"^G"
(make-bool-vector 3 nil)
     ⇒ #&3"^@"
```

这些结果是有意义的，因为 `C-g` 的二进制代码是111，`C-@` 是代码为0的字符。

如果长度不是8的倍数，则打印的表示将显示额外的元素，但这些额外的元素实际上没有区别。

例如，在下一个例子中，两个布尔向量是相等的，因为只使用了前3位:

``` Elisp
(equal #&3"\377" #&3"\007")
     ⇒ t
```

#### 2.4.12 Hash Table Type ####

哈希表是一种非常快速的查找表，有点像列表，因为它将键映射到相应的值，但要快得多。

哈希表的打印形式指定了它的属性和内容，如下所示:

``` Elisp
(make-hash-table)
     ⇒ #s(hash-table size 65 test eql rehash-size 1.5
                             rehash-threshold 0.8125 data ())
```

#### 2.4.13 Function Type ####

Lisp函数是可执行代码，就像其他编程语言中的函数一样。

在Lisp中，与大多数语言不同，函数也是Lisp对象。

Lisp中的非编译函数是lambda表达式:也就是说，它的第一个元素是符号lambda的列表(参见lambda表达式)。

在大多数编程语言中，没有名称的函数是不可能的。

在Lisp中，函数没有内部名称 (*intrinsic name*)。

lambda表达式可以作为函数调用，即使它没有名称;为了强调这一点，我们也称其为匿名函数(参见匿名函数 Anonymous Functions)。

在Lisp中，命名函数named function只是一个符号，它的函数单元function cell中有一个有效的函数(参见定义函数 Defining Functions)。

大多数情况下，函数的名称是在Lisp程序中的Lisp表达式中编写的。

但是，您可以在运行时构造或获取函数对象，然后使用基本函数 `funcall` 和 `apply` 调用它。参见调用函数Calling Functions。

#### 2.4.14 Macro Type ####

Lisp宏(*Lisp macro*) 是扩展Lisp语言的用户定义结构。

它被表示为一个非常类似于函数的对象，但具有不同的参数传递语义 argument-passing semantics。

Lisp宏具有列表的形式，其第一个元素是符号宏 symbol macro，其CDR是Lisp函数对象，包括lambda符号。

Lisp宏对象通常是用内置的 `defmacro` 宏定义的，但是对于Emacs来说，任何以macro开头的列表都是宏。

> 警告:Lisp macro 和键盘宏(参见键盘宏)是完全不同的东西。当我们不加限定地使用“宏”这个词时，我们指的是Lisp宏，而不是键盘宏。

有关如何编写宏的说明，请参阅 Macro。

#### 2.4.15 Primitive Function Type ####

原始函数 *Primitive Function* 是可从Lisp调用但用C编程语言编写的函数。

原始函数也称为子函数 *subrs * 或内置函数 *build-in functions*。

(*subr* 这个词来源于 子程序 *subroutine*。)

大多数原始函数在调用时都会计算所有参数。

不计算其所有参数的原始函数称为 特殊形式(参见特殊形式 Special Forms)。

对于函数的调用者来说，函数是否是原始的无关紧要。

但是，如果您试图用Lisp编写的函数重新定义原始函数，这就很重要了。

原因是原始函数可以直接从C代码中调用。

从Lisp调用重新定义的函数将使用新的定义，但从C代码调用可能仍然使用内置定义。

因此，我们不鼓励重新定义原始函数。

术语 函数*function* 指的是所有Emacs函数，无论是用Lisp还是c编写的。参见函数类型，了解用Lisp编写的函数的信息。

基本函数没有读语法，并以哈希表示法打印子例程subroutine 的名称。

``` Elisp
(symbol-function 'car)          ; Access the function cell
                                ;   of the symbol.
     ⇒ #<subr car>
(subrp (symbol-function 'car))  ; Is this a primitive function?
     ⇒ t                       ; Yes.
```

#### 2.4.16 Byte-Code Function Type ####

字节码函数对象 *Byte-code function objects* 由字节编译Lisp代码产生(参见字节编译 Byte Compilation)。

在内部，字节码函数对象很像vector;但是，当此数据类型出现在函数调用中时，求值器evaluator 会特别处理该数据类型。

参见字节码函数对象。 Byte-Code Function Objects

字节码函数对象的打印表示和读取语法类似于vector，只是在开头的'['之前加了一个 `#`。

#### 2.4.17  Record Type ####

record 很像vector。

但是，第一个元素用于保存由 type-of 返回的类型。

record 的目的是允许程序员创建具有Emacs中没有内置的新类型的对象。

有关处理 Record 的函数，请参阅Record。

#### 2.4.18 Type Descriptors ####

类型描述符 *type descriptor* 是保存有关 type 信息的 record。

record 中的 `Slot 1` 必须是一个命名该类型的符号，type-of依赖于此来返回record对象的类型。

Emacs没有使用其他类型描述 slot;它们是免费供Lisp扩展使用的。

类型描述符 的一个例子是 `cl-structure-class` 的任何实例。

#### 2.4.19 Autoload Type ####

autoload 对象是一个列表，它的第一个元素是符号autoload。

它被存储为符号的函数定义，作为实际定义的占位符。

autoload对象表示，真正的定义是在Lisp代码文件中找到的，必要时应该加载该文件。

它包含文件的名称，以及关于实际定义的一些其他信息。

加载完文件后，符号应该有一个不是自动加载对象的新函数定义。

然后调用新的定义，就好像它一开始就存在一样。

从用户的角度来看，函数调用按照预期工作，使用加载文件中的函数定义。

自动加载对象通常是用 `autoload` 函数创建的，该函数将对象存储在符号的函数单元function cell中。

有关详细信息，请参阅自动加载 Autoload。

#### 2.4.20 Finalizer Type ####

终结器对象 *finalizer object* 帮助Lisp代码在不再需要的对象之后进行清理。

终结器保存一个Lisp函数对象。

当垃圾收集 garbage collection 通过后，结束器对象变得不可访问时，Emacs调用结束器关联的函数对象。

在决定终结器是否可访问时，Emacs不计算来自终结器对象本身的引用，从而允许您使用终结器，而不必担心意外捕获对终结器对象本身的引用。

终结器中的错误被打印到 `*Messages*`。

Emacs只运行给定终结器对象的关联函数一次，即使该函数失败。

##### Function: make-finalizer function #####

创建一个将运行 function 的终结器 finalizer。

函数将在垃圾收集之后，当返回的终结器对象变得不可达时调用。

如果终结器对象只能通过对终结器对象的引用来访问，则在决定是否运行函数时，它不算作可访问对象。

函数将在每个终结器对象上运行一次。

### 2.5 Editing Types ###

前一节中的类型用于一般编程目的，并且它们中的大多数对于大多数Lisp方言都是通用的。

Emacs Lisp为与编辑相关的目的提供了几种额外的数据类型。

* Buffer Type
* Marker Type
* Window Type
* Frame Type
* Terminal Type
* Window Configuration Type
* Frame Configuration Type
* Process Type
* Thread Type
* Mutex Type
* Condition Variable Type
* Stream Type
* Keymap Type
* Overlay Type
* Font Type
* Xwidget Type

#### 2.5.1 Buffer Type ####

buffer 是一个保存可编辑文本的对象(参见缓冲区)。

大多数缓冲区保存磁盘文件的内容(参见文件)，因此它们可以被编辑，但有些缓冲区用于其他目的。

大多数缓冲区也意味着用户可以看到，因此在某个时候显示在窗口中(参见Windows)。

但是缓冲区不需要显示在任何窗口中。

每个缓冲区都有一个指定的位置，称为点(见 Position);大多数编辑命令作用于点附近当前缓冲区的内容。

在任何时候，都有一个缓冲区是当前缓冲区 *current buffer*。

缓冲区的内容与字符串非常相似，但在Emacs Lisp中，缓冲区的使用方式与字符串不同，可用的操作也不同。

例如，您可以有效地将文本插入到现有的缓冲区中，从而改变缓冲区的内容，而将文本插入到字符串中则需要连接子字符串，结果是一个全新的字符串对象。

许多标准Emacs函数操作或测试当前缓冲区中的字符;本手册有一整章专门描述这些函数(见 Text)。

与每个缓冲区相关联的其他几个数据结构:

* a local syntax table (see Syntax Tables);
* a local keymap (see Keymaps);
* a list of buffer-local variable bindings (see Buffer-Local Variables).
* overlays (see Overlays).
* text properties for the text in the buffer (see Text Properties). 

local keymap 和 variable list 包含单独覆盖全局绑定或值的项。

它们用于自定义不同缓冲区中程序的行为，而无需实际更改程序。

缓冲区可能是间接的，这意味着它共享另一个缓冲区的文本，但表示方式不同。参见间接缓冲区 Indirect Buffers。

缓冲区没有读语法。

它们以哈希表示法打印，显示缓冲区名称。

``` Elisp
(current-buffer)
     ⇒ #<buffer objects.texi>
```

#### 2.5.2 Marker Type ####

marker 表示在特定缓冲区中的位置。

因此，标记有两个组成部分:一个用于缓冲区，一个用于位置。

缓冲区文本的更改会根据需要自动重新定位位置值，以确保标记始终指向缓冲区中相同的两个字符之间。

标记没有读语法。

它们以哈希表示法打印，给出当前字符位置和缓冲区名称。

``` Elisp
(point-marker)
     ⇒ #<marker at 10779 in objects.texi>
```

有关如何测试、创建、复制和移动标记的信息，请参阅 Markers。

#### 2.5.3 Window Type ####

窗口描述了Emacs用来显示缓冲区的屏幕部分。

每个活动窗口(参见Emacs Windows的基本概念)都有一个关联的缓冲区，其内容显示在该窗口中。

相反，给定的缓冲区可能出现在一个窗口、没有窗口或多个窗口中。

窗口在屏幕上被分组为框架 frame; 每个窗口只属于一个框架。

请参见框架类型 Frame Type。

虽然许多窗口可以同时存在，但在任何时候，一个窗口被指定为选定窗口(参见选择窗口 Selecting Windows)。

当Emacs准备好执行命令时，这个窗口(通常)显示光标 Cursor。

所选窗口通常显示当前缓冲区(参见当前缓冲区 Current Buffer)，但情况并非如此。

Windows没有 read syntax。

它们以散列表示法打印，给出窗口号window numbers和正在显示的缓冲区的名称。

窗口号的存在是为了唯一地标识窗口，因为在任何给定窗口中显示的缓冲区可以频繁更改。

``` Elisp
(selected-window)
     ⇒ #<window 1 on objects.texi>
```

有关在Windows上工作的函数的描述，请参阅Windows。

#### 2.5.4 Frame Type ####

框架Frame 是包含一个或多个Emacs窗口的屏幕区域;

我们还使用术语 *frame* 来指代Emacs用来指代屏幕区域的Lisp对象。

Frame 没有读语法。它们以哈希表示法打印，给出 frame 的标题，加上它的核心地址 address in core(用于唯一标识frame)。

``` Elisp
(selected-frame)
     ⇒ #<frame emacs@psilocin.gnu.org 0xdac80>
```

有关在框架上工作的函数的描述，请参阅框架Frame。

#### 2.5.5 Terminal Type ####

终端是能够显示一个或多个Emacs Frame 的device(参见Frame)。

终端没有读语法。它们以散列表示法打印，给出终端的序数 ordinal number 及其TTY设备文件名。

``` Elisp
(get-device-terminal nil)
⇒#<terminal 1 on /dev/tty>
```

#### 2.5.6 Window Configuration Type ####

窗口配置 window configuration 在框架中存储有关窗口的位置、大小和内容的信息，因此您可以稍后重新创建相同的窗口排列。

窗口配置没有读语法;它们的打印语法看起来像 `#<window-configuration>`。

有关与窗口配置相关的几个函数的描述，请参阅窗口配置。Window Cinfiguration

#### 2.5.7 Frame Configuration Type ####

框架配置存储frame configuration type 有关所有框架中窗口的位置、大小和内容的信息。

它不是一个 原始类型primitive——它实际上是一个列表，它的CAR是帧配置frame configuration，它的CDR是一个 alist。

每个 alist 元素描述一个框架，该框架显示为该元素的CAR。

请参阅框架配置Frame Configuration，以获得与框架配置相关的几个功能的描述。

#### 2.5.8 Process Type ####

进程Process 这个词通常是指运行中的程序。

Emacs本身就运行在这种进程中。

然而，在Emacs Lisp中，进程是一个Lisp对象，它指定由Emacs进程创建的子进程 subprocess。

运行在Emacs子进程中的shell、GDB、ftp和编译器等程序扩展了Emacs的功能。

Emacs子进程接受来自Emacs的文本输入，并将文本输出返回给Emacs以供进一步操作。

Emacs还可以向子进程发送信号signals。

进程对象没有读语法。它们以哈希表示法打印，给出进程的名称:

``` Elisp
(process-list)
     ⇒ (#<process shell>)
```

有关创建、删除、返回有关进程的信息、向进程发送输入或信号以及从进程接收输出的函数的信息，请参见进程Process。

#### 2.5.9 Thread Type ####

Emacs中的一个线程表示Emacs Lisp执行的一个独立线程。

它运行自己的Lisp程序，有自己的当前缓冲区，并且可以将子进程锁定在它上面，也就是说，子进程的输出只有这个线程可以接受。

See Threads。

线程对象没有读语法。

它们以哈希表示法打印，给出线程的名称(如果已经给定了名称)或其在core中的地址:

``` Elisp
(all-threads)
    ⇒ (#<thread 0176fc40>)
```

#### 2.5.10 Mutex Type ####

互斥锁 Mutex 是一种排他性锁 exclusive lock，线程可以拥有或不拥有它，以便在它们之间同步synchronize。

See Mutexes

互斥对象 Mutex objects 没有读语法。

它们以哈希表示法打印，给出互斥锁的名称(如果已经给定了名称)或其在core中的地址:

``` Elisp
(make-mutex "my-mutex")
    ⇒ #<mutex my-mutex>
(make-mutex)
    ⇒ #<mutex 01c7e4e0>
```

#### 2.5.11 Condition Variable Type ####

条件变量 condition variable 是一种比互斥锁支持的线程同步更复杂的设备。

线程可以在条件变量上等待，当其他线程通知条件notifies the condition时被唤醒。

条件变量对象没有读语法。

它们以哈希表示法打印，给出条件变量的名称(如果已经给定了名称)或其在core中的地址:

``` Elisp
(make-condition-variable (make-mutex))
    ⇒ #<condvar 01c45ae8>
```

#### 2.5.12 Stream Type ####

stream 是一个对象，它可以用作字符的源或接收——要么提供字符作为输入，要么接受字符作为输出。

许多不同的类型都可以这样使用:标记markers、缓冲区buffer、字符串strings和函数functions。

大多数情况下，输入流input stream(字符源character sources)从键盘、缓冲区或文件中获取字符，输出流output stream(字符汇character sinks)将字符发送到缓冲区，如 `*Help*缓冲区` 或回显区 echo area。

对象 `nil` 除了它的其他含义外，还可以用作流。

它表示变量 标准输入*standart-input* 或 标准输出*standard-output* 的值。

同样，对象 `t` 作为流指定使用 minibuffer(参见Minibuffers)的输入或在echo区域(参见the echo area)的输出。

流没有特殊的打印表示或读语法，并以它们的基本类型打印。

请参阅读取和打印Lisp对象，了解与流相关的函数的描述，包括解析和打印函数。

#### 2.5.13 Keymap Type ####

keymap将用户输入的键映射到命令。

该映射控制如何执行用户的命令输入。

keymap实际上是一个列表，它的CAR是keymap符号。

有关创建键映射、处理前缀键、本地和全局键映射以及更改键绑定的信息，请参阅键映射 Keymaps。

#### 2.5.14 Overlay Type ####

覆盖overlay 指定应用于缓冲区的一部分的属性。

每个覆盖overlay 应用于缓冲区的指定范围，并包含一个属性列表(其元素是交替的属性名称和值)。

覆盖属性用于以不同的显示样式暂时显示缓冲区的部分。

覆盖层没有读语法，并以哈希表示法打印，给出缓冲区名称和位置范围。

有关如何创建和使用覆盖的信息，请参阅“覆盖” Overlays。

#### 2.5.15 Font Type ####

字体 font 指定如何在图形终端上显示文本。

实际上有三种独立的字体类型——字体对象、字体规范和字体实体——每一种都有稍微不同的属性。

* font objects
* font specs
* font entities

它们都没有读语法;它们的打印语法分别类似于

* `#<font-object>`
* `#<font-spec>`
* `#<font-entity>`

有关这些Lisp对象的描述，请参阅低级字体表示 Low-Level Font Representation。

#### 2.5.16 Xwidget Type ####

xwidget是一种特殊的显示元素，例如web浏览器，可以嵌入到缓冲区中。

显示xwidget的每个窗口也将有一个xwidget视图，在X- windows上对应于用于显示小部件的单个X窗口。

这两个对象都不是可读的;它们的打印语法分别类似于 `#<xwidget>` 和 `#<xwidget-view>`

有关xwidgets的更详细描述，请参见Embedded Native Widgets。

### Read Syntax for Circular Objects ###

为了在复杂的Lisp对象中表示 共享shared 或 循环结构circular structures，您可以使用读取器reader 构造 `#n=` 和 `#n#`。

在对象前使用 `#n=` 标记以供以后引用;随后，您可以使用 `#n#` 在另一个地方引用相同的对象。

这里n是一个整数。

例如，下面是如何创建一个列表，其中第一个元素作为第三个元素重复出现:

``` Elisp
(#1=(a) b #1#)
```

这与下面的普通语法不同

``` Elisp
((a) b (a))
```

这将导致一个列表，其第一个和第三个元素看起来很相似，但不是同一个Lisp对象。

这显示了不同之处:

``` Elisp
(prog1 nil
  (setq x '(#1=(a) b #1#)))
(eq (nth 0 x) (nth 2 x))
     ⇒ t
(setq x '((a) b (a)))
(eq (nth 0 x) (nth 2 x))
     ⇒ nil
```

您还可以使用相同的语法创建一个循环结构，该结构在其内部显示为元素。下面是一个例子:

``` Elisp
#1=(a #1#)
```

这将生成一个列表，其第二个元素是列表本身。

以下是你如何看到它真正有效的方法:

``` Elisp
(prog1 nil
  (setq x '#1=(a #1#)))
(eq x (cadr x))
     ⇒ t
```

如果将变量 `print-circle` 绑定到一个非空值，Lisp printer 可以生成这个语法来记录Lisp对象中的循环和共享结构。

参见影响输出的变量 Variables Affecting Output。

### Type Predicates ###

当调用函数时，Emacs Lisp解释器本身并不对传递给函数的实际参数执行类型检查。

它不能这样做，因为Lisp中的函数参数没有声明的数据类型，而在其他编程语言中是这样。

因此，由单个函数来测试每个实际参数是否属于该函数可以使用的类型。

所有内置函数都会在适当的时候检查其实际参数的类型，并在参数类型错误时发出错误类型参数错误信号。

例如，如果你向 `+` 传递一个它无法处理的参数，会发生以下情况:

``` Elisp
(+ 2 'a)
     error→ Wrong type argument: number-or-marker-p, a
```

如果你想让你的程序以不同的方式处理不同的类型，你必须做显式的类型检查 explicit type checking。

检查对象类型的最常用方法是调用类型谓词函数 *type predicate function*。

Emacs为每种类型都提供了类型谓词，也为类型组合提供了一些谓词。

类型谓词函数接受一个参数;如果参数属于适当的类型，则返回t，否则返回nil。

按照Lisp对谓词函数的一般约定，大多数类型谓词的名称以“p”结尾。

下面是一个使用谓词listp检查列表和symbolp检查符号的示例。

``` Elisp
(defun add-on (x)
  (cond ((symbolp x)
         ;; If X is a symbol, put it on LIST.
         (setq list (cons x list)))
        ((listp x)
         ;; If X is a list, add its elements to LIST.
         (setq list (append x list)))
        (t
         ;; We handle only symbols and lists.
         (error "Invalid argument %s in add-on" x))))
```

下面是一个预定义类型谓词的表，按字母顺序排列，其中包含对进一步信息的引用。


* `atom`: See atom.
* `arrayp`: See arrayp.
* `bignump`: See floatp.
* `bool-vector-p`: See bool-vector-p.
* `booleanp`: See booleanp.
* `bufferp`: See bufferp.
* `byte-code-function-p`: See byte-code-function-p.
* `compiled-function-p`: See compiled-function-p.
* `case-table-p`: See case-table-p.
* `char-or-string-p`: See char-or-string-p.
* `char-table-p`: See char-table-p.
* `commandp`: See commandp.
* `condition-variable-p`: See condition-variable-p.
* `consp`: See consp.
* `custom-variable-p`: See custom-variable-p.
* `fixnump`: See floatp.
* `floatp`: See floatp.
* `fontp`: See Low-Level Font Representation.
* `frame-configuration-p`: See frame-configuration-p.
* `frame-live-p`: See frame-live-p.
* `framep`: See framep.
* `functionp`: See functionp.
* `hash-table-p`: See hash-table-p.
* `integer-or-marker-p`: See integer-or-marker-p.
* `integerp`: See integerp.
* `keymapp`: See keymapp.
* `keywordp`: See Variables that Never Change.
* `listp`: See listp.
* `markerp`: See markerp.
* `mutexp`: See mutexp.
* `nlistp`: See nlistp.
* `number-or-marker-p`: See number-or-marker-p.
* `numberp`: See numberp.
* `overlayp`: See overlayp.
* `processp`: See processp.
* `recordp`: See recordp.
* `sequencep`: See sequencep.
* `string-or-null-pc`: See string-or-null-p.
* `stringp`: See stringp.
* `subrp`: See subrp.
* `symbolp`: See symbolp.
* `syntax-table-p`: See syntax-table-p.
* `threadp`: See threadp.
* `vectorp`: See vectorp.
* `wholenump`: See wholenump.
* `window-configuration-p`: See window-configuration-p.
* `window-live-p`: See window-live-p.
* `windowp`: See windowp. 

检查对象类型的最通用方法是调用函数 `type-of`。

回想一下，每个对象都属于且仅属于一个基本类型;

type-of告诉您是哪一种(请参阅Lisp数据类型)。

但是type-of对 非原始类型 non-primitive 一无所知。

在大多数情况下，使用 类型谓词type predicates 比使用type-of更方便。

#### Function: type-of object ####

这个函数返回一个命名对象基本类型的符号。

该值是以下符号之一:bool-vector、buffer、char-table、compiled-function、condition-variable、cons、finalizer、float、font-entity、font-object、font-spec、frame、hash-table、integer、marker、mutex、overlay、process、string、subr、symbol、thread、vector、window或window-configuration。

但是，如果object是一条记录，则返回其第一个槽指定的类型; Records。

``` Elisp
(type-of 1)
     ⇒ integer
(type-of 'nil)
     ⇒ symbol
(type-of '())    ; () is nil.
     ⇒ symbol
(type-of '(x))
     ⇒ cons
(type-of (record 'foo))
     ⇒ foo
```

### Equality Predicates ###

这里我们描述了测试两个对象是否相等的函数。

其他函数测试特定类型对象(如字符串)之间的内容是否相等。

对于这些谓词，请参见描述数据类型的相应章节。

#### Function: eq object1 object2 ####

如果object1和object2是相同的对象，这个函数返回t，否则返回nil。

如果object1和object2是具有相同名称的符号，则它们通常是相同的对象，但请参见创建和修改符号以了解异常 Creating and Interning Symbols。

对于其他非数字类型(例如，列表、向量、字符串)，具有相同内容或元素的两个参数彼此 **不一定相等**: 只有当它们是相同对象时，它们才相等，这意味着一个对象内容的变化将反映在另一个对象内容的相同变化上。

如果object1和object2是具有不同类型或值的数字，那么它们不可能是相同的对象，并且eq返回nil。

如果它们是具有相同值的fixnum，那么它们是相同的对象，eq返回t。

如果它们是单独计算的，但恰好具有相同的值和相同的非fixnum数字类型，那么它们可能是也可能不是相同的对象，并且eq返回 t 或 nil 取决于Lisp解释器创建的是一个对象还是两个对象。

``` Elisp
(eq 'foo 'foo)
     ⇒ t

(eq ?A ?A)
     ⇒ t

(eq 3.0 3.0)
     ⇒ t or nil
;; Equal floats may or may not be the same object.

(eq (make-string 3 ?A) (make-string 3 ?A))
     ⇒ nil

(eq "asdf" "asdf")
     ⇒ t or nil
;; Equal string constants or may not be the same object.

(eq '(1 (2 (3))) '(1 (2 (3))))
     ⇒ nil

(setq foo '(1 (2 (3))))
     ⇒ (1 (2 (3)))
(eq foo foo)
     ⇒ t
(eq foo '(1 (2 (3))))
     ⇒ nil

(eq [(1 2) 3] [(1 2) 3])
     ⇒ nil

(eq (point-marker) (point-marker))
     ⇒ nil
```

`make-symbol` 函数返回一个非内部符号，与在Lisp表达式中写入名称时使用的符号不同。

具有相同名称的不同符号不相等。

参见创建和修改符号。

``` Elisp
(eq (make-symbol "foo") 'foo)
     ⇒ nil
```

Emacs Lisp 字节编译器可以将相同的文字对象(如文字字符串)折叠为对相同对象的引用，其结果是字节编译的代码将比较eq等对象，而同一代码的解释版本则不会。

因此，您的代码永远不应该依赖于具有相同文字内容的对象，无论是eq还是非eq，它应该使用比较对象内容的函数，如equal，如下所述。

类似地，你的代码不应该修改文字对象(例如，将文本属性放在文字字符串上)，因为这样做可能会影响具有相同内容的其他文字对象，如果字节编译器将它们折叠。

#### Function: equal object1 object2 ####

如果object1和object2具有相等的组件components，则此函数返回t，否则返回nil。

eq测试其参数是否为相同对象，而equal则查看非相同参数内部的元素或内容是否相同。

所以，如果两个对象相等，它们是相等的，但反过来并不总是正确的。

``` Elisp
(equal 'foo 'foo)
     ⇒ t

(equal 456 456)
     ⇒ t

(equal "asdf" "asdf")
     ⇒ t

(eq "asdf" "asdf")
     ⇒ nil

(equal '(1 (2 (3))) '(1 (2 (3))))
     ⇒ t

(eq '(1 (2 (3))) '(1 (2 (3))))
     ⇒ nil

(equal [(1 2) 3] [(1 2) 3])
     ⇒ t

(eq [(1 2) 3] [(1 2) 3])
     ⇒ nil

(equal (point-marker) (point-marker))
     ⇒ t

(eq (point-marker) (point-marker))
     ⇒ nil
```

字符串的比较区分大小写，但不考虑文本属性——它只比较字符串中的字符。

参见文本属性。

使用 `equal-include-properties` 也可以比较文本属性。

出于技术原因，单字节字符串和多字节字符串当且仅当它们包含相同的字符代码序列并且所有这些代码都在0到127 (ASCII)的范围内时相等。

``` Elisp
(equal "asdf" "ASDF")
     ⇒ nil
```

`equal` 函数递归地比较对象的内容，如果它们是整数、字符串、标记、向量、布尔向量、字节码函数对象、字符表、记录或字体对象。

其他对象只有在相等时才被认为相等。

例如，两个不同的缓冲区永远不会被认为相等，即使它们的文本内容相同。

对于equal，相等是递归定义的;

例如，给定两个 cons cells x和y，`(equal x y)` 返回 t 当且仅当下面的表达式都返回t:

``` Elisp
(equal (car x) (car y))
(equal (cdr x) (cdr y))
```

因此，比较循环列表可能会导致导致错误的深度递归，这可能会导致违反直觉的行为，例如(equal a b)返回t，而(equal b a) 引发错误。

#### Function: equal-including-properties object1 object2 ####

此函数在所有情况下都表现为相等，但也要求两个字符串相等，它们具有相同的文本属性。

``` Elisp
(equal "asdf" (propertize "asdf" 'asdf t))
     ⇒ t

(equal-including-properties "asdf"
                            (propertize "asdf" 'asdf t))
     ⇒ nil
```

### Mutability ###

一些Lisp对象不应该改变。

例如，Lisp表达式“aaa”产生一个字符串，但您不应该更改其内容。

有些对象是不能改变的;例如，虽然您可以通过计算1来创建一个新数字，但Lisp不提供更改现有数字值的操作。

其他Lisp对象是可变的:通过涉及 副作用side effects 的破坏性操作destructive operations 来改变它们的值是安全的。

例如，可以通过将标记移动到指向其他地方来更改现有标记。

尽管数字永远不会改变，而且所有标记都是可变的，但有些类型的成员有些是可变的，有些是不可变的。

这些类型包括 conses、vector和string。

例如，虽然 `cons` 和 `(symbol-name 'cons)` 都产生不应更改的字符串，但 `(copy-sequence "cons")` 和 `(make-string 3 ?a)` 都产生可变字符串，可以通过稍后调用 `aset` 来更改。

如果一个可变对象是被求值表达式的一部分，那么它就不再是可变的。

例如:

``` Elisp
(let* ((x (list 0.5))
       (y (eval (list 'quote x))))
  (setcar x 1.5) ;; The program should not do this.
  y)
```

虽然列表(0.5)在创建时是可变的，但它不应该通过 `setcar` 进行更改，因为它是给 `eval` 的。

相反的情况不会发生: 不应该更改的对象之后永远不会变。

如果一个程序试图改变不应该改变的对象，结果行为是未定义的: Lisp解释器可能会发出错误信号，或者它可能会崩溃或以其他方式表现出不可预测的行为

当类似的常量作为程序的一部分出现时，Lisp解释器可以通过重用现有的常量或它们的组件来节省时间或空间。

例如，`(eq "abc" "abc")` 如果解释器只创建字符串字面值"abc"的一个实例，则返回t，如果创建两个实例，则返回nil。

Lisp程序的编写应该使它们不管是否使用这种优化都能正常工作。

## 3 Numbers ##

[Number](https://www.gnu.org/software/emacs/manual/html_node/elisp/Numbers.html)

GNU Emacs支持两种数字数据类型:

1. 整数 *integer*
2. 浮点数 *floating-point numbers*

整数是指−3、0、7、13、511等整数。

浮点数是指带有小数部分的数字，如−4.5、0.0和2.71828。

它们也可以用指数表示法表示: `1.5e2` 与 `150.0` 相同;这里，e2代表10的2次方，再乘以1.5。

整数计算是精确的。

浮点计算通常涉及舍入误差，因为这些数字具有固定的精度。

### 3.1 Integer Basics ###

Lisp阅读器将整数读取为具有可选初始符号和可选最后句号的非空十进制数字序列。

``` Elisp
 1               ; The integer 1.
 1.              ; The integer 1.
+1               ; Also the integer 1.
-1               ; The integer −1.
 0               ; The integer 0.
-0               ; The integer 0.
```

非10进制整数的语法由 `#` 后跟一个基数指示*radix indication*，后跟一个或多个数字组成。

基数表示为 `b` 表示二进制， `o` 表示八进制，`x` 表示十六进制，`radixr` 表示dd radix基数。

因此，`#binteger` 读取二进制形式的整数，`#radixrinteger` 读取radix进制的整数。

允许的基数取值范围是2 ~ 36，允许的数字是0 ~ 9、A ~ Z中的第一个基数字符。

忽略字母大小写，没有起始符号或最后句号。

例如:

``` Elisp
#b101100 ⇒ 44
#o54 ⇒ 44
#x2c ⇒ 44
#24r1k ⇒ 44
```

要理解各种函数如何处理整数，特别是位运算符(参见整数的位运算符)，查看二进制形式的数字通常会有所帮助。

在二进制中，十进制整数5看起来像这样:

``` Elisp
...000101
```

(省略号 `...` 表示与前导位匹配的概念上的无限位数;

这里是无限个0位。后面的例子也使用这个 `...` 符号。)

整数−1看起来像这样:

``` Elisp
...111111
```

−1表示为全1。(这被称为2的补码表示法。)

−1减去4，得到负整数−5。在二进制中，十进制整数4是100。因此，−5看起来像这样:

``` Elisp
...111011
```

本章中描述的许多函数接受 marker 作为参数来代替数字。(见 Markers。)

由于这些函数的实际实参可以是数字或标记，因此我们通常将这些实参命名为number-or-marker。

当参数值是一个 marker 时，使用它的 position，而忽略它的 buffer。

在Emacs Lisp中，文本字符由整数表示。

任何介于0和 `(max-char)` 之间的整数(包括)都被认为是有效的字符。

参见字符代码 Character Codes。

Emacs Lisp中的整数不限于机器字长。

然而，在底层，有两种整数:

1. 较小的，称为 *fixnums*

2. 较大的，称为 *bignums*

虽然Emacs Lisp代码通常不应该依赖于整数是固定数还是大位数，但旧的Emacs版本只支持固定数，Emacs中的一些函数仍然只接受固定数，并且旧的Emacs Lisp代码在给定大位数时可能会遇到问题。

例如，虽然旧的Emacs Lisp代码可以安全地将整数与 `eq` 进行数值相等的比较，但双位数的存在意味着现在应该使用 `eql` 和 `=` 这样的相等谓词来比较整数。

二进制数的取值范围受到主存容量、机器特征(如用于表示二进制数指数的字的大小)以及 `integer-width` 变量的限制。

这些限制通常比固定物的限制宽松得多。

bignum 在数值上永远不等于 fixnum;

Emacs总是将固定数范围内的整数表示为固定数，而不是大整数。

fixnum 的取值范围取决于机器。

最小范围为−536,870,912至536,870,911(30位;即- 2**29至2**29 - 1)，但许多机器提供更宽的范围。

#### 变量: `most-positive-fixnum` ####

这个变量的值是Emacs Lisp可以处理的最大的“小”整数。

典型值为32位平台为2**29−1,64位平台为2**61−1。

#### 变量: `most-negative-fixnum` ####

这个变量的值是Emacs Lisp可以处理的最小的“小”整数。

它是负的。

典型值为- 2**29(32位)和- 2**61(64位)。

#### 变量: `integer-width` ####

该变量的值是一个非负整数，用于控制Emacs在计算大整数时是否发出范围错误信号。

绝对值小于 `2**n` 的整数，其中n为该变量的值，不表示范围错误。

尝试创建更大的整数通常表示 range error，尽管如果可以便宜地创建更大的整数可能没有信号。

如果计算创建了巨大的整数，将该变量设置为较大的数字可能会代价高昂。

### 3.2 Floating-Point Basics ###

#### 浮点数基础知识 ####

[Floating-PointBasics](https://www.gnu.org/software/emacs/manual/html_node/elisp/Float-Basics.html)

浮点数在表示非整数时很有用。

浮点数的范围与您正在使用的机器上的C数据类型double的范围相同。

在Emacs支持的几乎所有计算机上，这是IEEE binary64浮点格式，由IEEE Std 754-2019标准化，并在David Goldberg的论文“每个计算机科学家都应该知道关于浮点算术的什么”中进一步讨论。

在现代平台上，浮点运算严格遵循IEEE-754标准;然而，在某些系统上，特别是32位x86，结果并不总是正确的。

在一些旧的计算机系统上，Emacs可能不使用IEEE浮点数。

我们知道有一个这样的系统，Emacs可以正常运行，但不遵循IEEE-754:使用GCC 10.4.0运行NetBSD的VAX，其中使用了VAX ' D_Floating '格式。

源自IBM System/370的大型机及其XL/C编译器也能够利用十六进制浮点格式，但是Emacs还没有在这样的配置下构建。

浮点数的读语法要么需要小数点，要么需要指数，要么两者都需要。

可选的符号 `(+` 或 `-)` 在数字及其指数前面。

例如, `1500.0`, `+15e2`, `15.0e+2`, `+1500000e-3`, 是浮点数 `1500` 的五种写法。它们都是等价的。

与Common Lisp一样，Emacs Lisp要求在没有指数的浮点数中，小数点后至少有一位数字; `1500.` 是整数，而不是浮点数。

对于像 `=` 这样的数值比较，Emacs Lisp将 `-0.0` 视为数值上等于普通零。

这遵循IEEE浮点标准，该标准规定 `-0.0` 和 `0.0` 在数字上是相等的，即使其他操作可以区分它们。

IEEE浮点标准支持正无穷大和负无穷大作为浮点值。

它还提供了一个名为 `NaN` 的值类，即 *not a number*; 数值函数在没有正确答案的情况下返回这样的值。

例如，`(/ 0.0 0.0)` 返回一个NaN。

NaN在数值上永远不等于任何值，甚至不等于它本身。

nan带有一个符号和一个有效数，当两个nan的符号sign和有效数significands一致时，非数值函数将其视为相等。

nan的有效位数与机器相关，其字符串表示中的数字也是如此。

在不使用IEEE浮点运算的系统上不能使用nan;

例如，如果在VAX上使用NaN的读取语法，则读取器会发出错误信号。

当涉及nan和带符号零时，像 `eql`、`equal`、`sxhash-eql`、`sxhash-equal` 和 `gethash` 这样的非数字函数决定值是否不可区分，而不是它们是否在数字上相等。

例如，当x和y是相同的NaN时，`(equal x y)` 返回t，而 `(= x y)` 使用数值比较并返回nil;

相反，`(= 0.0 -0.0)` 返回nil，而 `(= 0.0 -0.0)` 返回t。

下面是这些特殊浮点数的读取语法:

#### 无穷大的语法 `infinity` ####

* `1.0e+INF`
* `-1.0e+INF`

#### NAN的语法 `not-a-number` ####

* `0.0e+NAN`
* `-0.0e+NAN`

#### 函数: `isnan x` ####

如果该谓词的浮点参数是NaN，则返回t，否则返回nil。

#### 函数: `frexp x` ####

这个函数返回一个cons cell `(s . e)`，其中 s 和 e 分别是浮点数x的有效位数和指数。

如果 x 是有限的，则 s 是介于 0.5(含) 和 1.0(不含) 之间的浮点数，e是整数，`x = s*2**e`。

如果x是零或无穷大，那么s和x是一样的。

如果x是NaN，那么s也是NaN。

如果x = 0，那么e = 0。

#### 函数: `ldexp s e` ####

给定一个有效的数字 s 和一个整数指数 e，这个函数返回浮点数 `s*2**e`。

#### 函数: `copysign x1 x2` ####

这个函数将 x2 的符号复制到 x1 的值，并返回结果。

x1 和 x2 必须是浮点数。

#### 函数: `logb x` ####

此函数返回 x 的二进制指数。

更准确地说，如果 x 是有限且非零的，则该值为以 2 为底的 `|x|` 的对数，并舍入为整数。

如果x为零或无穷大，则值为无穷大;如果x是NaN，则该值为NaN。

``` Elisp
(logb 10)
;     ⇒ 3
(logb 10.0e20)
;     ⇒ 69
(logb 0)
;     ⇒ -1.0e+INF
```

### 3.3 Type Predicates for Numbers ###

本节中的函数对数字或特定类型的数字进行测试。

函数 `integerp` 和 `floatp` 可以接受任何类型的Lisp对象作为参数(否则它们就没有多大用处了)，但是 `zerop` 谓词需要一个数字作为参数。

另见 *Predicates on Markers* 中的 `integer-or-marker-p` 和 `number-or-marker-p`

#### 函数: `bignump object` ####

该谓词测试其参数是否为大整数，如果是则返回t，否则返回nil。

与小整数不同，大整数可以是 `=` 或 `eql`，即使它们不是 `eq`。

#### 函数: `fixnump object` ####

该谓词测试其参数是否为小整数，如果是则返回t，否则返回nil。

小整数可以用eq进行比较。

#### 函数: `floatp object` ####

该谓词测试其参数是否为浮点数，如果是则返回t，否则返回nil。

#### 函数: `integerp object` ####

该谓词测试其参数是否为整数，如果是则返回t，否则返回nil。

#### 函数: `numberp object` ####

该谓词测试其参数是否为数字(整数或浮点数)，如果是则返回t，否则返回nil。

#### 函数: `natnump object` ####

该谓词(其名称来自短语“自然数nature number”)测试其参数是否为非负整数，如果是则返回t，否则返回nil。

0被认为是非负的。

`wholenump` 是 `natnump` 的同义词synonym。

#### 函数: zerop number ####

该谓词测试其参数是否为零，如果为零则返回t，否则返回nil。

参数必须是一个 **数字number**。

`(zerop x)` 等价于 `(= x 0)`。

### 3.4 Comparison of Numbers ###

要测试数字是否相等，通常应该使用 `=` 代替非数字比较谓词，如 `eq`、`eql` 和 `equal`。

不同的浮点型和大整数型对象可以在数字上相等。

如果你用eq来比较它们，你要测试它们是否为同一个对象;

如果使用eql或equal，则测试它们的值是否不可区分。

相反，`=` 使用数值比较，有时当非数值比较将返回nil时返回t，反之亦然。

参见浮点基础 *Floating-Point Basics*。

在Emacs Lisp中，如果两个 *fixnums* 数值相等，则它们是相同的Lisp对象。

也就是说，`eq` 在 *fixnums* 上等价于 `=`。

有时使用 `eq` 比较未知值与固定数是很方便的，因为如果未知值不是数字，eq不会报告错误——它接受任何类型的参数。

相反，如果参数不是数字或标记，则 `=` 表示错误。

但是，如果可以的话，使用 = 是更好的编程实践，即使是比较整数也是如此。

有时用eql或equal来比较数字是有用的，如果两个数字具有相同的数据类型(都是整数，或者都是浮点数)和相同的值，则将它们视为相等。

相比之下，= 可以将整数和浮点数视为相等。

参见等式谓词Equality Predicates 。

还有一个问题:因为浮点运算不是精确的，所以检查浮点值是否相等通常不是一个好主意。

通常最好是测试近似相等。

这里有一个函数可以做到这一点:

``` Elisp
(defvar fuzz-factor 1.0e-6)
(defun approx-equal (x y)
  (or (= x y)
      (< (/ (abs (- x y))
            (max (abs x) (abs y)))
         fuzz-factor)))
```

#### 函数: `= number-or-marker &rest number-or-markers` ####

此函数测试其所有参数是否在数值上相等，如果相等则返回t，否则返回nil。

#### 函数: `eql value1 value2` ####

这个函数的作用类似于 eq，只是两个参数都是数字。

它通过类型和数值比较数字，因此 `(eql 1.0 1)` 返回 nil，但 `(eql 1.0 1.0)` 和 `(eql 1 1)` 都返回t。

这可以用于比较大整数和小整数。

具有相同符号、指数和分数的浮点值是相等的。

这与数值比较不同: `(eql 0.0 -0.0)` 返回nil， `(eql 0.0e+NaN 0.0e+NaN)` 返回t，而 `=` 则相反。

#### 函数: 不等于 `/= number-or-marker1 number-or-marker2` ####

此函数测试其参数在数值上是否相等，如果不相等则返回t，如果相等则返回nil。

和 `=` 刚好相反

#### 函数: `< number-or-marker &rest number-or-markers` ####

该函数测试每个实参是否严格小于下一个实参。

如果是，返回t，否则返回nil。

#### 函数: `<= number-or-marker &rest number-or-markers` ####

此函数测试每个参数是否小于或等于后面的参数。

如果是，返回t，否则返回nil。

#### 函数: `> number-or-marker &rest number-or-markers` ####

该函数测试每个参数是否严格大于下一个参数。

如果是，返回t，否则返回nil。

#### 函数: `>= number-or-marker &rest number-or-markers` ####

此函数测试每个参数是否大于或等于后面的参数。

如果是，返回t，否则返回nil。

#### 函数: `max number-or-marker &rest numbers-or-markers` ####

这个函数返回最大的参数。

``` Elisp
(max 20)
;     ⇒ 20
(max 1 2.5)
;     ⇒ 2.5
(max 1 3 2.5)
;     ⇒ 3
```

#### 函数: `min number-or-marker &rest numbers-or-markers` ####

这个函数返回最小的参数。

``` Elisp
(min -4 1)
;     ⇒ -4
```

#### 函数: `abs number` ####

这个函数返回number的绝对值。

### 3.5 Numeric Conversions ###

要将整数转换为浮点数，请使用 `float` 函数。

[Numeric Conversions](https://www.gnu.org/software/emacs/manual/html_node/elisp/Numeric-Conversions.html)

#### 函数: `float number` ####

这将返回转换为浮点数的数字。

如果number已经是浮点数，float返回它不变。

有四个函数可以将浮点数转换为整数;它们的舍入方式不同。

它们都接受一个参数 `number` 和一个可选的参数除数 `divisor`。

两个参数都可以是整数或浮点数。

除数也可以是nil。

如果divisor为nil或省略，这些函数将number转换为整数，如果它已经是整数，则返回它不变。

如果divisor非nil，则用number除以divisor并将结果转换为整数。

如果divisor为零(无论是整数还是浮点数)，Emacs都会发出算术错误的信号。

#### 函数: `truncate number &optional divisor` ####

这将返回number，通过向零舍入将其转换为整数。

``` Elisp
(truncate 1.2)
;     ⇒ 1
(truncate 1.7)
;     ⇒ 1
(truncate -1.2)
;     ⇒ -1
(truncate -1.7)
;     ⇒ -1
```

#### 函数: `floor number &optional divisor` ####

这将返回number，通过向下舍入(趋向负无穷)将其转换为整数。

如果指定了divisor，则使用与mod对应的除法操作，向下舍入。

``` Elisp
(floor 1.2)
;     ⇒ 1
(floor 1.7)
;     ⇒ 1
(floor -1.2)
;     ⇒ -2
(floor -1.7)
;     ⇒ -2
(floor 5.99 3)
;     ⇒ 1
```

#### 函数: `ceiling number &optional divisor` ####

这将返回number，通过向上舍入将其转换为整数(朝向正无穷大)。

``` Elisp
(ceiling 1.2)
;     ⇒ 2
(ceiling 1.7)
;     ⇒ 2
(ceiling -1.2)
;     ⇒ -1
(ceiling -1.7)
;     ⇒ -1
```

#### 函数: `round number &optional divisor` ####

这将返回number，通过向最接近的整数四舍五入将其转换为整数。

舍入两个整数之间等距的值将返回偶数。

``` Elisp
(round 1.2)
;     ⇒ 1
(round 1.7)
 ;    ⇒ 2
(round -1.2)
;     ⇒ -1
(round -1.7)
;     ⇒ -2
```

### 3.6 Arithmetic Operations 算术运算 ###

Emacs Lisp提供了传统的四种算术运算(加、减、乘、除)，以及余数和求模函数，以及加减1的函数。

除了 % 之外，这些函数都接受整数和浮点参数，如果任何参数是浮点数，则返回一个浮点数。

#### 函数: `1+ number-or-marker` ####

这个函数返回数字或标记加1。例如,

``` Elisp
(setq foo 4)
;     ⇒ 4
(1+ foo)
;     ⇒ 5
```

这个函数与c++操作符不同: **它不增加变量的值**

它只是计算一个和。

因此，如果我们继续，

``` Elisp
foo
;     ⇒ 4
```

如果你想增加变量，你必须使用 `setq`，像这样:

``` Elisp
(setq foo (1+ foo))
;     ⇒ 5
```

#### 函数: `1- number-or-marker` ####

这个函数返回数字或标记 - 1。

#### 函数: `+ &rest number-or-markers` ####

这个函数将其参数相加。当没有给定参数时，+返回0。

``` Elisp
(+)
;     ⇒ 0
(+ 1)
;     ⇒ 1
(+ 1 2 3 4)
;     ⇒ 10
```

#### 函数: `- &optional number-or-marker &rest more-numbers-or-markers` ####

`-` 函数有两个用途:相反数和减法。

当 `-` 只有一个参数时，值是参数的负数。

当有多个参数时，`-` 从number-or-marker中累计减去多个number-or-marker。

如果没有参数，结果为0。

``` Elisp
(- 10 1 2 3 4)
;     ⇒ 0
(- 10)
;     ⇒ -10
(-)
;     ⇒ 0
```

#### 函数: `* &rest numbers-or-markers` ####

该函数将其参数相乘，并返回乘积。

当没有参数时，*返回1。

``` Elisp
(*)
;     ⇒ 1
(* 1)
;     ⇒ 1
(* 1 2 3 4)
;     ⇒ 24
```

#### 函数: `/ number &rest divisors` ####

对于一个或多个除数，此函数依次将数除以除数中的每个除数，并返回商。

如果没有除数，这个函数返回 `1/number`，即number的 **乘法倒数**。

每个参数可以是一个数字或一个标记。

如果所有参数都是整数，则结果是一个 **整数**，在每次除法后将商舍入到零。

``` Elisp


(/ 6 2)
;     ⇒ 3

(/ 5 2)
;     ⇒ 2

(/ 5.0 2)
;     ⇒ 2.5

(/ 5 2.0)
;     ⇒ 2.5

(/ 5.0 2.0)
;     ⇒ 2.5

(/ 4.0)
;     ⇒ 0.25

(/ 4)
;     ⇒ 0

(/ 25 3 2)
;     ⇒ 4

(/ -17 6)
;     ⇒ -2
```

如果将一个整数除以整数0,Emacs将发出一个算术错误(请参阅错误)。

在使用IEEE-754浮点数的系统上，非零数除以零的浮点除法产生正无穷大或负无穷大(参见浮点基础);否则，像往常一样发出 算术错误信号 `arith-error`。

#### 函数: `% dividend divisor` ####

这个函数返回被除数除以除数后的整数余数。

参数必须是整数或标记。

对于任意两个整数，当除数非零时，总是等于除数:

``` Elisp
(+ (% dividend divisor)
   (* (/ dividend divisor) divisor))
```

``` Elisp
(% 9 4)
;     ⇒ 1
(% -9 4)
;     ⇒ -1
(% 9 -4)
;     ⇒ 1
(% -9 -4)
;     ⇒ -1
```

#### 函数: `mod dividend divisor` ####

这个函数返回被除数模数的值;

换句话说，被除数除以除数后的余数，但与除数的符号相同。参数必须是数字或标记。

与 `%` 不同， **mod允许使用浮点参数**;

它将商向下舍入(趋于负无穷)为整数，并使用该商计算余数。

如果divisor为零，如果两个参数都是整数，则mod表示算术错误错误 `arith-error`，否则返回NaN。

``` Elisp
(mod 9 4)
;     ⇒ 1

(mod -9 4)
;     ⇒ 3

(mod 9 -4)
;     ⇒ -3

(mod -9 -4)
;     ⇒ -1

(mod 5.5 2.5)
;     ⇒ .5

```

对于任何两个数字，除数和除数总是等于除数，如果其中一个参数是浮点数，则会产生舍入误差;如果除数是整数，除数是0，则会产生算术误差。

``` Elisp
(+ (mod dividend divisor)
   (* (floor dividend divisor) divisor))
```

有关 `floor`，请参见数字转换。

### 3.7 Rounding Operations ###

函数`ffloor`、`fceiling`、`fround`和`ftruncate`接受一个浮点参数，并 **返回一个浮点结果**，其值是附近的整数。

`ffloor`返回下面最接近的整数; `fceiling`，上面最接近的整数;`ftruncate`，趋近于0方向上最接近的整数; `fround` 求出最接近的整数。

#### 函数: `ffloor float` ####

此函数将float舍入为下一个更高的整数值，并将该值作为浮点数返回。

#### 函数: `fceiling float` ####

此函数将float舍入为下一个更高的整数值，并将该值作为浮点数返回。

#### 函数: `ftruncate float` ####

这个函数将float向零舍入为整数值，并将该值作为浮点数返回。

#### 函数: `fround float` ####

此函数将float舍入到最接近的整数值，并将该值作为浮点数返回。舍入两个整数之间等距的值将返回偶数。

### 3.8 Bitwise Operations on Integers ###

在计算机中，整数被表示为二进制数，即一串位(0或1的数字)。

从概念上讲，位序列在左边是无限的，最高有效位要么全为零，要么全为一。

按位操作作用于这样一个序列的单个位。

例如，移动将整个序列向左或向右移动一个或多个位置，从而再现移动后的相同模式。

Emacs Lisp中的按位操作仅适用于整数。

#### 函数: `ash integer1 count` ####

ash(算术移位 *arithmetic shift*) 将integer1中的位移到左边的计数位，如果count为负则移到右边。

左移在右侧引入零位; 右移丢弃最右边的位。

作为一个整数操作，ash 将 `integer1` 乘以 `2**count`，然后通过向下舍入将结果转换为一个整数，直到负无穷。

这里有一些 `ash` 的例子，它们将一种位的模式向左和向右移动一个位置。

这些例子只显示了二进制模式的低阶位;前导位都与显示的最高位一致。

正如你所看到的，向左移动一个等于乘以2，而向右移动一个等于除以2然后向负无穷四舍五入。

``` Elisp


(ash 7 1) ⇒ 14
;; Decimal 7 becomes decimal 14.
…000111
     ⇒
…001110


(ash 7 -1) ⇒ 3
…000111
     ⇒
…000011


(ash -7 1) ⇒ -14
…111001
     ⇒
…110010


(ash -7 -1) ⇒ -4
…111001
     ⇒
…111100

```

以下是向左或向右移动两位的例子:

``` Elisp
                  ;         binary values
(ash 5 2)         ;   5  =  …000101
     ⇒ 20         ;      =  …010100
(ash -5 2)        ;  -5  =  …111011
     ⇒ -20        ;      =  …101100

(ash 5 -2)
     ⇒ 1          ;      =  …000001

(ash -5 -2)
     ⇒ -2         ;      =  …111110
```

#### 函数: `lsh integer1 count` ####

Lsh是逻辑移位(*logical shift*)的缩写，它将`integer1`中的位移到左边的计数位，如果count为负，则移到右边，将空出的位变为零。

如果count为负，则integer1必须是一个fixnum或一个正的 bignum，并且lsh在移动之前减去两次最负的固定数，从而将负的固定数视为无符号固定数，从而产生非负的结果。

这种古怪的行为可以追溯到Emacs只支持fixnums的时候;现在 `ash` 是更好的选择。

由于除了integer1和count1都为负数之外，lsh的行为与ash类似，因此下面的示例主要关注这些异常情况。

这些例子假设固定值为30位。

``` Elisp
                 ;      binary values
(ash -7 -1)      ; -7 = …111111111111111111111111111001
     ⇒ -4        ;    = …111111111111111111111111111100
(lsh -7 -1)
     ⇒ 536870908 ;    = …011111111111111111111111111100

(ash -5 -2)      ; -5 = …111111111111111111111111111011
     ⇒ -2        ;    = …111111111111111111111111111110
(lsh -5 -2)
     ⇒ 268435454 ;    = …001111111111111111111111111110

```

#### 函数: `logand &rest ints-or-markers` ####

此函数返回参数的按位与:当且仅当所有参数的第n位为1时，结果的第n位为1。

例如，使用4位二进制数，13和12的位与运算是12:1101和1100的组合得到1100。

在这两个二进制数中，最左边的两位都是1，因此返回值的最左边的两位都是1。

但是，对于最右边的两位，至少在一个参数中每个都是0，因此返回值的最右边的两位都是0。

因此,

``` Elisp
(logand 13 12)
     ⇒ 12
```

如果logand没有传递任何参数，则返回值为−1。

这个数字是logand的单位元素，因为它的二进制表示完全由1组成。

如果logand只传递一个参数，它将返回该参数。

``` Elisp
                   ;        binary values

(logand 14 13)     ; 14  =  …001110
                   ; 13  =  …001101
     ⇒ 12         ; 12  =  …001100

(logand 14 13 4)   ; 14  =  …001110
                   ; 13  =  …001101
                   ;  4  =  …000100
     ⇒ 4          ;  4  =  …000100


(logand)
     ⇒ -1         ; -1  =  …111111

```

#### 函数: `logior &rest ints-or-markers` ####

此函数返回其参数的按位或:当且仅当至少一个参数的第n位为1时，结果的第n位为1。

如果没有参数，结果为0，这是此操作的标识元素。

如果只向logior传递一个参数，它将返回该参数。

``` Elisp
                   ;        binary values

(logior 12 5)      ; 12  =  …001100
                   ;  5  =  …000101
     ⇒ 13         ; 13  =  …001101

(logior 12 5 7)    ; 12  =  …001100
                   ;  5  =  …000101
                   ;  7  =  …000111
     ⇒ 15         ; 15  =  …001111
```

#### 函数: `logxor &rest ints-or-markers` ####

此函数返回其参数的按位异或:当且仅当第n位在奇数个参数中为1时，结果的第n位为1。

如果没有参数，结果为0，这是此操作的标识元素。

如果只向logxor传递一个参数，它将返回该参数。

``` Elisp
                   ;        binary values

(logxor 12 5)      ; 12  =  …001100
                   ;  5  =  …000101
     ⇒ 9          ;  9  =  …001001

(logxor 12 5 7)    ; 12  =  …001100
                   ;  5  =  …000101
                   ;  7  =  …000111
     ⇒ 14         ; 14  =  …001110
```

#### 函数: `lognot integer` ####

此函数返回其参数的按位补码:当且仅当第n位为整数0时，结果中的第n位为1，反之亦然。

结果等于 `− 1 − integer`。

``` Elisp
(lognot 5)
     ⇒ -6
;;  5  =  …000101
;; becomes
;; -6  =  …111010
```

#### 函数: `logcount integer` ####

这个函数返回整数的 **汉明权重** *Hamming weight*:整数的二进制表示中1的个数。

如果integer为负数，则返回其二进制补码表示中0位的个数。

结果总是非负的。

``` Elisp
(logcount 43)     ;  43 = …000101011
     ⇒ 4
(logcount -43)    ; -43 = …111010101
     ⇒ 3
```

### 3.9 Standard Mathematical Functions ###

这些数学函数允许整数和浮点数作为参数。


#### 函数: `sin arg` ####

#### 函数: `cos arg` ####

#### 函数: `tan arg` ####

这些是基本的三角函数，参数arg以 **弧度** 为单位。

#### 函数: `asin arg` ####

`(asin arg)` 的值是介于 `-pi/2` 和 `pi/2(包括)` 之间的数字，其正弦值为arg。

如果arg超出范围(在[−1,1]之外)，asin返回NaN。

#### 函数: `acos arg` ####

`(acos arg)` 的值是一个介于 `0和pi(包括)` 之间的数字，其余弦值为arg。

如果arg超出范围(在[−1,1]之外)，则acos返回NaN。

#### 函数: `atan y &optional x` ####

`(atan y)` 的值是一个介于 `-pi/2和pi/2(不包含)` 之间的数字，其正切为y。

如果给出了可选的第二个参数x，则(atan y x)的值是以 **弧度** 为单位的 向量[x, y]与x轴之间的夹角。

#### 函数: `exp arg` ####

这是指数函数;它返回e到幂参数。

#### 函数: `log arg &optional base` ####

这个函数返回arg的对数，以base为底。

如果不指定底数，则使用自然底数e。

如果arg或base为负，log返回NaN。

#### 函数: `expt x y` ####

该函数返回x的y次幂。

如果两个参数都是整数且y非负，则结果为整数;在这种情况下，溢出表示错误 overflow signals an error，所以要小心。

如果x是一个有限的负数，y是一个有限的非整数，export返回一个NaN。


#### 函数: `sqrt arg` ####

这将返回arg的平方根。如果arg是有限的且小于零，sqrt返回NaN。

此外，Emacs还定义了以下常用的数学常数:

#### 变量: `float-e` ####

数学常数 `e(2.71828...)`。

#### 变量: `float-pi` ####

数学常数圆周率 `pi(3.14159…..)`

### 3.10 Random Numbers ###

一个确定性的计算机程序不能产生真正的随机数。

对于大多数用途，伪随机数 *pseudo-random* 就足够了。

以确定性的方式生成一系列伪随机数。

这些数字并不是真正随机的，但它们具有模仿随机序列的某些属性。

例如，所有可能的值在伪随机序列中出现的频率相同。

伪随机数是从种子值 seed value 生成的。

从任意给定的种子开始，随机函数总是生成相同的数字序列。

默认情况下，Emacs在启动时初始化随机种子，这样在每次运行Emacs时，随机值的序列(极有可能)是不同的。

随机种子通常由 **系统熵** *system entropy* 初始化;

然而，在缺乏熵池的过时平台上，种子是从较不随机的易失性数据(如当前时间)中获取的。

有时您希望随机数序列是可重复的。

例如，在调试其行为依赖于随机数序列的程序时，在每个运行的程序中获得相同的行为是有帮助的。

要使序列重复，执行 `(random "")`。

这将为您的特定Emacs可执行文件设置一个常量值(尽管它可能与其他Emacs版本有所不同)。

您可以使用其他字符串来选择各种种子值。

#### 函数: `random &optional limit` ####

这个函数返回一个伪随机整数。重复调用返回一系列伪随机整数。

如果limit为正整数，则选择该值为非负且小于limit。

否则，该值可以是任何固定num，即从 `most-negative-fixnum` 到 `most-positive-fixnum` 的任何整数(参见integer Basics)。

如果limit是一个字符串，则意味着根据字符串的内容选择一个新的种子。

这导致以后对random的调用返回可重复的结果序列。

如果limit为t，则表示选择一个新种子，就像Emacs正在重新启动一样。

这将导致以后对random的调用返回一个不可预测的结果序列。

如果您需要用于 **加密目的** *cryptographic purpose* 的 random nonce，使用随机通常不是最佳方法，原因如下:

* 尽管可以使用 `(random t)` 来查询系统熵，但这样做可能会对程序中受益于可重复结果的其他部分产生不利影响。
* 随机使用的系统相关伪随机数生成器(PRNG)不一定适用于密码学。
* 调用 `(random t)` 不能直接访问系统熵;熵通过依赖于系统的PRNG传递，因此可能会使结果产生偏差。
* 在典型的平台上，随机种子只包含32位，这通常比Emacs的固定值窄，对于加密目的来说远远不够。
* `(random t)` 调用会留下有关nonce的信息，这些信息分散在Emacs的内部状态中，从而增加了内部攻击面的大小。
* 在缺乏熵池的过时平台 obsolescent plantforms 上，`(random t)` 是从加密弱源中播种的。(random t) is seeded from a cryptographically weak source. 

## 4. Strings and Characters ##

在Emacs Lisp中，字符串是包含有序字符序列的数组。

字符串用作符号、缓冲区和文件的名称;向用户发送消息;保存在缓冲区之间复制的文本;还有很多其他的用途。

因为字符串非常重要，所以Emacs Lisp有许多明确的函数来操作它们。

Emacs Lisp程序更多地使用字符串而不是单个字符。

请参阅 将键盘事件放在字符串中 *Putting Keyboard Events in Strings* ，了解键盘字符事件字符串的特殊考虑。

* String and Character Basics
* Predicates for Strings
* Creating Strings
* Modifying Strings
* Comparison of Characters and Strings
* Conversion of Characters and Strings
* Formatting Strings
* Custom Format Strings
* Case Conversion in Lisp
* The Case Table

### 4.1 String and Character Basics ###

[String and Character Basics](https://www.gnu.org/software/emacs/manual/html_node/elisp/String-Basics.html)

字符是一个Lisp对象，它表示文本中的单个字符。

在Emacs Lisp中，字符是简单的整数;整数是否为字符仅取决于如何使用它。

有关Emacs中字符表示的详细信息，请参阅字符代码 Character Codes。

字符串是固定的字符序列。

它是一种称为数组的序列类型，这意味着它的长度是固定的，一旦创建就不能改变(参见序列Sequences、数组Arrays和向量Vectors)。

与C语言不同的是，Emacs Lisp字符串不以特殊字符代码结束。

由于字符串是数组，因此也是序列，您可以使用在序列、数组和向量中记录的通用数组和序列函数对它们进行操作。

例如，您可以使用函数 `aref` 访问字符串中的单个字符(参见对数组进行操作的函数 Functions that Operate on Arrays)。

Emacs字符串(以及缓冲区)中的非ascii字符有两种文本表示形式:单字节和多字节。

对于大多数Lisp编程，您不需要关心这两种表示。

有关详细信息，请参阅文本表示 Text Representations。

有时 键序列 key sequences 表示为单字节字符串 unibyte strings。

当单字节字符串是键序列时，128到255范围内的字符串元素表示元字符 meta characters(大整数 large integers)，而不是128到255范围内的字符代码。

字符串不能包含带有hyper、super或alt修饰符的字符;它们可以保存ASCII控制字符，但不能保存其他控制字符。

它们不区分ASCII控制字符中的大小写。

如果要将此类字符存储在序列中，例如键序列，则必须使用vector而不是string。

有关键盘输入字符的详细信息，请参阅字符类型 Character Type。

字符串用于保存正则表达式很有用。

您还可以使用 `string-match` 对字符串匹配正则表达式(参见正则表达式搜索 Regular Expression Searching)。

函数 `match-string` (参见简单匹配数据访问 Simple Match Data Access)和 `replace-match` (参见替换匹配的文本 Replacing the Text that Matched) 对于在匹配正则表达式后分解和修改字符串非常有用。

与缓冲区一样，字符串可以包含其中字符的文本属性以及字符本身。

参见文本属性 Text Properties。

所有将文本从字符串复制到缓冲区或其他字符串的Lisp原语也复制被复制字符的属性。

有关显示字符串或将字符串复制到缓冲区的函数的信息，请参阅文本 Text。

有关字符和字符串语法的信息，请参见字符类型和字符串类型 Character Type and String Type。

有关在文本表示之间进行转换以及对字符代码进行编码和解码的函数，请参阅非ascii字符 Non-ASCII Characters。

另外，请注意，长度不应该用于计算显示字符串的宽度;请使用 `string-width`(参见显示文本的大小 Size of Displayed Text)。

### 4.2 Predicates for Strings ###

有关一般序列和数组谓词的详细信息，请参见序列、数组和向量以及数组。

#### 函数: `stringp object` ####

如果object是字符串，则返回t，否则返回nil。

#### 函数: `string-or-null-p object` ####

如果object是字符串或nil，这个函数返回t。否则返回nil。

#### 函数: `char-or-string-p object` ####

如果object是字符串或字符(即整数)，此函数返回t，否则返回nil。

### 4.3 Creating Strings ###

下面的函数可以从零开始创建字符串，或者将字符串组合在一起，或者将它们分开。

(对于基于修改后的其他字符串的内容创建字符串的函数，如 `string-replace` 和 `replace-regexp-in-string`，请参见Search and Replace。)


#### 函数: `make-string count character &optional multibyte` ####

这个函数返回一个由重复数个字符组成的字符串。

如果count为负，则出现错误。

``` Elisp
(make-string 5 ?x)
     ⇒ "xxxxx"
(make-string 0 ?x)
     ⇒ ""
```

通常，如果character是一个ASCII字符，则结果是一个单字节字符串。

但如果可选参数multibyte非nil，则该函数将生成一个多字节字符串。

当您以后需要将结果与非ascii字符串连接或用非ascii字符替换其中的一些字符时，这很有用。

与此比较的其他函数包括 `make-vector`(参见Vectors)和 `make-list`(参见构建Cons单元格和列表 Building Cons Cells and Lists)。

#### 函数: `string &rest characters` ####

这将返回一个包含字符characters的字符串。

``` Elisp
(string ?a ?b ?c)
     ⇒ "abc"
```

#### 函数: `substring string &optional start end` ####

此函数返回一个新字符串，该字符串由字符串中的字符组成，范围从(包括)索引开始处的字符到(但不包括)索引结束处的字符。

`[start end)`

第一个字符在索引0处。

只有一个参数，这个函数只是复制string。

``` Elisp
(substring "abcdefg" 0 3)
     ⇒ "abc"
```

在上面的例子中，“a”的索引是0，“b”的索引是1，“c”的索引是2。

索引3(字符串中的第四个字符)标记子字符串被复制到的字符位置。

因此，"abc" 是从字符串 "abcdefg" 中复制的。

负数从字符串的末尾开始计数，因此−1表示字符串最后一个字符的索引。例如:

``` Elisp
(substring "abcdefg" -3 -1)
     ⇒ "ef"
```

在这个例子中，e 的索引是 -3，f 的索引是 -2，g 的索引是 -1。

因此，包含 e 和 f，不包括 g。

当nil用于end时，它代表字符串的长度。因此,

``` Elisp
(substring "abcdefg" -3 nil)
     ⇒ "efg"
```

省略参数end等同于指定nil。

由此可见(substring string 0)返回string的所有副本。

但是我们推荐使用复制序列 `copy-sequence` (参见序列)。

如果从字符串复制的字符具有文本属性，则这些属性也会复制到新字符串中。参见文本属性 Text Properties。

Substring还接受一个vector作为第一个参数。例如:

``` Elisp
(substring [a b (c) "d"] 1 3)
     ⇒ [b (c)]
```

如果start不是整数，或者end既不是整数也不是nil，则会提示类型参数错误 `wrong-type-argument` error。

如果start表示end后面的字符，或者任一整数超出string的范围，则会发出 `args-out-of-range` 错误信号。

将此函数与 `buffer-substring`(请参阅检查缓冲区内容 Examining Buffer Contents)进行对比，后者返回一个字符串，其中包含当前缓冲区中文本的一部分。

字符串的开始位置是索引0，而缓冲区的开始位置是索引1。

#### 函数: `substring-no-properties string &optional start end` ####

它的工作原理类似于substring，但会丢弃值中的所有文本属性。

此外，start也可以省略或为nil，这相当于0。

因此，`(substring-no-properties string)` 返回一个删除了所有文本属性的string副本。

#### 函数: `concat &rest sequences` ####

这个函数返回一个字符串，由传递给它的参数中的字符组成(如果有的话，还包括它们的文本属性)。

参数可以是字符串、数字列表或数字向量;他们自己并没有改变。

如果concat没有接受任何参数，则返回一个空字符串。

``` Elisp
(concat "abc" "-def")
     ⇒ "abc-def"
(concat "abc" (list 120 121) [122])
     ⇒ "abcxyz"
;; nil is an empty sequence.
(concat "abc" nil "-def")
     ⇒ "abc-def"
(concat "The " "quick brown " "fox.")
     ⇒ "The quick brown fox."
(concat)
     ⇒ ""
```

这个函数并不总是分配一个新的字符串。

建议调用者不依赖于结果是一个新字符串，也不依赖于它等于一个现有字符串。

特别是，改变返回值可能会无意中改变另一个字符串，改变程序中的常量字符串，甚至引发错误。

若要获得可以安全更改的字符串，请对结果使用复制序列。

有关其他连接函数的信息，请参见映射函数Mapping Functions中的 `mapconcat` 描述，向量函数Functions for Vectors中的`vconcat`描述，以及构建Cons单元格和列表 Building Cons Cells and Lists 中的`append`描述。

有关将单个命令行参数连接成一个字符串以用作shell命令，请参阅组合和引号字符串。 combine-and-quote-strings

#### 函数: `split-string string &optional separators omit-nulls trim` ####

这个函数基于正则表达式分隔符 `separators`(参见正则表达式)将字符串分割成子字符串。分隔符的每个匹配定义了一个分裂点;分隔点之间的子字符串被制作成一个列表，并返回该列表。

如果`separators`为nil(或省略)，则默认为 `split-string-default-separators` 的值，并且该函数的行为就像 `omit-nulls` 为t一样。

如果 `omit-nulls` 为nil(或省略)，则只要分隔符有两个连续匹配，或者匹配紧邻字符串的开始或结束，结果将包含空字符串。如果 `omit-null` 为t，则从结果中省略这些空字符串。

如果可选参数 `trim` 非nil，则它应该是一个正则表达式，从每个子字符串的开始和结束匹配要修剪的文本。如果微调使子字符串为空，则将其视为空。

如果您需要将字符串拆分为适合于 `call-process` 或 `start-process` 的单个命令行参数列表，请参阅 `split-string-and-unquote`。

``` Elisp
(split-string " two words ")
     ⇒ ("two" "words")
```

结果不是 `( "" "two" "words" "")`，这很少有用。

如果您需要这样的结果，请为分隔符使用显式值:

``` Elisp
(split-string "  two words "
              split-string-default-separators)
     ⇒ ("" "two" "words" "")
```

``` Elisp
(split-string "Soup is good food" "o")
     ⇒ ("S" "up is g" "" "d f" "" "d")
(split-string "Soup is good food" "o" t)
     ⇒ ("S" "up is g" "d f" "d")
(split-string "Soup is good food" "o+")
     ⇒ ("S" "up is g" "d f" "d")
```

空匹配计数，除了 `split-string` 在使用非空匹配到达字符串末尾或字符串为空时不会查找最后的空匹配:

``` Elisp
(split-string "aooob" "o*")
     ⇒ ("" "a" "" "b" "")
(split-string "ooaboo" "o*")
     ⇒ ("" "" "a" "b" "")
(split-string "" "")
     ⇒ ("")
```

但是，当分隔符可以匹配空字符串时，`omit-null` 通常是t，因此前面三个示例中的微妙之处很少相关:

``` Elisp
(split-string "Soup is good food" "o*" t)
     ⇒ ("S" "u" "p" " " "i" "s" " " "g" "d" " " "f" "d")
(split-string "Nice doggy!" "" t)
     ⇒ ("N" "i" "c" "e" " " "d" "o" "g" "g" "y" "!")
(split-string "" "" t)
     ⇒ nil
```

分隔符的某些 **非贪婪** 值可能更喜欢空匹配而不是非空匹配，这种行为有些奇怪，但可以预测。同样，这样的值在实践中很少出现:

``` Elisp
(split-string "ooo" "o*" t)
     ⇒ nil
(split-string "ooo" "\\|o+" t)
     ⇒ ("o" "o" "o")
(split-string "ooo" "o+" t)
     ⇒ nil
(split-string "ooo" "\\o+" t)
     ⇒ nil
(split-string "ooo" "|o+" t)
     ⇒ ("ooo")
```

#### 变量: `split-string-default-separators` ####

分割字符串的分隔符的默认值。它通常的值是`"“[ \f\t\n\r\v]+`。

#### 函数: `string-clean-whitespace string` ####

清除字符串中的空白，方法是将空白段折叠为单个空格字符，以及从字符串的开始和结束处删除所有空白。

#### 函数: `string-trim-left string &optional regexp` ####

从string中删除与regexp匹配的开头文本。`regexp`默认为 `[\t\n\r]+`。

#### 函数: `string-trim-right string &optional regexp` ####

从string中删除与regexp匹配的尾随文本。`regexp`默认为 `[ \t\n\r]+`。

#### 函数: `string-trim string &optional trim-left trim-right` ####

从字符串中删除与 `term-left` 匹配的开头文本和与 `trim-right` 匹配的尾随文本。两个 `regexp` 默认为 `[ \t\n\r]+`。

#### 函数: `string-fill string length` ####

尝试对字符串进行换行，使行长度不超过length。填充只在空白边界上完成。如果有个别单词比 `length` 长，这些单词不会被缩短。

``` Elisp
(string-fill "aaa bbb ccc ddd" 3)
;   => "aaa
bbb
ccc
ddd"
```

#### 函数: `string-limit string length &optional end coding-system` ####

如果 `string` 小于 `length` 字符，则按原样返回string。否则，返回由第一个 `length` 字符组成的 `string` 的子字符串。如果给出了可选的 `end` 形参，则返回 `length` 为最后一个字符的字符串。

如果 `encoding-system` 为非空，则字符串将在限制之前进行编码，结果将是一个小于 `length` 字节的单字节字符串unibyte string。如果 `string` 包含被编码成几个字节的字符(例如，当使用utf-8时)，则生成的单字节字符串永远不会在字符表示的中间被截断。

此函数以字符或字节为单位测量字符串长度，因此如果您需要缩短字符串以用于显示目的，则通常不合适;使用 `truncate-string-to-width` 或 `window-text-pixel-size` 或 `string-glyph-split` 代替(参见显示文本的大小 Size of Displayed Text)

``` Elisp
(string-limit "abcdef" 5)
;        => "abcde"
(string-limit "谔谔呃呃饿饿" 5)
;        => "谔谔呃呃饿"
```

#### 函数: `string-lines string &optional omit-nulls keep-newlines` ####

将字符串按 **换行符** 分隔为字符串列表。如果可选参数 `omit-nulls` 非nil，则从结果中删除空行。如果可选参数 `keep-newlines` 非nil，则不要从结果字符串中删除尾随的换行符。

#### 函数: `string-pad string length &optional padding start` ####

使用 `padding` 作为填充字符将字符串填充为给定的长度。填充默认为空格字符。如果`string`大于`length`，则不填充。如果`start`为nil或省略，则将 `padding` 添加到string的字符中，如果它是非nil，则将 `padding` 添加到string的字符中。

``` Elisp
(string-pad "0b1011" 10 ?0)
;    =>  "0b10110000"
```

#### 函数: `string-chop-newline string` ####

从字符串中删除结尾换行符(如果有的话)。

``` Elisp
(string-chop-newline "aaa\n")
;    => "aaa"
```

### 4.4 Modifying Strings ###

您可以通过本节中描述的操作更改可变字符串的内容。见
 Mutability。

修改现有字符串内容的最基本方法是使用 `aset`(参见对数组进行操作的函数 Functions that Operate on Arrays)。

`(aset string idx char)` 将字符存储为字符索引 idx 处的字符串。如果需要，它会自动将纯ascii字符串转换为多字节字符串(参见文本表示)，但我们建议始终确保字符串是多字节的(例如，通过使用字符串到多字节，参见转换文本表示)，如果char是非ascii字符，而不是原始字节。

一个更强大的函数是 `store-substring`

#### 函数: `store-substring string idx obj` ####

这个函数通过存储从字符索引 `idx` 开始的 `obj` 来改变指定字符串的部分内容。参数 `obj` 可以是一个字符(在这种情况下，函数的行为与 `aset` 完全相同)或一个(较小的)字符串。如果obj是一个多字节字符串，我们建议确保string也是多字节的，即使它是纯ascii。

由于不可能更改现有字符串中的字符数，因此如果obj包含的字符多于从字符索引idx开始的字符串所能容纳的字符，则会产生错误。

``` Elisp
(store-substring "abc" 2 ?d)
;    => "abd"
```

#### 函数: `clear-string string` ####

要清除包含密码的字符串，使用clear-string

这将使 `string` 成为一个单字节字符串，并将其内容清除为零。它也可以改变字符串的长度。

### 4.5 Comparison of Characters and Strings ###


#### 函数: `char-equal character1 character2` ####

如果参数表示相同的字符，此函数返回t，否则返回nil。如果 `case-fold-search` 为非nil，则此函数忽略大小写差异。

``` Elisp
(char-equal ?x ?x)
     ⇒ t
(let ((case-fold-search nil))
  (char-equal ?x ?X))
     ⇒ nil
(char-equal ?x ?X)
     ⇒ t
```

#### 函数: `string= string1 string2` ####

如果两个字符串的字符完全匹配，该函数返回t。Symbols也可以作为参数，在这种情况下使用符号名。大小写总是很重要的，不管 `case-fold-search` 如何。

这个函数等价于equal，用于比较两个字符串(参见相等谓词)。特别是，两个字符串的文本属性被忽略;如果需要区分仅在文本属性上不同的字符串，请使用 `equal-including-properties`。然而，与equal不同的是，如果参数中的任何一个不是字符串或符号，`string=` 产生错误。

``` Elisp
(string= "abc" "abc")
     ⇒ t
(string= "abc" "ABC")
     ⇒ nil
(string= "ab" "ABC")
     ⇒ nil
```

当且仅当它们包含相同的字符代码序列, 单字节字符串和多字节字符串在 `string=` 才相等的, 且都在0-127 (ASCII)范围内。参见文本表示。

#### 函数: `string-equal string1 string2` ####

等同于 `string=`

#### 函数: `string-equal-ignore-case string1 string2` ####

`string-equal-ignore-case` 比较忽略大小写差异的字符串，例如当 `case-fold-search`为t时的 `char-equal`。

#### 函数: `string-collate-equalp string1 string2 &optional locale ignore-case` ####

如果 `string1` 和 `string2` 相对于 指定语言环境 (默认为当前系统语言环境)的排序规则相等，则此函数返回t。

排序规则 collation rules 不仅由`string1`和`string2`中包含的字符的字典顺序 lexicographic order 决定，而且还由有关这些字符之间关系的进一步规则决定。通常，它是由运行Emacs的语言环境和与Emacs相关联的标准C库定义的。

有关排序规则及其区域依赖关系的详细信息，请参见Unicode排序算法 The Unicode Collation Algorithm。一些标准C库，如GNU C库(又名glibc)实现了Unicode排序算法的大部分，并使用相关的语言环境数据、公共语言环境数据存储库或CLDR。

例如，具有不同码位但含义相同的字符，例如不同的重音Unicode字符，在某些地区可能被认为是相等的:

``` Elisp
(string-collate-equalp (string ?\uFF40) (string ?\u1FEF))
     ⇒ t
```

可选参数`locale`(一个字符串)覆盖用于排序的当前区域设置标识符的设置。该值与系统有关;区域设置“en_US”。`UTF-8` 适用于POSIX系统，而它将是，例如，`enu_USA`。在MS-Windows系统上是1252”。

如果 `ignore-case` 为非空值，则通过将字符转换为小写来不区分大小写地比较字符。但是，如果底层系统库没有提供特定于语言环境的排序规则，则此函数会退回到 `string-equal`，在这种情况下忽略 `ignore-case` 参数，并且比较将始终区分大小写。

要在MS-Windows系统上模拟unicode兼容的排序，请将 `w32-collate-ignore-punctuation` 绑定到一个非nil值，因为区域设置的代码集部分在MS-Windows上不能为“UTF-8”。

如果您的系统不支持语言环境，则此函数的行为类似string-equal。

不要使用这个函数来比较文件名是否相等，因为文件系统通常不支持排序实现的字符串的语言等价性。

#### 函数: `string< string1 string2` ####

这个函数一次比较两个字符串中的一个字符。它同时扫描两个字符串，以查找第一对不匹配的对应字符。如果这两个字符中较小的字符是来自string1的字符，则string1较小，此函数返回t。如果较小的字符是来自string2的字符，则string1较大，此函数返回nil。如果两个字符串完全匹配，则值为nil。

根据字符代码对字符进行比较。请记住，在ASCII字符集中，小写字母比大写字母具有更高的数值;数字和许多标点符号的数值比大写字母的数值要低。一个ASCII字符小于任何非ASCII字符;单字节非ascii字符总是小于任何多字节非ascii字符(参见文本表示)。

``` Elisp
(string< "abc" "abd")
     ⇒ t
(string< "abd" "abc")
     ⇒ nil
(string< "123" "abc")
     ⇒ t
```

当两个字符串的长度不同，并且它们匹配到string1的长度时，结果为t。如果它们匹配到string2的长度，结果为nil。没有字符的字符串小于任何其他字符串。

``` Elisp
(string< "" "abc")
     ⇒ t
(string< "ab" "abc")
     ⇒ t
(string< "abc" "")
     ⇒ nil
(string< "abc" "ab")
     ⇒ nil
(string< "" "")
     ⇒ nil
```

符号也可以作为参数，在这种情况下，它们的打印名称将被比较。

#### 函数: `string-lessp string1 string2` ####

同 `string<`

#### 函数: `String-greaterp string1 string2` ####

这个函数返回以相反顺序比较string1和string2的结果，也就是说，它相当于调用 `(string-lessp string2 string1)`。

#### 函数: `string-collate-lessp string1 string2 &optional locale ignore-case` ####

如果按照指定语言环境的排序顺序string1小于string2(默认为当前系统语言环境)，则此函数返回t。排序顺序不仅由string1和string2中包含的字符的字典顺序决定，还由有关这些字符之间关系的进一步规则决定。通常，它是由运行Emacs的地区环境和Emacs所链接的标准C库定义的。

例如，标点符号和空白字符可能会在排序时被忽略(参见序列):

``` Elisp
(sort (list "11" "12" "1 1" "1 2" "1.1" "1.2") 'string-collate-lessp)
     ⇒ ("11" "1 1" "1.1" "12" "1 2" "1.2")
```

这种行为是系统相关的;例如，无论语言环境如何，标点符号和空白都不会在Cygwin上被忽略。

可选参数locale(一个字符串)覆盖用于排序的当前区域设置标识符的设置。该值与系统有关;区域设置“en_US”。UTF-8”适用于POSIX系统，而它将是，例如，“enu_USA”。在MS-Windows系统上是1252”。"POSIX"或"C"的locale值让string-collate-lessp的行为类似string-lessp:

``` Elisp
(sort (list "11" "12" "1 1" "1 2" "1.1" "1.2")
      (lambda (s1 s2) (string-collate-lessp s1 s2 "POSIX")))
     ⇒ ("1 1" "1 2" "1.1" "1.2" "11" "12")
```

如果ignore-case为非空值，则通过将字符转换为小写来不区分大小写地比较字符。但是，如果底层系统库不提供特定于语言环境的排序规则，则此函数会退回到string-lessp，在这种情况下忽略ignore-case参数，并且比较将始终区分大小写。

要在MS-Windows系统上模拟unicode兼容的排序，请将w32-collate-ignore-punctuation绑定到一个非nil值，因为区域设置的代码集部分在MS-Windows上不能为“UTF-8”。

如果您的系统不支持语言环境，则此函数的行为类似string-lessp。

#### 函数: `string-version-lessp string1 string2` ####

此函数按字典顺序比较字符串，但它将**数字字符序列视为由十进制数字组成**，然后比较数字。因此，根据这个谓词，' foo2.png '比' foo12.png '“小”，即使' 12 '在字典顺序上比' 2 '“小”。

``` Elisp
(string-version-lessp "aaa11" "aaa0")
;    => nil
(string-version-lessp "filename14" "filename5")
;    => nil
```

#### 函数: `string-prefix-p string1 string2 &optional ignore-case` ####

如果string1是string2的前缀，则此函数返回非nil;也就是说，如果string2从string1开始。如果可选参数ignore-case非nil，比较会忽略大小写差异。

``` Elisp
(string-prefix-p "aa" "aabc")
;    => t
```

#### 函数: `string-suffix-p suffix string &optional ignore-case` ####

如果`suffix`是string的后缀，则此函数返回非nil;例如，如果 `string` 以 `suffix` 结尾。如果可选参数ignore-case非nil，比较会忽略大小写差异。

``` Elisp
(string-suffix-p "name" "filename")
;    => t
```

#### 函数: `string-search needle haystack &optional start-pos` ####

返回 `haystack` 中 `needle` 的第一个实例的位置，这两个实例都是字符串。

如果 `start-pos` 非nil，则从 `haystack` 中的该位置开始搜索。如果没有找到匹配项，则返回nil。这个函数在进行比较时只考虑字符串中的字符;文本属性将被忽略。匹配总是区分大小写的。

#### 函数: `compare-strings string1 start1 end1 string2 start2 end2 &optional ignore-case` ####

这个函数比较string1的指定部分和string2的指定部分。string1的指定部分从索引start1(含)运行到索引end1(不含);start1的Nil表示字符串的开始，而end1的Nil表示字符串的长度。同样，string2的指定部分从索引start2一直运行到索引end2。

字符串通过其字符的数值进行比较。例如，如果 `str1` 的第一个不同字符具有较小的数值，则认为str1小于str2。如果ignore-case非nil，字符在比较之前会使用当前缓冲区的大小写表(请参阅大小写表)将其转换为大写。单字节字符串被转换为多字节进行比较(参见文本表示)，因此一个单字节字符串和它到多字节的转换总是被视为相等的。

如果两个字符串的指定部分匹配，则该值为t。否则，该值为一个整数，表示有多少个前导字符是一致的，哪个字符串更少。它的绝对值是1加上两个字符串开头一致的字符数。如果string1(或其指定的部分)较小，则符号为负。

#### 函数: `string-distance string1 string2 &optional bytecompare` ####

这个函数返回源字符串string1和目标字符串string2之间的 **Levenshtein距离**。Levenshtein距离是将源字符串转换为目标字符串所需的单字符更改(删除、插入或替换)的数量;这是字符串之间编辑距离的一种可能定义。

字符串的字母大小写对于计算的距离很重要，但它们的文本属性将被忽略。如果可选参数byteccompare非nil，则该函数以字节而不是字符计算距离。逐字节比较使用字符的内部Emacs表示，因此对于包含原始字节的多字节字符串将产生不准确的结果(参见文本表示);如果您需要原始字节的精确结果，则通过对它们进行编码(参见显式编码和解码)使字符串成为单字节。

#### 函数: `assoc-string key alist &optional case-fold` ####

这个函数的工作方式类似于 `assoc`，除了key必须是字符串或符号，并且比较是使用 `compare-strings` 完成的。

在测试之前将符号转换为字符串。如果 `case-fold` 为非nil，则在比较之前将key和list中的元素转换为大写。与assoc不同的是，这个函数也可以匹配列表中字符串或符号而不是 `conses` 的元素。特别是，list可以是字符串或符号的列表，而不是实际的列表。参见关联列表 Association Lists。

另请参阅比较文本中的 `compare-buffer-substrings` 函数，了解比较缓冲区中的文本的方法。`string-match` 函数用于将正则表达式与字符串进行匹配，可用于某种字符串比较;请参见正则表达式搜索。

### 4.6 Conversion of Characters and Strings ###

介绍字符、字符串和整数之间转换的函数。

`format`(参见格式化字符串) 和 `prin1-to-string`(参见输出函数 Output Functions)也可以将Lisp对象转换为字符串。

`read-from-string`(参见输入函数 Input Functions)可以将Lisp对象的字符串表示转换为对象。

`string-to-multibute` 和 `string-to-unibyte` 函数转换字符串的文本表示(参见转换文本表示 Converting Text Representations)。

有关生成文本字符的文本描述和一般输入事件(`single-key-description`和 `text-char-description`)的函数，请参阅 Documentation。它们主要用于生成帮助消息。


#### 函数: `number-to-string number` ####

这个函数返回一个字符串，该字符串由数字的十进制表示形式组成。如果参数为负，则返回值以负号开头。

``` Elisp
(number-to-string 256)
     ⇒ "256"

(number-to-string -23)
     ⇒ "-23"

(number-to-string -23.5)
     ⇒ "-23.5"
```

`int-to-string`是此函数的半废弃别名。

另请参阅格式化字符串中的format函数 Formatting Strings。

#### 函数: `string-to-number string &optional base` ####

这个函数返回字符串中字符的数值。如果base非nil，则它必须是2到16(含16)之间的整数，整数将以该基数进行转换。如果以零为基数，则使用以10为基数。浮点转换只适用于十进制;我们还没有为浮点数实现其他的小数，因为那要做更多的工作，而且似乎没什么用。

解析跳过字符串开头的空格和制表符，然后读取尽可能多的字符串，将其解释为给定基数中的数字。(在某些系统上，它会忽略开头的其他空白，而不仅仅是空格和制表符。)如果string不能解释为数字，则此函数返回0。

``` Elisp
(string-to-number "256")
     ⇒ 256
(string-to-number "25 is a perfect square.")
     ⇒ 25
(string-to-number "X256")
     ⇒ 0
(string-to-number "-4.5")
     ⇒ -4.5
(string-to-number "1e5")
     ⇒ 100000.0
```

`string-to-int` 是该函数的过时别名。

#### 函数: `char-to-string character` ####

这个函数返回一个包含一个字符的新字符串，character。这个函数是半废弃的，因为函数字符串更通用。参见创建字符串。

#### 函数: `string-to-char string` ####

这个函数返回字符串中的第一个字符。这与 `(aref string 0)` 基本相同，除了如果字符串为空则返回0。(当字符串的第一个字符为空字符时，该值也为0,ASCII码为0。)如果这个函数看起来不太有用，将来可能会被删除。

#### 函数: `concat` ####

这个函数将vector或list转换为string对象。参见创建字符串。

#### 函数: `vconcat` ####

这个函数将字符串转换为向量。参见向量函数。

#### 函数: `append` ####

这个函数将字符串转换为列表。参见构建Cons单元格和列表 Building Cons Cells and Lists。

#### 函数: `byte-to-string` ####

这个函数将一个字节的字符数据转换为一个单字节字符串。参见转换文本表示。 Converting Text Representations

### 4.7 Formatting Strings ###

[Formatting Strings](https://www.gnu.org/software/emacs/manual/html_node/elisp/Formatting-Strings.html)

*format* 意味着通过在常量字符串的不同位置替换计算值来构造字符串。这个常量字符串控制其他值的打印方式，以及它们出现的位置;它被称为格式字符串 *format string* 。

格式化对于计算要显示的消息通常很有用。事实上，函数`message`和`error`提供了与这里描述的相同的格式化特性;它们与格式化消息的不同之处在于它们如何使用格式化的结果。

#### 函数: `format string &rest objects` ####

这个函数返回一个等于string的字符串，用相应对象的编码替换任何格式规范。参数对象是要格式化的计算值。

除了格式规范之外，string中的字符会被直接复制到输出中，包括它们的文本属性(如果有的话)。格式规范的任何文本属性都被复制到参数对象的生成的字符串表示形式中。

输出字符串不需要重新分配。例如，如果x是字符串"foo"，表达式 `(eq x (format x))` 和 `(eq x (format "%s" x))` 可能都会产生t。

#### 函数: `format-message string &rest objects` ####

这个函数的作用类似于format，除了它还根据 `text-quotes-style` 的值转换字符串中的任何重音符号(反引号)和撇号(引号)。

通常，格式中的重音和撇号会转换为匹配的弯曲引号，例如

"Missing `%s'" 会是 “Missing 'foo'”。

请参阅文本引用风格 Text Quoting Style，了解如何影响或抑制这种翻译。

#### 格式规范 format specification ####

格式规范是以 `%` 开头的字符序列。因此，如果string中有 `%d`，则format函数将其替换为要格式化的值之一(参数对象之一)的打印表示形式。例如:

``` Elisp
(format "The value of fill-column is %d." fill-column)
     ⇒ "The value of fill-column is 72."
```

由于format将 `%` 字符解释为格式规范，因此永远不要将任意字符串作为第一个参数传递。当字符串由一些Lisp代码生成时尤其如此。除非已知字符串从不包含任何 `%` 字符，否则将下面描述的 `%s` 作为第一个参数，并将字符串作为第二个参数，如下所示:

``` Elisp
(format "%s" arbitrary-string)
```

某些格式规范需要特定类型的值。如果您提供的值不符合要求，则会发出错误信号。

以下是有效格式规范的表格:

##### `%s` 不带引号的字符串 #####

将规范替换为对象的打印表示形式，不加引号(即使用`princ`，而不是`prin1` -参见输出函数 Output Functions)。因此，字符串仅由其内容表示，不含 `"` 字符，符号也不含 `\` 字符。

如果对象是字符串，则将其文本属性复制到输出中。`%s` 本身的文本属性也被复制，但对象的文本属性优先。

``` Elisp
(format "This is a %s." "apple")
;  "This is a apple."
```

##### `%S` 带引号的字符串 #####

将规范替换为对象的打印表示形式，并加上引号(即使用`prin1` -参见输出函数)。因此，字符串被 `"` 字符包围，`\` 字符出现在特殊字符之前的必要位置。

``` Elisp
(format "We call it %S." "banana")
;  "We call it \"banana\"."
```

##### `%o` 八进制 #####

将该规范替换为整数的以8为基数的表示形式。负整数的格式依赖于平台。该对象还可以是格式化为整数的浮点数，省略任何分数。

``` Elisp
(format "It is #o%o." 16)   ; "It is #o20."
(format "It is #o%o." -1)   ; "It is #o-1."
(format "It is #o%o." 0.1)  ; "It is #o0."
(format "It is #o%o." 3.4)  ; "It is #o3."
```

##### `%d` 十进制整数 #####

将规范替换为带符号整数的十进制表示形式。该对象还可以是格式化为整数的浮点数，省略任何分数。

``` Elisp
(format "There are %s chars" 16)  ; "There are 16 chars"
(let ((a 3) (b 4))
    (format "a=%d, b=%d" a b))
```

##### `%x` 和 `%X` #####

将该规范替换为整数的十六进制表示形式。负整数的格式依赖于平台。`%x` 使用小写，`%x` 使用大写。该对象还可以是格式化为整数的浮点数，省略任何分数。

``` Elisp
(format "%d is #o%o and #x%x #X%X" 15 15 15 15)
;  "15 is #o17 and #xf #XF"
```

##### `%c` 字符 #####

用给定值的字符替换规范。

``` Elisp
(format "%c" ?\C-M)
;  "
"
```

##### `%e` 指数表示法 #####

将规范替换为浮点数的指数表示法。

``` Elisp
(format "%e" 1500.78)
;  "1.500780e+03"
```

##### `%f` 浮点数 #####

将规范替换为浮点数的小数点表示法。

``` Elisp
(format "%f" pi)
;  "3.141593"
(format "%f" float-pi)
;  "3.141593"
(format "%f" float-e)
;  "2.718282"
```

##### `%g` 浮点数 #####

将规范替换为浮点数的表示法，使用指数表示法或小数点表示法。如果指数小于 `-4` 或大于或等于精度(默认值:6)，则使用指数表示法。默认情况下，从结果的小数部分删除尾随零，并且只有在后跟数字时才会出现小数点字符。

``` Elisp
(format "%g" float-pi)
;  "3.14159"
(format "%g" float-e)
;  "2.71828"
(format "%g" 975.0001)
;  "975"
(format "%g" 975.001)
;  "975.001"
```

##### `%%` 百分号 #####

用单个 `%` 替换该规格。这个格式规范是不寻常的，因为它的唯一形式是普通的 `%%`，而且它不使用值。例如，`(format "%% %d" 30)`返回 "% 30"。

##### 例子 #####

任何其他格式字符将导致“无效格式操作 invalid format operation”错误。

下面是几个示例，它们采用了典型的 文本引用样式 *text-quoting-style* 设置:

``` Elisp
(format "The octal value of %d is %o,
         and the hex value is %x." 18 18 18)
     ⇒ "The octal value of 18 is 22,
         and the hex value is 12."

(format-message
 "The name of this buffer is ‘%s’." (buffer-name))
     ⇒ "The name of this buffer is ‘strings.texi’."

(format-message
 "The buffer object prints as `%s'." (current-buffer))
     ⇒ "The buffer object prints as ‘strings.texi’."
```

默认情况下，格式规范对应于对象的连续值。因此，字符串中的第一个格式规范使用第一个这样的值，第二个格式规范使用第二个这样的值，依此类推。任何额外的格式规范(没有对应值的格式规范)都会导致错误。任何要格式化的额外值都将被忽略。

``` Elisp
(format "%d" 15 15 15)
;  "15"
```

##### field number #####

格式规范可以有一个字段号 *field number*，它是紧接在初始 `%` 之后的十进制数，后面跟着一个文字美元符号 `$`。它导致格式规范将参数转换为给定的数字，而不是下一个参数。**字段编号从1开始**。格式可以包含编号或未编号的格式规范，但不能同时包含这两种格式规范，除了 `%%` 可以与编号规范混合。

例如, `%2$d` 表示第二个参数以整数格式化

``` Elisp
(format "%2$s, %3$s, %%, %1$s" "x" "y" "z")
     ⇒ "y, z, %, x"
(format "2: %2$d, 4: %4$d, 1: %1$d, 3: %3$d." 1 2 3 4)
;  "2: 2, 4: 4, 1: 1, 3: 3."
```

##### flag 字符 #####

在 `%` 和任何字段号之后，您可以放置某些标志字符。

标志 `+` 在非负数之前插入一个加号，因此它总是有一个符号。

作为标志的 `空格` 字符在非负数之前插入一个空格。(否则，非负数从第一个数字开始。)

这些标志对于确保非负数和负数使用相同数量的列非常有用。除了 `%d` ， `%e`， `%f` ， `%g` 之外，它们将被忽略，如果同时使用两个标志，`+` 优先。

``` Elisp
(format "%2$+d" 2 3)
;  "+3"
(format "%1$ d" 5)
;  " 5"
(format "%1$+ d" 2)
;  "+2"
```

标志 `#` 指定了另一种形式，这取决于所使用的格式。对于 `%o`，它确保结果以 `0` 开头。对于 `%x` 和 `%x`，它在非零结果前加上' 0x '或' 0x '。对于 `%e` 和 `%f`， `#` 标志表示包含小数点，即使精度为零。对于 `%g`，它总是包含一个小数点，并且还强制将小数点后的任何尾随零留在原本会被删除的位置。

``` Elisp
(format "%1$#+o" 1)
;  "+01"
(format "%1$#+x" 15)
;  "+0xf"
```

标志 `0` 确保填充由 `0` 字符而不是空格组成。对于 `%s`、`%S` 和 `%c` 等非数字规范字符，此标志将被忽略。这些规范字符接受 `0` 标志，但仍然用空格填充。

``` Elisp
(format "%05d" 5)
;  "00005"
```

标记 `-` 将导致按宽度插入的任何填充(如果指定)插入到右侧而不是左侧。如果同时存在 `-` 和 `0`，则忽略 `0` 标志。

``` Elisp
(format "%06d is padded on the left with zeros" 123)
     ⇒ "000123 is padded on the left with zeros"

(format "'%-6d' is padded on the right" 123)
     ⇒ "'123   ' is padded on the right"

(format "The word '%-7s' actually has %d letters in it."
        "foo" (length "foo"))
     ⇒ "The word 'foo    ' actually has 3 letters in it."
```

规范可以有一个 宽度 *width*，它是一个十进制数字，出现在任何字段号和标志之后。如果对象的打印表示包含的字符少于此宽度，则format将使用填充对其进行扩展。任何由宽度引入的填充通常由左侧插入的空格组成:

``` Elisp
(format "%5d is padded on the left with spaces" 123)
     ⇒ "  123 is padded on the left with spaces"
```

如果宽度太小，则格式不会截断对象的打印表示形式。因此，可以使用宽度来指定列之间的最小间距，而不会有丢失信息的风险。在以下两个示例中， `%7s`' 指定最小宽度为7。在第一种情况下，插入 `%7s` 的字符串只有3个字母，并且需要4个空格作为填充。在第二种情况下，字符串"specification"有13个字母宽，但没有被截断。

``` Elisp
(format "The word '%7s' has %d letters in it."
        "foo" (length "foo"))
     ⇒ "The word '    foo' has 3 letters in it."
(format "The word '%7s' has %d letters in it."
        "specification" (length "specification"))
     ⇒ "The word 'specification' has 13 letters in it."
```

所有规范字符都允许在字段号、标志和宽度(如果存在)之后选择精度。精度是一个小数点后面跟着一个数字字符串。对于浮点数规范(`%e` 和 `%f`)，精度指定要显示小数点后的位数;如果为零，小数点本身也被省略。对于 `%g`，精度指定要显示多少位有效数字(有效数字是小数点前的第一位数字及其后的所有数字)。如果 `%g` 的精度为零或未指定，则将其视为1。对于 `%s` 和 `%s`，精度将字符串截断为给定的宽度，因此 `%.3s` 只显示对象表示的前三个字符。对于其他规范字符，精度的影响是由`printf`族的本地库函数产生的。

``` Elisp
(format "%.4f" float-pi)
;  "3.1416"
(format "%.5g" 9.0001)
;  "9.0001"
```

如果您计划稍后在格式化字符串上使用 `read` 来检索格式化值的副本，请使用允许read重构该值的规范。要以这种可逆的方式格式化数字，你可以使用 `%s` 和 `%s`，要格式化整数，你也可以使用 `%d`，要格式化非负整数，你也可以使用 `#x%x` 和 `#o%o`。其他格式可能有问题;例如，`%d` 和 `%g` 可能错误地处理nan，并可能丢失精度和类型， `#x%x` 和 `#o%o` 可能错误地处理负整数。参见输入函数 Input Functions。

本节中描述的函数接受一组固定的规范字符。下一节描述一个函数格式规范，它可以接受自定义规范字符，如 `%a` 或 `%z`。

### 4.8 Custom Format Strings ###

有时，允许用户和Lisp程序通过自定义格式控制字符串来控制如何生成某些文本是很有用的。例如，格式字符串可以控制如何显示某人的名字、姓氏和电子邮件地址。使用前一节中描述的函数格式，格式字符串可以是类似“%s %s <%s>”的格式。然而，这种方法很快就变得不切实际，因为不清楚哪个规范字符对应于哪个信息。

对于这种情况，更方便的格式字符串应该是类似 `%f %l <%e>` 这样的字符串，其中每个规范字符携带更多的语义信息，并且可以很容易地相对于其他规范字符进行重新排列，从而使用户更容易定制此类格式字符串。

本节中描述的函数 `format-spec` 执行与format类似的功能，只是它对使用任意规范字符的格式控制字符串进行操作。

#### 函数: `format-spec template spec-alist &optional ignore-missing split` ####

此函数返回一个字符串，该字符串根据 `spec-alist` 中指定的转换从格式字符串 模板 template 生成，该字符串是一个alist(参见关联列表)，格式为 `(letter . replacement)`。在格式化结果字符串时，模板中的每个规格 `%` 字母将被替换。

除了格式规范之外，模板中的字符会直接复制到输出中，包括它们的文本属性(如果有的话)。格式规范的任何文本属性都被复制到它们的替换项中。

使用 alist 来指定转换会产生一些有用的属性:

* 如果 `spec-alist` 包含的 唯一字母键 unique letter keys 多于模板中的唯一规范字符，则忽略未使用的键。
* 如果`spec-alist` 包含多个与相同字母的关联，则使用最接近列表开头的关联。
* 如果模板不止一次包含相同的规范字符，那么在 `spec-alist`中找到的相同替换将被用作所有该字符替换的基础。
* 模板中规范的顺序不必与 `spec-alist` 中的关联顺序相对应。

`REPLACEMENT` 也可以是一个不接受参数的函数，并返回一个用于替换的字符串。它只会在模板中使用相应的 `LETTER` 时被调用。这很有用，例如，避免提示输入，除非需要输入。

可选参数 `ignore-missing` 指示如何处理 `spec-alist` 中找不到的模板中的规范字符。如果为nil或省略，则该函数发出错误信号;如果它被忽略，这些格式规范将被保留在输出中，包括它们的文本属性(如果有的话);如果是delete，这些格式规范将从输出中删除;任何其他非nil值都被处理为忽略，但任何出现的 `%%` 也会在输出中原样保留。

如果可选参数 `split` 为非nil，则 `format-spec` 将根据执行替换的位置将结果拆分为字符串列表，而不是返回单个字符串。例如:

``` Elisp
(format-spec "foo %b bar" '((?b . "zot")) nil t)
     ⇒ ("foo " "zot" " bar")
```

`format-spec` 所接受的格式规范的语法与 `format` 所接受的语法相似，但不完全相同。在这两种情况下，格式规范都是以 `%` 开头，以 `s` 等字母结尾的字符序列。

`format` 将特定的含义分配给一组固定的规范字符，而`format-spec`与`format`不同，它接受任意的规范字符，并平等地对待它们。例如:

``` Elisp
(setq my-site-info
      (list (cons ?s system-name)
            (cons ?t (symbol-name system-type))
            (cons ?c system-configuration)
            (cons ?v emacs-version)
            (cons ?e invocation-name)
            (cons ?p (number-to-string (emacs-pid)))
            (cons ?a user-mail-address)
            (cons ?n user-full-name)))

(format-spec "%e %v (%c)" my-site-info)
     ⇒ "emacs 27.1 (x86_64-pc-linux-gnu)"

(format-spec "%n <%a>" my-site-info)
     ⇒ "Emacs Developers <emacs-devel@gnu.org>"
```

格式规范可以在 `%` 后面立即包含以下任意数量的标志字符，以修改替换的各个方面。

* `0`: 此标志导致由宽度指定的任何填充由' 0 '字符而不是空格组成。
* `-`: 此标志将导致由宽度指定的任何填充被插入到右侧而不是左侧。
* `<`: 如果指定了宽度和精度，这个标志会导致替换在左边被截断。
* `>`: 如果指定，此标志将导致替换在给定宽度的右侧被截断。
* `^`: 此标志将替换的文本转换为大写(参见Lisp中的大小写转换)。
* `_ (underscore)`: 此标志将替换的文本转换为小写(参见Lisp中的大小写转换)。

使用矛盾的标志(例如，大写和小写)的结果是未定义的。

与格式的情况一样，格式规范可以包括宽度(出现在任何标志之后的十进制数)和精度(小数点')。，后跟一个十进制数字，该数字出现在任何标志和宽度之后。

如果替换包含的字符少于其指定的宽度，则在左侧填充:

``` Elisp
(format-spec "%8a is padded on the left with spaces"
             '((?a . "alpha")))
     ⇒ "   alpha is padded on the left with spaces"
```

如果替换包含的字符多于其指定的精度，则在右侧截断:

``` Elisp
(format-spec "%.2a is truncated on the right"
             '((?a . "alpha")))
     ⇒ "al is truncated on the right"
```

下面是一个更复杂的例子，它结合了前面提到的几个特性:

``` Elisp
(setq my-battery-info
      (list (cons ?p "73")      ; Percentage
            (cons ?L "Battery") ; Status
            (cons ?t "2:23")    ; Remaining time
            (cons ?c "24330")   ; Capacity
            (cons ?r "10.6")))  ; Rate of discharge

(format-spec "%>^-3L : %3p%% (%05t left)" my-battery-info)
     ⇒ "BAT :  73% (02:23 left)"

(format-spec "%>^-3L : %3p%% (%05t left)"
             (cons (cons ?L "AC")
                   my-battery-info))
     ⇒ "AC  :  73% (02:23 left)"
```

正如本节中的示例所示，格式规范通常用于有选择地格式化各种不同的信息片段。这在提供用户可自定义格式字符串的程序中很有用，因为用户可以选择使用常规语法和以任何期望的顺序格式化程序提供的信息的子集。

``` Elisp
(setq my-datetime-info
      (list (cons ?Y "2024")     ; year
	        (cons ?M "4")        ; month
			(cons ?D "4")        ; day
			(cons ?h "10")       ; hour
			(cons ?m "54")       ; minutes
			(cons ?t "15")))     ; seconds

(format-spec "% 4Y年-%02M月-%02D日-%02h时-%02m分-%02t秒" my-datetime-info)
;  "2024年-04月-04日-10时-54分-15秒"
```

### 4.9 Case Conversion in Lisp ###

字符大小写函数改变单个字符或字符串内容的大小写。这些函数通常只转换字母字符(字母 A 到 Z 和 a 到 z ，以及非ascii字母);其他字符不改变。您可以通过指定一个case表来指定一个不同的case转换映射(参见case table)。

这些函数不修改作为参数传递给它们的字符串。

下面的示例使用字符 X 和 x，它们分别具有ASCII码88和120。

#### 函数: `downcase string-or-char` ####

这个函数将 `string-or-char`(应该是字符或字符串)转换为小写。

当`string-or-char`为字符串时，此函数返回一个新字符串，其中参数中的每个大写字母都将转换为小写字母。当`string-or-char`为字符时，此函数返回对应的小写字符(整数);如果原始字符是小写，或者不是字母，则返回值等于原始字符。

``` Elisp
(downcase "The cat in the hat")
     ⇒ "the cat in the hat"

(downcase ?X)
     ⇒ 120
```

#### 函数: `upcase string-or-char` ####

这个函数将 `string-or-char`(应该是字符或字符串)转换为大写。

当`string-or-char`为字符串时，此函数返回一个新字符串，其中参数中的每个小写字母都将转换为大写字母。当`string-or-char`为字符时，此函数返回相应的大写字符(整数);如果原字符是大写，或者不是字母，则返回值等于原字符。

``` Elisp
(upcase "The cat in the hat")
     ⇒ "THE CAT IN THE HAT"

(upcase ?x)
     ⇒ 88
```

#### 函数: `capitalize string-or-char` ####

这个函数将字符串或字符大写。如果`string-or-char`是字符串，则该函数返回一个新字符串，其内容是`string-or-char`的副本，其中每个单词都大写。这意味着每个单词的第一个字符转换为大写，其余字符转换为小写。

单词的定义是在当前语法表中分配给单词组成语法类的任何连续字符序列(参见语法类表)。

当`string-or-char`是字符时，此函数的作用与upcase相同。

``` Elisp
(capitalize "The cat in the hat")
     ⇒ "The Cat In The Hat"

(capitalize "THE 77TH-HATTED CAT")
     ⇒ "The 77th-Hatted Cat"

(capitalize ?x)
     ⇒ 88
```

#### 函数: `upcase-initials string-or-char` ####

如果`string-or-char`是一个字符串，该函数将`string-or-char`中单词的首字母大写，而不改变首字母以外的任何字母。它返回一个新字符串，其内容是string-or-char的副本，其中每个单词的首字母都已转换为大写。

单词的定义是在当前语法表中分配给单词组成语法类的任何连续字符序列(参见语法类表)。

当大写字母的参数是一个字符时，大写字母的结果与大写字母的结果相同。

``` Elisp
(upcase-initials "The CAT in the hAt")
     ⇒ "The CAT In The HAt"
```

请注意，大小写转换不是代码点的一对一映射，结果的长度可能与参数的长度不同。此外，由于传递字符强制返回类型为字符，函数无法执行适当的替换，与处理单字符字符串相比，结果可能不同。例如:

``` Elisp
(upcase "ﬁ")  ; note: single character, ligature "fi"
     ⇒ "FI"

(upcase ?ﬁ)
     ⇒ 64257  ; i.e. ?ﬁ

(upcase "我喜欢Emacs")
;  "我喜欢EMACS"
```

为了避免这种情况，必须首先使用字符串函数将字符转换为字符串，然后再将其传递给其中一个大小写函数。当然，不能对结果的长度作任何假设。

这些特殊情况的映射来自特殊大写，特殊小写和特殊标题，参见字符属性。

有关比较字符串的函数，请参阅字符和字符串的比较;其中一些忽略大小写差异，或者可以选择忽略大小写差异。

### 4.10 The Case Table ###

您可以通过安装特殊的 *case table* 来自定义大小写转换。大小写表指定大写字母和小写字母之间的映射。它既影响Lisp对象的大小写转换函数(参见上一节)，也影响那些应用于缓冲区文本的函数(参见大小写更改)。每个缓冲区都有一个case表;还有一个标准的case表，用于初始化新缓冲区的case表。

case table 是一个 `char-table`(参见Char-Tables)，它的子类型是 `case-table`。这个字符表将每个字符映射为相应的小写字符。它有三个额外的槽，用来存放相关的表:

* `upcase`: 大写表将每个字符映射到相应的大写字符。
规范化
* `canonicalize`: 规范化表将一组与大小写相关的字符映射到该集合的特定成员。
* `equivalences`: 等价表将一组与大小写相关的字符中的每一个映射到该集合中的下一个字符。

在简单的情况下，你只需要指定到小写的映射;三个相关的表将从那个表自动计算出来。

对于某些语言，大写字母和小写字母不是一一对应的。可能有两个不同的小写字母，但大写字母相同。在这些情况下，您需要同时指定小写和大写的映射。

额外的表规范化将每个字符映射到规范等效;任何两个通过大小写转换相关联的字符都具有相同的规范等效字符。例如，由于 A 和 a 是通过大小写转换相关联的，因此它们应该具有相同的规范等效字符(对于它们两个来说应该是 a ，或者对于它们两个来说都应该是 A )。


额外的表 *equivalences* 是一个映射，它循环地排列每个等价类(具有相同规范等价的字符)。(对于普通ASCII，这将把 a 映射为 A ， A 映射为 a，对于每组等效字符也是如此。)

在构造case表时，可以为规范化提供nil;然后Emacs从小写和大写映射中填充这个槽。你也可以为等价提供nil;然后Emacs从规范化中填补这个空缺。在实际使用的case表中，这些组件是非空的。不要在没有同时指定规范化的情况下尝试指定等价。

以下是使用大小写表的函数:

#### 函数: `case-table-p object` ####

如果对象是有效的case表，则此谓词返回非nil。

#### 函数: `set-standard-case-table table` ####

这个函数使table成为标准的case表，以便在随后创建的任何缓冲区中使用它。

#### 函数: `standard-case-table` ####

这将返回标准 case table。

#### 函数: `current-case-table` ####

这个函数返回当前缓冲区的case表。

#### 函数: `set-case-table table` ####

这将当前缓冲区的大小写表设置为表。

#### 函数: `with-case-table table body` ####

`with-case-table` 宏保存当前case table，使table成为当前的case table，计算body forms，最后恢复case table。返回值是body中最后一个表单的值。即使在通过抛出或错误异常退出的情况下，case表也会恢复(请参阅非本地退出 Nonlocal Exits)。

一些语言环境修改了ASCII字符的大小写转换;例如，在土耳其语环境中，ASCII大写I被小写为土耳其式无点I ('ı')。这可能会干扰需要普通ASCII大小写转换的代码，例如基于ASCII的网络协议的实现。在这种情况下，使用with-case-table宏和变量ASCII -case-table，该变量存储ASCII字符集未修改的case表。

#### 函数: `ascii-case-table` ####

ASCII字符集的大小写表。任何语言环境设置都不应修改此设置。

以下三个函数是用于定义非ascii字符集的包的方便子例程。它们修改指定的case表case-table;它们还修改标准语法表 Syntax Tables。参见语法表。通常，您将使用这些函数来更改标准case table。

#### 函数: `set-case-syntax-pair uc lc case-table` ####

该函数指定一对对应的字母，一个大写，一个小写。

#### 函数: `set-case-syntax-delims l r case-table` ####

这个函数使字符l和r成为一对匹配的大小写不变分隔符。

#### 函数: `set-case-syntax char syntax case-table` ####

这个函数使char保持大小写不变，使用syntax语法。

#### 命令: `describe-buffer-case-table` ####

该命令显示当前缓冲区的case表内容的描述。

## 5 Lists ##

列表表示由零个或多个元素组成的序列(可以是任何Lisp对象)。列表和向量的重要区别在于，两个或多个列表可以共享其部分结构;此外，您可以在列表中插入或删除元素，而无需复制整个列表。


* Lists and Cons Cells
* Predicates on Lists
* Accessing Elements of Lists
* Building Cons Cells and Lists
* Modifying List Variables
* Modifying Existing List Structure
* Using Lists as Sets
* Association Lists
* Property Lists

#### 5.1 Lists and Cons Cells ####

Lisp中的列表不是原始数据类型(primitive data type);它们由 `cons cells` 构建(参见cons单元格和列表类型)。cons单元格是表示有序对的数据对象。也就是说，它有两个槽，每个槽存放或引用某个Lisp对象。一个插槽称为CAR，另一个称为CDR。(这些名字是传统的;请参阅Cons单元格和列表类型。)CDR读作“could-er”。

我们说“这个 cons cell 的CAR是”它的CAR槽当前持有的任何对象，CDR也是如此。

列表是一系列链接在一起的 cell，因此每个cell都引用下一个单元格。列表中的每个元素都有一个cons cell。按照惯例，cons单元格的CAR保存列表的元素，CDR用于链接列表(CAR和CDR之间的这种不对称完全是惯例问题;在cons单元级别，CAR和CDR槽具有相似的属性)。因此，列表中每个cons cell对应的话单槽位对应下面的cons cell。

此外，按照惯例，列表中最后一个cons单元格的CDR为nil。我们称这种以零结尾的结构为适当的列表4。在Emacs Lisp中，符号nil既是一个符号，也是一个没有元素的列表。为方便起见，将符号nil视为其CDR(以及CAR)为nil。

因此，正确列表的CDR始终是正确列表。非空适当列表的CDR是包含除第一个元素以外的所有元素的适当列表。

如果列表的最后一个cons单元格的CDR不是nil，我们称该结构为 点列表(dotted list)，因为它的打印表示将使用点对表示法(参见点对表示法)。还有另一种可能性:某些cons单元的CDR可能指向列表中以前的一个cons单元。我们称这种结构为循环表 circular list。

对于某些目的，列表是正确的、圆形的还是虚线的并不重要。如果程序没有在列表中查找到最后一个cons单元格的CDR，它也不会在意。然而，一些对列表进行操作的函数需要正确的列表，如果给出点列表则会发出错误信号。如果给定一个循环列表，大多数试图找到列表末端的函数都会进入无限循环。您可以使用函数 `proper-list-p`(在下一节中描述)来确定一个列表是否是合适的列表。

因为大多数cons单元格被用作列表的一部分，所以我们将任何由cons单元格组成的结构称为列表结构。

#### 5.2 Predicates on Lists ####

下面的谓词测试一个Lisp对象是 atom，是cons cell还是list，还是区别对象nil。(这些谓语中的许多都可以根据其他谓语来定义，但它们的使用非常频繁，因此值得拥有它们。)

#### 函数: `consp object` ####

如果对象是cons单元格，此函数返回t，否则返回nil。Nil不是cons单元格，尽管它是一个列表。

``` Elisp
(consp nil)
;  t
```

#### 函数: `atom object` ####

如果对象是原子，则返回t，否则返回nil。除了细胞外，所有的物体都是原子。符号nil是一个原子，也是一个列表;它是唯一兼具两者的Lisp对象。

``` Elisp
(atom nil)
;  t
;  (atom object) ≡ (not (consp object))
```

#### 函数: `listp object` ####

如果对象是cons单元格或nil，则此函数返回t。否则，它返回nil。

``` Elisp
(listp '(1))
;  t
(listp '())
```

#### 函数: `nlistp object` ####

此函数与listp相反:如果object不是列表，则返回t。否则，它返回nil。

`(listp object) ≡ (not (nlistp object))`

#### 函数: `null object` ####

如果object为nil，此函数返回t，否则返回nil。这个函数与not相同，但是为了清楚起见，我们在对象被认为是列表时使用null，而不是在对象被认为是真值时使用null(请参阅组合条件的构造)。

``` Elisp
(null '(1))
;  nil
(null nil)
;  t
(null '())
;  t
```

#### 函数: `proper-list-p object` ####

如果对象是一个正确的列表，这个函数返回对象的长度，否则返回nil(参见列表和Cons单元格)。除了 listp 外，一个正确的列表既不是循环列表，也不是点列表。

``` Elisp
(proper-list-p '(a b c))
    ⇒ 3

(proper-list-p '(a b . c))
    ⇒ nil

```

#### 5.3 Accessing Elements of Lists ####

#### 函数: `car cons-cell` ####

这个函数返回 `cons-cell` 的第一个槽所引用的值。换句话说，它返回con -cell的CAR。

作为特殊情况，如果cons-cell为nil，则此函数返回nil。因此，任何列表都是有效参数。如果参数不是cons cell或nil，则会发出错误信号。

``` Elisp
(car '(a b c))
     ⇒ a

(car '())
     ⇒ nil
```

#### 函数: `cdr cons-cell` ####

这个函数返回 `cons-cell` 的第二个槽所引用的值。换句话说，它返回cons-cell的CDR。

作为一种特殊情况，如果cons-cell为nil，则此函数返回nil;因此，任何列表都是有效参数。如果参数不是cons单元格或nil，则会发出错误信号。

``` Elisp
(cdr '(a b c))
     ⇒ (b c)

(cdr '())
     ⇒ nil
```

#### 函数: `car-safe object` ####

该函数允许您获取cons单元格的CAR，同时避免其他数据类型的错误。如果object是cons cell，则返回object的CAR，否则返回nil。这与car相反，如果object不是列表，则会发出错误信号。

``` Elisp
(car-safe object)
≡
(let ((x object))
  (if (consp x)
      (car x)
    nil))
```

#### 函数: `cdr-safe object` ####

该函数使您可以获取cons单元格的CDR，同时避免其他数据类型的错误。如果object是cons cell，则返回object的CDR，否则返回nil。这与cdr相反，如果object不是列表，cdr会发出错误信号。

``` Elisp
(cdr-safe object)
≡
(let ((x object))
  (if (consp x)
      (cdr x)
    nil))
```

#### 宏: `pop listname` ####

这个宏提供了一种方便的方法来检查列表的CAR，并立即将其从列表中删除。它对存储在 `listname` 中的列表进行操作。它从列表中删除第一个元素，将CDR保存到listname中，然后返回被删除的元素。

在最简单的情况下，listname是一个未加引号的符号，用来命名一个列表;在这种情况下，这个宏等价于

``` Elisp
(prog1 (car listname) (setq listname (cdr listname))).
```

``` Elisp
(setq x '(a b c))

(pop x)
;   a

x
;  (b c)
```

更一般地说，listname可以是一个一般化变量。在这种情况下，这个宏使用 `setf` 保存到listname中。参见广义变量 Generalized Variables。

关于push宏，它将元素添加到列表中，请参见修改列表变量。

#### 函数: `nth n list` ####

这个函数返回list的第n个元素。元素从0开始编号，因此list的CAR是元素编号为0。如果list的长度小于等于n，则该值为nil。

``` Elisp
(nth 2 '(1 2 3 4))
     ⇒ 3

(nth 10 '(1 2 3 4))
     ⇒ nil

(nth n x) ≡ (car (nthcdr n x))
```

函数elt是类似的，但适用于任何类型的序列。由于历史原因，它以相反的顺序进行论证。看到序列。

#### 函数: `nthcdr n list` ####

这个函数返回列表的第n个CDR。换句话说，它跳过列表的前n个链接，返回后面的链接。

如果n为0，则nthcdr返回list的所有内容。如果列表的长度小于等于n，则nthcdr返回nil。

``` Elisp
(nthcdr 1 '(1 2 3 4))
     ⇒ (2 3 4)

(nthcdr 10 '(1 2 3 4))
     ⇒ nil

(nthcdr 0 '(1 2 3 4))
     ⇒ (1 2 3 4)
```

#### 函数: `take n list` ####

这个函数返回列表的前n个元素。本质上，它返回列表中nthcdr跳过的部分。

如果小于n个元素，则返回list;如果n为零或负值，它返回nil。

``` Elisp
(take 3 '(a b c d))
     ⇒ (a b c)

(take 10 '(a b c d))
     ⇒ (a b c d)

(take 0 '(a b c d))
     ⇒ nil
```

#### 函数: `ntake n list` ####

这是通过破坏性地修改实参的链表结构来工作的take函数的一个版本。这使得它更快，但是list的原始值可能会丢失。

如果小于n个元素，则返回未修改的列表;如果n为零或负值，它返回nil。否则，它返回被截断到前n个元素的列表。

这意味着使用返回值而不是仅仅依赖截断效应通常是一个好主意，除非已知n为正数。

#### 函数: `last list &optional n` ####

这个函数返回列表的最后一个链接。这个链接的car是列表的最后一个元素。如果list为空，则返回nil。如果n为非nil，则返回倒数第n个链接，如果n大于list的长度则返回整个列表。

``` Elisp
(last '(3 2 1))
;  (1)
(last '(3 2 1) 2)
;  (2 1)
```

#### 函数: `safe-length list` ####

这个函数返回list的长度，没有错误或无限循环的风险。它通常返回列表中不同的cons单元格的数量。然而，对于循环列表，该值只是一个上界;它往往太大了。

如果list不是nil或cons单元格，则安全长度返回0。

当你不担心列表可能是循环的时候，最常用的计算列表长度的方法是使用length。看到序列。

#### 函数: `caar cons-cell` ####

等同于 `(car (car cons-cell))`

``` Elisp
(caar '((1 2 3) 4 5))
;  1
```

#### 函数: `cadr cons-cell` ####

同 `(car (cdr cons-cell)) ` 或 ` (nth 1 cons-cell)`

#### 函数: `cdar cons-cell` ####

同 `(cdr (car cons-cell))`

#### 函数: `cddr cons-cell` ####

同 `(cdr (cdr cons-cell))` 或 `(nthcdr 2 cons-cell)`

除此之外，car和cdr的另外24个组合被定义为cxxxr和cxxxr，其中每个x都是a或d。cadr、caddr和caddr分别选择列表的第二、第三或第四个元素。Cl-lib以cl-second、cl-third和cl-fourth的名称提供相同的功能。参见公共Lisp扩展中的列表函数。

#### 函数: `butlast x &optional n` ####

这个函数返回删除最后一个元素或最后n个元素的列表x。如果n大于零，它会复制列表，以免损坏原始列表。通常，`(append (butlast x n) (last x n))` 将返回一个等于x的列表。

#### 函数: `nbutlast x &optional n` ####

这是butlast的一个版本，它通过破坏性地修改适当元素的cdr而不是创建列表的副本来工作。

#### 5.4 Building Cons Cells and Lists ####

许多函数构建列表，因为列表是Lisp的核心。Cons是基本的列表构建功能;然而，有趣的是，在Emacs的源代码中，list比cons使用的次数更多。

#### 函数: `cons object1 object2` ####

这个函数是构建新的列表结构的最基本的函数。它创建一个新的cons cell，使object1为CAR, object2为CDR。然后返回新的cons单元格。参数object1和object2可以是任何Lisp对象，但object2通常是一个列表。

``` Elisp
(cons 1 '(2))
     ⇒ (1 2)

(cons 1 '())
     ⇒ (1)

(cons 1 2)
     ⇒ (1 . 2)
```

cons通常用于将单个元素添加到列表的前面。这称为将 元素转换到列表中 *consing the element onto the list*。例如:

``` Elisp
(setq list (cons newelt list))
```

请注意，本例中使用的名为list的变量和下面描述的名为list的函数之间没有冲突;任何符号都可以同时满足这两种目的。

#### 函数: `list &rest objects` ####

这个函数创建一个以对象作为元素的列表。生成的列表总是以nil结尾。如果没有给出对象，则返回空列表。

``` Elisp
(list 1 2 3 4 5)
;  (1 2 3 4 5)
(list 1 2 '(3 4 5) 'foo)
;  (1 2 (3 4 5) foo)
(list)
;  nil
```

#### 函数: `make-list length object` ####

这个函数创建一个长度元素列表，其中每个元素都是object。比较make-list和make-string(参见创建字符串)。

``` Elisp
(make-list 3 'pigs)
     ⇒ (pigs pigs pigs)

(make-list 0 'pigs)
     ⇒ nil

(setq l (make-list 3 '(a b)))
     ⇒ ((a b) (a b) (a b))
(eq (car l) (cadr l))
     ⇒ t
```

#### 函数: `append &rest sequences` ####

这个函数返回一个包含序列所有元素的列表。序列可以是列表、向量、布尔向量或字符串，但最后一个通常应该是列表。除了最后一个参数外，所有参数都被复制，因此没有任何参数被改变。(请参阅重新排列列表的函数中的 `nconc`，了解不复制连接列表的方法。)

更一般地说，要追加的最后一个参数可以是任何Lisp对象。最后参数不会被复制或转换;它成为新列表中最后一个cons单元格的CDR。如果最后一个参数本身是一个列表，那么它的元素实际上成为结果列表的元素。如果最后一个元素不是列表，则结果是一个带点的列表，因为它的最终CDR不是nil，nil在正确的列表中是必需的(参见列表和Cons单元格)。

``` Elisp
(setq trees '(pine oak))
     ⇒ (pine oak)
(setq more-trees (append '(maple birch) trees))
     ⇒ (maple birch pine oak)


trees
     ⇒ (pine oak)
more-trees
     ⇒ (maple birch pine oak)

(eq trees (cdr (cdr more-trees)))
     ⇒ t
```

通过查看框图，您可以了解append是如何工作的。变量trees被设置为列表 `(pine oak)`，然后变量more-trees被设置为列表 `(maple birch pine oak)`。然而，变量 trees 继续引用原始列表:

``` PlainText
more-trees                trees
|                           |
|     --- ---      --- ---   -> --- ---      --- ---
 --> |   |   |--> |   |   |--> |   |   |--> |   |   |--> nil
      --- ---      --- ---      --- ---      --- ---
       |            |            |            |
       |            |            |            |
        --> maple    -->birch     --> pine     --> oak
```

空序列对append返回的值没有任何贡献。因此，最后的nil实参强制使用前一个实参的副本:

``` Elisp
trees
     ⇒ (pine oak)

(setq wood (append trees nil))
     ⇒ (pine oak)

wood
     ⇒ (pine oak)

(eq wood trees)
     ⇒ nil
```

在 `copy-sequence` 函数发明之前，这曾经是复制列表的常用方法。参见序列、数组和向量。

下面演示了如何使用vector和string作为 append 的参数:

``` Elisp
(apply 'append '((a b c) nil (x y z) nil))
     ⇒ (a b c x y z)
```

如果没有给出序列，则返回nil:

``` Elisp
(append)
;  nil
```

下面是一些最终参数不是列表的例子:

``` Elisp
(append '(x y) 'z)
     ⇒ (x y . z)
(append '(x y) [z])
     ⇒ (x y . [z])
```

第二个例子表明，当最后一个参数是序列而不是列表时，序列的元素不会成为结果列表的元素。相反，序列成为最终的CDR，就像任何其他非列表最终参数一样。

#### 函数: `copy-tree tree &optional vecp` ####

这个函数返回 tree 的副本。如果tree是一个cons单元，则生成一个具有相同CAR和CDR的新cons单元，然后以相同的方式递归复制CAR和CDR。

通常，当tree不是cons单元格时，copy-tree只返回tree。但是，如果vecp非nil，它也会复制vector(并递归地对其元素进行操作)。

#### 函数: `flatten-tree tree` ####

此函数返回tree的“扁平”副本，即包含所有非nil终端节点或叶子的列表，这些节点或叶子是根在tree上的cons单元的树。返回列表中的叶子与tree中的顺序相同。

``` Elisp
(flatten-tree '(1 (2 . 3) nil (4 5 (6)) 7))
    ⇒(1 2 3 4 5 6 7)
```

#### 函数: `ensure-list object` ####

这个函数返回一个列表形式的对象。如果object已经是一个列表，函数返回它;否则，函数返回一个包含object的单元素列表。

如果你有一个变量可能是也可能不是一个列表，这通常是有用的，然后你可以说，例如:

``` Elisp
(dolist (elem (ensure-list foo))
  (princ elem))
```

#### 函数: `number-sequence from &optional to separation` ####

这个函数返回一个由数字组成的列表，该列表以from开始，按分隔递增，以to或to之前结束。分隔可以是正的，也可以是负的，默认为1。如果to为nil或在数字上等于from，则值为单元素列表(from)。如果to小于from且分隔为正，或大于from且分隔为负，则值为nil，因为这些参数指定了空序列。

如果分隔为0，而to既不是nil，也不是数字上等于from，则number-sequence表示错误，因为这些参数指定了一个无限序列。

所有参数都是数字。浮点参数可能比较棘手，因为浮点运算是不精确的。例如，根据机器的不同，(number-sequence 0.4 0.6 0.2)很可能返回一个元素列表(0.4)，而(number-sequence 0.4 0.8 0.2)返回一个包含三个元素的列表。列表的第n个元素由精确的公式(+ from (* n分隔))计算。因此，如果要确保to包含在列表中，则可以为to传递这种类型的表达式。或者，可以用稍微大一点的值替换to(或者如果分隔为负，则使用稍微负一点的值)。

一些例子:

``` Elisp
(number-sequence 4 9)
;  (4 5 6 7 8 9)
(number-sequence 9 4 -1)
;  (9 8 7 6 5 4)
(number-sequence 9 4 -2)
;  (9 7 5)
(number-sequence 8)
;  (8)
(number-sequence 8 5)
;  nil
(number-sequence 5 8 -1)
;  nil
(number-sequence 1.5 6 2)
;  (1.5 3.5 5.5)
```

没有严格等效的方法将元素添加到列表的末尾。您可以使用 `(append listname (list newelt))`，它通过复制listname并在其末尾添加newelt来创建一个全新的列表。或者您可以使用 `(nconc listname (list newelt))`，它通过跟踪所有cdr来修改listname，然后替换终止的nil。将此与使用cons将元素添加到列表的开头进行比较，后者既不复制也不修改列表。

``` Elisp
; 在结尾添加元素
(setq list1 '(1 2 3))

;  1. 不改变 list1
(append list1 (list 1))
;  2. 改变 list1
(nconc list1 (list 2))
```

#### 5.5 Modifying List Variables ####

这些函数和一个宏提供了方便的方法来修改存储在变量中的列表。

#### 宏: `push element listname` ####

这个宏创建一个新列表，其CAR是element, CDR是listname指定的列表，并将该列表保存在listname中。在最简单的情况下，listname是一个命名列表的未加引号的符号，这个宏等价于 `(setq listname (cons element listname))`。

``` Elisp
(setq l '(a b))
     ⇒ (a b)
(push 'c l)
     ⇒ (c a b)
l
     ⇒ (c a b)
```

更一般地说，listname可以是一个一般化变量。在这种情况下，这个宏的作用相当于(setf listname (cons element listname))。参见广义变量。

关于pop宏，它从列表中删除第一个元素，请参见访问列表的元素。

有两个函数修改列表，它们是变量的值。

#### 函数: `add-to-list symbol element &optional append compare-fn` ####

如果element不是旧值的成员，则该函数通过将element转换为旧值来设置变量符号。它返回结果列表，无论是否更新。在调用之前，符号的值最好是一个列表。Add-to-list使用compare-fn将元素与现有列表成员进行比较;如果compare-fn为nil，则使用equal。

通常，如果添加元素，则将其添加到符号的前面，但如果可选参数append非nil，则将其添加到末尾。

参数符号没有隐式引号;Add-to-list是一个普通函数，与set类似，但与setq不同。如果你想的话，你可以自己引用这个论点。

当symbol引用词法变量时，不要使用此函数。

下面是一个演示如何使用add-to-list的场景:

``` Elisp
(setq foo '(a b))
     ⇒ (a b)

(add-to-list 'foo 'c)     ;; Add c.
     ⇒ (c a b)

(add-to-list 'foo 'b)     ;; No effect.
     ⇒ (c a b)

foo                       ;; foo was changed.
     ⇒ (c a b)
```

`(add-to-list 'var value)`的等价表达式如下:

``` Elisp
(if (member value var)
    var
  (setq var (cons value var)))
```

#### 函数: `add-to-ordered-list symbol element &optional order` ####

该函数通过在order指定的位置将元素插入旧值(必须是一个列表)来设置变量符号。如果元素已经是列表的成员，则根据顺序调整其在列表中的位置。使用eq测试成员关系。该函数返回结果列表，无论是否更新。

顺序通常是一个数字(整数或浮点数)，列表中的元素按非递减的数字顺序排序。

顺序也可以省略或为空。如果已经有一个元素，则该元素的数字顺序保持不变;否则，元素没有数字顺序。没有数字列表顺序的元素被放置在列表的末尾，没有特定的顺序。

对于order的任何其他值，如果元素已经有一个数字顺序，则删除该元素的数字顺序;否则，它等于nil。

参数符号没有隐式引号; `add-to-ordered-list` 是一个普通函数，与set类似，但与setq不同。如有必要，你自己引用论点。

排序信息存储在符号的 `list-order` 属性的哈希表中。符号不能引用词法变量 lexical variable。

下面是一个演示如何使用add-to-ordered-list的场景:

``` Elisp
(setq foo '())
     ⇒ nil

(add-to-ordered-list 'foo 'a 1)     ;; Add a.
     ⇒ (a)

(add-to-ordered-list 'foo 'c 3)     ;; Add c.
     ⇒ (a c)

(add-to-ordered-list 'foo 'b 2)     ;; Add b.
     ⇒ (a b c)

(add-to-ordered-list 'foo 'b 4)     ;; Move b.
     ⇒ (a c b)

(add-to-ordered-list 'foo 'd)       ;; Append d.
     ⇒ (a c b d)

(add-to-ordered-list 'foo 'e)       ;; Add e.
     ⇒ (a c b e d)

foo                       ;; foo was changed.
     ⇒ (a c b e d)
```

#### 5.6 Modifying Existing List Structure ####

可以使用原语 `setcar` 和 `setcdr` 修改cons单元格的CAR和CDR内容。这些都是破坏性操作，因为它们改变了现有的列表结构。破坏性操作应该只应用于可变列表，即通过cons、list或类似操作构造的列表。通过引用创建的列表是程序的一部分，不应该通过破坏性操作来更改。见可变性 Mutability。

> Common Lisp注意: Common Lisp使用rplaca和rplacd函数来改变列表结构;它们改变结构的方式与setcar和setcdr相同，但是Common Lisp函数返回cons单元格，而setcar和setcdr返回新的CAR或CDR。


* Altering List Elements with setcar
* Altering the CDR of a List
* Functions that Rearrange Lists


### 5.6.1 Altering List Elements with setcar ###

通过`setcar`来改变一个控制单元的CAR。在列表中使用setcar时，将列表中的一个元素替换为另一个元素。

#### 函数: `setcar cons object` ####

此函数将对象存储为cons的新CAR，以替换其先前的CAR。换句话说，它改变了控件的CAR槽以引用对象。它返回值对象。例如:

``` Elisp
(setq x (list 1 2))
     ⇒ (1 2)
(setcar x 4)
     ⇒ 4
x
     ⇒ (4 2)
```

当cons cell是几个列表的共享结构的一部分时，将新的CAR存储到cons中会更改每个列表的一个元素。下面是一个例子:

``` Elisp
;; Create two lists that are partly shared.
(setq x1 (list 'a 'b 'c))
     ⇒ (a b c)
(setq x2 (cons 'z (cdr x1)))
     ⇒ (z b c)


;; Replace the CAR of a shared link.
(setcar (cdr x1) 'foo)
     ⇒ foo
x1                           ; Both lists are changed.
     ⇒ (a foo c)
x2
     ⇒ (z foo c)


;; Replace the CAR of a link that is not shared.
(setcar x1 'baz)
     ⇒ baz
x1                           ; Only one list is changed.
     ⇒ (baz foo c)
x2
     ⇒ (z foo c)

```

下面是变量x1和x2中两个列表共享结构的图形描述，说明了为什么替换b会同时改变它们:

``` PlainText
        --- ---        --- ---      --- ---
x1---> |   |   |----> |   |   |--> |   |   |--> nil
        --- ---        --- ---      --- ---
         |        -->   |            |
         |       |      |            |
          --> a  |       --> b        --> c
                 |
       --- ---   |
x2--> |   |   |--
       --- ---
        |
        |
         --> z
```

这是另一种形式的箱形图，显示了相同的关系:

``` Plaintext
x1:
 --------------       --------------       --------------
| car   | cdr  |     | car   | cdr  |     | car   | cdr  |
|   a   |   o------->|   b   |   o------->|   c   |  nil |
|       |      |  -->|       |      |     |       |      |
 --------------  |    --------------       --------------
                 |
x2:              |
 --------------  |
| car   | cdr  | |
|   z   |   o----
|       |      |
 --------------
```

### 5.6.2 Altering the CDR of a List ###

修改CDR的最低级原语是setcdr:

#### 函数: `setcdr cons object` ####

该函数将对象存储为cons的新CDR，替换之前的CDR。换句话说，它将con的CDR槽更改为引用对象。它返回值对象。

下面是一个用另一个列表替换列表的CDR的示例。列表中除第一个元素外的所有元素都被删除，取而代之的是一个不同的元素序列。第一个元素是不变的，因为它位于列表的CAR中，不能通过CDR到达。

``` Elisp
(setq x (list 1 2 3))
     ⇒ (1 2 3)

(setcdr x '(4))
     ⇒ (4)

x
     ⇒ (1 4)
```

您可以通过更改列表中cons cell的CDR来删除列表中间的元素。例如，这里我们通过更改第一个cons cell的CDR，从列表 `(a b c)` 中删除第二个元素b:

``` Elisp
(setq x1 (list 'a 'b 'c))
     ⇒ (a b c)
(setcdr x1 (cdr (cdr x1)))
     ⇒ (c)
x1
     ⇒ (a c)
```

``` PlainText
                   --------------------
                  |                    |
 --------------   |   --------------   |    --------------
| car   | cdr  |  |  | car   | cdr  |   -->| car   | cdr  |
|   a   |   o-----   |   b   |   o-------->|   c   |  nil |
|       |      |     |       |      |      |       |      |
 --------------       --------------        --------------
```

第二个cons cell以前保存元素b，它仍然存在，它的CAR仍然是b，但是它不再构成这个列表的一部分。

通过更改cdr插入新元素同样容易:

``` Elisp
(setq x1 (list 'a 'b 'c))
     ⇒ (a b c)
(setcdr x1 (cons 'd (cdr x1)))
     ⇒ (d b c)
x1
     ⇒ (a d b c)
```

``` PlainText
 --------------        -------------       -------------
| car  | cdr   |      | car  | cdr  |     | car  | cdr  |
|   a  |   o   |   -->|   b  |   o------->|   c  |  nil |
|      |   |   |  |   |      |      |     |      |      |
 --------- | --   |    -------------       -------------
           |      |
     -----         --------
    |                      |
    |    ---------------   |
    |   | car   | cdr   |  |
     -->|   d   |   o------
        |       |       |
         ---------------
```



### 5.6.3 Functions that Rearrange Lists ###

下面是一些通过修改列表组件单元格的cdr来破坏性地重新排列列表的函数。这些函数是破坏性的，因为它们会分解作为参数传递给它们的原始列表，重新链接它们的cons单元格，形成一个作为返回值的新列表。

参见使用列表作为集合中的delq，了解另一个修改cons单元格的函数。

#### 函数: `nconc &rest lists` ####

这个函数返回一个包含列表中所有元素的列表。与追加不同(参见构建Cons单元格和列表)，列表不会被复制。相反，将每个列表的最后一个CDR更改为引用以下列表。最后一个列表没有改变。例如:

``` Elisp
(setq x (list 1 2 3))
     ⇒ (1 2 3)

(nconc x '(4 5))
     ⇒ (1 2 3 4 5)

x
     ⇒ (1 2 3 4 5)
```

由于nconc的最后一个参数本身没有被修改，因此使用常量列表是合理的，例如'(4 5)，如上例所示。出于同样的原因，最后一个参数不必是一个列表:

``` Elisp
(setq x (list 1 2 3))
     ⇒ (1 2 3)

(nconc x 'z)
     ⇒ (1 2 3 . z)

x
     ⇒ (1 2 3 . z)

```

然而，其他参数(除了最后一个)应该是可变列表。

一个常见的陷阱是使用常量列表作为nconc的非最后参数。如果这样做，结果行为是未定义的(参见自评估表单 Self-Evaluating Forms)。您的程序每次运行时都可能发生变化!以下是可能发生的情况(虽然不能保证会发生):

``` Elisp
(defun add-foo (x)            ; We want this function to add
  (nconc '(foo) x))           ;   foo to the front of its arg.


(symbol-function 'add-foo)
     ⇒ (lambda (x) (nconc '(foo) x))


(setq xx (add-foo '(1 2)))    ; It seems to work.
     ⇒ (foo 1 2)

(setq xy (add-foo '(3 4)))    ; What happened?
     ⇒ (foo 1 2 3 4)

(eq xx xy)
     ⇒ t


(symbol-function 'add-foo)
     ⇒ (lambda (x) (nconc '(foo 1 2 3 4) x))

```

#### 5.7 Using Lists as Sets ####

列表可以表示一个无序的数学集——只要将出现在列表中的值视为集合的元素，忽略列表的顺序即可。要形成两个集合的并集，请使用append(只要您不介意有重复的元素)。可以使用delete-dup或seq-uniq删除相同的重复项。其他有用的set函数包括memq和delq，以及它们的等价版本member和delete。

> Common Lisp注意: Common Lisp有联合函数(避免重复元素)和集合操作的交集函数。在Emacs Lisp中，这些功能的变体由cl-lib库提供。参见公共Lisp扩展中的列表作为集合。

#### 函数: `memq object list` ####

这个函数测试对象是否为list的成员。如果是，memq返回一个从object第一次出现开始的列表。否则，它返回nil。memq中的字母“q”表示它使用eq将对象与列表中的元素进行比较。例如:

``` Elisp
(memq 'b '(a b c b a))
     ⇒ (b c b a)

(memq '(2) '((1) (2)))    ; The two (2)s need not be eq.
     ⇒ Unspecified; might be nil or ((2)).
```

#### 函数: `delq object list` ####

> 这个函数删除第一个元素和中间的元素行为不一样, 它不会更改CAR, 但是会更改CDR

这个函数破坏性地从list中移除所有元素eq到object，并返回结果列表。delq中的字母“q”表示它使用eq将对象与列表中的元素进行比较，如memq和remq。

通常，在调用delq时，应该通过将返回值赋值给保存原始列表的变量来使用返回值。原因解释如下。

delq函数通过简单地向下移动列表来删除列表前面的元素，并返回从这些元素之后开始的子列表。例如:

``` Elisp
(delq 'a '(a b c)) ≡ (cdr '(a b c))
```

当要删除的元素出现在列表中间时，删除它涉及更改CDR(参见更改列表的CDR)。

``` Elisp
(setq sample-list (list 'a 'b 'c '(4)))
     ⇒ (a b c (4))

(delq 'a sample-list)
     ⇒ (b c (4))

sample-list
     ⇒ (a b c (4))

(delq 'c sample-list)
     ⇒ (a b (4))

sample-list
     ⇒ (a b (4))

```

注意，`(delq 'c sample-list)` 修改了sample-list以拼接出第三个元素，但是 `(delq 'a sample-list)` 没有拼接任何东西—它只是返回一个更短的列表。不要假设以前保存参数列表的变量现在拥有更少的元素，或者它仍然保存原始列表!相反，保存delq的结果并使用它。大多数情况下，我们将结果存储回保存原始列表的变量中:

``` Elisp
(setq flowers (delq 'rose flowers))
```

在下面的例子中，delq试图匹配的(list 4)和样本列表中的(4)是相等的，但不相等:

``` Elisp
(delq (list 4) sample-list)
     ⇒ (a c (4))
```

如果要删除等于给定值的元素，请使用delete(见下文)。

#### 函数: `remq object list` ####

这个函数返回list的一个副本，删除所有eq到object的元素。remq中的字母“q”表示它使用eq将对象与list中的元素进行比较。

``` Elisp
(setq sample-list (list 'a 'b 'c 'a 'b 'c))
     ⇒ (a b c a b c)

(remq 'a sample-list)
     ⇒ (b c b c)

sample-list
     ⇒ (a b c a b c)
```

#### 函数: `memql object list` ####

函数memql使用eql将成员与对象进行比较，以测试对象是否为list的成员，因此浮点元素是按值比较的。如果object是一个成员，memql返回一个从它在list中第一次出现开始的列表。否则，它返回nil。

``` Elisp
(memql 1.2 '(1.1 1.2 1.3))  ; 1.2 and 1.2 are eql.
     ⇒ (1.2 1.3)

(memq 1.2 '(1.1 1.2 1.3))  ; The two 1.2s need not be eq.
     ⇒ Unspecified; might be nil or (1.2 1.3).
```

下面三个函数类似于memq、delq和remq，但使用equal而不是eq来比较元素。参见等式谓词。

#### 函数: `member object list` ####

函数member使用equal将成员与object进行比较，以测试object是否为list的成员。如果object是一个成员，member返回一个从它在list中的第一次出现开始的列表。否则，它返回nil。

将其与memq进行比较:

``` Elisp
(member '(2) '((1) (2)))  ; (2) and (2) are equal.
     ⇒ ((2))

(memq '(2) '((1) (2)))    ; The two (2)s need not be eq.
     ⇒ Unspecified; might be nil or (2).

;; Two strings with the same contents are equal.
(member "foo" '("foo" "bar"))
     ⇒ ("foo" "bar")

```

#### 函数: `delete object sequence` ####

这个函数从序列中移除所有等于object的元素，并返回结果序列。

如果sequence是list，则delete与delq的关系就像member与memq的关系一样:它使用equal来比较元素与object的关系，如member;当它找到一个匹配的元素时，就像delq一样删除该元素。与delq一样，通常应该通过将返回值赋值给保存原始列表的变量来使用返回值。

如果sequence是vector或string类型，则delete返回sequence的副本，其中所有元素都等于被移除的对象。

例如:

``` Elisp
(setq l (list '(2) '(1) '(2)))
(delete '(2) l)
     ⇒ ((1))
l
     ⇒ ((2) (1))
;; If you want to change l reliably,
;; write (setq l (delete '(2) l)).

(setq l (list '(2) '(1) '(2)))
(delete '(1) l)
     ⇒ ((2) (2))
l
     ⇒ ((2) (2))
;; In this case, it makes no difference whether you set l,
;; but you should do so for the sake of the other case.

(delete '(2) [(2) (1) (2)])
     ⇒ [(1)]
```

#### 函数: `remove object sequence` ####

这个函数是delete的非破坏性对应函数。它返回sequence、list、vector或string的副本，其中元素等于object removed。例如:

``` Elisp
(remove '(2) '((2) (1) (2)))
     ⇒ ((1))

(remove '(2) [(2) (1) (2)])
     ⇒ [(1)]
```

> Common Lisp注意:函数member, delete和remove在GNU Emacs Lisp中是从Maclisp派生的，而不是Common Lisp。Common Lisp版本不使用equal来比较元素。

#### 函数: `member-ignore-case object list` ####

这个函数类似于member，除了对象应该是字符串，并且忽略字母和文本表示的差异:大写字母和小写字母被视为相等，并且在比较之前将单字节字符串转换为多字节。

#### 函数: `delete-dups list` ####

这个函数破坏性地从list中删除所有相等的重复项，将结果存储在list中并返回。如果一个元素在list中多次相等地出现，delete-dup会保留第一个元素。非破坏性操作参见seq-uniq(参见序列)。

#### 5.8 Association Lists ####

关联列表(简称列表)记录了从键到值的映射。它是一个被称为关联的cons cell的列表:每个cons单元的CAR是键，CDR是相关联的值

下面是列表的一个例子。键 *pine* 与值 *cones* 相关联; 键 *oak* 与 *acorns* 关联; 而键 *maple* 与 *seeds* 有关。

``` Elisp
((pine . cones)
 (oak . acorns)
 (maple . seeds)) 
```

列表中的值和键都可以是任何Lisp对象。例如，在下面的列表中，符号a与数字1相关联，字符串“b”与列表 `(2 3)` 相关联，这是列表元素的CDR:

``` Elisp
((a . 1) ("b" 2 3))
```

有时，最好设计一个列表，将相关值存储在元素的CDR的CAR中。下面是这样一个列表的例子:

``` Elisp
((rose red) (lily white) (buttercup yellow))
```

这里我们把 red 看作与 rose 相关的值。这种列表的一个优点是，您可以在CDR的CDR中存储其他相关信息——甚至是其他项目的列表。缺点之一是不能使用`rassq`(见下文)来查找包含给定值的元素。当这些考虑都不重要时，选择是一个品味问题，只要你对任何给定的列表保持一致。

上面显示的相同列表可以被视为在元素的CDR中具有关联值;与rose相关联的值将是列表 `(red)`。

关联列表通常用于记录可能保存在堆栈中的信息，因为可以很容易地将新的关联添加到列表的前面。在关联列表中搜索具有给定键的关联时，如果有多个关联，则返回找到的第一个关联。

在Emacs Lisp中，如果关联列表中的元素不是cons cells，则不会产生错误。列表搜索函数直接忽略这些元素。在这种情况下，许多其他版本的Lisp都会发出错误信号。

注意，属性列表在几个方面与关联列表相似。属性列表的行为类似于关联列表，其中每个键只能出现一次。有关属性列表和关联列表的比较，请参阅属性列表 Property Lists。


#### 函数: `assoc key alist &optional testfn` ####

这个函数返回list中key的第一个关联，如果它是一个函数，则使用testfn将key与list元素进行比较，否则使用 `equals`(参见相等谓词 Equality Predicates)。如果 testfn 是一个函数，则使用两个参数调用它:list中元素的CAR和key。如testfn测试的那样，如果list中没有关联的CAR等于key，则该函数返回nil。例如:

``` Elisp
(setq trees '((pine . cones) (oak . acorns) (maple . seeds)))
     ⇒ ((pine . cones) (oak . acorns) (maple . seeds))
(assoc 'oak trees)
     ⇒ (oak . acorns)
(cdr (assoc 'oak trees))
     ⇒ acorns
(assoc 'birch trees)
     ⇒ nil
```

下面是另一个例子，其中的键和值不是符号:

``` Elisp
(setq needles-per-cluster
      '((2 "Austrian Pine" "Red Pine")
        (3 "Pitch Pine")
        (5 "White Pine")))

(cdr (assoc 3 needles-per-cluster))
     ⇒ ("Pitch Pine")
(cdr (assoc 2 needles-per-cluster))
     ⇒ ("Austrian Pine" "Red Pine")
```

函数`assoc-string`与`assoc`非常相似，只是它忽略了字符串之间的某些差异。参见字符和字符串的比较 Comparison of Characters and Strings。

#### 函数: `rassoc value alist` ####

这个函数返回list中与value value的第一个关联。如果列表中没有关联的CDR等于value，则返回nil。

rassoc类似于assoc，只是它比较每个列表关联的CDR而不是CAR。您可以将其视为反向关联，查找给定值的键。

#### 函数: `assq key alist` ####

这个函数类似于assoc，因为它返回list中key的第一个关联，但它使用`eq`进行比较。如果list中没有关联具有CAR eq到key，则asq返回nil。这个函数比assoc更常用，因为eq比equal更快，而且大多数列表使用符号作为键。参见等式谓词。

``` Elisp
(setq trees '((pine . cones) (oak . acorns) (maple . seeds)))
     ⇒ ((pine . cones) (oak . acorns) (maple . seeds))
(assq 'pine trees)
     ⇒ (pine . cones)
```

另一方面，在键可能不是符号的列表中，assq通常不太有用:

``` Elisp
(setq leaves
      '(("simple leaves" . oak)
        ("compound leaves" . horsechestnut)))

(assq "simple leaves" leaves)
     ⇒ Unspecified; might be nil or ("simple leaves" . oak).
(assoc "simple leaves" leaves)
     ⇒ ("simple leaves" . oak)
```

#### 函数: `alist-get key alist &optional default remove testfn` ####

这个函数类似于assq。它查找第一个 `(key . Value)`，如果找到，则返回该关联的值。如果没有找到关联，则该函数返回default。键与列表元素的比较使用testfn指定的函数，默认为eq。

这是一个通用变量(参见通用变量 Generalized Variables)，可以用`setf`来改变一个值。当使用它来设置值时，可选参数remove non-nil意味着如果新值等于默认值，则从列表中删除键的关联。

#### 函数: `rassq value alist` ####

这个函数返回list中与value value的第一个关联。如果list中没有关联具有CDR eq to value，则返回nil。

rassq类似于assq，只是它比较每个列表关联的CDR而不是CAR。你可以把它想象成反向的赋值，找到给定值的键。

例如:

``` Elisp
(setq trees '((pine . cones) (oak . acorns) (maple . seeds)))
(rassq 'acorns trees)
     ⇒ (oak . acorns)
(rassq 'spores trees)
     ⇒ nil
```

rassq不能搜索存储在元素CDR的CAR中的值:

``` Elisp
(setq colors '((rose red) (lily white) (buttercup yellow)))

(rassq 'white colors)
     ⇒ nil
```

在这种情况下，关联 `(lily white)` 的CDR不是符号 white，而是列表 `(white)`。如果将关联写成点对表示法，则会更清楚:

``` Elisp
(lily white) ≡ (lily . (white))
```

#### 函数: `assoc-default key alist &optional test default` ####

这个函数在列表中搜索匹配的键。对于list的每个元素，它将元素(如果它是一个原子)或元素的CAR(如果它是一个元素)与key进行比较，方法是用两个参数调用test:元素或它的CAR，以及key。参数按此顺序传递，以便您可以使用包含正则表达式的列表的字符串匹配获得有用的结果(请参阅正则表达式搜索)。如果test省略或为nil，则使用equal进行比较。

如果一个列表元素符合此条件，则关联-default返回一个基于该元素的值。如果元素为cons，则该值为元素的CDR。否则，返回值为default。

如果没有列表元素匹配key，则关联-default返回nil。

#### 函数: `copy-alist alist` ####

这个函数返回list的一个两层two-level深度拷贝:它为每个关联创建一个新的副本，这样你就可以在不改变旧列表的情况下改变新列表的关联。

``` Elisp
(setq needles-per-cluster
      '((2 . ("Austrian Pine" "Red Pine"))
        (3 . ("Pitch Pine"))

        (5 . ("White Pine"))))
⇒
((2 "Austrian Pine" "Red Pine")
 (3 "Pitch Pine")
 (5 "White Pine"))

(setq copy (copy-alist needles-per-cluster))
⇒
((2 "Austrian Pine" "Red Pine")
 (3 "Pitch Pine")
 (5 "White Pine"))

(eq needles-per-cluster copy)
     ⇒ nil
(equal needles-per-cluster copy)
     ⇒ t
(eq (car needles-per-cluster) (car copy))
     ⇒ nil
(cdr (car (cdr needles-per-cluster)))
     ⇒ ("Pitch Pine")

(eq (cdr (car (cdr needles-per-cluster)))
    (cdr (car (cdr copy))))
     ⇒ t

```

这个例子展示了copyalist如何在不影响另一个副本的情况下改变一个副本的关联:

``` Elisp
(setcdr (assq 3 copy) '("Martian Vacuum Pine"))
(cdr (assq 3 needles-per-cluster))
     ⇒ ("Pitch Pine")
```

#### 函数: `assq-delete-all key alist` ####

这个函数从list中删除CAR为eq to key的所有元素，就像使用delq逐个删除每个这样的元素一样。它返回缩短的列表，并且经常修改list的原始列表结构。为了得到正确的结果，使用assq-delete-all的返回值，而不是查看list的保存值。

``` Elisp
(setq alist (list '(foo 1) '(bar 2) '(foo 3) '(lose 4)))
     ⇒ ((foo 1) (bar 2) (foo 3) (lose 4))
(assq-delete-all 'foo alist)
     ⇒ ((bar 2) (lose 4))
alist
     ⇒ ((foo 1) (bar 2) (lose 4))
```

#### 函数: `assoc-delete-all key alist &optional test` ####

这个函数类似于`assq-delete-all`，不同之处在于它接受一个可选参数test，这是一个比较list中的键的谓词函数。如果省略或为nil，则test默认为equal。作为assq-delete-all，这个函数经常修改list的原始列表结构。

#### 函数: `rassq-delete-all value alist` ####

此函数从列表中删除CDR为eq to value的所有元素。它返回缩短的列表，并且经常修改list的原始列表结构。`rassq-delete-all`类似于 `assq-delete-all`，只是它比较每个列表关联的CDR而不是CAR。

#### 宏: `let-alist alist body` ####

为关联列表列表中用作键的每个符号创建绑定，前缀为点。当访问同一关联列表中的多个项目时，这可能很有用，通过一个简单的例子可以最好地理解它:

``` Elisp
(setq colors '((rose . red) (lily . white) (buttercup . yellow)))
(let-alist colors
  (if (eq .rose 'red)
      .lily))
     ⇒ white
```

在编译时检查 body，并且只检查代码体中带有 `.` 的符号。作为符号名称中的第一个字符将被绑定。查找键是用`assq`完成的，这个`assq`的返回值的cdr被分配为绑定的值。

支持嵌套关联列表:

``` Elisp
(setq colors '((rose . red) (lily (belladonna . yellow) (brindisi . pink))))
(let-alist colors
  (if (eq .rose 'red)
      .lily.belladonna))
     ⇒ yellow
```

允许 `let-list` 相互嵌套，但是内部 `let-list`中的代码不能访问外部 `let-list`绑定的变量。

“键”的这种用法与术语“键序列”无关;它表示用于查找表中的项的值。在本例中，表是列表，列表关联是项。

#### 5.9 Property Lists ####

属性列表(简称*plist*)是成对元素的列表。每一对都将一个属性名(通常是一个符号)与一个属性或值关联起来。下面是一个属性列表的例子:

``` Elisp
(pine cones numbers (1 2 3) color "blue")
```

这个属性列表将 pine 与 cones 关联起来，将 numbers 与 (1 2 3) 关联起来，将color与 "blue" 关联起来。属性名和值可以是任何Lisp对象，但名称通常是符号(就像在这个例子中一样)。

属性列表在几个上下文中使用。例如，`put-text-property` 函数接受一个参数，该参数是一个属性列表，指定将应用于字符串或缓冲区中的文本的文本属性和相关值。参见文本属性 Text Properties。

属性列表的另一个重要用途是存储符号属性。每个符号都有一个属性列表，用来记录有关该符号的各种信息;这些属性以属性列表的形式存储。参见符号属性 Symbol Properties。

#### 函数: `plistp object` ####

如果对象是有效的属性列表，则此谓词函数返回非空值。


* Property Lists and Association Lists
* Property Lists Outside Symbols

#### 5.9.1 Property Lists and Association Lists ####

关联列表(参见关联列表)与属性列表非常相似。与关联列表相反，属性列表中对的顺序并不重要，因为属性名称必须不同。

属性列表比关联列表更适合将信息附加到各种Lisp函数名或变量上。如果您的程序将所有这些信息保存在一个关联列表中，则每次检查特定Lisp函数名或变量的关联时，通常需要搜索整个列表，这可能很慢。相比之下，如果在函数名或变量本身的属性列表中保留相同的信息，则每次搜索将只扫描一个属性列表的长度，该属性列表通常很短。这就是为什么变量的文档记录在名为variable-documentation的属性中。字节编译器同样使用属性来记录那些需要特殊处理的函数。

然而，关联列表也有其自身的优势。根据您的应用程序，将关联添加到关联列表的前面可能比更新属性更快。一个符号的所有属性都存储在相同的属性列表中，因此属性名称的不同用法之间有可能发生冲突。(由于这个原因，最好选择可能唯一的属性名，例如在属性名的开头加上程序中变量和函数的常用名称前缀。)关联列表可以像堆栈一样使用，其中关联被推到列表的前面，然后被丢弃;这在属性列表中是不可能的。

#### 5.9.2 Property Lists Outside Symbols ####

以下函数可用于操作属性列表。它们都默认使用eq来比较属性名。

#### 函数: `plist-get plist property &optional predicate` ####

这将返回存储在属性列表列表中的属性属性的值。比较使用默认为eq的predicate完成。它接受一个格式错误的plist参数。如果在列表中没有找到属性，则返回nil。例如,

``` Elisp
(plist-get '(foo 4) 'foo)
     ⇒ 4
(plist-get '(foo 4 bad) 'foo)
     ⇒ 4
(plist-get '(foo 4 bad) 'bad)
     ⇒ nil
(plist-get '(foo 4 bad) 'bar)
     ⇒ nil
```

#### 函数: `plist-put plist property value &optional predicate` ####

这将值存储为属性列表 plist 中属性 property 的值。比较使用默认值为eq的predicate完成。它可以破坏性地修改plist，也可以构造一个新的列表结构而不改变旧的列表结构。该函数返回修改后的属性列表，因此您可以将其存储回获得plist的位置。例如,

``` Elisp
(setq my-plist (list 'bar t 'foo 4))
     ⇒ (bar t foo 4)
(setq my-plist (plist-put my-plist 'foo 69))
     ⇒ (bar t foo 69)
(setq my-plist (plist-put my-plist 'quux '(a)))
     ⇒ (bar t foo 69 quux (a))
```

#### 函数: `lax-plist-get plist property` ####

这个过时的函数obsolete function类似于plist-get，只是它使用equal而不是eq来比较属性。

#### 函数: `lax-plist-put plist property value` ####

这个过时的函数类似于plist-put，只是它使用equal而不是eq来比较属性。

#### 函数: `plist-member plist property &optional predicate` ####

如果plist包含给定的属性，则返回非nil。比较使用谓词完成，默认为eq。与plist-get不同，它允许您区分缺失属性和值为nil的属性。该值实际上是 car 为property的plist的尾部。

## 6 Sequences, Arrays, and Vectors ##

[Sequences Arrays and Vectors](https://www.gnu.org/software/emacs/manual/html_node/elisp/Sequences-Arrays-Vectors.html)

序列类型 *sequence* 是另外两种Lisp类型:列表list和数组array的并集。换句话说，任何列表都是一个序列，任何数组都是一个序列。所有序列都有一个共同的特性，那就是每个序列都是元素的有序集合。

数组是固定长度的对象，每个元素都有一个槽。所有元素都可以在常数时间内访问。数组的四种类型是字符串、向量、字符表和布尔向量。

列表是一个元素序列，但它不是一个单一的原始对象;它由两个单元组成，每个单元一个单元。查找第n个元素需要查找n个cons单元格，因此访问距离列表开头较远的元素需要更长的时间。但是可以向列表中添加元素，或者删除元素。

下图显示了这些类型之间的关系:

``` PlainText
          _____________________________________________
         |                                             |
         |          Sequence                           |
         |  ______   ________________________________  |
         | |      | |                                | |
         | | List | |             Array              | |
         | |      | |    ________       ________     | |
         | |______| |   |        |     |        |    | |
         |          |   | Vector |     | String |    | |
         |          |   |________|     |________|    | |
         |          |  ____________   _____________  | |
         |          | |            | |             | | |
         |          | | Char-table | | Bool-vector | | |
         |          | |____________| |_____________| | |
         |          |________________________________| |
         |_____________________________________________|
```

* Sequences
* Arrays
* Functions that Operate on Arrays
* Vectors
* Functions for Vectors
* Char-Tables
* Bool-vectors
* Managing a Fixed-Size Ring of Objects

### 6.1 Sequences ###

本节描述了接受任何类型序列的函数。

#### 函数: `sequencep object` ####

如果对象是列表、向量、字符串、bool-vector或char-table，则返回t，否则返回nil。另请参见下面的章节。

#### 函数: `length sequence` ####

这个函数返回按顺序排列的元素个数。如果实参不是序列或点列表，则该函数会发出错误类型实参错误信号;如果实参是循环列表，则表示循环列表错误。对于字符表，返回的值总是比最大Emacs字符码多一个。

相关函数safe-length请参见安全长度的定义。

``` Elisp
(length '(1 2 3))
    ⇒ 3

(length ())
    ⇒ 0

(length "foobar")
    ⇒ 6

(length [1 2 3])
    ⇒ 3

(length (make-bool-vector 5 nil))
    ⇒ 5
```

另请参阅文本表示中的字符串字节。

如果您需要计算显示的字符串的宽度，您应该使用string-width(参见显示文本的大小)，而不是length，因为length只计算字符的数量，但不考虑每个字符的显示宽度。

#### 函数: `length< sequence length` ####

如果序列小于length则返回非nil。如果序列是一个长列表，这可能比计算序列的长度更有效。

#### 函数: `length> sequence length` ####

如果序列长度大于length则返回非nil。

#### 函数: `length= sequence length` ####

如果序列的长度等于length，则返回非nil。

#### 函数: `elt sequence index` ####

这个函数返回按index索引的序列的元素。合法的index值是0到小于序列长度1的整数。如果sequence是一个列表，则超出范围的值的行为与 `nth` 相同。参见nth的定义。否则，超出范围的值将触发 `args-out-of-range`错误。

``` Elisp
(elt [1 2 3 4] 2)
     ⇒ 3

(elt '(1 2 3 4) 2)
     ⇒ 3

;; We use string to show clearly which character elt returns.
(string (elt "1234" 2))
     ⇒ "3"

(elt [1 2 3 4] 4)
     error→ Args out of range: [1 2 3 4], 4

(elt [1 2 3 4] -1)
     error→ Args out of range: [1 2 3 4], -1

(elt '(1 2 3) -1)
;  1
```

这个函数泛化了 `aref`(参见对数组进行操作的函数)和nth(参见nth的定义)。

#### 函数: `copy-sequence seqr` ####

这个函数返回一个seqr的副本，它应该是一个序列或一条记录。副本是与原始对象相同类型的对象，并且具有相同顺序的相同元素。但是，如果seqr为空，如字符串或长度为零的向量，则此函数返回的值可能不是副本，而是与seqr相同类型的空对象。

将新元素存储到副本中不会影响原始seqr，反之亦然。然而，副本的元素不是副本;它们与原版本的元素完全相同。因此，在这些元素中所做的更改(通过副本发现)在原始元素中也是可见的。

如果参数是具有文本属性的字符串，则副本中的属性列表本身就是副本，不与原始属性列表共享。但是，属性的实际值是共享的。参见文本属性。

此函数不适用于点列表。试图复制循环列表可能会导致无限循环。

有关复制序列的其他方法，请参见《构建Cons单元格和列表》中的append，《创建字符串》中的concat，以及《vector函数》中的vconcat。

``` Elisp
(setq bar (list 1 2))
     ⇒ (1 2)

(setq x (vector 'foo bar))
     ⇒ [foo (1 2)]

(setq y (copy-sequence x))
     ⇒ [foo (1 2)]


(eq x y)
     ⇒ nil

(equal x y)
     ⇒ t

(eql x y)
;  nil

(eq (elt x 1) (elt y 1))
     ⇒ t


;; Replacing an element of one sequence.
(aset x 0 'quux)
x ⇒ [quux (1 2)]
y ⇒ [foo (1 2)]


;; Modifying the inside of a shared element.
(setcar (aref x 1) 69)
x ⇒ [quux (69 2)]
y ⇒ [foo (69 2)]

```

#### 函数: `reverse sequence` ####

这个函数创建一个新的序列，它的元素是sequence的元素，但是顺序相反。原始参数序列不会改变。注意，char-tables不能反转。

``` Elisp
(setq x '(1 2 3 4))

(reverse x)
;  (4 3 2 1)

x
(1 2 3 4)

(setq x [1 2 3 4])

(reverse x)
;  [4 3 2 1]

x
;  [1 2 3 4]

(setq x "xyzzy")
(reverse x)
;  "yzzyx"
x
; "xyzzy"

```

#### 函数: `nreverse sequence` ####

这个函数颠倒sequence中元素的顺序。与反向不同，原始序列可以被修改。

例如:

``` Elisp
(setq x (list 'a 'b 'c))
;  (a b c)

x
;  (a b c)
(nreverse x)
;  (c b a)

;; The cons cell that was first is now last
x
;  (a)
```

为了避免混淆，我们通常将nreverse操作的结果存储回保存原始列表的变量中:

``` Elisp
(setq x (nreverse x))
```

这是我们最喜欢的例子(a b c)的 nreverse，用图形表示:

``` PlainText
Original list head:                       Reversed list:
 -------------        -------------        ------------
| car  | cdr  |      | car  | cdr  |      | car | cdr  |
|   a  |  nil |<--   |   b  |   o  |<--   |   c |   o  |
|      |      |   |  |      |   |  |   |  |     |   |  |
 -------------    |   --------- | -    |   -------- | -
                  |             |      |            |
                   -------------        ------------
```

对于vector，它甚至更简单，因为你不需要setq:

``` Elisp
(setq x (copy-sequence [1 2 3 4]))
;  [1 2 3 4]
(nreverse x)
;  [4 3 2 1]
x
;  [4 3 2 1]
```

请注意，与reverse不同，此函数不适用于字符串。尽管您可以通过使用asset来更改字符串数据，但强烈建议将字符串视为不可变的，即使它们是可变的。见 Mutability。

#### 函数: `sort sequence predicate` ####

这个函数对序列进行稳定排序。注意，这个函数并不适用于所有序列;它只能用于 **列表和向量**。如果sequence是一个列表，则对其进行破坏性修改。该函数返回排序后的序列，并使用谓词比较元素。稳定排序是指具有相同排序键的元素在排序前后保持其相对顺序。当使用逐次排序根据不同的准则对元素排序时，稳定性是很重要的。

参数谓词必须是一个接受两个参数的函数。它被称为具有两个元素的序列。要获得递增顺序排序，如果第一个元素 **小于** 第二个元素，谓词应该返回非nil，否则返回nil。

比较函数谓词必须为任何给定的参数对提供可靠的结果，至少在一个sort调用中是这样。它必须是反对称的;也就是说，如果a小于b, b一定不小于a。它必须是可传递的——也就是说，如果a小于b, b小于c，那么a一定小于c。如果你使用不满足这些要求的比较函数，排序的结果是不可预测的。

列表排序的破坏性方面是，它通过改变它们的内容(可能以不同的顺序重新排列它们)来重用形成序列的单元格。这意味着排序后输入列表的值是未定义的;只有排序返回的列表具有定义良好的值。例子:

``` Elisp
(setq nums (list 2 1 4 3 0))
(sort nums #'<)  ;  #'< 是函数的缩写
;  (0 1 2 3 4)
;  nums is unpredicable at this point
```

大多数情况下，我们将结果存储回保存原始列表的变量中:

``` Elisp
(setq nums (sort nums #'<))
```

如果你希望在不破坏原稿的情况下制作一份已排序的复本，请先将复本复制，然后再排序:

``` Elisp
(setq nums (list 2 1 4 3 0))
(sort (copy-sequence nums) #'<)
;  (0 1 2 3 4)

nums
;  (2 1 4 3 0)
```

为了更好地理解什么是稳定排序，请考虑下面的向量示例。排序后，所有car为8的项被分组在vector的开头，但保持它们的相对顺序。所有car为9的项都分组在vector的末尾，但它们的相对顺序也保持不变:

``` Elisp
(setq
  vector
  (vector '(8 . "xxx") '(9 . "aaa") '(8 . "bbb") '(9 . "zzz")
          '(9 . "ppp") '(8 . "ttt") '(8 . "eee") '(9 . "fff")))
;  [(8 . "xxx") (9 . "aaa") (8 . "bbb") (9 . "zzz") (9 . "ppp") (8 . "ttt") (8 . "eee") (9 . "fff")]

(sort vector (lambda (x y) (< (car x) (car y))))
;  [(8 . "xxx") (8 . "bbb") (8 . "ttt") (8 . "eee") (9 . "aaa") (9 . "zzz") (9 . "ppp") (9 . "fff")]
```

有关执行排序的更多函数，请参阅排序文本 Sorting Text。有关排序的有用示例，请参阅访问文档字符串中的文档 Access to Documentation Strings。

`seq.el` 库提供了以下附加的序列操作宏和函数，前缀为 `seq-`。

本库中定义的所有函数都没有副作用;也就是说，它们不会修改作为参数传递的任何序列(列表、向量或字符串)。除非另有说明，否则结果是与输入相同类型的序列。对于那些接受谓词的函数，这应该是一个只有一个参数的函数。

`seq.el` 可以扩展库以处理其他类型的顺序数据结构。为此，所有函数都使用 `cl-defgeneric` 定义。有关使用 `cl-defgeneric` 添加扩展的详细信息，请参阅泛型函数 Generic Functions。

#### 函数: `seq-elt sequence index` ####

此函数返回序列在指定索引处的元素，该元素是一个整数，其有效值范围是0到比序列长度小1。对于内置序列类型上的超出范围的值，seq-elt的行为类似于elt。详细信息请参见elt的定义。

``` Elisp
(seq-elt [1 2 3 4] 2)
;  3
```

`seq-elt` 返回可使用 setf 设置的位置place(参见setf宏)。

``` Elisp
(setq vec [1 2 3 4])
;  [1 2 3 4]
(setf (seq-elt vec 2) 5)
;  5
vec
;  [1 2 5 4]
```

#### 函数: `seq-length sequence` ####

这个函数返回 sequence 中的元素个数。对于内置序列类型，`seq-length` 的行为类似于length。参见长度的定义。

#### 函数: `seqp object` ####

如果object是一个序列(列表或数组)，或通过 seq.el 中的泛型函数定义的任何其他类型的序列，则此函数返回非nil。这是sequencep的可扩展变体。

``` Elisp
(seqp [1 2])
;  t
(seqp 2)
;  nil
```

#### 函数: `seq-drop sequence n` ####

这个函数返回 sequence 中除前n个(整数)元素外的所有元素。如果n为负或为零，则结果为 sequence。

``` Elisp
(seq-drop [1 2 3 4 5 6] 3)
;  [4 5 6]
(seq-drop "hello world" -4)
;  "hello world"
```

#### 函数: `seq-take sequence n` ####

这个函数返回序列的前n个(整数)元素。如果n为负或为零，则结果为nil。

``` Elisp
(seq-take '(1 2 3 4) 3)
;  (1 2 3)

(seq-take [1 2 3 4] 0)
;  []
```

#### 函数: `seq-take-while predicate sequence` ####

这个函数按顺序返回序列的成员，在谓词返回nil的第一个成员之前停止。

``` Elisp
(seq-take-while (lambda (elt) (> elt 0)) '(1 2 3 -1 -2 1))
;  (1 2 3)

(seq-take-while (lambda (elt) (> elt 0)) [-1 4 6])
;  []
```

#### 函数: `seq-drop-while predicate sequence` ####

该函数按顺序返回序列的成员，从谓词返回nil的第一个成员开始, 到最后全部返回。

``` Elisp
(seq-drop-while (lambda (elt) (> elt 0)) '(1 2 3 -1 -2 1 -1))
;  (-1 -2 1 -1)

(seq-drop-while (lambda (elt) (< elt 0)) [1 4 6 -1])
;  [1 4 6 -1]
```

#### 函数: `seq-split sequence length` ####

emacs 28.2 没有这个函数

这个函数返回一个由(最多)长度为length的序列的子序列组成的列表。(如果序列的长度不是length的倍数，则最后一个元素可能比length短。

``` Elisp
(seq-split [0 1 2 3 4] 2)
```

#### 函数: `seq-do function sequence` ####

这个函数依次对sequence中的每个元素应用function(可能是为了产生副作用)，并返回sequence。

这个函数不会用

``` Elisp
(setq nums '(2 4 6))
(seq-do #'1+ nums)
;  (2 4 6)
nums
;  (2 4 6)
```

#### 函数: `seq-map function sequence` ####

这个函数返回对序列的每个元素应用函数的结果。返回值是一个列表。

``` Elisp
(seq-map #'1+ '(2 4 6))
;  (3 5 7)

(seq-map #'symbol-name [foo bar])
;  ("foo" "bar")
```

#### 函数: `seq-map-indexed function sequence` ####

这个函数返回将函数应用于sequence的每个元素及其在seq中的索引的结果。返回值是一个列表。

``` Elisp
(seq-map-indexed (lambda (elt idx)
                   (list idx elt))
				   '(a b c))
;  ((0 a) (1 b) (2 c))
```

#### 函数: `seq-mapn function &rest sequences` ####

这个函数返回对序列的每个元素应用函数的结果。函数的arity(参见 `sub-arity`)必须匹配序列的个数。映射在最短序列的末尾停止，返回值是一个列表。

``` Elisp
(seq-mapn #'+ '(2 4 6) '(20 40 60))
;  (22 44 66)

(seq-mapn #'concat '("moskito" "bite") '("bee" "sting"))
;  ("moskitobee" "bitesting")
```

#### 函数: `seq-filter predicate sequence` ####

这个函数返回一个序列中所有谓词返回非nil的元素的列表。

``` Elisp
(seq-filter (lambda (elt) (> elt 0)) [1 -1 3 -3 5])
;  (1 3 5)

(seq-filter (lambda (elt) (> elt 0)) '(-1 -3 -5))
;  nil
```

#### 函数: `seq-remove predicate sequence` ####

这个函数返回一个序列中所有谓词返回nil的元素的列表。

``` Elisp
(seq-remove (lambda (elt) (> elt 0)) [1 -1 3 -3 5])
;  (-1 -3)

(seq-remove (lambda (elt) (< elt 0)) '(-1 -3 -5))
;  nil
```

#### 函数: `seq-remove-at-position sequence n` ####

这个函数返回一个序列的副本，其中索引为(从零开始)n的元素被删除。结果是一个与sequence类型相同的序列。

emacs 28.2 没有这个函数

``` Elisp
(seq-remove-at-position [1 -1 3 -3 5] 0)
⇒ [-1 3 -3 5]

(seq-remove-at-position [1 -1 3 -3 5] 3)
⇒ [1 -1 3 5]
```

#### 函数: `seq-keep function sequence` ####

这个函数返回所有非nil结果的列表，这些非nil结果来自于对序列中的元素调用函数。

emacs 28.2 没有这个函数

``` Elisp
(seq-keep #'cl-digit-char-p '(?6 ?a ?7))
⇒ (6 7)
```

#### 函数: `seq-reduce function sequence initial-value` ####

这个函数返回的结果是:调用带有initial-value的函数并调用sequence的第一个元素，然后调用带有该结果和sequence的第二个元素的函数，然后调用带有该结果和sequence的第三个元素的函数，等等。function应该是一个有两个参数的函数。

函数调用时带两个参数。初始值(然后是累积值)用作第一个参数，序列中的元素用于第二个参数。

如果sequence为空，则返回初始值而不调用function。

``` Elisp
(seq-reduce #'+ [1 2 3 4] 0)
;  10
(seq-reduce #'+ '(1 2 3 4) 5)
;  15

(seq-reduce #'+ '() 3)
;  3

(seq-reduce #'* '(1 2 3 4) 1)
;  24
```

#### 函数: `seq-some predicate sequence` ####

该函数通过对序列的每个元素依次应用谓词返回第一个非nil值。

``` Elisp
(seq-some #'numberp ["abc" 1 nil])
;  t

(seq-some #'numberp ["abc" "def"])
;  nil

(seq-some #'null ["abc" 1 nil])
;  t

(seq-some #'1+ [2 4 6])
;  3
```

#### 函数: `seq-find predicate sequence &optional default` ####

这个函数返回序列中谓词返回非nil的第一个元素。如果没有元素匹配谓词，则返回default。

请注意，如果找到的元素与默认值相同，则此函数具有歧义性，因为在这种情况下，无法知道是否找到了元素。

``` Elisp
(seq-find #'numberp ["abc" 1 nil])
;  1

(seq-find #'numberp ["abc" "def"])
;  nil
```

#### 函数: `seq-every-p predicate sequence` ####

如果对序列的每个元素应用谓词返回非nil，则此函数返回非nil。

``` Elisp
(seq-every-p #'numberp [2 4 6])
;  t

(seq-every-p #'numberp [2 4 ?6])
;  t

(seq-every-p #'numberp [2 4 "6"])
;  nil
```

#### 函数: `seq-empty-p sequence` ####

如果序列为空，则此函数返回非nil。

``` Elisp
(seq-empty-p "not empty")
;  nil
(seq-empty-p "")
;  t
```

#### 函数: `seq-count predicate sequence` ####

此函数返回谓词返回非nil的序列中元素的个数。

``` Elisp
(seq-count (lambda (elt) (> elt 0)) [-1 2 0 3 -2])
;  2
```

#### 函数: `seq-sort function sequence` ####

这个函数返回一个序列的副本，该序列按照函数排序，函数有两个参数，如果第一个参数应该在第二个参数之前排序，则返回非nil。

#### 函数: `seq-sort-by function predicate sequence` ####

这个函数类似于seq-sort，但是sequence的元素是通过在排序之前对它们应用函数来转换的。函数是只有一个参数的函数。

``` Elisp
(seq-sort-by #'seq-length #'> ["a" "ab" "abc"])
;  ["abc" "ab" "a"]
```

#### 函数: `seq-contains-p sequence elt &optional function` ####

如果序列中至少有一个元素等于elt，则此函数返回非nil。如果可选参数函数非nil，则它是一个包含两个参数的函数，而不是默认的相等参数。

``` Elisp
(seq-contains-p '(symbol1 symbol2) 'symbol1)
⇒ t

⇒ t

(seq-contains-p '(symbol1 symbol2) 'symbol3)
⇒ nil
```

#### 函数: `seq-set-equal-p sequence1 sequence2 &optional testfn` ####

这个函数检查sequence1和sequence2是否包含相同的元素，而不管顺序如何。如果可选参数testfn非nil，则它是一个包含两个参数的函数，而不是使用默认的相等参数。

``` Elisp
(seq-set-equal-p '(a b c) '(c b a))
;  t

(seq-set-equal-p '(a b c) '(c b))
;  nil

(seq-set-equal-p '("a" "b" "c") '("c" "b" "a"))
;  t

(seq-set-equal-p '("a" "b" "c") '("c" "b" "a") #'eq)
;  nil
```

#### 函数: `seq-position sequence elt &optional function` ####

这个函数返回序列中第一个等于elt的元素的(从零开始的)索引。如果可选参数函数非nil，则它是一个包含两个参数的函数，而不是默认的相等参数。

``` Elisp
(seq-position '(a b c) 'b)
;  1

(seq-position '(a b c) 'd)
;  nil
```

#### 函数: `seq-positions sequence elt &optional testfn` ####

这个函数返回一个序列中元素(从零开始)索引的列表，testfn在传递元素和elt作为参数时返回非nil。Testfn默认为equal。

emacs 28.2 没有这个函数

``` Elisp
(seq-positions '(a b c a d) 'a)
⇒ (0 3)

(seq-positions '(a b c a d) 'z)
⇒ nil

(seq-positions '(11 5 7 12 9 15) 10 #'>=)
⇒ (0 3 5)
```

#### 函数: `seq-uniq sequence &optional function` ####

这个函数返回一个删除重复项的序列元素列表。如果可选参数函数非nil，则它是一个包含两个参数的函数，而不是默认的相等参数。

``` Elisp
(seq-uniq '(1 2 2 1 3))
;  (1 2 3)

(seq-uniq '(1 2 2.0 1.0) #'=)
; (1 2)
```

#### 函数: `seq-subseq sequence start &optional end` ####

这个函数从开始到结束返回序列的子集，都是整数(end默认为最后一个元素)。如果start或end为负数，则从序列的末尾开始计数。

``` Elisp
(seq-subseq '(1 2 3 4 5) 1)
;  (2 3 4 5)

(seq-subseq '[1 2 3 4 5] 1 3)
;  [2 3]

(seq-subseq '[1 2 3 4 5] -3 -1)
;  [3 4]
```

#### 函数: `seq-concatenate type &rest sequences` ####

这个函数返回由多个序列串联而成的type类型的序列。类型可以是:vector、list或string。

``` Elisp
(seq-concatenate 'list '(1 2) '(3 4) [5 6])
;  (1 2 3 4 5 6)

(seq-concatenate 'string "Hello " "world")
;  "Hello world"

(seq-concatenate 'string '(88 89 90) "Hello")
;  "XYZHello"

(seq-concatenate 'list "Hello")
;  (72 101 108 108 111)
```

#### 函数: `seq-mapcat function sequence &optional type` ####

这个函数返回将seq-concatenate应用于将function应用于sequence的每个元素的结果。结果是type类型的序列，如果type为nil则为列表。

``` Elisp
(seq-mapcat #'seq-reverse '((3 2 1) (6 5 4)))
;  (1 2 3 4 5 6)
```

#### 函数: `seq-partition sequence n` ####

这个函数返回一个序列的元素列表，其中的元素被分组为长度为n的子序列。最后一个序列包含的元素可以少于n。如果n是负整数或0，则返回值为nil。

``` Elisp
(seq-partition '(0 1 2 3 4 5 6 7) 3)
;  ((0 1 2) (3 4 5) (6 7))
```

#### 函数: `seq-union sequence1 sequence2 &optional function` ####

这个函数返回出现在sequence1或sequence2中的元素列表。返回列表的元素都是唯一的，也就是说没有两个元素比较起来相等。如果可选参数函数为非nil，则它应该是一个包含两个参数的函数，用于比较元素，而不是默认的相等。

并集

``` Elisp
(seq-union [1 2 3] [3 5])
;  (1 2 3 5)
```

#### 函数: `seq-intersection sequence1 sequence2 &optional function` ####

这个函数返回一个同时出现在sequence1和sequence2中的元素列表。如果可选参数函数为非nil，则它是一个包含两个参数的函数，用于比较元素，而不是默认的相等。

交集

``` Elisp
(seq-intersection [2 3 4 5] [1 3 5 6 7])
;  (3 5)
```

#### 函数: `seq-difference sequence1 sequence2 &optional function` ####

此函数返回出现在sequence1中但未出现在sequence2中的元素列表。如果可选参数函数为非nil，则它是一个包含两个参数的函数，用于比较元素，而不是默认的相等。

差

``` Elisp
(seq-difference '(2 3 4 5) [1 3 5 6 7])
;  (2 4)
```

#### 函数: `seq-group-by function sequence` ####

这个函数将sequence中的元素分离成一个列表，该列表的键是对sequence中的每个元素应用function的结果。使用equal对键进行比较。

按照 function 对每个元素运算的结果分类

``` Elisp
(seq-group-by #'integerp '(1 2.1 3 2 3 3.2))
;  ((t 1 3 2 3) (nil 2.1 3.2))

(seq-group-by #'car '((a 1) (b 2) (a 3) (c 4)))
;  ((b (b 2)) (a (a 1) (a 3)) (c (c 4)))
```

#### 函数: `seq-into sequence type` ####

这个函数将序列 sequence 转换为类型为type的序列。类型可以是下列符号之一:vector、string或list。

``` Elisp
(seq-into [1 2 3] 'list)
;  (1 2 3)

(seq-into nil 'vector)
;  []

(seq-into "hello" 'vector)
;  [104 101 108 108 111]
```

#### 函数: `seq-min sequence` ####

这个函数返回序列中最小的元素。序列的元素必须是数字或标记(参见标记)。

``` Elisp
(seq-min [3 1 2])
;  1

(seq-min "Hello")
;  72  ?H
```

#### 函数: `seq-max sequence` ####

这个函数返回序列中最大的元素。序列的元素必须是数字或标记。

``` Elisp
(seq-max [1 2 3])
;  3

(seq-max "Hello")
;  111  ?o
```

#### 宏: `seq-doseq (var sequence) body` ####

这个宏类似于dolist(参见dolist)，只不过序列可以是列表、向量或字符串。这主要用于 side-effects。

#### 宏: `seq-let var-sequence val-sequence body…..` ####

这个宏将`var-sequence`中定义的变量绑定到 `val-sequence`中相应元素的值。这就是所谓的解构绑定(*destructuring binding*)。var-sequence的元素本身可以包含序列，从而允许嵌套解构。

var-sequence序列还可以包括&rest标记，后面跟着要绑定到val-sequence其余部分的变量名。

``` Elisp
(seq-let [first second] [1 2 3 4] (list first second))
;  (1 2)

(seq-let (_ a _ b) '(1 2 3 4) (list a b a b))
;  (2 4 2 4)

(seq-let [a [b [c]]] [1 [2 [3]]] (list a b c))
;  (1 2 3)

(seq-let [a b &rest others] [1 2 3 4] others)
;  [3 4]
```

pcase模式为解构绑定提供了另一种工具，请参阅使用pcase模式进行解构 Destructuring with pcase Patterns。

#### 宏: `seq-setq var-sequence val-sequence` ####

这个宏的工作方式类似于seq-let，只是将值赋给变量的方式与setq一样，而不是像let绑定那样。

``` Elisp
(let ((a nil)
      (b nil))
  (seq-setq (_ a _ b) '(1 2 3 4))
  (list a b))
;  (2 4)
```

#### 函数: `seq-random-elt sequence` ####

这个函数返回一个随机取的序列元素。

``` Elisp
(seq-random-elt [1 2 3 4])
;  3
;  1
;  2
```

如果sequence为空，则该函数发出错误信号。

### 6.2 Arrays ###

数组对象具有存放许多其他Lisp对象的槽，这些对象称为数组的元素。数组中的任何元素都可以在常量时间内被访问。相反，访问列表中元素的时间与该元素在列表中的位置成正比。

Emacs定义了四种数组类型，它们都是一维的:

* 字符串(请参阅字符串类型)
* 向量(请参阅向量类型)
* 布尔向量(请参阅布尔向量类型)
* char-tables(请参阅Char-Table类型)

vector和char-table可以保存任何类型的元素，但是字符串只能保存字符，而bool-vector只能保存t和nil。

这四种数组都有以下特点:

* 数组的第一个元素的索引为0，第二个元素的索引为1，依此类推。这被称为零源索引。例如，一个包含四个元素的数组的下标为0、1、2和3。
* 一旦你创建了数组，它的长度就固定了;不能更改已存在数组的长度。
* 为了求值evaluation，数组是一个常量，即:，则求值为自身。
* 数组的元素可以分别通过函数`aref`和`aset`来引用或修改(参见对数组进行操作的函数 Functions that Operate on Arrays)。

当您创建一个数组，而不是一个字符表时，您必须指定它的长度。您不能指定字符表的长度，因为这是由字符代码的范围决定的。

原则上，如果需要一个文本字符数组，可以使用字符串或向量。在实践中，我们总是为这样的应用程序选择字符串，原因有四个:

1. 它们所占的空间是相同元素向量的四分之一。
2. 字符串以一种更清晰地显示文本内容的方式打印。
3. 字符串可以保存文本属性。参见文本属性。
4. Emacs的许多专门的编辑和I/O工具只接受字符串。例如，不能像插入字符串那样将字符向量插入缓冲区。请参阅字符串和字符。

相比之下，对于键盘输入字符数组(如键序列)，可能需要向量，因为许多键盘输入字符超出了字符串的范围。参见键序列输入 Key Sequence Input。

### 6.3 Functions that Operate on Arrays ###

在本节中，我们将描述接受所有类型数组的函数。

#### 函数: `arrayp object` ####

如果object是数组(即向量、字符串、bool-vector或char-table)，则此函数返回t。

``` Elisp
(arrayp [a])
;  t

(arrayp "asdf")
;  t
(arrayp (syntax-table))  ;; A char-table
;  t
```

#### 函数: `aref arr index` ####

这个函数返回数组或record arr的第indexth元素。第一个元素在索引0处。

``` Elisp
(setq primes [2 3 5 7 11 13])
;  [2 3 5 7 11 13]
(aref primes 4)
;  11

(aref "abcdefg" 1)
;  98  ?b
```

#### 函数: `aset array index object` ####

这个函数将数组的第一个元素设置为object。它返回object。

``` Elisp
(setq w (vector 'foo 'bar 'baz))
;  [foo bar baz]
(aset w 0 'fu)
;  fu
w
;  [fu bar baz]
(aset w 3 'op)
;  args-out-of-range [fu bar baz] 3)

;; copy-sequence copies the string to be modified later.
(setq x (copy-sequence "asdfasdf"))
;  "asdfasdf"
(aset x 3 ?Z)
;  90  ?Z
x
;  "asdZasdf"
```

数组应该是可变的。看到可变性Mutability。

如果array是字符串而object不是字符，则会导致错误的类型参数错误。如果需要插入字符，该函数将单字节字符串转换为多字节字符串。

#### 函数: `fillarray array object` ####

这个函数用object填充数组数组，使数组的每个元素都是object。它返回数组。

``` Elisp
(setq a (copy-sequence [a b c d e f g]))
;  [a b c d e f g]
(fillarray a 0)
;  [0 0 0 0 0 0 0]
a
;  [0 0 0 0 0 0 0]

(setq s (copy-sequence "When in the course"))
;  "When in the course"
(fillarray s ?-)
;  "------------------"
```

如果array是字符串而object不是字符，则会导致错误的类型参数错误。

一般的序列函数copy-sequence和length对于已知为数组的对象通常很有用。见序列。

### 6.4 Vectors ###

vector是一种通用数组，其元素可以是任何Lisp对象。(相比之下，字符串的元素只能是字符。请参阅字符串和字符。)

向量在Emacs中有很多用途:

* 作为键序列(参见键序列 Key Sequences)
* 作为符号查找表(参见创建和修改符号 Creating and Interning Symbols)
* 作为字节编译函数表示的一部分(参见字节编译 Byte Compilation)

与其他数组一样，向量使用零源索引:第一个元素的索引为0。

向量在元素周围用方括号打印。因此，元素为符号a、b和a的向量被打印为`[a b a]`。您可以在Lisp输入中以相同的方式编写向量。

与字符串或数字一样，vector也被认为是求值的常量:求值的结果是相同的vector。它不计算甚至不检查向量的元素。参见自我评价表格Self Evaluating Forms。用方括号写的向量不应该通过`aset`或其他破坏性操作来修改。看到可变性Mutability。

以下是说明这些原则的例子:

``` Elisp
(setq avector [1 two '(three) "four" [five]])
;  [1 two '(three) "four" [five]]
(eval avector)
;  [1 two '(three) "four" [five]]
(eq avector (eval avector))
;  t
```

### 6.5 Functions for Vectors ###

#### 函数: `vectorp object` ####

如果对象是一个向量，这个函数返回t。

``` Elisp
(vectorp [a])
;  t
(vectorp "asdf")
;  nil
```
#### 函数: `vsctor &rest objects` ####

这个函数创建并返回一个向量，其元素是实参，对象。

``` Elisp
(vector 'foo 23 [bar baz] "rats")
; [foo 23 [bar baz] "rats"]
(vector)
;  []
```

#### 函数: `make-vector length object` ####

这个函数返回一个由length元素组成的新向量，每个元素初始化为object。

``` Elisp
(setq sleepy (make-vector 9 'Z))
;  [Z Z Z Z Z Z Z Z Z]
```

#### 函数: `vconcat &rest sequences` ####

这个函数返回一个包含序列所有元素的新向量。实参序列可以是proper lists、向量、字符串或布尔向量。如果没有给出序列，则返回空向量。

该值要么是空向量，要么是新构造的不等于任何现有向量的非空向量。

``` Elisp
(setq a (vconcat '(A B C) '(D E F)))
;  [A B C D E F]
(eq a (vconcat a))
;  nil

(vconcat)
;  []
(vconcat [A B C] "aa" '(foo (6 7)))
;  [A B C 97 97 foo (6 7)]
```

vconcat函数还允许 byte-code function对象作为参数。这是一个特殊的特性，可以方便地访问字节码函数对象的全部内容。参见字节码函数对象 Byte-Code Function Objects。

有关其他连接函数，请参见映射函数Mapping Functions 中的`mapconcat`，创建字符串中的concat，以及构建Cons单元格和列表中的append。

append函数还提供了一种将vector转换为具有相同元素的list的方法:

``` Elisp
(setq avector [1 two (quote (three)) "four" [five]])
;  [1 two '(three) "four" [five]]
(append avector nil)
;  (1 two '(three) "four" [five])
```

### 6.6 Char-Tables ###

字符表很像矢量，不同之处在于它是由字符代码索引的。任何有效的字符代码(不带修饰符 without modifiers)都可以用作字符表中的索引。您可以使用`aref`和`aset`来访问字符表的元素，就像使用任何数组一样。此外，字符表可以有额外的槽来保存与特定字符代码无关的额外数据。与vector一样，char-table在求值时也是常量，并且可以保存任何类型的元素。

每个char-table都有一个子类型subtyoe(一个符号)，它有两个用途:

1. subtype 提供了一种简单的方法来说明这个char表的用途。例如，`display-table` 是以`display-table`为子类型的char-table，`syntax-table` 是以`syntax-table`为子类型的char-table。可以使用下面描述的 `char-table-subtype` 函数查询子类型。
2. 子类型控制char-table中额外槽的数量。这个数字由子类型的 `char-table-extra-slots` 符号属性指定(参见符号属性Symbol Properties)，其值应该是0到10之间的整数。如果子类型没有这样的符号属性，则char-table没有额外的槽。

一个char-table可以有一个 *parent*，parent 是另一个char-table。如果是这样，那么每当char-table为特定字符c指定nil时，它将继承父元素中指定的值。换句话说，如果char-table本身指定nil，则 `(aref char-table c)` 返回来自char-table父节点的值。

一个字符表char-table也可以有一个默认值。如果是，则 `(aref char-table c)` 在char-table没有指定任何其他非空值时返回默认值。

#### 函数: `make-char-table subtype &optional init` ####

返回一个新创建的字符表，子类型为subtype(一个符号)。每个元素初始化为init，默认为nil。创建char-table后，不能更改其子类型。

没有参数指定char-table的长度，因为所有char-table都有空间容纳任何有效的字符代码作为索引。

如果子类型具有 `char-table-extra-slots` 符号属性，则指定char-table中额外插槽的数量。这应该是一个0到10之间的整数;否则，`make-char-table` 会引发一个错误。如果子类型没有char-table-extra-slots符号属性(参见属性列表 Property Lists)，则该char-table没有额外的slot。

#### 函数: `char-table-p object` ####

如果object是一个char-table，这个函数返回t，否则返回nil。

#### 函数: `char-table-subtype char-table` ####

这个函数返回char-table的子类型符号。

没有特殊的函数可以访问字符表中的默认值。要做到这一点，请使用 `char-table-range`(见下文)。

#### 函数: `char-table-parent char-table` ####

这个函数返回char-table的父元素。父对象总是nil或另一个char-table。

#### 函数: `set-char-table-parent char-table new-parent` ####

这个函数将char-table的父节点设置为new-parent。

#### 函数: `char-table-extra-slot char-table n` ####

这个函数返回char-table的额外槽n(从零开始)的内容。字符表中额外插槽的数量由其子类型决定。

#### 函数: `set-char-table-extra-slot char-table n value` ####

此函数将值存储在char-table的额外槽n(从零开始)中。

字符表可以为单个字符代码指定元素值;它还可以为整个字符集指定一个值。

#### 函数: `char-table-range char-table range` ####

这将返回char-table中指定的字符范围的值。以下是范围的可能性:

* `nil` 默认值
* `char` 引用字符char的元素(假设char是一个有效的字符代码)。
* `(from . to)` cons cell 指的是包含范围 `[from..to]` 中的所有字符。在本例中，函数返回由from指定的字符的值。

#### 函数: `set-char-table-range char-table range value` ####

这个函数在char-table中设置一个字符范围的值。以下是范围的可能性:

* `nil` 默认值
* `t` 指的是字符码的全部范围。
* `char` 引用字符char的元素(假设char是一个有效的字符代码)。
* `(from . to)` cons cell指的是包含范围 `[from..to]` 中的所有字符。

#### 函数: `map-char-table function char-table` ####

此函数为char-table中具有非空值的每个元素调用其参数函数。函数调用有两个参数，一个键和一个值。键是char-table-range的一个可能的范围参数——一个有效字符或一个 cons cell `(from . to)`，指定共享相同值的字符范围。该值是 `(char-table-range char-table key)` 返回的值。

总的来说，传递给函数的键值对描述了存储在char-table中的所有值。

返回值总是nil;为了使对map-char-table的调用有用，函数应该具有副作用。例如，下面是如何检查语法表中的元素:

``` Elisp
(let (accumulator)
   (map-char-table
    (lambda (key value)
      (setq accumulator
            (cons (list
                   (if (consp key)
                       (list (car key) (cdr key))
                     key)
                   value)
                  accumulator)))
    (syntax-table))
   accumulator)
⇒
(((2597602 4194303) (2)) ((2597523 2597601) (3))
 ... (65379 (5 . 65378)) (65378 (4 . 65379)) (65377 (1))
 ... (12 (0)) (11 (3)) (10 (12)) (9 (0)) ((0 8) (3)))
```

### 6.7 Bool-vectors ###

bool-vector与vector非常相似，只不过它只存储值t和nil。如果试图将任何非nil值存储到bool向量的元素中，结果是将t存储在那里。与所有数组一样，布尔向量的索引从0开始，一旦创建了布尔向量，长度就不能改变。布尔向量在求值时是常量。

有几个函数专门处理布尔向量;除此之外，您还可以使用用于其他类型数组的相同函数来操作它们。

#### 函数: `make-bool-vector length initial` ####

返回一个包含长度元素的新bool-vector，每个元素初始化为initial。

#### 函数: `bool-vector &rest objects` ####

这个函数创建并返回一个布尔向量，其元素是实参，对象。

``` Elisp
(bool-vector t t t nil)
;  值为 7, 二进制 1110
(bool-vector t t t nil t nil t t)
;  #&8"\327"
;  也就是?\327, 八进制 #o327, 十六进制 #xd7, 二进制#b11101011
```

#### 函数: `bool-vector-p object` ####

如果object是布尔向量，则返回t，否则返回nil。

还有一些布尔向量集操作函数，描述如下:

#### 函数: `bool-vector-exclusive-or a b &optional c` ####

返回bool向量a和b的位异或。如果给出可选参数c，则此操作的结果存储在c中。所有参数都应该是长度相同的bool向量。

``` Elisp
(setq a (bool-vector t t)
      b (bool-vector nil t))
(bool-vector-exclusive-or a b)
;  #x1, #b01
```

#### 函数: `bool-vector-union a b &optional c` ####

返回bool向量a和b的按位或值。如果给出可选参数c，则此操作的结果存储在c中。所有参数都应该是长度相同的bool向量。

``` Elisp
(bool-vector-union (bool-vector t t)
                   (bool-vector nil t))
;  #x3, #b11
```

#### 函数: `bool-vector-intersection a b &optional c` ####

按位返回bool向量a和b的且。如果给出可选参数c，则此操作的结果存储在c中。所有参数都应该是长度相同的bool向量。

``` Elisp
(bool-vector-intersection (bool-vector t nil)
                          (bool-vector nil t))
```

#### 函数: `bool-vector-set-difference a b &optional c` ####

返回bool向量a和b的集合差值。如果给出可选参数c，则此操作的结果存储在c中。所有参数都应该是相同长度的bool向量。

不懂

``` Elisp
(bool-vector-set-difference (bool-vector t t)
	                        (bool-vector t nil))
;  2, #b10

(bool-vector-set-difference (bool-vector t t t)
	                        (bool-vector t nil t))
;  2, #b010

(bool-vector-set-difference (bool-vector t t)
	                        (bool-vector t t))

(bool-vector-set-difference (bool-vector t t)
	                        (bool-vector nil nil))
;  #b11
```

#### 函数: `bool-vector-not a &optional b` ####

返回bool向量a的集合补集。如果给出了可选参数b，则此操作的结果存储在b中。所有参数都应该是长度相同的bool向量。

#### 函数: `bool-vector-subsetp a b` ####

如果a中的每个t值也是b中的t，则返回t，否则返回nil。所有参数都应该是相同长度的bool向量。

a 是否是 b 的子集

``` Elisp
(bool-vector-subsetp (bool-vector t nil t)
                     (bool-vector t t t))
;  t
```

#### 函数: `bool-vector-count-consecutive a b i` ####

返回a中从i开始等于b的连续元素的个数。a是bool向量，b是t或nil, i是a的索引。

``` Elisp
(bool-vector-count-consecutive (bool-vector t t nil t)
                               t
							   0)
;  2
(bool-vector-count-consecutive (bool-vector t t nil t)
                               nil
							   2)
;  1
(bool-vector-count-consecutive (bool-vector t t nil t)
                               nil
							   0)
;  0
```

#### 函数: `bool-vector-count-population a` ####

返回bool向量a中为t的元素个数。

``` Elisp
(bool-vector-count-population (bool-vector nil t t))
;  2
```

打印的形式表示多达8个布尔值作为一个字符:

``` Elisp
(bool-vector t nil t nil)
(bool-vector)
;  #&0""
```

你可以使用vconcat像打印其他向量一样打印一个bool向量:

``` Elisp
(vconcat (bool-vector nil t nil t))
;  [nil t nil t]
```

下面是另一个创建、检查和更新bool vector的例子:

``` Elisp
(setq bv (make-bool-vector 5 t))
(vconcat bv)
;  [t t t t t]
(aref bv 1)
;  t
(aset bv 3 nil)
;  nil
bv
(vconcat bv)
;  [t t t nil t]
;  #b10111

(bool-vector t t t nil t)
```

这些结果是有意义的，因为 `control-_` 和 `control-W`的二进制代码分别是11111和10111。

``` Elisp
(bool-vector nil nil t t)
;  #b1100
```

### 6.8 Managing a Fixed-Size Ring of Objects ###

环(ring) 是一种固定大小的数据结构，支持插入、删除、旋转以及模索引引用和遍历。

* insertion 插入
* deletion 删除
* rotation 旋转
* modulo-indexed reference 模索引引用
* traversal 遍历

利用 ring package 实现了一种高效的环数据结构。它提供了本节中列出的功能。

> 请注意，Emacs中的几个环，如kill ring和mark ring，实际上是作为简单列表实现的，而不是使用ring包;因此，下面的函数对它们不起作用。

#### 函数: `make-ring size` ####

这将返回一个能够容纳size对象的新环。Size应该是一个整数。

``` Elisp
(make-ring 5)
;  (0 0 . [nil nil nil nil nil])
(make-ring 1)
;  (0 0 . [nil])
(make-ring 0)
;  (0 0 . [])
```

#### 函数: `ring-p object` ####

如果object是一个环，则返回t，否则返回nil。

#### 函数: `ring-size ring` ####

这将返回环的最大容量。

``` Elisp
(ring-size (make-ring 5))
;  5
```

#### 函数: `ring-length ring` ####

返回ring当前包含的对象数。该值永远不会超过ring-size返回的值。

``` Elisp
(ring-length (make-ring 5))
;  0
```

#### 函数: `ring-elements ring` ####

这将返回一个对象列表，按顺序排列，最新的在前。

#### 函数: `ring-copy ring` ####

这将返回一个新的ring，它是ring的副本。新的ring包含与ring相同的(eq)对象。

#### 函数: `ring-empty-p ring` ####

如果ring为空，则返回t，否则返回nil。

#### 环的索引 ####

环中最新的元素总是索引为0。较高的索引对应于较老的元素。

索引按环长模计算。`−1`对应最老的元素，`−2`对应次老的元素，以此类推。

#### 函数: `ring-ref ring index` ####

这将返回在索引 index处找到的环中的对象。索引可以是负数，也可以大于环的长度。如果ring为空，ring-ref表示错误。

#### 函数: `ring-insert ring object` ####

这将对象插入到环中，使其成为最新的元素，并返回对象。

如果环已满，则插入将删除最老的元素，为新元素腾出空间。

``` Elisp
(setq ring-1 (make-ring 5))
;  (0 0 . [nil nil nil nil nil])
(ring-insert ring-1 'a)
;  a
ring-1
;  (0 1 . [a nil nil nil nil])
(ring-ref ring-1 0)
;  a
(ring-elements ring-1)
;  (a)
```

#### 函数: `ring-remove ring &optional index` ####

从环中移除一个对象，并返回该对象。参数index指定要删除的项;如果它是nil，那就意味着删除最老的项。如果ring为空，ring-remove表示错误。

``` Elisp
(setq ring-1 (make-ring 2))
;  (0 0 . [nil nil])

(ring-insert ring-1 'a)
;  a

ring-1
;  (0 1 . [a nil])

(ring-insert ring-1 'b)
;  b

ring-1
;  (0 2 . [a b])

(ring-insert ring-1 'c)
;  c

ring-1
;  (1 2 . [c b])

(ring-remove ring-1 -1)
;  b

ring-1
;  (1 1 . [nil c])

(ring-remove ring-1)
;  c
ring-1
;  (1 0 . [nil nil])

(ring-insert ring-1 'd)
;  d
ring-1
;  (1 1 . [nil d])
(ring-insert ring-1 'e)
;  e
ring-1
;  (1 2 . [e d])
(ring-insert ring-1 'f)
;  f
ring-1
;  (0 2 . [e f])
```

#### 函数: `ring-insert-at-beginning ring object` ####

这将对象插入到ring中，并将其视为最古老的元素。返回值不重要。

如果环已满，则该函数删除最新的元素，为插入的元素腾出空间。

``` Elisp
(setq ring-2 (make-ring 2))
(0 0 . [nil nil])

(ring-insert-at-beginning ring-2 'a)
;  1
ring-2
;  (1 1 . [nil a])

(ring-insert-at-beginning ring-2 'b)
;  2
ring-2
;  (0 2 . [b a])

(ring-insert-at-beginning ring-2 'c)
;  2
ring-2
(1 2 . [b c])

(ring-insert ring-2 'd)
;  d
ring-2
;  (0 2 . [b d])

(ring-insert-at-beginning ring-2 'e)
;  2
ring-2
;  (1 2 . [b e])
```

#### 函数: `ring-resize ring size` ####

设置 ring 的大小为size。如果新size较小，则丢弃环中最旧的项。

如果您注意不要超过环的大小，您可以将环用作先进先出队列。例如:

``` Elisp
(let ((fifo (make-ring 5)))
  (mapc (lambda (obj) (ring-insert fifo obj))
        '(0 one "two"))
  (list (ring-remove fifo) t
        (ring-remove fifo) t
		(ring-remove fifo)))
;  (0 t one t "two")
```

## 7 Records ##

records 的目的是允许程序员创建具有Emacs中没有内置的新类型的对象。它们被用作 `cl-defstruct`和 `defclass` 实例的底层表示。

在内部，一个记录对象很像一个向量;可以使用`aref`访问它的插槽，并且可以使用 `copy-sequence` 复制它。但是，第一个槽用于保存由 `type-of` 返回的类型。此外，在当前的实现中，记录最多可以有4096个槽，而向量可以大得多。与数组一样，记录使用零源索引:第一个槽的索引为0。

type slot 应该是一个符号或类型描述符 type descriptor。如果它是类型描述符，则返回命名其类型的符号;类型描述符。任何其他类型的对象都按原样返回。

record 的打印表示形式是 `#s` 后跟一个指定内容的列表。第一个列表元素必须是 record type。之后的元素是记录槽 record slots。

为了避免与其他类型名称冲突，定义新记录类型的Lisp程序通常应该使用引入这些记录类型的包的命名约定作为类型的名称。请注意，在加载定义记录类型的包时，可能不知道可能发生冲突的类型的名称;它们可能在未来的某个时间点被加载。

record 被认为是一个用于评估的常量:评估它的结果是相同的记录。这并不计算或甚至检查槽。参见自我评价表格 Self-Evaluating Forms。


* Record Functions
* Backward Compatibility

### 7.1 Record Functions ###

#### 函数: `recordp object` ####

如果对象是一条记录，这个函数返回t。

``` Elisp
(recordp #s(a))
;  t
```

#### 函数: `record type &rest objects` ####

这个函数创建并返回一个类型为type的记录，剩余的槽是参数的剩余部分，即对象。

``` Elisp
(record 'foo 23 [bar baz] "rats")
;  #s(foo 23 [bar baz] "rats")
```

#### 函数: `make-record type length object` ####

这个函数返回一个类型为type和 length个多余槽 的新记录，每个记录初始化为object。

``` Elisp
(setq sleepy (make-record 'foo 9 'Z))
;  #s(foo Z Z Z Z Z Z Z Z Z)
```

### 7.2 Backward Compatibility ###

使用旧版本的不使用记录的 `cl-defstruct` 编译的代码在新的Emacs中使用时可能会遇到问题。为了减轻这种情况，Emacs检测何时使用旧的cl-defstruct，并启用一种模式，在这种模式中，type-of会像处理记录一样处理旧的结构对象。

#### 函数: `cl-old-struct-compat-mode arg` ####

如果arg为正数，则启用与旧式结构的向后兼容。

## 8 Hash Tables ##

哈希表是一种非常快速的查找表，有点像alist(参见关联列表)，因为它将键映射到相应的值。它在以下几个方面与alist不同:

* 对于大型表，在哈希表中查找非常快——实际上，所需的时间基本上与表中存储了多少元素无关。对于较小的表(几十个元素)，列表可能仍然更快，因为散列表有或多或少的常量开销。
* 哈希表中的对应关系没有特定的顺序。
* 没有办法在两个哈希表之间共享结构，不像两个列表可以共享一个共同的尾。

Emacs Lisp提供了一种通用的哈希表数据类型，以及一系列对其进行操作的函数。哈希表有一种特殊的打印表示，它由 `#s` 和一个指定哈希表属性和内容的列表组成。参见创建哈希表Creating Hash Tables。(哈希表示法，在没有读表示的对象的打印表示中使用的初始' # '字符，与哈希表无关。参见打印表示和读取语法 Printed Representation and Read Syntax。)

*Obarrays* 也是一种哈希表，但它们是一种不同类型的对象，仅用于记录内部符号(参见创建和内部符号Creating and Interning Symbols)。


* Creating Hash Tables
* Hash Table Access
* Defining Hash Comparisons
* Other Hash Table Functions

### 8.1 Creating Hash Tables ###

创建哈希表的主要函数是 `make-hash-table`。

#### 函数: `make-hash-table &rest keyword-args` ####

这个函数根据指定的参数创建一个新的哈希表。参数应该由交替的关键字(特殊识别的特定符号)和与之对应的值组成。

在 `make-hash-table` 中有几个关键字是有意义的，但是您真正需要知道的只有两个 `:test` 和 `:weakness`。

##### 关键字 `:test` #####

这指定了这个哈希表的键查找方法。默认值是eql; eq 和 equal是其他选项:

* `eql`: 作为数字的键，如果它们相等，也就是说，如果它们的值相等并且都是整数或都是浮点数，则它们是相同的;否则，两个不同的对象永远不会相同。
* `eq`: 任何两个不同的Lisp对象作为键都是不同的。
* `equal`: 两个Lisp对象是相同的，就像键一样，如果它们根据equal相等。

您可以使用 `define-hash-table-test`(参见定义哈希比较 Defining Hash Comparisons)来定义 test 的其他可能性。

##### 关键字: `:weakness weak` #####

哈希表的 weakness 是指定哈希表中键或值的存在是否使其免受垃圾收集 garbage collection。

值(weak)必须是

* nil
* key
* value
* key-or-value
* key-and-value
* t (key-and-value的别名)

中的一个。

* 如果 weak 是 key，那么哈希表不会阻止它的键被收集为垃圾(如果它们没有在其他任何地方被引用);如果收集到特定键，则从哈希表中删除相应的关联。
* 如果 weak 是 value，那么哈希表不会阻止值被收集为垃圾(如果它们没有在其他任何地方被引用);如果收集到特定值，则从哈希表中删除相应的关联。
* 如果 weak 是 `key-and-value`或 `t`，则键和值必须都是活的，以便保持关联。因此，哈希表不保护键或值免受垃圾收集;如果其中任何一个被作为垃圾收集，则会删除关联。
* 如果 weak 是 `key-or-value`，则该键或值可以保留关联。因此，当关联的键和值都将作为垃圾收集时(如果不是来自弱哈希表的引用)，将从哈希表中删除关联。
* weak 的默认值为nil，因此哈希表中引用的所有键和值都不会被垃圾收集。

##### 关键字: `:size` #####

这为您计划在哈希表中存储多少关联指定了提示。如果你知道近似的数字，你可以通过这样指定它来使事情更有效率。如果指定的大小太小，哈希表将在必要时自动增长，但这样做需要一些额外的时间。

默认大小为65。

##### 关键字: `:rehash-size rehash-size` #####

当向哈希表添加关联时，如果哈希表已满，则会自动增长。这个值指定了如何使哈希表在那时变得更大。

如果rehash-size是一个整数，那么它应该是正的，并且哈希表通过向标称大小添加大约这么多来增长。如果rehash-size是浮点数，它最好大于1，并且哈希表通过将旧的大小乘以大约这个数字来增长。

默认值为1.5。

##### 关键字: `rehash-threshold threshold` #####

这指定了哈希表何时满的标准(因此应该使它变大)。阈值应该是一个不大于1的正浮点数。每当实际条目数超过标称大小乘以该值的近似值时，哈希表就满了。

threshold 的默认值是0.8125。

##### 使用打印形式创建哈希表 #####

您还可以使用哈希表的打印表示创建哈希表。只要指定哈希表中的每个元素都具有有效的读语法(参见打印表示和读语法 Printed Representation and Read Syntax)，Lisp reader 就可以读取这种打印表示。例如，下面指定一个散列表，其中包含键key1和key2(两个符号)，分别与val1(一个符号)和300(一个数字)相关联。

``` Elisp
#s(hash-table size 30 data (key1 val1 key2 300))
;  #s(hash-table size 30 test eql rehash-size 1.5 rehash-threshold 0.8125 data (key1 val1 key2 300))
```

但是，请注意，在Emacs Lisp代码中使用该方法时，它没有定义是否创建一个新的散列表。如果你想创建一个新的哈希表，你应该总是使用make-hash-table(参见自评估表单 Self-Evaluating Forms)。

哈希表的打印表示由“#s”和以“hash-table”开头的列表组成。列表的其余部分应该由零个或多个属性值对组成，这些属性值对指定哈希表的属性和初始内容。属性和值按字面读取。有效的属性名称是size、test、weakness、rehash-size、rehash-threshold和data。data属性应该是初始内容的键值对列表;其他属性与上面描述的匹配的make-hash表关键字(:size、:test等)具有相同的含义。

注意，不能指定初始内容包含 **没有读语法的对象**(如缓冲区和帧)的散列表。这样的对象可以在哈希表创建后添加到哈希表中。

### 8.2 Hash Table Access ###

介绍在哈希表中访问和存储关联的函数。一般来说，任何Lisp对象都可以用作散列键，除非比较方法施加了限制。任何Lisp对象也可以用作值。

#### 函数: `gethash key table &optional default` ####

此函数在表中查找键，并返回其关联值(如果键在表中没有关联，则返回默认值)。

``` Elisp
(setq ht-1 #s(hash-table size 5 data (key1 20 key2 50)))
(gethash 'key1 ht-1)
;  20
(gethash 'key3 ht-1 6)
;  6
```

#### 函数: `puthash key value table` ####

这个函数为表中的键输入一个关联，值为value。如果key已经在表中有关联，则value替换旧的关联值。这个函数总是返回值。

``` Elisp
(setq ht-1 #s(hash-table size 5 data (key1 20 key2 50)))
(puthash 'key4 4 ht-1)
;  4
ht-1#s
;  (hash-table size 5 test eql rehash-size 1.5 rehash-threshold 0.8125 data (key1 20 key2 50 key4 4))
```

#### 函数: `remhash key table` ####

这个函数从表中删除键的关联(如果有的话)。如果key没有关联，则remhash不做任何事情。

> Common Lisp注:在Common Lisp中，如果确实删除了关联，则remhash返回非nil，否则返回nil。在Emacs Lisp中，remhash 总是返回nil。

``` Elisp
(remhash 'key4 ht-1)
;  nil
ht-1
;  #s(hash-table size 5 test eql rehash-size 1.5 rehash-threshold 0.8125 data (key1 20 key2 50))
```

#### 函数: `clrhash table` ####

该函数从哈希表中删除所有关联，使其变为空。这也称为清除哈希表。clrash返回空表。

``` Elisp
(clrhash ht-1)
;  #s(hash-table size 5 test eql rehash-size 1.5 rehash-threshold 0.8125 data ())
```

#### 函数: `maphash function table` ####

此函数为表中的每个关联调用function一次。函数函数应该接受两个参数——表中列出的键和它的关联值。mapash返回nil。

### 8.3 Defining Hash Comparisons ###

可以通过 `define-hash-table-test` 定义键查找的新方法。为了使用这个特性，您需要了解哈希表是如何工作的，以及哈希码 hash code 的含义。

从概念上讲，您可以将哈希表看作一个由许多槽组成的大数组，每个槽可以保存一个关联。要查找键，`gethash` 首先从键计算一个整数，即哈希码。它可以减少这个整数模数组的长度，在数组中产生一个索引。然后，它会在那个槽中查找，如果必要的话，还会在附近的其他槽中查找，看看是否找到了要查找的键。

因此，要定义键查找的新方法，需要指定一个从键计算哈希码的函数，以及一个直接比较两个键的函数。这两个函数应该彼此一致:也就是说，如果键比较为相等，则两个键的哈希码应该相同。另外，由于这两个函数可以在任何时候调用(例如由垃圾收集器调用)，因此函数应该没有副作用，并且应该快速返回，并且它们的行为应该仅依赖于不改变的键的属性。

#### 函数: `define-hash-table-test name test-fn hash-fn` ####

这个函数定义了一个新的哈希表测试，名为name。

以这种方式定义name之后，您可以将其用作 `make-hash-table` 中的测试参数。当您这样做时，哈希表将使用 `test-fn` 比较键值，并使用 `hash-fn`从键值计算哈希码。

函数test-fn应该接受两个参数，两个键，如果认为它们是相同的，则返回非nil。

函数hash-fn应该接受一个参数，一个键，并返回一个整数作为该键的哈希码。为了获得良好的结果，该函数应该使用哈希码的整个固定数范围，包括负固定数。

指定的函数存储在属性哈希表test下的name属性列表中;属性值的形式是(test-fn hash-fn)。

#### 函数: `sxhash-equal obj` ####

这个函数返回Lisp对象obj的哈希码。这是一个反映obj和它所指向的其他Lisp对象的内容的整数。

如果两个对象obj1和obj2相等，则 `(sxhash-equal obj1)` 和 `(sxhash-equal obj2)` 是相同的整数。

如果两个对象不相等，则 `sxhash-equal` 返回的值通常不同，但并非总是如此。sxhash-equal被设计得相当快(因为它用于索引哈希表)，所以它不会递归到嵌套结构中。偶尔，如果幸运的话，您会遇到两个看起来不同的简单对象，它们从sxhash-equal中给出相同的结果。因此，通常不能使用sxhash-equal来检查对象是否发生了更改。

``` Elisp
(sxhash-equal 1)
;  1
(sxhash-equal 'o)
;  -34658338628848
```

> Common Lisp注意:在Common Lisp中有一个类似的函数叫做sxhash。Emacs提供这个名称作为sxhash-equal的兼容性别名。

#### 函数: `sxhash-eq obj` ####

这个函数返回Lisp对象obj的哈希码。它的结果反映了obj的身份，而不是它的内容。

如果两个对象obj1和obj2是eq，则 `(sxhash-eq obj1)` 和 `(sxhash-eq obj2)` 是相同的整数。

``` Elisp
(sxhash-eq 1)
;  7
(sxhash-eq 'o)
-34658338628848
```

#### 函数: `sxhash-eql obj` ####

这个函数返回Lisp对象obj的哈希码，适合于等价比较。也就是说，它反映了obj的身份，除非对象是一个大数值或浮点数，在这种情况下，会为该值生成一个哈希码。

如果两个对象obj1和obj2是eql，则 `(sxhash-eql obj1)` 和 `(sxhash-eql obj2)` 是相同的整数。

``` Elisp
(sxhash-eql 1)
;  7
(sxhash-eql 'o)
;  -34658338628848
```

这个例子创建了一个散列表，它的键是不区分大小写的字符串。

``` Elisp
(defun string-hash-ignore-case (a)
  (sxhash-equal (upcase a)))

(define-hash-table-test 'ignore-case
  'string-equal-ignore-case 'string-hash-ignore-case)

(make-hash-table :test 'ignore-case)
;  #s(hash-table size 65 test ignore-case rehash-size 1.5 rehash-threshold 0.8125 data ())
```

下面是如何定义一个与预定义的测试值相等的散列表测试。键可以是任何Lisp对象，外观相等的对象被认为是相同的键。

``` Elisp
(define-hash-table-test 'contents-hash 'equal 'sxhash-equal)

(make-hash-table :test 'contents-hash)
```

Lisp程序不应该依赖于在Emacs会话之间保存的哈希码，因为哈希函数的实现使用了对象存储的一些细节，这些细节可能在会话之间和不同体系结构之间发生变化。

### 8.4 Other Hash Table Functions ###

#### 函数: `hash-table-p table` ####

如果table是哈希表对象，则返回非nil。

#### 函数: `copy-hash-table table` ####

这个函数创建并返回一个表的副本。只有表本身被复制——键和值是共享的。

#### 函数: `hash-table-count table` ####

这个函数返回表中条目的实际数目。

``` Elisp
(setq ht-1 (make-hash-table))
(hash-table-count ht-1)
;  0
(puthash 'key 'val ht-1)
;  val
(hash-table-count ht-1)
;  1
```

#### 函数: `hash-table-test table` ####

这将返回创建表时给出的测试值，以指定如何散列和比较键。参见make-hash-table(参见创建哈希表)。

``` Elisp
(hash-table-test ht-1)
;  eql
```

#### 函数: `hash-table-weakness table` ####

这个函数返回为哈希表指定的弱值。

``` Elisp
(hash-table-weakness ht-1)
;  nil
```

#### 函数: `hash-table-rehash-size table` ####

这将返回表的重哈希大小。

``` Elisp
(hash-table-rehash-size ht-1)
;  1.5
```

#### 函数: `hash-table-rehash-threshold table` ####

这将返回表的重新散列阈值。

``` Elisp
(hash-table-rehash-threshold ht-1)
;  0.8125
```

#### 函数: `hash-table-size table` ####

这将返回表的当前标称大小nominal size。

``` Elisp
(hash-table-size ht-1)
;  65
```

## 9 Symbols ##

符号是具有唯一名称的对象。本章描述了符号，它们的组件，它们的属性列表，以及它们是如何被创建和存储的。单独的章节描述了符号作为变量和函数名的使用;参见变量和函数 Variables and Functions。有关符号的精确读取语法，请参见符号类型 Symbol Type。

你可以用`symbolp`来测试一个任意Lisp对象是否是一个符号:

#### 函数: `symbolp object` ####

如果object是一个符号，这个函数返回t，否则返回nil。

* Symbol Components
* Defining Symbols
* Creating and Interning Symbols
* Symbol Properties
* Shorthands
* Symbols with Position

### 9.1 Symbol Components ###

每个符号有四个组件(或“cells”)，每个组件引用另一个对象:

1. `Print name` 符号的名称
2. `Value` 符号当前的值
3. `Function` 符号的函数定义。它还可以保存符号 symbol、键映射 keymap 或 键盘宏 keyboard macro。
4. `Property list` 符号的属性列表

print name cell 始终保存一个字符串，并且不能更改。其他三个cell中的每一个都可以设置为任何Lisp对象。

打印名称单元格保存作为符号名称的字符串。由于符号在文本中是由它们的名称表示的，所以重要的是不要有两个具有相同名称的符号。Lisp读取器确保了这一点:每次读取一个符号时，它在创建一个新符号之前，先查找具有指定名称的现有符号。要获取符号的名称，请使用函数 `symbol-name` (参见创建和修改符号 Creating and Interning Symbols)。然而，尽管每个符号只有一个唯一的打印名称，但仍然可以通过称为“缩写shorthands”的不同别名alias来引用相同的符号(参见缩写 Shorthands)。

value cell 以变量的形式保存符号的值，如果将符号本身作为Lisp表达式求值，则得到该值。有关如何设置和检索值的详细信息，包括局部绑定和范围规则等复杂性，请参阅变量 Variables。

大多数符号可以将任何Lisp对象作为值，但某些特殊符号的值不能更改;这些包括nil和t，以及任何名称以':'开头的符号(这些被称为关键字)。参见永远不变的变量 Variables that Never Change。

函数单元格 function cell 保存着符号的函数定义。通常，当我们真正指的是存储在foo的函数单元中的函数时，我们指的是“函数foo”;我们只在必要时明确区分。通常，函数单元格用于保存函数(参见函数Functions)或宏(参见宏Macros)。然而，它也可以用来保存符号(参见符号函数间接 Symbol Function Indirection)、键盘宏(参见键盘宏 Keyboard Macros)、键图(参见键图 Keymaps)或自动加载对象(参见自动加载 Autoloading)。要获取符号函数单元的内容，请使用函数 `symbol-function` (参见访问函数单元内容 Accessing Function Cell Contents)。

Property list cell 属性列表单元格通常应该包含格式正确的属性列表。要获取符号的属性列表，请使用 `symbol-plist` 函数。参见符号属性 Symbol Properties。

函数单元格或值单元格可能是空的 void，这意味着单元格不引用任何对象。(这与持有符号void不同，也与持有符号nil不同。)检查为空的函数或值单元格会导致错误，例如“Symbol’s value as variable is void”。

因为每个符号都有独立的值单元格和函数单元格，所以变量名和函数名不会冲突。例如，符号 `buffer-file-name` 有一个值(当前缓冲区中被访问的文件的名称)和一个函数定义(返回文件名的基本函数):

``` Elisp
buffer-file-name"
;  d:/Coding/MyGitHub/SCAME/learn_elisp.md"
(symbol-function 'buffer-file-name)
;  #<subr buffer-file-name>
```

### 9.2 Defining Symbols ###

*definition* 定义是一种特殊类型的Lisp表达式，它宣布您打算以特定方式使用符号。它通常为符号指定一种用途的值或含义，以及以这种方式使用时其含义的文档。因此，当您将符号定义为变量时，您可以为该变量提供初始值，并为该变量提供文档。

`defvar` 和 `defconst` 是将符号定义为全局变量 *global variable* (可以在Lisp程序的任何位置访问的变量)的*特殊形式 special forms*。有关变量的详细信息，请参见“变量Variables”。要定义可自定义的变量，请使用`defcustom`宏，它也将`defvar`作为子例程subroutine调用(请参阅自定义设置 Customization Settings)。

原则上，您可以使用`setq`将变量值赋给任何符号，无论该符号是否首先被定义为变量。然而，你应该为你想要使用的每个全局变量写一个变量定义;否则，如果启用了词法作用域，Lisp程序可能无法正确运行(请参阅变量绑定的作用域规则 Scoping Rules for Variable Bindings)。

`defun` 将符号定义为函数function，创建lambda表达式并将其存储在符号的函数单元格中。这个lambda表达式因此成为符号的函数定义。(术语“函数定义 function definition”，意思是函数单元格的内容，源于defun将符号定义为函数的想法。)

`defsubst`和`defalias`是定义函数的另外两种方式。见函数Functions。

`defmacro`将符号定义为宏。它创建一个宏对象并将其存储在符号的函数单元格中。请注意，给定的符号可以是宏或函数，但不能同时是宏和函数，因为宏和函数的定义都保存在函数单元格中，而该单元格在任何给定时间只能保存一个Lisp对象。见宏Macros。

如前所述，Emacs Lisp允许将同一个符号定义为变量(例如，使用defvar)和函数或宏(例如，使用defun)。这些定义并不冲突。

这些定义还可以作为编程工具的指南。例如，`C-h-f` 和 `C-h-v` 命令创建帮助缓冲区，其中包含指向相关变量、函数或宏定义的链接。请参见《GNU Emacs手册》中的名称帮助 Name Help。

### 9.3 Creating and Interning Symbols ###

要理解符号是如何在GNU Emacs Lisp中创建的，您必须知道Lisp是如何读取它们的。Lisp必须确保每次在相同上下文中读取相同的字符序列时都能找到相同的符号。如果不这样做，就会引起完全的混乱。

当Lisp阅读器遇到引用源代码中的符号的名称时，它读取该名称的所有字符。然后，它在一个名为`obarray`的表中查找该名称，以找到程序员所指的符号。这种查找中使用的技术称为“哈希”，这是一种通过将字符序列转换为数字(称为“哈希码”)来查找内容的有效方法。例如，在查找简·琼斯的时候，你不是从头到尾搜索一本电话簿，而是从J开始，然后从那里开始。这是哈希的一个简单版本。`obarray`的每个元素都是一个`bucket`，其中包含具有给定哈希码的所有符号;要查找给定的名称，只需在`bucket`中查找该名称的哈希码的所有符号即可。(同样的想法也用于通用的Emacs哈希表，但它们是不同的数据类型;参见哈希表。)

在查找名字时，Lisp读者也会考虑“缩写Shorthands”。如果程序员提供了它们，这允许读者找到一个符号，即使它的名称在源代码中没有以完整的形式出现。当然，读者需要知道一些关于这些简写的预先建立的上下文，就像一个人需要上下文才能唯一地用“Jan”来指代简·琼斯一样:在琼斯一家中间，或者最近被提到Jan的时候，这可能是好的，但在任何其他情况下都是模棱两可的。见缩写 Shorthands。

如果找到了具有所需名称的符号，则读取器使用该符号。如果obarray中不包含具有该名称的符号，则阅读器生成一个新符号并将其添加到obarray中。找到或添加一个具有一定名称的符号，就叫它实习 *interning*，这个符号就叫实习符号 *interned symbol*。

Interning 确保每个数组只有一个具有特定名称的符号。其他类似命名的符号可能存在，但不在同一数组中。因此，只要您继续使用相同的数组进行阅读，读者就可以获得相同名称的相同符号。

实习通常在阅读器中自动发生，但有时其他程序可能想要这样做。例如，在M-x命令使用minibuffer获取命令名作为字符串后，它将对字符串进行实习，以获得具有该名称的实习符号。另一个例子是，一个假设的电话簿程序可以把每个查询到的人的名字作为一个符号，即使数组中不包含这个符号，这样它就可以把信息附加到这个新符号上，比如有人上次查询它的时间。

没有数组包含所有的符号;事实上，有些符号并不是任意排列的。它们被称为非内在符号 *uninterned sumbols*。非内部符号与其他符号具有相同的四个单元;但是，访问它的唯一方法是在其他对象或变量的值中找到它。非内部符号有时在生成Lisp代码时很有用，见下文。

在Emacs Lisp中，obarray实际上是一个向量。vector的每个元素都是一个bucket;它的值要么是一个内部符号，其名称散列到该桶bucket，要么是0，如果桶为空。每个内部符号都有一个指向桶中下一个符号的内部链接(用户不可见)。因为这些链接是不可见的，所以除了使用`mapatoms`(见下文)之外，没有办法找到obarray中的所有符号。桶中符号的顺序并不重要。

在一个空的obarray中，每个元素都是0，所以你可以用 `(make-vector length 0)` 创建一个obarray。这是创建obarray的唯一有效方法。质数作为长度往往会产生良好的散列;长度小于2的幂也是好的。

不要试图自己把符号放在一个数组中。这行不通——只有 intern 才能正确地在数组中输入符号。

> Common Lisp注意:与Common Lisp不同，Emacs Lisp不提供在几个不同的“包”中实习相同的名称，从而创建具有相同名称但不同包的多个符号。Emacs Lisp提供了一种不同的命名空间系统，称为“速记Shorthands”(参见速记Shorthands)。

下面的大多数函数都以名称(有时是obarray)作为参数。如果名称不是字符串，或者数组不是向量，则会提示类型参数错误。

#### 函数: `symbol-name symbol` ####

这个函数返回作为符号名称的字符串。例如:

``` Elisp
(symbol-name 'foo)
;  "foo"
```

> **警告**:永远不要改变该函数返回的字符串。这样做可能会使Emacs功能失调，甚至可能使Emacs崩溃。

创建非内部符号 uninterned sumbol 在生成Lisp代码时很有用，因为在生成的代码中用作变量的非内部符号不会与其他Lisp程序中使用的任何变量冲突。

#### 函数: `make-symbol name` ####

这个函数返回一个新分配的、未内部化的uninterned符号，其名称为name(必须是字符串)。它的值和函数定义为空，其属性列表为nil。在下面的例子中，`sym`的值不等于foo，因为它是一个独立的非内部符号，其名称也是 foo。

``` Elisp
(sxhash-eq 'foo)
;  -34658330326240
(setq sym (make-symbol "foo"))
;  foo
(eq sym 'foo)
;  nil
(sxhash-eq sym)
;  -34658847493860
```

#### 函数: `gensym &optional prefix` ####

该函数使用make-symbol返回一个符号，该符号的名称是通过将 `gensym-counter` 添加到prefix并对该计数器进行加1而获得的，从而保证对该函数的两次调用不会生成具有相同名称的符号。前缀默认为“g”。

为了避免意外使用生成代码的打印表示时出现问题(请参阅打印表示和读取语法Printed Representation and Read Syntax)，建议使用`gensym`而不是make-symbol。

#### 函数: `intern name &optional obarray` ####

这个函数返回名称为name的内部符号 interned symbol。如果obarray中没有这样的符号，`intern`将创建一个新的符号，将其添加到obarray中，并返回它。如果省略obarray，则使用全局变量obarray的值。

``` Elisp
(setq sym (intern "foo"))
;  foo
(eq sym 'foo)
;  t

(setq sym1 (intern "foo" other-obarray))
     ⇒ foo
(eq sym1 'foo)
     ⇒ nil
```

> Common Lisp注意事项:在Common Lisp中，您可以在数组中嵌入现有的符号。在Emacs Lisp中，您不能这样做，因为intern的参数必须是字符串，而不是符号。

#### 函数: `intern-soft name &optional obarray` ####

此函数返回obarray中名称为name的符号，如果obarray中没有该名称的符号，则返回nil。因此，您可以使用intern-soft来测试具有给定名称的符号是否已经被实习。如果省略obarray，则使用全局变量obarray的值。

参数name也可以是一个符号;在这种情况下，如果name被存储在指定的obarray中，则函数返回name，否则返回nil。

``` Elisp
(intern-soft "frazzle")  ; 不存在这个符号
;  nil
(make-symbol "frazzle")  ; 创建一个 uninterned 的
;  frazzle

(intern-soft "frazzle")  ; 找不到那个
l  nil

(setq sym (intern "frazzle"))  ; 创建一个 interned 的
;  frazzle
(intern-soft "frazzle")  ; 能找到了
;  frazzle
(eq sym 'frazzle)  ; 并且是同一个
;  t
```

#### 变量: `obarray` ####

这个变量是供intern和read使用的标准obarray。

#### 函数: `mapatoms function &optional obarray` ####

这个函数对数组obarray中的每个符号调用function一次。然后返回nil。如果省略obarray，则默认为普通符号的标准obarray值。

``` Elisp
(setq count 0)
;  0
(defun count-syms (s)
  (setq count (1+ count)))
;  count-syms
(mapatoms 'count-syms)
;  nil
count
;  41019
```

另一个使用mapatoms的例子，请参阅访问文档字符串中的文档 Access to Documentation Strings。

#### 函数: `unintern symbol obarray` ####

这个函数从数组obarray中删除符号。如果symbol实际上不在数组中，unintern什么也不做。如果obarray为nil，则使用当前obarray。

如果您提供字符串而不是符号作为symbol，则它代表符号名称。然后unintern删除数组中具有该名称的符号(如果有的话)。如果没有这样的符号，unintern什么也不做。

如果unintern删除了一个符号，它返回t，否则返回nil。

``` Elisp
(if (unintern "foo")
    (message "foo deleted!")
	(message "foo not fount!"))
;  "foo deleted!"
;  "foo not fount!"
```

### 9.4 Symbol Properties ###

一个符号可以拥有任意数量的符号属性，这些属性可用于记录有关该符号的各种信息。例如，当一个符号具有非nil值的 `risk-local-variable` 属性时，这意味着该符号命名的变量是一个有风险的文件局部变量(参见文件局部变量 File Local Variables)。

每个符号的属性和属性值以属性列表的形式存储在符号的属性列表单元格中(参见符号组件 Symbol Components)(参见属性列表 Property Lists)。

* Accessing Symbol Properties
* Standard Symbol Properties

#### 9.4.1 Accessing Symbol Properties ####

以下函数可用于访问符号属性。

#### 函数: `get symbol property` ####

这个函数返回符号属性列表中名为property的属性的值。如果没有这样的属性，则返回nil。因此，值为nil和不存在属性之间没有区别。

使用eq将name属性与现有的属性名称进行比较，因此任何对象都是合法的属性。

参见put的例子。

#### 函数: `put symbol property value` ####

这个函数将值放入属性名称property下的符号属性列表中，替换之前的任何属性值。put函数返回 value。

``` Elisp
(put 'fly 'verb 'transitive)
;  transitive
(put 'fly 'noum '(a buzzing little bug))
;  (a buzzing little bug)
(get 'fly 'verb)
;  transitive
(symbol-plist 'fly)
;  (verb transitive noum (a buzzing little bug))
```

#### 函数: `symbol-plist symbol` ####

返回 symbol 的 property list

#### 函数: `setplist symbol plist` ####

这个函数将符号的属性列表设置为plist。通常，plist应该是一个格式良好的属性列表，但这不是强制要求的。返回值是plist。

``` Elisp
(setplist 'foo '(a 1 b (2 3) c nil))
;  (a 1 b (2 3) c nil)
(symbol-plist 'foo)
;  (a 1 b (2 3) c nil)
```

对于不用于普通用途的特殊 obarrays 中的符号，以非标准方式使用属性列表单元格可能是有意义的;事实上，缩写机制(*abbrev mechanism*)就是这样做的(参见缩写和缩写扩展 Abbrevs and Abbrev Expansion)。

你可以用`setplist`和`plist-put`来定义 put，如下所示:

``` Elisp
(def put (symbol prop value)
  (setplist symbol
            (plist-put (symbol-plist symbol) prop value)))
```

#### 函数: `function-get symbol property &optional autoload` ####

此函数与get相同，除了如果symbol是函数别名alias的名称，它将在命名实际函数的符号的属性列表中查找。参见定义函数 Defining Functions。如果可选参数autolload非nil，并且symbol是自动加载的，则此函数将尝试自动加载它，因为自动加载可能会设置symbol的属性。如果autolload是符号宏 symbol macro，则只有当symbol是自动加载宏时才尝试自动加载。

#### 函数: `function-put function property value` ####

这个函数将函数的属性设置为value。function 应该是一个符号。这个函数比调用put函数来设置函数的属性更可取，因为它将允许我们在将来实现旧属性到新属性的重新映射。

#### 9.4.2 Standard Symbol Properties ####

这里，我们列出了Emacs中用于特殊目的的符号属性。在下表中，每当我们说“命名函数”时，意思是其名称是相关符号的函数;类似于“命名变量”等。

* `:advertised-binding` 此属性值在显示指定函数的文档时指定首选键绑定。请参阅文档中的替换键绑定 Substituting Key Bindings in Documentation。
* `char-table-extra-slots` 如果该值为非nil，则指定命名char-table类型中额外插槽的数量。见Char-Tables。
* `customized-face`、`face-defface-spec`、`saved-face`、`theme-face` 这些属性用于记录 face 的 standard, saved, customized, themed face specs。不要直接设置;它们由 `defface` 和相关函数管理。参见定义face Defining Faces。
* `customized-value`、`saved-valie`、`standard-value`、`theme-value` 这些属性用于记录可自定义变量的标准值、保存值、自定义但未保存的值和主题值。不要直接设置;它们由 `defcustom` 和相关函数管理。参见定义自定义变量 Defining Customization Variables。
* `definition-name` 当通过源文件的文本搜索可能难以找到定义时，此属性用于在源代码中查找符号的定义。例如 define-derived-mode (参见定义派生模式Defining Derived Modes) 可能隐式地定义 mode-specific 的函数或变量;或者您的Lisp程序可能会生成一个运行时调用来定义一个函数(参见定义函数 Defining Functions)。在这些和类似的情况下，符号的 `definition-name` 属性应该是另一个符号，其定义可以通过文本搜索找到，并且其代码定义了原始符号。在 `define-derived-mode` 的例子中，它定义的函数和变量的这个属性的值应该是mode符号。Emacs帮助命令，如 `C-h f` (参见GNU Emacs手册中的帮助)使用此属性通过 `*Help*` 缓冲区中的按钮显示符号的定义，该按钮显示该符号的文档。
* `disabled` 如果该值为非空值，则命名函数作为命令被禁用。请参见禁用命令 Disabling Commands。
* `face-documentation` 该值存储命名面的文档字符串。这是由默认自动设置的。参见 Defining Faces。
* `history-length` 如果该值为非空值，则指定命名历史列表变量的最大minibuffer历史长度。参见Minibuffer历史记录 Minibuffer History。
* `interactive-form` 该值是指定函数的交互形式。通常，你不应该直接设置它;使用 *interactive* special form 代替。参见交互式调用 Interactive Call。
* `menu-enable` 该值是一个表达式，用于确定是否应该在菜单中启用指定的菜单项。参见简单菜单项 Simple Menu Items。
* `mode-class` 如果该值是 *special*，则指定的主模式是特殊的。参见主要模式约定 Major Mode Conventions。
* `permanent-local` 如果该值非nil，则命名变量是缓冲区局部变量，在更改主模式时不应重置其值。请参见创建和删除缓冲区本地绑定 Creating and Deleting Buffer-Local Binding。
* `permant-local-hook` 如果该值非nil，则在更改主模式时不应从钩子变量的局部值中删除命名函数。参见设置挂钩 Setting Hooks。
* `pure` 如果该值非nil，则认为命名函数是纯函数(参见什么是函数? What Is a Function?) 带常量参数的调用可以在编译时求值。这可能会将运行时错误转移到编译时。不要与纯存储混淆(参见纯存储 Pure Storage)。
* `risky-local-variable` 如果该值非nil，则认为命名变量作为文件局部变量存在风险。参见文件局部变量 File Local Variables。
* `safe-function` 如果值非nil，则认为命名函数通常可以安全求值。请参阅确定函数是否可以安全调用 Determining whether a Function is Safe to Call。
* `safe-local-eval-function` 如果该值非nil，则命名函数可以安全地以文件本地求值形式调用。参见文件局部变量 File Local Variables。
* `safe-local-variable`  该值指定一个函数，用于确定指定变量的安全文件本地值。参见文件局部变量 File Local Variables。由于在加载文件时要参考这个值，因此该函数应该是高效的，并且理想情况下不应该导致加载任何库来确定安全性(例如，它不应该是一个自动加载的函数)。
* `side-effect-free` 非nil值表示命名函数没有副作用(请参阅什么是函数? What Is a Function?)，因此字节编译器可能会忽略值未使用的调用。如果属性的值没有错误，字节编译器甚至可以删除这些未使用的调用。除了字节编译器优化之外，此属性还用于确定函数安全性(请参阅确定函数是否可以安全调用 Determining whether a Function is Safe to Call)。
* `undo-inhibit-region` 如果非空，如果在函数之后立即调用undo，则命名函数防止将undo操作限制到活动区域。看到撤销 Undo。
* `variable-documentation` 如果非nil，则指定命名变量的文档字符串。这是由defvar和相关函数自动设置的。参见定义面孔 Defining Faces。

### 9.5 Shorthands ###

符号简写 symbol *shorthands*，有时被称为“重命名符号 renamed symbols”，是在Lisp源代码中发现的符号形式。它们就像常规的符号形式，除了当Lisp reader遇到它们时，它产生具有不同且通常更长打印名称的符号(参见符号组件 Symbol Components)。

把 shorthands 看作是所要表达的符号的全称abbreviating 的缩写是很有用的。尽管如此，不要将简写shorthands与缩写abbrev系统混淆(参见缩写和缩写扩展 Abbrevs and Abbrev Expansion)。

简写使得Emacs Lisp的命名空间规范 *namespacing etiquette* 更容易使用。由于所有符号都存储在单个数组中(参见创建和修改符号 Creating and Interning)，程序员通常在每个符号名称前加上其起源库的名称。例如，函数 `text-property-search-forward` 和 `text-property-search-backward` 都属于 `text-property-search.el` 库(参见加载 Loading)。通过适当地为符号名加上前缀，可以有效地防止属于不同库的命名相似的符号之间的冲突，从而实现不同的功能。然而，这种做法通常源于非常长的符号名称，不方便键入和阅读一段时间后。速记以一种简洁的方式解决了这些问题。

#### 变量: `read-symbol-shorthands` ####

该变量的值是一个 alist，其元素的形式为 `(shorthand-prefix . longhand-prefix)` 。每个元素都指示Lisp reader读取以缩写前缀开头的每个符号形式，就像以长前缀开头一样。

这个变量只能在文件局部变量中设置(参见GNU Emacs手册中的文件局部变量 Local Variables in Files)。

下面是一个在假设的字符串操作库 `some-nice-string-utils.el` 中使用速记的示例。

``` Elisp
(defun some-nice-string-utils-split (separator s &optional omit-nulls)
  "A match-data saving variant of `split-string'."
  (save-match-data (split-string s separator omit-nulls)))

(defun some-nice-string-utils-lines (s)
  "Split string S at newline characters into a list of strings."
  (some-nice-string-utils-split "\\(\r\n\\|[\n\r]\\)" s))
```

可以看到，阅读或开发这段代码非常乏味，因为要输入的符号名太长了。我们可以用速记来缓解这种情况。

``` Elisp
(defun snu-split (separator s &optional omit-nulls)
  "A match-data saving variation on `split-string'."
  (save-match-data (split-string s separator omit-nulls)))

(defun snu-lines (s)
  "Split string S into a list of strings on newline characters."
  (snu-split "\\(\r\n\\|[\n\r]\\)" s))

;; Local Variables:
;; read-symbol-shorthands: (("snu-" . "some-nice-string-utils-"))
;; End:
```

尽管这两个摘录看起来不同，但在Lisp读者处理它们之后，它们是完全相同的。两者都会导致相同的符号被保留(参见创建和保留符号Creating and Interning Symbols)。因此，加载或字节编译这两个文件中的任何一个都会得到相同的结果。第二个版本中使用的简写 `snu-split` 和 `snu-lines` 不在数组中保留。通过将point移动到使用简写的位置，并等待ElDoc(请参阅GNU Emacs手册中的文件中的局部变量 Local Variables in Files)在回声区域中提示point下的符号的真实全名，可以很容易地看到这一点。

由于 `read-symbol-shorhands` 是一个文件局部变量，因此可能有多个库依赖于 `some-nice-string-utils-lines.el`。我们用不同的简写表示相同的符号，或者根本不使用简写。在下一个例子中，`my-tricks.el` 库引用符号 `some-nice-string-utils-lines` 时使用 `sns-` 前缀而不是 `snu-`。

``` Elisp
(defun t-reverse-lines (s) (string-join (sns-lines s)) "\n")

;; Local Variables:
;; read-symbol-shorthands: (("t-" . "my-tricks-")
                            ("sns-" . "some-nice-string-utils"))
;; End:
```

#### 9.5.1 Exceptions ####

管理速记转换的规则有两个例外:

1. 在Emacs Lisp符号组成类(参见语法类表 Table of Syntax Classes)中，完全由字符组成的符号形式不会被转换。例如，可以使用 `-` 或 `/=` 作为速记前缀，但这不会掩盖这些名称的算术函数。
2. 名称以 `#_` 开头的符号形式不会被转换。

### 9.6 Symbols with Position ###

带位置的符号 *symbol with position* 是一个符号，裸符号(bare symbol)，加上一个称为位置 position 的无符号整数。这些对象供字节编译器使用，字节编译器在其中记录每个符号出现的位置，并在警告和错误消息中使用这些位置。

带有位置的符号的打印表示使用在打印表示和读取语法中概述的哈希表示法。它看起来像 `#<symbol foo at 12345>`。它没有读语法。通过在打印操作周围将变量 `print-symbols-bare` 绑定为非nil，可以只打印裸符号。字节编译器在将其输出写入已编译的Lisp文件之前执行此操作。

对于大多数用途，当标志变量 `symbols-with-pos-enabled` 为非nil时，具有位置的符号的行为就像裸符号一样。例如，`(eq #<symbol foo at 12345> foo)` 在设置该变量时具有值t(但在未设置时为nil)。在Emacs中，大多数时候这个变量是nil，但是字节编译器在运行时将它绑定到t。

通常，带有位置的符号是由字节编译器调用读取器函数 `read-positioning-symbols` 创建的(参见输入函数 Input Functions)。也可以使用 `position-symbol` 函数来创建。

#### 变量: `symbols-with-pos-enabled` ####

当此变量为非nil时，具有位置的符号的行为就像包含的裸符号一样。在这种情况下，Emacs运行得稍微慢一些。

#### 变量: `print-symbol-bare` ####

当绑定为非空值时，Lisp打印机只打印带有位置的符号的裸符号，而忽略位置。

#### 函数: `symbol-with-pos=p symbol` ####

如果symbol是有位置的符号，则返回t，否则返回nil。

#### 函数: `bare-symbol symbol` ####

这个函数返回包含在symbol中的裸符号，如果已经是裸符号，则返回symbol本身。对于任何其他类型的对象，它表示错误。

#### 函数: `symbol-with-pos-pos symbol` ####

这个函数从带有位置的符号返回位置，一个数字。对于任何其他类型的对象，它表示错误。

#### 函数: `position-symbol sym pos` ####

用创建一个有位置的新符号。Sym可以是一个裸符号，也可以是一个带位置的符号，并提供新对象的符号部分。pos是一个整数，它成为新对象的数字部分，或者是一个带有位置的符号，它的位置被使用。如果其中一个参数无效，Emacs将发出错误信号。

## 10 Evaluation ##

[Evaluation](https://www.gnu.org/software/emacs/manual/html_node/elisp/Evaluation.html)

在Emacs Lisp中，表达式的求值是由Lisp解释器执行的——Lisp解释器是一个接收Lisp对象作为输入并将其值作为表达式计算出来的程序。这取决于对象的数据类型，根据本章描述的规则。解释器自动运行以计算程序的部分，但也可以通过Lisp原始函数eval显式调用。


* Introduction to Evaluation
* Kinds of Forms
* Quoting
* Backquote
* Eval
* Deferred and Lazy Evaluation

### 10.1 Introduction to Evaluation ###

Lisp解释器或求值器是Emacs的一部分，用于计算给定表达式的值。当调用用Lisp编写的函数时，求值器通过计算函数体中的表达式来计算函数的值。因此，运行任何Lisp程序实际上意味着运行Lisp解释器。

用于求值的Lisp对象称为形式 *form* 或表达式 *expression*。forms 是数据对象而不仅仅是文本这一事实是类lisp语言和典型编程语言之间的根本区别之一。任何对象都可以求值，但实际上只有数字、符号、列表和字符串经常被求值。

在随后的部分中，我们将详细描述每种形式的求值含义。

读取一个Lisp表单然后求值是很常见的，但是读取和求值是独立的活动，两者都可以单独执行。读取本身并不能评价任何东西;它将Lisp对象的打印表示形式转换为对象本身。由read的调用者来指定该对象是要求值的形式，还是用于完全不同的目的。参见输入函数 Input Functions。

求值是一个递归过程，对form求值通常涉及对该表单中的各个部分求值。例如，当你计算一个函数调用形式，如(car x)， Emacs首先计算参数(子形式x)。计算参数后，Emacs执行函数(car)，如果函数是用Lisp编写的，执行工作通过计算函数体(然而，在这个例子中，car不是一个Lisp函数;它是在C语言中实现的原始函数。有关函数和函数调用的更多信息，请参阅函数Functions。

求值在称为环境的上下文中进行，该上下文中包含所有Lisp变量的当前值和绑定(参见变量)每当form引用变量而不为其创建新绑定时，该变量的计算结果为当前环境给出的值。评估form也可能通过绑定变量临时改变环境(参见局部变量)。

对form进行评估也可能产生持久的更改;这些变化被称为副作用 *side effects*。一个产生副作用的form示例是 `(setq foo 1)`。

不要将 求值evaluation 与 命令键解释command key interpretation 混淆。编辑器命令循环使用活动键映射将键盘输入转换为命令(可交互调用的函数)，然后使用调用交互方式执行该命令。如果命令是用Lisp编写的，执行命令通常涉及求值;但是，这个步骤不被认为是命令键解释的一部分。参见命令循环 Command Loop。

### 10.2 Kinds of Forms ###

用于求值的Lisp对象称为形式 form (或表达式 expression)。Emacs计算form的方式取决于form的数据类型。Emacs有三种不同的形式，它们的计算方式不同:

1. 符号
2. 列表
3, 所有其他类型

本节将逐一介绍这三种类型，从其他类型开始，这些类型是自评估表单 Self-evaluating forms。

* Self-Evaluating Forms
* Symbol Forms
* Classification of List Forms
* Symbol Function Indirection
* Evaluation of Function Forms
* Lisp Macro Evaluation
* Special Forms
* Autoloading

#### 10.2.1 Self-Evaluating Forms ####

一个自我求值的形式是除了列表或符号以外的任何形式。自我求值形式是对自己的求值:求值的结果就是被求值的对象。因此，数字25的计算结果为25，字符串“foo”的计算结果为字符串“foo”。同样，对vector求值不会导致对vector的元素求值——它返回内容不变的同一个vector。

``` Elisp
'123               ; A number, shown without evaluation.
     ⇒ 123
123                ; Evaluated as usual—result is the same.
     ⇒ 123
(eval '123)        ; Evaluated "by hand"—result is the same.
     ⇒ 123
(eval (eval '123)) ; Evaluating twice changes nothing.
     ⇒ 123
```

自求值形式产生的值成为程序的一部分，您不应该尝试通过`setcar`、`aset`或类似操作来修改它。Lisp解释器可以统一程序的自求值形式产生的常量，以便这些常量可以共享结构。见可变性 Mutability。

在Lisp代码中编写数字、字符、字符串甚至向量是很常见的，这利用了它们的自求值特性。然而，对于缺乏读语法的类型，这样做是非常不寻常的，因为没有办法以文本方式编写它们。通过Lisp程序可以构造包含这些类型的Lisp表达式。下面是一个例子:

``` Elisp
;; Build an expression containing a buffer object.
(setq print-exp (list 'print (current-buffer)))
     ⇒ (print #<buffer eval.texi>)

;; Evaluate it.
(eval print-exp)
     -| #<buffer eval.texi>
     ⇒ #<buffer eval.texi>
```

#### 10.2.2 Symbol Forms ####

当一个符号被求值时，它被视为一个变量。结果是变量的值(如果它有)。如果符号没有作为变量的值，Lisp解释器发出错误信号。有关变量使用的更多信息，请参见变量。

在下面的示例中，我们使用setq设置符号的值。然后对符号求值，并返回setq存储的值。

``` Elisp
(setq a 123)
     ⇒ 123

(eval 'a)
     ⇒ 123

a
     ⇒ 123
```

符号nil和t被特殊处理，因此nil的值总是nil，而t的值总是t;您不能将它们设置或绑定到任何其他值。因此，这两个符号就像自求值形式一样，尽管eval把它们当作任何其他符号来对待。名称以':'开头的符号也以同样的方式进行自求值;同样，它的值通常不能更改。参见永远不变的变量 Variables that Never Change。

#### 10.2.3 Classification of List Forms ####

非空列表的形式根据其第一个元素是函数调用、宏调用或特殊形式。这三种形式以不同的方式进行求值，如下所述。其余列表元素构成函数、宏或特殊形式的参数。

计算非空列表的第一步是检查它的第一个元素。这个元素单独决定了列表的形式，以及如何处理列表的其余部分。不计算第一个元素，就像在Scheme等一些Lisp方言中那样。

#### 10.2.4 Symbol Function Indirection ####

[Symbol Function Indirection](https://www.gnu.org/software/emacs/manual/html_node/elisp/Function-Indirection.html)

如果列表的第一个元素是一个符号，则求值检查该符号的函数单元格，并使用其内容而不是原始符号。如果内容是另一个符号，则重复这个过程，称为符号间接函数，直到它获得一个非符号。有关符号间接函数的更多信息，请参见命名函数 Naming a Function。

这个过程的一个可能结果是无限循环，如果一个符号的函数单元指向同一个符号。否则，我们最终得到一个非符号，它应该是一个函数或其他合适的对象。

更准确地说，我们现在应该有一个Lisp函数(lambda表达式)、一个字节码函数、一个原语函数、一个Lisp宏、一个特殊形式或一个自动加载对象。这些类型中的每一种都是以下章节中描述的一种情况。如果对象不是这些类型之一，Emacs将发出无效函数错误信号。

下面的示例说明了符号间接过程。我们使用 `fset` 来设置符号的函数单元，`symbol-function` 来获取函数单元的内容(参见访问函数单元内容 Accessing Function Cell Contents)。具体来说，我们将符号 `car` 存储到 `first` 的函数单元中，将符号`first`存储到`erste`的函数单元中。

``` PlainText
;; Build this function cell linkage:
;;   -------------       -----        -------        -------
;;  | #<subr car> | <-- | car |  <-- | first |  <-- | erste |
;;   -------------       -----        -------        -------

(symbol-function 'car)
     ⇒ #<subr car>

(fset 'first 'car)
     ⇒ car

(fset 'erste 'first)
     ⇒ first

(erste '(1 2 3))   ; Call the function referenced by erste.
     ⇒ 1

```

相比之下，下面的例子调用了一个没有间接符号函数的函数，因为第一个元素是一个匿名Lisp函数，而不是符号。

``` Elisp
((lambda (arg) (erste arg))
 '(1 2 3))
     ⇒ 1
```

执行函数本身对函数体求值;这确实涉及到调用erste时间接的符号函数。

这种形式很少使用，现在已弃用。相反，你应该这样写:

``` Elisp
(funcall (lambda (arg) (erste arg))
         '(1 2 3))
```

或者

``` Elisp
(let ((arg '(1 2 3))) (erste arg))
```

内置函数 `indirect-function` 提供了一种简单的方法来显式地执行符号函数间接。

##### 函数: `indirect-function function &optional noerror` #####

这个函数以函数的形式返回函数的含义。如果function是一个符号，那么它找到function的函数定义，并从该值开始。如果function不是一个符号，那么它返回function本身。

如果最后一个符号未绑定，则此函数返回nil。如果符号链中存在循环，则表示循环函数间接错误。

可选参数noerror已经过时了，保留它是为了向后兼容，没有任何作用。

下面是如何在Lisp中定义间接函数:

``` Elisp
(defun indirect-function (function)
  (if (and function
           (symbolp function))
      (indirect-function (symbol-function function))
    function))
```

#### 10.2.5 Evaluation of Function Forms ####

如果要计算的列表的第一个元素是Lisp函数对象、字节码对象或基本函数对象，则该列表是一个函数调用。例如，下面是对函数 `+` 的调用:

``` Elisp
(1+ x)
```

计算函数调用的第一步是从左到右计算列表中剩余的元素。结果是实际的参数值，每个列表元素对应一个值。下一步是使用这个参数列表调用函数，有效地使用apply函数(参见调用函数)。如果函数是用Lisp编写的，则参数用于绑定函数的参数变量(参见Lambda表达式);然后按顺序计算函数体中的形式，最后一个形式的值成为函数调用的值。

#### 10.2.6 Lisp Macro Evaluation ####

如果正在求值的列表的第一个元素是一个宏对象，那么该列表就是一个宏调用。当计算宏调用时，列表其余部分的元素不会初始计算。相反，这些元素本身被用作宏的参数。宏定义计算一种替代形式，称为宏的展开，以代替原始形式进行计算。展开可以是任何形式:自求值常数、符号或列表。如果展开本身是一个宏调用，那么这个展开过程会重复，直到出现其他形式。

宏调用的普通求值通过求展开结束。然而，宏展开不一定会立即求值，或者根本不会求值，因为其他程序也会展开宏调用，它们可能会也可能不会求值展开。

通常，参数表达式不作为计算宏展开的一部分进行计算，而是作为展开的一部分出现，因此在计算展开时计算它们。

例如，给定如下定义的宏:

``` Elisp
(defmacro cadr (x)
  (list 'car (list 'cdr x)))
```

像 `(cadr (assq 'handler list))` 这样的表达式是一个宏调用，其展开为:

``` Elisp
(car (cdr (assq 'handler list)))
```

注意，参数 `(assq 'handler list)` 出现在展开中。

有关Emacs Lisp宏的完整描述，请参阅宏。

#### 10.2.7 Special Forms ####

一种特殊形式是特别标记的原语，这样它的参数就不会全部求值。大多数特殊的form定义控制结构或执行变量绑定——这些都是函数无法做到的。

每种特殊形式都有自己的规则，对哪些参数求值，哪些不求值。是否评估一个特定的论证可能取决于评估其他论证的结果。

如果表达式的第一个符号是特殊形式的符号，则表达式应遵循该特殊形式的规则;否则，Emacs的行为就没有很好的定义(尽管它不会崩溃)。例如， `((lambda (x) x . 3) 4)` 包含以lambda开头的子表达式，但不是格式良好的lambda表达式，因此Emacs可能会发出错误信号，或者可能返回3、4或nil，或者可能以其他方式表现。

##### 函数: `special-form-p object` #####

该谓词测试其参数是否为特殊形式，如果是则返回t，否则返回nil。

下面是Emacs Lisp中所有特殊形式的列表，按字母顺序排列，并提供了描述每种形式的参考。

* `and` 见 Constructs for Combining Conditions
* `catch` 见 Explicit Nonlocal Exits: catch and throw
* `cond` 见 Conditionals
* `condition-case` 见 Writing Code to Handle Errors
* `defconst` 见 Defining Global Variables
* `defvar` 见 Defining Global Variables
* `function` 见 Anonymous Functions
* `if` 见 Conditions
* `interactive` 见 Interactive Call
* `lambda` 见 Lambda Expressions
* `let`, `let*` 见 Local Variables
* `or` 见 Constructs for Combining Conditions
* `prog1`, `prog2`, `progn` 见 Sequencing
* `quote` 见 Quoting
* `save-current-buffer` 见 The Current Buffer
* `save-excursion` 见 Excursions
* `save-restriction` 见 Narrowing
* `setq` 见 Setting Variable Values
* `setq-default` 见 Creating and Deleting Buffer-local Bindings
* `unwind-protect` 见 Nonlocal Exits
* `while` 见 Oteratopm

> Common Lisp注意事项:下面是GNU Emacs Lisp和公共Lisp中特殊形式的一些比较。setq、if和catch是Emacs Lisp和Common Lisp中的特殊形式。保存偏移是Emacs Lisp中的一种特殊形式，但在Common Lisp中不存在。throw在Common Lisp中是一种特殊的形式(因为它必须能够抛出多个值)，但它在Emacs Lisp中是一个函数(它没有多个值)。

#### 10.2.8 Autoloading ####

自动加载特性允许您调用尚未加载到Emacs中的函数定义的函数或宏。它指定哪个文件包含定义。当自动加载对象作为符号的函数定义出现时，将该符号作为函数调用将自动加载指定的文件;然后调用从该文件加载的实际定义。安排自动加载对象显示为符号的函数定义的方法见自动加载。 Autoload

### 10.3 Quoting ###

特殊形式quote返回它的单个参数，不计算它。这提供了一种在程序中包含常量符号和列表的方法，它们不是自求值的对象。(没有必要引用自求值对象，如数字、字符串和向量。)

#### 特殊形式: `quote object` ####

这种特殊的形式返回对象，而不求值。返回值可能是共享的，不应该被修改。参见自我评价表格。 

因为quote在程序中经常使用，Lisp为它提供了一种方便的读语法。撇号字符 `'` 后跟Lisp对象(在read语法中)展开为一个列表，其第一个元素是quote，第二个元素是object。因此，read语法 `'x` 是 `(quote x)` 的缩写。

下面是一些使用quote的例子:

``` Elisp
(quote (+ 1 2))
     ⇒ (+ 1 2)

(quote foo)
     ⇒ foo

'foo
     ⇒ foo

''foo
     ⇒ 'foo

'(quote foo)
     ⇒ 'foo

['foo]
     ⇒ ['foo]
```

虽然表达式 `(list '+ 1 2)` 和 `'(+ 1 2)` 产生的列表都等于 `(+ 1 2)`，但前者产生的是一个新创建的可变列表，而后者产生的是一个由conses构建的列表，该列表可能是共享的，不应该被修改。参见自我评价表格。

其他引用结构包括function(请参阅匿名函数)，它导致用Lisp编写的匿名lambda表达式被编译，以及反引号 (请参阅反引用Backquote)，它用于在计算和替换其他部分时仅引用列表的一部分。

### 10.4 Backquote ###

反引号结构允许您引用列表，但有选择地计算该列表中的元素。在最简单的情况下，它与特殊形式quote(在前一节中描述;见引用)。例如，这两种形式产生相同的结果:

``` Elisp
`(a list of (+ 2 3) elements)
     ⇒ (a list of (+ 2 3) elements)

'(a list of (+ 2 3) elements)
     ⇒ (a list of (+ 2 3) elements)
```

参数内的特殊标记 `,` 表示该值不是常量。Emacs Lisp求值器对 `,` 的参数求值，并将值放入列表结构中:

``` Elisp
`(a list of ,(+ 2 3) elements)
;  (a list of 5 elements)
```

在列表结构的更深层也允许使用 `,` 进行替换。例如:

``` Elisp
`(1 2 (3 ,(+ 4 5)))
;  (1 2 (3 9))
```

您还可以使用特殊标记 `,@` 将计算值拼接到结果列表中。拼接列表的元素成为与结果列表的其他元素处于同一级别的元素。不使用 反引号 的等效代码通常是不可读的。下面是一些例子:

``` Elisp
(setq some-list '(2 3))
;  (2 3)

(cons 1 (append some-list '(4) some-list))
;  (1 2 3 4 2 3)

`(1 ,@some-list 4 ,@some-list)
;  (1 2 3 4 2 3)

(setq list '(hack foo bar))
;  (hack foo bar)

(cons 'use
  (cons 'the
    (cons 'words (append (cdr list) '(as elements)))))
     ⇒ (use the words foo bar as elements)

`(use the words ,@(cdr list) as elements)
     ⇒ (use the words foo bar as elements)
```

如果反引号结构的子表达式没有替换或拼接，则它的作用类似于引号，因为它产生可以共享且不应修改的conse、vector和字符串。参见自我评价表格。

### 10.5 Eval ###

大多数情况下，由于forms出现在正在运行的程序中，因此会自动评估表单。在极少数情况下，您可能需要编写代码来计算在运行时计算的表单，例如在从正在编辑的文本中读取表单或从属性列表中获取表单之后。在这些情况下，使用eval函数。通常不需要eval，而应该使用其他方法。例如，要获取变量的值，在eval工作时，symbol-value更可取;或者与其将表达式存储在属性列表中，然后需要通过eval，不如存储函数，然后将其传递给funcall。

本节中描述的函数和变量对forms进行求值，指定求值过程的限制，或记录最近返回的值。加载文件也会执行计算(参见加载 Loading)。

将函数存储在数据结构中，并使用funcall或apply调用它，通常比将表达式存储在数据结构中并对其求值更简洁、更灵活。使用函数提供了将信息作为参数传递给它们的能力。

#### 函数: `eval form &optional lexical` ####

这是计算表达式的基本函数。它在当前环境中计算表单，并返回结果。表单对象的类型决定了它的计算方式。参见表单的种类。

参数lexical指定局部变量的作用域规则(参见变量绑定的作用域规则 Scoping Rules for Variable Bindings)。如果省略或为nil，则意味着使用默认的动态范围规则计算表单。如果是t，那就意味着使用词法 lexical scoping rule 作用域规则。lexical的值也可以是一个非空列表，指定词法绑定的特定词法环境;但是，该特性仅对特定目的有用，例如在Emacs Lisp调试器中。参见词法绑定。Lexical Binding

由于eval是一个函数，因此在调用eval时出现的参数表达式会被求值两次:一次作为调用eval之前的准备，另一次由eval函数本身求值。下面是一个例子:

``` Elisp
(setq foo 'bar)
     ⇒ bar

(setq bar 'baz)
     ⇒ baz
;; Here eval receives argument foo
(eval 'foo)
     ⇒ bar
;; Here eval receives argument bar, which is the value of foo
(eval foo)
     ⇒ baz
```

当前对eval的活动调用的数量限制为 `max-lisp-eval-depth` (见下文)。

#### 命令: `eval-region start end &optional stream read-function` ####

这个函数计算当前缓冲区中由位置start和end定义的区域中的表单。它从区域读取表单并对其调用eval，直到到达区域的末尾，或者直到发出错误信号而未处理为止。

默认情况下，`eval-region`不产生任何输出。但是，如果stream是非空值，则输出函数产生的任何输出(请参阅输出函数Output Functions)，以及对该区域中的表达式求值所产生的值都将使用stream打印。参见输出流Output Streams。

如果`read-function`是非nil，它应该是一个函数，它被用来代替read逐个读取表达式。调用该函数时带一个参数，即读取输入的流。您也可以使用变量 `load-read-function` (请参阅程序如何加载 How Programs Do Loading)来指定此函数，但使用 `read-function` 参数更为健壮。

`eval-region` 不移动 point, 它总是返回 nil

#### 命令: `eval-buffer &optional buffer-or-name stream filename unibute print` ####

这类似于 `eval-region`，但是参数提供了不同的可选特性。`eval-buffer`对缓冲区 `buffer-or-name` 的整个可访问部分进行操作(参见《GNU Emacs手册》中的“缩小Narrowing”)。`buffer-or-name`可以是缓冲区、缓冲区名称(字符串)或nil(或省略)，这意味着使用当前缓冲区。stream和eval-region一样使用，除非stream为nil并且输出非nil。在这种情况下，计算表达式得到的值仍然会被丢弃，但是输出函数的输出会被打印在回显区。filename是用于加载历史(请参阅卸载)的文件名，默认为 `buffer-file-name`(请参阅缓冲区文件名 Buffer File Name)。如果unibyte非nil，则read将字符串尽可能转换为单字节。

#### 用户选项: `max-lisp-eval-depth` ####

该变量定义了在发出错误信号之前调用eval、apply和funcall所允许的最大深度(错误消息为“Lisp嵌套超过max-lisp-eval-depth”)。

这个限制，以及超过该限制时的相关错误，是Emacs Lisp避免对定义不清的函数进行无限递归的方式。如果将max-lisp-eval-depth的值增加得太多，这样的代码反而会导致堆栈溢出。在某些系统上，可以处理这种溢出。在这种情况下，正常的Lisp求值被中断，控制被转移回顶级命令循环(top-level)。注意，在这种情况下没有办法进入Emacs Lisp调试器。请参阅在出现错误时进入调试器Entering the Debugger on an Error。

深度限制计算eval、apply和funcall的内部使用，例如调用Lisp表达式中提到的函数，递归计算函数调用参数和函数体形式，以及Lisp代码中的显式调用。

这个变量的默认值是1600。如果将其设置为小于100的值，Lisp将在达到给定值时将其重置为100。进入Lisp调试器会增加该值(如果剩余空间很少)，以确保调试器本身有执行的空间。

#### 变量: `values` ####

这个变量的值是由标准Emacs命令从缓冲区(包括minibuffer)读取、求值和打印的所有表达式返回的值的列表。(注意，这并不包括在 `*ielm*` 缓冲区中的求值，也不包括在 `lisp-interaction-mode` 中使用C-j, C-x C-e和类似的求值命令的求值。)

这个变量已经过时了，将在将来的版本中删除，因为它会不断地增加Emacs进程的内存占用。因此，我们建议不要使用它。

values 的元素按最近的顺序排列。

``` Elisp
(setq x 1)
;  1
(list 'A (1+ 2) auto-save-default)
;  (A 3 t)

values
;  nil
```

这个变量对于引用最近计算过的表单的值很有用。打印values本身的值通常不是一个好主意，因为它可能很长。相反，检查特定的元素，像这样:

``` Elisp
;; Refer to the most recent evaluation result.
(nth 0 values)
     ⇒ (A 3 t)

;; That put a new element on,
;;   so all elements move back one.
(nth 1 values)
     ⇒ (A 3 t)

;; This gets the element that was next-to-most-recent
;;   before this example.
(nth 3 values)
     ⇒ 1
```

### 10.6 Deferred and Lazy Evaluation ###

有时，延迟delay 表达式的求值是有用的，例如，如果您想避免执行耗时的计算，如果结果在程序的未来中不需要。`thunk` 库提供了以下函数和宏来支持这种延迟求值:

#### 宏: `thunk-delay forms...` ####

返回一个值，用于对 forms 求值。thunk是一个闭包(参见闭包 Closures)，它继承了 `thunk-delay` 调用的词法环境。使用这个宏需要 `lexical-binding`。

#### 函数: `thunk-force thunk` ####

强制 思维thunk 执行在创造思维的 `thunk-delay` 中指定的形式的评估。返回最后一个表单的求值结果。thunk 也“记得”它已经被强迫了:任何用同样的 thunk 再次调用 `thunk-force` 只会返回同样的结果，而不会再次评估形式。

#### 宏: `thunk-let (bindings...) forms...` ####

这个宏类似于 let，但创建了“惰性lazy”变量绑定。任何绑定都有这样的形式 `(symbol value-form)`。与let不同的是，任何值形式的求值都被延迟，直到在求值形式时第一次使用相应符号的绑定。任何值形式最多求值一次。使用这个宏需要进行词汇绑定 lexical-binding。

``` Elisp
(defun f (number)
  (thunk-let ((derived-number
              (progn (message "Calculating 1 plus 2 times %d" number)
                     (1+ (* 2 number)))))
    (if (> number 10)
        derived-number
      number)))


(f 5)
⇒ 5


(f 12)
-| Calculating 1 plus 2 times 12
⇒ 25

```

由于惰性绑定变量的特殊性质，设置它们是错误的(例如使用setq)。

#### 宏: `thunk-let* (bindings...) forms...` ####

这类似于 `thunk-let`，但是允许绑定中的任何表达式以这种 `thunk-let*` 形式引用前面的绑定。使用这个宏需要进行词汇绑定 lexical-binding。

``` Elisp
(thunk-let* ((x (prog2 (message "Calculating x...")
                    (+ 1 1)
                  (message "Finished calculating x")))
             (y (prog2 (message "Calculating y...")
                    (+ x 1)
                  (message "Finished calculating y")))
             (z (prog2 (message "Calculating z...")
                    (+ y 1)
                  (message "Finished calculating z")))
             (a (prog2 (message "Calculating a...")
                    (+ z 1)
                  (message "Finished calculating a"))))
  (* z x))

-| Calculating z...
-| Calculating y...
-| Calculating x...
-| Finished calculating x
-| Finished calculating y
-| Finished calculating z
⇒ 8
```

`thunk-let` 和 `thunk-let*` 隐式地使用thunk:它们的扩展创建辅助符号并将它们绑定到包含绑定表达式的thunk上。然后用一个表达式替换对主体形式中原始变量的所有引用，该表达式使用相应的辅助变量作为参数调用thunk-force。因此，任何使用 `thunk-let` 或 `thunk-let*` 的代码都可以重写为使用thunk，但在许多情况下，使用这些宏产生的代码比显式使用thunk更好。

## 11 Control Structures ##

[Control Structures](https://www.gnu.org/software/emacs/manual/html_node/elisp/Control-Structures.html)

Lisp程序由一组表达式 *expressions* 或形式 *forms* 组成(参见形式的种类 Kinds of Forms)。我们通过将这些 forms 封装在控制结构中来控制它们的执行顺序。控制结构是控制何时、是否或执行多少次它们所包含的表单的特殊表单 special forms。

最简单的执行顺序是顺序执行:首先是 form a，然后是 form b，依此类推。当您在函数体中或在Lisp代码文件的顶层连续编写多个 form 时，就会发生这种情况——表单将按照编写的顺序执行。我们称之为文本顺序 *textual order* 。例如，如果函数体由a和b两种形式组成，则函数的求值首先求a，然后求b。求b的结果成为函数的值。

显式控制结构 *Explicit control structures* 使执行顺序而不是顺序成为可能。(Explicit control structures make possible an order of execution other than sequential. )

Emacs Lisp提供了几种控制结构，包括其他种类的排序sequencing、条件conditionals、迭代interation和(受控controlled)跳转jumps——下面将讨论所有这些。内置控制结构是特殊的形式，因为它们的子形式不一定按顺序求值或不按顺序求值。您可以使用宏来定义自己的控制结构构造(请参阅宏 Macros)。


* Sequencing
* Conditionals
* Constructs for Combining Conditions
* Pattern-Matching Conditional
* Iteration
* Generators
* Nonlocal Exits

### 11.1 Sequencing ###

按照表单出现的顺序计算表单是控制权从一个表单传递到另一个表单的最常见方式。在某些上下文中，例如在函数体中，这是自动发生的。在其他地方，您必须使用控制结构结构来实现这一点:编程 *progn*，Lisp中最简单的控制结构。

一个 *progn* 的特殊形式是这样的:

``` Elisp
(progn a b c ...)
```

它说要按顺序执行form a b c，等等。这些形式称为程序体形式 *body of the progn form*。程序体中最后一个表单的值成为整个 progn 的值。

`(progn)`返回nil。

``` Elisp
(eq (progn) nil)
;  t
```

在Lisp的早期，progn 是连续执行两个或多个form并使用其中最后一个表单的值的唯一方法。但是程序员发现他们经常需要在函数体中使用程序，而(当时)只允许一种形式。因此，函数体被制成隐式程序(implicit progn):就像在实际程序体中一样，允许有几种形式。许多其他控制结构同样包含隐式程序。因此，程序的使用不像许多年前那么多了。现在最需要的是在一个 `unwind-protect`, and, or，或if的 then-part。

#### 特殊形式: `progn forms...` ####

这个特殊形式按文本顺序计算所有form，返回最终form的结果。

``` Elisp
(progn (print "The first form")
       (print "The second form")
	   (print "The third form"))
;  "The third form"
```

另外两个结构同样对一系列形式求值，但返回不同的值:

#### 特殊形式: `prog1 form1 forms...` ####

这个特殊的表单按文本顺序计算 `form1` 和所有表单，返回form1的结果。

``` Elisp
(prog1 (print "The first form")
       (print "The second form")
	   (print "The third form"))
;  "The first form"
```

下面是一种从变量x的列表中删除第一个元素的方法，然后返回前一个元素的值:

``` Elisp
(setq x '(a b c))
;  (a b c)
(prog1 (car x) (setq x (cdr x)))
;  a
x
;  (b c)
```

#### 特殊形式: `prog2 form1 form2 forms...` ####

这个特殊的表单按文本顺序计算form1、form2和以下所有表单，返回form2的结果。

``` Elisp
(prog2 (print "The first form")
       (print "The second form")
	   (print "The third form"))
;  "The second form"
```

### 11.2 Conditionals ###

[Conditionals](https://www.gnu.org/software/emacs/manual/html_node/elisp/Conditionals.html)

条件控制结构在备选方案中进行选择。Emacs Lisp有五种条件形式:

* `if`，这与其他语言非常相似;
* `When`和`unless`是if的变体;
* `cond`，这是一个概括的 case statement;
* `pcase`，后者是cond的泛化(参见模式匹配条件 Pattern-Matching Conditional)。

#### 特殊形式: `if condition then-form else-forms...` ####

它根据条件的值在 `then-form` 和 `else-forms` 之间进行选择。如果计算条件非nil，则计算then-form并返回结果。否则，else-forms将按文本顺序求值，并返回最后一个的值。if的else部分是隐式 progn 的一个例子。参见排序 Sequencing)。

如果condition的值为nil，并且没有给出else形式，则 if 返回nil。

if 是一种特殊形式，因为没有选择的分支永远不会被求值——它将被忽略。因此，在这个例子中，true不会被输出，因为print从未被调用:

``` Elisp
(if nil
    (print 'true)
	'very-false)
;  very-false
```

#### 宏: `when condition then-forms...` ####

when 是if的变体，没有else-forms，可能有几个then-forms。特别是,

``` Elisp
(when condition a b c)
```

等同于

``` Elisp
(if condition (progn a b c) nil)
```

#### 宏: `unless condition forms...` ####

unless 是if的变体，没有then-form:

``` Elisp
(unless condition a b c)
```

等同于

``` Elisp
(if condition nil
    a b c)
```

#### 特殊形式: `cond clause...` ####

cond 在任意数量的选项中进行选择。cond 中的每个 clause 必须是一个列表。这个列表的CAR是条件;剩下的元素，如果有的话，就是形体。因此，一个子句看起来像这样:

``` Elisp
(condition body-forms...)
```

cond 通过计算每个子句的条件，按文本顺序对子句进行测试。如果condition的值非nil，则子句成功;然后，cond计算其形体，并返回最后一个形体的值。其余的子句将被忽略。

如果condition的值为nil，则子句失败，因此cond移动到下一个子句，尝试其条件。

子句也可能是这样的:

``` Elisp
(condition)
```

然后，如果测试时condition为非nil，则cond返回condition的值。

如果每个条件求值为nil，那么每个子句都失败，cond返回nil。

下面的例子有四个子句，分别测试x的值为数字、字符串、缓冲区和符号的情况:

``` Elisp
(cond ((numberp x) x)
      ((stringp x) x)
	  ((bufferp x)
	   (setq temporary-hack x) ; multiple body-forms
	   (buffer-name x))        ; in one clause
	  ((symbolp x) (symbol-value x)))
```

通常，当前面的子句都没有成功时，我们希望执行最后一个子句。为此，我们使用 t 作为最后一个子句的条件，如下所示: `(t body-forms)`。形式t的求值为t，它永远不会为nil，所以这个子句永远不会失败，只要cond到达它。例如:

``` Elisp
(setq a 5)
(cond ((eq a 'hack) 'foo)
      (t "default"))
;  "default"
```

如果a的值为hack，则返回foo，否则返回字符串"default"。

任何条件构式都可以用cond或if来表示。因此，在它们之间的选择是一个风格问题。例如:

``` Elisp
(if a b c)

; 等同于

(cond (a b) (t c))
```

结合使用条件绑定变量会很方便。通常情况下，你计算了一个值，然后想对这个值做一些事情，如果它不是nil。最直接的方法就是这样写，例如:

``` Elisp
(let ((result 1 (do-computation)))
  (when result 1
    (let ((result2 (do-more result1)))
	  (when result2
	    (do-something result2)))))
```

由于这是一种非常常见的模式，因此Emacs提供了许多宏来简化和提高可读性。上面的代码可以写成如下方式:

``` Elisp
(when-let ((result1 (do-computation))
           (result2 (do-more result1)))
  (do-something result2))
```

关于这个主题有很多变体，下面简要介绍一下。

#### 宏: `if-let spec then-form else-forms...` ####

在 spec 中依次计算每个绑定，就像在 `let*` 中一样(参见局部变量Local Variables)，如果绑定值为nil就停止。如果都是非nil，则返回then-form的值，否则返回else-forms中的最后一个形式。

#### 宏: `when-let spec then-forms...` ####

像 `if-let`，但没有 `else-forms`。

#### 宏: `while-let spec then-forms...` ####

就像when-let一样，但是要重复，直到spec中的绑定为nil。返回值总是nil。

### 11.3Constructs for Combining Conditions ###

[Constructs for Combining Conditions](https://www.gnu.org/software/emacs/manual/html_node/elisp/Combining-Conditions.html)

本节描述经常与if和cond一起使用来表达复杂条件的结构。and和or结构也可以单独用作多种条件结构。

#### 函数: `not condition` ####

这个函数测试条件是否为假。如果condition为nil，则返回t，否则返回nil。函数not与null相同，如果您正在测试空列表，我们建议使用名称null。

``` Elisp
(not nil)
;  t

(not t)
;  nil

(null ())
;  t
```

#### 特殊形式: `and conditions...` ####

and 特殊形式测试是否所有条件都为真。它的工作原理是按照所写的顺序逐一计算条件。

如果任何条件的计算结果为nil，则无论剩余条件如何，and的结果都必须为nil;因此，and 立即返回nil，忽略剩下的条件。

如果所有条件都是非空的，那么最后一个条件的值就变成and形式的值。不带任何条件的 and 返回t，这很合适，因为所有的条件都是非nil。(想想看;哪一个没有?)

这里有一个例子。第一个条件返回整数1，它不是nil。类似地，第二个条件返回整数2，它不是nil。第三个条件为nil，因此不计算剩余的条件。

``` Elisp
(and (print 1) (print 2) nil (print 3))
;  nil
```

下面是一个使用and的更现实的例子:

``` Elisp
(setq foo '(x y z))
;  (x y z)
(consp foo)
;  t
(if (and (consp foo) (eq (car foo) 'x))
    (message "foo is a list starting with x"))
;  "foo is a list starting with x"

(setq foo 'y)
;  y
(consp foo)
;  nil
;  t
(if (and (consp foo) (eq (car foo) 'x))
    (message "foo is a list starting with x"))
;  nil
```

注意，如果 `(consp foo)` 返回nil，则不会执行(car foo)，从而避免出现错误。

表达式也可以用if或cond来写。方法如下:

``` Elisp
(and arg1 arg2 arg3)

(if arg1 (if arg2 arg3))

(cond (arg1 (cond (arg2 arg3))))
```

#### 特殊形式: `or conditions...` ####

or 特殊形式测试是否至少有一个条件为真。它的工作原理是按照所写的顺序逐一计算所有条件。

如果任何条件的计算结果为非nil值，则or的结果必须为非nil;因此，or立即返回，忽略剩余的条件。它返回的值是刚刚求值的条件的非空值。

如果所有条件都为nil，则or表达式返回nil。不带任何条件的 or 返回nil，这很合适，因为所有的条件都是nil。(想想看;哪一个没有?)

例如，下面的表达式测试x是nil还是整数0:

``` Elisp
(setq x nil)
;  nil
(or (eq x nil) (eq x 0))
;  t

(setq x 0)
;  0
(or (eq x nil) (eq x 0))
;  t

(setq x 1)
;  1
(or (eq x nil) (eq x 0))
;  nil
```

就像and结构一样，or 可以用cond来表示。例如:

``` Elisp
(or arg1 arg2 arg3)

(cond (arg1)
      (arg2)
	  (arg3))
```

你几乎可以把or写成if的形式，但不完全是:

``` Elisp
(if arg1 arg1
  (if arg2 arg2
    arg3))
```

这不是完全等价的因为它可以计算arg1或arg2两次。相比之下，(或arg1 arg2 arg3)永远不会对任何参数求值超过一次。



#### 函数: `xor condition1 condition2` ####

这个函数返回条件1和条件2的布尔异或值 boolean exclusive-or。也就是说，如果两个参数都为nil，或者两个参数都非nil, xor返回nil。否则，它返回该参数的非nil值。

注意，与or相反，两个参数总是被求值。

``` Elisp
(xor t nil)
;  t

(xor t t)
;  nil

(xor nil nil)
;  nil

(xor 1 0)
;  nil

(eq 0 nil)
;  nil

(eql 0 nil)
;  nil

(equal 0 nil)
;  nil
```

### 11.4 Pattern-Matching Conditional ###

#### 函数: `` ####

除了四种基本条件形式之外，Emacs Lisp还有一种模式匹配条件形式 `pcase` 宏，它是`cond`和`cl-case`的混合(请参阅Common Lisp扩展中的条件 Conditionals in Common Lisp Extensions)，克服了它们的局限性，并引入了模式匹配编程风格 *pattern matching programming style*。`pcase`克服的限制是:

* cond 通过计算它的每个子句的谓词条件(参见条件)在备选项中进行选择。主要的限制是，在条件中允许绑定的变量对子句的主体形式不可用。

另一个烦恼(与其说是限制，不如说是不便)是，当一系列条件谓词实现相等性测试时，会有大量重复的代码。(cl-case解决了这个不便。)

cl-case宏通过对一组特定值评估其第一个参数的相等性来在备选项中进行选择。

它的局限性有两方面:

1. 相等性测试使用eql。
2. 这些值必须事先知道并写入。

这些使得 `cl-case` 不适合字符串或复合数据结构(例如，列表或向量)。(cond没有这些限制，但它有其他限制，见上文。)

从概念上讲，pcase宏借用了cl-case的第一个参数焦点(first-arg focus)和cond的子句处理流程，用模式匹配的一种变体——等式检验的泛化取代了condition，并添加了一些工具，以便您可以简洁地表达子句的谓词，并安排在子句的谓词和主体形式之间共享let绑定。

谓词的简明表达式称为模式 pattern。当对第一个参数的值调用的谓词返回非nil时，我们说“模式匹配值”(或者有时“值匹配模式”)。


* The pcase macro
* Extending pcase
* Backquote-Style Patterns
* Destructuring with pcase Patterns

#### 11.4.1 The pcase macro ####

##### 宏: `pcase expression &rest clauses` #####

从句中的每个子句的形式是: `(pattern body-forms...)`

计算表达式以确定其值，`expval`。查找模式与`expval`匹配的子句中的第一个子句，并将控制权传递给该子句的主体形式。

如果存在匹配，则pcase的值是成功子句中最后一个`body-forms`的值。否则，pcase的计算结果为nil。

每个模式都必须是`pcase`模式，它可以使用下面定义的核心模式之一，也可以使用通过 `pcase-defmacro` 定义的模式之一(请参阅扩展pcase Extending pcase)。

本小节的其余部分描述了核心模式 core pattern 的不同形式，给出了一些示例，并在使用某些模式形式提供的let绑定功能时给出了重要的注意事项。核心模式可以有以下形式:

* `_` (underscore): 匹配任何exp。这也被称为不关心或通配符 wildcard。
* `'val` 匹配如果expval等于val。比较就像通过equal来完成(参见相等谓词Equality Predicates)。
* `keyword`, `integer`, `string`: 如果expval等于字面对象 liberal obejct，则匹配。这是上面val的一个特例，可能是因为这些类型的文字对象是自引用的。
* `symbol` 匹配任何expval，并将 let-binds 符号附加到expval，以便此绑定可用于body-forms(参见动态绑定 Dynamic Binding)。如果symbol是序列模式`seqpat`的一部分(例如，下面使用and)，则该绑定也可用于seqpat中出现symbol之后的部分。这种用法有一些注意事项，请参阅注意事项 caveats。两个要避免的符号是t，它的行为类似于上面的 `_`，是不赞成使用的，而nil表示错误。同样，绑定关键字符号也没有意义(参见永远不变的变量 Variables that Never Change)。
* 反引号qpat `` `qpat ``: 反引号样式的模式。有关详细信息，请参阅反引号样式模式 Backquote-Style Patterns。
* `(cl-type type)`: 如果expval的类型为type，则匹配type, type是被cl-typep接受的类型描述符(参见公共Lisp扩展中的类型谓词Type Predicates in Common Lisp Extensions)。例子:

``` Elisp
(cl-type integer)
(cl-type (integer 0 10))
```

(pred function) 如果谓词函数在以expval方式调用时返回非nil，则匹配。可以使用语法 `(pred (not function))`来否定测试。谓词函数可以有下列形式之一:

* `function name (a symbol)`: 用一个参数expval调用命名函数。例如:integerp
* `lambda expression`: 使用一个参数expval调用匿名函数(参见Lambda表达式)。示例: `(lambda (n) (= 42 n))`
* `function call with n args`: 调用函数(函数调用的第一个元素)时带有n个参数(其他元素)和额外的第n+1个参数expval。示例: `(= 42)` 在本例中，函数为`=`，n为1，实际的函数调用变为:(= 42 expval)。
* `(app function pattern)`: 在expval上调用的if函数返回匹配pattern的值。函数可以采用上面为pred描述的一种形式。然而，与pred不同的是，app根据模式测试结果，而不是根据布尔真值。
* `(guard boolean-expression)`: 如果布尔表达式的计算结果为非nil，则匹配。
* `(let pattern expr)`: 计算expr以获得expr，并在expr与模式匹配时进行匹配。(之所以叫let，是因为pattern可以使用symbol将符号绑定到值。)

排序模式 sequencing pattern(也称为seqpat)是一种按顺序处理子模式参数的模式。有两个for case: `and`和`or`。它们的行为方式与共享其名称的特殊形式类似(请参阅组合条件的构造 Constructs for Combining Conditions)，但它们处理的不是值，而是子模式。

* `and pattern1...`: 尝试匹配模式1…，按顺序，直到其中一个无法匹配。在这种情况下，和同样无法匹配，并且其余的子模式不进行测试。如果所有子模式匹配，则匹配。
* `or pattern1 pattern2...`: 尝试按顺序匹配 `pattern1, pattern2，...`，直到其中一个成功。在这种情况下，或同样匹配，并且不测试其余的子模式。为了向形体呈现一致的环境(参见求值介绍 Introduction to Evaluation)(从而避免匹配时的求值错误)，模式绑定的变量集是每个子模式绑定的变量的并集。如果一个变量没有被匹配的子模式绑定，那么它被绑定为nil。
* `(rx rx-expr...)`: 使用rx regexp表示法(参见rx结构化regexp表示法 The rx Structured Regexp Notation)，就像通过字符串匹配一样，根据regexp rx-expr…匹配字符串。

除了常用的rx语法外，`rx-expr...`还可以包含以下结构:

* `(let ref rx-expr...)`: 将符号ref绑定到匹配 `rx-expr...` 的子匹配Ref以body形式绑定到submatch的字符串或nil，但也可以在backref中使用。
* `(backref ref)`: 就像标准的 backref 结构一样，但是ref在这里也可以是由前一个 `(let ref...)`结构引入的名称。

##### 例子 Advantage Over cl-case #####

这里有一个例子，突出了pcase相对于cl-case的一些优势(参见Common Lisp扩展中的条件 Conditionals in Common Lisp Extensions)。

``` Elisp
(pcase (get-return-code x)
  ;; string
  ((and (pred stringp) msg)
   (message "%s" msg))

  ;; symbol
  ('success       (message "Done!"))
  ('would-block   (message "Sorry, can't do it now"))
  ('read-only     (message "The schmiliblick is read-only"))
  ('access-denied (message "You do not have the needed rights"))

  ;; default
  (code           (message "Unknown return code %s" code)))
```

使用cl-case，您需要显式声明一个局部变量code来保存get-return-code的返回值。另外，cl-case很难用于字符串，因为它使用eql进行比较。

##### 例子: Using and #####

常见的习惯用法idiom是编写以and开头的模式，并使用一个或多个符号子模式为后面的子模式(以及主体形式)提供绑定。例如，下面的模式匹配单位数整数。

``` Elisp
(and
  (pred integerp)
  n                      ; bind n to expval
  (guard (<= -9 n 9)))
```

首先，pred匹配if `(integerp expval)` 的求值为非nil。接下来，n是一个符号模式，它匹配任何东西并将n绑定到expval。最后，如果布尔表达式 `(<= -9 n 9)` (注意对n的引用)的计算结果为非nil，则guard匹配。如果所有这些子模式都匹配，则匹配。

##### 例子： Reformulation with pcase #####

下面是另一个示例，展示了如何将一个简单的匹配任务从其传统实现 `(function grok/traditional)` 重新定义为使用pcase `(function grok/pcase)` 的匹配任务。如果OBJ是 `"key:NUMBER"` 形式的字符串，则返回字符串 `NUMBER`。否则，返回列表 `("149" default)`。首先，传统的实现(参见正则表达式 Regular Expressions):

``` Elisp
(defun grok/traditional (obj)
  (if (and (stringp obj)
           (string-match "^key:\\([[:digit:]]+\\)$" obj))
	  (match-string 1 obj)
    (list "149" 'default)))

(grok/traditional "key:0")
;  "0"
(grok/traditional "key:149")
;  "149"
(grok/traditional "monolith")
;  ("149" default)
```

这个重新表述演示了符号绑定以及or、and、pred、app和let。

``` Elisp
(defun grok/pcase (obj)
  (pcase obj
    ((or
	  (and
	    (pred stringp)
		(pred (string-match
		      "^key:\\([[:digit:]]+\\)$"))
	    (app (match-string 1)
		     val))
	  (let val (list "149 'default")))
	 val)))

(grok/pcase "key:0")
;  "0"
(grok/pcase "key:149")
;  "149"
(grok/pcase "monolith")
;  ("149 'default")
```

`grok/pcase` 的大部分是pcase形式的单个子句，即第1-8行中的模式，第9行中的(单个)主体形式。模式是or，它依次尝试匹配其参数子模式，首先是and(第2-7行)，然后是let(第8行)，直到其中一个成功。

与前面的示例一样(参见示例1)，并以pred子模式开始，以确保以下子模式使用正确类型的对象(在本例中为字符串)。如果 `(stringp expval)`返回nil, pred失败，因此and也失败。

下一个pred(第4-5行)计算 `(string-match RX expval)`并在结果非nil时进行匹配，这意味着expval具有所需的形式: `key:NUMBER`。再一次，失败了，pred也失败了。

最后(在这个系列和子模式中)，app计算(match-string 1 expval)(第6行)以获得临时值tmp(即“NUMBER”子字符串)并尝试将tmp与模式val(第7行)相匹配。因为这是一个符号模式，它无条件匹配并额外将val绑定到tmp。

现在 app 匹配了，所有 and 子模式都匹配了，所以 and 匹配了。同样，一旦and匹配，or 匹配而不继续尝试子模式let(第8行)。

让我们考虑这样一种情况:obj不是字符串，或者它是字符串但形式错误。在这种情况下，其中一个pred(第3-5行)无法匹配，因此and(第2行)无法匹配，因此or(第1行)继续尝试子模式let(第8行)。

首先，let求值 `(list "149" 'default)` 以获取 `("149" default)`，然后尝试将expreval与模式val进行匹配。由于这是一个符号模式，因此它会无条件匹配并将val额外绑定到expreval。现在let已经匹配了，或者匹配了。

注意and和如何让子模式以相同的方式结束:在进程绑定val中尝试(总是成功地)匹配符号模式val。因此，or总是匹配并且控制总是传递给体形式(第9行)。因为这是成功匹配的pcase子句中的最后一个体形式，它是pcase的值，同样也是grok/pcase的返回值(参见什么是函数? What Is a Function?)。

##### Caveats for symbol in Sequencing Patterns 序列模式中符号的注意事项 #####

前面的示例都使用排序模式 sequencing patterns，其中以某种方式包含符号子模式。这里有一些关于这种用法的重要细节。

1. 当symbol在seqpat中出现不止一次时，第二次和随后的出现不会展开为重新绑定，而是展开为使用eq进行相等性测试。

下面的例子是一个带有两个子句和两个seqpat的pcase形式，A和B。A和B首先检查expval是一对(使用pred)，然后将符号绑定到expval的car和cdr(分别使用一个 app)。

对于A，因为符号 st 被提及两次，第二次提及使用eq成为一个相等性测试。另一方面，B使用两个单独的符号s1和s2，它们都成为独立的绑定。

``` Elisp
(defun grok (object)
  (pcase object
    ((and (pred consp)        ; seqpat A
	      (app car st)        ; first mention: st
		  (app cdr st))       ; second mention: st
	 (list 'eq st))
	 
	((and (pred consp)        ; seqpat B
	      (app car s1)        ; first mention: s1
		  (app cdr s2))       ; first mention: s2
	 (list 'not-eq s1 s2))))
```

2. 副作用代码引用符号未定义。避免的。例如，下面是两个类似的函数。都使用and, symbol和guard:

``` Elisp
(defun square-double-digit-p/CLEAN (integer)
  (pcase (* integer integer)
    ((and n (guard (< 9 n 100))) (list 'yes n))
	(sorry (list 'no sorry))))

(square-double-digit-p/CLEAN 9)
;  (yes 81)
(square-double-digit-p/CLEAN 3)
;  (no 9)
(square-double-digit-p/CLEAN 4)
;  (yes 16)
(square-double-digit-p/CLEAN 10)
;  (no 100)


(defun square-double-digit-p/MAYBE (integer)
  (pcase (* integer integer)
    ((and n (guard (< 9 (cl-incf n) 100))) (list 'yes n))
	(sorry (list 'no sorry))))

(square-double-digit-p/MAYBE 9)
;  (yes 81)
(square-double-digit-p/MAYBE 3)
;  (yes 9)  ; WRONG!
;; void function incf, you need to use cl-incf
```

区别在于guard中的布尔表达式:CLEAN简单而直接地引用n，而MAYBE在表达式`(incf n)`中引用n时会产生副作用。当integer为3时，会发生以下情况:

* 第一个n将它绑定到expval，即计算 `(* 3 3)`或9的结果。
* 布尔表达式求值:

``` PlainText
start:   (< 9 (incf n)        100)
becomes: (< 9 (setq n (1+ n)) 100)
becomes: (< 9 (setq n (1+ 9)) 100)

becomes: (< 9 (setq n 10)     100)
                                   ; side-effect here!
becomes: (< 9       n         100) ; n now bound to 10
becomes: (< 9      10         100)
becomes: t
```

因为求值的结果是非空的，所以guard匹配, and 匹配，并且控制传递给该子句的主体形式。

除了断言9是一个两位数整数的数学错误之外，MAYBE还有另一个问题。体形式再次引用n，但我们根本看不到更新后的值10。它怎么了?

总而言之，最好完全避免对符号模式的副作用引用，不仅在布尔表达式(在guard中)中，而且在expr(在let中)和函数(在pred和app中)中也是如此。

3. 匹配时，子句的主体形式可以引用模式允许绑定的符号集。当seqpat为and时，该集合是它的每个子模式的所有符号的并集。这是有意义的，因为要匹配，所有子模式必须匹配。

当seqpat为or时，情况就不同了:or匹配匹配的第一个子模式;其余的子模式将被忽略。让每个子模式绑定一组不同的符号是没有意义的，因为主体形式没有办法区分哪个子模式匹配，并在不同的集合中进行选择。例如:

``` Elisp
(require 'cl-lib)
(pcase (read-number "Enter an integer: ")
  ((or (and (pred cl-evenp)
            e-num)
	   o-num)
   (list e-num o-num)))

Enter an integer: 42
error→ Symbol’s value as variable is void: o-num

Enter an integer: 149
error→ Symbol’s value as variable is void: e-num
```

计算 body 形式 `(list e-num o-num)`表示错误。为了区分子模式，您可以使用另一个符号，在所有子模式中名称相同，但值不同。修改上面的例子:

``` Elisp
(require 'cl-lib)
(pcase (read-number "Enter an integer: ")
  ((and num                                ; line 1
        (or (and (pred cl-evenp)           ; line 2
                 (let spin 'even))         ; line 3
            (let spin 'odd)))              ; line 4
   (list spin num)))                       ; line 5


Enter an integer: 42
⇒ (even 42)

Enter an integer: 149
⇒ (odd 149)
```

第1行 “factors out” 与and和符号(在本例中为num)的指数绑定。在第2行，或以与之前相同的方式开始，但不是绑定不同的符号，而是使用两次let(第3-4行)在两个子模式中绑定相同的符号旋转。spin的值区分子模式。主体形式引用这两个符号(第5行)。

#### 11.4.2 Extending pcase ####

[Extending pcase](https://www.gnu.org/software/emacs/manual/html_node/elisp/Extending-pcase.html)

pcase宏支持几种模式(参见模式匹配条件)。您可以使用pcase-defmacro宏添加对其他类型模式的支持。

##### 宏: `pcase-defmacro name args [doc] &rest body` #####

为pcase定义一种新的模式，以 `(name actual-args)` 的形式调用。pcase宏将其扩展为一个对body求值的函数调用，其工作是在args绑定到实际args的环境中，将调用的模式重写为其他模式。

此外，安排将doc与pcase的文档字符串一起显示。按照惯例，doc应该使用EXPVAL来表示计算表达式的结果(第一个参数到pcase)。

通常，body会重写被调用的模式以使用更基本的模式。虽然所有的模式最终都减少到核心模式，但身体不需要立即使用核心模式。下面的示例定义了两个模式，分别命名为less-than和integer-less-than。

``` Elisp
(pcase-defmacro less-than (n)
  "Matches if EXPVAL is a number less than N."
  `(pred (> ,n)))

(pcase-defmacro integer-less-than (n)
  "Matches if EXPVAL is an integer less than N."
  `(and (pred integerp)
        (less-than ,n)))
```

请注意，文档字符串以通常的方式提到了参数(在本例中，只有一个:n)，并且还按照约定提到了EXPVAL。第一次重写(即，body for less-than)使用一个核心模式:pred。第二个使用两个核心模式:and和pred，以及新定义的less-than模式。两者都使用单个反引号结构(参见反引号 Backquote)。

#### 11.4.3 Backquote-Style Patterns ####

> 本小节的内容看不懂, 以后回来看

本小节描述反向引用样式模式，这是一组简化结构匹配的内置模式。有关背景信息，请参见模式匹配条件。

反引号样式的模式是一组功能强大的pcase模式扩展(使用pcase-defmacro创建)，可以很容易地将expval与其结构规范相匹配。

例如，要匹配expval，它必须是一个包含两个元素的列表，其中第一个元素是特定的字符串，第二个元素是任何值，你可以编写一个核心模式:

``` Elisp
(and (pred listp)
     ls

     (guard (= 2 (length ls)))
     (guard (string= "first" (car ls)))
     (let second-elem (cadr ls)))
```

或者你可以写一个等价的反引号样式的模式:

``` Elisp
`("first" ,second-elem)
```

反引号样式的模式更简洁，类似于expval的结构，并且避免了绑定。

反引号样式的模式有 `` `qpat `` 的形式，其中qpat可以有以下形式:

* `(qpat1 . qpat2)`: 如果expval是一个car匹配qpat1且cdr匹配qpat2的cons单元格，则匹配。这很容易推广到 `(qpat1 qpat2...)`中的列表。
* `[qpat1 qpat2 ... qpatm]`: 如果expval是长度为m的向量，其0..(m-1)个元素分别匹配qpat1, qpat2 .. qpatm，则匹配。
* `symbol`, `keyword`, `number`, `string` 如果expval的对应元素等于指定的文字对象，则匹配。
* `,pattern`: 如果expval的对应元素匹配pattern，则匹配。请注意，模式是pcase支持的任何类型。(在上面的例子中，second-elem是一个符号核心模式;因此，它匹配任何东西，并将第二元素let绑定。)

对应的元素是expval中与qpat在反引号样式模式中的结构位置相同的部分。(在上面的例子中，second-elem的对应元素是expval的第二个元素。)

下面是一个使用pcase实现小表达式语言的简单解释器的示例(注意，这需要在fn子句中对lambda表达式进行词法绑定，以正确捕获body和arg(参见词法绑定 Lexical Binding):

``` Elisp
(defun evaluate (form env)
  (pcase form
    (`(add ,x ,y)       (+ (evaluate x env)
                           (evaluate y env)))

    (`(call ,fun ,arg)  (funcall (evaluate fun env)
                                 (evaluate arg env)))
    (`(fn ,arg ,body)   (lambda (val)
                          (evaluate body (cons (cons arg val)
                                               env))))

    ((pred numberp)     form)
    ((pred symbolp)     (cdr (assq form env)))
    (_                  (error "Syntax error: %S" form))))

```

前三个子句使用反引号样式的模式。`` `(add ,x ,y) `` 是一个模式，它检查表单是否是一个以文字符号add开头的三元素列表，然后提取第二个和第三个元素，并分别将它们绑定到符号x和y。这就是所谓的解构，请参阅使用pcase模式进行解构。子句体计算x和y并添加结果。类似地，call子句实现函数调用，fn子句实现匿名函数定义。

其余子句使用核心模式。如果表单是数字，则(pred numberp)匹配。匹配时，身体会进行评估。如果形式是一个符号，则(pred symbolp)匹配。匹配时，程序体在env中查找符号并返回它的关联。最后，_是一个可以匹配任何内容的通用模式，因此它适合报告语法错误。

下面是一些用这种小语言编写的示例程序，包括它们的计算结果:

``` Elisp
(evaluate '(add 1 2) nil)                 ⇒ 3
(evaluate '(add x y) '((x . 1) (y . 2)))  ⇒ 3
(evaluate '(call (fn x (add 1 x)) 2) nil) ⇒ 3
(evaluate '(sub 1 2) nil)
```

#### 11.4.4 Destructuring with pcase Patterns ####


Pcase模式不仅表达了它们可以匹配的对象的形式的条件，而且还可以提取这些对象的子字段。例如，我们可以从一个列表中提取2个元素，这个列表是变量my-list的值，代码如下:

``` Elisp
(setq my-list '(add 1 2))
(pcase my-list
  (`(add ,x ,y) (message "Contains %S and %S" x y)))
;  "Contains 1 and 2"
```

这不仅会提取x和y，还会额外测试my-list是否恰好包含3个元素，并且其第一个元素是符号add。如果这些测试中的任何一个失败，pcase将立即返回nil而不调用message。

提取存储在对象中的多个值称为解构。使用pcase模式允许执行解构绑定，这类似于局部绑定(请参阅局部变量 Local Variables)，但通过从兼容结构的对象中提取这些值来为变量的多个元素提供值。

本节中描述的宏使用pcase模式来执行解构绑定。对象具有兼容结构的条件意味着对象必须匹配模式，因为只有这样才能提取对象的子字段。例如:

``` Elisp
(pcase-let ((`(add ,x ,y) my-list))
  (message "Contains %S and %S" x y))
```

执行与前面的示例相同的操作，只是它直接尝试从my-list中提取x和y，而不首先验证my-list是否是一个具有正确数量的元素并将add作为其第一个元素的列表。当对象实际上与模式不匹配时，精确的行为是未定义的，尽管主体不会被静默地跳过:要么发出错误信号，要么运行主体，其中一些变量可能绑定到任意值(如nil)。

对于解构绑定有用的pcase模式通常是在反向引用样式模式中描述的那些模式，因为它们表达了将匹配的对象结构的规范。

有关解构绑定的另一种工具，请参见seq-let。

##### 宏: `pcase-let bindings body...` #####

根据绑定对变量进行解构绑定，然后求值。

Bindings是格式为 `(pattern exp)` 的绑定列表，其中exp是要求值的表达式，pattern是pcase模式。

所有exp首先求值，然后根据它们各自的模式进行匹配，引入新的变量绑定，然后在主体内部使用。变量绑定是通过解构模式元素到被求值的exp对应元素值的绑定来产生的。

这里有一个简单的例子:

``` Elisp
(pcase-let ((`(,major ,minor)
             (split-string "image/png" "/")))
  minor)
;  "png"
```

##### 宏: `pcase-let* bindings body...` #####

根据绑定对变量进行解构绑定，然后求值。

Bindings是格式为 `(pattern exp)` 的绑定列表，其中exp是要求值的表达式，pattern是pcase模式。变量绑定是通过解构模式元素到被求值的exp对应元素值的绑定来产生的。

与pcase-let不同，但与 `let*` 相似，在处理绑定的下一个元素之前，每个exp都针对其相应的模式进行匹配，因此在每个绑定中引入的变量绑定除了在主体中可用外，还可以在后面绑定的exp中可用。

##### 宏: `pcase-dolist (pattern list) body...` #####

对list的每个元素执行一次body，在每次迭代中执行模式变量到list元素对应子字段值的解构绑定。绑定就像通过pcase-let执行一样。当pattern是一个简单变量时，它最终等同于dolist(参见迭代 Iteration)。

##### 宏: `pcase-setq pattern value...` #####

以setq形式给变量赋值，并根据其各自的模式解构每个值。

##### 宏: `pcase-lambda lambda-list &rest body` #####

这类似于lambda，但允许每个参数都是一个模式。例如，下面是一个简单的函数，它接受一个cons单元格作为参数:

``` Elisp
(setq fun
      (pcase-lambda (`(,key . ,val))
	    (vector key (* val 10))))
(funcall fun '(foo . 2))
;  [foo 20]
```

### 11.5 Iteration ###

迭代意味着重复执行程序的某些部分。例如，你可能想要对列表中的每个元素重复一次计算，或者对从0到n的每个整数重复一次计算。你可以在Emacs Lisp中使用特殊形式while来完成:

#### 特殊形式: `while condition forms...` ####

While首先计算条件。如果结果非nil，则按文本顺序计算表单。然后重新求值condition，如果结果非nil，则再次求值forms。这个过程一直重复，直到condition的计算结果为nil。

对可能发生的迭代次数没有限制。循环将继续，直到其中一个条件的计算结果为nil，或者直到跳出错误或抛出(请参阅非局部退出 Nonlocal Exits)。

while形式的值总是nil。

``` Elisp
(setq num 0)
;  0

(while (< num 4)
  (princ (format "Iteration %d." num))
  (setq num (1+ num)))
; nil
```

要编写一个repeat-until循环，它将在每次迭代中执行一些操作，然后执行end-test，将end-test后跟的循环体放在程序中作为while的第一个参数，如下所示:

``` Elisp
(while (progn
         (forward-line 1)
		 (not (looking-at "^$"))))
```

它向前移动一行，并继续逐行移动，直到到达空行。它的特殊之处在于，while没有主体，只有end test(它也做移动点的实际工作)。

dolist和dotimes宏为编写两种常见的循环提供了方便的方法。

#### 宏: `dolist (var list [result]) body...` ####

该构造对list的每个元素执行一次body，在本地绑定变量var以保存当前元素。然后返回计算结果的值，如果省略result则返回nil。例如，以下是如何使用dolist来定义 reverse 函数:

``` Elisp
(defun reverse (list)
  (let (value)
    (dolist (elt list value)
	  (setq value (cons elt value)))))
```

#### 宏: `dotimes (varf count [result]) body...` ####

该构造对从0(含)到count(不含)的每个整数执行一次body，将变量var绑定到当前迭代的整数。然后返回计算结果的值，如果省略result则返回nil。不赞成使用result。下面是一个使用dotimes将某事做100次的例子:

``` Elisp
(dotimes (i 100)
  (insert "I will not obey absurd orders\n"))
```

### 11.6 Generators ###

生成器是一个函数，它产生一个可能无限的值流。每次函数产生一个值时，它都会挂起自己并等待调用者请求下一个值。

#### 宏: `iter-defun name args [doc] [declare] [interactive] body...` ####

`iter-defun` 定义了一个生成器函数。生成器函数与普通函数具有相同的签名，但工作方式不同。生成器函数在调用时不执行body，而是返回一个迭代器对象。该迭代器运行body生成值，发出一个值，并在 `iter-yield` 或 `iter-yield-from` 出现时暂停。当body正常返回时，`iter-next` 用body的结果作为条件数据通知 `iter-end-of-sequence`。

任何类型的Lisp代码在代码体内部都是有效的，但是 `iter-yield` 和 `iter-yield-from` 不能出现在 `unwind-protect` 形式中。

#### 宏: `iter-lambda args [doc] [interactive] body...` ####

`iter-lambda` 生成一个未命名的生成器函数，其工作方式与使用iter-defun生成的生成器函数类似。

#### 宏: `iter-yield value` ####

当它出现在生成器函数中时，`iter-yield` 表示当前迭代器应该暂停并从 `iter-next` 返回值。`iter-yield` 计算为下一次调用 `iter-next` 的value参数。

#### 宏: `iter-yield-from iterator` ####

`iter-yield-from` 返回迭代器产生的所有值，并计算为迭代器生成器函数正常返回的值。当它拥有控制权时，迭代器使用 `iter-next`接收发送给迭代器的值。

要使用生成器函数，首先按常规调用它，生成一个迭代器对象。迭代器是生成器的特定实例。然后使用 `iter-next` 从这个迭代器中检索值。当没有更多的值可以从迭代器中提取时，iter-next用迭代器的最终值引发一个 `iter-end-of-sequence` 条件。

需要注意的是，生成器函数体只执行对 `iter-next` 的内部调用。调用用 `iterter-defun`定义的函数会产生一个迭代器;您必须使用 `iter-next` 来驱动这个迭代器，以便发生任何有趣的事情。每次调用生成器函数都会产生一个不同的迭代器，每个迭代器都有自己的状态。

#### 函数: `iter-next iterator &optional value` ####

从迭代器中检索下一个值。如果没有更多的值要生成(因为迭代器的生成器函数返回)，`iter-next` 表示 `iter-end-of-sequence`;与此条件关联的数据值是迭代器生成器函数返回的值。

Value被发送到迭代器中，并成为迭代yield计算的值。Value在对给定迭代器的第一次 `iter-next` 调用时被忽略，因为在迭代器的生成器函数开始时，生成器函数没有计算任何 `iter-yield` 形式。

#### 函数: `iter-close iterator` ####

如果迭代器挂起在unwind保护的主体中，并且变得不可访问，Emacs将在垃圾收集通过后最终运行unwind处理程序。(注意，在unwind_protect的unwindform中，iter-yield是非法的。)要确保在此之前运行这些处理程序，请使用iter-close。

提供了一些方便的函数使使用迭代器更容易:

#### 宏: `iter-do (var iterator) body...` ####

运行 body，var绑定到迭代器产生的每个值。

Common Lisp循环功能还包含使用迭代器的特性。参见公共Lisp扩展中的循环功能。Loop Facility

下面这段代码演示了使用迭代器的一些重要原则。

``` Elisp
(require 'generator)
(iter-defun my-iter (x)
  (iter-yield (1+ (iter-yield (1+ x))))
  ;; Return normally
  -1)

(let* ((iter (my-iter 5))
       (iter2 (my-iter 0)))
  ;; Print 6
  (print (iter-next iter))
  ;; Print 9
  (print (iter-next iter 8))
  ;; Prints 1; iter and iter2 have distinct states
  (print (iter-next iter2 nil))

  ;; We expect the iter sequence to end now
  (condition-case x
    (iter-next iter)
	(iter-end-of-sequence
	  ;; Prints -1, which my-iter returned normally
	  (print (cdr x)))))
```

### 11.7 Nonlocal Exits ###

*nonlocal exit* 是将控制从程序中的一个点转移到另一个远程点。在Emacs Lisp中，错误可能导致非本地退出;您也可以在显式控制下使用它们。非局部退出取消由退出的构造所做的所有变量绑定。


* Explicit Nonlocal Exits: catch and throw
* Examples of catch and throw
* Errors
* Cleaning Up from Nonlocal Exits

#### 11.7.1 Explicit Nonlocal Exits: catch and throw ####

[Explicit Nonlocal Exits: catch and throw](https://www.gnu.org/software/emacs/manual/html_node/elisp/Catch-and-Throw.html)

大多数控制构造只影响构造本身内的控制流。函数throw是正常程序执行规则的例外:它在请求时执行非本地退出。(还有其他异常，但它们仅用于错误处理。)throw在catch中使用，并跳转回该catch。例如:

``` Elisp
(defun foo-outer ()
  (catch 'foo
    (foo-inner)))

(defun foo-inner ()
  ...
  (if x
      (throw 'foo t))
  ...)
```

throw form，如果执行，将控制直接转移回相应的catch，后者立即返回。不执行抛出后的代码。throw的第二个参数用作catch的返回值。

throw函数根据第一个参数查找匹配的catch:它搜索第一个参数为与throw中指定的参数相等的catch。如果有多个适用catch，则最内层的catch优先。因此，在上面的例子中，throw指定了foo, foo-outer中的catch指定了相同的符号，因此catch是适用的(假设两者之间没有其他匹配的catch)。

执行throw退出匹配catch之前的所有Lisp构造，包括函数调用。当绑定构造(如let或函数调用)以这种方式退出时，绑定将被解除绑定，就像这些构造正常退出时一样(参见局部变量 Local Variables)。同样地，throw恢复由 `save-excursion` 保存的缓冲区和位置(参见Excursions)，以及由 `save-restriction`保存的缩小状态。当退出 `unwind-protect` 特殊形式时，它还运行使用该表单建立的任何清理(参见从非本地出口进行清理 Cleaning Up from Nonlocal Exits)。

throw在词法上不需要出现在它跳转到的catch中。它同样可以从catch中调用的另一个函数中调用。只要throw按时间顺序发生在进入catch之后，并在退出catch之前，它就可以访问该catch。这就是为什么可以在 `exit-recursive-edit` 等命令中使用throw，这些命令会将throw扔回编辑器命令循环(参见递归编辑 Recursive Editing)。

> 注意:大多数其他版本的Lisp，包括Common Lisp，都有几种非顺序转移控制的方法:例如，return、return-from 和 go。Emacs Lisp只有throw。cl-lib库提供了其中一些版本。参见公共Lisp扩展中的块和出口 Blocks and Exits。

##### 特殊形式: `catch tag body...` #####

catch为抛出函数建立一个返回点。返回点通过标签与其他返回点区分开来，标签可以是除nil以外的任何Lisp对象。参数标签通常在返回点建立之前求值。

返回点生效后，catch按文本顺序计算语句体的形式。如果 body 正常执行(没有错误或非局部退出)，则从catch返回最后一个 body 的值。

如果在body执行期间执行throw，并指定相同的value标签，则catch形式立即退出;它返回的值是作为throw的第二个参数指定的任何值。

##### 函数: `throw tag value` #####

throw的目的是从先前用catch建立的返回点返回。参数标签用于在各种现有返回点之间进行选择;它必须等于catch中指定的值。如果多个返回点匹配标签，则使用最里面的返回点。

实参value用作该catch的返回值。

如果tag没有返回点，则使用 data `(tag value)` 发出无捕获错误信号 `no-catch` error。

#### 11.7.2 Examples of catch and throw ####

使用catch和throw的一种方法是从双重嵌套循环中退出。(在大多数语言中，这将通过goto来完成。)这里我们计算 `(foo i j)` 对于i和j从0到9的变化:

``` Elisp
(defun search-foo ()
  (catch 'loop
    (let ((i 0))
	  (while (< i 10)
	    (let ((j 0))
		  (while (< j 10)
		    (if (foo i j)
			    (throw 'loop (list i j)))
			(setq j (1+ j))))
		(setq i (1+ i))))))
```

如果foo返回非nil，则立即停止并返回i和j的列表。如果foo总是返回nil，则catch正常返回，并且值为nil，因为这是while的结果。

这里有两个棘手的例子，略有不同，同时显示两个返回点。首先，两个具有相同标签的返回点，hack:

``` Elisp
(defun catch2 (tag)
  (catch tag
    (throw 'hack 'yes)))
;  catch2

(catch 'hack
  (print (catch2 'hack))
  'no)

```

因为两个返回点都有与throw匹配的标记，所以它会转到内部的返回点，即在catch2中建立的返回点。因此，catch2通常返回值yes，并打印该值。最后，外部catch中的第二个body形式(no)被求值并从外部catch返回。

现在让我们改变给catch2的参数:

``` Elisp
(catch 'hack
  (print (catch2 'quux))
  'no)
;  yes
```

我们仍然有两个返回点，但这一次只有外部的一个有标签hack;内部的标签quux代替。因此，throw使外部catch返回值yes。函数print永远不会被调用，body-form 'no也永远不会被求值。

#### 11.7.3 Errors ####

[Errors](https://www.gnu.org/software/emacs/manual/html_node/elisp/Errors.html)

当Emacs Lisp试图计算由于某种原因无法计算的表单时，它会发出错误信号。

当发出错误信号时，Emacs的默认反应是打印错误消息并终止当前命令的执行。在大多数情况下，这是正确的做法，例如在缓冲区末尾键入C-f。

在复杂的程序中，简单的终止可能不是您想要的。例如，程序可能对数据结构进行了临时更改，或者创建了应该在程序结束之前删除的临时缓冲区。在这种情况下，您可以使用 `unwind-protect` 来建立 *清理表达式 cleanup expressions*  ，以便在发生错误时进行计算。(参见清理非本地出口。 Cleaning Up From Nonlocal Exits)有时，您可能希望程序在子例程出错的情况下继续执行。在这些情况下，您将使用条件-情况来建立错误处理程序，以便在发生错误时恢复控制。

要报告问题而不终止当前命令的执行，可以考虑发出警告。请参阅报告警告 Reporting Warnings。

抵制使用错误处理将控制从程序的一部分转移到另一部分的诱惑;用接球和扔球来代替。参见显式非局部退出:catch和throw Explicit Nonlocal Exits: catch and throw。

* How to Signal an Error
* How Emacs Processes Errors
* Writing Code to Handle Errors
* Error Symbols and Condition Names

##### 11.7.3.1 How to Signal an Error #####

发出错误信号意味着开始错误处理。错误处理通常会终止正在运行的程序的全部或部分，并返回到为处理错误而设置的点(请参阅Emacs如何处理错误 How Emacs Processes Errors)。这里我们描述如何发出错误信号。

大多数错误在Lisp原语中自动发出信号，您可以出于其他目的调用这些原语，例如，如果您试图取整数的CAR或在缓冲区末尾移动一个字符。还可以使用函数error和signal显式地表示错误。

当用户键入C-g时发生的退出不被视为错误，但它的处理方式几乎与错误一样。看到Quitting。

每个错误都以这样或那样的方式指定一个错误消息。消息应该说明哪里出了问题(“文件不存在”)，而不是事情应该如何(“文件必须存在”)。Emacs Lisp中的约定是错误消息应该以大写字母开头，但不应该以任何标点符号结束。

#### 函数: `error format-string &rest args` ####

此函数通过将 `format-message` (请参阅格式化字符串 Formatting Strings)应用于 *format-string* 和 *args* 来构造错误消息，以发出错误信号。

下面的例子展示了error的典型用法:

``` Elisp
(error "That is an error -- try something else")
;  error→ That is an error -- try something else

(error "Invalid name `%s'" "A%%B")
; error→ Invalid name ‘A%%B’
```

Error通过带两个参数调用signal来工作:错误符号Error和包含format-message返回的字符串的列表。

通常，格式中的重音和撇号会转换为匹配的弯曲引号，例如，`` "Missing `%s'" might result in "Missing ‘foo’"  ``。请参阅文本引用风格 Text Quoting Style，了解如何影响或抑制这种翻译。

> 警告:如果你想使用你自己的字符串作为错误消息，不要只写(error string)。如果字符串字符串包含 % 或 反引号，它可能会被重新格式化，产生不希望的结果。相反，使用(error "%s" string)。
> 当noninteractive为非nil时(请参阅批处理模式 Batch Mode)，如果所发出的错误没有处理程序，则此函数会杀死Emacs。

#### 函数: `signal error-symbol data` ####

这个函数用 `error-symbol` 表示错误。参数data是与错误情况相关的附加Lisp对象的列表。

参数 `error-symbol` 必须是一个错误符号——一个用 `define-error` 定义的符号。这就是Emacs Lisp如何对不同类型的错误进行分类的。有关错误符号、错误条件和条件名称的描述，请参阅错误符号和条件名称 Error Symbols and Condition Names。

如果没有处理错误，则在打印错误消息时使用这两个参数。通常，该错误消息由 `error-symbol` 的 `error-message` 属性提供。如果data为非nil，则后面跟着冒号和逗号分隔的data未求值元素列表。对于错误，错误消息是数据的CAR(必须是字符串)。对文件错误 file-error 的子类 subcategories 别进行特殊处理。

data 中对象的数量和重要性取决于错误符号 error-symbol。例如，如果类型参数错误 wrong-type-argument error，则列表中应该有两个对象:描述预期类型的谓词和不适合该类型的对象。

error-symbol和data对于任何处理错误的错误处理程序都是可用的: `condition-case` 将一个局部变量绑定到一个 `(error-symbol . data)` 形式的列表。(参见编写代码处理错误 Writing Code to Handle Errors)。

函数 signal 永远不会返回。如果error error-symbol 没有处理程序，并且 `noninteractive` 为非nil(请参阅批处理模式)，则此函数最终会终止Emacs。

``` Elisp
(signal 'wrong-number-of-arguments '(x y))
;     error→ Wrong number of arguments: x, y

(signal 'no-such-error '("My unknown error condition"))
;     error→ peculiar error: "My unknown error condition"
```

#### 函数: `user-error format-string &rest args` ####

这个函数的行为完全类似于error，除了它使用错误符号 `user-error` 而不是error。顾名思义，这是为了报告用户的错误，而不是代码本身的错误。例如，如果您尝试使用命令 `Info-history-back` (l)移回信息浏览历史记录的起始位置，Emacs将发出用户错误信号。这样的错误不会导致进入调试器，即使debug-on-error为非nil时也是如此。请参阅在出现错误时进入调试器 Entering the Debugger on an Error。

> Common Lisp注意事项:Emacs Lisp不像公共Lisp那样有可持续性错误的概念。

##### 11.7.3.2 How Emacs Processes Errors #####

当发出错误信号时，signal 将为该错误搜索活动处理程序 active handler。处理程序是一个Lisp表达式序列，指定在Lisp程序的某个部分发生错误时执行。如果错误具有适用的处理程序，则执行该处理程序，并在处理程序之后恢复控制。处理程序在建立它的条件用例 condition-case 的环境中执行;在这种情况下调用的所有函数都已经退出，处理程序不能返回到它们。

如果没有适用于错误的处理程序，则终止当前命令并将控制返回给编辑器命令循环 command loop。(命令循环有一个隐式处理各种错误的处理程序。)命令循环的处理程序使用错误符号和相关数据来打印错误消息。你可以使用变量 `command-error-function` 来控制这是如何完成的:

###### 变量: `command-error-function` ######

如果该变量为非nil，则指定一个函数用于处理将控制权返回给Emacs命令循环的错误。该函数应该有三个参数:

1. `data`: 一个与condition-case绑定到其变量的形式相同的列表;
2. `context`: 一个描述错误发生情况的字符串，或者(更常见的)nil;
3. `caller`: 调用发出错误信号的原语的Lisp函数。

没有显式处理程序的错误可以调用Lisp调试器(参见调用调试器 Invoking the Debugger)。如果变量 `debug-on-error`(请参阅在错误时进入调试器 Entering the Debugger on an Error)为非空，则启用调试器。与错误处理程序不同，调试器在发生错误的环境中运行，因此您可以精确地检查变量的值，就像它们在发生错误时的值一样。在批处理模式下(参见批处理模式 Batch Mode)，Emacs进程通常以非零退出状态退出。

##### 11.7.3.3 Writing Code to Handle Errors #####

发出错误信号的通常效果是终止正在运行的命令，并立即返回到Emacs编辑器命令循环。您可以通过建立一个带有特殊形式condition-case的错误处理程序来安排捕获在程序的某个部分中发生的错误。一个简单的例子如下:

``` Elisp
(condition-case nil
    (delete-file filename)
  (error nil))
```

这将删除名为filename的文件，捕获任何错误，并在发生错误时返回nil。(对于这种简单的情况，您可以使用宏`ignore-errors`;见下文)。

`condition-case` 构造通常用于捕获可预测的错误，例如在调用 `insert-file-contents` 时未能打开文件。它还用于捕获完全不可预测的错误，例如当程序计算从用户读取的表达式时。

`condition-case` 的第二个参数称为受保护形式 `protected form` 。(在上面的例子中，受保护的表单是对 `delete-file`的调用。)错误处理程序在此表单开始执行时生效，并在此表单返回时停用。它们在这期间一直有效。特别是，它们在此表单调用的函数执行期间、在其子例程中等等都有效。这是一件好事，因为严格来说，错误只能由受保护的形式调用的Lisp原语(包括signal和error)来表示，而不能由受保护的形式本身来表示。

受保护表单后面的参数是处理程序。每个处理程序列出一个或多个条件名称(它们是符号)，以指定它将处理哪些错误。在发出错误信号时指定的错误符号还定义了一个条件名称列表。如果处理程序有任何共同的条件名称，则处理程序应用于错误。在上面的示例中，有一个处理程序，它指定了一个条件名称error，它涵盖了所有错误。

搜索适用的处理程序将检查所有已建立的处理程序，从最近建立的处理程序开始。因此，如果两个嵌套的条件用例形式提供处理相同的错误，则两个条件用例的内部形式将处理该错误。

如果错误由某种条件情况形式处理，这通常会阻止调试器运行，即使 `debug-on-error` 表示该错误应该调用调试器。

如果希望能够调试由条件情况捕获的错误，请将变量 `debug-on-signal` 设置为非nil值。你也可以指定一个特定的处理程序应该让调试器先运行，通过在条件中写入debug，像这样:

``` Elisp
(condition-case nil
    (delete-file filename)
  ((debug error) nil))
```

这里调试的作用只是防止条件情况抑制对调试器的调用。任何给定的错误只有在错误调试和其他常用过滤机制认为应该这样做的情况下才会调用调试器。请参阅在出现错误时进入调试器 Entering the Debugger on an Error。

###### 宏: `condition-case-unless-debug var protected-form handlers...` ######

宏 `condition-case-unless-debug` 提供了另一种处理此类表单调试的方法。它的行为完全类似于condition-case，除非变量debug-on-error是非nil，在这种情况下，它根本不处理任何错误。

一旦Emacs决定某个处理程序处理错误，它将控制权返回给该处理程序。要做到这一点，Emacs取消所有由退出的绑定构造所做的变量绑定，并执行所有退出的 `unwind-protect` 表单的清理。一旦控制到达处理程序，处理程序的主体就会正常执行。

执行处理程序体之后，执行从条件情况形式返回。由于受保护的表单在处理程序执行之前完全退出，处理程序不能在出现错误时恢复执行，也不能检查在受保护表单中进行的变量绑定。它所能做的就是清理和继续。

错误信号和处理与throw和catch有一些相似之处(参见显式非本地出口:catch和throw)，但它们是完全独立的设施。catch不能捕获错误，而throw不能由错误处理程序处理(尽管在没有合适的catch时使用throw表示可以处理错误)。

###### 特殊形式: `condition-case var protected-form handlers...` ######

这个特殊形式围绕 protected-form 的执行建立了错误处理程序。如果 protected-form 执行没有错误，它返回的值将成为条件情况表单的值(在没有成功处理程序的情况下;见下文)。在这种情况下，条件情况没有影响。当在 protected-form 期间发生错误时，condition-case 形式会产生影响。

每个处理程序都是形式 `(conditions body...)` 的列表。这里的conditions是要处理的错误条件名称，或条件名称列表(其中可以包括debug，以允许调试器在处理程序之前运行)。条件名称 `t` 匹配任何条件。body是这个处理程序处理错误时要执行的一个或多个Lisp表达式。以下是处理程序的示例:

``` Elisp
(error nil)

(arith-error (message "Division by zero"))

((arith-error file-error)
 (message
  "Either division by zero or failuer to open a file"))
```

发生的每个错误都有一个错误符号，它描述了错误的类型，并且还描述了一个条件名称列表(参见错误符号和条件名称 Error Symbols and Condition Names)。Emacs搜索所有活动的 condition-case forms，寻找指定一个或多个条件名称的处理程序;最内层的匹配条件用例处理错误。在这种情况下，第一个适用的处理程序处理错误。

在执行处理程序体之后，条件情况正常返回，使用处理程序体中最后一个表单的值作为总体值。

参数var是一个变量。在执行 protected-form 时，condition-case 仅在处理错误时才绑定此变量。此时，它将var本地绑定到一个 error description，这是一个给出错误细节的列表。错误描述的形式为 `(error-symbol . data)`。处理程序可以参考这个列表来决定要做什么。例如，如果错误是由于打开文件失败，则文件名是数据的第二个元素——错误描述的第三个元素。

如果var为nil，则意味着没有变量被绑定。然后错误符号和相关数据对处理程序不可用。

作为一种特殊情况，其中一个处理程序可以是格式为 `(:success body...)` 的列表，其中body在执行时将 var (如果非nil)绑定到protected-form的返回值，当表达式无错误终止时。

有时需要重新抛出一个被condition-case捕获的信号，以供某些外部处理程序捕获。以下是如何做到这一点:

``` Elisp
(signal (car err) (cdr err))
```

其中err是错误描述变量，是要重新抛出其错误条件的condition-case的第一个参数。参见信号的定义 Definition of signal。

###### 函数: `error-message-string error-descriptior` ######

这个函数返回给定错误描述符的错误消息字符串。如果您希望通过打印该错误的常见错误消息来处理错误，则此功能非常有用。参见信号的定义 Definition of signal。

下面是一个使用条件用例处理除零所导致的错误的示例。处理程序显示错误消息(但没有哔哔声)，然后返回一个非常大的数字。

``` Elisp
(defun safe-divide (dividend divisor)
  (condition-case err
      ;; Protected form.
	  (/ dividend divisor)

    ;; The handler.
	(arith-error                             ; Condition
	 ;; Display the usual message for this error.
	 (message "%s" (error-message-string err))
	 1000000)))
; safe-divide

(safe-divide 5 0)
;  1000000
```

处理程序指定条件名称 arith-error，以便它只处理除零错误。其他类型的错误将不被处理(在这种情况下)。因此:

``` Elisp
(safe-divide nil 3)
;     error→ Wrong type argument: number-or-marker-p, nil
```

下面是一个捕获所有类型错误的条件用例，包括来自error的错误:

``` Elisp
(setq baz 34)
     ⇒ 34

(condition-case err
    (if (eq baz 35)
        t
      ;; This is a call to the function error.
      (error "Rats!  The variable %s was %s, not 35" 'baz baz))
  ;; This is the handler; it is not a form.
  (error (princ (format "The error was: %s" err))
         2))
-| The error was: (error "Rats!  The variable baz was 34, not 35")
⇒ 2
```

###### 宏: `ignore-errors boody...` ######

此构造执行body，忽略在执行过程中发生的任何错误。如果执行没有错误，则ignore-errors返回body中最后一个表单的值;否则，它返回nil。

下面是本小节开头使用ignore-errors重写的示例:

``` Elisp
(ignore-errors
  (delete-file filename))
```

###### 宏: `ignore-error condition body...` ######

这个宏类似于ignore-errors，但只会忽略指定的特定错误条件。

``` Elisp
(ignore-error end-of-file
  (read ""))
;  nil
```

Condition也可以是错误条件的列表。

###### 宏: `with-demoted-errors format body...` ######

这个宏就像一个温和版本的忽略错误。它不是完全压制错误，而是将它们转换为消息。它使用字符串格式来格式化消息。格式应该包含一个 % 序列;例如，`"Error: %S"`。使用带有降级错误的代码，这些代码预计不会发出错误信号，但如果确实发生错误，应该是健壮的。注意，这个宏使用 `condition-case-unless-debug` 而不是 `condition-case`。

##### 11.7.3.4 Error Symbols and Condition Names #####

当您发出错误信号时，您可以指定一个错误符号来指定您所想到的错误类型。每个错误都有且只有一个错误符号来对其进行分类。这是Emacs Lisp语言定义的最好的错误分类。

这些狭窄的分类被分组到称为错误条件的更广泛类的层次结构中，由条件名称标识。最狭窄的类属于错误符号本身:每个错误符号也是一个条件名称。对于更广泛的类也有条件名，直到条件名error，它接受所有类型的错误(但不退出)。因此，每个错误都有一个或多个条件名称:error，错误符号(如果与error不同)，可能还有一些中间分类。

###### 函数: `define-error name message &optional parent` ######

为了使一个符号成为错误符号，它必须用带有父条件(默认为error)的define-error来定义。这个父类定义了这种错误所属的条件。父类的传递集总是包含错误符号本身和符号error。因为退出不被认为是错误，所以quit的父类集合就是(quit)。

除了它的父类之外，错误符号还有一个消息，该消息是一个字符串，当错误发出信号但不处理时打印出来。如果该消息无效，则使用错误消息 *奇特错误 peculiar error*。参见信号的定义 Definition of signal。

在内部，父元素集存储在错误符号的 `error-conditions` 属性中，消息存储在错误符号的error-message属性中。

下面是我们如何定义一个新的错误符号new-error:

``` Elisp
(define-error 'new-error "A new error" 'my-own-errors)
;  "A new error"
```

此错误有几个条件名称:

* `new-error`，最窄的分类;
* `my-own-errors`，我们认为这是一个更广泛的分类;
* 以及我自己的所有条件——错误应该包括 error，这是所有错误中最广泛的。

错误字符串应该以大写字母开头，但不应该以句号结束。这是为了与Emacs的其余部分保持一致。

当然，Emacs不会自己发出新错误信号;只有在代码中显式调用signal(参见signal的定义)才能做到这一点:

``` Elisp
(signal 'new-error '(x y))
;     error→ A new error: x, y
```

这个错误可以通过它的任何条件名来处理。这个例子处理new-error和my-own-errors类中的任何其他错误:

``` Elisp
(condition-case foo
    (bar nil t)
  (my-own-errors nil))
```

对错误进行分类的重要方法是根据它们的条件名称进行分类——这些名称用于将错误与处理程序进行匹配。错误符号仅作为一种方便的方式来指定预期的错误消息和条件名称列表。给signal一个条件名称列表而不是一个错误符号会很麻烦。

相比之下，只使用错误符号而不使用条件名称将严重降低条件大小写的功能。在编写错误处理程序时，条件名可以根据不同的通用性级别对错误进行分类。仅使用错误符号就可以排除除最窄级别以外的所有分类。

有关主要错误符号及其条件的列表，请参阅标准错误 Standard Errors。

#### 11.7.4 Cleaning Up from Nonlocal Exits ####

当您临时将数据结构置于不一致状态时，`unwind-protect` 结构是必需的;它允许您在发生错误或抛出时使数据再次保持一致。(另一个更具体的清理构造是原子更改组，它只用于缓冲区内容的更改;原子变化组 Atomic Change Groups。)

##### 特殊形式: `unwind-protect body-form cleanup-forms...` #####

`unwind-protect` 执行 `body-form` ，并保证如果控制离开body-form，无论如何发生，都将对 `cleanup-form` 进行运算。body-form 可以正常完成，也可以在 unwind-protect 外执行 throw 动作，也可以出现 error;在所有情况下，cleanup-forms 都将被运算。

如果body-form正常完成，则unwind-protect在计算清理表单后返回body-form的值。如果形体未完成，unwind-protect不返回任何正常意义上的值。

只有形体受到unwind-protect的保护。如果任何 cleanup-forms 本身非本地退出(通过抛出或错误)，则不保证unwind-protect会评估其余的清理表单。如果某个清理表单的失败有可能导致问题，那么使用围绕该表单的另一个unwind保护来保护它。

例如，这里我们创建了一个临时使用的不可见缓冲区，并确保在完成之前杀死它:

``` Elisp
(let ((buffer (get-buffer-create " *temp*")))
  (with-current-buffer buffer
    (unwind-protect
	    body-form
	  (kill-buffer buffer))))
```

您可能会认为我们可以直接写入(kill-buffer (current-buffer))并省略变量buffer。但是，如果body-form碰巧在切换到不同的缓冲区后出现错误，那么上面显示的方法更安全!(或者，您可以在body-form周围编写save-current-buffer，以确保临时缓冲区能够及时恢复为当前以终止它。)

Emacs包含一个名为 `with-temp-buffer` 的标准宏，它可以扩展成以上所示的代码(参见Current Buffer)。本手册中定义的几个宏以这种方式使用 `unwind-protect`。

下面是一个来自FTP包的实际示例。它创建一个进程(请参阅进程 Processes)来尝试建立到远程计算机的连接。由于 `ftp-login` 函数极易受到函数编写者无法预料的许多问题的影响，因此使用了一种表单来保护它，以保证在发生故障时删除该进程。否则，Emacs可能会被无用的子进程填满。

``` Elisp
(let ((win nil))
  (unwind-protect
      (progn
	    (setq process (ftp-setup-buffer host file))
		(if (setq win (ftp-login process host user password))
		    (message "Logged in")
		  (error "Ftp login failed")))
    (or win (and process (delete-process)))))
```

这个例子有一个小错误:如果用户输入C-g来退出，并且退出发生在ftp-setup-buffer函数返回之后，但在设置变量process之前，进程不会被杀死。没有简单的方法来修复这个bug，但至少不太可能。

## 12 Variables 变量 ##

变量是程序中用来表示值的名称。在Lisp中，每个变量由一个Lisp符号表示(参见符号)。变量名就是符号的名称，变量的值存储在符号的 value cell 中。参见符号组件 Symbol Components。在Emacs Lisp中，符号作为变量的使用与作为函数名的使用是独立的。

如本手册前面所述，Lisp程序主要由Lisp对象表示，其次才是文本。Lisp程序的文本形式是由构成程序的Lisp对象的读语法给出的。因此，Lisp程序中变量的文本形式是使用表示该变量的符号的read语法编写的。


* Global Variables
* Variables that Never Change
* Local Variables
* When a Variable is Void
* Defining Global Variables
* Tips for Defining Variables Robustly
* Accessing Variable Values
* Setting Variable Values
* Running a function when a variable is changed.
* Scoping Rules for Variable Bindings
* Buffer-Local Variables
* File Local Variables
* Directory Local Variables
* Connection Local Variables
* Variable Aliases
* Variables with Restricted Values
* Generalized Variables
* Multisession Variables

### 12.1 Global Variables ###

最简单的使用变量的方法是全局的。这意味着变量一次只有一个值，并且这个值在整个Lisp系统中有效(至少暂时有效)。该值在指定新值之前一直有效。当一个新值替换旧值时，旧值的痕迹不会保留在变量中。

使用setq为符号指定一个值。例如,

``` Elisp
(setq x '(a b))
```

给变量x赋值(a b)。注意setq是一个特殊形式(参见特殊形式);它不计算它的第一个参数，即变量名，但它计算第二个参数，即新值。

一旦变量有了值，您就可以使用符号本身作为表达式来引用它。因此,

``` Elisp
x ⇒ (a b)
```

假设上面显示的setq表单已经执行。

如果再次设置相同的变量，新值将替换旧值:

``` Elisp
x
     ⇒ (a b)

(setq x 4)
     ⇒ 4

x
     ⇒ 4
```

### 12.2 Variables that Never Change ###

在Emacs Lisp中，某些符号通常对自己求值。这些包括nil和t，以及任何名称以':'开头的符号(这些被称为关键字keywords)。这些符号不能重新绑定，也不能改变它们的值。任何设置或绑定nil或t的尝试都表示设置常量错误。对于关键字(名称以':'开头的符号)来说也是如此，如果它被嵌入到标准数组中，除了将这样的符号设置为自身不是错误之外。

``` Elisp
nil ≡ 'nil
     ⇒ nil

(setq nil 500)
error→ Attempt to set constant symbol: nil
```

#### 函数: `keywordp object` ####

如果对象是一个名称以':'开头的符号，则函数返回t，并将其存储在标准数组中，否则返回nil。

这些常量从根本上不同于使用 `defconst` 特殊形式定义的常量(参见定义全局变量 Defining Global Variables)。`defconst` 形式用于通知读者您不打算更改变量的值，但是如果您确实更改了它，Emacs不会引发错误。

由于各种实际原因，少量附加符号被设置为只读。这些选项包括 `enable-multibyte-characters`、`most-positive-fixnum`、`most-negative-fixnum` 以及其他一些选项。任何设置或绑定这些的尝试也会发出设置常数错误的信号。

### 12.3 Local Variables ###

全局变量的值一直持续到被新值显式取代。有时给变量一个局部值是很有用的——这个值只在Lisp程序的某个特定部分起作用。当一个变量有一个局部值时，我们说它是局部绑定到那个值的，并且它是一个局部变量。

例如，当调用函数时，其参数变量接收局部值，这些值是提供给函数调用的实际参数;这些局部绑定在函数体中生效。再举一个例子，let特殊形式显式地为特定变量建立局部绑定，这些绑定仅在let形式的主体内生效。

我们还谈到全局绑定，这是(概念上 conceptually)保存全局值的地方。

建立一个局部绑定可以省去变量之前的值(或者没有)。我们说前面的值被遮蔽了(shadowed)。全局值和局部值都可能被遮蔽。如果本地绑定有效，则在本地变量上使用setq将指定的值存储在本地绑定中。当该局部绑定不再有效时，先前隐藏的值(或缺少值)就会返回。

一个变量可以同时有多个本地绑定(例如，如果有嵌套的let形式绑定变量)。当前绑定是实际生效的本地绑定。它通过计算变量符号来确定返回的值，并且它是setq所作用的绑定。

对于大多数目的，您可以将当前绑定视为最内层的本地绑定，如果没有本地绑定，则可以将其视为全局绑定。更准确地说，称为 *作用域规则 scoping rule* 的规则决定了局部绑定在程序中的哪个位置生效。Emacs Lisp中的默认作用域规则称为 *动态作用域 dynamic scoping*，它简单地指出，在程序执行的任何给定点上的当前绑定都是为仍然存在的变量最近创建的绑定。有关动态作用域和另一种称为 *词法作用域 lexical scoping* 的作用域规则的详细信息，请参见 变量绑定的作用域规则 Scoping Rules for Variable Bindings。最近，Emacs正朝着在越来越多的地方使用词法绑定的方向发展，其目标是最终使词法绑定成为默认值。特别是，所有Emacs Lisp源文件和 `*scratch*` 缓冲区都使用词法作用域。

用于创建局部绑定的特殊形式 `let` 和 `let*` 存在:

#### 特殊形式: `let (bindings...) forms...` ####

这个特殊形式为特定的一组变量设置本地绑定(由绑定指定)，然后按文本顺序计算所有表单。它的返回值是forms中最后一个表单的值。let设置的本地绑定将仅在表单主体内生效。

每个绑定都是

1. 一个符号 a symbol，在这种情况下，该符号局部绑定为nil;
2. 形式的列表 (symbol value-form)，在这种情况下，symbol局部绑定到 `value-form` 的求值结果。如果省略value-form，则使用nil。

绑定中的所有值形式在将任何符号绑定到它们之前，都按照它们出现的顺序求值。这里有一个例子: z被绑定到y的旧值，即2，而不是y的新值，即1。

``` Elisp
(setq y 2)
;  2

(let ((y 1)
      (z y))
  (list y z))
;  (1 2)
```

另一方面，绑定的顺序是未指定的:在下面的示例中，可能打印1或2。

``` Elisp
(let ((x 1)
      (x 2))
  (print x))
```

因此，避免在一个let形式中多次绑定一个变量。

#### 特殊形式: `let* (bindings...) forms...` ####

这种特殊的形式类似于 `let`，但是它在计算每个变量的局部值之后，在计算下一个变量的局部值之前绑定每个变量。因此，绑定中的表达式可以引用前面以 `let*` 形式绑定的符号。将下面的示例与上面的let示例进行比较。

``` Elisp
(setq y 2)
     ⇒ 2

(let* ((y 1)
       (z y))    ; Use the just-established value of y.
  (list y z))
     ⇒ (1 1)
```

基本上，上例中x和y的 `let*` 绑定等同于使用嵌套let绑定:

``` Elisp
(let ((y 1))
  (let ((z y))
    (list y z)))
```

#### 特殊形式: `letrec (bindings...) forms...` ####

这种特殊的形式类似于 `let*`，但是所有的变量都是在计算任何局部值之前绑定的。然后将这些值赋给本地绑定变量。这只在 词法绑定 lexical binding 生效时有用，并且您希望创建的 闭包(closures) 引用使用 `let*` 时尚未生效的绑定。

例如，这里有一个闭包，它在运行一次后将自己从钩子中移除:

``` Elisp
(letrec ((hookfun (lambda ()
                    (message "Run once")
                    (remove-hook 'post-command-hook hookfun))))
  (add-hook 'post-command-hook hookfun))
```

#### 特殊形式: `dlet (bindings...) forms...` ####

这种特殊的形式类似于let，但它动态地绑定了所有变量。这很少有用—您通常希望动态地绑定普通变量和特殊变量(即使用defvar定义的变量)，这就是let所做的。

当与假设某些变量是动态绑定的旧代码接口时，dlet可能很有用(请参阅动态绑定 Dynamic binding)，但是defvar这些变量是不切实际的。dlet将暂时使绑定的变量特殊，执行表单，然后再次使变量非特殊。

#### 特殊形式: `named-let name bindings &rest body` ####

这种特殊形式是受 Scheme 语言启发的循环结构。它类似于 let:它在绑定中绑定变量，然后求值。然而，`named-let` 也将name绑定到一个局部函数，其形式参数是绑定中的变量，其主体是body。这允许body通过调用name递归地调用自身，其中传递给name的参数用作递归调用中绑定变量的新值。

对数字列表求和的循环示例:

``` Elisp
(named-let sum ((numbers '(1 2 3 4))
                (running-sum 0))
  (if numbers
      (sum (cdr numbers) (+ running-sum (car numbers)))
    running-sum))
;  10
```

对位于body尾部位置的name的递归调用保证被优化为尾部调用，这意味着无论递归运行多深，它们都不会消耗任何额外的堆栈空间。这样的递归调用将有效地跳到循环的顶部，并为变量赋新值。

如果函数调用是最后完成的事情，那么调用返回的值就是body本身的值，就像上面对sum的递归调用一样。

> 警告: `name-let` 仅在启用词法绑定时才按预期工作。参见词法绑定 Lexical binding。

下面是创建本地绑定的其他工具的完整列表:

* 函数调用(参见函数 Functions)。
* 宏调用(参见宏 Macros)。
* 条件情况(参见错误 Errors)。

变量也可以有 `buffer-local` 绑定(参见 Buffer-local Variables);

一些变量具有终端本地绑定(参见多终端 Multiple Terminals)。这些类型的绑定的工作方式有点像普通的本地绑定，但是它们是本地化的，这取决于您在Emacs中的位置。

### 12.4 When a Variable is Void ###

如果一个变量的符号有一个未赋值单元格(参见符号组件 Symbol Components)，则该变量为void。

在Emacs Lisp的默认动态作用域规则(参见变量绑定的作用域规则 Scoping Rules for Variable Bindings)下，值单元存储变量的当前(局部或全局)值。请注意，未分配的值单元格与值单元格中有nil是不一样的。符号nil是一个Lisp对象，可以是变量的值，就像任何其他对象一样;但它仍然是一个值。如果一个变量是空的，尝试计算该变量会发出空变量错误的信号，而不是返回一个值。

在可选的词法作用域规则下，值单元格只保存变量的全局值——任何词法绑定构造之外的值。当变量被词法绑定时，局部值由词法环境决定;因此，即使变量符号的值单元格未赋值，变量也可以具有局部值。

#### 函数: `makunbound symbol` ####

这个函数清空符号的值单元格，使变量为空。它返回符号。

如果symbol具有动态局部绑定，makunbound将使当前绑定失效，并且这种失效仅在局部绑定生效时持续。之后，先前隐藏的局部或全局绑定被重新暴露;然后变量将不再为空，除非重新暴露的绑定也是空的。

下面是一些例子(假设动态绑定生效):

``` Elisp
(setq x 1)               ; Put a value in the global binding.
     ⇒ 1
(let ((x 2))             ; Locally bind it.
  (makunbound 'x)        ; Void the local binding.
  x)
error→ Symbol's value as variable is void: x

x                        ; The global binding is unchanged.
     ⇒ 1

(let ((x 2))             ; Locally bind it.
  (let ((x 3))           ; And again.
    (makunbound 'x)      ; Void the innermost-local binding.
    x))                  ; And refer: it’s void.
error→ Symbol's value as variable is void: x

(let ((x 2))
  (let ((x 3))
    (makunbound 'x))     ; Void inner binding, then remove it.
  x)                     ; Now outer let binding is visible.
     ⇒ 2
```

#### 函数: `boundp variable` ####

如果变量(符号)不为空，则返回t，如果为空则返回nil。

下面是一些例子(假设动态绑定生效):

``` Elisp
(boundp 'abracadabra)          ; Starts out void.
     ⇒ nil

(let ((abracadabra 5))         ; Locally bind it.
  (boundp 'abracadabra))
     ⇒ t

(boundp 'abracadabra)          ; Still globally void.
     ⇒ nil

(setq abracadabra 5)           ; Make it globally nonvoid.
     ⇒ 5

(boundp 'abracadabra)
     ⇒ t
```

### 12.5 Defining Global Variables ###

变量定义 variable definition 是一种结构，它宣布您打算将符号用作全局变量。它使用特殊的形式defvar或defconst，它们将在下面进行说明。

变量定义有三个目的。

1. 它告知阅读代码的人该符号打算以某种方式使用(作为变量)。
2. 它通知Lisp系统这一点，可选地提供一个初始值和一个文档字符串。
3. 它向编程工具(如etags)提供信息，使它们能够找到定义变量的位置。

defconst和defvar之间的区别主要是意图的问题，用于告知人类读者是否应该更改值。Emacs Lisp实际上并不阻止您更改用defconst定义的变量的值。这两种形式之间的一个显著区别是，defconst无条件地初始化变量，而defvar只有在它最初为空时才初始化它。

要定义一个可定制的变量，应该使用defcustom(它将defvar作为子例程调用)。参见定义自定义变量 Defining Customization Variables。

#### 特殊形式: `defvar symbol [value [doc-string]]` ####

这个特殊的形式将symbol定义为一个变量。注意符号不会被求值;要定义的符号应该显式地出现在defvar形式中。该变量被标记为特殊，这意味着它应该始终是动态绑定的(参见变量绑定的作用域规则 Scoping Rules for Variable Bindings)。

* 如果指定了value，并且symbol为void(即没有动态绑定值);(参见当变量为Void时 When a Variable is Void)，则计算value并将symbol设置为结果。
* 但是如果symbol不是void，则不计算value，并且symbol的值保持不变。
* 如果value被省略，symbol的值在任何情况下都不会被改变。

注意，指定一个值，即使是nil，也会将该变量永久标记为特殊值。然而，如果省略value，则变量仅在局部被标记为特殊(即在当前词法作用域中，或在顶层文件 top-level file)。这对于抑制字节编译警告非常有用，请参阅编译器错误 Compiler Errors。

如果symbol在当前缓冲区中具有buffer-local绑定，则defvar将作用于与缓冲区无关的默认值，而不是buffer-local绑定。如果默认值为空，则设置默认值。参见缓冲区局部变量 Buffer-Local Variables。

如果symbol已经被let绑定(例如，如果defvar形式出现在let形式中)，则defvar设置顶层默认值，如 `set-default-top-value`。let绑定在其绑定构造退出之前一直有效。参见变量绑定的作用域规则 Scoping Rules for Variable Bindings。

当您在Emacs Lisp模式下使用 `C-M-x (eval-defun)` 或 `C-x C-e (eval-last-sexp)` 计算顶级defvar form时，这两个命令的一个特殊特性安排无条件地设置变量，而不测试其值是否为空。

如果提供了 `doc-string` 参数，它将为变量指定文档字符串(存储在符号的 `variable-documentation` 属性中)。见文档 Documentation。

这里有一些例子。这个表单定义了foo，但没有初始化它:

``` Elisp
(defvar foo)
;  foo
```

下面的例子将bar的值初始化为23，并给它一个文档字符串:

``` Elisp
(defvar bar 23
  "The normal weight of a bar.")
;  bar
```

defvar返回符号，但它通常用于文件的顶层，在那里它的值无关紧要。

有关使用不带值的defvar的更详细示例，请参见Local defvar示例 Local defvar example。

#### 特殊形式: `defconst symbol value [doc-string]` ####

这个特殊的形式将symbol定义为一个值并对其进行初始化。它告诉阅读你代码的人，符号有一个标准的全局值，在这里建立，不应该被用户或其他程序改变。注意符号不会被求值;要定义的符号必须显式地出现在defconst中。

像defvar一样，defconst形式将变量标记为特殊，这意味着它应该始终是动态绑定的(参见变量绑定的作用域规则 Scoping Rules for Variable Bindings)。此外，它还将变量标记为有风险的(参见文件局部变量 File Local Variables)。

defconst总是计算value，并将symbol的值设置为结果。如果symbol在当前缓冲区中有buffer-local绑定，则defconst将设置默认值，而不是buffer-local值。(但是您不应该为使用defconst定义的符号制作缓冲区局部绑定。)

使用defconst的一个例子是Emacs对float-pi(数学常数pi)的定义，任何人都不应该改变它(尽管印第安纳州立法机构试图改变它)。然而，正如第二种形式所示，defconst只是建议的。

``` Elisp
(defconst float-pi 3.141592653589793 "The value of Pi.")
     ⇒ float-pi

(setq float-pi 3)
     ⇒ float-pi

float-pi
     ⇒ 3
```

> 警告:如果您使用defconst或defvar特殊形式，而变量具有局部绑定(使用let或函数参数)，则它将设置局部绑定而不是全局绑定。这不是你通常想要的。为了防止这种情况发生，在文件的顶层使用这些特殊的表单，在这里通常没有有效的局部绑定，并确保在为变量进行局部绑定之前加载文件。

### 12.6 Tips for Defining Variables Robustly ###

当定义值为函数或函数列表的变量时，请分别使用以 `-function` 或 `-functions` 结尾的名称。

还有其他几个变量名约定;以下是完整的清单:

* `-hook`: 变量是一个普通的 hook(见 Hooks)
* `-function`: 值是一个函数
* `-functions`: 值是一个函数列表
* `-form`: 值是一个形式(一个表达式)
* `-forms`: 值是形式列表
* `-predicate`: 值是一个谓词, 一个有一个参数的函数，成功时返回非nil，失败时返回nil。
* `-flag`: 该值仅在是否为nil时才有意义。由于随着时间的推移，这些变量通常最终会获得更多的值，因此不强烈推荐使用这种约定。
* `-program`: 值是一个程序名
* `-command`: 值是一个完整的 shell 命令
* `-switches`: 该值指定命令的选项。
* `prefix--`: 该变量用于内部使用，并在文件 `prefix.el` 中定义。(2018年之前贡献的Emacs代码可能会遵循其他约定，这些约定正在逐步淘汰。)
* `-internal`: 该变量用于内部使用，并在C代码中定义。(2018年之前贡献的Emacs代码可能会遵循其他约定，这些约定正在逐步淘汰。)

当你定义一个变量时，总是要考虑是否应该把它标记为安全的还是有风险的;参见文件局部变量 File Local Variables。

当定义和初始化一个包含复杂值的变量(比如主模式的语法表)时，最好将值的整个计算放入defvar中，如下所示:

``` Elisp
(defvar my-major-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?# "<" table)
    …
    table)
  docstring)
```

这种方法有几个好处。

1. 首先，如果用户在加载文件时退出，则变量要么仍然未初始化，要么初始化正确，永远不会处于两者之间。如果它仍然未初始化，重新加载文件将正确初始化它。
2. 第二，一旦变量初始化，重新加载文件不会改变它;如果用户改变了它的值，这一点很重要。
3. 第三，用C-M-x计算defvar形式将完全重新初始化变量。

### 12.7 Accessing Variable Values ###

引用变量的通常方法是写出命名它的符号。参见符号形式。

有时，您可能希望引用仅在运行时确定的变量。在这种情况下，不能在程序文本中指定变量名。您可以使用 `symbol-value` 函数来提取值。

#### 函数: `symbol-value symbol` ####

这个函数返回存储在符号的值单元格中的值。这是存储变量当前(动态)值的地方。如果变量没有本地绑定，这就是它的全局值。如果变量为空，则会发出空变量错误信号 void-variable error。

如果变量是词法绑定的，symbol-value报告的值不一定与变量的词法值相同，后者由词法环境决定，而不是由符号的值单元决定。参见变量绑定的作用域规则 Scoping Rules for Variables Bindings。

``` Elisp

(setq abracadabra 5)
     ⇒ 5

(setq foo 9)
     ⇒ 9

;; Here the symbol abracadabra
;;   is the symbol whose value is examined.
(let ((abracadabra 'foo))
  (symbol-value 'abracadabra))
     ⇒ foo

;; Here, the value of abracadabra,
;;   which is foo,
;;   is the symbol whose value is examined.
(let ((abracadabra 'foo))
  (symbol-value abracadabra))
     ⇒ 9

(symbol-value 'abracadabra)
     ⇒ 5
```

### 12.8 Setting Variable Values ###

更改变量值的常用方法是使用特殊形式 `setq`。当您需要在运行时计算变量的选择时，请使用函数 `set`。

#### 特殊形式: `setq [symbol form]...` ####

这种特殊形式是更改变量值的最常用方法。每个符号被赋予一个新值，这是计算相应形式的结果。符号的当前绑定被更改。

Setq不求值;它设置了你要写的符号。我们说这个论点被自动引用了。setq中的“q”代表“引号”。

setq表单的值就是最后一个表单的值。

``` Elisp
(setq x (1+ 2))
     ⇒ 3

x                   ; x now has a global value.
     ⇒ 3

(let ((x 5))
  (setq x 6)        ; The local binding of x is set.
  x)
     ⇒ 6

x                   ; The global value is unchanged.
     ⇒ 3

```

注意，先计算第一种形式，然后设置第一个符号，然后计算第二种形式，然后设置第二个符号，以此类推:

``` Elisp
(setq x 10          ; Notice that x is set before
      y (1+ x))     ;   the value of y is computed.
     ⇒ 11
```

#### 函数: `set symbol value` ####

这个函数将值放入符号的值单元格中。由于它是一个函数而不是特殊形式，因此为symbol编写的表达式被求值以获得要设置的符号。返回值为value。

当动态变量绑定生效时(默认值)，set与setq具有相同的效果，除了set计算其符号参数而setq不计算之外。但是，当变量被词法绑定时，set影响它的动态值，而setq影响它的当前(词法)值。参见变量绑定的作用域规则。

``` Elisp
(set one 1)
error→ Symbol's value as variable is void: one

(set 'one 1)
     ⇒ 1

(set 'two 'one)
     ⇒ one

(set two 2)         ; two evaluates to symbol one.
     ⇒ 2

one                 ; So it is one that was set.
     ⇒ 2
(let ((one 1))      ; This binding of one is set,
  (set 'one 3)      ;   not the global value.
  one)
     ⇒ 3

one
     ⇒ 2
```

如果symbol实际上不是一个符号，则会发出错误的类型参数错误信号。

``` Elisp
(set '(x y) 'z)
error→ Wrong type argument: symbolp, (x y)
```

#### 宏: `setopt [symbol form]...` ####

这类似于setq(见上文)，但用于用户选项。这个宏使用自定义机制来设置变量(参见定义自定义变量 Defining Customization Variables)。特别是，`setopt`将运行与变量关联的setter函数。例如，如果你有:

``` Elisp
(defcustom my-var 1
  "My var."
  :type 'number
  :set (lambda (var val)
         (set-default var val)
		 (message "We set %s to %s" var val)))
```

然后下面的代码，除了将my-var设置为 2 之外，还会发出一条消息:

``` Elisp
(setopt my-var 2)
```

setopt还检查该值是否对用户选项有效。例如，使用setopt将用数字类型定义的用户选项设置为字符串将发出错误信号。

与defcustom和相关的定制命令(如customize-variable)不同，setopt用于非交互式使用，特别是在用户初始化文件中。因此，它不记录标准的、保存的和用户设置的值，也不将变量标记为要保存在自定义文件中的候选变量。

setopt宏可以用于常规的非用户选项变量，但是比setq效率低得多。这个宏的主要用例是在用户的init文件中设置用户选项。

### 12.9 Running a function when a variable is changed. ###

当变量改变其值时，有时采取一些操作是有用的。可变观察点 variable watchpoint 功能提供了这样做的方法。此特性的一些可能用途包括保持显示与变量设置同步，以及调用调试器来跟踪对变量的意外更改(请参阅在修改变量时进入调试器)。

以下函数可用于操作和查询变量的watch函数。

#### 函数: `add-variable-watcher symbol watch-function` ####

这个函数安排每当符号被修改时调用 `watch-function`。通过别名(参见变量别名 Variable Aliases)进行修改将具有相同的效果。

`watch-function` 将被调用，就在改变symbol的值之前，有4个参数:symbol, newval, operation, and where。

* `symbol` 是被改变的变量。
* `newval` 是它将被更改为的值。(旧的值可以在watch-function中作为symbol的值使用，因为它还没有被更改为newval。)
* `operation` 是一个表示更改类型的符号，它是:set、let、unlet、makunbound或defvaralias之一。
* `where` 如果buffer-local值要被更改，则 where 为缓冲区，否则为nil。

#### 函数: `remove-variable-watcher symbol watch-function` ####

这个函数从符号的观察者列表中移除watch-function。

#### 函数: `get-variable-watchers symbol` ####

这个函数返回符号的活动监视函数列表。

#### 12.9.1 Limitations 限制 ####

有几种方法可以在不触发观察点的情况下修改变量(或至少看起来被修改)。

由于观察点被附加到符号上，对变量中包含的对象的修改(例如，通过列表修改函数，参见修改现有列表结构 Modifying Existing List Structure)不会被该机制捕获。

此外，C代码可以直接修改变量的值，绕过观察点机制。

这个特性的一个小限制(同样是因为它针对的是符号)是，只能观察动态范围的变量。这几乎没有什么困难，因为对词法变量的修改可以通过检查变量作用域内的代码轻松发现(与动态变量不同，动态变量可以被任何代码修改，请参阅变量绑定的作用域规则)。

### 12.10 Scoping Rules for Variable Bindings ###

当为变量创建局部绑定时，该绑定仅在程序的有限部分内生效(请参阅局部变量)。本节将详细描述这意味着什么。

每个局部绑定都有一定的范围和程度。作用域指的是文本源代码中可以访问绑定的位置。范围是指在程序执行时绑定存在的时间。

默认情况下，Emacs创建的本地绑定是动态绑定。这样的绑定具有动态作用域，这意味着程序的任何部分都可能访问变量绑定。它还具有动态范围，这意味着绑定仅在绑定构造(例如let表单的主体)执行时才持续。

Emacs可以选择性地创建词法绑定。词法绑定具有词法作用域，这意味着对变量的任何引用都必须在文本上位于绑定构造中。它还具有不确定的范围，这意味着在某些情况下，通过称为 *闭包 closures* 的特殊对象，即使在绑定构造完成执行之后，绑定也可以继续存在。

多年来，动态绑定一直是(现在仍然是)Emacs的默认值，但最近Emacs在越来越多的地方转向使用词法绑定，其目标是最终使其成为默认值。

下面的小节更详细地描述了动态绑定和词法绑定，以及如何在Emacs Lisp程序中启用词法绑定。


* Dynamic Binding
* Proper Use of Dynamic Binding
* Lexical Binding
* Using Lexical Binding
* Converting to Lexical Binding

#### 12.10.1 Dynamic Binding ####

[Dynamic Binding](https://www.gnu.org/software/emacs/manual/html_node/elisp/Dynamic-Binding.html)

默认情况下，Emacs所做的局部变量绑定是动态绑定。当一个变量被动态绑定时，在Lisp程序执行的任何时刻，它的当前绑定都只是为该符号最近创建的动态局部绑定，如果没有这样的局部绑定，则为全局绑定。

动态绑定具有动态的作用域和范围，如下例所示:

``` Elisp
(defvar x -99)  ; x receives an initial value of −99.

(defun getx ()
  x)            ; x is used free in this function.

(let ((x 1))    ; x is dynamically bound.
  (getx))
     ⇒ 1

;; After the let form finishes, x reverts to its
;; previous value, which is −99.

(getx)
     ⇒ -99
```

函数getx指向x。这是一个自由引用 free reference，在这个defun构造本身中没有对x的绑定。当我们从(动态)绑定x的let形式内部调用getx时，它会检索局部值(即1)。但是当我们在let形式外部调用getx时，它会检索全局值(即 `-99`)。

下面是另一个例子，它演示了使用setq设置一个动态绑定变量:

``` Elisp
(defvar x -99)      ; x receives an initial value of −99.

(defun addx ()
  (setq x (1+ x)))  ; Add 1 to x and return its new value.

(let ((x 1))
  (addx)
  (addx))
     ⇒ 3           ; The two addx calls add to x twice.

;; After the let form finishes, x reverts to its
;; previous value, which is −99.

(addx)
     ⇒ -98
```

动态绑定在Emacs Lisp中以一种简单的方式实现。每个符号都有一个值单元格，它指定了它当前的动态值(或没有值)。参见符号组件。当一个符号被赋予动态局部绑定时，Emacs将值单元的内容(或没有)记录在堆栈中，并将新的局部值存储在值单元中。当绑定构造完成执行时，Emacs将旧值从堆栈中弹出，并将其放入值单元中。

注意，当使用动态绑定的代码是本机编译时，本机编译器不会执行任何特定于Lisp的优化。

#### 12.10.2 Proper Use of Dynamic Binding ####

动态绑定是一个强大的特性，因为它允许程序引用未在其本地文本范围内定义的变量。但是，如果不加约束地使用，也会使程序难以理解。有两种简单的方法可以使用这个技巧:

1. 如果变量没有全局定义，则仅在绑定构造中将其用作局部变量，例如在绑定变量的let形式的主体中。如果在整个程序中始终遵循这一约定，则变量的值将不会影响，也不会受到程序中其他地方使用同一变量符号的影响。
2. 否则，用defvar、defconst(见定义全局变量)或defcustom(见定义自定义变量)定义变量。通常，该定义应该位于Emacs Lisp文件的顶层。尽可能地，它应该包含一个解释变量的含义和目的的文档字符串。您还应该选择变量的名称以避免名称冲突(参见Emacs Lisp编码约定 Emacs Lisp Coding Conventions [Emacs Lisp Coding Conventions](https://www.gnu.org/software/emacs/manual/html_node/elisp/Coding-Conventions.html))。

然后，您可以在程序中的任何地方绑定变量，从而可靠地知道将会产生什么效果。无论在哪里遇到这个变量，都可以很容易地引用它的定义，例如，通过 `C-h-v` 命令(前提是变量定义已经加载到Emacs中)。请参见《GNU Emacs手册》中的名称帮助 Name Help。

例如，对 `case-fold-search` 这样的可定制变量使用局部绑定是很常见的:

``` Elisp
(defun search-for-abc ()
  "Search for the string \"abc\", ignoring case differences."
  (let ((case-fold-search t))
    (re-search-forward "abc")))
```

#### 12.10.3 Lexical Binding ####

词法绑定是在24.1版本中作为可选特性引入Emacs的。我们预计其重要性将随着时间的推移而增加。词法绑定为优化提供了更多的机会，因此使用它的程序可能在未来的Emacs版本中运行得更快。词法绑定与并发性 concurrency 也更加兼容，并发性是在Emacs 26.1版中添加的。

词法绑定变量具有 *词法作用域 lexical scope*，这意味着对该变量的任何引用都必须在文本上位于绑定构造中。下面是一个示例(参见使用词法绑定 Using Lexical Binding，了解如何实际启用词法绑定):

``` Elisp
(let ((x 1))    ; x is lexically bound.
  (+ x 3))
     ⇒ 4

(defun getx ()
  x)            ; x is used free in this function.

(let ((x 1))    ; x is lexically bound.
  (getx))
error→ Symbol's value as variable is void: x
```

这里，变量x没有全局值。当它在词法上绑定在let形式中时，它可以在该let形式的文本范围内使用。但是它不能在从let形式调用的getx函数中使用，因为getx的函数定义发生在let形式本身之外。

以下是词法绑定的工作原理。每个绑定构造定义一个词法环境，指定在该构造中绑定的变量及其局部值。当Lisp求值器需要一个变量的当前值时，它首先在词法环境中查找;如果没有在这里指定变量，它将在符号的值单元格中查找，动态值存储在这里。

(在内部，词法环境是一个列表，其成员通常是 symbol-value pair的cons cell，但它的一些成员可以是符号而不是cons cell。列表中的符号意味着词法环境将该符号的变量声明为局部认为是动态绑定的。该列表可以作为第二个参数传递给eval函数，以便指定对表单求值的词法环境。看到Eval。然而，大多数Emacs Lisp程序不应该以这种方式直接与词法环境交互;只有专门的程序，比如调试器 debuggers。)

词法绑定具有不确定的范围。即使在绑定构造完成执行之后，它的词法环境也可以在称为 *闭包Closures* 的Lisp对象中“保留”。闭包是在定义启用词法绑定的命名或匿名函数时创建的。有关详细信息，请参阅闭包。

当闭包作为函数调用时，其定义中的任何词法变量引用都使用保留的词法环境。下面是一个例子:

``` Elisp
(defvar my-ticker nil)   ; We will use this dynamically bound
                         ; variable to store a closure.

(let ((x 0))             ; x is lexically bound.
  (setq my-ticker (lambda ()
                    (setq x (1+ x)))))
    ⇒ (closure ((x . 0)) ()
          (setq x (1+ x)))

(funcall my-ticker)
    ⇒ 1

(funcall my-ticker)
    ⇒ 2

(funcall my-ticker)
    ⇒ 3

x                        ; Note that x has no global value.
error→ Symbol's value as variable is void: x
```

let绑定定义了一个词法环境，其中变量x局部绑定为0。在这个绑定构造中，我们定义了一个lambda表达式，它将x加1并返回加1后的值。该lambda表达式自动转换为闭包，即使在let绑定构造退出后，其中的词法环境仍然存在。每次求值闭包时，它使用该词法环境中x的绑定，使x加1。

请注意，与符号对象本身绑定的动态变量不同，词法变量和符号之间的关系仅存在于解释器(或编译器)中。因此，接受符号参数的函数(如symbol-value, boundp和set)只能检索或修改变量的动态绑定(即其符号值单元的内容)。

#### 12.10.4 Using Lexical Binding ####

当加载Emacs Lisp文件或计算Lisp缓冲区时，如果缓冲区局部变量lexical-binding为非nil，则启用词法绑定:

##### 变量: `lexical-binding` #####

如果这个buffer-local变量非nil，则使用词法绑定而不是动态绑定来评估Emacs Lisp文件和缓冲区。(但是，特殊变量仍然是动态绑定的;见下文)。如果为nil，则对所有局部变量使用动态绑定。这个变量通常是为整个Emacs Lisp文件设置的，作为文件局部变量(请参阅文件局部变量 File Local Variables)。请注意，与其他此类变量不同，这个变量必须在文件的第一行设置。

当使用eval调用直接计算Emacs Lisp代码时，如果eval的词法参数为非nil，则启用词法绑定。看到Eval。

词法绑定也在Lisp交互和IELM模式中启用，在 `*scratch*` 和 `*IELM*` 缓冲区中使用，也在通过 `M-: (eval-expression)` 计算表达式和处理Emacs的 `——eval` 命令行选项(参见《GNU Emacs手册》中的动作参数 Action Arguments)和emacsclient(参见《GNU Emacs手册》中的emacsclient选项)时启用。

即使启用了词法绑定，某些变量也会继续被动态绑定。这些被称为 *特殊变量 special variables*。每个用defvar、defcustom或defconst定义的变量都是一个特殊的变量(参见定义全局变量)。所有其他变量都服从词法绑定。

使用不带值的 `defvar`，可以只在一个文件中动态绑定变量，或者只在文件的一部分中绑定变量，同时仍然在其他地方对其进行词法绑定。例如:

``` Elisp
(let (_)
  (defvar x)      ; Let-bindings of x will be dynamic within this let.
  (let ((x -99))  ; This is a dynamic binding of x.
    (defun get-dynamic-x ()
      x)))

(let ((x 'lexical)) ; This is a lexical binding of x.
  (defun get-lexical-x ()
    x))

(let (_)
  (defvar x)
  (let ((x 'dynamic))
    (list (get-lexical-x)
          (get-dynamic-x))))
    ⇒ (lexical dynamic)
```

##### 函数: `special-variable-p symbol` #####

如果symbol是一个特殊变量(即，它有defvar, defcustom或defconst变量定义)，则此函数返回非nil。否则，返回值为nil。

请注意，由于这是一个函数，它只能为永久特殊的变量返回非nil，而不能为那些仅在当前词法范围内特殊的变量返回非nil。

不支持在函数中使用特殊变量作为形式参数。

#### 12.10.5 Converting to Lexical Binding ####

将Emacs Lisp程序转换为词法绑定很容易。首先，在Emacs Lisp源文件的头行中添加一个 `lexical-binding` 为 t 的文件局部变量设置(请参阅文件局部变量 File Local Variables)。其次，检查程序中需要动态绑定的每个变量是否都有变量定义，这样它就不会不经意地在词法上绑定。

找出哪些变量需要变量定义的一种简单方法是对源文件进行字节编译。参见字节编译 Byte Compilation。如果在let形式之外使用非特殊变量，字节编译器将警告对自由变量的引用或赋值。如果绑定了一个非特殊变量，但没有在let形式中使用，则字节编译器将警告未使用的词法变量。如果使用特殊变量作为函数参数，字节编译器也会发出警告。

关于对自由变量的引用或赋值的警告通常是一个明确的信号，表明该变量应该被标记为动态作用域，因此您需要在第一次使用该变量之前添加适当的defvar。

关于未使用变量的警告可能是一个很好的提示，表明该变量打算被动态作用域(因为它实际上是在另一个函数中使用的)，但它也可能表明该变量实际上没有被使用，可以简单地删除。因此，您需要找出是哪种情况，并在此基础上添加defvar或完全删除该变量。如果不可能或不希望删除(通常是因为它是一个正式参数，我们不能或不想更改所有调用者)，您还可以在变量名前添加一个下划线，以向编译器表明这是一个已知不会使用的变量。

##### Cross-file variable checking #####

> 警告:这是一个实验性的功能，可能会在没有事先通知的情况下改变或消失。

字节编译器还可以对其他Emacs Lisp文件中的特殊词法变量发出警告，通常表示缺少defvar声明。这个有用但有些特殊的检查需要三个步骤:

1. 将环境变量 `EMACS_GENERATE_DYNVARS` 设置为非空字符串，对其特殊变量声明可能感兴趣的所有文件进行字节编译。这些通常是同一包或相关包或Emacs子系统中的所有文件。该进程将为每个已编译的Emacs Lisp文件生成一个以 `.dynvars` 结尾的文件。
2. 将 `.dynvars` 文件连接到单个文件中。
3. 对需要检查的文件进行字节编译，这一次将环境变量 `EMACS_DYNVARS_FILE` 设置为步骤2中创建的聚合文件的名称。

这里有一个例子说明了如何做到这一点，假设Unix shell和make用于字节编译:

``` shell
$ rm *.elc                                # force recompilation
$ EMACS_GENERATE_DYNVARS=1 make           # generate .dynvars
$ cat *.dynvars > ~/my-dynvars            # combine .dynvars
$ rm *.elc                                # force recompilation
$ EMACS_DYNVARS_FILE=~/my-dynvars make    # perform checks
```

### 12.11 Buffer-Local Variables ###

在大多数编程语言中，全局和局部变量绑定都以这样或那样的形式存在。但是，Emacs还支持额外的、不常见的变量绑定类型，例如仅在一个缓冲区中应用的缓冲区本地绑定。在不同的缓冲区中为变量设置不同的值是一种重要的定制方法。(变量也可以具有对每个终端本地的绑定。参见多终端 Multiple Terminals。)


* Introduction to Buffer-Local Variables
* Creating and Deleting Buffer-Local Bindings
* The Default Value of a Buffer-Local Variable

#### 12.11.1 Introduction to Buffer-Local Variables ####

[Introduction to Buffer-Local Variables](https://www.gnu.org/software/emacs/manual/html_node/elisp/Intro-to-Buffer_002dLocal.html)

缓冲区局部变量具有与特定缓冲区关联的缓冲区局部绑定。当缓冲区为当前时，绑定生效;否则，它不生效。如果在缓冲区本地绑定生效时设置变量，则新值将进入该绑定，因此它的其他绑定保持不变。这意味着更改仅在您进行更改的缓冲区中可见。

变量的 *普通绑定 ordinary binding* (不与任何特定缓冲区关联)称为 *默认绑定 default binding*。在大多数情况下，这是*全局绑定 global binding*。

变量可以在某些缓冲区中具有缓冲区本地绑定，但在其他缓冲区中不具有。默认绑定由所有没有自己的变量绑定的缓冲区共享。(这包括所有新创建的缓冲区。)如果在没有缓冲区本地绑定的缓冲区中设置变量，则会设置默认绑定，因此新值在所有看到默认绑定的缓冲区中可见。

缓冲区本地绑定最常见的用途是让主要模式更改控制命令行为的变量。例如，C模式和Lisp模式都设置了变量paragraph-start，以指定只有空行分隔段落。它们通过在要进入C模式或Lisp模式的缓冲区中设置变量buffer-local，然后将其设置为该模式的新值来实现这一点。参见主要模式 Major Modes。

创建缓冲区本地绑定的常用方法是使用 `make-local-variable`，这是主模式命令通常使用的方法。这只影响当前缓冲区;所有其他缓冲区(包括尚未创建的缓冲区)将继续共享默认值，除非显式地为它们提供自己的缓冲区本地绑定。

一个更强大的操作是通过调用 `make-variable-buffer-local` 将变量自动标记为buffer-local。您可以将其视为在所有缓冲区(甚至是尚未创建的缓冲区)中使变量本地化。更准确地说，设置变量的效果是，如果变量还不是当前缓冲区的局部变量，则会自动使其成为当前缓冲区的局部变量。所有缓冲区开始时都像往常一样共享变量的默认值，但是设置变量会为当前缓冲区创建一个缓冲区本地绑定。新值存储在缓冲区本地绑定中，而不改变默认绑定。这意味着默认值不能在任何缓冲区中使用setq更改;改变它的唯一方法是使用 `setq-default`。

> 警告:当一个变量在一个或多个缓冲区中具有缓冲区局部绑定时，让它重新绑定当前有效的绑定。例如，如果当前缓冲区有一个buffer-local值，让它临时重新绑定。如果没有有效的缓冲区本地绑定，让它重新绑定默认值。如果在let中更改到另一个当前缓冲区，其中生效的绑定不同，则不会再看到let绑定。如果在另一个缓冲区中退出let，则不会看到解除绑定发生(尽管它会正确地发生)。这里有一个例子来说明:

``` Elisp
(setq foo 'g)
(set-buffer "a")
(make-local-variable 'foo)

(setq foo 'a)
(let ((foo 'temp))
  ;; foo ⇒ 'temp  ; let binding in buffer ‘a’
  (set-buffer "b")
  ;; foo ⇒ 'g     ; the global value since foo is not local in ‘b’
  body…)

foo ⇒ 'g        ; exiting restored the local value in buffer ‘a’,
                 ; but we don’t see that in buffer ‘b’

(set-buffer "a") ; verify the local value was restored
foo ⇒ 'a
```

注意，在body中对foo的引用访问缓冲区 b 的buffer-local绑定。

当文件指定局部变量值时，当您访问该文件时，这些值将成为缓冲区本地值。参见《GNU Emacs手册》中的文件变量 File Variables。

不能将buffer-local变量设置为terminal-local(参见多终端 Multiple Terminals)。

#### 12.11.2 Creating and Deleting Buffer-Local Bindings ####

##### 命令: `make-local-variable variabel` #####

这个函数在当前缓冲区中为变量(符号)创建一个缓冲区局部绑定。其他缓冲区不受影响。返回的值是可变的。

变量的 buffer-local 值开始时与先前的值相同。如果变量是空的，它仍然是空的。

``` Elisp
;; In buffer ‘b1’:
(setq foo 5)                ; Affects all buffers.
     ⇒ 5

(make-local-variable 'foo)  ; Now it is local in ‘b1’.
     ⇒ foo

foo                         ; That did not change
     ⇒ 5                   ;   the value.

(setq foo 6)                ; Change the value
     ⇒ 6                   ;   in ‘b1’.

foo
     ⇒ 6


;; In buffer ‘b2’, the value hasn’t changed.
(with-current-buffer "b2"
  foo)
     ⇒ 5
```

在变量的let绑定中将变量设置为buffer-local并不能可靠地工作，除非您执行此操作的缓冲区在进入或退出let时不是当前的。这是因为let不区分不同类型的绑定;它只知道绑定的对象是哪个变量。

将常量或只读变量设置为buffer-local是错误的。参见永远不变的变量 Variables that Never Change。

如果变量是终端本地的(参见多终端 Multiple Terminals)，这个函数会发出错误信号。这样的变量也不能具有缓冲区本地绑定。

> 警告:不要对 **钩子变量** 使用 `make-local-variable`。如果您使用local参数 `add-hook` 或 `remove-hook`，则根据需要自动将钩子变量设置为buffer-local。

##### 宏: `setq-local &rest pairs` #####

`pairs` 是 变量和值对 的列表。这个宏在当前缓冲区中为每个变量创建一个缓冲区本地绑定，并为它们提供一个缓冲区本地值。它相当于对每个变量调用 `make-local-variable`，然后调用 `setq`。变量应该是 unquoted 的符号。

``` Elisp
(setq-local var1 "value1"
            var2 "value2")
```

##### 命令: `make-variable-buffer-local variable` #####

这个函数将 variable (一个符号)自动标记为 buffer-local，因此任何后续尝试设置它都会使其在当前缓冲区中本地。与 `make-local-variable` 不同的是，这是无法撤消的，并且会影响变量在所有缓冲区中的行为。

这个特性的一个特殊之处在于绑定变量(使用let或其他绑定构造)不会为它创建缓冲区局部绑定。只有设置变量(使用set或setq)，而变量没有在当前缓冲区中进行的let风格的绑定，才会这样做。

如果变量没有默认值，那么调用该命令将给它一个默认值nil。如果变量已经有默认值，则该值保持不变。随后在变量上调用 `makunbound` 将导致缓冲区本地值为空，而默认值不受影响。

返回的值是 variable。

将常量或只读变量设置为buffer-local是错误的。参见永远不变的变量 Variables that Never Change。

> 警告:不要认为应该对用户选项变量使用 `make-variable-buffer-local`，因为用户可能希望在不同的缓冲区中对它们进行不同的定制。用户可以将任何变量设置为本地，只要他们愿意。最好让他们自己选择。

使用 `make-variable-buffer-local` 的时机是在没有两个缓冲区共享相同绑定的情况下。例如，当一个变量在Lisp程序中用于内部目的时，它依赖于在单独的缓冲区中有单独的值，那么使用 `make-variable-buffer-local` 可能是最好的解决方案。

##### 宏: `defvar-local variable value &optional docstring` #####

这个宏将变量定义为具有初始值 value 和 docstring 的变量，并将其标记为自动 buffer-local。它相当于调用defvar，然后调用make-variable-buffer-local。变量应该是不加引号的符号。

##### 函数: `local-variable-p variable &optional buffer` #####

如果变量是buffer buffer中的buffer-local(默认为当前缓冲区)，则返回t;否则,nil。

##### 函数: `local-variable-if-set-p variable &optional buffer` #####

如果变量在buffer buffer中具有 buffer-local 值，或者自动为buffer-local，则返回t。否则，它返回nil。如果省略或为nil，则buffer默认为当前缓冲区。

##### 函数: `buffer-local-variable variable buffer` #####

这个函数返回buffer buffer中变量(符号)的buffer-local绑定。如果variable在buffer buffer中没有buffer-local绑定，它将返回variable的默认值(参见buffer-local变量的默认值 The Default Value of a Buffer-Local Variable)。

##### 函数: `buffer-local-boundp variable buffer` #####

如果在buffer buffer中存在变量(符号)的缓冲区局部绑定，或者变量具有全局绑定，则返回非nil。

##### 函数: `buffer-local-variables &optional buffer` #####

这个函数返回一个描述buffer buffer中 buffer-local变量的列表。(如果省略buffer，则使用当前缓冲区。)通常，每个列表元素的形式为 `(sym . val)`，其中sym是缓冲局部变量(符号)，Val是它的缓冲局部值。但是当一个变量在buffer中的buffer-local绑定为空时，它的list元素就是sym。

``` Elisp
(make-local-variable 'foobar)
(makunbound 'foobar)
(make-local-variable 'bind-me)
(setq bind-me 69)

(setq lcl (buffer-local-variables))
    ;; First, built-in variables local in all buffers:
⇒ ((mark-active . nil)
    (buffer-undo-list . nil)
    (mode-name . "Fundamental")
    …

    ;; Next, non-built-in buffer-local variables.
    ;; This one is buffer-local and void:
    foobar
    ;; This one is buffer-local and nonvoid:
    (bind-me . 69))
```

请注意，将新值存储到此列表中cons单元格的cdr中不会更改变量的缓冲区本地值。

##### 命令: `kill-local-variable variable` #####

这个函数删除当前缓冲区中变量(符号)的缓冲区本地绑定(如果有的话)。因此，变量的默认绑定在此缓冲区中变得可见。这通常会导致变量的值发生变化，因为默认值通常不同于刚刚消除的buffer-local值。

如果您终止一个变量的buffer-local绑定(该变量在设置时自动变为buffer-local)，则会使默认值在当前缓冲区中可见。但是，如果您再次设置变量，将再次为它创建一个缓冲区本地绑定。

kill-local-variable 返回 variable。

这个函数是一个命令，因为有时交互式地终止一个缓冲区局部变量很有用，就像交互式地创建缓冲区局部变量很有用一样。

##### 函数: `kill-all-local-variables &optional kill-permanent` #####

此函数消除当前缓冲区的所有缓冲区局部变量绑定。因此，缓冲区将看到大多数变量的默认值。默认情况下，对于具有非空permanent-local-hook属性(参见设置钩子 Setting Hooks)的标记为永久和局部钩子函数的变量不会被杀死，但如果可选的kill-permanent参数是非空的，即使这些变量也会被杀死。

此函数还重置与缓冲区有关的某些其他信息:它将本地keymap设置为nil，将语法表设置为(standard-syntax-table)的值，将大小写表设置为(standard-case-table)，将缩写表设置为fundamental-mode-abbrev-table的值。

这个函数做的第一件事就是运行普通钩子change-major-mode-hook(见下文)。

每个主要模式命令都以调用这个函数开始，它的作用是切换到基本模式并消除前一个主要模式的大部分效果。为了确保它的工作，主要模式设置的变量不应该被标记为永久的。

kill-all-local-variables 返回 nil. 

##### 变量: `change-major-mode-hook` #####

kill-all-local-variables 函数在执行其他操作之前运行这个普通钩子。这为主模式提供了一种方法，以便在用户切换到不同的主模式时安排一些特殊的事情。它对于缓冲区特定的次要模式也很有用，如果用户更改主模式，则应该忘记这些模式。

为了获得最佳效果，将此变量设置为缓冲区局部，以便它在完成其工作后消失，并且不会干扰后续的主模式。看到钩子 Hooks。

如果变量名(符号)具有非nil的永久局部属性，则缓冲局部变量是永久性的。这些变量不受kill-all-local-variables的影响，因此它们的局部绑定不会通过更改主模式来清除。永久局部变量适用于与文件来自何处或如何保存有关的数据，而不适用于如何编辑内容。

#### 12.11.3 The Default Value of a Buffer-Local Variable ####

具有缓冲区本地绑定的变量的全局值也称为默认值，因为当前缓冲区和所选帧都没有自己的变量绑定时，该值才会生效。

无论当前缓冲区是否具有缓冲区本地绑定，函数default-value和setq-default都可以访问和更改变量的默认值。例如，你可以使用setq-default来改变大多数缓冲区的paragraph-start的默认设置;即使在C或Lisp模式缓冲区中，该变量具有缓冲区局部值，这也会起作用。

特殊形式defvar和defconst也设置默认值(如果它们设置了变量的话)，而不是任何缓冲区本地值。

##### 函数: `default-value symbol` #####

这个函数返回symbol的默认值。这是在 buffer 和 frame 中看到的值，这些缓冲区和帧没有自己的值。如果symbol不是缓冲区本地的，这相当于symbol-value(参见访问变量值)。

##### 函数: `default-boundp symbol` #####

函数 `default-boundp` 告诉你symbol的默认值是否为nonvoid。如果(default-boundp 'foo)返回nil，那么(default-value 'foo)将得到一个错误。

`default-boundp` 对应于 default-valuec，正如 `boundp` 对应于 `symbol-value` 一样。

##### 特殊形式: `setq-default [symbol form]...` #####

这个特殊的形式给每个符号一个新的默认值，这是计算相应形式的结果。它不评价符号，但评价形式。setq-default表单的值是最后一个表单的值。

如果一个符号不是当前缓冲区的buffer-local，并且没有被自动标记为buffer-local, setq-default具有与setq相同的效果。如果symbol是当前缓冲区的buffer-local，那么这会改变其他缓冲区将看到的值(只要它们没有buffer-local值)，但不会改变当前缓冲区看到的值。

``` Elisp
;; In buffer ‘foo’:
(make-local-variable 'buffer-local)
     ⇒ buffer-local

(setq buffer-local 'value-in-foo)
     ⇒ value-in-foo

(setq-default buffer-local 'new-default)
     ⇒ new-default

buffer-local
     ⇒ value-in-foo

(default-value 'buffer-local)
     ⇒ new-default


;; In (the new) buffer ‘bar’:
buffer-local
     ⇒ new-default

(default-value 'buffer-local)
     ⇒ new-default

(setq buffer-local 'another-default)
     ⇒ another-default

(default-value 'buffer-local)
     ⇒ another-default


;; Back in buffer ‘foo’:
buffer-local
     ⇒ value-in-foo
(default-value 'buffer-local)
     ⇒ another-default
```

##### 函数: `set-default symbol value` #####

这个函数类似于setq-default，只不过符号是一个普通的求值参数。

``` Elisp
(set-default (car '(a b c)) 23)
     ⇒ 23

(default-value 'a)
     ⇒ 23
```

一个变量可以被 `let-bound` 绑定到一个值(参见局部变量Local Variables)。这使得它的全局值被绑定遮蔽; default-value将返回该绑定的值，而不是全局值， `set-default` 将被阻止设置全局值(它将更改let-bound值)。下面两个函数允许引用全局值，即使它被let绑定遮蔽。

##### 函数: `default-toplevel-value symbol` #####

这个函数返回symbol的顶级默认值，这是它在任何let绑定之外的值。

``` Elisp
(defvar variable 'global-value)
    ⇒ variable

(let ((variable 'let-binding))
  (default-value 'variable))
    ⇒ let-binding

(let ((variable 'let-binding))
  (default-toplevel-value 'variable))
    ⇒ global-value
```

##### 函数: `set-default-toplevel-value symbol value` #####

此函数将符号的顶级默认值设置为指定值。当你想设置symbol的全局值，而不管你的代码是否在symbol的let-binding上下文中运行时，这就派上用场了。

### 12.12 File Local Variables ###

文件可以指定局部变量值;Emacs使用它们为访问该文件的缓冲区中的变量创建缓冲区本地绑定。有关文件局部变量的基本信息，请参阅《GNU Emacs手册》中的文件局部变量。本节介绍影响如何处理文件局部变量的函数和变量。

``` Elisp
;; -*- global-display-line-numbers-mode: 1; -*-
```

如果文件局部变量可以指定稍后将调用的任意函数或Lisp表达式，则访问文件可能会占用您的Emacs。Emacs通过只自动设置那些已知其指定值是安全的文件本地变量来防止这种情况。只有在用户同意的情况下才设置其他文件局部变量。

为了额外的安全性，当Emacs读取文件局部变量时，read-circle被临时绑定为nil(参见输入函数 Input Functions)。这将阻止Lisp阅读器识别循环和共享Lisp结构(参见循环对象的读取语法 Read Syntax for Circular Objects)。

#### 用户选项: `enable-local-variables` ####

该变量控制是否处理文件局部变量。可能的值是:

* `t` 设置安全变量，并查询query(一次)任何不安全变量。
* `:safe` 只设置安全变量，不查询。
* `:all` 设置所有变量，不查询。
* `nil`: 不设置任何变量
* 其他: 查询所有变量(一次)。

#### 变量: `inhibit-local-variables-regexps` ####

这是一个正则表达式列表。如果文件的名称与此列表中的元素匹配，则不会扫描它以查找任何形式的文件局部变量。有关为什么要使用这种模式的示例，请参见Emacs如何选择主模式 How Emacs Chooses a major Mode。

#### 变量: `permanently-enabled-local-variables` ####

默认情况下，即使 `enable-local-variables` 为nil，也需要一些局部变量设置。默认情况下，这种情况仅适用于词法绑定局部变量设置，但可以通过使用该变量来控制，该变量是一个符号列表。

#### 函数: `hack-local-variables &optional handle-mode` ####

该函数解析、绑定或计算由当前缓冲区的内容指定的任何局部变量。变量 `enable-local-variables` 在这里起作用。但是，这个函数不会在 ` -*- ` 行中查找 `mode:` 局部变量。`set-auto-mode` 做到了这一点，同时也考虑了enable-local-variables(参见Emacs如何选择主模式 How Emacs Chooses a Major Mode)。

这个函数的工作原理是遍历存储在 `file-local-variables-list` 中的列表，并依次应用每个局部变量。它分别在应用变量之前和之后调用 `before-hack-local-variables-hook` 和 `hack-local-variables-hook`。它只在列表非nil时调用before-hook;它总是调用另一个钩子。如果 mode 元素指定的主模式与缓冲区已有的主模式相同，则该函数将忽略该元素。

如果可选参数 `handle-mode` 是 t，那么这个函数所做的就是返回一个指定主模式的符号，如果 `-*-` 行或局部变量列表指定了一个，否则返回nil。它不设置模式或任何其他文件本地变量。如果 `handle-mode` 的值不是nil或t，则忽略 `-*-` 行或局部变量列表中 mode 的任何设置，并应用其他设置。如果 `handle-mode` 为nil，则设置所有文件局部变量。

#### 变量: `file-local-variables-alist` ####

这个buffer-local变量保存了文件本地变量设置列表。列表中的每个元素的形式为 `(var . value)` 其中var是局部变量的符号，Value是它的值。当Emacs访问一个文件时，它首先将所有的文件局部变量收集到这个列表中，然后 `hack-local-variables` 函数逐个应用它们。

#### 变量: `before-hack-local-variables-hook` ####

在应用存储在 `file-local-variables-list` 中的 file-local 变量之前，Emacs会立即调用这个钩子。

#### 变量: `hack-local-variables-hook` ####

Emacs在应用完存储在 `file-local-variables-list` 中的文件局部变量后立即调用这个钩子。

可以使用 `safe-local-variable` 属性为变量指定安全值。这个属性必须是一个参数的函数;如果函数返回给定值的非空值，则任何值都是安全的。许多常见的文件变量都有安全的局部变量属性;这些模式包括 `fill-column`、`fill-prefix` 和 `indent-tabs-mode`。对于安全的布尔值变量，使用 `booleanp` 作为属性值。

如果您想为C源代码中定义的变量定义安全局部变量属性，请将这些变量的名称和属性添加到 `files.el` 的 *安全局部变量 Safe local variables* 部分中的列表中。

当使用 `defcustom` 定义用户选项时，您可以通过向 `defcustom` 添加参数 `:safe function` 来设置其 `safe-local-variable` 属性(参见定义自定义变量Defining Customization Variables)。然而，使用 `:safe` 定义的安全谓词只有在包含该定义的包被加载时才会被知道，而这通常为时已晚。作为一种替代方法，你可以使用 *autolload cookie* (参见autolload)为该选项分配其安全谓词，如下所示:

``` Elisp
;;;###autoload (put 'var 'safe-local-variable 'pred)
```

用autolload指定的安全值定义被复制到包的autolload文件(loaddefs)中。(对于大多数与Emacs捆绑在一起的包)，并且从会话开始就为Emacs所知。

#### 用户选项: `safe-local-variable-values` ####

该变量提供了另一种将某些变量值标记为安全的方法。它是一个cons单元格列表 `(var . val)`，其中var是变量名，val是该变量的安全值。

当Emacs询问用户是否遵守一组文件本地变量规范时，用户可以选择将它们标记为安全。这样做会将这些变量/值对添加到safe-local-variable-values中，并将其保存到用户的自定义文件中。

#### 用户选项: `ignore-local-variable-values` ####

如果您总是希望完全忽略某些特定局部变量的值，则可以使用此变量。其值的形式与 `safe-local-variable-values` 相同;在处理文件指定的局部变量时，将始终忽略为列表中出现的值的文件局部变量。与该变量一样，当Emacs询问用户是否遵守文件局部变量时，用户可以选择永久忽略它们的特定值，这将改变该变量并将其保存到用户的自定义文件中。在此变量中出现的变量值对优先于在 `safe-local-variable-values` 中出现的相同对。

#### 函数: `safe-local-variable-p sym val` ####

如果根据上述条件给sym值val是安全的，则此函数返回非nil。

有些变量被认为有风险。如果一个变量有风险，它永远不会被自动输入到 `safe-local-variable-values` 中;Emacs总是在设置有风险的变量之前进行查询，除非用户通过直接自定义 `safe-local-variable-values` 显式地允许一个值。

任何具有非nil的 `risk-local-variable` 属性的变量都被认为是有风险的。当您使用 `defcustom` 定义用户选项时，您可以通过向 `defcustom` 添加参数 `:risky value` 来设置其 `risky-local-variable` 属性(参见定义自定义变量 Defining Customization Variables)。此外，任何以 `-command`、`-frame-alist`、`-function`、`-functions`、`-hook`、`-hooks`、`-form`、`-forms`、`-map`、`-map-alist`、`-mode-alist`、`-program`或`-predicate`结尾的变量都会被自动认为是有风险的。变量 `font-lock-keywords` 、`font-lock-keywords` 后面跟着一个数字，以及 `font-lock-syntactic-keywords` 也被认为是有风险的。

#### 函数: `risky-local-variable-p sym` ####

如果sym是一个有风险的变量，根据上述条件，这个函数返回非nil。

#### 变量: `ignored-local-variables` ####

这个变量保存了一个变量列表，这些变量不应该被文件赋予局部值。为其中一个变量指定的任何值都将被完全忽略。

`Eval:` variable 也是一个潜在的漏洞，因此Emacs通常在处理它之前要求确认。

#### 用户选项: `enable-local-eval` ####

该变量控制 `-*-` 行中 `Eval:` 的处理或访问文件中的局部变量列表。值t表示无条件地处理它们; nil表示忽略它们;其他任何情况都意味着询问用户对每个文件做什么。默认值是 `maybe`。

#### 用户选项: `safe-local-eval-forms` ####

该变量保存了一个表达式列表，当在文件局部变量列表中的 `Eval:` variable 中找到时，可以安全地求值。

如果表达式是函数调用，并且函数具有 `safety-local-eval-function` 属性，则该属性值确定表达式是否可以安全求值。属性值可以是一个用来测试表达式的谓词，也可以是一个这样的谓词列表(如果任何谓词成功，则是安全的)，或者是t(只要参数是常量，则总是安全的)。

Text properties 文本属性也是潜在的漏洞，因为它们的值可能包含要调用的函数。因此，Emacs将从为文件局部变量指定的字符串值中丢弃所有文本属性。

### 12.13 Directory Local Variables ###

目录可以指定该目录中所有文件通用的本地变量值;Emacs使用它们为访问该目录中任何文件的缓冲区中的变量创建缓冲区本地绑定。当目录中的文件属于某个项目，因此共享相同的本地变量时，这很有用。

有两种不同的方法可以指定目录局部变量:

1. 将它们放在一个特殊的文件中
2. 为该目录定义一个项目类 project class。

#### 常量: `dir-locals-file` ####

这个常量是Emacs希望在其中找到目录本地变量的文件名。文件名为 `.dir-locals.el`。目录中同名的文件会导致Emacs将其设置应用于该目录或其子目录中的任何文件(可选地，您可以排除子目录;见下文)。如果某些子目录有自己的.dir-locals。Emacs使用从文件目录开始并沿着目录树向上移动的最深处的文件中的设置。该常量还用于派生第二个dir-locals文件 `.dir-locals-2.el` 的名称。如果存在第二个dir-locals文件，那么除了加载.dir-locals.el之外，还会加载这个文件。这在 `.dir-locals` 共享存储库中不能用于个人定制, 受版本控制中很有用。该文件将局部变量指定为特殊格式的列表;有关详细信息，请参阅《GNU Emacs手册》中的每目录本地变量 Per-directory Local Variables。

[Per-Directory Local Variables](https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html#Directory-Variables)

#### 函数: `hack-dir-local-variables` ####

这个函数读取 `.dir-locals.el` 并将目录本地变量存储在 `file-local-variables-alist` 中，该列表对于访问目录中的任何文件的缓冲区来说是本地的，而不应用它们。它还将目录本地设置存储在 `dir-locals-class-alist`中，它为 `.dir-locals.el` 所在的目录定义了一个特殊的类。该函数通过调用 `dir-locals-set-class-variables` 和 `dir-locals-set-directory-class` 来工作，如下所述。

#### 函数: `hack-dir-local-variables-non-file-buffer` ####

这个函数查找目录本地变量，并立即将它们应用到当前缓冲区中。它旨在在非文件缓冲区(如Dired缓冲区)的mode命令中调用，以使它们服从目录本地变量设置。对于非文件缓冲区，Emacs在default-directory及其父目录中查找目录本地变量。

#### 函数: `dir-locals-set-class-variables class variables` ####

这个函数为 named `class` 定义了一组变量设置，它是一个符号。稍后可以将该类分配给一个或多个目录，Emacs将把这些变量设置应用于这些目录中的所有文件。变量列表可以是以下两种形式之一

1. `(major-mode . alist)`
2. `(directory . list)`

对于第一种形式，如果文件的缓冲区打开了一个从major-mode派生的模式，那么关联列表中的所有变量都被应用; `alist` 的格式应该是 `(name . value)`。major-mode的特殊值nil表示该设置适用于任何模式。在列表中，您可以使用一个特殊的名称: `subdirs`。如果关联值为nil，则该列表仅应用于相关目录下的文件，而不应用于任何子目录下的文件。

对于第二种形式的变量，如果directory是文件目录的初始子字符串，那么list将按照上述规则递归应用; list应该是这个函数在变量中接受的两种形式之一。

#### 函数: `dir-locals-set-directory-class directory class &optional mtime` ####

这个函数为目录及其子目录中的所有文件分配 `class`。此后，为 `class` 指定的所有变量设置将应用于directory及其子目录中的任何访问文件。类必须已经由 `dir-locals-set-class-variables` 定义。

Emacs在从 `.dir-locals.el` 加载目录变量时在内部使用这个函数。在这种情况下，可选参数 `mtime` 保存文件修改时间 (由 `file-attributes` 返回)。Emacs使用这段时间检查存储的局部变量是否仍然有效。如果你直接赋值一个类，而不是通过文件赋值，这个参数应该是nil。

#### 变量: `dir-locals-class-alist` ####

这个 alist 包含类符号和相关的变量设置。它由 `dir-locals-set-class-variables` 更新。

#### 变量: `dir-locals-directory-cache` ####

这个列表包含目录名、它们分配的类名以及关联目录局部变量文件的修改时间(如果有的话)。`dir-locals-set-directory-class` 函数更新这个列表。

#### 变量: `enable-dir-local-variables` ####

如果为nil，则忽略目录本地变量。这个变量对于想要忽略目录本地变量而仍然尊重文件本地变量的模式可能很有用(参见文件本地变量)。

### 12.14 Connection Local Variables ###

Connection-local variables 为远程连接缓冲区中的不同变量设置提供了一种通用机制(请参阅《GNU Emacs手册》中的远程文件 Remove Files)。它们的绑定和设置取决于缓冲区专用于的远程连接。

* Connection Local Profiles
* Applying Connection Local Variables

#### 12.14.1 Connection Local Profiles ####

Emacs使用连接本地配置文件来存储应用于特定连接的变量设置。然后，您可以使用 `connection-local-set-profiles` 定义应用这些连接的条件，从而将它们与远程连接关联起来。

##### 函数: `connection-local-set-profile-variables profile variables` #####

这个函数为连接配置文件定义了一组变量设置，它是一个符号。稍后可以将连接配置文件分配给一个或多个远程连接，Emacs将把这些变量设置应用于这些连接的所有进程缓冲区。变量中的 alist 形式为 `(name . valuie)`。例子:

``` Elisp
(connection-local-set-profile-variables
  'remote-bash
  '((shell-file-name . "/bin/bash")
    (shell-command-switch . "-c")
    (shell-interactive-switch . "-i")
    (shell-login-switch . "-l")))


(connection-local-set-profile-variables
  'remote-ksh
  '((shell-file-name . "/bin/ksh")
    (shell-command-switch . "-c")
    (shell-interactive-switch . "-i")
    (shell-login-switch . "-l")))


(connection-local-set-profile-variables
  'remote-null-device
  '((null-device . "/dev/null")))
```

如果希望将变量设置附加到现有配置文件中，可以使用 `connection-local-get-profile-variables` 函数来检索现有设置，例如

``` Elisp
(connection-local-set-profile-variables
  'remote-bash
  (append
   (connection-local-get-profile-variables 'remote-bash)
   '((shell-command-dont-erase-buffer . t))))
```

##### 用户选项: `connection-local-profile-alist` #####

该 alist 包含连接配置文件符号和相关的变量设置。它由 `connection-local-set-profile-variables` 更新。

##### 函数: `connection-local-set-profiles criteria &rest profiles` #####

该函数将配置文件 profiles (即符号)分配给由标准 `criteria` 标识的所有远程连接。`criteria`是标识连接和使用该连接的应用程序的列表 plist。属性名可以是 `:application`、`:protocol`、`:user` 和 `:machine`。application的属性值是一个符号，所有其他属性值都是字符串。所有属性都是可选的;如果criteria为nil，则始终适用。例子:

``` Elisp
(connection-local-set-profiles
  '(:application tramp :protocol "ssh" :machine "localhost")
  'remote-bash 'remote-null-device)


(connection-local-set-profiles
  '(:application tramp :protocol "sudo"
    :user "root" :machine "localhost")
  'remote-ksh 'remote-null-device)
```

如果criteria为nil，则适用于所有远程连接。因此，上面的例子等价于

``` Elisp
(connection-local-set-profiles
  '(:application tramp :protocol "ssh" :machine "localhost")
  'remote-bash)


(connection-local-set-profiles
  '(:application tramp :protocol "sudo"
    :user "root" :machine "localhost")
  'remote-ksh)


(connection-local-set-profiles
  nil 'remote-null-device)
```

配置文件的任何连接配置文件必须已经由 `connection-local-set-profile-variables`定义。

##### 用户选项: `connection-local-criteria-alist` #####

此 alist 含连接标准及其分配的配置文件名称。函数 `connection-local-set-profiles`更新这个列表。

#### 12.14.2 Applying Connection Local Variables ####

在编写 connection-aware code 时，需要收集并可能应用任何连接局部变量。有几种方法可以做到这一点，如下所述。

##### 函数: `hack-connection-local-variables criteria` #####

该函数收集与 `connection-local-variables-alist` 中的标准关联的适用的连接局部变量，而不应用它们。例子:

``` Elisp
(hack-connection-local-variables
  '(:application tramp :protocol "ssh" :machine "localhost"))


connection-local-variables-alist
     ⇒ ((null-device . "/dev/null")
        (shell-login-switch . "-l")
        (shell-interactive-switch . "-i")
        (shell-command-switch . "-c")
        (shell-file-name . "/bin/bash"))
```

##### 函数: `hack-connection-local-variables-apply criteria` #####

该函数根据标准查找连接局部变量，并立即将它们应用到当前缓冲区中。

##### 宏: `with-connection-local-application-variables application &rest body` #####

应用 application 的所有连接本地变量，这些变量由默认目录指定。

之后，执行body，并展开连接局部变量。例子:

``` Elisp
(connection-local-set-profile-variables
  'my-remote-perl
  '((perl-command-name . "/usr/local/bin/perl5")
    (perl-command-switch . "-e %s")))


(connection-local-set-profiles
  '(:application my-app :protocol "ssh" :machine "remotehost")
  'my-remote-perl)


(let ((default-directory "/ssh:remotehost:/working/dir/"))
  (with-connection-local-application-variables 'my-app
    do something useful))
```

##### 变量: `connection-local-default-application` #####

默认的 application 是一个符号，将在 `with-connection-local-variables` 中应用。它默认为tramp，但您可以让它绑定来临时更改应用程序(参见局部变量 Local Variables)。

这个变量不能被全局修改。

##### 宏: `with-connection-local-variables &rest body` #####

这相当于 `with-connection-local-application-variables`，但是对应用程序使用 `connection-local-default-application`。

##### 宏: `setq-connection-local [symbol form]...` #####

这个宏使用 `connection-local-profile-name-for-setq` 中指定的 connection-local 配置文件，将每个符号的connection-local设置为相应形式的求值结果;如果配置文件名称为nil，这个宏将像setq一样正常设置变量(参见设置变量值)。

例如，你可以将这个宏与 `with-connection-local-variables` 或 `with-connection-local-application-variables` 结合使用，以惰性初始化 connection-local 设置:

``` Elisp
(defvar my-app-variable nil)

(connection-local-set-profile-variables
 'my-app-connection-default-profile
 '((my-app-variable . nil)))

(connection-local-set-profiles
 '(:application my-app)
 'my-app-connection-default-profile)


(defun my-app-get-variable ()
  (with-connection-local-application-variables 'my-app
    (or my-app-variable
        (setq-connection-local my-app-variable
                               do something useful))))
```

##### 变量: `connection-local-profile-name-for-setq` #####

在通过 `setq-connection-local` 设置变量时使用的连接本地配置文件名称，一个符号。这是在 `with-connection-local-variables` 的主体中绑定的，但是如果您想在不同的配置文件上设置变量，也可以自己绑定它。

这个变量不能被全局修改。

##### 变量: `enable-connection-local-variables` #####

如果为nil，则忽略连接局部变量。该变量只能在特殊模式下临时更改。

### 12.15 Variable Aliases ###

有时使两个变量成为同义词很有用，这样两个变量总是具有相同的值，更改其中一个也会更改另一个。每当更改一个变量的名称时(可能是因为您意识到它的旧名称选择得不好，或者因为它的含义部分地发生了变化)，为了保持兼容性，将旧名称保留为新名称的别名是很有用的。您可以使用 `defvaralias` 来实现这一点。

#### 函数: `defvaralias new-alias base-variable &optional docstring` ####

这个函数将符号 `new-alias` 定义为符号 `base-variable` 的变量别名。这意味着检索 `new-alias` 的值将返回 `base-variable` 的值，而更改 `new-alias` 的值将更改 `base-variable` 的值。两个别名变量名总是共享相同的值和相同的绑定。

如果docstring参数非nil，它为new-alias指定文档;否则，别名将获得与base-variable相同的文档(如果有的话)，除非base-variable本身是别名，在这种情况下，new-alias将获得位于别名链末端的变量文档。

这个函数返回 base-variable。

变量别名可以方便地将变量的旧名称替换为新名称。`make-obsolete-variable` 声明旧的名称已经过时，因此它可能在将来的某个阶段被删除。

#### 函数: `make-obsolete-variable obsolete-name current-name when &optional access-type` ####

这个函数使字节编译器警告变量obsolete-name已过时。如果current-name是一个符号，它是变量的新名称;然后警告消息说要使用当前名称而不是过时名称。如果current-name是一个字符串，这就是消息，没有替代变量。When应该是一个字符串，表示变量第一次被废弃的时间(通常是一个版本号字符串)。

可选参数access-type，如果非nil，应该指定将触发过时警告的访问类型;它可以是get或set。

可以使用宏define-obsolete-variable-alias将两个变量声明为同义词，同时将一个变量声明为废弃变量。

#### 宏: `define-obsolete-variable-alias obsolete-name current-name when &optional docstring` ####

这个宏将变量obsolete-name标记为obsolete，并使其成为变量current-name的别名。它相当于以下内容:

``` Elisp
(defvaralias obsolete-name current-name docstring)
(make-obsolete-variable obsolete-name current-name when)
```

这个宏计算它的所有参数，obsolete-name和current-name都应该是符号，所以典型的用法是这样的:

``` Elisp
(define-obsolete-variable-alias 'foo-thing 'bar-thing "27.1")
```

#### 函数: `indirect-variable variable` ####

这个函数返回变量别名链末尾的变量。如果变量不是符号，或者变量没有定义为别名，则函数返回变量。

如果符号链中存在循环，该函数会发出 循环变量间接错误 cyclic-variable-indirection 信号。

``` Elisp
(defvaralias 'foo 'bar)
(indirect-variable 'foo)
     ⇒ bar
(indirect-variable 'bar)
     ⇒ bar
(setq bar 2)
bar
     ⇒ 2

foo
     ⇒ 2

(setq foo 0)
bar
     ⇒ 0
foo
     ⇒ 0
```

### 12.16 Variables with Restricted Values ###

具有限制值的变量

普通的Lisp变量可以被赋任何有效的Lisp对象值。然而，某些Lisp变量不是在Lisp中定义的，而是在C中定义的。大多数这些变量是使用 `DEFVAR_LISP` 在C代码中定义的。与Lisp中定义的变量一样，这些变量可以取任意值。但是，有些变量是使用 `DEFVAR_INT` 或 `DEFVAR_BOOL` 定义的。有关C实现的简要讨论，请参阅编写Emacs原语 [Writing Emacs Primitives](https://www.gnu.org/software/emacs/manual/html_node/elisp/Writing-Emacs-Primitives.html#Defining-Lisp-variables-in-C)，特别是对 `syms_of_filename` 类型函数的描述。

`DEFVAR_BOOL` 类型的变量只能接受nil或t的值。尝试给它们赋任何其他值都会将它们设置为t:

``` Elisp
(let ((display-hourglass 5))
  display-hourglass)
     ⇒ t
```

#### 变量: `byte-boolean-vars` ####

该变量保存 `DEFVAR_BOOL` 类型的所有变量的列表。

`DEFVAR_INT` 类型的变量只能接受整数值。尝试给它们赋任何其他值将导致错误:

``` Elisp
(setq undo-limit 1000.0)
error→ Wrong type argument: integerp, 1000.0
```

### 12.17 Generalized Variables ###

*广义变量 generalized variable* 或 *位置形式 place form* 是Lisp内存中可以使用 `setf` 宏存储值的众多位置之一(参见setf宏 The setf Macro)。最简单的位置形式是一个普通的Lisp变量。但是 lists 的car和cdr、数组的元素、符号的属性以及许多其他位置也是存储Lisp值的地方。

广义变量类似于C语言中的左值，其中 `x = a[i]` 从数组中获取元素， `a[i] = x` 使用相同的表示法存储元素。就像在C语言中 `a[i]` 可以是左值一样，在Lisp中也有一组可以作为广义变量的形式。


* The setf Macro
* Defining new setf forms

#### 12.17.1 The setf Macro ####

`setf` 宏是操作广义变量的最基本方法。setf形式类似于setq，不同之处在于它接受每对参数的第一个(左)参数中的任意位置形式，而不仅仅是符号。

例如，`(setf (car a) b)` 将a的car设置为b，执行与 `(setcar a b)` 相同的操作，但不必使用两个单独的函数来设置和访问这种类型的位置。

##### 宏: `setf [place form]...` #####

该宏计算 form 并将其值存储在适当的位置，该值必须是有效的广义变量形式。如果有多个位置和形式对，则像使用setq一样按顺序完成赋值。setf返回最后一个表单的值。

下面的Lisp形式是Emacs中作为广义变量的形式，因此可能出现在setf的place参数中:

* 一个 symbol。换句话说，`(setf x y)` 完全等价于 `(setq x y)`，并且在setf存在的情况下，setq本身严格来说是冗余的。但是，由于风格和历史原因，大多数程序员将继续使用setq来设置简单的变量。宏(setf x y)实际上展开为(setq x y)，因此在编译后的代码中使用它没有性能损失。
* 对下列任何标准Lisp函数的调用:

``` Elisp
aref      cddr      symbol-function
car       elt       symbol-plist
caar      get       symbol-value
cadr      gethash
cdr       nth
cdar      nthcdr
```

* 对以下emacs特定函数的调用:

``` Elisp
alist-get                     overlay-start
default-value                 overlay-get
face-background               process-buffer
face-font                     process-filter
face-foreground               process-get
face-stipple                  process-sentinel
face-underline-p              terminal-parameter
file-modes                    window-buffer
frame-parameter               window-dedicated-p
frame-parameters              window-display-table
get-register                  window-hscroll
getenv                        window-parameter
keymap-parent                 window-point
match-data                    window-start
overlay-end
```

调用形式为 `(substring subplace n [m])`，其中 `subplace`本身是一个有效的广义变量，其当前值是字符串，并且存储的值也是字符串。将新字符串拼接到目标字符串的指定部分中。例如:

``` Elisp
(setq a (list "hello" "world"))
     ⇒ ("hello" "world")
(cadr a)
     ⇒ "world"
(substring (cadr a) 2 4)
     ⇒ "rl"
(setf (substring (cadr a) 2 4) "o")
     ⇒ "o"
(cadr a)
     ⇒ "wood"
a
     ⇒ ("hello" "wood")
```

* if 和 cond 将作为广义变量。例如，这会将foo或bar变量设置为zot:

``` Elisp
(setf (if (zerop (random 2))
	  foo
	bar)
      'zot)
```

如果传递的位置表单不知道如何处理，setf就会发出错误信号。

注意，对于 `nthcdr`，函数的list参数本身必须是有效的位形式。例如， `(setf (nthcdr 0 foo) 7)` 会将foo本身设置为7。

宏`push`(参见修改列表变量 Modifying List Variables)和`pop`(参见访问列表元素 Accessing Elements of Lists)可以操作广义变量，而不仅仅是列表。`(pop place)` 删除并返回存储在原地的列表的第一个元素。它类似于 `(prog1 (car place) (setf place (cdr place)))`，不同之处在于它只对所有子表单求值一次。`(push x place)` 将x插入到存储在位置的列表的前面。它类似于 `(setf place (cons x place))`，只是对子形式求值不同。请注意，`ncdr`位置上的push和pop可用于在列表中的任何位置插入或删除。

cl-lib库为通用变量定义了各种扩展，包括附加的集合位置。参见公共Lisp扩展中的广义变量。

#### 12.17.2 Defining new setf forms ####

本节描述如何定义setf可以操作的新 form。

##### 宏: `gv-define-simple-setter name setter &optional fix-return` #####

这个宏使您能够轻松地为简单的情况定义 `setf` 方法。`name`是函数、宏或特殊形式的名称。只要name有一个直接对应的setter函数来更新它，你就可以使用这个宏，例如 `(gv-define-simple-setter car setcar)`。

这个宏将一个形式的调用

``` Elisp
(setf (name args...) value)
```

转换为

``` Elisp
(setter args... value)
```

这样的setf调用被记录为返回值。例如，对于car和setcar，这是没有问题的，因为setcar返回它所设置的值。如果你的setter函数不返回值，使用一个非nil值作为 `gv-define-simple-setter` 的 `fix-return` 参数。这个展开后等于

``` Elisp
(let ((temp value))
  (setter args… temp)
  temp)
```

确保它返回正确的结果。

##### 宏: `gv-define-setter name arglist &rest body` #####

这个宏允许比以前的形式更复杂的集合展开。您可能需要使用这种形式，例如，如果没有简单的setter函数要调用，或者如果有一个，但它需要不同的参数给place形式。

这个宏扩展了形式 `(setf (name args...) value)`，首先根据arglist绑定setf参数形式 `(value args...)`，然后执行主体。body应该返回一个执行赋值的Lisp表单，并最终返回所设置的值。使用这个宏的一个例子是:

``` Elisp
(gv-define-setter caar (val x) `(setcar (car ,x) ,val))
```

##### 宏: `gv-define-expander name handler` #####

要对展开进行更多控制，可以使用 `gv-define-expander` 宏。例如，一个可设置的子字符串可以这样实现:

``` Elisp
(gv-define-expander substring
  (lambda (do place from &optional to)
    (gv-letplace (getter setter) place
      (macroexp-let2* (from to)
        (funcall do `(substring ,getter ,from ,to)
                 (lambda (v)
                   (macroexp-let2* (v)
                     `(progn
                        ,(funcall setter `(cl--set-substring
                                           ,getter ,from ,to ,v))
                        ,v))))))))
```

##### 宏: `gv-letplace (getter setter) place &rest body` #####

宏 `gv-letplace` 在定义与setf类似的宏时非常有用;例如，Common Lisp的incf宏可以这样实现:

``` Elisp
(defmacro incf (place &optional n)
  (gv-letplace (getter setter) place
    (macroexp-let2* ((v (or n 1)))
      (funcall setter `(+ ,v ,getter)))))
```

getter将绑定到一个可复制的表达式，该表达式返回place的值。setter将绑定到一个函数，该函数接受一个表达式v并返回一个将place设置为v的新表达式。body应该通过getter和setter返回一个Emacs Lisp表达式操作place。

参考源文件 `gv.el` 以了解更多细节

##### 函数: `make-obsolete-generalized-variable obsolete-name current-name when` #####

此函数使字节编译器警告通用变量obsolete-name已过时。如果current-name是一个符号，那么警告消息会说使用current-name而不是obsolete-name。如果current-name是一个字符串，这就是消息。When应该是一个字符串，表示变量第一次被废弃的时间(通常是一个版本号字符串)。

> Common Lisp 注意:公共Lisp定义了另一种方法来指定函数的setf行为，即setf函数，其名称是列表(setf name)而不是符号。例如，`(defun (setf foo)...)` 定义了当setf应用于foo时使用的函数。Emacs不支持这个。在尚未定义适当展开的窗体上使用setf会导致编译时错误。在Common Lisp中，这不是错误，因为函数(setf func)可能会在以后定义。

### 12.18 Multisession Variables ###

当您将一个变量设置为一个值，然后关闭Emacs并重新启动它时，该值不会自动恢复。用户通常在他们的启动文件中设置普通变量，或者使用自定义(参见自定义设置)来永久设置用户选项，并且不同的包有不同的文件来存储数据(例如，Gnus将其存储在 `.newsrc` 中)。字段和URL库将cookie存储在 `~/.emacs.d/url/cookies`)中。

对于介于这两个极端之间的东西(例如，配置放在启动文件中，大量应用程序状态放在单独的文件中)，Emacs提供了一种工具来在会话之间复制数据，称为多会话变量 multisession variables。(此功能可能不适用于所有系统。)为了让你了解这些词是如何使用的，这里有一个小例子:

``` Elisp
(define-multisession-variable foo 0)
(defun my-adder (num)
  (interactive "nAdd number: ")
  (setf (multisession-value foo)
        (+ (multisession-value foo) num))
  (message "The new number is: %s" (multisession-value foo)))
```

这将定义变量foo并将其绑定到一个特殊的multisession对象，该对象初始化为值 0 (如果该变量在前一个会话中不存在)。`my-adder` 命令向用户查询一个数字，将其添加到旧的(可能保存的值)，然后保存新值。

此功能并不意味着用于庞大的数据结构，但对于大多数值应该是高性能的。

#### 宏: `define-multisession-variable name initial-value &optional doc &rest args` ####

这个宏将name定义为一个多会话变量，如果这个变量之前没有被赋值，就给它一个初始值。Doc是Doc字符串，在args中可以使用几个关键字参数:

* `:package package-symbol` 该关键字表示多会话变量属于由package-symbol指定的包。包装符号和名称的组合必须是唯一的。如果未给出package-symbol，则默认为名称符号名称的第一个“段”，这是其名称的一部分，直到并不包括第一个' - '。例如，如果name为foo而没有给出package-symbol, package-symbol将默认为foo。
* `:synchronized bool` 如果bool为非空值，则可以同步多会话变量。这意味着，如果有两个并发的Emacs实例在运行，而另一个Emacs更改了多会话变量foo，那么当前的Emacs实例将在访问该值时检索修改后的数据。如果synchronized为nil或缺失，则不会发生这种情况，并且使用该变量的所有Emacs会话中的值将彼此独立。
* `:storage storage` 使用指定的存储方法。它可以是sqlite(在使用sqlite支持编译的Emacs中)或文件。如果没有给出，则默认为多会话存储变量的值，如下所述。

#### 函数: `multisession-value variable` ####

这个函数返回变量的当前值。如果这个变量在这个Emacs会话中以前没有被访问过，或者它在外部被修改过，那么它将从外部存储器中读入。如果不是，则按原样返回此会话中的当前值。对于非多会话变量调用此函数是错误的。

通过multisession-value检索到的值可能彼此相等，也可能不相等，但它们总是相等的。

这是一个广义变量(参见广义变量)，所以更新这样一个变量的方法是这样说，例如:

``` Elisp
(setf (multisession-value foo-bar) 'zot)
```

只有具有可读打印语法的Emacs Lisp值才能以这种方式保存(请参阅打印表示和读取语法 Printed Representation and Read Syntax)。

如果multisession变量是同步的，设置它可能首先更新值。例如:

``` Elisp
(cl-incf (multisession-value foo-bar))
```

它首先检查值是否在不同的Emacs实例中发生了更改，检索该值，然后对该值添加1并存储它。但请注意，这是在没有锁定的情况下完成的，因此如果许多实例同时更新值，则无法预测哪个实例“获胜”。

#### 函数: `multisession-delete object` ####

这个函数从持久化存储中删除对象及其值。

#### 函数: `make-multisession` ####

``` Elisp
(setq foo (make-multisession :package "mail"
                             :key "friends"))
(setf (multisession-value foo) 'everybody)
```

它支持与define-multisession-variable相同的关键字，但也支持一个`:initial-value`关键字，该关键字指定默认值。

#### 用户选项: `multisession-storage` ####

该变量控制如何存储多会话变量。它的值默认为files，这意味着这些值存储在multisession-directory指定的目录中一个文件一个变量的结构中。如果该值为sqlite，则该值存储在sqlite数据库中;这只有在Emacs使用SQLite支持构建时才可用。

#### 用户选项: `multisession-directory` ####

multisession变量存储在这个目录下，默认是 user-emacs-directory 的 `multisession/` 子目录，通常是 `~/.emacs.d/multisession/`。

#### 命令: `list-multisession-values` ####

该命令弹出一个列出所有multisession变量的缓冲区，并进入一个特殊模式 `multisession-edit-mode`，允许您删除它们并编辑它们的值。

## 13 Functions ##

Lisp程序主要由Lisp函数组成。本章解释函数是什么，它们如何接受参数，以及如何定义它们。

* What Is a Function?
* Lambda Expressions
* Naming a Function
* Defining Functions
* Calling Functions
* Mapping Functions
* Anonymous Functions
* Generic Functions
* Accessing Function Cell Contents
* Closures
* Open Closures
* Advising Emacs Lisp Functions
* Declaring Functions Obsolete
* Inline Functions
* The declare Form
* Telling the Compiler that a Function is Defined
* Determining whether a Function is Safe to Call
* Other Topics Related to Functions

### 13.1 What Is a Function? ###

[What Is a Function?](https://www.gnu.org/software/emacs/manual/html_node/elisp/What-Is-a-Function.html)

在一般意义上，函数是在给定的输入值(称为参数 arguments)下执行计算的规则。计算的结果称为函数的值或返回值。计算也可能有副作用，例如变量值或数据结构内容的持续变化(参见副作用的定义 Definition of side effect)。*纯函数pure function* 是这样一种函数，它除了没有副作用之外，对于相同的参数组合总是返回相同的值，而不考虑诸如机器类型或系统状态之类的外部因素。

在大多数计算机语言中，每个函数都有一个名称。但是在Lisp中，严格意义上的函数是没有名称的:它是一个对象，可以选择性地与作为函数名的符号(例如，car)相关联。参见为函数命名 Naming a Function。当一个函数被赋予一个名称时，我们通常也把这个符号称为“函数”(例如，我们称 car 为函数 car)。在本手册中，函数名和函数对象本身之间的区别通常是不重要的，但我们将在相关的地方注意到它。

某些类似函数的对象，称为特殊形式和宏，也接受参数来执行计算。然而，正如下面所解释的，这些在Emacs Lisp中不被认为是函数。

下面是函数和类函数对象的重要术语:

* `lambda expression`
    用Lisp编写的函数(严格意义上，即函数对象)。这些将在下一节中进行描述。参见Lambda表达式 Lambda Expressions。
* `primitive`
    可从Lisp调用但实际上是用c编写的函数。原语也称为 *内置函数 build-in functions* 或 *子函数 subrs*。例子包括car和append这样的函数。此外，所有特殊形式(见下文)也被视为原语。
    通常，函数被实现为原语是因为它是Lisp的基础部分(例如，car)，或者因为它提供了操作系统服务的底层接口，或者因为它需要快速运行。与Lisp中定义的函数不同，只能通过更改C源代码和重新编译Emacs来修改或添加原语。参见编写Emacs原语 Writing Emacs Primitives。
* `special form`
    一种类似于函数的原语，但不以通常的方式计算其所有参数。它可能只计算一些参数，也可能以不寻常的顺序或多次计算它们。例子包括if, and和while。参见特殊形式 Special Forms。
* `macro`
    Lisp中定义的一种结构，它与函数的不同之处在于，它将Lisp表达式转换为另一个要计算的表达式，而不是原始表达式。宏使Lisp程序员能够做特殊形式可以做的事情。看到宏 Macros。
* `command`
    可以通过命令执行原语调用的对象，通常由于用户键入与该命令绑定的键序列而调用。参见交互式呼叫 Interactive Call。命令通常是一个函数;如果函数是用Lisp编写的，它会在函数定义中通过交互形式生成命令(参见定义命令)。作为函数的命令也可以从Lisp表达式中调用，就像其他函数一样。
    键盘宏(字符串和向量)也是命令，尽管它们不是函数。参见键盘宏 Keyboard Macros。如果一个符号的 *函数单元格 function cell* 包含一个命令，我们就说它是一个命令(参见符号组件 Symbol Components);这样的命名命令可以用 `M-x` 调用。
* `closure`
    与lambda表达式非常相似的函数对象，不同之处在于它还包含词法变量绑定的环境。看到闭包 Closures。
* `byte-code function`
    已被字节编译器编译的函数。请参见字节码功能类型 Byte-Code Function Type。
* `autoload object`
    实际函数的 *占位符 place-holder*。如果调用了autoload对象，Emacs将加载包含实际函数定义的文件，然后调用实际函数。看到自动装载 Autoload。

你可以使用函数 `functionp` 来测试一个对象是否是函数:

#### 函数: `functionp object` ####

如果对象是任何类型的函数，即可以传递给 `funcall`，则此函数返回t。注意，function对于作为函数名的符号返回t，对于作为宏或特殊形式的符号返回nil。

如果object不是一个函数，这个函数通常返回nil。然而，函数对象的表示是复杂的，并且出于效率原因，在极少数情况下，即使对象不是函数，该函数也可以返回t。

也可以找出任意函数需要多少个参数:

#### 函数: `func-arity function` ####

此函数提供有关指定函数的参数列表的信息。返回值是形式为 `(min . max)`，其中min是参数的最小数量，max是参数的最大数量，或者是带有 `&rest` 参数的函数的符号 `many`，或者是函数是特殊形式时的符号 `unevalled`。

请注意，在某些情况下，此函数可能会返回不准确的结果，例如:

* 使用 `apply-partially` 定义的函数(参见部分应用 apply-partially)。
* 使用 `advice-add` 建议的函数(参见建议命名函数 Advising Named Functions)。
* 动态确定参数列表的函数，作为其代码的一部分。

与functionp不同，接下来的三个函数不将符号视为其函数定义。

#### 函数: `subrp object` ####

如果对象是内置函数(即Lisp原语)，则此函数返回t。

``` Elisp
(subrp 'message)            ; message is a symbol,
     ⇒ nil                 ;   not a subr object.

(subrp (symbol-function 'message))
     ⇒ t
```

#### 函数: `byte-code-function-p object` ####

如果object是字节码函数，则此函数返回t。例如:

``` Elisp
(byte-code-function-p (symbol-function 'next-line))
     ⇒ t
```

#### 函数: `compiled-function-p object` ####

如果object是一个不是ELisp源代码形式的函数对象，而是类似于机器码或字节码的形式，则此函数返回t。更具体地说，如果函数是内置的(又名“原始的”，参见什么是函数? What Is a Function?)，或者是字节编译的(参见字节编译 Byte Compilation)，或者是本地编译的natively-compiled(参见Lisp编译为本地代码 Compilation of Lisp to Native Code)，或者是从动态模块加载的函数(参见Emacs动态模块 Emacs Dynamic Modules)，它返回t。

#### 函数: `subr-arity subr` ####

这类似于 `func-arity`，但仅适用于内置函数并且没有符号间接 symbol indirection。它对非内置函数发出错误信号。我们建议使用 `func-arity` 代替。

### 13.2 Lambda Expressions ###

lambda表达式是用Lisp编写的函数对象。下面是一个例子:

``` Elisp
(lambda (x)
  "Return the hyperbolic cosine of X."
  (* 0.5 (+ (exp x) (exp (- x)))))
```

在Emacs Lisp中，这样的列表是一个有效表达式，其计算结果为函数对象。

lambda表达式本身没有名称;它是一个 *匿名函数 anonymous function*。虽然lambda表达式可以以这种方式使用(参见匿名函数 Anonymous Functions)，但它们更常与符号关联以创建命名函数(参见命名函数 Naming a Function)。在讨论这些细节之前，下面的小节描述lambda表达式的组件及其功能。

* Components of a Lambda Expression
* A Simple Lambda Expression Example
* Features of Argument Lists
* Documentation Strings of Functions

#### 13.2.1 Components of a Lambda Expression ####

lambda表达式是一个看起来像这样的列表:

``` Elisp
(lambda (arg-variables...)
  [documentation-string]
  [interactive-declaration]
  body-forms...)
```

lambda表达式的第一个元素总是符号`lambda`。这表明该列表表示一个函数。函数被定义为以lambda开头的原因是，用于其他用途的其他列表不会意外地作为函数有效。

第二个元素是符号列表——参数变量名(参见参数列表的特性 Features of Argument Lists)。这被称为 *lambda list*。当调用Lisp函数时，参数值将与lambda列表中的变量进行匹配，这些变量将与所提供的值进行局部绑定。参见局部变量 Local Variables。

文档字符串是一个放在函数定义中的Lisp字符串对象，用于描述Emacs帮助工具的函数。请参阅函数字符串文档 Documentation Strings of Functions。

交互式声明是表单(交互式代码字符串 interactive code-string)的列表。这声明了在交互使用函数时如何提供参数。带有此声明的函数称为命令 commands;它们可以使用 `M-x` 或绑定到一个键来调用。不打算以这种方式调用的函数不应该具有交互式声明。有关如何编写交互式声明，请参阅定义命令 Defining Commands。

其余的元素是函数的主体:完成函数工作的Lisp代码(或者，正如Lisp程序员所说，“要计算的Lisp forms 列表”)。函数返回的值是函数体最后一个元素返回的值。

#### 13.2.2 A Simple Lambda Expression Example ####

考虑下面的例子:

``` Elisp
(lambda (a b c) (+ a b c))
```

我们可以通过将其传递给 `funcall` 来调用这个函数，像这样:

``` Elisp
(funcall (lambda (a b c) (+ a b c))
         1 2 3)
;  6
```

这个调用用变量a绑定到1,b绑定到2,c绑定到3来计算lambda表达式的主体。对主体求值，将这三个数相加，结果为6;因此，对该函数的调用返回值6。

注意，参数可以是其他函数调用的结果，如下例所示:

``` Elisp
(funcall (lambda (a b c) (+ a b c))
         1 (* 2 3) (- 5 4))
;  8
```

这将从左到右计算参数1，`(* 2 3)` 和 `(- 5 4)`。然后，它将lambda表达式应用于参数值1,6和1，以产生值8。

正如这些示例所示，您可以使用带有lambda表达式作为CAR的表单来创建局部变量并为其赋值。在Lisp的早期，这种技术是绑定和初始化局部变量的唯一方法。但是现在，为了达到这个目的，使用特殊形式let(参见局部变量)更加清晰。Lambda表达式主要用作匿名函数，作为参数传递给其他函数(参见匿名函数 Anonymous Functions)，或者作为符号函数定义存储以产生命名函数(参见命名函数 Naming a Function)。

#### 13.2.3 Features of Argument Lists ####

我们的简单示例函数 `(lambda (a b c) (+ a b c))` 指定了三个参数变量，因此必须使用三个参数来调用它:如果您试图仅使用两个或四个参数来调用它，则会得到错误的参数数量错误 wrong-number-of-arguments error(请参阅错误 Errors)。

编写允许省略某些参数的函数通常很方便。例如，函数`substring`接受三个参数—字符串、开始索引和结束索引—但是如果省略它，第三个参数默认为字符串的长度。对于某些函数来说，接受不定数量的参数也很方便，就像函数list和+所做的那样。

要指定在调用函数时可以省略的可选参数，只需在可选参数之前包含关键字 `&optional`。要指定一个包含零个或多个额外参数的列表，请在最后一个参数之前包含关键字 `&rest`。

因此，参数列表的完整语法如下:

``` Elisp
(required-vars…
 [&optional [optional-vars…]]
 [&rest rest-var])
```

方括号表示 `&optional` 和 `&rest` 子句以及它们后面的变量是可选的。

对函数的调用需要为每个必需的变量提供一个实际参数。可以有0个或多个可选变量的实际参数，除非lambda列表使用&rest，否则不能有任何实际参数。在这种情况下，可能有任意数量的额外实际参数。

如果省略了可选变量和rest变量的实际参数，则它们总是默认为`nil`。函数无法区分nil的显式参数和省略的参数。然而，函数体可以自由地认为nil是其他一些有意义值的缩写。这就是substring所做的;nil作为substring的第三个参数意味着使用所提供的字符串长度。

> Common Lisp注意: Common Lisp允许函数指定在省略可选参数时使用的默认值;Emacs Lisp总是使用`nil`。Emacs Lisp不支持告诉您参数是否显式传递的提供变量。

例如，一个参数列表看起来像这样:

``` Elisp
(a b &optional c d &rest e)
```

将a和b绑定到前两个实际参数，这是必需的。如果提供了一个或两个以上的参数，则c和d分别绑定到它们;在前四个参数之后的任何参数都被收集到一个列表中，并且e被绑定到该列表中。因此，如果只有两个参数，则c, d和e为nil;如果有两个或三个参数，d和e为nil;如果参数不超过4个，则e为nil。请注意，为e提供的带有显式nil参数的五个参数将导致该nil参数作为包含一个元素 `(nil)` 的列表传递，就像e的任何其他单个值一样。

没有办法在可选参数后面加上必需参数——这是没有意义的。要了解为什么必须如此，假设示例中的c是可选的，而d是必需的。假设给出了三个实际的参数;第三个参数是哪个变量?是用在c上，还是d上?两种可能性都有。类似地，在 `&rest` 参数之后有更多的参数(无论是必需的还是可选的)是没有意义的。

下面是一些参数列表和正确调用的例子:

``` Elisp
(funcall (lambda (n) (1+ n))        ; One required:
         1)                         ; requires exactly one argument.
     ⇒ 2
(funcall (lambda (n &optional n1)   ; One required and one optional:
           (if n1 (+ n n1) (1+ n))) ; 1 or 2 arguments.
         1 2)
     ⇒ 3
(funcall (lambda (n &rest ns)       ; One required and one rest:
           (+ n (apply '+ ns)))     ; 1 or more arguments.
         1 2 3 4 5)
     ⇒ 15
```

> `(apply FUNCTION &rest ARGUMENTS)`
> Call FUNCTION with our remaining args, using our last arg as list of args.
> Then return the value FUNCTION returns.
> With a single argument, call the argument’s first element using the
> other elements as args.
> Thus, `(apply '+ 1 2 '(3 4))` returns 10.

#### 13.2.4 Documentation Strings of Functions ####

lambda表达式可以选择在lambda列表后面有一个文档字符串。这个字符串不影响函数的执行;它是一种注释，但是是一种系统化的注释，它实际上出现在Lisp世界中，并且可以由Emacs帮助工具使用。请参阅文档 Documentation，了解如何访问文档字符串。

为程序中的所有函数提供文档字符串是一个好主意，即使是那些只从程序内部调用的函数。文档字符串类似于注释，只是它们更容易访问。

**文档字符串的第一行** 应该独立存在，因为适当地只显示这第一行。它应该由一个或两个完整的句子组成，总结函数的目的。

文档字符串的开头通常在源文件中缩进，但由于这些空格位于开始双引号之前，因此它们不是字符串的一部分。有些人习惯缩进字符串的任何附加行，以便文本在程序源中对齐。这是一个错误。以下行的缩进在字符串内;在源代码中看起来不错的东西，在帮助命令中显示出来时就会显得很难看。

您可能想知道为什么文档字符串是可选的，因为它后面有函数的必需组件(主体)。由于对字符串求值返回该字符串，没有任何副作用，因此如果它不是主体中的最后一个形式，则没有任何影响。因此，在实践中，主体的第一种形式和文档字符串之间没有混淆;如果唯一的主体形式是字符串，那么它既可以作为返回值，也可以作为文档。

文档字符串的最后一行可以指定与实际函数参数不同的 **调用约定 calling conventions**。像这样写文本:

``` Elisp

\(fn arglist)

```

在空白行之后，在行开头，在文档字符串中没有换行符。( `\` 是为了避免混淆Emacs运动命令。 Emacs motion commands)以这种方式指定的调用约定出现在帮助消息中，而不是从函数的实际参数派生的调用约定。

这个特性对于宏定义特别有用，因为在宏定义中编写的参数通常与用户认为的宏调用部分的方式不一致。

如果您希望弃用调用约定而支持上述规范所宣传的约定，则不要使用此特性。相反，请使用 *公告调用约定 advertised-calling-convention* 声明(参见声明形式 The declare Form)或 *设置公告调用约定 set-advertised-calling-convention*(参见声明函数过时 Declaring Functions Obsolete)，因为这两个将导致字节编译器在编译使用废弃的调用约定的Lisp程序时发出警告消息。

`(fn)` 特性通常用于以下情况:

* 在宏或函数中详细说明参数及其用途。例子:

``` Elisp
(defmacro lambda (&rest cdr)
  "...
\(fn ARGS [DOCSTRING] [INTERACTIVE] BODY)"...)
```

* 提供更详细的描述和参数名称。例子:

``` Elisp
(defmacro macroexp--accumulate (var+list &rest body)
  "…
\(fn (VAR LIST) BODY…)"
  (declare (indent 1))
  (let ((var (car var+list))
	  (list (cadr var+list))
…)))
```

为了更好地解释 `defalias` 的目的。例子:

``` Elisp
(defalias 'abbrev-get 'get
  "…
\(fn ABBREV PROP)")
```

文档字符串通常是静态的，但有时也需要动态地生成它们。在某些情况下，您可以通过编写一个宏来实现这一点，该宏在编译时生成函数的代码，包括所需的文档字符串。但是您也可以通过编写 `(:documentation form)` 而不是文档字符串来动态地生成文档字符串。这将在定义函数时在运行时计算form，并将其用作文档string。还可以在请求时动态地计算文档字符串，方法是将函数符号的 *function-documentation* 属性设置为Lisp形式，计算结果为字符串(这只适用于使用词法绑定的代码。)。

例如:

``` Elisp
(defun adder (x)
  (lambda (y)
    (:documentation (format "Add %S to the argument Y." x))
    (+ x y)))
(defalias 'adder5 (adder 5))
(documentation 'adder5)
    ⇒ "Add 5 to the argument Y."


(put 'adder5 'function-documentation
     '(concat (documentation (symbol-function 'adder5) 'raw)
              "  Consulted at " (format-time-string "%H:%M:%S")))
(documentation 'adder5)
    ⇒ "Add 5 to the argument Y.  Consulted at 15:52:13"
(documentation 'adder5)
    ⇒ "Add 5 to the argument Y.  Consulted at 15:52:18"
```

> 格式化时间字符串
> `(format-time-string FORMAT-STRING &optional TIME ZONE)`

### 13.3 Naming a Function ###

符号可以作为函数的名称。当符号的 *函数单元 function cell*(参见符号组件 Symbol Components)包含函数对象(例如lambda表达式)时，就会发生这种情况。然后符号本身成为一个有效的、可调用的函数，相当于函数单元格中的函数对象。

函数单元格的内容也称为符号的函数定义 *function definition*。用符号的函数定义代替符号的过程称为 *符号间接函数 symbol function indirection*;参见符号函数间接 Symbol Function Indirection。如果你没有给一个符号一个函数定义，它的函数单元被认为是空的 void，它不能作为一个函数使用。

实际上，几乎所有函数都有名称，并通过它们的名称来引用。您可以通过定义lambda表达式并将其放入函数单元中来创建一个命名的Lisp函数(请参阅访问函数单元内容 Accessing Function Cell Contents)。然而，更常见的是使用defun宏，这将在下一节中描述。参见定义函数 Defining Functions。

我们给函数命名是因为在Lisp表达式中通过函数名来引用它们很方便。而且，命名Lisp函数可以很容易地引用自己——它可以是递归的 recursive。此外，原语只能通过其名称在文本中引用，因为原语函数对象(参见原语函数类型)没有读语法。

函数不需要有唯一的名称。给定的函数对象通常只出现在一个符号的函数单元格中，但这只是一种惯例。使用fset很容易将其存储在几个符号中;然后，每个符号都是同一函数的有效名称。

注意，用作函数名的符号也可以用作变量;符号的这两种用法是独立的，并不冲突。(在Lisp的一些方言中不是这样，比如Scheme。)

按照惯例，如果一个函数的符号由两个以 `——` 分隔的名称组成，则该函数打算用于内部使用，并且第一部分命名定义该函数的文件。例如，名为 `vc-git——rev-parse` 的函数是在 `vc-git.el` 中定义的内部函数。用C编写的内部使用函数的名称以 `-internal` 结尾，例如，`bury-buffer-internal`。2018年之前贡献的Emacs代码可能会遵循其他内部使用的命名约定，这些约定正在逐步淘汰。

### 13.4 Defining Functions ###

我们通常在函数第一次创建时给它起一个名字。这被称为 *定义函数 defining a function*，我们通常使用`defun`宏来完成。本节还描述了定义函数的其他方法。

#### 宏: `defun name args [doc] [declare] [interactive] body...` ####

`defun` 是定义新的Lisp函数的常用方法。它将符号 `name` 定义为具有参数列表 `args`(参见参数列表的特性 Features of Argument Lists)和 `body` 给出的体形式的函数。名字和参数都不应该加引号。

`doc`，如果存在，应该是一个字符串，指定函数的文档字符串(参见函数的文档字符串 Documentation Strings of Functions)。

`declare`，如果存在的话，应该是一个指定函数元数据的声明表单(参见声明表单)。

`interactive`，如果存在的话，应该是一个交互式表单，指定如何交互式地调用函数(参见交互式调用 Interactive Call)。

defun的返回值未定义。

下面是一些例子:

``` Elisp
(defun foo () 5)
(foo)
     ⇒ 5


(defun bar (a &optional b &rest c)
    (list a b c))
(bar 1 2 3 4 5)
     ⇒ (1 2 (3 4 5))

(bar 1)
     ⇒ (1 nil nil)

(bar)
error→ Wrong number of arguments.


(defun capitalize-backwards ()
  "Upcase the last letter of the word at point."
  (interactive)
  (backward-word 1)
  (forward-word 1)
  (backward-char 1)
  (capitalize-word 1))
```

大多数Emacs函数都是Lisp程序源代码的一部分，并且在Emacs Lisp阅读器在执行程序源代码之前读取程序源代码时定义。然而，你也可以在运行时动态定义函数，例如，通过在程序代码执行时生成defun调用。如果您这样做，请注意，Emacs的帮助命令(例如 `C-h-f`)可能无法找到源代码，因为动态生成函数通常看起来与通常的静态调用defun非常不同，它在 `*Help*` 缓冲区中提供了一个跳转到函数定义的按钮。您可以通过使用 `definition-name`属性来简化查找生成此类函数的代码的工作，请参见标准符号属性 Standard Symbol Properties。

注意不要在无意中重新定义现有的函数。`defun`甚至可以在没有任何犹豫或通知的情况下重新定义像car这样的原始函数。Emacs并不阻止您这样做，因为重新定义函数有时是故意的，并且没有办法区分有意的重新定义和无意的重新定义。

#### 函数: `defalias name definition &optional doc` ####

这个函数将符号 `name` 定义为一个函数，带有定义 `definition`。定义可以是任何有效的Lisp函数或宏，也可以是特殊形式(参见特殊形式)，也可以是键映射(参见键映射 Keymaps)，也可以是向量或字符串(键盘宏)。defalias的返回值是undefined。

如果doc非nil，它将成为name的函数文档。否则，将使用定义提供的任何文档。

在内部，defalias通常使用fset来设置定义。但是，如果name具有 `defalias-fset-function`属性，则使用关联值作为要调用的函数来代替fset。

使用默认值的适当位置是定义特定函数或宏名称的地方—特别是该名称显式出现在加载的源文件中的地方。这是因为defalias记录了定义函数的文件，就像defun一样(参见卸载 Unloading)。

相比之下，在为其他目的操作函数定义的程序中，最好使用不保留此类记录的fset。参见访问功能单元内容 Accessing Function Call Contents。

#### 函数: `function-alias-p object &optional noerror` ####

检查object是否为函数别名。如果是，则返回一个表示函数别名链的符号列表，否则返回nil。例如，如果a是b的别名，b是c的别名:

``` Elisp
(function-alias-p 'a)
    ⇒ (b c)
```

如果定义中存在循环，则会发出错误信号。如果noerror为非nil，则返回链的非循环部分。

您不能使用defun或defalias创建新的原语函数，但是您可以使用它们来更改任何符号的函数定义，甚至像 car 或 `x-popup-menu` 这样的正常定义是原语的符号。然而，这是有风险的:例如，在不完全破坏Lisp的情况下重新定义car几乎是不可能的。重新定义像 `x-popup-menu`这样晦涩的函数不那么危险，但它仍然可能不像您期望的那样工作。如果从C代码中调用原语，它们会直接调用原语的C定义，因此更改符号的定义不会对它们产生影响。

另请参见 `defsubst`，它定义了一个类似defun的函数，并告诉Lisp编译器对其执行内联展开。参见内联函数 Inline Functions。

要取消函数名的定义，请使用 `fmakunbound`。参见访问功能单元内容 Accessing Function Cell Contents。

### 13.5 Calling Functions ###

定义函数只是战斗的一半。函数在你调用它们之前不会做任何事情，也就是说，告诉它们运行。调用函数 calling a function 也称为调用 invocation。

调用函数最常用的方法是对列表求值。例如，计算列表 `(concat "a" "b")` 调用带有参数"a"和"b"的concat函数。有关评估的描述，请参阅评估 Evaluation。

当你在程序中将列表写成表达式时，你要在程序文本中指定调用哪个函数，以及给它提供多少参数。通常这正是你想要的。有时您需要在运行时计算要调用哪个函数。要做到这一点，请使用函数`funcall`。当您还需要在运行时确定要传递多少参数时，请使用`apply`。

#### 函数: `funcall function &rest arguments` ####

funcall调用带参数的函数，并返回函数返回的任何值。

由于funcall是一个函数，它的所有参数(包括function)都在调用funcall之前求值。这意味着您可以使用任何表达式来获得要调用的函数。这也意味着funcall看不到你为参数写的表达式，只看到它们的值。这些值在调用函数时不会被求值第二次;funcall的操作类似于调用函数的正常过程，一旦它的参数已经被求值。

实参函数必须是Lisp函数或基本函数。不允许使用特殊的形式和宏，因为它们只有在给出未求值的参数表达式时才有意义。funcall不能提供这些，因为，正如我们上面看到的，它从一开始就不知道这些。

如果您需要使用funcall来调用命令并使其表现为交互式调用，请使用 `funcall-interactively`(参见交互式调用 Interactive Call)。

``` Elisp


(setq f 'list)
     ⇒ list

(funcall f 'x 'y 'z)
     ⇒ (x y z)

(funcall f 'x 'y '(z))
     ⇒ (x y (z))

(funcall 'and t nil)
error→ Invalid function: #<subr and>

```

将这些例子与 apply 程序的例子进行比较。

#### 函数: `apply function &rest arguments` ####

`apply` 调用带参数的函数，就像`funcall`一样，但有一点不同: **最后一个参数是一个对象列表**，这些对象作为单独的参数传递给函数，而不是一个单独的列表。我们说apply *扩展 spreads* 这个列表，以便每个单独的元素都成为一个参数。

使用单个参数的apply是特殊的:参数的第一个元素(必须是一个非空列表)作为函数调用，其余元素作为单独的参数。传递两个或更多参数会更快。

apply返回调用函数的结果。与funcall一样，function必须要么是Lisp函数，要么是原始函数;特殊的表单和宏在apply中没有意义。

``` Elisp
(setq f 'list)
     ⇒ list

(apply f 'x 'y 'z)
error→ Wrong type argument: listp, z

(apply '+ 1 2 '(3 4))
     ⇒ 10

(apply '+ '(1 2 3 4))
     ⇒ 10


(apply 'append '((a b c) nil (x y z) nil))
     ⇒ (a b c x y z)


(apply '(+ 3 4))
     ⇒ 7
```

有关使用apply的有趣示例，请参见`mapcar`的定义。

有时将函数的一些参数固定在某些值是有用的，而将其他参数留到实际调用函数时使用。*固定 fixing* 函数的某些参数的行为称为 *函数的部分应用 partial application of the function*。结果是一个新函数，它接受其余参数并调用合并了所有参数的原始函数。

> 这与柯里化相关 currying，但不同于柯里化，柯里化将一个接受多个参数的函数转换为可以作为一个函数链调用的方式，每个函数都有一个参数。

下面是如何在Emacs Lisp中实现部分应用程序:

#### 函数: `apply-partially func &rest args` ####

此函数返回一个新函数，当调用该函数时，将使用由args和调用时指定的附加参数组成的参数列表调用func。如果func接受 n 个参数，那么使用 `m <= n` 个参数调用 `apply-partially` 将产生一个包含 `n - m` 个参数的新函数。

> 如果函数可以接受的参数数量是无限的，那么新函数也将接受无限数量的参数，因此在这种情况下，apply-partially 不会减少新函数可以接受的参数数量。

下面是我们如何定义内置函数 `1+`，如果它不存在的话，使用 `apply-partially` 和 `+`，另一个内置函数:

> 请注意，与内置函数不同，此版本接受任意数量的参数。

``` Elisp
(defalias '1+ (apply-partially '+ 1)
  "Increment argument by one.")

(1+ 10)
     ⇒ 11
```

Lisp函数通常接受函数作为参数，或者在数据结构(尤其是钩子变量和属性列表)中找到它们，然后使用funcall或apply调用它们。接受函数参数的函数通常被称为 *函数式函数 functionals*。

有时，在调用函数时，提供无操作函数作为参数是很有用的。下面是三种不同的no-op函数:

#### 函数: `identity argument` ####

这个函数返回参数，没有副作用。

#### 函数: `ignore &rest arguments` ####

这个函数忽略任何参数并返回nil。

#### 函数: `always &rest arguments` ####

这个函数忽略任何参数并返回t。

有些函数是用户可见的命令，可以交互式地调用(通常通过一个键序列)。通过使用 `call-interactively` 函数，可以完全像交互式调用那样调用这样的命令。参见交互式调用 Interactive Call。

### 13.6 Mapping Functions ###

*映射函数 mapping function* 将给定的函数(不是特殊的形式或宏)应用于列表或其他集合的每个元素。Emacs Lisp有几个这样的函数;本节描述`mapcar`、`mapc`、`mapconcat`和`mapcan`，它们在 *列表 obarray* 上进行映射。

请参阅 `mapatoms` 的定义 Definition of mapatoms，了解mapatoms函数，该函数对 *数组 obarray* 中的符号进行映射。

请参阅`mapash`的定义，了解映射哈希表中的键/值关联的`mapash`函数。

这些映射函数 **不允许使用char-table**，因为char-table是一个稀疏数组，其标称索引范围非常大。要以适当处理其稀疏特性的方式映射到一个char-table上，请使用 `map-char-table` 函数(参见Char-Tables)。

#### 函数: `mapcar function sequence` ####

mapcar依次对序列中的每个元素应用函数，并返回结果列表。

参数序列可以是除 `char-table` 以外的任何类型的序列;也就是说，一个列表、一个向量、一个布尔向量或一个字符串。结果总是一个列表。结果的长度与序列的长度相同。例如:

``` Elisp
(mapcar #'car '((a b) (c d) (e f)))
     ⇒ (a c e)
(mapcar #'1+ [1 2 3])
     ⇒ (2 3 4)
(mapcar #'string "abc")
     ⇒ ("a" "b" "c")


;; Call each function in my-hooks.
(mapcar 'funcall my-hooks)


(defun mapcar* (function &rest args)
  "Apply FUNCTION to successive cars of all ARGS.
Return the list of results."
  ;; If no list is exhausted,
  (if (not (memq nil args))
      ;; apply function to CARs.
      (cons (apply function (mapcar #'car args))
            (apply #'mapcar* function
                   ;; Recurse for rest of elements.
                   (mapcar #'cdr args)))))


(mapcar* #'cons '(a b c) '(1 2 3 4))
     ⇒ ((a . 1) (b . 2) (c . 3))
```

> (memq ELT LIST)
> Return non-nil if ELT is an element of LIST.  Comparison done with ‘eq’.
> The value is actually the tail of LIST whose car is ELT.

#### 函数: `mapcan function sequence` ####

这个函数将function应用于序列的每个元素，就像mapcar一样，但是它不是将结果收集到一个列表中，而是通过 **修改结果**(使用`nconc`;参见重新排列列表的函数 Functions that Rearrange Lists)。与mapcar一样，sequence可以是除char-table以外的任何类型。

``` Elisp
;; Contrast this:
(mapcar #'list '(a b c d))
     ⇒ ((a) (b) (c) (d))
;; with this:
(mapcan #'list '(a b c d))
     ⇒ (a b c d)
```

#### 函数: `mapc function sequence` ####

`mapc` 类似于 `mapcar`，只不过函数 **只用于副作用**——它返回的值被 ignore，而不是收集到列表中。mapc总是返回 sequence。

#### 函数: `mapconcat function sequence &optional separator` ####

`mapconcat` 对 sequence 的每个元素应用函数;结果(必须是字符序列(字符串、向量或列表))被连接到单个字符串返回值中。在每对结果序列之间，mapconcat插入分隔符中的字符，分隔符也必须是字符串、矢量或字符列表;nil 值被视为空字符串。参见序列、数组和向量。

实参函数必须是一个可以接受一个实参并返回字符序列的函数:字符串、向量或列表。参数序列可以是除char-table以外的任何类型的序列;也就是说，一个列表、一个向量、一个布尔向量或一个字符串。

``` Elisp
(mapconcat #'symbol-name
           '(The cat in the hat)
           " ")
     ⇒ "The cat in the hat"


(mapconcat (lambda (x) (format "%c" (1+ x)))
           "HAL-8000")
     ⇒ "IBM.9111"
```

### 13.7 Anonymous Functions ###

虽然函数通常同时使用defun和给定名称来定义，但有时使用显式lambda表达式(匿名函数)也很方便。无论函数名在哪里，匿名函数都是有效的。它们通常被赋值为变量值，或者作为函数的参数;例如，您可以将一个作为函数参数传递给mapcar，它将该函数应用于列表的每个元素(参见映射函数)。有关这方面的一个实际例子，请参阅描述符号示例 describe-symbols example。

在定义要用作匿名函数的lambda表达式时，原则上可以使用任何方法来构造列表。但通常你应该使用`lambda`宏，或者 特殊形式 `function`，或者 `#'` 读语法

#### 宏: `lambda args [doc] [interactive] body...` ####

这个宏返回一个匿名函数，带有参数列表args、文档字符串doc(如果有的话)、交互式规范interactive(如果有的话)和body给出的主体形式。

在动态绑定下，这个宏有效地使lambda form 自引用 self-quoting: 计算CAR为lambda的表单将生成表单本身:

``` Elisp
(lambda (x) (* x x))
;  (lambda (x) (* x x))
```

注意，在词法绑定下求值时，结果是一个闭包对象(参见闭包 Closures)。

lambda形式还有另一个作用:它通过使用函数作为子例程 subroutine(见下文)，告诉Emacs求值器和字节编译器它的参数是一个函数。

#### 特殊形式: `function function-object` ####

这种特殊形式返回函数对象而不求值。在这一点上，它类似于 quote(参见引用 Quoting)。但与quote不同的是，它还可以作为Emacs求值器和字节编译器的一个提示，即 *function-object* 打算用作函数。假设 function-object 是一个有效的lambda表达式，它有两个效果:

1. 当代码被字节编译时，function-object被编译成字节码函数对象(参见字节编译 Byte Compilation)。
2. 启用词法绑定后，函数对象被转换为闭包。看到闭包 Closures。

当function-object是一个符号并且代码是字节编译时，如果该函数未定义或可能在运行时未知，字节编译器将发出警告。

#### 读语法: `#'` ####

read语法 `#'` 是 function 的简写。以下形式都是等价的:

``` Elisp
(lambda (x) (* x x))

(function (lambda (x) (* x x)))

#'(lambda (x) (* x x))
```

在下面的例子中，我们定义了一个 `change-property` 函数，它接受一个函数作为第三个参数，然后是一个 双属性 double-property 函数，通过传递一个匿名函数来利用change-property:

``` Elisp
defun change-property (symbol prop function)
  (let ((value (get symbol prop)))
    (put symbol prop (funcall function value))))


(defun double-property (symbol prop)
  (change-property symbol prop (lambda (x) (* 2 x))))

```

> (get SYMBOL PROPNAME)
> Return the value of SYMBOL’s PROPNAME property.
> This is the last value stored with ‘(put SYMBOL PROPNAME VALUE)’.

注意，我们没有引用lambda形式。

如果编译上面的代码，匿名函数也会被编译。如果你通过引用一个列表来构造匿名函数，就不会发生这种情况:

``` Elisp
(defun double-property (symbol prop)
  (change-property symbol prop '(lambda (x) (* 2 x))))
```

在这种情况下，匿名函数在编译后的代码中保留为lambda表达式。字节编译器不能假设这个列表是一个函数，尽管它看起来像一个函数，因为它不知道change-property打算将它作为函数使用。

### 13.8 Generic Functions ###

泛型函数

这一节的内容看不懂

[Generic Functions](https://www.gnu.org/software/emacs/manual/html_node/elisp/Generic-Functions.html)

使用defun定义的函数对其参数的类型和期望值有一组硬编码的假设。例如，设计用于处理其参数值(数字或数字列表)的函数，如果使用任何其他类型的值(如vector或string)调用，则会失败或发出错误信号。这是因为函数的实现没有准备好处理设计期间假定的类型以外的类型。

相比之下，面向对象程序使用 **多态函数 polymorphic**:一组具有相同名称的专门化函数，其中每个函数都是为特定的参数类型集编写的。实际调用哪个函数是在运行时根据实际参数的类型决定的。

Emacs提供了对多态性的支持。像其他Lisp环境一样，特别是Common Lisp及其 *Common Lisp Object System* (CLOS)，这种支持是基于 *泛型函数 generic functions* 的。Emacs泛型函数与CLOS密切相关，包括使用类似的名称，因此，如果您有使用CLOS的经验，本节的其余部分听起来会非常熟悉。

泛型函数通过定义其名称和参数列表来指定抽象操作，但(通常)没有实现。几个特定参数类的实际实现是由方法提供的，这些方法应该单独定义。实现泛型函数的每个方法都具有与泛型函数相同的名称，但是方法的定义指明了它可以通过专门化泛型函数定义的参数来处理哪些类型的参数。这些参数特化器可以或多或少地特定;例如，字符串类型比一般类型(如sequence)更具体。

请注意，与基于消息的 面对对象语言(如c++和Simula)不同，实现泛型函数的方法不属于类，它们属于它们实现的泛型函数。

当调用泛型函数时，它通过比较调用者传递的实际参数与每个方法的参数特化器来选择适用的方法。如果调用的实际参数与方法的专门化符兼容，则该方法适用。如果适用的方法不止一个，则使用下面描述的某些规则将它们组合起来，然后组合处理调用。

#### 宏: `cl-defgeneric name arguments [documentation] [options-and-methods…] &rest body` ####

这个宏定义了一个具有指定名称和参数的泛型函数。

如果存在body，则提供默认实现。如果存在文档(应该总是存在)，它以 `(:documentation docstring)` 的形式为泛型函数指定文档字符串。

可选的 `options-and-methods`可以是以下形式之一:

* `(declare declarations)`
    声明形式，如声明表单中所述 The declare Form。
* `(:argument-precedence-oreder &rest args)`
    此形式影响组合适用方法的排序顺序。通常，在组合过程中比较两个方法时，从左到右检查方法参数，参数特化器更具体的第一个方法将出现在另一个方法之前。由这种形式定义的顺序将覆盖该顺序，并且根据其在这种形式中的顺序检查参数，而不是从左到右。
* `(:method [qualifiers…] args &rest body)`
    这个形式定义了一个类似于 `cl-defmethod` 的方法。

#### 宏: `cl-defmethod name [extra] [qualifier] arguments [&context (expr spec)…] &rest [docstring] body` ####

这个宏定义了名为 `name` 的泛型函数的特定实现。实现代码由主体给出。如果存在，docstring是该方法的文档字符串。参数列表在实现泛型函数的所有方法中必须相同，并且必须与该函数的参数列表相匹配，它提供了 `(arg spec)` 形式的参数特化器 argument specializers，其中arg是在 `cl-defgeneric` 调用中指定的参数名称，spec是以下特化器形式之一:

* `type`
    此特化器要求参数为给定类型，即下面描述的类型层次结构中的类型之一。
* `(eql object)`
    这个特化器要求实参等于给定的对象。
* `(head object)`
    参数必须是一个 cons cell，它的 car 与对象相等。
* `struct-type`
    参数必须是一个用 `cl-defstruct` 定义的名为struct-type的类的实例(参见GNU Emacs Lisp的公共Lisp扩展中的结构)，或者是它的一个子类的实例。

方法定义可以使用一个新的参数列表关键字 `&context`，它引入了额外的专门化器，用于在方法运行时测试环境。这个关键字应该出现在必需参数列表之后，但在任何 `&rest` 或 `&optional` 关键字之前。`&context` 特化器看起来很像常规的参数特化器 `(expr spec)`，只不过 `expr` 是一个要在当前上下文中求值的表达式，而 `spec` 是一个要进行比较的值。例如，`&context (overwrite-mode (eql t))` 将使该方法仅在 `overwrite-mode` 打开时才适用。`&context` 关键字可以后跟任意数量的上下文特化器。因为上下文特化器不是泛型函数参数签名的一部分，所以在不需要它们的方法中可以省略它们。

类型专门化符 `(arg type)` 可以指定以下列表中的一种系统类型。当指定父类型时，其类型为其更具体的子类型以及孙子、孙子等的参数也将是兼容的。

* `integer`
    父类型: number
* `number`
* `null`
    父类型: symbol
* `symbol`
* `string`
    父类型: array
* `array`
    父类型: sequence
* `cons`
    父类型: list
* `list`
    父类型: sequence
* `marker`
* `overlay`
* `float`
    父类型: number
* `window-configuration`
* `process`
* `window`
* `subr`
* `compiled-function`
* `buffer`
* `char-table`
    父类型: array
* `bool-vector`
    父类型: array
* `vector`
    父类型: array
* `frame`
* `hash-table`
* `font-spec`
* `font-object`

可选的 `extra` 元素，表示为 `:extra string`，允许您为相同的特化器和限定符添加更多的方法，以字符串区分。

可选 *限定符 qualifier* 允许组合几个适用的方法。如果不存在，则定义的方法是主方法，负责为专门化参数提供泛型函数的主要实现。你也可以定义 *辅助方法 auxiliary methods*，通过使用以下值之一作为 *限定符 qualifier*:

* `:before`
    此辅助方法将在主方法之前运行。更准确地说，所有:before方法都将以最具体的优先顺序在primary方法之前运行。
* `:after`
    此辅助方法将在主方法之后运行。更准确地说，所有这些方法都将在主方法之后以最具体的最后一个顺序运行。
* `:around`
    这个辅助方法将代替主方法运行。其中最具体的方法将在任何其他方法之前运行。这些方法通常使用下面描述的cl-call-next-method来调用其他辅助方法或主方法。

使用 `cl-defmethod` 定义的函数 **不能** 通过添加交互式表单来实现交互，例如命令(参见定义命令 Defining Commands)。如果您需要一个多态命令，我们建议定义一个普通命令，该命令调用通过 `cl-defgeneric` 和 `cl-defmethod`定义的多态函数。

每次调用泛型函数时，它都会构建有效的方法，该方法将通过组合为该函数定义的适用方法来处理此调用。寻找适用的方法并产生有效方法的过程称为 *调度 dispatch*。适用的方法是那些其专门化符都与调用的实际参数兼容的方法。因为所有的参数都必须与专门化符兼容，所以它们都决定一个方法是否适用。显式专门化多个参数的方法称为 *多调度方法 multiple-dispatch methods*。

适用的方法按其组合的顺序进行排序。最左边的参数特殊化器是最具体参数的方法将按顺序排在前面。(如上所述，指定 `:argument-precedence-order` 作为 `cl-defmethod`的一部分将覆盖该顺序。)如果方法体调用 `cl-call-next-method`，则将运行下一个最特定的方法。如果有适用的 `:around` 方法，最具体的方法将首先运行;它应该调用 `cl-call-next-method` 来运行任何不太具体的 `:around` 方法。接下来是 `:before` 方法按照其特异性的顺序运行，其次是 `primary` 方法，最后是 `:after` 方法按照其特异性的相反顺序运行。

#### 函数: `cl-call-next-method &rest args` ####

当从 primary 或 `:around` 方法的 lexical body 中调用此函数时，为同一泛型函数调用下一个适用的方法。通常，调用它时不带参数，这意味着调用下一个适用的方法时使用调用方法时使用的相同参数。否则，将使用指定的参数。

#### 函数: `cl-next-method-p` ####

当从 primary 或 `:around` 方法的 lexical body 中调用此函数时，如果有下一个方法可调用，则返回非nil。

### 13.9 Accessing Function Cell Contents ###

符号的函数定义是存储在该符号的函数单元中的对象。这里描述的函数 access、test 和 set 符号的函数单元。

参见函数 `indirect-function` 。参见间接函数的定义 Definition of indirect-function。

#### 函数: `symbol-function symbol` ####

这将返回symbol函数单元格中的对象。它不检查返回的对象是否是一个合法的函数。

如果函数单元格为空，则返回值为nil。要区分一个函数单元格是void还是nil，可以使用`fboundp`(见下文)。

``` Elisp
(defun bar (n) (+ n 2))
(symbol-function 'bar)
     ⇒ (lambda (n) (+ n 2))

(fset 'baz 'bar)
     ⇒ bar

(symbol-function 'baz)
     ⇒ bar
```

如果你从来没有给一个符号任何函数定义，我们说这个符号的函数单元是空的。换句话说，函数单元格中没有任何Lisp对象。如果您尝试将符号作为函数调用，Emacs将发出 `void-function` 错误信号。

请注意，void 与nil或 symbol void不同。符号nil和void是Lisp对象，可以像其他对象一样存储在函数单元中(如果用defun定义void，它可以是一个有效的函数)。一个void函数单元不包含任何对象。

您可以使用 `fboundp` 测试符号函数定义的空白性。在给符号一个函数定义之后，可以使用 `fmakunbound` 再次使其无效。

#### 函数: `fboundp symbol` ####

如果符号的函数单元格中有对象，则返回t，否则返回nil。它不检查对象是否为合法函数。

#### 函数: `fmakunbound symbol` ####

此函数使symbol的函数单元格为空，因此后续访问该单元格的尝试将导致void-function错误。它返回符号。(请参见当变量为Void时的makunbound。 When a Variable is Void)

``` Elisp
(defun foo (x) x)
(foo 1)
     ⇒1

(fmakunbound 'foo)
     ⇒ foo

(foo 1)
error→ Symbol's function definition is void: foo
```

#### 函数: `fset symbol definition` ####

该函数将 definition 存储在符号的函数单元中。结果就是 definition。通常，definition 应该是一个函数或函数名，但这一点没有进行检查。参数符号是一个普通的求值参数。

这个函数的主要用途是作为定义或修改函数的构造的子例程 subroutine，如defun或 `advice-add` (参见建议Emacs Lisp函数 Advising Emacs Lisp Functions)。你也可以用它给一个符号一个非函数的函数定义，例如，一个键盘宏(参见键盘宏):

``` Elisp
;; Define a named keyboard macro.
(fset 'kill-two-lines "\^u2\^k")
     ⇒ "\^u2\^k"
```

如果您希望使用fset为函数创建一个替代名称，请考虑使用 `defalias`。参见定义别名 Definition of defalias。

### 13.10 Closures ###

[Closures](https://www.gnu.org/software/emacs/manual/html_node/elisp/Closures.html)

正如 *变量绑定的作用域规则 Scoping Rules for Variable Bindings* 中所解释的，Emacs可以选择性地启用变量的词法绑定。当启用词法绑定时，您创建的任何命名函数(例如，使用defun)，以及使用lambda宏或函数特殊形式或 `#'` 语法(请参阅匿名函数)创建的任何匿名函数，都会自动转换为闭包 closure。

闭包是一个函数，它还携带定义该函数时存在的词法环境的记录。当调用它时，其定义中的任何词法变量引用都使用保留的词法环境。在所有其他方面，闭包的行为与普通函数非常相似;特别是，它们可以以与普通函数相同的方式调用。

有关使用闭包的示例，请参阅词法绑定 Lexical Binding。

目前，Emacs Lisp闭包对象是由一个列表表示的，其中 符号闭包 closure 作为第一个元素，一个列表表示词法环境作为第二个元素，参数列表和主体形式作为其余元素:

``` Elisp
;; lexical binding is enabled.
(lambda (x) (* x x))
     ⇒ (closure (t) (x) (* x x))
```

然而，闭包的内部结构向Lisp世界的其他部分公开这一事实被认为是内部实现细节。由于这个原因，我们建议不要直接检查或修改闭包对象的结构。

### 13.11 Open Closures ###

传统上，函数是 *不透明的对象 opaque objects*，除了调用它们之外不提供其他功能。(Emacs Lisp函数并不是完全不透明的，因为您可以从中提取一些信息，比如它们的文档字符串、参数列表或交互规范，但它们大部分仍然是不透明的。)这通常是我们想要的，但偶尔我们需要函数公开更多关于它们自己的信息。

*开闭包 open closures*(简称 OClosures) 是函数对象，它携带额外的类型信息，并以 *插槽 slots* 的形式公开一些关于自身的信息，您可以通过 *访问器函数 accessor functions* 访问这些信息。

OClosures 的定义分两个步骤:

1. 首先使用 `oclosure-define` 通过指定该类型的 OClosure 所携带的槽来定义新的 OClosure 类型
2. 然后使用 `oclosure-lambda` 创建给定类型的开闭包对象。

假设我们想要定义键盘宏，即重新执行一系列键事件的交互函数(参见键盘宏 Keyboard Macros)。你可以用一个简单的函数来做，如下所示:

``` Elisp
(defun kbd-macro (key-sequence)
  (lambda (&optional arg)
    (interactive "P")
    (execute-kbd-macro key-sequence arg)))
```

但是有了这样的定义，就没有简单的方法从函数中提取键序列，例如打印它。

我们可以使用如下的开闭包来解决这个问题。首先，我们定义键盘宏的类型(我们决定在其中添加一个 计数器槽 counter):

``` Elisp
(oclosure-define kbd-macro
  "Keyboard macro."
  keys (counter :mutable t))
```

之后我们可以重写我们的 `kbd-macro` 函数:

``` Elisp
(defun kbd-macro (key-sequence)
  (oclosure-lambda (kbd-macro (keys key-sequence) (counter 0))
      (&optional arg)
    (interactive "P")
    (execute-kbd-macro keys arg)
    (setq counter (1+ counter))))
```

正如您所看到的，可以从闭包的主体内部作为局部变量访问闭包的键和计数器槽。但是我们现在也可以从闭包体外部访问它们，例如描述一个键盘宏:

``` Elisp
(defun describe-kbd-macro (km)
  (if (not (eq 'kbd-macro (oclosure-type km)))
      (message "Not a keyboard macro")
    (let ((keys    (kbd-macro--keys km))
          (counter (kbd-macro--counter km)))
      (message "Keys=%S, called %d times" keys counter))))
```

其中，`kbd-macro——keys` 和 `kbd-macro——counter` 是由宏 oclosure-define 为类型为 kbd-macro 的开闭包生成的访问器函数。

#### 宏: `oclosure-define oname &optional docstring &rest slots` ####

这个宏定义了一个新的 OClosure 类型及其槽的访问器函数。oname可以是一个符号(新类型的名称)，也可以是表单列表 `(oname . type-props)`，在这种情况下，`type-props` 是该开闭包类型的附加属性列表。slots 是一个slot描述列表，其中每个slot可以是一个符号(slot的名称)，也可以是形式 `(slot-name . slot-props)`，其中 `slot-props` 是对应槽 slot-name 的属性列表。由 `type-props` 指定的闭包类型属性可以包括以下内容:

* `(:predicate pred-name)`
    这使得类型 otype of closures成为类型onname的父类。类型为onname的开闭包继承其父类型定义的槽。
* `(:copier copier-name copier-args)`
    这导致定义了一个函数更新函数，称为copier，该函数将类型为onname的closure作为其第一个参数，并返回它的副本，其中在copier-args中命名的槽被修改为包含在实际调用copier-name时相应参数中传递的值。

对于slots中的每个slot，宏 oclosure-define 创建一个名为 `onname——slot-name` 的访问器函数;这些可以用来访问槽的值。slot中的slot定义可以指定slot的以下属性:

* `:mutable val`
    默认情况下，slot是不可变的，但是如果你用一个非nil值指定 `:mutable` 属性，slot可以被改变，例如使用setf(参见setf宏)。
* `:type val-type`
    这指定了预期出现在槽中的值的类型。

#### 宏: `Macro: oclosure-lambda (type . slots) arglist &rest body` ####

这个宏创建一个 OClosure 类型的匿名开闭包，该开闭包应该用 `oclosure-define` 定义。slot应该是一个形式为 `(slot-name expr)` 的元素列表。在运行时，按顺序计算每个expr，然后创建closure，并用结果值初始化其槽。

当作为函数调用时(参见调用函数)，由该宏创建的 OCclosure将根据arglist接受参数并执行代码体中的代码。Body可以直接引用其任意slot的值，就像它是静态作用域捕获的局部变量一样。

#### 函数: `oclosure-type` ####

如果对象是一个 OClosure，这个函数返回该对象的开闭包类型(一个符号)，否则返回nil。

另一个与闭包相关的函数是 `oclosure-interactive-form`，它允许某些类型的闭包动态地计算它们的交互表单。看到 `oclosure-interactive-form`。

### 13.12 Advising Emacs Lisp Functions ###

> 本节内容主要用于修改原始的函数或值，并且不改变原始的定义，例如在调用函数之前/之后加一个打印

当你需要修改在另一个库中定义的函数时，或者当你需要修改钩子(比如foo函数、进程过滤器，或者基本上是任何保存函数值的变量或对象字段)时，你可以使用适当的setter函数，比如对命名函数使用fset或defun，对钩子变量使用setq，对进程过滤器使用set-process-filter，但是这些通常都太钝了，完全抛弃了之前的值。

*通知特性 advice feature* 允许您通过 *通知函数 advising the function* 来添加到函数的现有定义中。这是一个比重新定义整个函数更简洁的方法。

Emacs的通知系统为此提供了两组原语:

1. *核心组 the core set*，用于保存在变量和对象字段中的函数值(对应的原语是 `add-function` 和 `remove-function`);
2. 另一组在其之上的原语用于命名函数(主要的原语是 `advice-add` 和 `advice-remove`)。

作为一个简单的例子，下面是如何添加 advice，在每次调用函数时修改函数的返回值:

``` Elisp
(defun my-double (x)
  (* x 2))
(defun my-increase (x)
  (+ x 1))
(advice-add 'my-double :filter-return #'my-double)
```

在添加这个 advice 之后，如果你用 3 调用my-double，返回值将是 7。要删除这个 advice，说

``` Elisp
(advice-remove 'my-double #'my-increase)
```

一个更高级的例子是跟踪对 process `proc` 的 process filter 的调用:

``` Elisp
(defun my-tracing-function (proc string)
  (message "Proc %S received %S" proc string))

(add-function :before (process-filter proc) #'my-tracing-function)
```

这将导致流程的输出在传递给原始流程过滤器之前先传递给 `my-tracing-function`。`my-tracing-function` 接收与原始函数相同的参数。当你完成它时，你可以使用以下命令恢复到未跟踪的行为:

``` Elisp
(remove-function (process-filter proc) #'my-tracing-function)
```

类似地，如果你想跟踪名为display-buffer的函数的执行，你可以使用:

``` Elisp
(defun his-tracing-function (orig-fun &rest args)
  (message "display-buffer called with arg %S" args)
  (let ((res (apply orig-fun args)))
    (message "display-buffer returned %S" res)
	res))

(advice-add 'display-buffer :around #'his-tracing-function)
```

在这里，调用 `his-tracing-function` 而不是原始函数，并接收原始函数(以及该函数的参数)作为参数，因此它可以在需要时调用它。当你厌倦了看到这个输出时，你可以使用以下命令恢复到未跟踪的行为:

``` Elisp
(advice-remove 'display-buffer #'his-tracing-function)
```

上述示例中使用的参数 `:before` 和 `:around` 指定了如何组合这两个函数，因为有许多不同的方法可以组合。添加的函数也称为 a piece of advice。

* Primitives to manipulate advices
* Advising Named Functions
* Ways to compose advice
* Adapting code using the old defadvice
* Advice and Byte Code

#### 13.12.1 Primitives to manipulate advices ####

操作通知的原语

##### 宏: `add-function where place function &optional props` #####

这个宏是一种方便的方式，可以将 advice `function` 添加到存储在 `place` 的函数中(参见通用变量 Generalized Variables)。

`where` 决定 `function` 如何与现有函数组合，例如，函数应该在原函数之前还是之后调用。请参阅组合通知的方法 Ways ti compose advice，以获得组合这两个函数的可用方法列表。

在修改变量(其名称通常以 `-function` 结尾)时，您可以选择是全局使用 `function`还是仅在当前缓冲区中使用:

* 如果`place`只是一个符号，则将function添加到place的全局值中。
* 如果`place`是 `(local symbol)` 的形式，其中symbol是返回变量名的表达式，则function将只添加到当前缓冲区中。
* 最后，如果要修改词法变量，则必须使用 `(var variable)`。

使用 `add-function` 添加的每个函数都可以伴随着属性 `props` 的关联列表。目前，这些属性中只有两个具有特殊含义:

* `name`
    这为 advice 提供了一个名称，`remove-function` 可以使用该名称来标识要删除的函数。通常在函数是匿名函数时使用。
* `depth`
    这指定了在存在多个 advice 时如何排序 advice。缺省情况下，深度为0。深度为100表示这条建议应该尽可能保持深度，而深度为−100表示它应该保持在最外层。当两个通知指定相同的深度时，最近添加的通知将位于最外层。

* `:before`，最外层意味着这个advice将首先运行，在任何其他advice之前，而最内层意味着它将在原始函数之前运行，在它自己和原始函数之间没有其他 advice 运行。
* `:after`, 最内层意味着它将在原始函数之后运行，中间没有其他通知，而最外层意味着它将在所有其他通知之后的最后运行。
* 最内层的 `:override` 通知只会覆盖原始函数，其他通知也会应用于它，而最外层的 `:override` advice 不仅会覆盖原始函数，还会覆盖应用于它的所有其他 advice。

如果函数不是交互的，那么合并后的函数将继承原始函数的 *交互规范 interactive spec*(如果有的话)。否则，合并后的函数将是交互式的，并将使用函数的交互式规范。

一个例外:如果函数的交互规范是一个函数(即，lambda表达式或fbound符号，而不是表达式或字符串)，那么组合函数的交互规范将是对该函数的调用，原始函数的交互规范作为唯一参数。要将收到的规范解释为参数，请使用 `advice-eval-interactive-spec`。

注意:函数的交互规范将适用于组合函数，因此应遵守组合函数的调用约定，而不是函数的调用约定。在许多情况下，这没有什么区别，因为它们是相同的，但对于 `:around`、`:filter-args`和 `:filter-return` 来说，这确实很重要，因为函数接收的参数与存储在原位的原始函数不同。

##### 宏: `remove-function place function` #####

这个宏从存储在适当位置的函数中删除函数。这只适用于使用 `add-function` 将函数添加到 place 的情况。

function 与使用equal添加到place的函数进行比较，以尝试使其也适用于lambda表达式。它还会与添加到place的函数的name属性进行比较，这比使用equal比较lambda表达式更可靠。

##### 函数: `advice-function-member-p advice function-def` #####

如果 advice 已经在 `function-def` 中，则返回非nil。就像上面的 `remove-function` 一样，advice 不是实际的函数，它也可以是 a piece of advice 的名称。

##### 函数: `advice-function-mapc f function-def` #####

为每一条添加到 `function-def` 中的通知调用函数f。调用 `f` 时带两个参数:通知函数及其属性。

##### 函数: `advice-eval-interactive-spec spec` #####

对 interactive spec 进行运算，就像对具有此类 spec 的函数进行交互式调用一样，然后返回所构建的相应参数列表。例如，`(advice-eval-interactive-spec "r\nP")` 将返回一个包含三个元素的列表，其中包含区域的边界和当前前缀参数。

例如，如果你想让 `C-x m` (compose-mail)命令提示符显示 From: 头，你可以这样说:

``` Elisp
(defun my-compose-mail-advice (orig &rest args)
  "Read From: address interactively."
  (interactive
   (lambda (spec)
     (let* ((user-mail-address
             (completing-read "From: "
                              '("one.address@example.net"
                                "alternative.address@example.net")))
            (from (message-make-from user-full-name
                                     user-mail-address))
            (spec (advice-eval-interactive-spec spec)))
       ;; Put the From header into the OTHER-HEADERS argument.
       (push (cons 'From from) (nth 2 spec))
       spec)))
  (apply orig args))

(advice-add 'compose-mail :around #'my-compose-mail-advice)
```

#### 13.12.2 Advising Named Functions ####

advice 的常见用途是用于命名函数和宏。你可以使用 `add-function` 如下所示:

``` Elisp
(add-function :around (symbol-function 'fun) #'his-tracing-function)
```

但是你应该使用 `advice-add` 和 `advice-remove`。这组单独的函数用于操作应用于命名函数的通知片段，与 `add-function` 相比，它们提供了以下额外的特性:

* 它们知道如何处理宏和自动加载的函数
* 它们允许 `describe-function` 保留原始的文档字符串以及记录添加的通知
* 并且允许您在定义函数之前添加和删除通知。

`advice-add` 对于更改对现有函数的现有调用的行为非常有用，而不必重新定义整个函数。然而，它可能是bug的来源，因为函数的现有调用者可能会假设旧的行为，并且在通过通知更改行为时不正确地工作。如果进行调试的人没有注意到或记住函数已被通知修改，则通知还会在调试中造成混乱。

由于这些原因，advice 应该保留给不能以任何其他方式修改函数行为的情况。

* 如果可以通过钩子做同样的事情，这是可取的(参见钩子 Hooks)。如果您只是想更改特定键的功能，那么最好编写一个新命令，并将旧命令的键绑定重新映射到新命令(请参阅重新映射命令 Remapping Commands)。

如果您正在编写发布代码，供其他人使用，请尽量避免在其中包含 advice。如果你想建议的函数没有钩子来完成这项工作，请与Emacs开发人员讨论添加合适的钩子。特别是，Emacs自己的源文件不应该对Emacs中的函数提出建议。(目前这个惯例有一些例外，但我们的目标是纠正它们。)通常，在foo中创建一个新钩子，并让bar使用该钩子，比让bar在foo中放置advice更简洁。

特殊形式不能添加 advice(参见特殊形式 Special Forms)，但是宏可以。当然，这不会影响已经进行宏展开的代码，因此您需要确保在宏展开之前安装通知。

可以 advice 一个原语(参见什么是函数? What Is a Function?)，但通常不应该这样做，原因有两个。

1. 首先，通知机制使用了一些原语，通知它们可能会导致无限递归。
2. 其次，许多原语是直接从C语言调用的，这样的调用忽略了通知;因此，最终会出现一种令人困惑的情况，即一些调用(来自Lisp代码)遵守通知，而其他调用(来自C代码)不遵守通知。

##### 宏: `define-advice symbol (where lambda-list &optional name depth) &rest body` #####

这个宏定义了一条 advice，并将其添加到名为symbol的函数中。如果name为nil，则advice是一个匿名函数或名为 `symbol@name` 的函数。参见advice-add对其他参数的解释。

##### 函数: `advice-add symbol where function &optional props` #####

将advice函数添加到命名函数符号中。where和props的含义与add-function相同(参见操作通知的原语 Primitives to manipulate advices)。

##### 函数: `advice-remove symbol function` #####

从命名函数符号中删除通知函数。函数也可以是一条建议的名称。

##### 函数: `advice-member-p function symbol` #####

如果通知函数已经在命名函数符号中，则返回非nil。函数也可以是一条建议的名称。

##### 函数: `advice-mapc function symbol` #####

为添加到命名函数符号中的每个 advice 调用函数。函数调用时带两个参数:通知函数及其属性。

#### 13.12.3 Ways to compose advice ####

排版advice

下面是add-function和advice-add的where参数可能的不同值，它们指定了建议函数和原始函数应该如何组合。

##### `:before` #####

在旧函数之前调用函数。两个函数接收相同的参数，复合函数的返回值是旧函数的返回值。更具体地说，这两个函数的组合行为如下:

``` Elisp
(lambda (&rest r) (apply function r) (apply oldfun r))
```

`(add-function :before funvar function)` 对于单函数钩子和 `(add-hook 'hookvar function)` 对于普通钩子是可比较的。

##### `:after` #####

在旧函数之后调用函数。两个函数接收相同的参数，复合函数的返回值是旧函数的返回值。更具体地说，这两个函数的组合行为如下:

``` Elisp
(lambda (&rest r) (prog1 (apply oldfun r) (apply function r)))
```

`(add-function :after funvar function)` 对于单函数钩子和 `(add-hook 'hookvar function 'append)` 对于普通钩子是可比较的。

##### `:override` #####

这完全用信函数取代了旧的函数。如果稍后调用remove-function，当然可以恢复旧的函数。


##### `:around` #####

调用function而不是旧函数，但将旧函数作为函数的额外参数提供。这是最灵活的构图。例如，它允许您使用不同的参数或多次调用旧函数，或者在let-binding中调用旧函数，或者您有时可以将工作委托给旧函数，有时可以完全覆盖它。更具体地说，这两个函数的组合行为如下:

``` Elisp
(lambda (&rest r) (apply function oldfun r))
```

##### `:before-while` #####

在旧函数之前调用函数，如果函数返回nil，不要调用旧函数。两个函数接收相同的参数，复合函数的返回值是旧函数的返回值。更具体地说，这两个函数的组合行为如下:

``` Elisp
(lambda (&rest r) (and (apply function r) (apply oldfun r)))
```

当通过 `run-hook-with-args-until-failure` 运行hookvar时，`(add-function :before-while funvar function)`与 `(add-hook 'hookvar function)`是可比较的单函数钩子。

##### `:before-until` #####

在旧函数之前调用函数，并且只在函数返回nil时调用旧函数。更具体地说，这两个函数的组合行为如下:

``` Elisp
(lambda (&rest r) (or (apply function r) (apply oldfun r)))
```

当通过 `run-hook-with-args-until-success` 运行hookvar时，`(add-function :before-until funvar function)` 与 `(add-hook 'hookvar function)` 的单函数钩子是相似的。

##### `:after-while` #####

在旧函数之后调用函数，并且仅当旧函数返回非nil时才调用函数。两个函数接受相同的参数，组合的返回值是函数的返回值。更具体地说，这两个函数的组合行为如下:

``` Elisp
(lambda (&rest r) (and (apply oldfun r) (apply function r)))
```

当通过 `run-hook-with-args-until-failure` 运行hookvar时，`(add-function :after-while funvar function)` 与 `(add-hook 'hookvar function 'append)` 对于单函数钩子是类似的。

##### `:after-until` #####

在旧函数之后调用函数，并且仅当旧函数返回nil时才调用函数。更具体地说，这两个函数的组合行为如下:

``` Elisp
(lambda (&rest r) (or  (apply oldfun r) (apply function r)))
```

当通过 `run-hook-with-args-until-success` 运行hookvar时，`(add-function :after-until funvar function)` 与 `(add-hook 'hookvar function 'append)` 对于单函数钩子是相似的。

##### `:filter-args` #####

首先调用function并使用结果(应该是一个列表)作为传递给旧函数的新参数。更具体地说，这两个函数的组合行为如下:

``` Elisp
(lambda (&rest r) (apply oldfun (funcall function r)))
```

##### `:filter-return` #####

首先调用旧函数并将结果传递给函数。更具体地说，这两个函数的组合行为如下:

``` Elisp
(lambda (&rest r) (funcall function (apply oldfun r)))
```

#### 13.12.4 Adapting code using the old defadvice ####

许多代码使用旧的默认通知机制，而新的advice-add在很大程度上取代了旧的默认通知机制，因为新的advice-add的实现和语义要简单得多。

一条旧的 advice，如:

``` Elisp
(defadvice previous-line (before next-line-at-end
                                 (&optional arg try-vscroll))
  "Insert an empty line when moving up from the top line."
  (if (and next-line-add-newlines (= arg 1)
           (save-excursion (beginning-of-line) (bobp)))
      (progn
        (beginning-of-line)
        (newline))))
```

可以在新的 advice 机制中转换成一个普通函数:

``` Elisp
(defun previous-line--next-line-at-end (&optional arg try-vscroll)
  "Insert an empty line when moving up from the top line."
  (if (and next-line-add-newlines (= arg 1)
           (save-excursion (beginning-of-line) (bobp)))
      (progn
        (beginning-of-line)
        (newline))))
```

显然，这实际上并没有修改前一行。要做到这一点，老的建议是:

``` Elisp
(ad-activate 'previous-line)
```

鉴于新的通知机制需要:

``` Elisp
(advice-add 'previous-line :before #'previous-line--next-line-at-end)
```

注意，ad-activate具有全局效应:它激活为指定功能启用的所有通知。如果您只想激活或禁用特定的部分，则需要使用ad-enable-advice和ad-disable-advice来启用或禁用它。新机制消除了这种区别。

围绕以下建议:

``` Elisp
(defadvice foo (around foo-around)
  "Ignore case in `foo'."
  (let ((case-fold-search t))
    ad-do-it))
(ad-activate 'foo)
```

可以翻译成:

``` Elisp
(defun foo--foo-around (orig-fun &rest args)
  "Ignore case in `foo'."
  (let ((case-fold-search t))
    (apply orig-fun args)))
(advice-add 'foo :around #'foo--foo-around)
```

关于通知的类，请注意新的 `:before` 并不完全等同于旧的before，因为在旧的通知中，你可以修改函数的参数(例如，使用 `ad-set-arg`)，这将影响原始函数看到的参数值，而在新的 `:before` 中，通过通知中的setq修改参数对原始函数看到的参数没有影响。当移植依赖于此行为的before通知时，您需要将其转换为新的 `:around` 或 `:filter-args` 通知。

类似地，旧的 `after`通知可以通过更改 `ad-return-value` 来修改返回值，而新的 `:after` 通知则不能，因此在移植此类旧 after通知时，您需要将其转换为新的 `:around` 或 `:filter-return` 通知。

#### 13.12.5 Advice and Byte Code ####

并非所有功能都可以可靠地提供建议。字节编译器可能会选择用不调用您想要更改的函数的指令序列来替换对函数的调用。

这通常是由于以下三种机制之一造成的:

##### byte-compile properties #####

如果函数的符号具有 *byte-compile* 属性，则将使用该属性而不是符号的函数定义。参见字节编译函数 Byte-compilation Functions。

##### byte-optimize properties #####

如果函数的符号具有字节优化属性，字节编译器可能会重写函数参数，或者决定完全使用不同的函数。

##### compiler-macro declare forms #####

函数可以在其定义中具有特殊的编译器宏声明形式(参见声明形式 The declare Form)，该声明形式定义了在编译函数时要调用的展开器。扩展器可能会导致生成的字节码不调用原始函数。

### 13.13 Declaring Functions Obsolete ###

声明过时的函数

声明函数过时可以将命名函数标记为 *过时 obsolete*，这意味着它可能在将来的某个时候被删除。这导致Emacs在对包含该函数的代码进行字节编译时，以及在显示该函数的文档时，都会警告该函数已经过时。在所有其他方面，过时的函数的行为与任何其他函数一样。

将函数标记为过时的最简单方法是在函数的defun定义中放入 `(declare (obsolete ...)`)形式。见 The declare Form。或者，您可以使用下面描述的 `make-obsolete` 函数。

宏(参见宏)也可以用make-obsolete标记为obsolete;这与函数的效果相同。函数或宏的别名也可以标记为obsolete;这使得别名本身过时，而不是它解析到的函数或宏。

##### 函数: `make-obsolete obsolete-name current-name when` #####

这个函数将 `obsolete-name` 标记为obsolete。`obsolete-name` 应该是一个命名函数或宏的符号，或者函数或宏的别名。

如果 `current-name` 是一个符号，则警告消息要求使用 `current-name` 而不是 `obsolete-name`。`current-name` 不需要是 `obsolete-name` 的别名;它可以是具有相似功能的不同函数。`current-name` 也可以是一个字符串，用作警告消息。消息应该以小写字母开头，以句号结尾。它也可以是nil，在这种情况下，警告消息不提供额外的详细信息。

参数 `when` 应该是一个字符串，表示函数首次过时的时间——例如，日期或发布号。

##### 宏: `define-obsolete-function-alias obsolete-name current-name when &optional doc` #####

这个便利宏将函数 `obsolete-name` 标记为obsolete，并将其定义为函数current-name的别名。它相当于以下内容:

``` Elisp
(defalias obsolete-name current-name doc)
(make-obsolete obsolete-name current-name when)
```

另外，你可以将函数的特定调用约定标记为obsolete:

##### 函数: `set-advertised-calling-convention function signature when` #####

此函数将参数列表签名指定为调用函数的正确方式。这将导致Emacs字节编译器在遇到以其他方式调用函数的Emacs Lisp程序时发出警告(但是，它仍然允许对代码进行字节编译)。when应该是一个字符串，表示变量第一次被废弃的时间(通常是一个版本号字符串)。

例如，在旧版本的Emacs中，`sit-for` 函数接受三个参数，如下所示

``` Elisp
(sit-for seconds milliseconds nodisp)
```

然而，以这种方式调用 `sit-for` 被认为是过时的(参见等待经过时间或输入 Waiting for Elapsed Time or Input)。旧的调用约定被弃用如下:

``` Elisp
(set-advertised-calling-convention
  'sit-for '(seconds &optional nodisp) "22.1")
```

使用此函数的替代方法是 `advertised-caling-convention` declare spec，请参阅声明表单 The declare Form。

### 13.14 Inline Functions ###

内联函数是一种与普通函数一样工作的函数，除了一件事:当您对函数的调用进行字节编译时(请参阅字节编译 Byte Compilation)，函数的定义扩展到调用方。

定义内联函数的简单方法是编写`defsubst`而不是defun。定义的其余部分看起来是一样的，但是使用`defsubst`表示将其内联用于字节编译。

#### 宏: `defsubst name args [doc] [declare] [interactive] body...` ####

这个宏定义了一个内联函数。它的语法与defun完全相同(参见定义函数 Defining Functions)。

将函数内联通常会使其函数调用运行得更快。但它也有缺点。

1. 首先，它降低了灵活性;如果更改了函数的定义，已经内联的调用在重新编译之前仍然使用旧的定义。
2. 另一个缺点是，使大型函数内联可能会增加文件和内存中编译代码的大小。由于内联函数的速度优势对于小函数是最大的，所以通常不应该将大函数内联。
3. 此外，内联函数在调试、跟踪和 advice 方面表现不佳(参见建议Emacs Lisp函数
)。由于易于调试和重新定义函数的灵活性是Emacs的重要特性，所以不应该将函数内联，即使它很小，除非它的速度非常重要，并且您已经对代码进行了计时，以验证使用defun确实存在性能问题。

定义内联函数后，可以在稍后的同一文件中执行其内联展开，就像宏一样。

可以使用`defmacro`来定义一个宏，将其扩展为与内联函数执行的代码相同的代码(参见宏)。但是，宏将被限制在表达式中直接使用—不能通过apply、mapcar等调用宏。此外，将普通函数转换为宏也需要一些工作。将其转换为内联函数很容易;只需将defun替换为defsubst。由于内联函数的每个参数只求值一次，因此不必像处理宏那样担心函数体使用这些参数的次数。

或者，您可以通过提供将其内联为编译器宏的代码来定义函数(参见声明表单 The deckare Form)。下面的宏可以实现这一点。

#### 宏: `define-inline name args [doc] [declare] body...` ####

通过提供内联函数的代码(作为编译器宏)来定义函数名。该函数将接受参数列表args，并具有指定的函数体。

如果存在，doc应该是函数的文档字符串(参见函数的文档字符串);declare，如果存在的话，应该是一个声明形式(参见声明形式)，指定函数的元数据 metadata。

与由`defsubst`或`defmacro`定义的宏相比，通过`define-inline`定义的函数有几个优点:

* 它们可以传递给`mapcar`(参见映射函数)。
* 他们更有效率。
* 它们可以用作 *place forms* 来存储值(参见通用变量 Generalized Variables)。
* 它们的行为比 `cl-defsubst` 更可预测(参见GNU Emacs Lisp的公共Lisp扩展中的参数列表 Argument List)。

与`defmacro`类似，使用`define-inline`内联的函数从调用站点继承动态或词法范围规则。参见变量绑定的作用域规则。

以下宏应该在`define-inline`定义的函数体中使用。

#### 宏: `inline-quote expression` ####

`define-inline`的引用表达式。这类似于反引号(参见反引号 Backquote)，但只引用代码，并且只接受 `,,` 而不接受，`@`。

#### 宏: `inline-letevals (bindings...) body...` ####

这提供了一种方便的方法来确保内联函数的参数只求值一次，以及创建局部变量。

它类似于`let`(参见局部变量):它按照绑定指定的方式设置局部变量，然后在这些绑定生效的情况下计算body。

绑定的每个元素要么是一个符号，要么是如下形式的列表 `(var expr)`;结果是计算expr并将var绑定到结果。然而，当绑定的元素只是一个符号var时，求值var的结果被重新绑定到var(这与let的工作方式完全不同)。

绑定的尾部可以是nil，也可以是一个应该保存参数列表的符号，在这种情况下，每个参数都被求值，并且符号被绑定到结果列表。

#### 宏: `inline-const-p expression` ####

如果表达式的值已知，则返回非空值。

#### 宏: `inline-const-val expression` ####

返回 expression 的值。

#### 宏: `inline-error format &rest args` ####

发出错误信号，根据format格式化参数。

下面是一个使用define-inline的例子:

``` Elisp
(define-inline myaccessor (obj)
  (inline-letevals (obj)
    (inline-quote (if (foo-p ,obj) (aref (cdr ,obj) 3) (aref ,obj 2)))))
```

这个等价于

``` Elisp
(defsubst myaccessor (obj)
  (if (foo-p obj) (aref (cdr obj) 3) (aref obj 2)))
```

### 13.15 The declare Form ###

declare是一个特殊的宏，可用于向函数或宏添加元属性:例如，将其标记为过时的，或者在Emacs Lisp模式下为其形式提供特殊的TAB缩进约定。

#### 宏: `declare specs...` ####

这个宏忽略它的参数并求值为nil;它没有运行时效果。然而，当声明形式出现在defun或defsubst函数定义(参见定义函数)或defmacro宏定义(参见定义宏)的declare参数中时，它会将 specs 指定的属性附加到函数或宏中。这项工作是由defun、defsubst和defmacro专门执行的。

specs中的每个元素都应该有这样的形式 `(property args...)`，它不应该被 quoted。它们有以下效果:

##### `(advertised-calling-convention signature when)` #####

这类似于对 `set-advertised-calling-convention` 的调用(参见声明过时函数);

*signature* 为调用函数或宏指定正确的参数列表，when应该是一个字符串，表示旧的参数列表首次被淘汰的时间。

##### `(debug edebug-form-spec)` #####

这只对宏有效。当使用Edebug逐步执行宏时，请使用 `edebug-form-spec`。参见检测宏调用 Instrumenting Macro Calls。

##### `(doc-string n)` #####

这在定义函数或宏时使用，这些函数或宏本身将用于定义函数、宏或变量等实体。它表明第n个参数(如果有的话)应该被视为文档字符串。

##### `(indent indent-spec)` #####

根据缩进规范调用此函数或宏。这通常用于宏，但它也适用于函数。参见缩进宏 Indenting Macros。

##### `(interactive-only value)` #####

将函数的 `interactive-only` 属性设置为value。参见interactive-only属性 The interactive-only property。

##### `(obsolete current-name when)` #####

将函数或宏标记为obsolete，类似于make-obsolete的调用(参见声明函数obsolete)。current-name应该是一个符号(在这种情况下，警告消息说要使用它)，一个字符串(指定警告消息)，或者nil(在这种情况下，警告消息没有提供额外的细节)。When应该是一个字符串，表示函数或宏首次被废弃的时间。

##### `(compiler-macro expander)` #####

这只能用于函数，并告诉编译器将expander用作优化函数。当遇到对函数的调用时，形式为 `(function args...)`，宏扩展器将使用该形式以及 `args...`调用expander, expander可以返回一个新的表达式来代替函数调用，或者它可以只返回不变的形式，以表明函数调用应该单独保留。

当expander是lambda形式时，它应该使用单个参数(即 `(lambda (arg) body)`的形式)编写，因为函数的形式参数会自动添加到lambda的参数列表中。

##### `(gv-expander expander)` #####

将`expander`声明为处理宏(或函数)调用的函数，作为通用变量 generalized variable，类似于 `gv-define-expander`。expander可以是一个符号，也可以是 `(lambda (arg) body)` 的形式，在这种情况下，函数将额外访问宏(或函数)的参数。

##### `(gv-setter setter)` #####

将setter声明为作为通用变量处理对宏(或函数)调用的函数。setter可以是一个符号，在这种情况下，它将被传递给 `gv-define-simple-setter`，或者它可以是 `(lambda (arg) body)`的形式，在这种情况下，函数将额外访问宏(或函数)的参数，它将被传递给 `gv-define-setter`。

##### `(completion completion-predicate)` #####

将 `completion-predicate` 声明为函数，以确定在 `M-x` 中请求补全时是否将函数的符号包含在函数列表中。只有当 `read-extended-command-predicate` 被定制为 `command-completion-default-include-p` 时，这个谓词函数才会被调用;缺省情况下，`read-extended-command-predicate` 的值为nil(参见execute-extended-command)。用两个参数调用谓词补全谓词:函数的符号和当前缓冲区。

##### `(modes modes)` #####

指定此命令仅适用于指定的模式。请参见指定命令的模式 Specifying Modes For Commands。

##### `(interactive-arg arg ...)` #####

指定应该为 `repeat-command` 存储的参数。每个参数都在参数名称表单中。

##### `(pure val)` #####

如果val非nil，则此函数为纯函数(参见什么是函数? What Is a Function?)

这与函数符号的pure属性相同(参见标准符号属性 Standard Symbol Properties)。

##### `(side-effect-free val)` #####

如果val非nil，则此函数没有副作用，因此字节编译器可以忽略其值被忽略的调用。这与函数符号的 `side-effect-free` 属性相同，请参阅标准符号属性 Standard Symbol Properties。

##### `(speed n)` #####

为此函数的本机编译指定 `native-comp-speed` 的值(请参阅本机编译变量 Native-Compilation Variables)。这允许对为函数发出的本机代码所使用的优化级别进行函数级控制。特别是，如果n为 `-1`，函数的本机编译将发出字节码而不是函数的本机代码。

##### `no-font-lock-keyword` #####

这只对宏有效。具有此声明的宏被 `font-lock`(参见Font Lock Mode)作为普通函数高亮显示，而不是特别作为宏。

### 13.16 Telling the Compiler that a Function is Defined ###

对文件进行字节编译通常会产生关于编译器不知道的函数的警告(参见编译器错误 Compiler Errors)。有时这表明了一个真正的问题，但通常有问题的函数是在其他文件中定义的，如果运行该代码，这些文件将被加载。例如，字节编译 `simple.el` 用来警告:

``` log
simple.el:8727:1:Warning: the function ‘shell-mode’ is not known to be
    defined.
```

实际上，shell-mode 只在调用shell模式之前执行 `(require 'shell)` 的函数中使用，因此shell模式将在运行时正确定义。当您知道这样的警告并不表示真正的问题时，最好抑制警告 suppress the warning。这使得可能意味着真正问题的新警告更加明显。您可以使用declare-function来实现这一点。

你所需要做的就是在第一次使用该函数之前添加一个declare-function语句:

``` Elisp
(declare-function shell-mode "shell" ())
```

这说明shell模式是在 `shell.el` 中定义的。(`.el` 可以省略)。编译器理所当然地认为该文件确实定义了函数，而不进行检查。

可选的第三个参数指定 `shell-mode` 的参数列表。在这种情况下，它不接受任何参数(nil与不指定值不同)。在其他情况下，这可能类似于 `(file &optional overwrite)`。您不必指定参数列表，但如果指定了，字节编译器可以检查调用是否与声明匹配。

#### 宏: `declare-function function file &optional arglist fileonly` ####

告诉字节编译器假设该函数在文件 file 中定义。可选的第三个参数arglist要么是t，这意味着参数列表未指定，要么是与defun风格相同的形式参数列表(包括括号)。省略的参数列表默认为t，而不是nil;这是省略参数的非典型行为，这意味着要提供第四个而不是第三个参数，必须为第三个参数占位符指定t，而不是通常的nil。可选的第四个参数fileonly non-nil意味着只检查该文件是否存在，而不检查它是否真正定义了函数。

要验证这些函数是否真的在 `declare-function` 声明的位置声明，可以使用 `check-declare-file` 检查一个源文件中的所有 `declare-function` 调用，或者使用 `check-declare-directory` 检查某个目录中的所有文件。

这些命令使用 `location-library` 查找应该包含函数定义的文件;如果没有找到文件，它们就展开定义文件名，相对于包含 `declare-function` 调用的文件的目录。

您还可以通过指定以 `.c` 或 `.m` 结尾的文件名来说明函数是一个原语。这只有在调用仅在某些系统上定义的原语时才有用。大多数原语总是被定义的，所以它们永远不会给你一个警告。

有时，文件会选择性地使用外部包中的函数。如果在 `declare-function` 语句中使用 `ext:` 作为文件名的前缀，则会检查它是否找到，否则会跳过，不会出错。

有一些函数定义 `check-declare` 不理解(例如，defstruct和其他一些宏)。在这种情况下，可以向 `declare-function` 传递一个非nil的fileonly参数，这意味着只检查文件是否存在，而不检查它是否实际定义了函数。请注意，要做到这一点而不必指定参数列表，您应该将arglist参数设置为t(因为nil意味着空参数列表，而不是未指定的参数列表)。

### 13.17 Determining whether a Function is Safe to Call ###

一些主要模式(如SES)调用存储在用户文件中的函数。(有关ses的更多信息，请参见(ses)Simple Emacs Spreadsheet。)用户文件有时有着糟糕的血统——你可以从你刚认识的人那里得到一个电子表格，或者你可以从你从未见过的人那里通过电子邮件得到一个电子表格。因此，调用源代码存储在用户文件中的函数是有风险的，除非您确定它是安全的。

#### 函数: `unsafep form &optional unsafep-vars` ####

如果form是一个安全的Lisp表达式，则返回nil，或者返回一个描述为什么它可能不安全的列表。参数 `unsafep-vars` 是目前已知具有临时绑定的符号列表;它主要用于内部递归调用。当前缓冲区是一个隐式参数，它提供了一个缓冲区本地绑定列表。

由于快速和简单，unsafep只做了很少的分析，并拒绝了许多实际上是安全的Lisp表达式。对于不安全表达式，没有已知的unsafe返回nil的情况。然而，一个安全的Lisp表达式可以返回一个带有display属性的字符串，其中包含一个相关的Lisp表达式，该表达式将在字符串插入缓冲区后执行。这种相关的表达可以是病毒。为了安全起见，必须在将用户代码计算的所有字符串插入缓冲区之前删除它们的属性。

### 13.18 Other Topics Related to Functions ###

下面是几个函数的表，这些函数做与函数调用和函数定义相关的事情。它们在其他地方有记录，但我们在这里提供交叉参考。

* `apply`
    See Calling Functions.
* `autoload`
    See Autoload.
* `call-interactively`
    See Interactive Call.
* `called-interactively-p`
    See Distinguish Interactive Calls.
* `commandp`
    See Interactive Call.
* `documentation`
    See Access to Documentation Strings.
* `eval`
    See Eval.
* `funcall`
    See Calling Functions.
* `function`
    See Anonymous Functions.
* `ignore`
    See Calling Functions.
* `indirect-function`
    See Symbol Function Indirection.
* `interactive`
    See Using interactive.
* `interactive-p`
    See Distinguish Interactive Calls.
* `mapatoms`
    See Creating and Interning Symbols.
* `mapcar`
    See Mapping Functions.
* `map-char-table`
    See Char-Tables.
* `mapconcat`
    See Mapping Functions.
* `undefined`
    See Functions for Key Lookup. 

## 14 Macros ##

[Macros](https://www.gnu.org/software/emacs/manual/html_node/elisp/Macros.html)

宏使您能够定义新的控件结构和其他语言特性。宏的定义很像函数，但它不是告诉如何计算一个值，而是告诉如何计算另一个Lisp表达式，该表达式将依次计算该值。我们称这个表达式为宏的展开。

宏可以做到这一点，因为它们对参数的未求值表达式进行操作，而不是像函数那样对参数值进行操作。因此，它们可以构造一个包含这些参数表达式或它们的一部分的展开。

如果您使用宏来完成普通函数可以完成的任务，只是为了提高速度，请考虑使用内联函数。参见内联函数 Inline Functions。


* A Simple Example of a Macro
* Expansion of a Macro Call
* Macros and Byte Compilation
* Defining Macros
* Common Problems Using Macros
* Indenting Macros

### 14.1 A Simple Example of a Macro ###

假设我们想要定义一个Lisp结构来增加变量值，就像c中的++操作符一样，我们想要写 `(inc x)` 并具有 `(setq x (1+ x))` 的效果。这里有一个宏定义:

``` Elisp
(defmacro inc (var)
   (list 'setq var (list '1+ var)))
```

当使用 `(inc x)` 调用该函数时，参数var是符号x，而不是函数中x的值。宏的主体使用它来构造展开，即 `(setq x (1+ x))`。一旦宏定义返回这个展开，Lisp继续对其求值，从而使x递增。

#### 函数: `macrop object` ####

该谓词测试其参数是否为宏，如果是则返回t，否则返回nil。

### 14.2 Expansion of a Macro Call ###

宏调用看起来就像函数调用，因为它是一个以宏名称开头的列表。列表中其余的元素是宏的参数。

宏调用的求值开始与函数调用的求值类似，除了一个关键的区别:**宏参数是宏调用中出现的实际表达式**。在将它们赋给宏定义之前，不会对它们求值。相反，函数的实参是对函数调用列表中的元素求值的结果。

获得参数后，Lisp调用宏定义，就像调用函数一样。宏的参数变量被绑定到宏调用的参数值，或者在 `&rest` 参数的情况下绑定到它们的列表。宏体执行并返回它的值，就像函数体一样。

宏和函数之间的第二个关键区别是宏主体返回的值是另一个Lisp表达式，也称为宏的扩展。Lisp解释器从宏返回后立即对展开进行求值。

由于展开是以正常方式计算的，因此它可能包含对其他宏的调用。它甚至可能是对同一个宏的调用，尽管这是不寻常的。

注意，Emacs在加载未编译的Lisp文件时会尝试展开宏。这并不总是可能的，但如果是，它会加快后续的执行。请参阅程序如何加载 How Programs Do Loading。

您可以通过调用`macroexpand`来查看给定宏调用的展开。

#### 函数: `macroexpand form &optional environment` ####

如果是宏调用，则该函数展开形式。如果结果是另一个宏调用，则依次展开它，直到结果不是宏调用。这是macroexpand返回的值。如果form不是宏调用的起始点，则返回它。

注意，`macroexpand` 不查看 form 的子表达式(尽管一些宏定义可能会查看)。即使它们本身是宏调用，macroexpand也不会展开它们。

函数`macroexpand`不会展开对内联函数的调用。通常不需要这样做，因为调用内联函数并不比调用普通函数更难理解。

如果提供了environment，它将指定一个宏定义列表，这些宏定义将显示当前定义的宏。字节编译使用这个特性。

``` Elisp
(defmacro inc (var)
    (list 'setq var (list '1+ var)))


(macroexpand '(inc r))
     ⇒ (setq r (1+ r))


(defmacro inc2 (var1 var2)
    (list 'progn (list 'inc var1) (list 'inc var2)))


(macroexpand '(inc2 r s))
     ⇒ (progn (inc r) (inc s))  ; inc not expanded here.
```

#### 函数: `macroexpand-all form &optional environment` ####

`macroexpand-all` 像 `macroexpand` 一样展开宏，但将在形式上查找并展开所有宏，而不仅仅是在顶层。如果没有展开宏，返回值等于form。

重复上面使用 `macroexpand-all` 的例子，我们看到macroexpand-all确实扩展了对inc的内嵌调用:

``` Elisp
(macroexpand-all '(inc2 r s))
     ⇒ (progn (setq r (1+ r)) (setq s (1+ s)))
```

#### 函数: `macroexpand-1 form &optional environment` ####

这个函数像macroexpand一样展开宏，但是它只执行展开的一个步骤:如果结果是另一个宏调用，`macroexpand-1` 将不会展开它。

### 14.3 Macros and Byte Compilation ###

你可能会问，为什么我们要不厌其烦地计算一个宏的展开，然后再对展开进行运算。为什么不让宏体直接产生期望的结果呢?原因与编译有关。

当正在编译的Lisp程序中出现宏调用时，Lisp编译器就像解释器一样调用宏定义，并接受展开。但是它不计算这个展开，而是把它当作直接出现在程序中来编译。因此，编译后的代码产生用于宏的值和副作用，但以完全编译的速度执行。如果宏体本身计算了值和副作用，这将不起作用——它们将在编译时计算，这是没有用的。

为了使宏调用的编译能够工作，在编译对宏的调用时，必须已经在Lisp中定义了宏。编译器有一个特殊的功能来帮助您做到这一点:如果正在编译的文件包含defmacro形式，则该宏将在该文件的其余编译过程中临时定义。

对文件进行字节编译还会在文件的顶层执行任何require调用，因此您可以通过要求定义它们的文件来确保在编译期间必要的宏定义可用(请参阅功能 Features)。为了避免在运行编译后的程序时加载宏定义文件，请在require调用周围编写 `eval-when-compile`(参见编译期间的求值 Evaluation During Compilation)。

### 14.4 Defining Macros ###

Lisp宏对象是一个列表，它的CAR是宏，CDR是函数。通过将函数(带apply)应用于来自宏调用的未求值参数列表来展开宏。

可以像使用匿名函数一样使用匿名Lisp宏，但从来没有这样做过，因为将匿名宏传递给mapcar等函数是没有意义的。实际上，所有Lisp宏都有名字，它们几乎总是用defmacro宏定义的。

#### 宏: `defmacro name args [doc] [declare] body...` ####

defmacro将符号名(不应该加引号)定义为一个宏，如下所示:

``` Elisp
(macro lambda args . body)
```

(注意，该列表的CDR是一个lambda表达式。)这个宏对象存储在name的函数单元格中。args的含义与函数中的含义相同，可以使用关键字&rest和&optional(请参阅参数列表的特性 Features of Argument Lists)。名字和参数都不应该加引号。defmacro的返回值未定义。

doc(如果存在)应该是一个字符串，指定宏的文档字符串。declare，如果存在的话，应该是一个为宏指定元数据的声明表单(参见声明表单The declare Form)。注意，宏不能有交互式声明，因为它们不能被交互式地调用。

宏通常需要从常量和非常量部分的混合中构造大型列表结构。为了使这更容易，使用反引号语法(参见反引号 Backquote)。例如:

``` Elisp
(defmacro t-becomes-nil (variable)
  `(if (eq ,variable t)
       (setq ,variable nil)))


(t-becomes-nil foo)
     ≡ (if (eq foo t) (setq foo nil))
```

> 注: 反引号表示不要对后面的列表进行求值, 但是可以通过 `,` 来对里面的列表单独求值, 或者使用 `,@` 对符号进行求值

``` Elisp
`(a list of ,(+ 2 3) elements)
     ⇒ (a list of 5 elements)


(setq some-list '(2 3))
     ⇒ (2 3)

(cons 1 (append some-list '(4) some-list))
     ⇒ (1 2 3 4 2 3)

`(1 ,@some-list 4 ,@some-list)
     ⇒ (1 2 3 4 2 3)
```

### 14.5 Common Problems Using Macros ###

宏展开可能产生违反直觉的后果。本节描述可能导致故障的一些重要后果，以及避免故障需要遵循的规则。

* Wrong Time
* Evaluating Macro Arguments Repeatedly
* Local Variables in Macro Expansions
* Evaluating Macro Arguments in Expansion
* How Many Times is the Macro Expanded?

#### 14.5.1 Wrong Time ####

编写宏时最常见的问题是在展开宏的同时 **过早地** 完成了一些实际工作，而不是在展开宏的过程中。例如，一个真正的包有这样的宏定义:

``` Elisp
(defmacro my-set-buffer-multibyte (arg)
  (if (fboundp 'set-buffer-multibyte)
      (set-buffer-multibyte arg)))
```

使用这个错误的宏定义，程序在解释时工作正常，但在编译时失败。这个宏定义在编译期间调用set-buffer-multibyte，这是错误的，然后在编译包运行时什么也不做。程序员真正想要的定义是:

``` Elisp
(defmacro my-set-buffer-multibyte (arg)
  (if (fboundp 'set-buffer-multibyte)
      `(set-buffer-multibyte ,arg)))
```

如果合适的话，这个宏将展开为对 `set-buffer-multibyte` 的调用，该调用将在编译后的程序实际运行时执行。

#### 14.5.2 Evaluating Macro Arguments Repeatedly ####

定义宏时，必须注意执行展开时计算参数的次数。下面的宏(用于促进迭代)说明了这个问题。这个宏允许我们编写for循环结构。

``` Elisp
(defmacro for (var from init to final do &rest body)
  "Execute a simple \"for\" loop.
For example, (for i from 1 to 10 do (print i))."
  (list 'let (list (list var init))
        (cons 'while
              (cons (list '<= var final)
                    (append body (list (list 'inc var)))))))

(for i from 1 to 3 do
   (setq square (* i i))
   (princ (format "\n%d %d" i square)))
→

(let ((i 1))
  (while (<= i 3)
    (setq square (* i i))
    (princ (format "\n%d %d" i square))
    (inc i)))


     -|1       1
     -|2       4
     -|3       9
⇒ nil
```

这个宏中的from、to和do参数是语法糖;他们完全被忽视了。这个想法是，您将在宏调用中的这些位置写入噪声词(例如from、to和do)。

下面是通过使用反引号简化的等价定义:

``` Elisp
(defmacro for (var from init to final do &rest body)
  "Execute a simple \"for\" loop.
For example, (for i from 1 to 10 do (print i))."
  `(let ((,var ,init))
     (while (<= ,var ,final)
       ,@body
       (inc ,var))))
```

该定义的两种形式(带反引号和不带反引号)都存在这样的缺陷，即每次迭代都要评估final。如果final是常数，这不是问题。如果它是一个更复杂的形式，比如(long-complex-calculation x)，这可能会显著降低执行速度。如果final有副作用，多次执行它可能是不正确的。

设计良好的宏定义会采取措施避免这个问题，方法是生成一个只对参数表达式求值一次的展开，除非重复求值是宏预期目的的一部分。下面是for宏的正确展开:

``` Elisp
(let ((i 1)
      (max 3))
  (while (<= i max)
    (setq square (* i i))
    (princ (format "%d      %d" i square))
    (inc i)))
```

下面是创建这个扩展的宏定义:

``` Elisp
(defmacro for (var from init to final do &rest body)
  "Execute a simple for loop: (for i from 1 to 10 do (print i))."
  `(let ((,var ,init)
         (max ,final))
     (while (<= ,var max)
       ,@body
       (inc ,var))))
```

不幸的是，此修复引入了另一个问题，将在下一节中描述。

#### 14.5.3 Local Variables in Macro Expansions ####

在上一节中，将for的定义固定如下，以使展开对宏参数求值的次数合适:

``` Elisp


(defmacro for (var from init to final do &rest body)
  "Execute a simple for loop: (for i from 1 to 10 do (print i))."

  `(let ((,var ,init)
         (max ,final))
     (while (<= ,var max)
       ,@body
       (inc ,var))))
```

for的新定义带来了一个新问题:它引入了一个用户意想不到的名为max的局部变量。这会在以下示例中造成麻烦:

``` Elisp
(let ((max 0))
  (for x from 0 to 10 do
    (let ((this (frob x)))
      (if (< max this)
          (setq max this)))))
```

(setq Max this)))))

for函数体中对max的引用，应该是对max的用户绑定的引用，实际上访问的是for所做的绑定。

纠正这个问题的方法是使用一个 *非内部的符号 uninterned symbol* 而不是max(参见创建和嵌入符号 Creating and Interning Symbols)。uninterned符号可以像任何其他符号一样被绑定和引用，但是由于它是由for创建的，我们知道它不能已经出现在用户的程序中。由于它没有被存储，因此用户以后无法将它放入程序中。它永远不会出现在任何地方，除非放在那里。以下是for的定义:

``` Elisp
(defmacro for (var from init to final do &rest body)
  "Execute a simple for loop: (for i from 1 to 10 do (print i))."
  (let ((tempvar (make-symbol "max")))
    `(let ((,var ,init)
           (,tempvar ,final))
       (while (<= ,var ,tempvar)
         ,@body
         (inc ,var)))))
```

这将创建一个名为max的非内部符号，并将其放在展开中，而不是通常出现在表达式中的通常内部符号max。

#### 14.5.4 Evaluating Macro Arguments in Expansion ####

如果宏定义本身计算任何宏参数表达式，例如通过调用eval(参见eval)，则可能发生另一个问题。您必须考虑到，当调用者的上下文(将对宏展开进行求值)还无法访问时，宏展开可能在执行代码之前很久就发生了。

另外，如果宏定义不使用 *词法绑定*，它的形式参数可能会隐藏同名的用户变量。在宏主体内部，宏参数绑定是此类变量的最局部绑定，因此正在计算的表单中的任何引用都引用它。下面是一个例子:

``` Elisp
(defmacro foo (a)
  (list 'setq (eval a) t))

(setq x 'b)
(foo x) → (setq b t)
     ⇒ t                  ; and b has been set.
;; but
(setq a 'c)
(foo a) → (setq a t)
     ⇒ t                  ; but this set a, not c.
```

用户变量的名称是a还是x是有区别的，因为a与宏参数变量a冲突。

另外，上面的 `(foo x)` 的展开将在编译代码时返回不同的内容或发出错误信号，因为在这种情况下 `(foo x)` 在编译期间展开，而 `(setq x 'b)` 的执行只会在代码执行后发生。

为了避免这些问题，**不要在计算宏展开时求值参数表达式**。相反，将表达式替换到宏展开中，以便在执行展开时计算其值。这就是本章中其他例子的工作原理。

#### 14.5.5 How Many Times is the Macro Expanded? ####

有时会出现这样的问题:宏调用在解释函数中每次求值时都会展开，而在编译函数中只展开一次(在编译期间)。如果宏定义具有副作用，则它们的工作方式将根据宏展开的次数而有所不同。

因此，您应该避免在计算宏展开时产生副作用，除非您确实知道自己在做什么。

有一种特殊的副作用是无法避免的:*构造Lisp对象 constructing Lisp objects*。几乎所有的宏展开都包含构造列表;这就是大多数宏的全部意义所在。这通常是安全的;只有一种情况必须小心:当您构建的对象是宏展开中**引用常量的一部分**时。

如果宏在编译中只展开一次，那么对象在编译期间只构造一次。但是在解释执行中，每次宏调用运行时都会展开宏，这意味着每次都会构造一个新对象。

在大多数干净的Lisp代码中，这种差异无关紧要。只有在对由宏定义构造的对象执行副作用时才有意义。因此，为了避免麻烦，请避免对由宏定义构造的对象产生副作用。下面是这些副作用如何给你带来麻烦的一个例子:

``` Elisp
(defmacro empty-object ()
  (list 'quote (cons nil nil)))


(defun initialize (condition)
  (let ((object (empty-object)))
    (if condition
        (setcar object condition))
    object))
```

如果解释initialize，则每次调用initialize时都构造一个新列表(nil)。因此，调用之间没有副作用。如果编译了initialize，则在编译期间展开宏empty-object，生成一个单独的常量(nil)，每次调用initialize时重用和修改该常量。

避免这种病态情况的一种方法是将空对象视为一种有趣的常量，而不是内存分配结构。你不会在像 `'(nil)` 这样的常量上使用setcar，所以自然你也不会在 `(empty-object)`上使用它。

### 14.6 Indenting Macros ###

在宏定义中，可以使用 declare 形式(参见定义宏 Defining Macros) 指定TAB应该如何缩进对宏的调用。缩进规范是这样写的:

``` Elisp
(declare (indent indent-spec))
```

这将导致在宏名称上设置 `lisp-indent-function` 属性。

以下是 `indent-spec` 的可能性:

* `nil`
    这与没有属性相同—使用标准缩进模式。
* `defun`
    像处理' def '构造一样处理这个函数:将第二行作为主体的开始。
* `an integer, number`
    函数的第一个 number 参数是有区别的参数;其余部分被认为是表达式的主体。表达式中的一行将根据该行上的第一个参数是否被区分而缩进。如果参数是正文的一部分，则该行缩进比包含表达式开头的开括号多列。如果参数是有区别的，并且是第一个或第二个参数，则会将其缩进 *两倍 twice* 的额外列。如果参数是有区别的，而不是第一个或第二个参数，则该行使用标准模式。
* `a symbol, symbol`
    symbol应该是函数名;调用该函数来计算该表达式内一行的缩进。该函数接收两个参数:
    1. `pos` 被缩进的行开始的位置。
    2. `state` `parse-partial-sexp` (用于缩进和嵌套计算的Lisp原语)在解析到该行开头时返回的值。

它应该返回一个数字，即该行缩进的列数，或者返回一个列表，其car就是这样一个数字。返回一个数字和返回一个列表之间的区别是，一个数字表示在同一嵌套级别的所有后续行都应该像这样缩进;列表说明以下行可能需要不同的缩进。当缩进由 `C-M-q` 计算时，这是不同的;如果值是数字，则 `C-M-q` 在列表结束之前不需要重新计算以下行的缩进。

## 15 Customization Settings ##

## 16 Loading ##

## 17 Byte Compilation ##

## 18 Compilation of Lisp to Native Code ##

## 19 Debugging Lisp Programs ##

## 20 Reading and Printing Lisp Objects ##

## 21 Minibuffers ##

## 22 Command Loop ##

[Command Loop](https://www.gnu.org/software/emacs/manual/html_node/elisp/Command-Loop.html)

当您运行Emacs时，它几乎立即进入 *编辑器命令循环 editor command loop*。这个循环读取 *键序列 key sequences*，执行它们的定义，并显示结果。在本章中，我们描述了这些事情是如何完成的，以及允许Lisp程序完成这些事情的子程序。


* Command Loop Overview
* Defining Commands
* Interactive Call
* Distinguish Interactive Calls
* Information from the Command Loop
* Adjusting Point After Commands
* Input Events
* Reading Input
* Special Events
* Waiting for Elapsed Time or Input
* Quitting
* Prefix Command Arguments
* Recursive Editing
* Disabling Commands
* Command History
* Keyboard Macros

### 22.1 Command Loop Overview ###



### 22.2 Defining Commands ###

### 22.3 Interactive Call ###

### 22.4 Distinguish Interactive Calls ###

### 22.5 Information from the Command Loop ###

### 22.6 Adjusting Point After Commands ###

### 22.7 Input Events ###

### 22.8 Reading Input ###

### 22.9 Special Events ###

### 22.10 Waiting for Elapsed Time or Input ###

### 22.11 Quitting ###

### 22.12 Prefix Command Arguments ###

### 22.13 Recursive Editing ###

### 22.14 Disabling Commands ###

### 22.15 Command History ###

### 22.16 Keyboard Macros ###

## 23 Keymaps ##

## 24 Major and Minor Modes ##

## 25 Documentation ##

## 26 Files ##

## 27 Backups and Auto-Saving ##

## 28 Buffers ##

[Buffers](https://www.gnu.org/software/emacs/manual/html_node/elisp/Buffers.html)

## 29 Windows ##

## 30 Frames ##

## 31 Positions ##

## 32 Markers ##

## 33 Text ##

## 34 Non-ASCII Characters ##

## 35 Searching and Matching ##

## 36 Syntax Tables ##

## 37 Parsing Program Source ##

## 38 Abbrevs and Abbrev Expansion ##

## 39 Threads ##

## 40 Processes ##

## 41 Emacs Display ##

## 42 Operating System Interface ##

## 43 Preparing Lisp code for distribution ##

## Appendix A Emacs 28 Antinews ##

## Appendix B GNU Free Documentation License ##

## Appendix C GNU General Public License ##

## Appendix D Tips and Conventions ##

## Appendix E GNU Emacs Internals ##

## Appendix F Standard Errors ##

## Appendix G Standard Keymaps ##

## Appendix H Standard Hooks ##

---

(global-display-line-numbers-mode 0)

<!-- -*- global-display-line-numbers-mode: 1; -*- -->

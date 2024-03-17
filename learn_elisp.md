# 学习Elisp #

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




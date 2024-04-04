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
;  ""
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



---


#### 函数: `` ####


#### 函数: `` ####



#### 函数: `` ####


#### 函数: `` ####



#### 函数: `` ####



#### 函数: `` ####



#### 函数: `` ####



#### 函数: `` ####



#### 函数: `` ####



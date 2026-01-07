# 第一章：COBOL 程序基础结构

## 1.1 COBOL 简介

COBOL（Common Business-Oriented Language，通用商业语言）诞生于 1959 年，是世界上最早的高级编程语言之一。虽然已有 60 多年历史，但至今仍在银行、保险、政府等领域的核心系统中广泛使用。

**COBOL 的特点：**
- 接近英语的语法，可读性强
- 强大的数据处理能力
- 精确的十进制运算（非常适合金融计算）
- 向后兼容性极好

## 1.2 程序的四大部（Division）

每个 COBOL 程序由四个部组成，必须按顺序出现：

```cobol
       IDENTIFICATION DIVISION.    *> 标识部：程序基本信息
       ENVIRONMENT DIVISION.       *> 环境部：运行环境配置
       DATA DIVISION.              *> 数据部：变量和数据结构定义
       PROCEDURE DIVISION.         *> 过程部：程序逻辑
```

### 1.2.1 标识部（IDENTIFICATION DIVISION）

标识部是程序的"身份证"，包含程序名称等基本信息。

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LEDGER-MAIN.
      *> PROGRAM-ID 是必须的，定义程序名称
      *> 程序名最长 30 个字符，建议使用大写和连字符
```

**注意：** `*>` 是 COBOL 的行内注释符号。

### 1.2.2 环境部（ENVIRONMENT DIVISION）

环境部定义程序运行的外部环境，如文件系统、特殊配置等。对于简单程序可以省略。

```cobol
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.
      *> 上面这行让我们可以直接使用内置函数，不需要加 FUNCTION 前缀
```

### 1.2.3 数据部（DATA DIVISION）

数据部定义程序使用的所有变量和数据结构。这是 COBOL 最重要的部分之一。

```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *> WORKING-STORAGE 存放程序运行期间的变量
       
       01 WS-COUNTER    PIC 9(3) VALUE 0.
       01 WS-NAME       PIC X(20) VALUE SPACES.
```

### 1.2.4 过程部（PROCEDURE DIVISION）

过程部包含程序的实际逻辑代码。

```cobol
       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           DISPLAY "Hello, COBOL!"
           STOP RUN.
```

## 1.3 代码格式

### 传统固定格式（Fixed Format）

传统 COBOL 使用 80 列固定格式：
- 第 1-6 列：行号（可选）
- 第 7 列：指示符（`*` 表示注释，`-` 表示续行）
- 第 8-11 列：A 区（DIVISION、SECTION、段落名等）
- 第 12-72 列：B 区（语句）
- 第 73-80 列：标识（可选）

### 自由格式（Free Format）

现代 GnuCOBOL 支持自由格式，更接近其他编程语言：

```cobol
*> 这是自由格式的 COBOL 代码
*> 不需要关心列位置
IDENTIFICATION DIVISION.
PROGRAM-ID. HELLO.
PROCEDURE DIVISION.
    DISPLAY "Hello, World!".
    STOP RUN.
```

**本教程使用自由格式**，编译时加 `-free` 参数。

## 1.4 第一个程序：Hello World

创建文件 `hello.cbl`：

```cobol
      *> hello.cbl - 第一个 COBOL 程序
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           DISPLAY "Hello, COBOL World!"
           DISPLAY "欢迎学习 COBOL 语言"
           STOP RUN.
```

编译并运行：

```bash
cobc -x -free hello.cbl -o hello
./hello
```

输出：
```
Hello, COBOL World!
欢迎学习 COBOL 语言
```

**编译参数说明：**
- `-x`：生成可执行文件
- `-free`：使用自由格式
- `-o hello`：指定输出文件名

## 1.5 COBOL 语法要点

### 句号的重要性

COBOL 使用句号（`.`）作为语句结束符，**非常重要**：

```cobol
      *> 正确：每个语句或段落以句号结束
       DISPLAY "Line 1".
       DISPLAY "Line 2".
       
      *> 错误：缺少句号会导致编译错误或逻辑错误
       DISPLAY "Line 1"
       DISPLAY "Line 2"
```

### 大小写不敏感

COBOL 关键字不区分大小写，但建议保持一致：

```cobol
      *> 这三种写法等价
       DISPLAY "Hello".
       display "Hello".
       Display "Hello".
```

### 保留字

COBOL 有大量保留字（约 300 个），常见的如：
- `DISPLAY`, `ACCEPT`, `MOVE`, `COMPUTE`
- `IF`, `ELSE`, `END-IF`, `PERFORM`
- `ADD`, `SUBTRACT`, `MULTIPLY`, `DIVIDE`

## 1.6 练习

1. 修改 Hello World 程序，显示你的名字
2. 尝试去掉句号，观察编译错误
3. 尝试使用固定格式编写同样的程序

## 下一章预告

下一章我们将深入学习 COBOL 的数据定义，了解如何声明各种类型的变量。

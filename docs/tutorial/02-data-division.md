# 第二章：数据定义与变量

## 2.1 数据部的结构

数据部（DATA DIVISION）是 COBOL 程序的核心，所有变量必须在这里预先声明。

```cobol
       DATA DIVISION.
       
       FILE SECTION.
      *> 文件相关的数据结构
       
       WORKING-STORAGE SECTION.
      *> 程序运行期间的变量（最常用）
       
       LOCAL-STORAGE SECTION.
      *> 每次调用时重新初始化的变量
       
       LINKAGE SECTION.
      *> 从其他程序传入的参数
```

本章重点讲解 `WORKING-STORAGE SECTION`。

## 2.2 层级结构

COBOL 使用层级号（01-49）来组织数据：

```cobol
       01 WS-PERSON.                    *> 01 级：顶层记录
          05 WS-NAME.                   *> 05 级：子项
             10 WS-FIRST-NAME  PIC X(20).  *> 10 级：更细的子项
             10 WS-LAST-NAME   PIC X(20).
          05 WS-AGE           PIC 9(3).
          05 WS-SALARY        PIC 9(7)V99.
```

**层级规则：**
- `01` 级是最高层，表示一个完整的数据记录
- 子层级号必须大于父层级号
- 常用层级：01, 05, 10, 15...（便于后续插入）
- `77` 级：独立变量（不属于任何组）
- `88` 级：条件名（布尔值）

## 2.3 PICTURE 子句（PIC）

`PIC`（PICTURE 的缩写）定义变量的类型和格式：

### 2.3.1 字符型（PIC X）

```cobol
       01 WS-NAME           PIC X(20).     *> 20 个字符
       01 WS-CODE           PIC XXX.       *> 3 个字符（等同于 X(3)）
       01 WS-CHAR           PIC X.         *> 1 个字符
```

### 2.3.2 数字型（PIC 9）

```cobol
       01 WS-COUNT          PIC 9(5).      *> 5 位无符号整数 (0-99999)
       01 WS-AMOUNT         PIC 9(7)V99.   *> 7 位整数 + 2 位小数
       01 WS-BALANCE        PIC S9(9)V99.  *> 带符号，9 位整数 + 2 位小数
```

**符号说明：**
- `9`：数字位
- `V`：隐含小数点（不占存储空间）
- `S`：符号位（正或负）

### 2.3.3 编辑型（用于显示）

```cobol
       01 WS-DISPLAY-AMT    PIC Z(6)9.99.  *> 前导零替换为空格
       01 WS-DISPLAY-BAL    PIC -(7)9.99.  *> 负数显示负号
       01 WS-DISPLAY-MONEY  PIC $$$,$$9.99. *> 货币格式
```

**编辑符号：**
- `Z`：前导零替换为空格
- `-`：负数时显示负号
- `$`：货币符号
- `,`：千位分隔符
- `.`：小数点

### 2.3.4 实际例子

```cobol
       WORKING-STORAGE SECTION.
       
      *> 原始数值
       01 WS-AMOUNT         PIC S9(7)V99 VALUE 12345.67.
       
      *> 显示格式
       01 WS-AMOUNT-DISP    PIC Z(6)9.99.
       
       PROCEDURE DIVISION.
           MOVE WS-AMOUNT TO WS-AMOUNT-DISP
           DISPLAY "Amount: " WS-AMOUNT-DISP
      *>   输出: Amount:   12345.67
```

## 2.4 VALUE 子句

`VALUE` 设置变量的初始值：

```cobol
       01 WS-COUNTER        PIC 9(3) VALUE 0.
       01 WS-NAME           PIC X(20) VALUE "John Doe".
       01 WS-NAME2          PIC X(20) VALUE SPACES.    *> 全空格
       01 WS-CODE           PIC X(5) VALUE ZEROS.      *> 全零
       01 WS-RATE           PIC 9V99 VALUE 1.25.
```

## 2.5 COMP 系列（二进制存储）

默认的 `PIC 9` 使用字符存储（每位占 1 字节），效率较低。`COMP` 系列使用二进制存储：

```cobol
      *> COMP / COMP-4：二进制整数
       01 WS-INT            PIC S9(9) COMP.
       
      *> COMP-5：原生二进制（与 C 语言兼容）
       01 WS-C-INT          PIC S9(9) COMP-5.    *> 等同于 C 的 int
       01 WS-C-LONG         PIC S9(18) COMP-5.   *> 等同于 C 的 long
       
      *> COMP-3：压缩十进制（BCD）
       01 WS-PACKED         PIC S9(7)V99 COMP-3.
```

**与 C 语言互操作时，必须使用 `COMP-5`！**

```cobol
      *> 正确：与 C 的 int 兼容
       01 WS-RETURN-CODE    PIC S9(9) COMP-5 VALUE 0.
       
      *> 错误：COMP 的字节序可能与 C 不同
       01 WS-RETURN-CODE    PIC S9(9) COMP VALUE 0.
```

## 2.6 条件名（88 级）

`88` 级定义条件名，类似于枚举或布尔值：

```cobol
       01 WS-STATUS         PIC 9 VALUE 0.
          88 STATUS-OK      VALUE 0.
          88 STATUS-ERROR   VALUE 1.
          88 STATUS-WARNING VALUE 2.
          88 STATUS-FATAL   VALUES 3 THRU 9.
       
       PROCEDURE DIVISION.
           IF STATUS-OK
               DISPLAY "Everything is fine"
           END-IF
           
           SET STATUS-ERROR TO TRUE    *> 将 WS-STATUS 设为 1
```

## 2.7 本项目中的数据定义

让我们看看 `ledger` 项目中的实际数据定义：

```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      *> 返回码（与 C 函数交互，必须用 COMP-5）
       01 WS-RC                    PIC S9(9) COMP-5 VALUE 0.
       01 WS-RETURN-CODE           PIC 9 VALUE 0.

      *> 命令行参数
       01 WS-COMMAND               PIC X(20) VALUE SPACES.
       01 WS-ARG-TYPE              PIC X(8) VALUE SPACES.
       01 WS-ARG-AMOUNT            PIC X(20) VALUE SPACES.
       01 WS-ARG-CATEGORY          PIC X(51) VALUE SPACES.
       01 WS-ARG-DATE              PIC X(11) VALUE SPACES.
       
      *> 金额处理
      *> 内部用整数"分"存储，避免浮点误差
       01 WS-AMOUNT-CENTS          PIC S9(18) COMP-5 VALUE 0.
      *> 用于计算的数值型
       01 WS-AMOUNT-NUM            PIC 9(10)V99 VALUE 0.
      *> 用于显示的编辑型
       01 WS-AMOUNT-DISP           PIC Z(9)9.99.

      *> 查询结果（与 C 函数交互）
       01 WS-LIST-ID               PIC S9(9) COMP-5 VALUE 0.
       01 WS-LIST-DATE             PIC X(11) VALUE SPACES.
       01 WS-LIST-TYPE             PIC X(8) VALUE SPACES.
       01 WS-LIST-CATEGORY         PIC X(51) VALUE SPACES.
       01 WS-LIST-AMOUNT           PIC S9(18) COMP-5 VALUE 0.
       01 WS-LIST-NOTE             PIC X(201) VALUE SPACES.
```

**设计要点：**

1. **字符串长度 +1**：`PIC X(51)` 而不是 `PIC X(50)`，为 C 的 null 终止符留空间

2. **金额用整数存储**：`3000000` 分 = `30000.00` 元，避免浮点误差

3. **COMP-5 用于 C 互操作**：确保与 C 的 `int`、`long` 兼容

## 2.8 练习

1. 定义一个学生记录，包含：学号（6位数字）、姓名（20字符）、成绩（3位整数）

2. 定义一个金额变量，能存储 -999999999.99 到 999999999.99

3. 定义一个状态变量，有三个条件名：ACTIVE、INACTIVE、DELETED

## 下一章预告

下一章我们将学习 COBOL 的过程逻辑，包括条件判断、循环、段落调用等。

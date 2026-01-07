# 第四章：命令行参数解析

## 4.1 命令行参数基础

当用户运行程序时，可以传入参数：

```bash
./ledger add --type expense --amount 100 --category food --date 2026-01-07
```

这里：
- `add` 是主命令
- `--type expense` 是一个选项及其值
- `--amount 100` 是另一个选项及其值

## 4.2 GnuCOBOL 的参数处理

GnuCOBOL 提供了两个特殊的 ACCEPT 语句来获取命令行参数：

### 4.2.1 获取参数数量

```cobol
       01 WS-ARG-COUNT    PIC 9(3) VALUE 0.
       
       PROCEDURE DIVISION.
           ACCEPT WS-ARG-COUNT FROM ARGUMENT-NUMBER
           DISPLAY "Total arguments: " WS-ARG-COUNT
```

**注意：** 参数数量不包括程序名本身。

### 4.2.2 获取参数值

```cobol
       01 WS-ARG-IDX      PIC 9(3) VALUE 0.
       01 WS-ARG-VALUE    PIC X(200) VALUE SPACES.
       
       PROCEDURE DIVISION.
           MOVE 1 TO WS-ARG-IDX
           ACCEPT WS-ARG-VALUE FROM ARGUMENT-VALUE
           DISPLAY "First argument: " WS-ARG-VALUE
```

**关键点：**
- 先设置 `WS-ARG-IDX` 为想要获取的参数位置（从 1 开始）
- 然后 `ACCEPT ... FROM ARGUMENT-VALUE` 会读取该位置的参数

### 4.2.3 遍历所有参数

```cobol
       01 WS-ARG-COUNT    PIC 9(3) VALUE 0.
       01 WS-ARG-IDX      PIC 9(3) VALUE 0.
       01 WS-ARG-VALUE    PIC X(200) VALUE SPACES.
       
       PROCEDURE DIVISION.
           ACCEPT WS-ARG-COUNT FROM ARGUMENT-NUMBER
           
           PERFORM VARYING WS-ARG-IDX FROM 1 BY 1 
                   UNTIL WS-ARG-IDX > WS-ARG-COUNT
               ACCEPT WS-ARG-VALUE FROM ARGUMENT-VALUE
               DISPLAY "Arg " WS-ARG-IDX ": " 
                       FUNCTION TRIM(WS-ARG-VALUE)
           END-PERFORM
           
           STOP RUN.
```

运行示例：
```bash
./program hello world foo bar
# 输出：
# Arg 1: hello
# Arg 2: world
# Arg 3: foo
# Arg 4: bar
```

## 4.3 解析选项和值

大多数 CLI 工具使用 `--option value` 格式。我们需要：
1. 识别选项名（如 `--type`）
2. 获取下一个参数作为值（如 `expense`）

### 4.3.1 基本解析逻辑

```cobol
       WORKING-STORAGE SECTION.
       01 WS-ARG-COUNT         PIC 9(3) VALUE 0.
       01 WS-ARG-IDX           PIC 9(3) VALUE 0.
       01 WS-CURRENT-ARG       PIC X(200) VALUE SPACES.
       01 WS-NEXT-ARG          PIC X(200) VALUE SPACES.
       01 WS-ARG-LOWER         PIC X(200) VALUE SPACES.
       
      *> 解析后的参数
       01 WS-COMMAND           PIC X(20) VALUE SPACES.
       01 WS-ARG-TYPE          PIC X(8) VALUE SPACES.
       01 WS-ARG-AMOUNT        PIC X(20) VALUE SPACES.
       
       PROCEDURE DIVISION.
       PARSE-ARGUMENTS.
           ACCEPT WS-ARG-COUNT FROM ARGUMENT-NUMBER
           
           IF WS-ARG-COUNT = 0
               MOVE "help" TO WS-COMMAND
               EXIT PARAGRAPH
           END-IF
           
      *>   第一个参数是主命令
           MOVE 1 TO WS-ARG-IDX
           ACCEPT WS-CURRENT-ARG FROM ARGUMENT-VALUE
           MOVE FUNCTION LOWER-CASE(FUNCTION TRIM(WS-CURRENT-ARG))
                TO WS-COMMAND
           
      *>   解析后续参数
           ADD 1 TO WS-ARG-IDX
           PERFORM UNTIL WS-ARG-IDX > WS-ARG-COUNT
               ACCEPT WS-CURRENT-ARG FROM ARGUMENT-VALUE
               MOVE FUNCTION LOWER-CASE(FUNCTION TRIM(WS-CURRENT-ARG))
                    TO WS-ARG-LOWER
               
               EVALUATE TRUE
                   WHEN WS-ARG-LOWER = "--type"
      *>               获取下一个参数作为值
                       ADD 1 TO WS-ARG-IDX
                       IF WS-ARG-IDX <= WS-ARG-COUNT
                           ACCEPT WS-NEXT-ARG FROM ARGUMENT-VALUE
                           MOVE FUNCTION UPPER-CASE(
                                FUNCTION TRIM(WS-NEXT-ARG))
                                TO WS-ARG-TYPE
                       END-IF
                       
                   WHEN WS-ARG-LOWER = "--amount"
                       ADD 1 TO WS-ARG-IDX
                       IF WS-ARG-IDX <= WS-ARG-COUNT
                           ACCEPT WS-NEXT-ARG FROM ARGUMENT-VALUE
                           MOVE FUNCTION TRIM(WS-NEXT-ARG)
                                TO WS-ARG-AMOUNT
                       END-IF
                       
                   WHEN OTHER
      *>               未知选项，可能是子命令
                       CONTINUE
               END-EVALUATE
               
               ADD 1 TO WS-ARG-IDX
           END-PERFORM.
```

## 4.4 完整的参数解析器

以下是 `ledger` 项目中完整的参数解析代码：

```cobol
       PARSE-ARGUMENTS.
           ACCEPT WS-ARG-COUNT FROM ARGUMENT-NUMBER
           IF WS-ARG-COUNT = 0
               MOVE "help" TO WS-COMMAND
               EXIT PARAGRAPH
           END-IF

      *>   获取主命令
           MOVE 1 TO WS-ARG-IDX
           ACCEPT WS-CURRENT-ARG FROM ARGUMENT-VALUE
           MOVE FUNCTION LOWER-CASE(FUNCTION TRIM(WS-CURRENT-ARG))
                TO WS-COMMAND

      *>   解析后续参数
           ADD 1 TO WS-ARG-IDX
           PERFORM UNTIL WS-ARG-IDX > WS-ARG-COUNT
               ACCEPT WS-CURRENT-ARG FROM ARGUMENT-VALUE
               MOVE FUNCTION LOWER-CASE(FUNCTION TRIM(WS-CURRENT-ARG))
                    TO WS-ARG-LOWER

               EVALUATE TRUE
                   WHEN WS-ARG-LOWER = "--type"
                       ADD 1 TO WS-ARG-IDX
                       IF WS-ARG-IDX <= WS-ARG-COUNT
                           ACCEPT WS-NEXT-ARG FROM ARGUMENT-VALUE
                           MOVE FUNCTION UPPER-CASE(
                                FUNCTION TRIM(WS-NEXT-ARG))
                                TO WS-ARG-TYPE
                       END-IF
                       
                   WHEN WS-ARG-LOWER = "--amount"
                       ADD 1 TO WS-ARG-IDX
                       IF WS-ARG-IDX <= WS-ARG-COUNT
                           ACCEPT WS-NEXT-ARG FROM ARGUMENT-VALUE
                           MOVE FUNCTION TRIM(WS-NEXT-ARG)
                                TO WS-ARG-AMOUNT
                       END-IF
                       
                   WHEN WS-ARG-LOWER = "--category"
                       ADD 1 TO WS-ARG-IDX
                       IF WS-ARG-IDX <= WS-ARG-COUNT
                           ACCEPT WS-NEXT-ARG FROM ARGUMENT-VALUE
                           MOVE FUNCTION TRIM(WS-NEXT-ARG)
                                TO WS-ARG-CATEGORY
                       END-IF
                       
                   WHEN WS-ARG-LOWER = "--date"
                       ADD 1 TO WS-ARG-IDX
                       IF WS-ARG-IDX <= WS-ARG-COUNT
                           ACCEPT WS-NEXT-ARG FROM ARGUMENT-VALUE
                           MOVE FUNCTION TRIM(WS-NEXT-ARG)
                                TO WS-ARG-DATE
                       END-IF
                       
                   WHEN WS-ARG-LOWER = "--note"
                       ADD 1 TO WS-ARG-IDX
                       IF WS-ARG-IDX <= WS-ARG-COUNT
                           ACCEPT WS-NEXT-ARG FROM ARGUMENT-VALUE
                           MOVE FUNCTION TRIM(WS-NEXT-ARG)
                                TO WS-ARG-NOTE
                       END-IF
                       
                   WHEN WS-ARG-LOWER = "--id"
                       ADD 1 TO WS-ARG-IDX
                       IF WS-ARG-IDX <= WS-ARG-COUNT
                           ACCEPT WS-NEXT-ARG FROM ARGUMENT-VALUE
                           COMPUTE WS-ARG-ID = FUNCTION NUMVAL(
                                   FUNCTION TRIM(WS-NEXT-ARG))
                       END-IF
                       
                   WHEN WS-ARG-LOWER = "--month"
                       ADD 1 TO WS-ARG-IDX
                       IF WS-ARG-IDX <= WS-ARG-COUNT
                           ACCEPT WS-NEXT-ARG FROM ARGUMENT-VALUE
                           MOVE FUNCTION TRIM(WS-NEXT-ARG)
                                TO WS-ARG-MONTH
                       END-IF
                       
                   WHEN WS-ARG-LOWER = "--limit"
                       ADD 1 TO WS-ARG-IDX
                       IF WS-ARG-IDX <= WS-ARG-COUNT
                           ACCEPT WS-NEXT-ARG FROM ARGUMENT-VALUE
                           COMPUTE WS-ARG-LIMIT = FUNCTION NUMVAL(
                                   FUNCTION TRIM(WS-NEXT-ARG))
                       END-IF
                       
                   WHEN WS-ARG-LOWER = "--yes"
      *>               布尔标志，不需要值
                       MOVE 1 TO WS-ARG-YES
                       
                   WHEN WS-ARG-LOWER = "--db"
                       ADD 1 TO WS-ARG-IDX
                       IF WS-ARG-IDX <= WS-ARG-COUNT
                           ACCEPT WS-NEXT-ARG FROM ARGUMENT-VALUE
                           MOVE FUNCTION TRIM(WS-NEXT-ARG)
                                TO WS-DB-PATH
                       END-IF
                       
                   WHEN OTHER
      *>               可能是子命令（如 report month 中的 month）
                       IF WS-SUBCOMMAND = SPACES
                           MOVE FUNCTION LOWER-CASE(
                                FUNCTION TRIM(WS-CURRENT-ARG))
                                TO WS-SUBCOMMAND
                       END-IF
               END-EVALUATE
               
               ADD 1 TO WS-ARG-IDX
           END-PERFORM.
```

## 4.5 环境变量

除了命令行参数，还可以从环境变量读取配置：

```cobol
       RESOLVE-DB-PATH.
      *>   优先级：命令行参数 > 环境变量 > 默认值
           IF WS-DB-PATH NOT = SPACES
               EXIT PARAGRAPH
           END-IF
           
           ACCEPT WS-DB-PATH FROM ENVIRONMENT "LEDGER_DB"
           
           IF WS-DB-PATH = SPACES
               MOVE "./db/ledger.db" TO WS-DB-PATH
           END-IF.
```

使用：
```bash
# 使用环境变量
export LEDGER_DB=/path/to/my/ledger.db
./ledger list

# 或者命令行参数覆盖
./ledger list --db /another/path/ledger.db
```

## 4.6 帮助信息

良好的 CLI 工具应该提供帮助信息：

```cobol
       SHOW-HELP.
           DISPLAY "Ledger - COBOL CLI Accounting Tool"
           DISPLAY " "
           DISPLAY "Usage: ledger <command> [options]"
           DISPLAY " "
           DISPLAY "Commands:"
           DISPLAY "  init                    Initialize database"
           DISPLAY "  add                     Add transaction"
           DISPLAY "  list                    List transactions"
           DISPLAY "  update                  Update transaction"
           DISPLAY "  delete                  Delete transaction"
           DISPLAY "  report month            Monthly report"
           DISPLAY "  export tx               Export transactions CSV"
           DISPLAY "  help                    Show help"
           DISPLAY " "
           DISPLAY "add options:"
           DISPLAY "  --type income|expense   Type (required)"
           DISPLAY "  --amount 123.45         Amount (required)"
           DISPLAY "  --category food         Category (required)"
           DISPLAY "  --date 2026-01-07       Date (required)"
           DISPLAY "  --note 'note'           Note (optional)"
           DISPLAY " "
           DISPLAY "Return codes:"
           DISPLAY "  0 - Success"
           DISPLAY "  1 - Parameter error"
           DISPLAY "  2 - Database error"
           DISPLAY "  3 - Not found"
           DISPLAY "  4 - I/O error".
```

## 4.7 返回码

程序应该返回适当的退出码：

```cobol
       WORKING-STORAGE SECTION.
       01 WS-RETURN-CODE    PIC 9 VALUE 0.
       
       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           ...
           
      *>   设置返回码并退出
           MOVE WS-RETURN-CODE TO RETURN-CODE
           STOP RUN.
```

在 shell 中检查返回码：
```bash
./ledger add --type expense --amount 100 --category food --date 2026-01-07
echo $?  # 输出 0 表示成功

./ledger add  # 缺少参数
echo $?  # 输出 1 表示参数错误
```

## 4.8 练习

1. 写一个程序，接受 `--name` 和 `--age` 参数，输出 "Hello, {name}! You are {age} years old."

2. 添加 `--help` 选项，显示使用说明

3. 添加参数校验：name 不能为空，age 必须是正整数

## 下一章预告

下一章我们将学习 COBOL 如何调用 C 函数，这是访问系统功能和外部库的关键技术。

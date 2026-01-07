# 第七章：完整功能实现

## 7.1 项目回顾

到目前为止，我们已经学习了：
- COBOL 程序结构和语法
- 数据定义和变量
- 过程逻辑和控制流
- 命令行参数解析
- COBOL 调用 C 函数
- SQLite 数据库操作

现在让我们把这些知识整合起来，完成 `ledger` 工具的所有功能。

## 7.2 完整的 main.cbl

以下是主程序的完整结构：

```cobol
      *> ============================================================
      *> main.cbl - Ledger 主程序
      *> 命令行记账工具 - 使用 C 包装函数访问 SQLite
      *> ============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LEDGER-MAIN.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
      *> === 返回码 ===
       01 WS-RC                    PIC S9(9) COMP-5 VALUE 0.
       01 WS-RETURN-CODE           PIC 9 VALUE 0.

      *> === 参数解析 ===
       01 WS-ARG-COUNT             PIC 9(3) VALUE 0.
       01 WS-ARG-IDX               PIC 9(3) VALUE 0.
       01 WS-CURRENT-ARG           PIC X(200) VALUE SPACES.
       01 WS-NEXT-ARG              PIC X(200) VALUE SPACES.
       01 WS-ARG-LOWER             PIC X(200) VALUE SPACES.

      *> === 命令和参数 ===
       01 WS-COMMAND               PIC X(20) VALUE SPACES.
       01 WS-SUBCOMMAND            PIC X(20) VALUE SPACES.
       01 WS-ARG-TYPE              PIC X(8) VALUE SPACES.
       01 WS-ARG-AMOUNT            PIC X(20) VALUE SPACES.
       01 WS-ARG-CATEGORY          PIC X(51) VALUE SPACES.
       01 WS-ARG-DATE              PIC X(11) VALUE SPACES.
       01 WS-ARG-NOTE              PIC X(201) VALUE SPACES.
       01 WS-ARG-ID                PIC 9(10) VALUE 0.
       01 WS-ARG-FROM              PIC X(11) VALUE SPACES.
       01 WS-ARG-TO                PIC X(11) VALUE SPACES.
       01 WS-ARG-MONTH             PIC X(8) VALUE SPACES.
       01 WS-ARG-KEYWORD           PIC X(51) VALUE SPACES.
       01 WS-ARG-LIMIT             PIC S9(9) COMP-5 VALUE 50.
       01 WS-ARG-OFFSET            PIC S9(9) COMP-5 VALUE 0.
       01 WS-ARG-YES               PIC 9 VALUE 0.
       01 WS-DB-PATH               PIC X(201) VALUE SPACES.
       01 WS-OUT-PATH              PIC X(201) VALUE SPACES.

      *> === 金额处理 ===
       01 WS-AMOUNT-CENTS          PIC S9(18) COMP-5 VALUE 0.
       01 WS-AMOUNT-NUM            PIC 9(10)V99 VALUE 0.
       01 WS-AMOUNT-DISP           PIC Z(9)9.99.

      *> === 查询结果 ===
       01 WS-LIST-ID               PIC S9(9) COMP-5 VALUE 0.
       01 WS-LIST-ID-DISP          PIC Z(8)9.
       01 WS-LIST-DATE             PIC X(11) VALUE SPACES.
       01 WS-LIST-TYPE             PIC X(8) VALUE SPACES.
       01 WS-LIST-CATEGORY         PIC X(51) VALUE SPACES.
       01 WS-LIST-AMOUNT           PIC S9(18) COMP-5 VALUE 0.
       01 WS-LIST-NOTE             PIC X(201) VALUE SPACES.

      *> === 报表数据 ===
       01 WS-RPT-INCOME            PIC S9(18) COMP-5 VALUE 0.
       01 WS-RPT-EXPENSE           PIC S9(18) COMP-5 VALUE 0.
       01 WS-RPT-NET               PIC S9(18) COMP-5 VALUE 0.
       01 WS-RPT-CAT               PIC X(51) VALUE SPACES.
       01 WS-RPT-TYPE              PIC X(8) VALUE SPACES.
       01 WS-RPT-TOTAL             PIC S9(18) COMP-5 VALUE 0.
       01 WS-DISP-INCOME           PIC Z(9)9.99.
       01 WS-DISP-EXPENSE          PIC Z(9)9.99.
       01 WS-DISP-NET              PIC -(9)9.99.

      *> === 用户输入 ===
       01 WS-USER-INPUT            PIC X VALUE SPACE.

       PROCEDURE DIVISION.
       
      *> ============================================================
      *> 主程序入口
      *> ============================================================
       MAIN-PROGRAM.
           PERFORM PARSE-ARGUMENTS
           PERFORM RESOLVE-DB-PATH

           EVALUATE TRUE
               WHEN WS-COMMAND = "init"
                   PERFORM CMD-INIT
               WHEN WS-COMMAND = "add"
                   PERFORM CMD-ADD
               WHEN WS-COMMAND = "list"
                   PERFORM CMD-LIST
               WHEN WS-COMMAND = "update"
                   PERFORM CMD-UPDATE
               WHEN WS-COMMAND = "delete"
                   PERFORM CMD-DELETE
               WHEN WS-COMMAND = "report"
                   PERFORM CMD-REPORT
               WHEN WS-COMMAND = "export"
                   PERFORM CMD-EXPORT
               WHEN WS-COMMAND = "help"
                   PERFORM SHOW-HELP
               WHEN OTHER
                   PERFORM SHOW-HELP
                   MOVE 1 TO WS-RETURN-CODE
           END-EVALUATE

           CALL "db_close" RETURNING WS-RC
           MOVE WS-RETURN-CODE TO RETURN-CODE
           STOP RUN.
```

## 7.3 各命令的实现

### 7.3.1 init 命令

```cobol
       CMD-INIT.
           DISPLAY "Initializing database: " 
                   FUNCTION TRIM(WS-DB-PATH)
           
           CALL "db_open" USING BY REFERENCE WS-DB-PATH
                          RETURNING WS-RC
           IF WS-RC NOT = 0
               DISPLAY "Error: Cannot open database"
               MOVE 2 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           CALL "db_init_schema" RETURNING WS-RC
           IF WS-RC NOT = 0
               DISPLAY "Error: Cannot create schema"
               MOVE 2 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           DISPLAY "Database initialized successfully".
```

### 7.3.2 add 命令

```cobol
       CMD-ADD.
      *>   校验必填参数
           IF WS-ARG-TYPE = SPACES
               DISPLAY "Error: --type is required"
               MOVE 1 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           IF WS-ARG-AMOUNT = SPACES
               DISPLAY "Error: --amount is required"
               MOVE 1 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           IF WS-ARG-CATEGORY = SPACES
               DISPLAY "Error: --category is required"
               MOVE 1 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           IF WS-ARG-DATE = SPACES
               DISPLAY "Error: --date is required"
               MOVE 1 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

      *>   校验类型
           IF WS-ARG-TYPE NOT = "INCOME" AND WS-ARG-TYPE NOT = "EXPENSE"
               DISPLAY "Error: type must be INCOME or EXPENSE"
               MOVE 1 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

      *>   转换金额为分
           COMPUTE WS-AMOUNT-NUM = 
                   FUNCTION NUMVAL(FUNCTION TRIM(WS-ARG-AMOUNT))
           IF WS-AMOUNT-NUM <= 0
               DISPLAY "Error: amount must be greater than 0"
               MOVE 1 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           COMPUTE WS-AMOUNT-CENTS = WS-AMOUNT-NUM * 100

      *>   打开数据库
           CALL "db_open" USING BY REFERENCE WS-DB-PATH
                          RETURNING WS-RC
           IF WS-RC NOT = 0
               DISPLAY "Error: Cannot open database"
               MOVE 2 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

      *>   插入记录
           CALL "tx_add" USING BY REFERENCE WS-ARG-DATE
                               BY REFERENCE WS-ARG-TYPE
                               BY REFERENCE WS-ARG-CATEGORY
                               BY VALUE WS-AMOUNT-CENTS
                               BY REFERENCE WS-ARG-NOTE
                         RETURNING WS-RC
           IF WS-RC NOT = 0
               DISPLAY "Error: Failed to add transaction"
               MOVE 2 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           DISPLAY "Transaction added successfully".
```

### 7.3.3 list 命令

```cobol
       CMD-LIST.
           CALL "db_open" USING BY REFERENCE WS-DB-PATH
                          RETURNING WS-RC
           IF WS-RC NOT = 0
               DISPLAY "Error: Cannot open database"
               MOVE 2 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           CALL "tx_list_begin" 
                USING BY REFERENCE WS-ARG-FROM
                      BY REFERENCE WS-ARG-TO
                      BY REFERENCE WS-ARG-MONTH
                      BY REFERENCE WS-ARG-CATEGORY
                      BY REFERENCE WS-ARG-TYPE
                      BY REFERENCE WS-ARG-KEYWORD
                      BY VALUE WS-ARG-LIMIT
                      BY VALUE WS-ARG-OFFSET
                RETURNING WS-RC
           IF WS-RC NOT = 0
               DISPLAY "Error: Query failed"
               MOVE 2 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

      *>   输出表头
           DISPLAY "ID        DATE        TYPE     CATEGORY"
                   "            AMOUNT       NOTE"
           DISPLAY "--------- ----------  -------  "
                   "---------------  -----------  ----------"

      *>   循环输出结果
           MOVE 0 TO WS-RC
           PERFORM UNTIL WS-RC NOT = 0
               CALL "tx_list_next"
                    USING BY REFERENCE WS-LIST-ID
                          BY REFERENCE WS-LIST-DATE
                          BY REFERENCE WS-LIST-TYPE
                          BY REFERENCE WS-LIST-CATEGORY
                          BY REFERENCE WS-LIST-AMOUNT
                          BY REFERENCE WS-LIST-NOTE
                    RETURNING WS-RC
               IF WS-RC = 0
                   COMPUTE WS-AMOUNT-DISP = WS-LIST-AMOUNT / 100
                   MOVE WS-LIST-ID TO WS-LIST-ID-DISP
                   DISPLAY WS-LIST-ID-DISP "  "
                           FUNCTION TRIM(WS-LIST-DATE) "  "
                           FUNCTION TRIM(WS-LIST-TYPE) "  "
                           FUNCTION TRIM(WS-LIST-CATEGORY) "  "
                           WS-AMOUNT-DISP "  "
                           FUNCTION TRIM(WS-LIST-NOTE)
               END-IF
           END-PERFORM

           CALL "tx_list_end" RETURNING WS-RC.
```

### 7.3.4 delete 命令

```cobol
       CMD-DELETE.
           IF WS-ARG-ID = 0
               DISPLAY "Error: --id is required"
               MOVE 1 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

      *>   确认删除（除非指定 --yes）
           IF WS-ARG-YES = 0
               DISPLAY "Delete transaction ID " WS-ARG-ID "? (y/N): "
                       WITH NO ADVANCING
               ACCEPT WS-USER-INPUT FROM CONSOLE
               IF FUNCTION UPPER-CASE(WS-USER-INPUT) NOT = "Y"
                   DISPLAY "Cancelled"
                   EXIT PARAGRAPH
               END-IF
           END-IF

           CALL "db_open" USING BY REFERENCE WS-DB-PATH
                          RETURNING WS-RC
           IF WS-RC NOT = 0
               DISPLAY "Error: Cannot open database"
               MOVE 2 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           CALL "tx_delete" USING BY VALUE WS-ARG-ID
                            RETURNING WS-RC
           IF WS-RC = 3
               DISPLAY "Error: Transaction ID " WS-ARG-ID " not found"
               MOVE 3 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           IF WS-RC NOT = 0
               DISPLAY "Error: Delete failed"
               MOVE 2 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           DISPLAY "Transaction " WS-ARG-ID " deleted successfully".
```

### 7.3.5 report 命令

```cobol
       CMD-REPORT.
           IF WS-SUBCOMMAND NOT = "month"
               DISPLAY "Error: report requires subcommand 'month'"
               DISPLAY "Usage: ledger report month --month YYYY-MM"
               MOVE 1 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           IF WS-ARG-MONTH = SPACES
               DISPLAY "Error: --month is required"
               MOVE 1 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           CALL "db_open" USING BY REFERENCE WS-DB-PATH
                          RETURNING WS-RC
           IF WS-RC NOT = 0
               DISPLAY "Error: Cannot open database"
               MOVE 2 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

      *>   获取汇总数据
           CALL "rpt_get_totals" 
                USING BY REFERENCE WS-ARG-MONTH
                      BY REFERENCE WS-RPT-INCOME
                      BY REFERENCE WS-RPT-EXPENSE
                RETURNING WS-RC

           COMPUTE WS-RPT-NET = WS-RPT-INCOME - WS-RPT-EXPENSE

      *>   输出报表
           DISPLAY " "
           DISPLAY "Month: " FUNCTION TRIM(WS-ARG-MONTH)
           DISPLAY " "

           COMPUTE WS-DISP-INCOME = WS-RPT-INCOME / 100
           COMPUTE WS-DISP-EXPENSE = WS-RPT-EXPENSE / 100
           COMPUTE WS-DISP-NET = WS-RPT-NET / 100

           DISPLAY "Total Income:  " WS-DISP-INCOME
           DISPLAY "Total Expense: " WS-DISP-EXPENSE
           DISPLAY "Net:           " WS-DISP-NET
           DISPLAY " "

      *>   按类别汇总 - 支出
           CALL "rpt_category_begin" 
                USING BY REFERENCE WS-ARG-MONTH
                RETURNING WS-RC

           DISPLAY "Expense by Category:"
           MOVE 0 TO WS-RC
           PERFORM UNTIL WS-RC NOT = 0
               CALL "rpt_category_next"
                    USING BY REFERENCE WS-RPT-CAT
                          BY REFERENCE WS-RPT-TYPE
                          BY REFERENCE WS-RPT-TOTAL
                    RETURNING WS-RC
               IF WS-RC = 0 AND WS-RPT-TYPE = "EXPENSE"
                   COMPUTE WS-AMOUNT-DISP = WS-RPT-TOTAL / 100
                   DISPLAY "- " FUNCTION TRIM(WS-RPT-CAT) 
                           "  " WS-AMOUNT-DISP
               END-IF
           END-PERFORM
           CALL "rpt_category_end" RETURNING WS-RC

           DISPLAY " "
           
      *>   按类别汇总 - 收入
           DISPLAY "Income by Category:"
           CALL "rpt_category_begin" 
                USING BY REFERENCE WS-ARG-MONTH
                RETURNING WS-RC
           MOVE 0 TO WS-RC
           PERFORM UNTIL WS-RC NOT = 0
               CALL "rpt_category_next"
                    USING BY REFERENCE WS-RPT-CAT
                          BY REFERENCE WS-RPT-TYPE
                          BY REFERENCE WS-RPT-TOTAL
                    RETURNING WS-RC
               IF WS-RC = 0 AND WS-RPT-TYPE = "INCOME"
                   COMPUTE WS-AMOUNT-DISP = WS-RPT-TOTAL / 100
                   DISPLAY "- " FUNCTION TRIM(WS-RPT-CAT) 
                           "  " WS-AMOUNT-DISP
               END-IF
           END-PERFORM
           CALL "rpt_category_end" RETURNING WS-RC.
```

## 7.4 运行示例

```bash
# 初始化
./ledger init

# 添加数据
./ledger add --type income --amount 30000 --category salary --date 2026-01-07 --note "January salary"
./ledger add --type expense --amount 125.50 --category food --date 2026-01-07 --note "Lunch"
./ledger add --type expense --amount 50 --category transport --date 2026-01-07

# 查看列表
./ledger list

# 按月份筛选
./ledger list --month 2026-01

# 生成报表
./ledger report month --month 2026-01

# 删除（需确认）
./ledger delete --id 3

# 删除（跳过确认）
./ledger delete --id 3 --yes
```

## 7.5 错误处理最佳实践

1. **参数校验在前**：先检查所有必填参数，再执行操作
2. **明确的错误信息**：告诉用户哪里出错了
3. **适当的返回码**：让脚本可以判断执行结果
4. **资源清理**：确保数据库连接被关闭

```cobol
      *> 好的错误处理模式
       CMD-ADD.
      *>   1. 先校验所有参数
           IF WS-ARG-TYPE = SPACES
               DISPLAY "Error: --type is required"
               MOVE 1 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           
      *>   2. 再执行操作
           CALL "db_open" ...
           
      *>   3. 检查每个操作的结果
           IF WS-RC NOT = 0
               DISPLAY "Error: specific error message"
               MOVE appropriate-code TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           
      *>   4. 成功时给出反馈
           DISPLAY "Operation completed successfully".
```

## 7.6 练习

1. 添加 `--from` 和 `--to` 参数的日期格式校验

2. 实现 `update` 命令的完整功能

3. 添加一个 `stats` 命令，显示总交易数、总收入、总支出

## 下一章预告

下一章我们将学习如何编译、测试和部署这个项目。

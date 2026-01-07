      *> ============================================================
      *> main.cbl - Ledger 主程序
      *> 命令行记账工具 - 使用 C 包装函数访问 SQLite
      *> ============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LEDGER-MAIN.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *> 返回码
       01 WS-RC                    PIC S9(9) COMP-5 VALUE 0.
       01 WS-RETURN-CODE           PIC 9 VALUE 0.

      *> 参数解析
       01 WS-ARG-COUNT             PIC 9(3) VALUE 0.
       01 WS-ARG-IDX               PIC 9(3) VALUE 0.
       01 WS-CURRENT-ARG           PIC X(200) VALUE SPACES.
       01 WS-NEXT-ARG              PIC X(200) VALUE SPACES.
       01 WS-ARG-LOWER             PIC X(200) VALUE SPACES.

      *> 命令和参数
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

      *> 金额处理
       01 WS-AMOUNT-CENTS          PIC S9(18) COMP-5 VALUE 0.
       01 WS-AMOUNT-NUM            PIC 9(10)V99 VALUE 0.
       01 WS-AMOUNT-DISP           PIC Z(9)9.99.

      *> 列表查询结果
       01 WS-LIST-ID               PIC S9(9) COMP-5 VALUE 0.
       01 WS-LIST-ID-DISP          PIC Z(8)9.
       01 WS-LIST-DATE             PIC X(11) VALUE SPACES.
       01 WS-LIST-TYPE             PIC X(8) VALUE SPACES.
       01 WS-LIST-CATEGORY         PIC X(51) VALUE SPACES.
       01 WS-LIST-AMOUNT           PIC S9(18) COMP-5 VALUE 0.
       01 WS-LIST-NOTE             PIC X(201) VALUE SPACES.

      *> 报表数据
       01 WS-RPT-INCOME            PIC S9(18) COMP-5 VALUE 0.
       01 WS-RPT-EXPENSE           PIC S9(18) COMP-5 VALUE 0.
       01 WS-RPT-NET               PIC S9(18) COMP-5 VALUE 0.
       01 WS-RPT-CAT               PIC X(51) VALUE SPACES.
       01 WS-RPT-TYPE              PIC X(8) VALUE SPACES.
       01 WS-RPT-TOTAL             PIC S9(18) COMP-5 VALUE 0.
       01 WS-DISP-INCOME           PIC Z(9)9.99.
       01 WS-DISP-EXPENSE          PIC Z(9)9.99.
       01 WS-DISP-NET              PIC -(9)9.99.

      *> 用户输入
       01 WS-USER-INPUT            PIC X VALUE SPACE.

      *> CSV 导出
       01 WS-CSV-FILE              PIC X(201) VALUE SPACES.
       01 WS-TIMESTAMP             PIC X(14) VALUE SPACES.
       01 WS-CURRENT-DT.
          05 WS-DT-YEAR            PIC 9(4).
          05 WS-DT-MONTH           PIC 9(2).
          05 WS-DT-DAY             PIC 9(2).
          05 WS-DT-HOUR            PIC 9(2).
          05 WS-DT-MIN             PIC 9(2).
          05 WS-DT-SEC             PIC 9(2).
          05 FILLER                PIC 9(2).

       PROCEDURE DIVISION.
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


      *> ============================================================
      *> 解析命令行参数
      *> ============================================================
       PARSE-ARGUMENTS.
           ACCEPT WS-ARG-COUNT FROM ARGUMENT-NUMBER
           IF WS-ARG-COUNT = 0
               MOVE "help" TO WS-COMMAND
               EXIT PARAGRAPH
           END-IF

           MOVE 1 TO WS-ARG-IDX
           ACCEPT WS-CURRENT-ARG FROM ARGUMENT-VALUE
           MOVE FUNCTION LOWER-CASE(FUNCTION TRIM(WS-CURRENT-ARG))
                TO WS-COMMAND

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
                   WHEN WS-ARG-LOWER = "--from"
                       ADD 1 TO WS-ARG-IDX
                       IF WS-ARG-IDX <= WS-ARG-COUNT
                           ACCEPT WS-NEXT-ARG FROM ARGUMENT-VALUE
                           MOVE FUNCTION TRIM(WS-NEXT-ARG)
                                TO WS-ARG-FROM
                       END-IF
                   WHEN WS-ARG-LOWER = "--to"
                       ADD 1 TO WS-ARG-IDX
                       IF WS-ARG-IDX <= WS-ARG-COUNT
                           ACCEPT WS-NEXT-ARG FROM ARGUMENT-VALUE
                           MOVE FUNCTION TRIM(WS-NEXT-ARG)
                                TO WS-ARG-TO
                       END-IF
                   WHEN WS-ARG-LOWER = "--month"
                       ADD 1 TO WS-ARG-IDX
                       IF WS-ARG-IDX <= WS-ARG-COUNT
                           ACCEPT WS-NEXT-ARG FROM ARGUMENT-VALUE
                           MOVE FUNCTION TRIM(WS-NEXT-ARG)
                                TO WS-ARG-MONTH
                       END-IF
                   WHEN WS-ARG-LOWER = "--q"
                       ADD 1 TO WS-ARG-IDX
                       IF WS-ARG-IDX <= WS-ARG-COUNT
                           ACCEPT WS-NEXT-ARG FROM ARGUMENT-VALUE
                           MOVE FUNCTION TRIM(WS-NEXT-ARG)
                                TO WS-ARG-KEYWORD
                       END-IF
                   WHEN WS-ARG-LOWER = "--limit"
                       ADD 1 TO WS-ARG-IDX
                       IF WS-ARG-IDX <= WS-ARG-COUNT
                           ACCEPT WS-NEXT-ARG FROM ARGUMENT-VALUE
                           COMPUTE WS-ARG-LIMIT = FUNCTION NUMVAL(
                                   FUNCTION TRIM(WS-NEXT-ARG))
                       END-IF
                   WHEN WS-ARG-LOWER = "--offset"
                       ADD 1 TO WS-ARG-IDX
                       IF WS-ARG-IDX <= WS-ARG-COUNT
                           ACCEPT WS-NEXT-ARG FROM ARGUMENT-VALUE
                           COMPUTE WS-ARG-OFFSET = FUNCTION NUMVAL(
                                   FUNCTION TRIM(WS-NEXT-ARG))
                       END-IF
                   WHEN WS-ARG-LOWER = "--db"
                       ADD 1 TO WS-ARG-IDX
                       IF WS-ARG-IDX <= WS-ARG-COUNT
                           ACCEPT WS-NEXT-ARG FROM ARGUMENT-VALUE
                           MOVE FUNCTION TRIM(WS-NEXT-ARG)
                                TO WS-DB-PATH
                       END-IF
                   WHEN WS-ARG-LOWER = "--out"
                       ADD 1 TO WS-ARG-IDX
                       IF WS-ARG-IDX <= WS-ARG-COUNT
                           ACCEPT WS-NEXT-ARG FROM ARGUMENT-VALUE
                           MOVE FUNCTION TRIM(WS-NEXT-ARG)
                                TO WS-OUT-PATH
                       END-IF
                   WHEN WS-ARG-LOWER = "--yes"
                       MOVE 1 TO WS-ARG-YES
                   WHEN OTHER
                       IF WS-SUBCOMMAND = SPACES
                           MOVE FUNCTION LOWER-CASE(
                                FUNCTION TRIM(WS-CURRENT-ARG))
                                TO WS-SUBCOMMAND
                       END-IF
               END-EVALUATE
               ADD 1 TO WS-ARG-IDX
           END-PERFORM.

      *> ============================================================
      *> 解析数据库路径
      *> ============================================================
       RESOLVE-DB-PATH.
           IF WS-DB-PATH NOT = SPACES
               EXIT PARAGRAPH
           END-IF
           ACCEPT WS-DB-PATH FROM ENVIRONMENT "LEDGER_DB"
           IF WS-DB-PATH = SPACES
               MOVE "./db/ledger.db" TO WS-DB-PATH
           END-IF.


      *> ============================================================
      *> CMD: init - 初始化数据库
      *> ============================================================
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

      *> ============================================================
      *> CMD: add - 新增交易
      *> ============================================================
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


      *> ============================================================
      *> CMD: list - 查询交易列表
      *> ============================================================
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

      *> ============================================================
      *> CMD: update - 修改交易
      *> ============================================================
       CMD-UPDATE.
           IF WS-ARG-ID = 0
               DISPLAY "Error: --id is required"
               MOVE 1 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

      *>   校验类型（如果提供）
           IF WS-ARG-TYPE NOT = SPACES
               IF WS-ARG-TYPE NOT = "INCOME" 
                  AND WS-ARG-TYPE NOT = "EXPENSE"
                   DISPLAY "Error: type must be INCOME or EXPENSE"
                   MOVE 1 TO WS-RETURN-CODE
                   EXIT PARAGRAPH
               END-IF
           END-IF

      *>   转换金额（如果提供）
           MOVE 0 TO WS-AMOUNT-CENTS
           IF WS-ARG-AMOUNT NOT = SPACES
               COMPUTE WS-AMOUNT-NUM = 
                       FUNCTION NUMVAL(FUNCTION TRIM(WS-ARG-AMOUNT))
               IF WS-AMOUNT-NUM <= 0
                   DISPLAY "Error: amount must be greater than 0"
                   MOVE 1 TO WS-RETURN-CODE
                   EXIT PARAGRAPH
               END-IF
               COMPUTE WS-AMOUNT-CENTS = WS-AMOUNT-NUM * 100
           END-IF

           CALL "db_open" USING BY REFERENCE WS-DB-PATH
                          RETURNING WS-RC
           IF WS-RC NOT = 0
               DISPLAY "Error: Cannot open database"
               MOVE 2 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           CALL "tx_update" USING BY VALUE WS-ARG-ID
                                  BY REFERENCE WS-ARG-DATE
                                  BY REFERENCE WS-ARG-TYPE
                                  BY REFERENCE WS-ARG-CATEGORY
                                  BY VALUE WS-AMOUNT-CENTS
                                  BY REFERENCE WS-ARG-NOTE
                            RETURNING WS-RC
           IF WS-RC = 3
               DISPLAY "Error: Transaction ID " WS-ARG-ID " not found"
               MOVE 3 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF
           IF WS-RC NOT = 0
               DISPLAY "Error: Update failed"
               MOVE 2 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           DISPLAY "Transaction " WS-ARG-ID " updated successfully".

      *> ============================================================
      *> CMD: delete - 删除交易
      *> ============================================================
       CMD-DELETE.
           IF WS-ARG-ID = 0
               DISPLAY "Error: --id is required"
               MOVE 1 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

      *>   确认删除
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


      *> ============================================================
      *> CMD: report - 生成报表
      *> ============================================================
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

      *>   按类别汇总
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
               IF WS-RC = 0
                   COMPUTE WS-AMOUNT-DISP = WS-RPT-TOTAL / 100
                   IF FUNCTION TRIM(WS-RPT-TYPE) = "EXPENSE"
                       DISPLAY "- " FUNCTION TRIM(WS-RPT-CAT) 
                               "  " WS-AMOUNT-DISP
                   END-IF
               END-IF
           END-PERFORM
           CALL "rpt_category_end" RETURNING WS-RC

           DISPLAY " "
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
               IF WS-RC = 0
                   COMPUTE WS-AMOUNT-DISP = WS-RPT-TOTAL / 100
                   IF FUNCTION TRIM(WS-RPT-TYPE) = "INCOME"
                       DISPLAY "- " FUNCTION TRIM(WS-RPT-CAT) 
                               "  " WS-AMOUNT-DISP
                   END-IF
               END-IF
           END-PERFORM
           CALL "rpt_category_end" RETURNING WS-RC.


      *> ============================================================
      *> CMD: export - 导出 CSV
      *> ============================================================
       CMD-EXPORT.
           EVALUATE WS-SUBCOMMAND
               WHEN "tx"
                   PERFORM EXPORT-TX
               WHEN "report"
                   PERFORM EXPORT-REPORT
               WHEN OTHER
                   DISPLAY "Error: export requires subcommand"
                   DISPLAY "Usage: ledger export tx [filters]"
                   DISPLAY "       ledger export report --month YYYY-MM"
                   MOVE 1 TO WS-RETURN-CODE
           END-EVALUATE.

      *> ============================================================
      *> 导出交易明细 CSV
      *> ============================================================
       EXPORT-TX.
      *>   确定输出文件名
           IF WS-OUT-PATH NOT = SPACES
               MOVE WS-OUT-PATH TO WS-CSV-FILE
           ELSE
               MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DT
               STRING "./export/transactions_"
                      WS-DT-YEAR WS-DT-MONTH WS-DT-DAY "_"
                      WS-DT-HOUR WS-DT-MIN WS-DT-SEC
                      ".csv"
                      DELIMITED SIZE INTO WS-CSV-FILE
           END-IF

           CALL "db_open" USING BY REFERENCE WS-DB-PATH
                          RETURNING WS-RC
           IF WS-RC NOT = 0
               DISPLAY "Error: Cannot open database"
               MOVE 2 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

      *>   调用 C 函数导出
           CALL "export_tx_csv"
                USING BY REFERENCE WS-CSV-FILE
                      BY REFERENCE WS-ARG-FROM
                      BY REFERENCE WS-ARG-TO
                      BY REFERENCE WS-ARG-MONTH
                      BY REFERENCE WS-ARG-CATEGORY
                      BY REFERENCE WS-ARG-TYPE
                      BY REFERENCE WS-ARG-KEYWORD
                      BY VALUE WS-ARG-LIMIT
                      BY VALUE WS-ARG-OFFSET
                RETURNING WS-RC

           IF WS-RC NOT = 0
               DISPLAY "Error: Export failed"
               MOVE 4 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           DISPLAY "Exported to: " FUNCTION TRIM(WS-CSV-FILE).

      *> ============================================================
      *> 导出月度报表 CSV
      *> ============================================================
       EXPORT-REPORT.
           IF WS-ARG-MONTH = SPACES
               DISPLAY "Error: --month is required"
               MOVE 1 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

      *>   确定输出文件名
           IF WS-OUT-PATH NOT = SPACES
               MOVE WS-OUT-PATH TO WS-CSV-FILE
           ELSE
               STRING "./export/report_"
                      FUNCTION TRIM(WS-ARG-MONTH)
                      ".csv"
                      DELIMITED SIZE INTO WS-CSV-FILE
           END-IF

           CALL "db_open" USING BY REFERENCE WS-DB-PATH
                          RETURNING WS-RC
           IF WS-RC NOT = 0
               DISPLAY "Error: Cannot open database"
               MOVE 2 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

      *>   调用 C 函数导出
           CALL "export_report_csv"
                USING BY REFERENCE WS-CSV-FILE
                      BY REFERENCE WS-ARG-MONTH
                RETURNING WS-RC

           IF WS-RC NOT = 0
               DISPLAY "Error: Export failed"
               MOVE 4 TO WS-RETURN-CODE
               EXIT PARAGRAPH
           END-IF

           DISPLAY "Exported to: " FUNCTION TRIM(WS-CSV-FILE).


      *> ============================================================
      *> 显示帮助信息
      *> ============================================================
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
           DISPLAY "  export report           Export report CSV"
           DISPLAY "  help                    Show help"
           DISPLAY " "
           DISPLAY "add options:"
           DISPLAY "  --type income|expense   Type (required)"
           DISPLAY "  --amount 123.45         Amount (required)"
           DISPLAY "  --category food         Category (required)"
           DISPLAY "  --date 2026-01-07       Date (required)"
           DISPLAY "  --note 'note'           Note (optional)"
           DISPLAY " "
           DISPLAY "list options:"
           DISPLAY "  --from YYYY-MM-DD       Start date"
           DISPLAY "  --to YYYY-MM-DD         End date"
           DISPLAY "  --month YYYY-MM         Month"
           DISPLAY "  --category xxx          Category"
           DISPLAY "  --type income|expense   Type"
           DISPLAY "  --q keyword             Keyword search"
           DISPLAY "  --limit N               Limit (default 50)"
           DISPLAY "  --offset M              Offset (default 0)"
           DISPLAY " "
           DISPLAY "update options:"
           DISPLAY "  --id N                  Transaction ID (required)"
           DISPLAY "  --type/--amount/--category/--date/--note"
           DISPLAY " "
           DISPLAY "delete options:"
           DISPLAY "  --id N                  Transaction ID (required)"
           DISPLAY "  --yes                   Skip confirmation"
           DISPLAY " "
           DISPLAY "report options:"
           DISPLAY "  --month YYYY-MM         Month (required)"
           DISPLAY " "
           DISPLAY "Common options:"
           DISPLAY "  --db path               Database path"
           DISPLAY "  --out path              Output file path"
           DISPLAY " "
           DISPLAY "Return codes:"
           DISPLAY "  0 - Success"
           DISPLAY "  1 - Parameter error"
           DISPLAY "  2 - Database error"
           DISPLAY "  3 - Not found"
           DISPLAY "  4 - I/O error".

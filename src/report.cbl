      *> ============================================================
      *> report.cbl - 月度汇总报表逻辑
      *> ============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REPORT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY "common.cpy".
       COPY "types.cpy".

      *> 报表数据
       01 WS-TOTAL-INCOME          PIC S9(12) VALUE 0.
       01 WS-TOTAL-EXPENSE         PIC S9(12) VALUE 0.
       01 WS-NET-AMOUNT            PIC S9(12) VALUE 0.

      *> 显示格式
       01 WS-DISPLAY-INCOME        PIC Z(9)9.99.
       01 WS-DISPLAY-EXPENSE       PIC Z(9)9.99.
       01 WS-DISPLAY-NET           PIC -(9)9.99.
       01 WS-DISPLAY-AMOUNT        PIC Z(9)9.99.

      *> 类别汇总表（最多50个类别）
       01 WS-CATEGORY-TABLE.
          05 WS-CAT-ENTRY OCCURS 50 TIMES.
             10 WS-CAT-NAME-TBL    PIC X(50).
             10 WS-CAT-TYPE-TBL    PIC X(7).
             10 WS-CAT-TOTAL-TBL   PIC S9(12).

       01 WS-CAT-COUNT             PIC 9(2) VALUE 0.
       01 WS-CAT-IDX               PIC 9(2) VALUE 0.

      *> SQL 相关
       01 WS-SQL-TEXT              PIC X(1000) VALUE SPACES.
       01 WS-SQL-RESULT            PIC S9(9) COMP VALUE 0.

       PROCEDURE DIVISION.
       MAIN-REPORT.
           STOP RUN.

      *> ============================================================
      *> 生成月度报表
      *> ============================================================
       GENERATE-MONTHLY-REPORT SECTION.
      *>   校验月份参数
           IF WS-ARG-MONTH = SPACES
               DISPLAY "错误: 必须指定 --month 参数"
               MOVE 1 TO WS-RETURN-CODE
               EXIT SECTION
           END-IF

      *>   初始化
           MOVE 0 TO WS-TOTAL-INCOME
           MOVE 0 TO WS-TOTAL-EXPENSE
           MOVE 0 TO WS-CAT-COUNT

      *>   查询总收入
           STRING "SELECT COALESCE(SUM(amount_cents), 0) "
                  "FROM transactions "
                  "WHERE substr(tx_date, 1, 7) = '"
                  FUNCTION TRIM(WS-ARG-MONTH) "' "
                  "AND tx_type = 'INCOME';"
                  DELIMITED SIZE INTO WS-SQL-TEXT

           PERFORM EXECUTE-SUM-QUERY
           MOVE WS-RESULT-AMOUNT TO WS-TOTAL-INCOME

      *>   查询总支出
           STRING "SELECT COALESCE(SUM(amount_cents), 0) "
                  "FROM transactions "
                  "WHERE substr(tx_date, 1, 7) = '"
                  FUNCTION TRIM(WS-ARG-MONTH) "' "
                  "AND tx_type = 'EXPENSE';"
                  DELIMITED SIZE INTO WS-SQL-TEXT

           PERFORM EXECUTE-SUM-QUERY
           MOVE WS-RESULT-AMOUNT TO WS-TOTAL-EXPENSE

      *>   计算净额
           COMPUTE WS-NET-AMOUNT = WS-TOTAL-INCOME - WS-TOTAL-EXPENSE

      *>   查询按类别汇总
           STRING "SELECT category, tx_type, SUM(amount_cents) "
                  "FROM transactions "
                  "WHERE substr(tx_date, 1, 7) = '"
                  FUNCTION TRIM(WS-ARG-MONTH) "' "
                  "GROUP BY category, tx_type "
                  "ORDER BY tx_type, SUM(amount_cents) DESC;"
                  DELIMITED SIZE INTO WS-SQL-TEXT

           PERFORM EXECUTE-CATEGORY-QUERY

      *>   输出报表
           PERFORM DISPLAY-REPORT.

      *> ============================================================
      *> 显示报表
      *> ============================================================
       DISPLAY-REPORT SECTION.
           DISPLAY " "
           DISPLAY "Month: " FUNCTION TRIM(WS-ARG-MONTH)
           DISPLAY " "

      *>   转换为元显示
           COMPUTE WS-DISPLAY-INCOME = WS-TOTAL-INCOME / 100
           COMPUTE WS-DISPLAY-EXPENSE = WS-TOTAL-EXPENSE / 100
           COMPUTE WS-DISPLAY-NET = WS-NET-AMOUNT / 100

           DISPLAY "Total Income:  " WS-DISPLAY-INCOME
           DISPLAY "Total Expense: " WS-DISPLAY-EXPENSE
           DISPLAY "Net:           " WS-DISPLAY-NET
           DISPLAY " "

      *>   显示支出类别汇总
           DISPLAY "Expense by Category:"
           PERFORM VARYING WS-CAT-IDX FROM 1 BY 1 
                   UNTIL WS-CAT-IDX > WS-CAT-COUNT
               IF WS-CAT-TYPE-TBL(WS-CAT-IDX) = "EXPENSE"
                   COMPUTE WS-DISPLAY-AMOUNT = 
                           WS-CAT-TOTAL-TBL(WS-CAT-IDX) / 100
                   DISPLAY "- " WS-CAT-NAME-TBL(WS-CAT-IDX) 
                           "  " WS-DISPLAY-AMOUNT
               END-IF
           END-PERFORM
           DISPLAY " "

      *>   显示收入类别汇总
           DISPLAY "Income by Category:"
           PERFORM VARYING WS-CAT-IDX FROM 1 BY 1 
                   UNTIL WS-CAT-IDX > WS-CAT-COUNT
               IF WS-CAT-TYPE-TBL(WS-CAT-IDX) = "INCOME"
                   COMPUTE WS-DISPLAY-AMOUNT = 
                           WS-CAT-TOTAL-TBL(WS-CAT-IDX) / 100
                   DISPLAY "- " WS-CAT-NAME-TBL(WS-CAT-IDX) 
                           "  " WS-DISPLAY-AMOUNT
               END-IF
           END-PERFORM.

      *> ============================================================
      *> 执行汇总查询
      *> ============================================================
       EXECUTE-SUM-QUERY SECTION.
           MOVE 0 TO WS-RESULT-AMOUNT
      *>   实际实现需要调用 SQLite3 接口
           CONTINUE.

      *> ============================================================
      *> 执行类别汇总查询
      *> ============================================================
       EXECUTE-CATEGORY-QUERY SECTION.
      *>   实际实现需要调用 SQLite3 接口
      *>   将结果填充到 WS-CATEGORY-TABLE
           CONTINUE.

      *> ============================================================
      *> 获取报表数据（供导出使用）
      *> ============================================================
       GET-REPORT-DATA SECTION.
           PERFORM GENERATE-MONTHLY-REPORT.
